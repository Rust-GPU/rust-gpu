//! This algorithm is not intended to be an optimization, it is rather for legalization.
//! Specifically, spir-v disallows things like a `StorageClass::Function` pointer to a
//! `StorageClass::Input` pointer. Our frontend definitely allows it, though, this is like taking a
//! `&Input<T>` in a function! So, we inline all functions that take these "illegal" pointers, then
//! run mem2reg (see mem2reg.rs) on the result to "unwrap" the Function pointer.

use super::apply_rewrite_rules;
use super::ipo::CallGraph;
use super::simple_passes::outgoing_edges;
use super::{get_name, get_names};
use crate::custom_decorations::SpanRegenerator;
use crate::custom_insts::{self, CustomInst, CustomOp};
use rspirv::dr::{Block, Function, Instruction, Module, ModuleHeader, Operand};
use rspirv::spirv::{FunctionControl, Op, StorageClass, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet, FxIndexMap, FxIndexSet};
use rustc_errors::ErrorGuaranteed;
use rustc_session::Session;
use smallvec::SmallVec;
use std::cmp::Ordering;
use std::mem;

// FIXME(eddyb) this is a bit silly, but this keeps being repeated everywhere.
fn next_id(header: &mut ModuleHeader) -> Word {
    let result = header.bound;
    header.bound += 1;
    result
}

pub fn inline(sess: &Session, module: &mut Module) -> super::Result<()> {
    // This algorithm gets real sad if there's recursion - but, good news, SPIR-V bans recursion
    deny_recursion_in_module(sess, module)?;

    // Compute the call-graph that will drive (inside-out, aka bottom-up) inlining.
    let (call_graph, func_id_to_idx) = CallGraph::collect_with_func_id_to_idx(module);

    let custom_ext_inst_set_import = module
        .ext_inst_imports
        .iter()
        .find(|inst| {
            assert_eq!(inst.class.opcode, Op::ExtInstImport);
            inst.operands[0].unwrap_literal_string() == &custom_insts::CUSTOM_EXT_INST_SET[..]
        })
        .map(|inst| inst.result_id.unwrap());

    let legal_globals = LegalGlobal::gather_from_module(module);

    let header = module.header.as_mut().unwrap();

    // FIXME(eddyb) clippy false positive (separate `map` required for borrowck).
    #[allow(clippy::map_unwrap_or)]
    let mut inliner = Inliner {
        op_type_void_id: module
            .types_global_values
            .iter()
            .find(|inst| inst.class.opcode == Op::TypeVoid)
            .map(|inst| inst.result_id.unwrap())
            .unwrap_or_else(|| {
                let id = next_id(header);
                let inst = Instruction::new(Op::TypeVoid, None, Some(id), vec![]);
                module.types_global_values.push(inst);
                id
            }),

        custom_ext_inst_set_import: custom_ext_inst_set_import.unwrap_or_else(|| {
            let id = next_id(header);
            let inst = Instruction::new(
                Op::ExtInstImport,
                None,
                Some(id),
                vec![Operand::LiteralString(
                    custom_insts::CUSTOM_EXT_INST_SET.to_string(),
                )],
            );
            module.ext_inst_imports.push(inst);
            id
        }),

        func_id_to_idx,

        id_to_name: module
            .debug_names
            .iter()
            .filter(|inst| inst.class.opcode == Op::Name)
            .map(|inst| {
                (
                    inst.operands[0].unwrap_id_ref(),
                    inst.operands[1].unwrap_literal_string(),
                )
            })
            .collect(),

        cached_op_strings: FxHashMap::default(),

        header,
        debug_string_source: &mut module.debug_string_source,
        annotations: &mut module.annotations,

        legal_globals,

        // NOTE(eddyb) this is needed because our custom `Abort` instructions get
        // lowered to a simple `OpReturn` in entry-points, but that requires that
        // they get inlined all the way up to the entry-points in the first place.
        functions_that_may_abort: module
            .functions
            .iter()
            .filter_map(|func| {
                let custom_ext_inst_set_import = custom_ext_inst_set_import?;
                func.blocks
                    .iter()
                    .any(|block| match &block.instructions[..] {
                        [.., last_normal_inst, terminator_inst]
                            if last_normal_inst.class.opcode == Op::ExtInst
                                && last_normal_inst.operands[0].unwrap_id_ref()
                                    == custom_ext_inst_set_import
                                && CustomOp::decode_from_ext_inst(last_normal_inst)
                                    == CustomOp::Abort =>
                        {
                            assert_eq!(terminator_inst.class.opcode, Op::Unreachable);
                            true
                        }

                        _ => false,
                    })
                    .then_some(func.def_id().unwrap())
            })
            .collect(),

        inlined_dont_inlines_to_cause_and_callers: FxIndexMap::default(),
    };

    let mut functions: Vec<_> = mem::take(&mut module.functions)
        .into_iter()
        .map(Ok)
        .collect();

    // Inline functions in post-order (aka inside-out aka bottom-up) - that is,
    // callees are processed before their callers, to avoid duplicating work.
    for func_idx in call_graph.post_order() {
        let mut function = mem::replace(&mut functions[func_idx], Err(FuncIsBeingInlined)).unwrap();
        inliner.inline_fn(&mut function, &functions);
        fuse_trivial_branches(&mut function);
        functions[func_idx] = Ok(function);
    }

    module.functions = functions.into_iter().map(|func| func.unwrap()).collect();

    let Inliner {
        id_to_name,
        inlined_dont_inlines_to_cause_and_callers,
        ..
    } = inliner;

    let mut span_regen = SpanRegenerator::new(sess.source_map(), module);
    for (callee_id, (cause, callers)) in inlined_dont_inlines_to_cause_and_callers {
        let callee_name = get_name(&id_to_name, callee_id);

        // HACK(eddyb) `libcore` hides panics behind `#[inline(never)]` `fn`s,
        // making this too noisy and useless (since it's an impl detail).
        if cause == "panicking" && callee_name.starts_with("core::") {
            continue;
        }

        let callee_span = span_regen
            .src_loc_for_id(callee_id)
            .and_then(|src_loc| span_regen.src_loc_to_rustc(src_loc))
            .unwrap_or_default();
        sess.dcx()
            .struct_span_warn(
                callee_span,
                format!("`#[inline(never)]` function `{callee_name}` has been inlined"),
            )
            .with_note(format!("inlining was required due to {cause}"))
            .with_note(format!(
                "called from {}",
                callers
                    .iter()
                    .enumerate()
                    .filter_map(|(i, &caller_id)| {
                        // HACK(eddyb) avoid showing too many names.
                        match i.cmp(&4) {
                            Ordering::Less => {
                                Some(format!("`{}`", get_name(&id_to_name, caller_id)))
                            }
                            Ordering::Equal => Some(format!("and {} more", callers.len() - i)),
                            Ordering::Greater => None,
                        }
                    })
                    .collect::<SmallVec<[_; 5]>>()
                    .join(", ")
            ))
            .emit();
    }

    Ok(())
}

// https://stackoverflow.com/a/53995651
fn deny_recursion_in_module(sess: &Session, module: &Module) -> super::Result<()> {
    let func_to_index: FxHashMap<Word, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(index, func)| (func.def_id().unwrap(), index))
        .collect();
    let mut discovered = vec![false; module.functions.len()];
    let mut finished = vec![false; module.functions.len()];
    let mut has_recursion = None;
    for index in 0..module.functions.len() {
        if !discovered[index] && !finished[index] {
            visit(
                sess,
                module,
                index,
                &mut discovered,
                &mut finished,
                &mut has_recursion,
                &func_to_index,
            );
        }
    }

    fn visit(
        sess: &Session,
        module: &Module,
        current: usize,
        discovered: &mut Vec<bool>,
        finished: &mut Vec<bool>,
        has_recursion: &mut Option<ErrorGuaranteed>,
        func_to_index: &FxHashMap<Word, usize>,
    ) {
        discovered[current] = true;

        for next in calls(&module.functions[current], func_to_index) {
            if discovered[next] {
                let names = get_names(module);
                let current_name = get_name(&names, module.functions[current].def_id().unwrap());
                let next_name = get_name(&names, module.functions[next].def_id().unwrap());
                *has_recursion = Some(sess.dcx().err(format!(
                    "module has recursion, which is not allowed: `{current_name}` calls `{next_name}`"
                )));
                break;
            }

            if !finished[next] {
                visit(
                    sess,
                    module,
                    next,
                    discovered,
                    finished,
                    has_recursion,
                    func_to_index,
                );
            }
        }

        discovered[current] = false;
        finished[current] = true;
    }

    fn calls<'a>(
        func: &'a Function,
        func_to_index: &'a FxHashMap<Word, usize>,
    ) -> impl Iterator<Item = usize> + 'a {
        func.all_inst_iter()
            .filter(|inst| inst.class.opcode == Op::FunctionCall)
            .map(move |inst| {
                *func_to_index
                    .get(&inst.operands[0].id_ref_any().unwrap())
                    .unwrap()
            })
    }

    match has_recursion {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

/// Any type/const/global variable, which is "legal" (i.e. can be kept in SPIR-V).
///
/// For the purposes of the inliner, a legal global cannot:
/// - refer to any illegal globals
/// - (if a type) refer to any pointer types
///   - this rules out both pointers in composites, and pointers to pointers
///     (the latter itself *also* rules out variables containing pointers)
enum LegalGlobal {
    TypePointer(StorageClass),
    TypeNonPointer,
    Const,
    Variable,
}

impl LegalGlobal {
    fn gather_from_module(module: &Module) -> FxHashMap<Word, Self> {
        let mut legal_globals = FxHashMap::<_, Self>::default();
        for inst in &module.types_global_values {
            let global = match inst.class.opcode {
                Op::TypePointer => Self::TypePointer(inst.operands[0].unwrap_storage_class()),
                Op::Variable => Self::Variable,
                op if rspirv::grammar::reflect::is_type(op) => Self::TypeNonPointer,
                op if rspirv::grammar::reflect::is_constant(op) => Self::Const,

                // FIXME(eddyb) should this be `unreachable!()`?
                _ => continue,
            };
            let legal_result_type = match inst.result_type {
                Some(result_type_id) => matches!(
                    (&global, legal_globals.get(&result_type_id)),
                    (Self::Variable, Some(Self::TypePointer(_)))
                        | (Self::Const, Some(Self::TypeNonPointer))
                ),
                None => matches!(global, Self::TypePointer(_) | Self::TypeNonPointer),
            };
            let legal_operands = inst.operands.iter().all(|operand| match operand {
                Operand::IdRef(id) => matches!(
                    legal_globals.get(id),
                    Some(Self::TypeNonPointer | Self::Const)
                ),

                // NOTE(eddyb) this assumes non-ID operands are always legal.
                _ => operand.id_ref_any().is_none(),
            });
            if legal_result_type && legal_operands {
                legal_globals.insert(inst.result_id.unwrap(), global);
            }
        }
        legal_globals
    }

    fn legal_as_fn_param_ty(&self) -> bool {
        match *self {
            Self::TypePointer(storage_class) => matches!(
                storage_class,
                StorageClass::UniformConstant
                    | StorageClass::Function
                    | StorageClass::Private
                    | StorageClass::Workgroup
                    | StorageClass::AtomicCounter
            ),
            Self::TypeNonPointer => true,

            // FIXME(eddyb) should this be an `unreachable!()`?
            Self::Const | Self::Variable => false,
        }
    }

    fn legal_as_fn_ret_ty(&self) -> bool {
        #[allow(clippy::match_same_arms)]
        match *self {
            Self::TypePointer(_) => false,
            Self::TypeNonPointer => true,

            // FIXME(eddyb) should this be an `unreachable!()`?
            Self::Const | Self::Variable => false,
        }
    }
}

/// Helper type which encapsulates all the information about one specific call.
#[derive(Copy, Clone)]
struct CallSite<'a> {
    caller: &'a Function,
    call_inst: &'a Instruction,
}

fn has_dont_inline(function: &Function) -> bool {
    let def = function.def.as_ref().unwrap();
    let control = def.operands[0].unwrap_function_control();
    control.contains(FunctionControl::DONT_INLINE)
}

/// Helper error type for `should_inline` (see its doc comment).
#[derive(Copy, Clone, PartialEq, Eq)]
struct MustInlineToLegalize(&'static str);

/// Returns `Ok(true)`/`Err(MustInlineToLegalize(_))` if `callee` should/must be
/// inlined (either in general, or specifically from `call_site`, if provided).
///
/// The distinction made here is that `Err(MustInlineToLegalize(cause))` is
/// very much *not* a heuristic, and inlining is *mandatory* due to `cause`
/// (usually illegal signature/arguments, but also the panicking mechanism).
//
// FIXME(eddyb) the causes here are not fine-grained enough.
fn should_inline(
    legal_globals: &FxHashMap<Word, LegalGlobal>,
    functions_that_may_abort: &FxHashSet<Word>,
    callee: &Function,
    call_site: CallSite<'_>,
) -> Result<bool, MustInlineToLegalize> {
    let callee_def = callee.def.as_ref().unwrap();
    let callee_control = callee_def.operands[0].unwrap_function_control();

    if functions_that_may_abort.contains(&callee.def_id().unwrap()) {
        return Err(MustInlineToLegalize("panicking"));
    }

    let ret_ty = legal_globals
        .get(&callee_def.result_type.unwrap())
        .ok_or(MustInlineToLegalize("illegal return type"))?;
    if !ret_ty.legal_as_fn_ret_ty() {
        return Err(MustInlineToLegalize("illegal (pointer) return type"));
    }

    for (i, param) in callee.parameters.iter().enumerate() {
        let param_ty = legal_globals
            .get(param.result_type.as_ref().unwrap())
            .ok_or(MustInlineToLegalize("illegal parameter type"))?;
        if !param_ty.legal_as_fn_param_ty() {
            return Err(MustInlineToLegalize("illegal (pointer) parameter type"));
        }

        // If the call isn't passing a legal pointer argument (a "memory object",
        // i.e. an `OpVariable` or one of the caller's `OpFunctionParameter`s),
        // then inlining is required to have a chance at producing legal SPIR-V.
        //
        // FIXME(eddyb) rewriting away the pointer could be another alternative.
        if let LegalGlobal::TypePointer(_) = param_ty {
            let ptr_arg = call_site.call_inst.operands[i + 1].unwrap_id_ref();
            match legal_globals.get(&ptr_arg) {
                Some(LegalGlobal::Variable) => {}

                // FIXME(eddyb) should some constants (undef/null) be allowed?
                Some(_) => return Err(MustInlineToLegalize("illegal (pointer) argument")),

                None => {
                    let mut caller_param_and_var_ids = call_site
                        .caller
                        .parameters
                        .iter()
                        .chain(
                            call_site.caller.blocks[0]
                                .instructions
                                .iter()
                                .filter(|caller_inst| {
                                    // HACK(eddyb) this only avoids scanning the
                                    // whole entry block for `OpVariable`s, so
                                    // it can overapproximate debuginfo insts.
                                    let may_be_debuginfo = matches!(
                                        caller_inst.class.opcode,
                                        Op::Line | Op::NoLine | Op::ExtInst
                                    );
                                    !may_be_debuginfo
                                })
                                .take_while(|caller_inst| caller_inst.class.opcode == Op::Variable),
                        )
                        .map(|caller_inst| caller_inst.result_id.unwrap());

                    if !caller_param_and_var_ids.any(|id| ptr_arg == id) {
                        return Err(MustInlineToLegalize("illegal (pointer) argument"));
                    }
                }
            }
        }
    }

    Ok(callee_control.contains(FunctionControl::INLINE))
}

/// Helper error type for `Inliner`'s `functions` field, indicating a `Function`
/// was taken out of its slot because it's being inlined.
#[derive(Debug)]
struct FuncIsBeingInlined;

// Steps:
// Move OpVariable decls
// Rewrite return
// Renumber IDs
// Insert blocks

struct Inliner<'a, 'b> {
    /// ID of `OpExtInstImport` for our custom "extended instruction set"
    /// (see `crate::custom_insts` for more details).
    custom_ext_inst_set_import: Word,

    op_type_void_id: Word,

    /// Map from each function's ID to its index in `functions`.
    func_id_to_idx: FxHashMap<Word, usize>,

    /// Pre-collected `OpName`s, that can be used to find any function's name
    /// during inlining (to be able to generate debuginfo that uses names).
    id_to_name: FxHashMap<Word, &'a str>,

    /// `OpString` cache (for deduplicating `OpString`s for the same string).
    //
    // FIXME(eddyb) currently this doesn't reuse existing `OpString`s, but since
    // this is mostly for inlined callee names, it's expected almost no overlap
    // exists between existing `OpString`s and new ones, anyway.
    cached_op_strings: FxHashMap<&'a str, Word>,

    header: &'b mut ModuleHeader,
    debug_string_source: &'b mut Vec<Instruction>,
    annotations: &'b mut Vec<Instruction>,

    legal_globals: FxHashMap<Word, LegalGlobal>,
    functions_that_may_abort: FxHashSet<Word>,
    inlined_dont_inlines_to_cause_and_callers: FxIndexMap<Word, (&'static str, FxIndexSet<Word>)>,
    // rewrite_rules: FxHashMap<Word, Word>,
}

impl Inliner<'_, '_> {
    fn id(&mut self) -> Word {
        next_id(self.header)
    }

    /// Applies all rewrite rules to the decorations in the header.
    fn apply_rewrite_for_decorations(&mut self, rewrite_rules: &FxHashMap<Word, Word>) {
        // NOTE(siebencorgie): We don't care *what* decoration we rewrite atm.
        // AFAIK there is no case where keeping decorations on inline wouldn't be valid.
        for annotation_idx in 0..self.annotations.len() {
            let inst = &self.annotations[annotation_idx];
            if let [Operand::IdRef(target), ..] = inst.operands[..]
                && let Some(&rewritten_target) = rewrite_rules.get(&target)
            {
                // Copy decoration instruction and push it.
                let mut cloned_inst = inst.clone();
                cloned_inst.operands[0] = Operand::IdRef(rewritten_target);
                self.annotations.push(cloned_inst);
            }
        }
    }

    fn inline_fn(
        &mut self,
        function: &mut Function,
        functions: &[Result<Function, FuncIsBeingInlined>],
    ) {
        let mut block_idx = 0;
        while block_idx < function.blocks.len() {
            // If we successfully inlined a block, then repeat processing on the same block, in
            // case the newly inlined block has more inlined calls.
            // TODO: This is quadratic
            if !self.inline_block(function, block_idx, functions) {
                // TODO(eddyb) skip past the inlined callee without rescanning it.
                block_idx += 1;
            }
        }
    }

    fn inline_block(
        &mut self,
        caller: &mut Function,
        block_idx: usize,
        functions: &[Result<Function, FuncIsBeingInlined>],
    ) -> bool {
        // Find the first inlined OpFunctionCall
        let call = caller.blocks[block_idx]
            .instructions
            .iter()
            .enumerate()
            .filter(|(_, inst)| inst.class.opcode == Op::FunctionCall)
            .map(|(index, inst)| {
                (
                    index,
                    inst,
                    functions[self.func_id_to_idx[&inst.operands[0].id_ref_any().unwrap()]]
                        .as_ref()
                        .unwrap(),
                )
            })
            .find(|(_, inst, f)| {
                let call_site = CallSite {
                    caller,
                    call_inst: inst,
                };
                match should_inline(
                    &self.legal_globals,
                    &self.functions_that_may_abort,
                    f,
                    call_site,
                ) {
                    Ok(inline) => inline,
                    Err(MustInlineToLegalize(cause)) => {
                        if has_dont_inline(f) {
                            self.inlined_dont_inlines_to_cause_and_callers
                                .entry(f.def_id().unwrap())
                                .or_insert_with(|| (cause, Default::default()))
                                .1
                                .insert(caller.def_id().unwrap());
                        }
                        true
                    }
                }
            });
        let (call_index, call_inst, callee) = match call {
            None => return false,
            Some(call) => call,
        };

        // Propagate "may abort" from callee to caller (i.e. as aborts get inlined).
        if self
            .functions_that_may_abort
            .contains(&callee.def_id().unwrap())
        {
            self.functions_that_may_abort
                .insert(caller.def_id().unwrap());
        }

        let mut maybe_call_result_phi = {
            let ty = call_inst.result_type.unwrap();
            if ty == self.op_type_void_id {
                None
            } else {
                Some(Instruction::new(
                    Op::Phi,
                    Some(ty),
                    Some(call_inst.result_id.unwrap()),
                    vec![],
                ))
            }
        };

        // Get the debug "source location" instruction that applies to the call.
        let custom_ext_inst_set_import = self.custom_ext_inst_set_import;
        let call_debug_src_loc_inst = caller.blocks[block_idx].instructions[..call_index]
            .iter()
            .rev()
            .find_map(|inst| {
                Some(match inst.class.opcode {
                    Op::Line => Some(inst),
                    Op::NoLine => None,
                    Op::ExtInst
                        if inst.operands[0].unwrap_id_ref() == custom_ext_inst_set_import =>
                    {
                        match CustomOp::decode_from_ext_inst(inst) {
                            CustomOp::SetDebugSrcLoc => Some(inst),
                            CustomOp::ClearDebugSrcLoc => None,
                            _ => return None,
                        }
                    }
                    _ => return None,
                })
            })
            .flatten();

        // Rewrite parameters to arguments
        let call_arguments = call_inst
            .operands
            .iter()
            .skip(1)
            .map(|op| op.id_ref_any().unwrap());
        let callee_parameters = callee.parameters.iter().map(|inst| {
            assert!(inst.class.opcode == Op::FunctionParameter);
            inst.result_id.unwrap()
        });
        let mut rewrite_rules = callee_parameters.zip(call_arguments).collect();

        let return_jump = self.id();
        // Rewrite OpReturns of the callee.
        let mut inlined_callee_blocks = self.get_inlined_blocks(
            callee,
            call_debug_src_loc_inst,
            maybe_call_result_phi.as_mut(),
            return_jump,
        );
        // Clone the IDs of the callee, because otherwise they'd be defined multiple times if the
        // fn is inlined multiple times.
        self.add_clone_id_rules(&mut rewrite_rules, &inlined_callee_blocks);
        apply_rewrite_rules(&rewrite_rules, &mut inlined_callee_blocks);
        self.apply_rewrite_for_decorations(&rewrite_rules);

        if let Some(call_result_phi) = &mut maybe_call_result_phi {
            // HACK(eddyb) new IDs should be generated earlier, to avoid pushing
            // callee IDs to `call_result_phi.operands` only to rewrite them here.
            for op in &mut call_result_phi.operands {
                if let Some(id) = op.id_ref_any_mut()
                    && let Some(&rewrite) = rewrite_rules.get(id)
                {
                    *id = rewrite;
                }
            }

            // HACK(eddyb) this special-casing of the single-return case is
            // really necessary for passes like `mem2reg` which are not capable
            // of skipping through the extraneous `OpPhi`s on their own.
            if let [returned_value, _return_block] = &call_result_phi.operands[..] {
                let call_result_id = call_result_phi.result_id.unwrap();
                let returned_value_id = returned_value.unwrap_id_ref();

                maybe_call_result_phi = None;

                // HACK(eddyb) this is a conservative approximation of all the
                // instructions that could potentially reference the call result.
                let reaching_insts = {
                    let (pre_call_blocks, call_and_post_call_blocks) =
                        caller.blocks.split_at_mut(block_idx);
                    (pre_call_blocks.iter_mut().flat_map(|block| {
                        block
                            .instructions
                            .iter_mut()
                            .take_while(|inst| inst.class.opcode == Op::Phi)
                    }))
                    .chain(
                        call_and_post_call_blocks
                            .iter_mut()
                            .flat_map(|block| &mut block.instructions),
                    )
                };
                for reaching_inst in reaching_insts {
                    for op in &mut reaching_inst.operands {
                        if let Some(id) = op.id_ref_any_mut()
                            && *id == call_result_id
                        {
                            *id = returned_value_id;
                        }
                    }
                }
            }
        }

        // Split the block containing the `OpFunctionCall` into pre-call vs post-call.
        let pre_call_block_idx = block_idx;
        #[expect(unused)]
        let block_idx: usize; // HACK(eddyb) disallowing using the unrenamed variable.
        let mut post_call_block_insts = caller.blocks[pre_call_block_idx]
            .instructions
            .split_off(call_index + 1);

        // pop off OpFunctionCall
        let call = caller.blocks[pre_call_block_idx]
            .instructions
            .pop()
            .unwrap();
        assert!(call.class.opcode == Op::FunctionCall);

        // Insert non-entry inlined callee blocks just after the pre-call block.
        let non_entry_inlined_callee_blocks = inlined_callee_blocks.drain(1..);
        let num_non_entry_inlined_callee_blocks = non_entry_inlined_callee_blocks.len();
        caller.blocks.splice(
            (pre_call_block_idx + 1)..(pre_call_block_idx + 1),
            non_entry_inlined_callee_blocks,
        );

        if let Some(call_result_phi) = maybe_call_result_phi {
            // Add the `OpPhi` for the call result value, after the inlined function.
            post_call_block_insts.insert(0, call_result_phi);
        }

        // Insert the post-call block, after all the inlined callee blocks.
        {
            let post_call_block_idx = pre_call_block_idx + num_non_entry_inlined_callee_blocks + 1;
            let post_call_block = Block {
                label: Some(Instruction::new(Op::Label, None, Some(return_jump), vec![])),
                instructions: post_call_block_insts,
            };
            caller.blocks.insert(post_call_block_idx, post_call_block);

            // Adjust any `OpPhi`s in the (caller) targets of the original call block,
            // to refer to post-call block (the new source of those CFG edges).
            rewrite_phi_sources(
                caller.blocks[pre_call_block_idx].label_id().unwrap(),
                &mut caller.blocks,
                post_call_block_idx,
            );
        }

        // Fuse the inlined callee entry block into the pre-call block.
        // This is okay because it's illegal to branch to the first BB in a function.
        {
            // NOTE(eddyb) `OpExtInst`s have a result ID, even if unused, and
            // it has to be unique, so this allocates new IDs as-needed.
            let instantiate_debuginfo = |this: &mut Self, inst: &Instruction| {
                let mut inst = inst.clone();
                if let Some(id) = &mut inst.result_id {
                    *id = this.id();
                }
                inst
            };

            let custom_inst_to_inst = |this: &mut Self, inst: CustomInst<_>| {
                Instruction::new(
                    Op::ExtInst,
                    Some(this.op_type_void_id),
                    Some(this.id()),
                    [
                        Operand::IdRef(this.custom_ext_inst_set_import),
                        Operand::LiteralExtInstInteger(inst.op() as u32),
                    ]
                    .into_iter()
                    .chain(inst.into_operands())
                    .collect(),
                )
            };

            // Return the subsequence of `insts` made from `OpVariable`s, and any
            // debuginfo instructions (which may apply to them), while removing
            // *only* `OpVariable`s from `insts` (and keeping debuginfo in both).
            let mut steal_vars = |insts: &mut Vec<Instruction>| {
                // HACK(eddyb) this duplicates some code from `get_inlined_blocks`,
                // but that will be removed once the inliner is refactored to be
                // inside-out instead of outside-in (already finished in a branch).
                let mut enclosing_inlined_frames = SmallVec::<[_; 8]>::new();
                let mut current_debug_src_loc_inst = None;
                let mut vars_and_debuginfo_range = 0..0;
                while vars_and_debuginfo_range.end < insts.len() {
                    let inst = &insts[vars_and_debuginfo_range.end];
                    match inst.class.opcode {
                        Op::Line => current_debug_src_loc_inst = Some(inst),
                        Op::NoLine => current_debug_src_loc_inst = None,
                        Op::ExtInst
                            if inst.operands[0].unwrap_id_ref()
                                == self.custom_ext_inst_set_import =>
                        {
                            match CustomOp::decode_from_ext_inst(inst) {
                                CustomOp::SetDebugSrcLoc => current_debug_src_loc_inst = Some(inst),
                                CustomOp::ClearDebugSrcLoc => current_debug_src_loc_inst = None,
                                CustomOp::PushInlinedCallFrame => {
                                    enclosing_inlined_frames
                                        .push((current_debug_src_loc_inst.take(), inst));
                                }
                                CustomOp::PopInlinedCallFrame => {
                                    if let Some((callsite_debug_src_loc_inst, _)) =
                                        enclosing_inlined_frames.pop()
                                    {
                                        current_debug_src_loc_inst = callsite_debug_src_loc_inst;
                                    }
                                }
                                CustomOp::Abort => break,
                            }
                        }
                        Op::Variable => {}
                        _ => break,
                    }
                    vars_and_debuginfo_range.end += 1;
                }

                // `vars_and_debuginfo_range.end` indicates where `OpVariable`s
                // end and other instructions start (modulo debuginfo), but to
                // split the block in two, both sides of the "cut" need "repair":
                // - the variables are missing "inlined call frames" pops, that
                //   may happen later in the block, and have to be synthesized
                // - the non-variables are missing "inlined call frames" pushes,
                //   that must be recreated to avoid ending up with dangling pops
                //
                // FIXME(eddyb) this only collects to avoid borrow conflicts,
                // between e.g. `enclosing_inlined_frames` and mutating `insts`,
                // but also between different uses of `self`.
                let all_pops_after_vars: SmallVec<[_; 8]> = enclosing_inlined_frames
                    .iter()
                    .map(|_| custom_inst_to_inst(self, CustomInst::PopInlinedCallFrame))
                    .collect();
                let all_repushes_before_non_vars: SmallVec<[_; 8]> =
                    (enclosing_inlined_frames.into_iter().flat_map(
                        |(callsite_debug_src_loc_inst, push_inlined_call_frame_inst)| {
                            (callsite_debug_src_loc_inst.into_iter())
                                .chain([push_inlined_call_frame_inst])
                        },
                    ))
                    .chain(current_debug_src_loc_inst)
                    .map(|inst| instantiate_debuginfo(self, inst))
                    .collect();

                let vars_and_debuginfo =
                    insts.splice(vars_and_debuginfo_range, all_repushes_before_non_vars);
                let repaired_vars_and_debuginfo = vars_and_debuginfo.chain(all_pops_after_vars);

                // FIXME(eddyb) collecting shouldn't be necessary but this is
                // nested in a closure, and `splice` borrows the original `Vec`.
                repaired_vars_and_debuginfo.collect::<SmallVec<[_; 8]>>()
            };

            let [mut inlined_callee_entry_block]: [_; 1] =
                inlined_callee_blocks.try_into().unwrap();

            // Move the `OpVariable`s of the callee to the caller.
            let callee_vars_and_debuginfo =
                steal_vars(&mut inlined_callee_entry_block.instructions);
            self.insert_opvariables(&mut caller.blocks[0], callee_vars_and_debuginfo);

            caller.blocks[pre_call_block_idx]
                .instructions
                .append(&mut inlined_callee_entry_block.instructions);

            // Adjust any `OpPhi`s in the (inlined callee) targets of the
            // inlined callee entry block, to refer to the pre-call block
            // (the new source of those CFG edges).
            rewrite_phi_sources(
                inlined_callee_entry_block.label_id().unwrap(),
                &mut caller.blocks,
                pre_call_block_idx,
            );
        }

        true
    }

    fn add_clone_id_rules(&mut self, rewrite_rules: &mut FxHashMap<Word, Word>, blocks: &[Block]) {
        for block in blocks {
            for inst in block.label.iter().chain(&block.instructions) {
                if let Some(result_id) = inst.result_id {
                    let new_id = self.id();
                    let old = rewrite_rules.insert(result_id, new_id);
                    assert!(old.is_none());
                }
            }
        }
    }

    fn get_inlined_blocks(
        &mut self,
        callee: &Function,
        call_debug_src_loc_inst: Option<&Instruction>,
        mut maybe_call_result_phi: Option<&mut Instruction>,
        return_jump: Word,
    ) -> Vec<Block> {
        let Self {
            custom_ext_inst_set_import,
            op_type_void_id,
            ..
        } = *self;

        // Prepare the debuginfo insts to prepend/append to every block.
        // FIXME(eddyb) this could be more efficient if we only used one pair of
        // `{Push,Pop}InlinedCallFrame` for the whole inlined callee, but there
        // is no way to hint the SPIR-T CFG (re)structurizer that it should keep
        // the entire callee in one region - a SPIR-T inliner wouldn't have this
        // issue, as it would require a fully structured callee.
        let callee_name = self
            .id_to_name
            .get(&callee.def_id().unwrap())
            .copied()
            .unwrap_or("");
        let callee_name_id = *self
            .cached_op_strings
            .entry(callee_name)
            .or_insert_with(|| {
                let id = next_id(self.header);
                self.debug_string_source.push(Instruction::new(
                    Op::String,
                    None,
                    Some(id),
                    vec![Operand::LiteralString(callee_name.to_string())],
                ));
                id
            });
        let mut mk_debuginfo_prefix_and_suffix = || {
            // NOTE(eddyb) `OpExtInst`s have a result ID, even if unused, and
            // it has to be unique (same goes for the other instructions below).
            let instantiate_debuginfo = |this: &mut Self, inst: &Instruction| {
                let mut inst = inst.clone();
                if let Some(id) = &mut inst.result_id {
                    *id = this.id();
                }
                inst
            };
            let custom_inst_to_inst = |this: &mut Self, inst: CustomInst<_>| {
                Instruction::new(
                    Op::ExtInst,
                    Some(op_type_void_id),
                    Some(this.id()),
                    [
                        Operand::IdRef(custom_ext_inst_set_import),
                        Operand::LiteralExtInstInteger(inst.op() as u32),
                    ]
                    .into_iter()
                    .chain(inst.into_operands())
                    .collect(),
                )
            };

            (
                (call_debug_src_loc_inst.map(|inst| instantiate_debuginfo(self, inst)))
                    .into_iter()
                    .chain([custom_inst_to_inst(
                        self,
                        CustomInst::PushInlinedCallFrame {
                            callee_name: Operand::IdRef(callee_name_id),
                        },
                    )]),
                [custom_inst_to_inst(self, CustomInst::PopInlinedCallFrame)],
            )
        };

        let mut blocks = callee.blocks.clone();
        for block in &mut blocks {
            let mut terminator = block.instructions.pop().unwrap();

            // HACK(eddyb) strip trailing debuginfo (as it can't impact terminators).
            while let Some(last) = block.instructions.last() {
                let can_remove = match last.class.opcode {
                    Op::Line | Op::NoLine => true,
                    Op::ExtInst => {
                        last.operands[0].unwrap_id_ref() == custom_ext_inst_set_import
                            && matches!(
                                CustomOp::decode_from_ext_inst(last),
                                CustomOp::SetDebugSrcLoc | CustomOp::ClearDebugSrcLoc
                            )
                    }
                    _ => false,
                };
                if can_remove {
                    block.instructions.pop();
                } else {
                    break;
                }
            }

            if let Op::Return | Op::ReturnValue = terminator.class.opcode {
                if Op::ReturnValue == terminator.class.opcode {
                    let return_value = terminator.operands[0].id_ref_any().unwrap();
                    let call_result_phi = maybe_call_result_phi.as_deref_mut().unwrap();
                    call_result_phi.operands.extend([
                        Operand::IdRef(return_value),
                        Operand::IdRef(block.label_id().unwrap()),
                    ]);
                } else {
                    assert!(maybe_call_result_phi.is_none());
                }
                terminator =
                    Instruction::new(Op::Branch, None, None, vec![Operand::IdRef(return_jump)]);
            }

            let num_phis = block
                .instructions
                .iter()
                .take_while(|inst| inst.class.opcode == Op::Phi)
                .count();

            // HACK(eddyb) avoid adding debuginfo to otherwise-empty blocks.
            if block.instructions.len() > num_phis {
                let (debuginfo_prefix, debuginfo_suffix) = mk_debuginfo_prefix_and_suffix();
                // Insert the prefix debuginfo instructions after `OpPhi`s,
                // which sadly can't be covered by them.
                block
                    .instructions
                    .splice(num_phis..num_phis, debuginfo_prefix);
                // Insert the suffix debuginfo instructions before the terminator,
                // which sadly can't be covered by them.
                block.instructions.extend(debuginfo_suffix);
            }

            block.instructions.push(terminator);
        }

        blocks
    }

    fn insert_opvariables(&self, block: &mut Block, insts: impl IntoIterator<Item = Instruction>) {
        // HACK(eddyb) this isn't as efficient as it could be in theory, but it's
        // very important to make sure sure to never insert new instructions in
        // the middle of debuginfo (as it would be affected by it).
        let mut inlined_frames_depth = 0usize;
        let mut outermost_has_debug_src_loc = false;
        let mut last_debugless_var_insertion_point_candidate = None;
        for (i, inst) in block.instructions.iter().enumerate() {
            last_debugless_var_insertion_point_candidate =
                (inlined_frames_depth == 0 && !outermost_has_debug_src_loc).then_some(i);

            let changed_has_debug_src_loc = match inst.class.opcode {
                Op::Line => true,
                Op::NoLine => false,
                Op::ExtInst
                    if inst.operands[0].unwrap_id_ref() == self.custom_ext_inst_set_import =>
                {
                    match CustomOp::decode_from_ext_inst(inst) {
                        CustomOp::SetDebugSrcLoc => true,
                        CustomOp::ClearDebugSrcLoc => false,
                        CustomOp::PushInlinedCallFrame => {
                            inlined_frames_depth += 1;
                            continue;
                        }
                        CustomOp::PopInlinedCallFrame => {
                            inlined_frames_depth = inlined_frames_depth.saturating_sub(1);
                            continue;
                        }
                        CustomOp::Abort => break,
                    }
                }
                Op::Variable => continue,
                _ => break,
            };

            if inlined_frames_depth == 0 {
                outermost_has_debug_src_loc = changed_has_debug_src_loc;
            }
        }

        // HACK(eddyb) fallback to inserting at the start, which should be correct.
        // FIXME(eddyb) some level of debuginfo repair could prevent needing this.
        let i = last_debugless_var_insertion_point_candidate.unwrap_or(0);
        block.instructions.splice(i..i, insts);
    }
}

fn fuse_trivial_branches(function: &mut Function) {
    let all_preds = compute_preds(&function.blocks);
    'outer: for (dest_block, mut preds) in all_preds.iter().enumerate() {
        // Don't fuse branches into blocks with `OpPhi`s.
        let any_phis = function.blocks[dest_block]
            .instructions
            .iter()
            .filter(|inst| {
                // These are the only instructions that are allowed before `OpPhi`.
                !matches!(inst.class.opcode, Op::Line | Op::NoLine)
            })
            .take_while(|inst| inst.class.opcode == Op::Phi)
            .next()
            .is_some();
        if any_phis {
            continue;
        }

        // if there's two trivial branches in a row, the middle one might get inlined before the
        // last one, so when processing the last one, skip through to the first one.
        let pred = loop {
            if preds.len() != 1 || preds[0] == dest_block {
                continue 'outer;
            }
            let pred = preds[0];
            if !function.blocks[pred].instructions.is_empty() {
                break pred;
            }
            preds = &all_preds[pred];
        };
        let pred_insts = &function.blocks[pred].instructions;
        if pred_insts.last().unwrap().class.opcode == Op::Branch {
            let mut dest_insts = mem::take(&mut function.blocks[dest_block].instructions);
            let pred_insts = &mut function.blocks[pred].instructions;
            pred_insts.pop(); // pop the branch
            pred_insts.append(&mut dest_insts);

            // Adjust any `OpPhi`s in the targets of the original block, to refer
            // to the sole predecessor (the new source of those CFG edges).
            rewrite_phi_sources(
                function.blocks[dest_block].label_id().unwrap(),
                &mut function.blocks,
                pred,
            );
        }
    }
    function.blocks.retain(|b| !b.instructions.is_empty());
}

fn compute_preds(blocks: &[Block]) -> Vec<Vec<usize>> {
    let mut result = vec![vec![]; blocks.len()];
    for (source_idx, source) in blocks.iter().enumerate() {
        for dest_id in outgoing_edges(source) {
            let dest_idx = blocks
                .iter()
                .position(|b| b.label_id().unwrap() == dest_id)
                .unwrap();
            result[dest_idx].push(source_idx);
        }
    }
    result
}

/// Helper for adjusting `OpPhi` source label IDs, when the terminator of the
/// `original_label_id`-labeled block got moved to `blocks[original_block_idx]`.
fn rewrite_phi_sources(original_label_id: Word, blocks: &mut [Block], new_block_idx: usize) {
    let new_label_id = blocks[new_block_idx].label_id().unwrap();

    // HACK(eddyb) can't keep `blocks` borrowed, the loop needs mutable access.
    let target_ids: SmallVec<[_; 4]> = outgoing_edges(&blocks[new_block_idx]).collect();

    for target_id in target_ids {
        let target_block = blocks
            .iter_mut()
            .find(|b| b.label_id().unwrap() == target_id)
            .unwrap();
        let phis = target_block
            .instructions
            .iter_mut()
            .filter(|inst| {
                // These are the only instructions that are allowed before `OpPhi`.
                !matches!(inst.class.opcode, Op::Line | Op::NoLine)
            })
            .take_while(|inst| inst.class.opcode == Op::Phi);
        for phi in phis {
            for value_and_source_id in phi.operands.chunks_mut(2) {
                let source_id = value_and_source_id[1].id_ref_any_mut().unwrap();
                if *source_id == original_label_id {
                    *source_id = new_label_id;
                    break;
                }
            }
        }
    }
}
