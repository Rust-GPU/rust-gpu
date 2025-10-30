//! SPIR-T pass infrastructure and supporting utilities.

pub(crate) mod controlflow;
pub(crate) mod debuginfo;
pub(crate) mod diagnostics;
mod expand;
pub(crate) mod explicit_layout;
mod fuse_selects;
mod reduce;
pub(crate) mod validate;

use lazy_static::lazy_static;
use rustc_data_structures::fx::FxIndexSet;
use rustc_index::bit_set::DenseBitSet as BitSet;
use smallvec::SmallVec;
use spirt::cf::SelectionKind;
use spirt::func_at::FuncAt;
use spirt::mem::MemOp;
use spirt::qptr::QPtrOp;
use spirt::visit::{InnerVisit, Visitor};
use spirt::{
    AttrSet, Const, Context, DataInstKind, DeclDef, EntityOrientedDenseMap, Func, FuncDefBody,
    GlobalVar, Module, Node, NodeDef, NodeKind, Region, Type, Value, Var, VarKind, spv,
};
use std::collections::VecDeque;
use std::str;

// HACK(eddyb) `spv::spec::Spec` with extra `WellKnown`s (that should be upstreamed).
macro_rules! def_spv_spec_with_extra_well_known {
    ($($group:ident: $ty:ty = [$($entry:ident),+ $(,)?]),+ $(,)?) => {
        struct SpvSpecWithExtras {
            __base_spec: &'static spv::spec::Spec,

            well_known: SpvWellKnownWithExtras,
        }

        #[allow(non_snake_case)]
        pub struct SpvWellKnownWithExtras {
            __base_well_known: &'static spv::spec::WellKnown,

            $($(pub $entry: $ty,)+)+
        }

        impl std::ops::Deref for SpvSpecWithExtras {
            type Target = spv::spec::Spec;
            fn deref(&self) -> &Self::Target {
                self.__base_spec
            }
        }

        impl std::ops::Deref for SpvWellKnownWithExtras {
            type Target = spv::spec::WellKnown;
            fn deref(&self) -> &Self::Target {
                self.__base_well_known
            }
        }

        impl SpvSpecWithExtras {
            #[inline(always)]
            #[must_use]
            pub fn get() -> &'static SpvSpecWithExtras {
                lazy_static! {
                    static ref SPEC: SpvSpecWithExtras = {
                        #[allow(non_camel_case_types)]
                        struct PerWellKnownGroup<$($group),+> {
                            $($group: $group),+
                        }

                        let spv_spec = spv::spec::Spec::get();
                        let wk = &spv_spec.well_known;

                        let [decorations, storage_classes] = [wk.Decoration, wk.StorageClass].map(|kind| match kind.def() {
                            spv::spec::OperandKindDef::ValueEnum { variants } => variants,
                            _ => unreachable!(),
                        });

                        let lookup_fns = PerWellKnownGroup {
                            opcode: |name| spv_spec.instructions.lookup(name).unwrap(),
                            operand_kind: |name| spv_spec.operand_kinds.lookup(name).unwrap(),
                            decoration: |name| decorations.lookup(name).unwrap().into(),
                            storage_class: |name| storage_classes.lookup(name).unwrap().into(),
                        };

                        SpvSpecWithExtras {
                            __base_spec: spv_spec,

                            well_known: SpvWellKnownWithExtras {
                                __base_well_known: &spv_spec.well_known,

                                $($($entry: (lookup_fns.$group)(stringify!($entry)),)+)+
                            },
                        }
                    };
                }
                &SPEC
            }
        }
    };
}
def_spv_spec_with_extra_well_known! {
    opcode: spv::spec::Opcode = [
        OpCopyMemory,

        OpSelect,

        OpConstantNull,
        OpSpecConstantOp,
        OpConvertUToPtr,
        OpConvertPtrToU,
    ],
    operand_kind: spv::spec::OperandKind = [
        Capability,
        ExecutionModel,
        ImageFormat,
        MemoryAccess,
    ],
    decoration: u32 = [
        UserTypeGOOGLE,
        MatrixStride,
    ],
    storage_class: u32 = [
        PushConstant,
        Uniform,
        StorageBuffer,
        PhysicalStorageBuffer,
    ],
}

const SPIRT_MEM_LAYOUT_CONFIG: &spirt::mem::LayoutConfig = &spirt::mem::LayoutConfig {
    abstract_bool_size_align: (1, 1),
    logical_ptr_size_align: (4, 4),
    ..spirt::mem::LayoutConfig::VULKAN_SCALAR_LAYOUT_LE
};
const QPTR_SIZED_UINT: spirt::scalar::Type = {
    let (qptr_size, _) = SPIRT_MEM_LAYOUT_CONFIG.logical_ptr_size_align;
    spirt::scalar::Type::UInt(
        match spirt::scalar::IntWidth::try_from_bits(qptr_size * 8) {
            Some(w) => w,
            None => unreachable!(),
        },
    )
};

/// Run intra-function passes on all `Func` definitions in the `Module`.
//
// FIXME(eddyb) introduce a proper "pass manager".
// FIXME(eddyb) why does this focus on functions, it could just be module passes??
pub(super) fn run_func_passes<P>(
    module: &mut Module,
    passes: &[impl AsRef<str>],
    // FIXME(eddyb) this is a very poor approximation of a "profiler" abstraction.
    mut before_pass: impl FnMut(&'static str, &Module) -> P,
    mut after_pass: impl FnMut(Option<&Module>, P),
) {
    let cx = &module.cx();

    // FIXME(eddyb) reuse this collection work in some kind of "pass manager".
    let all_funcs = {
        let mut collector = ReachableUseCollector {
            cx,
            module,

            seen_types: FxIndexSet::default(),
            seen_consts: FxIndexSet::default(),
            seen_global_vars: FxIndexSet::default(),
            seen_funcs: FxIndexSet::default(),
        };
        for (export_key, &exportee) in &module.exports {
            export_key.inner_visit_with(&mut collector);
            exportee.inner_visit_with(&mut collector);
        }
        collector.seen_funcs
    };

    let mut needs_qptr_lifting = false;

    for name in passes {
        let name = name.as_ref();

        // HACK(eddyb) not really a function pass.
        if name == "qptr" {
            let profiler = before_pass("qptr::lower_from_spv_ptrs", module);
            spirt::passes::qptr::lower_from_spv_ptrs(module, SPIRT_MEM_LAYOUT_CONFIG);
            after_pass(Some(module), profiler);

            // HACK(eddyb) see `if needs_qptr_lifting` for where this is handled.
            needs_qptr_lifting = true;

            continue;
        }

        let (full_name, pass_fn): (_, fn(_, &mut _)) = match name {
            "reduce" => ("spirt_passes::reduce", reduce::reduce_in_func),
            "fuse_selects" => (
                "spirt_passes::fuse_selects",
                fuse_selects::fuse_selects_in_func,
            ),
            _ => panic!("unknown `--spirt-passes={name}`"),
        };

        let profiler = before_pass(full_name, module);
        for &func in &all_funcs {
            if let DeclDef::Present(func_def_body) = &mut module.funcs[func].def {
                pass_fn(cx, func_def_body);

                // FIXME(eddyb) avoid doing this except where changes occurred.
                remove_unused_values_in_func(func_def_body);
            }
        }
        after_pass(Some(module), profiler);
    }

    // HACK(eddyb) this is here so it can take effect *before* `qptr::lift`!
    {
        use expand::TypeDrivenExpansion as _;

        let profiler = before_pass("spirt_passes::expand::ExpandUnsupportedIntegers", module);
        expand::ExpandUnsupportedIntegers::for_max_supported_width(
            &module.cx(),
            spirt::scalar::IntWidth::I64,
        )
        .expand_module(module);
        after_pass(Some(module), profiler);
    }

    // HACK(eddyb) `qptr` is less of a "pass" and more of a "dialect", and it
    // largely doesn't make sense to have additional transformations between
    // "lifting `qptr` back to `OpTypePointer`s" and "lifting SPIR-T to SPIR-V".
    if needs_qptr_lifting {
        let profiler = before_pass("mem::analyze_accesses", module);
        spirt::passes::qptr::analyze_mem_accesses(module, SPIRT_MEM_LAYOUT_CONFIG);
        after_pass(Some(module), profiler);

        let profiler = before_pass("qptr::lift_to_spv_ptrs", module);
        spirt::passes::qptr::lift_to_spv_ptrs(module, SPIRT_MEM_LAYOUT_CONFIG);
        after_pass(Some(module), profiler);
    }
}

fn decode_spv_lit_str_with<R>(imms: &[spv::Imm], f: impl FnOnce(&str) -> R) -> R {
    let wk = &SpvSpecWithExtras::get().well_known;

    // FIXME(eddyb) deduplicate with `spirt::spv::extract_literal_string`.
    let words = imms.iter().enumerate().map(|(i, &imm)| match (i, imm) {
        (0, spirt::spv::Imm::Short(k, w) | spirt::spv::Imm::LongStart(k, w))
        | (1.., spirt::spv::Imm::LongCont(k, w)) => {
            assert_eq!(k, wk.LiteralString);
            w
        }
        _ => unreachable!(),
    });
    let bytes: SmallVec<[u8; 64]> = words
        .flat_map(u32::to_le_bytes)
        .take_while(|&byte| byte != 0)
        .collect();

    f(str::from_utf8(&bytes).expect("invalid UTF-8 in string literal"))
}

// FIXME(eddyb) this is just copy-pasted from `spirt` and should be reusable.
struct ReachableUseCollector<'a> {
    cx: &'a Context,
    module: &'a Module,

    // FIXME(eddyb) build some automation to avoid ever repeating these.
    seen_types: FxIndexSet<Type>,
    seen_consts: FxIndexSet<Const>,
    seen_global_vars: FxIndexSet<GlobalVar>,
    seen_funcs: FxIndexSet<Func>,
}

impl Visitor<'_> for ReachableUseCollector<'_> {
    // FIXME(eddyb) build some automation to avoid ever repeating these.
    fn visit_attr_set_use(&mut self, _attrs: AttrSet) {}
    fn visit_type_use(&mut self, ty: Type) {
        if self.seen_types.insert(ty) {
            self.visit_type_def(&self.cx[ty]);
        }
    }
    fn visit_const_use(&mut self, ct: Const) {
        if self.seen_consts.insert(ct) {
            self.visit_const_def(&self.cx[ct]);
        }
    }

    fn visit_global_var_use(&mut self, gv: GlobalVar) {
        if self.seen_global_vars.insert(gv) {
            self.visit_global_var_decl(&self.module.global_vars[gv]);
        }
    }
    fn visit_func_use(&mut self, func: Func) {
        if self.seen_funcs.insert(func) {
            self.visit_func_decl(&self.module.funcs[func]);
        }
    }
}

// FIXME(eddyb) maybe this should be provided by `spirt::visit`.
struct VisitAllRegionsAndNodes<S, ENCR, EXCR, ENCN, EXCN> {
    state: S,
    enter_region: ENCR,
    exit_region: EXCR,
    enter_node: ENCN,
    exit_node: EXCN,
}
const _: () = {
    use spirt::{func_at::*, visit::*, *};

    impl<
        'a,
        S,
        ENCR: FnMut(&mut S, FuncAt<'a, Region>),
        EXCR: FnMut(&mut S, FuncAt<'a, Region>),
        ENCN: FnMut(&mut S, FuncAt<'a, Node>),
        EXCN: FnMut(&mut S, FuncAt<'a, Node>),
    > Visitor<'a> for VisitAllRegionsAndNodes<S, ENCR, EXCR, ENCN, EXCN>
    {
        // FIXME(eddyb) this is excessive, maybe different kinds of
        // visitors should exist for module-level and func-level?
        fn visit_attr_set_use(&mut self, _: AttrSet) {}
        fn visit_type_use(&mut self, _: Type) {}
        fn visit_const_use(&mut self, _: Const) {}
        fn visit_global_var_use(&mut self, _: GlobalVar) {}
        fn visit_func_use(&mut self, _: Func) {}

        fn visit_region_def(&mut self, func_at_region: FuncAt<'a, Region>) {
            (self.enter_region)(&mut self.state, func_at_region);
            func_at_region.inner_visit_with(self);
            (self.exit_region)(&mut self.state, func_at_region);
        }
        fn visit_node_def(&mut self, func_at_node: FuncAt<'a, Node>) {
            (self.enter_node)(&mut self.state, func_at_node);
            func_at_node.inner_visit_with(self);
            (self.exit_node)(&mut self.state, func_at_node);
        }
    }
};

// FIXME(eddyb) maybe this should be provided by `spirt::transform`.
struct ReplaceValueWith<F>(F);
const _: () = {
    use spirt::{transform::*, *};

    impl<F: FnMut(Value) -> Option<Value>> Transformer for ReplaceValueWith<F> {
        fn transform_value_use(&mut self, v: &Value) -> Transformed<Value> {
            self.0(*v).map_or(Transformed::Unchanged, Transformed::Changed)
        }
    }
};

/// Clean up after a pass by removing unused (pure) `Value` definitions from
/// a function body (both `DataInst`s and `Region` inputs/outputs).
//
// FIXME(eddyb) should this be a dedicated pass?
fn remove_unused_values_in_func(func_def_body: &mut FuncDefBody) {
    // Avoid having to support unstructured control-flow.
    if func_def_body.unstructured_cfg.is_some() {
        return;
    }

    let wk = &SpvSpecWithExtras::get().well_known;

    struct Propagator {
        func_body_region: Region,

        // FIXME(eddyb) maybe this kind of "parent map" should be provided by SPIR-T?
        loop_body_to_loop: EntityOrientedDenseMap<Region, Node>,

        // FIXME(eddyb) there should be a (sparse) bitset version of this.
        used: EntityOrientedDenseMap<Var, ()>,

        queue: VecDeque<Var>,
    }
    impl Propagator {
        fn mark_used(&mut self, v: Value) {
            match v {
                Value::Const(_) => {}
                Value::Var(v) => {
                    if self.used.insert(v, ()).is_none() {
                        self.queue.push_back(v);
                    }
                }
            }
        }
        fn propagate_used(&mut self, func: FuncAt<'_, ()>) {
            while let Some(v) = self.queue.pop_front() {
                match func.at(v).decl().kind() {
                    VarKind::RegionInput { region, input_idx } => {
                        // NOTE(eddyb) only the whole function's body region,
                        // and `Loop`s' bodies, can have inputs right now.
                        let Some(&loop_node) = self.loop_body_to_loop.get(region) else {
                            assert!(region == self.func_body_region);
                            continue;
                        };

                        let initial_inputs = &func.at(loop_node).def().inputs;
                        self.mark_used(initial_inputs[input_idx as usize]);
                        self.mark_used(func.at(region).def().outputs[input_idx as usize]);
                    }
                    VarKind::NodeOutput { node, output_idx } => {
                        let node_def = func.at(node).def();
                        for &input in &node_def.inputs {
                            self.mark_used(input);
                        }
                        if let NodeKind::Select(_) = node_def.kind {
                            for &case in &node_def.child_regions {
                                self.mark_used(func.at(case).def().outputs[output_idx as usize]);
                            }
                        }
                    }
                }
            }
        }
    }

    // HACK(eddyb) it's simpler to first ensure `loop_body_to_loop` is computed,
    // just to allow the later unordered propagation to always work.
    let propagator = {
        let mut visitor = VisitAllRegionsAndNodes {
            state: Propagator {
                func_body_region: func_def_body.body,
                loop_body_to_loop: Default::default(),
                used: Default::default(),
                queue: Default::default(),
            },
            enter_region: |_: &mut _, _| {},
            exit_region: |_: &mut _, _| {},
            enter_node: |propagator: &mut Propagator, func_at_node: FuncAt<'_, Node>| {
                let node_def = func_at_node.def();
                if let NodeKind::Loop { .. } = node_def.kind {
                    propagator
                        .loop_body_to_loop
                        .insert(node_def.child_regions[0], func_at_node.position);
                }
            },
            exit_node: |_: &mut _, _| {},
        };
        func_def_body.inner_visit_with(&mut visitor);
        visitor.state
    };

    // HACK(eddyb) this kind of random-access is easier than using `spirt::transform`.
    let mut all_nodes_in_region_post_order_with_parent_region = vec![];

    let used_vars = {
        let mut visitor = VisitAllRegionsAndNodes {
            state: propagator,
            enter_region: |_: &mut _, _| {},
            exit_region: |_propagator: &mut Propagator, func_at_region: FuncAt<'_, Region>| {
                // HACK(eddyb) parent region tracked for convenient removal,
                // but it might make more sense to iterate regions directly.
                let region = func_at_region.position;
                all_nodes_in_region_post_order_with_parent_region.extend(
                    func_at_region
                        .at_children()
                        .into_iter()
                        .map(|func_at_node| (func_at_node.position, region)),
                );
            },
            enter_node: |_: &mut _, _| {},
            exit_node: |propagator: &mut Propagator, func_at_node: FuncAt<'_, Node>| {
                let mut mark_used_and_propagate = |v| {
                    propagator.mark_used(v);
                    propagator.propagate_used(func_at_node.at(()));
                };
                let node_def = func_at_node.def();
                let is_pure = match &node_def.kind {
                    DataInstKind::Scalar(_)
                    | DataInstKind::Vector(_)
                    | DataInstKind::Mem(
                        MemOp::FuncLocalVar(_)
                        // HACK(eddyb) removing dead loads allows
                        // unblocking `qptr::partition_and_propagate`
                        // when the load doesn't fit a previous store.
                        | MemOp::Load { .. },
                    )
                    | DataInstKind::QPtr(
                        QPtrOp::HandleArrayIndex
                        | QPtrOp::BufferData
                        | QPtrOp::BufferDynLen { .. }
                        | QPtrOp::Offset(_)
                        | QPtrOp::DynOffset { .. },
                    ) => true,

                    // HACK(eddyb) small selection relevant for now,
                    // but should be extended using e.g. a bitset.
                    DataInstKind::SpvInst(spv_inst, _) => {
                        [wk.OpNop, wk.OpSelect, wk.OpBitcast].contains(&spv_inst.opcode)
                    }

                    NodeKind::Select { .. }
                    | NodeKind::Loop { .. }
                    | NodeKind::ExitInvocation(spirt::cf::ExitInvocationKind::SpvInst(_))
                    | DataInstKind::FuncCall(_)
                    | DataInstKind::Mem(MemOp::Store { .. })
                    | DataInstKind::ThunkBind(_)
                    | DataInstKind::SpvExtInst { .. } => false,
                };
                // Ignore pure instructions (i.e. they're only used
                // if their output value is used, from somewhere else).
                if !is_pure {
                    // HACK(eddyb) `Select` nodes propagate through to their
                    // cases, so we don't want to do that.
                    if let NodeKind::Select(_) = node_def.kind {
                        for &v in &node_def.inputs {
                            mark_used_and_propagate(v);
                        }
                    } else if node_def.outputs.is_empty() {
                        // FIXME(eddyb) this is an utter mess, made worse
                        // by loop nodes not being enough like RVSDG.
                        if let NodeKind::Loop { repeat_condition } = node_def.kind {
                            mark_used_and_propagate(repeat_condition);
                        } else {
                            // HACK(eddyb) still need to mark the instruction's
                            // inputs as used, while it has no output `Value`.
                            for &input in &node_def.inputs {
                                mark_used_and_propagate(input);
                            }
                        }
                    } else {
                        for &output_var in &node_def.outputs {
                            mark_used_and_propagate(Value::Var(output_var));
                        }
                    }
                }
            },
        };
        func_def_body.inner_visit_with(&mut visitor);

        let mut propagator = visitor.state;
        for &v in &func_def_body.at_body().def().outputs {
            propagator.mark_used(v);
            propagator.propagate_used(func_def_body.at(()));
        }

        assert!(propagator.queue.is_empty());
        propagator.used
    };

    // Remove anything that didn't end up marked as used (directly or indirectly).
    for (node, parent_region) in all_nodes_in_region_post_order_with_parent_region {
        let mut remove_node = false;

        let func = func_def_body.at_mut(());
        let node_def = &mut func.nodes[node];
        match &node_def.kind {
            NodeKind::Select(_) | NodeKind::Loop { .. } => {
                let vars = match node_def.kind {
                    NodeKind::Select(_) => &mut node_def.outputs,
                    NodeKind::Loop { .. } => &mut func.regions[node_def.child_regions[0]].inputs,
                    _ => unreachable!(),
                };

                fn indexed_retain<T, const N: usize>(
                    v: &mut SmallVec<[T; N]>,
                    mut f: impl FnMut(usize, &T) -> bool,
                ) {
                    let mut indices = 0..;
                    v.retain(|x| f(indices.next().unwrap(), x));
                }

                let mut removed_vars = None;
                let original_var_count = vars.len();
                indexed_retain(vars, |var_idx, &var| {
                    let used = used_vars.get(var).is_some();
                    if !used {
                        removed_vars
                            .get_or_insert_with(|| BitSet::new_empty(original_var_count))
                            .insert(var_idx);
                    }
                    used
                });

                // Only update `VarDecl`s and `Value`s if any `Var`s were removed.
                if let Some(removed_vars) = removed_vars {
                    for (var_idx, &var) in vars.iter().enumerate() {
                        func.vars[var].def_idx = var_idx.try_into().unwrap();
                    }

                    let prune_values =
                        |values: &mut _| indexed_retain(values, |i, _| !removed_vars.contains(i));
                    if let NodeKind::Loop { .. } = node_def.kind {
                        prune_values(&mut node_def.inputs);
                    }
                    for &child_region in &node_def.child_regions {
                        prune_values(&mut func.regions[child_region].outputs);
                    }
                }

                // FIXME(eddyb) reacting to empty regions which *could* be the
                // result of node removals (performed earlier in this pass) means
                // this could cause even more value definitions to become unused
                // (specifically the `scrutinee` of the `Select`), but that can't
                // be easily detected without a second pass over the function,
                // *or* propagating and removing at the same time, which would
                // require iterating in reverse, to visit all possible uses of
                // a definition, before the definition itself.
                if let NodeKind::Select(_) = node_def.kind {
                    let all_cases_empty = node_def.child_regions.iter().all(|&case| {
                        let case_def = &func.regions[case];
                        case_def.outputs.is_empty() && case_def.children.is_empty()
                    });

                    remove_node = all_cases_empty;
                }
            }

            NodeKind::ExitInvocation { .. } => {}

            DataInstKind::Scalar(_)
            | DataInstKind::Vector(_)
            | DataInstKind::FuncCall(_)
            | DataInstKind::Mem(_)
            | DataInstKind::QPtr(_)
            | DataInstKind::ThunkBind(_)
            | DataInstKind::SpvInst { .. }
            | DataInstKind::SpvExtInst { .. } => {
                let used = match &node_def.kind {
                    DataInstKind::SpvInst(spv_inst, _) if spv_inst.opcode == wk.OpNop => false,

                    _ => {
                        node_def.outputs.is_empty()
                            || node_def
                                .outputs
                                .iter()
                                .any(|&output_var| used_vars.get(output_var).is_some())
                    }
                };
                remove_node = !used;
            }
        }
        if remove_node {
            func.regions[parent_region]
                .children
                .remove(node, func.nodes);

            // HACK(eddyb) there isn't a good "dummy" to use here,
            // but really the only thing we care about is no
            // heap allocations remain around.
            *func_def_body.at_mut(node).def() = NodeDef {
                attrs: Default::default(),
                kind: NodeKind::Select(SelectionKind::BoolCond),
                inputs: Default::default(),
                child_regions: Default::default(),
                outputs: Default::default(),
            };
        }
    }
}
