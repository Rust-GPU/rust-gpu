use super::{get_name, get_names};
use rspirv::dr::{Block, Function, Module};
use rspirv::spirv::{Decoration, ExecutionModel, Op, Word};
use rustc_codegen_spirv_types::Capability;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_session::Session;
use std::iter::once;
use std::mem::take;

/// Error marker type, indicating an integer/float type SPIR-V lacks support for.
struct UnsupportedType;

/// Returns the capability required for an integer type of the given width, if any.
fn capability_for_int_width(
    width: u32,
) -> Result<Option<rspirv::spirv::Capability>, UnsupportedType> {
    Ok(match width {
        8 => Some(rspirv::spirv::Capability::Int8),
        16 => Some(rspirv::spirv::Capability::Int16),
        32 => None,
        64 => Some(rspirv::spirv::Capability::Int64),
        _ => return Err(UnsupportedType),
    })
}

/// Returns the capability required for a float type of the given width, if any.
fn capability_for_float_width(
    width: u32,
) -> Result<Option<rspirv::spirv::Capability>, UnsupportedType> {
    Ok(match width {
        16 => Some(rspirv::spirv::Capability::Float16),
        32 => None,
        64 => Some(rspirv::spirv::Capability::Float64),
        _ => return Err(UnsupportedType),
    })
}

pub fn shift_ids(module: &mut Module, add: u32) {
    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(result_id) = &mut inst.result_id {
            *result_id += add;
        }

        if let Some(result_type) = &mut inst.result_type {
            *result_type += add;
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = op.id_ref_any_mut() {
                *w += add;
            }
        });
    });
}

/// spir-v requires basic blocks to be ordered so that if A dominates B, A appears before B (except
/// in the case of backedges). Reverse post-order is a good ordering that satisfies this condition
/// (with an "already visited set" that blocks going deeper, which solves both the fact that it's a
/// DAG, not a tree, as well as backedges).
pub fn block_ordering_pass(func: &mut Function) {
    if func.blocks.len() < 2 {
        return;
    }
    fn visit_postorder(
        func: &Function,
        visited: &mut FxHashSet<Word>,
        postorder: &mut Vec<Word>,
        current: Word,
    ) {
        if !visited.insert(current) {
            return;
        }
        let current_block = func
            .blocks
            .iter()
            .find(|b| b.label_id().unwrap() == current)
            .unwrap();
        let mut edges = outgoing_edges(current_block).collect::<Vec<_>>();
        // HACK(eddyb) treat `OpSelectionMerge` as an edge, in case it points
        // to an otherwise-unreachable block.
        if let Some(before_last_idx) = current_block.instructions.len().checked_sub(2)
            && let Some(before_last) = current_block.instructions.get(before_last_idx)
            && before_last.class.opcode == Op::SelectionMerge
        {
            edges.push(before_last.operands[0].unwrap_id_ref());
        }
        // Reverse the order, so reverse-postorder keeps things tidy
        for &outgoing in edges.iter().rev() {
            visit_postorder(func, visited, postorder, outgoing);
        }
        postorder.push(current);
    }

    let mut visited = FxHashSet::default();
    let mut postorder = Vec::new();

    let entry_label = func.blocks[0].label_id().unwrap();
    visit_postorder(func, &mut visited, &mut postorder, entry_label);

    let mut old_blocks = take(&mut func.blocks);
    // Order blocks according to reverse postorder
    for &block in postorder.iter().rev() {
        let index = old_blocks
            .iter()
            .position(|b| b.label_id().unwrap() == block)
            .unwrap();
        func.blocks.push(old_blocks.remove(index));
    }
    // Note: if old_blocks isn't empty here, that means there were unreachable blocks that were deleted.
    assert_eq!(func.blocks[0].label_id().unwrap(), entry_label);
}

pub fn outgoing_edges(block: &Block) -> impl Iterator<Item = Word> + '_ {
    let terminator = block.instructions.last().unwrap();
    // https://www.khronos.org/registry/spir-v/specs/unified1/SPIRV.html#Termination
    let operand_indices = match terminator.class.opcode {
        Op::Branch => (0..1).step_by(1),
        Op::BranchConditional => (1..3).step_by(1),
        Op::Switch => (1..terminator.operands.len()).step_by(2),
        Op::Return
        | Op::ReturnValue
        | Op::Kill
        | Op::Unreachable
        | Op::IgnoreIntersectionKHR
        | Op::TerminateRayKHR
        | Op::EmitMeshTasksEXT => (0..0).step_by(1),
        _ => panic!("Invalid block terminator: {terminator:?}"),
    };
    operand_indices.map(move |i| terminator.operands[i].unwrap_id_ref())
}

pub fn compact_ids(module: &mut Module) -> u32 {
    let mut remap = FxHashMap::default();

    let mut insert = |current_id: u32| -> u32 {
        let len = remap.len();
        *remap.entry(current_id).or_insert_with(|| len as u32 + 1)
    };

    module.all_inst_iter_mut().for_each(|inst| {
        if let Some(result_id) = &mut inst.result_id {
            *result_id = insert(*result_id);
        }

        if let Some(result_type) = &mut inst.result_type {
            *result_type = insert(*result_type);
        }

        inst.operands.iter_mut().for_each(|op| {
            if let Some(w) = op.id_ref_any_mut() {
                *w = insert(*w);
            }
        });
    });

    remap.len() as u32 + 1
}

pub fn sort_globals(module: &mut Module) {
    // Function declarations come before definitions. TODO: Figure out if it's even possible to
    // have a function declaration without a body in a fully linked module?
    module.functions.sort_by_key(|f| !f.blocks.is_empty());
}

pub fn name_variables_pass(module: &mut Module) {
    let variables = module
        .types_global_values
        .iter()
        .filter(|inst| inst.class.opcode == Op::Variable)
        .map(|inst| inst.result_id.unwrap())
        .collect::<FxHashSet<Word>>();
    module
        .debug_names
        .retain(|inst| variables.contains(&inst.operands[0].unwrap_id_ref()));
    // FIXME(eddyb) why does this remove `OpLine` instructions?
    module
        .types_global_values
        .retain(|inst| inst.class.opcode != Op::Line);
    for func in &mut module.functions {
        for block in &mut func.blocks {
            block
                .instructions
                .retain(|inst| inst.class.opcode != Op::Line);
        }
    }
}

// Some instructions are only valid in fragment shaders. Check them.
pub fn check_fragment_insts(sess: &Session, module: &Module) -> super::Result<()> {
    let mut visited = vec![false; module.functions.len()];
    let mut stack = Vec::new();
    let mut names = None;
    let func_id_to_idx: FxHashMap<Word, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(index, func)| (func.def_id().unwrap(), index))
        .collect();
    let entries = module
        .entry_points
        .iter()
        .filter(|i| i.operands[0].unwrap_execution_model() != ExecutionModel::Fragment)
        .map(|i| func_id_to_idx[&i.operands[1].unwrap_id_ref()]);
    let mut any_err = None;
    for entry in entries {
        let entry_had_err = visit(
            sess,
            module,
            &mut visited,
            &mut stack,
            &mut names,
            entry,
            &func_id_to_idx,
        )
        .err();
        any_err = any_err.or(entry_had_err);
    }
    return match any_err {
        Some(err) => Err(err),
        None => Ok(()),
    };

    fn visit<'m>(
        sess: &Session,
        module: &'m Module,
        visited: &mut Vec<bool>,
        stack: &mut Vec<Word>,
        names: &mut Option<FxHashMap<Word, &'m str>>,
        index: usize,
        func_id_to_idx: &FxHashMap<Word, usize>,
    ) -> super::Result<()> {
        if visited[index] {
            return Ok(());
        }
        visited[index] = true;
        stack.push(module.functions[index].def_id().unwrap());
        let mut any_err = None;
        for inst in module.functions[index].all_inst_iter() {
            if inst.class.opcode == Op::FunctionCall {
                let callee = func_id_to_idx[&inst.operands[0].unwrap_id_ref()];
                let callee_had_err =
                    visit(sess, module, visited, stack, names, callee, func_id_to_idx).err();
                any_err = any_err.or(callee_had_err);
            }
            if matches!(
                inst.class.opcode,
                Op::ImageSampleImplicitLod
                    | Op::ImageSampleDrefImplicitLod
                    | Op::ImageSampleProjImplicitLod
                    | Op::ImageSampleProjDrefImplicitLod
                    | Op::ImageQueryLod
                    | Op::ImageSparseSampleImplicitLod
                    | Op::ImageSparseSampleDrefImplicitLod
                    | Op::DPdx
                    | Op::DPdy
                    | Op::Fwidth
                    | Op::DPdxFine
                    | Op::DPdyFine
                    | Op::FwidthFine
                    | Op::DPdxCoarse
                    | Op::DPdyCoarse
                    | Op::FwidthCoarse
                    | Op::Kill
            ) {
                // These instructions are (usually) in system functions - if we get an error, allow
                // the system function to be visited again from elsewhere to emit another error
                // from another callsite.
                visited[index] = false;

                let names = names.get_or_insert_with(|| get_names(module));
                let stack = stack.iter().rev().map(|&s| get_name(names, s).into_owned());
                let note = once("Stack:".to_string())
                    .chain(stack)
                    .collect::<Vec<_>>()
                    .join("\n");
                any_err = Some(
                    sess.dcx()
                        .struct_err(format!(
                            "{} cannot be used outside a fragment shader",
                            inst.class.opname
                        ))
                        .with_note(note)
                        .emit(),
                );
            }
        }
        stack.pop();

        match any_err {
            Some(err) => Err(err),
            None => Ok(()),
        }
    }
}

/// Remove type-related capabilities that are not required by any types in the module.
///
/// This function specifically targets Int8, Int16, Int64, Float16, and Float64 capabilities,
/// removing them if no types in the module require them. All other capabilities are preserved.
/// This is part of the fix for issue #300 where constant casts were creating unnecessary types.
//
// FIXME(eddyb) move this to a SPIR-T pass (potentially even using sets of used
// exts/caps that validation itself can collect while traversing the module).
pub fn remove_unused_type_capabilities(module: &mut Module) {
    use rspirv::spirv::Capability;

    // Collect type-related capabilities that are actually needed
    let mut needed_type_capabilities = FxHashSet::default();

    // Scan all types to determine which type-related capabilities are needed
    for inst in &module.types_global_values {
        match inst.class.opcode {
            Op::TypeInt => {
                let width = inst.operands[0].unwrap_literal_bit32();
                if let Ok(Some(cap)) = capability_for_int_width(width) {
                    needed_type_capabilities.insert(cap);
                }
            }
            Op::TypeFloat => {
                let width = inst.operands[0].unwrap_literal_bit32();
                if let Ok(Some(cap)) = capability_for_float_width(width) {
                    needed_type_capabilities.insert(cap);
                }
            }
            _ => {}
        }
    }

    // Remove only type-related capabilities that aren't needed
    module.capabilities.retain(|inst| {
        let cap = inst.operands[0].unwrap_capability();
        match cap {
            // Only remove these type-related capabilities if they're not used
            Capability::Int8
            | Capability::Int16
            | Capability::Int64
            | Capability::Float16
            | Capability::Float64 => needed_type_capabilities.contains(&cap),
            // Keep all other capabilities
            _ => true,
        }
    });
}

/// Remove all [`Decoration::NonUniform`] if this module does *not* have [`Capability::ShaderNonUniform`].
/// This allows image asm to always declare `NonUniform` and not worry about conditional compilation.
pub fn remove_non_uniform_decorations(_sess: &Session, module: &mut Module) -> super::Result<()> {
    let has_shader_non_uniform_capability = module.capabilities.iter().any(|inst| {
        inst.class.opcode == Op::Capability
            && inst.operands[0].unwrap_capability() == Capability::ShaderNonUniform
    });
    if !has_shader_non_uniform_capability {
        module.annotations.retain(|inst| {
            !(inst.class.opcode == Op::Decorate
                && inst.operands[1].unwrap_decoration() == Decoration::NonUniform)
        });
    }
    Ok(())
}
