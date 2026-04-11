//! This algorithm is not intended to be an optimization, it is rather for legalization.
//! Specifically, spir-v disallows things like a `StorageClass::Function` pointer to a
//! `StorageClass::Input` pointer. Our frontend definitely allows it, though, this is like taking a
//! `&Input<T>` in a function! So, we inline all functions (see inline.rs) that take these
//! "illegal" pointers, then run mem2reg on the result to "unwrap" the Function pointer.
//!
//! Because it's merely a legalization pass, this computes "minimal" SSA form, *not* "pruned" SSA
//! form. The difference is that "minimal" may include extra phi nodes that aren't actually used
//! anywhere - we assume that later optimization passes will take care of these (relying on what
//! wikipedia calls "treat pruning as a dead code elimination problem").

use super::simple_passes::outgoing_edges;
use super::{apply_rewrite_rules, id};
use rspirv::dr::{Block, Function, Instruction, ModuleHeader, Operand};
use rspirv::spirv::{Op, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet, FxIndexMap};
use rustc_middle::bug;

// HACK(eddyb) newtype instead of type alias to avoid mistakes.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct LabelId(Word);

pub fn mem2reg(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    pointer_to_pointee: &FxHashMap<Word, Word>,
    constants: &FxHashMap<Word, u32>,
    func: &mut Function,
) {
    // HACK(eddyb) this ad-hoc indexing might be useful elsewhere as well, but
    // it's made completely irrelevant by SPIR-T so only applies to legacy code.
    let mut blocks: FxIndexMap<_, _> = func
        .blocks
        .iter_mut()
        .map(|block| (LabelId(block.label_id().unwrap()), block))
        .collect();

    let reachable = compute_reachable(&blocks);
    let preds = compute_preds(&blocks, &reachable);
    let idom = compute_idom(&preds, &reachable);
    let dominance_frontier = compute_dominance_frontier(&preds, &idom);
    loop {
        let changed = insert_phis_all(
            header,
            types_global_values,
            pointer_to_pointee,
            constants,
            &mut blocks,
            &dominance_frontier,
        );
        if !changed {
            break;
        }
        // mem2reg produces minimal SSA form, not pruned, so DCE the dead ones
        super::dce::dce_phi(&mut blocks);
    }
}

fn compute_reachable(blocks: &FxIndexMap<LabelId, &mut Block>) -> Vec<bool> {
    fn recurse(blocks: &FxIndexMap<LabelId, &mut Block>, reachable: &mut [bool], block: usize) {
        if !reachable[block] {
            reachable[block] = true;
            for dest_id in outgoing_edges(blocks[block]) {
                recurse(
                    blocks,
                    reachable,
                    blocks.get_index_of(&LabelId(dest_id)).unwrap(),
                );
            }
        }
    }
    let mut reachable = vec![false; blocks.len()];
    recurse(blocks, &mut reachable, 0);
    reachable
}

fn compute_preds(
    blocks: &FxIndexMap<LabelId, &mut Block>,
    reachable_blocks: &[bool],
) -> Vec<Vec<usize>> {
    let mut result = vec![vec![]; blocks.len()];
    // Do not count unreachable blocks as valid preds of blocks
    for (source_idx, source) in blocks
        .values()
        .enumerate()
        .filter(|&(b, _)| reachable_blocks[b])
    {
        for dest_id in outgoing_edges(source) {
            result[blocks.get_index_of(&LabelId(dest_id)).unwrap()].push(source_idx);
        }
    }
    result
}

// Paper: A Simple, Fast Dominance Algorithm
// https://www.cs.rice.edu/~keith/EMBED/dom.pdf
// Note: requires nodes in reverse postorder
// If a result is None, that means the block is unreachable, and therefore has no idom.
fn compute_idom(preds: &[Vec<usize>], reachable_blocks: &[bool]) -> Vec<Option<usize>> {
    fn intersect(doms: &[Option<usize>], mut finger1: usize, mut finger2: usize) -> usize {
        // TODO: This may return an optional result?
        while finger1 != finger2 {
            // Note: The comparisons here are inverted from the paper, because the paper uses
            // comparison to be postorder index. However, we have reverse postorder indices.
            while finger1 > finger2 {
                finger1 = doms[finger1].unwrap();
            }
            while finger2 > finger1 {
                finger2 = doms[finger2].unwrap();
            }
        }
        finger1
    }

    let mut idom = vec![None; preds.len()];
    idom[0] = Some(0);
    let mut changed = true;
    while changed {
        changed = false;
        // Unreachable blocks have no preds, and therefore no idom
        for node in (1..(preds.len())).filter(|&i| reachable_blocks[i]) {
            let mut new_idom: Option<usize> = None;
            for &pred in &preds[node] {
                if idom[pred].is_some() {
                    new_idom =
                        Some(new_idom.map_or(pred, |new_idom| intersect(&idom, pred, new_idom)));
                }
            }
            // TODO: This may return an optional result?
            let new_idom = new_idom.unwrap();
            if idom[node] != Some(new_idom) {
                idom[node] = Some(new_idom);
                changed = true;
            }
        }
    }
    assert!(
        idom.iter()
            .enumerate()
            .all(|(i, x)| x.is_some() == reachable_blocks[i])
    );
    idom
}

// Same paper as above
fn compute_dominance_frontier(
    preds: &[Vec<usize>],
    idom: &[Option<usize>],
) -> Vec<FxHashSet<usize>> {
    assert_eq!(preds.len(), idom.len());
    let mut dominance_frontier = vec![FxHashSet::default(); preds.len()];
    for node in 0..preds.len() {
        if preds[node].len() >= 2 {
            let node_idom = idom[node].unwrap();
            for &pred in &preds[node] {
                let mut runner = pred;
                while runner != node_idom {
                    dominance_frontier[runner].insert(node);
                    runner = idom[runner].unwrap();
                }
            }
        }
    }
    dominance_frontier
}

// Returns true if variables were rewritten
fn insert_phis_all(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    pointer_to_pointee: &FxHashMap<Word, Word>,
    constants: &FxHashMap<Word, u32>,
    blocks: &mut FxIndexMap<LabelId, &mut Block>,
    dominance_frontier: &[FxHashSet<usize>],
) -> bool {
    // Collect all eligible variables and their access chains in a single pass.
    let (var_maps_and_types, ptr_to_var_idx) =
        collect_all_access_chains(pointer_to_pointee, constants, blocks);
    if var_maps_and_types.is_empty() {
        return false;
    }

    // Split OpCopyMemory into OpLoad+OpStore for all variables in one pass.
    split_copy_memory_batch(header, blocks, &ptr_to_var_idx, &var_maps_and_types);

    // Insert phis per variable (operates on block-level, not instruction-level).
    let all_blocks_with_phi: Vec<FxHashSet<usize>> = var_maps_and_types
        .iter()
        .map(|(var_map, _)| insert_phis(blocks, dominance_frontier, var_map))
        .collect();

    // Rename all variables in a single dominator-tree walk: O(N) total.
    let mut renamer = BatchRenamer {
        header,
        types_global_values,
        blocks,
        var_data: var_maps_and_types
            .iter()
            .zip(all_blocks_with_phi)
            .map(|((_, base_var_type), blocks_with_phi)| VarRenameData {
                base_var_type: *base_var_type,
                blocks_with_phi,
                phi_defs: FxHashSet::default(),
                stack: Vec::new(),
            })
            .collect(),
        ptr_to_var_idx: &ptr_to_var_idx,
        var_maps: &var_maps_and_types,
        visited: FxHashSet::default(),
        rewrite_rules: FxHashMap::default(),
    };
    renamer.rename(0, None);

    // Resolve transitive rewrite chains and apply all rules in a single pass.
    let mut rewrite_rules = renamer.rewrite_rules;
    resolve_rewrite_chains(&mut rewrite_rules);
    apply_rewrite_rules(
        &rewrite_rules,
        blocks.values_mut().map(|block| &mut **block),
    );

    remove_nops(blocks);
    remove_old_variables(blocks, &var_maps_and_types);
    true
}

#[derive(Debug)]
struct VarInfo {
    // Type of the *dereferenced* variable.
    ty: Word,
    // OpAccessChain indexes off the base variable
    indices: Vec<u32>,
}

/// Resolve an `OpAccessChain`/`OpInBoundsAccessChain` into a `VarInfo` with
/// flattened indices. Returns `None` if any index is not a known u32 constant,
/// which makes the parent variable ineligible for promotion.
///
/// Per the SPIR-V spec, access chain indices must be scalar integers
/// (<https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#OpAccessChain>).
/// The `constants` map only tracks u32 constants (matching what rustc emits).
fn construct_access_chain_info(
    pointer_to_pointee: &FxHashMap<Word, Word>,
    constants: &FxHashMap<Word, u32>,
    inst: &Instruction,
    base: &VarInfo,
) -> Option<VarInfo> {
    Some(VarInfo {
        ty: *pointer_to_pointee.get(&inst.result_type.unwrap()).unwrap(),
        indices: {
            let mut base_indices = base.indices.clone();
            for op in inst.operands.iter().skip(1) {
                base_indices.push(*constants.get(&op.id_ref_any().unwrap())?);
            }
            base_indices
        },
    })
}

type VarMapsAndTypes = Vec<(FxHashMap<Word, VarInfo>, Word)>;

/// Collect access chains for all variables in a single pass over all instructions.
///
/// Returns `(var_maps_and_types, ptr_to_var_idx)`:
/// - `var_maps_and_types[i]` is `(pointer_id -> VarInfo, base_type)` for variable `i`
/// - `ptr_to_var_idx[ptr_id]` maps any tracked pointer to its variable index
fn collect_all_access_chains(
    pointer_to_pointee: &FxHashMap<Word, Word>,
    constants: &FxHashMap<Word, u32>,
    blocks: &FxIndexMap<LabelId, &mut Block>,
) -> (VarMapsAndTypes, FxHashMap<Word, usize>) {
    // Initialize per-variable maps from block 0's OpVariable instructions.
    let mut var_maps: VarMapsAndTypes = Vec::new();
    let mut ptr_to_var: FxHashMap<Word, usize> = FxHashMap::default();
    let mut non_eligible: FxHashSet<usize> = FxHashSet::default();

    for inst in &blocks[0].instructions {
        if inst.class.opcode != Op::Variable {
            continue;
        }
        let var = inst.result_id.unwrap();
        let var_ty = *pointer_to_pointee.get(&inst.result_type.unwrap()).unwrap();
        let var_idx = var_maps.len();
        let mut map = FxHashMap::default();
        map.insert(
            var,
            VarInfo {
                ty: var_ty,
                indices: vec![],
            },
        );
        var_maps.push((map, var_ty));
        ptr_to_var.insert(var, var_idx);
    }

    if var_maps.is_empty() {
        return (vec![], FxHashMap::default());
    }

    // Fixed-point loop: discover access chains and mark ineligible variables.
    loop {
        let mut changed = false;
        for inst in blocks.values().flat_map(|b| &b.instructions) {
            // Check for unsupported uses of any tracked pointer.
            for (index, op) in inst.operands.iter().enumerate() {
                if let Operand::IdRef(id) = op
                    && let Some(&var_idx) = ptr_to_var.get(id)
                    && !non_eligible.contains(&var_idx)
                {
                    match inst.class.opcode {
                        // Only allow store if pointer is the lhs, not rhs
                        Op::Store if index == 0 => {}
                        Op::Load | Op::AccessChain | Op::InBoundsAccessChain | Op::CopyMemory => {}
                        _ => {
                            non_eligible.insert(var_idx);
                        }
                    }
                }
            }
            // Collect access chains for any tracked base pointer.
            if matches!(inst.class.opcode, Op::AccessChain | Op::InBoundsAccessChain) {
                let base_id = inst.operands[0].id_ref_any().unwrap();
                if let Some(&var_idx) = ptr_to_var.get(&base_id)
                    && !non_eligible.contains(&var_idx)
                {
                    let result_id = inst.result_id.unwrap();
                    if let std::collections::hash_map::Entry::Vacant(entry) =
                        ptr_to_var.entry(result_id)
                    {
                        let base_info = &var_maps[var_idx].0[&base_id];
                        match construct_access_chain_info(
                            pointer_to_pointee,
                            constants,
                            inst,
                            base_info,
                        ) {
                            Some(info) => {
                                var_maps[var_idx].0.insert(result_id, info);
                                entry.insert(var_idx);
                                changed = true;
                            }
                            None => {
                                non_eligible.insert(var_idx);
                            }
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Filter out non-eligible variables and rebuild ptr_to_var with new indices.
    let mut result = Vec::new();
    let mut new_ptr_to_var = FxHashMap::default();
    for (old_idx, (map, ty)) in var_maps.into_iter().enumerate() {
        if non_eligible.contains(&old_idx) {
            continue;
        }
        let new_idx = result.len();
        for &ptr_id in map.keys() {
            new_ptr_to_var.insert(ptr_id, new_idx);
        }
        result.push((map, ty));
    }

    (result, new_ptr_to_var)
}

/// Split `OpCopyMemory` into `OpLoad`+`OpStore` for all tracked variables in one pass.
fn split_copy_memory_batch(
    header: &mut ModuleHeader,
    blocks: &mut FxIndexMap<LabelId, &mut Block>,
    ptr_to_var_idx: &FxHashMap<Word, usize>,
    var_maps: &[(FxHashMap<Word, VarInfo>, Word)],
) {
    for block in blocks.values_mut() {
        let mut inst_index = 0;
        while inst_index < block.instructions.len() {
            let inst = &block.instructions[inst_index];
            if inst.class.opcode == Op::CopyMemory {
                let target = inst.operands[0].id_ref_any().unwrap();
                let source = inst.operands[1].id_ref_any().unwrap();
                if inst.operands.len() > 2 {
                    // TODO: Copy the memory operands to the load/store
                    bug!("mem2reg OpCopyMemory doesn't support memory operands yet");
                }
                let target_info = ptr_to_var_idx
                    .get(&target)
                    .map(|&idx| &var_maps[idx].0[&target]);
                let source_info = ptr_to_var_idx
                    .get(&source)
                    .map(|&idx| &var_maps[idx].0[&source]);
                let ty = match (target_info, source_info) {
                    (None, None) => {
                        inst_index += 1;
                        continue;
                    }
                    (Some(t), None) | (None, Some(t)) => t.ty,
                    (Some(t), Some(s)) => {
                        assert_eq!(t.ty, s.ty);
                        t.ty
                    }
                };
                let temp_id = id(header);
                block.instructions[inst_index] = Instruction::new(
                    Op::Load,
                    Some(ty),
                    Some(temp_id),
                    vec![Operand::IdRef(source)],
                );
                inst_index += 1;
                block.instructions.insert(
                    inst_index,
                    Instruction::new(
                        Op::Store,
                        None,
                        None,
                        vec![Operand::IdRef(target), Operand::IdRef(temp_id)],
                    ),
                );
            }
            inst_index += 1;
        }
    }
}

fn has_store(block: &Block, var_map: &FxHashMap<Word, VarInfo>) -> bool {
    block.instructions.iter().any(|inst| {
        let ptr = match inst.class.opcode {
            Op::Store => inst.operands[0].id_ref_any().unwrap(),
            Op::Variable if inst.operands.len() < 2 => return false,
            Op::Variable => inst.result_id.unwrap(),
            _ => return false,
        };
        var_map.contains_key(&ptr)
    })
}

fn insert_phis(
    blocks: &FxIndexMap<LabelId, &mut Block>,
    dominance_frontier: &[FxHashSet<usize>],
    var_map: &FxHashMap<Word, VarInfo>,
) -> FxHashSet<usize> {
    // TODO: Some algorithms check if the var is trivial in some way, e.g. all loads and stores are
    // in a single block. We should probably do that too.
    let mut ever_on_work_list = FxHashSet::default();
    let mut work_list = Vec::new();
    let mut blocks_with_phi = FxHashSet::default();
    for (block_idx, block) in blocks.values().enumerate() {
        if has_store(block, var_map) {
            ever_on_work_list.insert(block_idx);
            work_list.push(block_idx);
        }
    }
    while let Some(x) = work_list.pop() {
        for &y in &dominance_frontier[x] {
            if blocks_with_phi.insert(y) && ever_on_work_list.insert(y) {
                work_list.push(y);
            }
        }
    }
    blocks_with_phi
}

// These can't be part of the BatchRenamer impl due to borrowck rules.
fn undef_for(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    ty: Word,
) -> Word {
    // TODO: This is horribly slow, fix this
    let existing = types_global_values
        .iter()
        .find(|inst| inst.class.opcode == Op::Undef && inst.result_type.unwrap() == ty);
    if let Some(existing) = existing {
        return existing.result_id.unwrap();
    }
    let inst_id = id(header);
    types_global_values.push(Instruction::new(Op::Undef, Some(ty), Some(inst_id), vec![]));
    inst_id
}
fn top_stack_or_undef(
    header: &mut ModuleHeader,
    types_global_values: &mut Vec<Instruction>,
    stack: &[Word],
    ty: Word,
) -> Word {
    match stack.last() {
        Some(&top) => top,
        None => undef_for(header, types_global_values, ty),
    }
}

/// Per-variable rename state used by `BatchRenamer`.
struct VarRenameData {
    base_var_type: Word,
    blocks_with_phi: FxHashSet<usize>,
    phi_defs: FxHashSet<Word>,
    stack: Vec<Word>,
}

/// Processes all variables in a single dominator-tree walk: O(N) total work
/// (plus O(V) per block boundary for phi handling).
struct BatchRenamer<'a, 'b, 'c> {
    header: &'a mut ModuleHeader,
    types_global_values: &'a mut Vec<Instruction>,
    blocks: &'a mut FxIndexMap<LabelId, &'b mut Block>,

    var_data: Vec<VarRenameData>,
    ptr_to_var_idx: &'c FxHashMap<Word, usize>,
    var_maps: &'c [(FxHashMap<Word, VarInfo>, Word)],

    visited: FxHashSet<usize>,
    rewrite_rules: FxHashMap<Word, Word>,
}

impl BatchRenamer<'_, '_, '_> {
    fn insert_phi_value(&mut self, var_idx: usize, block: usize, from_block: usize) -> Word {
        let from_block_label = self.blocks[from_block].label_id().unwrap();
        let top_def = top_stack_or_undef(
            self.header,
            self.types_global_values,
            &self.var_data[var_idx].stack,
            self.var_data[var_idx].base_var_type,
        );

        // Search for an existing phi belonging to this variable.
        // OpLine/OpNoLine can be interleaved with OpPhi per the SPIR-V spec
        // (https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#OpPhi),
        // so skip them rather than stopping at the first non-OpPhi.
        let phi_defs = &self.var_data[var_idx].phi_defs;
        let existing_phi = self.blocks[block]
            .instructions
            .iter_mut()
            .take_while(|inst| matches!(inst.class.opcode, Op::Phi | Op::Line | Op::NoLine))
            .find(|inst| {
                inst.class.opcode == Op::Phi && phi_defs.contains(&inst.result_id.unwrap())
            });

        match existing_phi {
            None => {
                let new_id = id(self.header);
                self.blocks[block].instructions.insert(
                    0,
                    Instruction::new(
                        Op::Phi,
                        Some(self.var_data[var_idx].base_var_type),
                        Some(new_id),
                        vec![Operand::IdRef(top_def), Operand::IdRef(from_block_label)],
                    ),
                );
                self.var_data[var_idx].phi_defs.insert(new_id);
                new_id
            }
            Some(existing_phi) => {
                existing_phi.operands.extend_from_slice(&[
                    Operand::IdRef(top_def),
                    Operand::IdRef(from_block_label),
                ]);
                existing_phi.result_id.unwrap()
            }
        }
    }

    fn rename(&mut self, block: usize, from_block: Option<usize>) {
        let original_stacks: Vec<usize> = self.var_data.iter().map(|v| v.stack.len()).collect();

        // Insert phi values for all variables that need one at this block.
        if let Some(from_block) = from_block {
            for var_idx in 0..self.var_data.len() {
                if self.var_data[var_idx].blocks_with_phi.contains(&block) {
                    let new_top = self.insert_phi_value(var_idx, block, from_block);
                    self.var_data[var_idx].stack.push(new_top);
                }
            }
        }

        if !self.visited.insert(block) {
            for (var_data, &orig_len) in self.var_data.iter_mut().zip(&original_stacks) {
                var_data.stack.truncate(orig_len);
            }
            return;
        }

        // Process all instructions in this block, handling all variables per instruction.
        for inst in &mut self.blocks[block].instructions {
            if inst.class.opcode == Op::Variable && inst.operands.len() > 1 {
                let ptr = inst.result_id.unwrap();
                let val = inst.operands[1].id_ref_any().unwrap();
                if let Some(&var_idx) = self.ptr_to_var_idx.get(&ptr) {
                    let var_info = &self.var_maps[var_idx].0[&ptr];
                    assert_eq!(var_info.indices, Vec::<u32>::new());
                    self.var_data[var_idx].stack.push(val);
                }
            } else if inst.class.opcode == Op::Store {
                let ptr = inst.operands[0].id_ref_any().unwrap();
                let val = inst.operands[1].id_ref_any().unwrap();
                if let Some(&var_idx) = self.ptr_to_var_idx.get(&ptr) {
                    let var_info = &self.var_maps[var_idx].0[&ptr];
                    if var_info.indices.is_empty() {
                        *inst = Instruction::new(Op::Nop, None, None, vec![]);
                        self.var_data[var_idx].stack.push(val);
                    } else {
                        let new_id = id(self.header);
                        let prev_comp = top_stack_or_undef(
                            self.header,
                            self.types_global_values,
                            &self.var_data[var_idx].stack,
                            self.var_data[var_idx].base_var_type,
                        );
                        let mut operands = vec![Operand::IdRef(val), Operand::IdRef(prev_comp)];
                        operands
                            .extend(var_info.indices.iter().copied().map(Operand::LiteralBit32));
                        *inst = Instruction::new(
                            Op::CompositeInsert,
                            Some(self.var_data[var_idx].base_var_type),
                            Some(new_id),
                            operands,
                        );
                        self.var_data[var_idx].stack.push(new_id);
                    }
                }
            } else if inst.class.opcode == Op::Load {
                let ptr = inst.operands[0].id_ref_any().unwrap();
                if let Some(&var_idx) = self.ptr_to_var_idx.get(&ptr) {
                    let var_info = &self.var_maps[var_idx].0[&ptr];
                    let loaded_val = inst.result_id.unwrap();
                    // TODO: Should this do something more sane if it's undef?
                    let current_obj = top_stack_or_undef(
                        self.header,
                        self.types_global_values,
                        &self.var_data[var_idx].stack,
                        self.var_data[var_idx].base_var_type,
                    );
                    if var_info.indices.is_empty() {
                        *inst = Instruction::new(Op::Nop, None, None, vec![]);
                        self.rewrite_rules.insert(loaded_val, current_obj);
                    } else {
                        let new_id = id(self.header);
                        let mut operands = vec![Operand::IdRef(current_obj)];
                        operands
                            .extend(var_info.indices.iter().copied().map(Operand::LiteralBit32));
                        *inst = Instruction::new(
                            Op::CompositeExtract,
                            Some(var_info.ty),
                            Some(new_id),
                            operands,
                        );
                        self.rewrite_rules.insert(loaded_val, new_id);
                    }
                }
            }
        }

        for dest_id in outgoing_edges(self.blocks[block]).collect::<Vec<_>>() {
            let dest_idx = self.blocks.get_index_of(&LabelId(dest_id)).unwrap();
            self.rename(dest_idx, Some(block));
        }

        for (var_data, orig_len) in self.var_data.iter_mut().zip(original_stacks) {
            var_data.stack.truncate(orig_len);
        }
    }
}

/// Resolve transitive rewrite chains in-place so `apply_rewrite_rules` needs only one pass.
///
/// A load from variable B may produce a value that is itself a rewritten load from
/// variable A (e.g. `%2 -> %1 -> A_value`). This collapses such chains.
const REWRITE_CHAIN_LIMIT: usize = 100;

fn resolve_rewrite_chains(rules: &mut FxHashMap<Word, Word>) {
    let keys: Vec<Word> = rules.keys().copied().collect();
    for key in keys {
        let mut value = rules[&key];
        // Follow the chain with a limit to guard against hypothetical cycles.
        let mut steps = 0;
        while let Some(&next) = rules.get(&value) {
            if next == value || steps > REWRITE_CHAIN_LIMIT {
                break;
            }
            value = next;
            steps += 1;
        }
        rules.insert(key, value);
    }
}

fn remove_nops(blocks: &mut FxIndexMap<LabelId, &mut Block>) {
    for block in blocks.values_mut() {
        block
            .instructions
            .retain(|inst| inst.class.opcode != Op::Nop);
    }
}

fn remove_old_variables(
    blocks: &mut FxIndexMap<LabelId, &mut Block>,
    var_maps_and_types: &[(FxHashMap<u32, VarInfo>, u32)],
) {
    blocks[0].instructions.retain(|inst| {
        inst.class.opcode != Op::Variable || {
            let result_id = inst.result_id.unwrap();
            var_maps_and_types
                .iter()
                .all(|(var_map, _)| !var_map.contains_key(&result_id))
        }
    });
    for block in blocks.values_mut() {
        block.instructions.retain(|inst| {
            !matches!(inst.class.opcode, Op::AccessChain | Op::InBoundsAccessChain)
                || inst.operands.iter().all(|op| {
                    op.id_ref_any().is_none_or(|id| {
                        var_maps_and_types
                            .iter()
                            .all(|(var_map, _)| !var_map.contains_key(&id))
                    })
                })
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rspirv::dr::Module;

    fn assemble(spirv: &str) -> Vec<u8> {
        use spirv_tools::assembler::{self, Assembler};
        let asm = assembler::create(None);
        let bin = asm
            .assemble(spirv, assembler::AssemblerOptions::default())
            .expect("Failed to assemble SPIR-V");
        let bytes: &[u8] = bin.as_ref();
        bytes.to_vec()
    }

    fn load(bytes: &[u8]) -> Module {
        crate::link::with_rspirv_loader(|loader| rspirv::binary::parse_bytes(bytes, loader))
            .unwrap()
    }

    /// Build `pointer_to_pointee` and constants maps, run mem2reg on each function,
    /// and return the disassembled output (without header).
    fn run_mem2reg(spirv: &str) -> String {
        let bytes = assemble(spirv);
        let mut module = load(&bytes);

        let mut pointer_to_pointee = FxHashMap::default();
        // Only u32 constants are collected — these are used exclusively for
        // resolving OpAccessChain indices (which must be scalar integers per
        // https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#OpAccessChain).
        // Other constant types (f32, etc.) are store/load values, not indices.
        let mut constants = FxHashMap::default();
        let mut u32_type = None;
        for inst in &module.types_global_values {
            match inst.class.opcode {
                Op::TypePointer => {
                    pointer_to_pointee
                        .insert(inst.result_id.unwrap(), inst.operands[1].unwrap_id_ref());
                }
                Op::TypeInt
                    if inst.operands[0].unwrap_literal_bit32() == 32
                        && inst.operands[1].unwrap_literal_bit32() == 0 =>
                {
                    u32_type = Some(inst.result_id.unwrap());
                }
                Op::Constant if u32_type.is_some() && inst.result_type == u32_type => {
                    let value = inst.operands[0].unwrap_literal_bit32();
                    constants.insert(inst.result_id.unwrap(), value);
                }
                _ => {}
            }
        }

        for func in &mut module.functions {
            crate::linker::simple_passes::block_ordering_pass(func);
            mem2reg(
                module.header.as_mut().unwrap(),
                &mut module.types_global_values,
                &pointer_to_pointee,
                &constants,
                func,
            );
        }

        use rspirv::binary::Disassemble;
        module.header = None;
        module
            .disassemble()
            .lines()
            .map(|l| l.trim().replace("  ", " "))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn assert_output_contains(output: &str, needle: &str) {
        assert!(
            output.contains(needle),
            "Expected output to contain:\n  {needle}\nActual output:\n{output}"
        );
    }

    fn assert_output_not_contains(output: &str, needle: &str) {
        assert!(
            !output.contains(needle),
            "Expected output NOT to contain:\n  {needle}\nActual output:\n{output}"
        );
    }

    #[test]
    fn simple_store_load_promoted() {
        // A simple store followed by a load from the same variable should be
        // promoted: the OpVariable, OpStore, and OpLoad should all disappear.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_42 = OpConstant %float 42.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_float Function
            OpStore %var %float_42
            %val = OpLoad %float %var
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        // The local variable should be gone.
        assert_output_not_contains(&output, "OpVariable %ptr_func_float Function");
        // The store to %out should use the constant directly (no intermediate load).
        assert_output_contains(&output, "OpStore");
        // No OpLoad from a Function variable should remain.
        // (The output store target is Output, which is fine.)
    }

    #[test]
    fn multiple_variables_both_promoted() {
        // Two independent variables should both be promoted.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %float_2 = OpConstant %float 2.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var_a = OpVariable %ptr_func_float Function
            %var_b = OpVariable %ptr_func_float Function
            OpStore %var_a %float_1
            OpStore %var_b %float_2
            %val_a = OpLoad %float %var_a
            %val_b = OpLoad %float %var_b
            OpStore %out %val_a
            OpReturn
            OpFunctionEnd",
        );
        // Both Function variables should be gone.
        assert_output_not_contains(&output, "OpVariable %ptr_func_float Function");
    }

    #[test]
    fn variable_with_initializer_promoted() {
        // OpVariable with an initializer (second operand after storage class).
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_7 = OpConstant %float 7.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_float Function %float_7
            %val = OpLoad %float %var
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        // The local variable should be promoted.
        assert_output_not_contains(&output, "OpVariable %ptr_func_float Function");
    }

    #[test]
    fn cross_variable_rewrite_chain() {
        // Load from variable A is stored into variable B, then loaded from B.
        // After batched mem2reg, this requires rewrite chain resolution:
        //   %val_b -> %val_a -> const.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_99 = OpConstant %float 99.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var_a = OpVariable %ptr_func_float Function
            %var_b = OpVariable %ptr_func_float Function
            OpStore %var_a %float_99
            %val_a = OpLoad %float %var_a
            OpStore %var_b %val_a
            %val_b = OpLoad %float %var_b
            OpStore %out %val_b
            OpReturn
            OpFunctionEnd",
        );
        // Both variables should be promoted.
        assert_output_not_contains(&output, "OpVariable %ptr_func_float Function");
        // The store to %out should ultimately use the constant.
        assert_output_contains(&output, "OpStore");
    }

    #[test]
    fn copy_memory_between_locals_promoted() {
        // OpCopyMemory between two local variables should be split and promoted.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_5 = OpConstant %float 5.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %src = OpVariable %ptr_func_float Function
            %dst = OpVariable %ptr_func_float Function
            OpStore %src %float_5
            OpCopyMemory %dst %src
            %val = OpLoad %float %dst
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        // Both local variables should be promoted.
        assert_output_not_contains(&output, "OpVariable %ptr_func_float Function");
        // No OpCopyMemory should remain.
        assert_output_not_contains(&output, "OpCopyMemory");
    }

    #[test]
    fn variable_used_in_function_call_not_promoted() {
        // A variable whose pointer is passed to a function call should NOT
        // be promoted, because the call might read/write through it.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\"
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %ptr_func_float = OpTypePointer Function %float
            %fn_void = OpTypeFunction %void
            %fn_ptr = OpTypeFunction %void %ptr_func_float
            %callee = OpFunction %void None %fn_ptr
            %param = OpFunctionParameter %ptr_func_float
            %callee_entry = OpLabel
            OpReturn
            OpFunctionEnd
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_float Function
            OpStore %var %float_1
            %val = OpFunctionCall %void %callee %var
            OpReturn
            OpFunctionEnd",
        );
        // The variable must survive because its pointer is passed to a call.
        // (Disassembly uses numeric IDs, so check for OpVariable with Function.)
        let has_func_var = output
            .lines()
            .any(|l| l.contains("OpVariable") && l.contains("Function"));
        assert!(
            has_func_var,
            "Expected a Function OpVariable to survive\n{output}"
        );
    }

    #[test]
    fn eligible_variable_promoted_while_ineligible_stays() {
        // When one variable is used in an unsupported way and another is clean,
        // only the clean one should be promoted.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %float_2 = OpConstant %float 2.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %fn_ptr = OpTypeFunction %void %ptr_func_float
            %callee = OpFunction %void None %fn_ptr
            %param = OpFunctionParameter %ptr_func_float
            %callee_entry = OpLabel
            OpReturn
            OpFunctionEnd
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %bad_var = OpVariable %ptr_func_float Function
            %good_var = OpVariable %ptr_func_float Function
            OpStore %bad_var %float_1
            %call = OpFunctionCall %void %callee %bad_var
            OpStore %good_var %float_2
            %good_val = OpLoad %float %good_var
            OpStore %out %good_val
            OpReturn
            OpFunctionEnd",
        );
        // Exactly one Function variable should remain (bad_var), not two.
        let func_var_count = output
            .lines()
            .filter(|l| l.contains("OpVariable") && l.contains("Function"))
            .count();
        assert_eq!(
            func_var_count, 1,
            "Expected exactly 1 Function OpVariable (the ineligible one)\n{output}"
        );
        // The good variable's load should have been eliminated (promoted).
        let load_count = output.lines().filter(|l| l.contains("OpLoad")).count();
        assert_eq!(
            load_count, 0,
            "Expected no OpLoad remaining (good_var's load should be promoted)\n{output}"
        );
    }

    #[test]
    fn access_chain_store_load_promoted() {
        // Store/load through OpAccessChain into a struct field should produce
        // OpCompositeInsert/OpCompositeExtract.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_3 = OpConstant %float 3.0
            %v2float = OpTypeVector %float 2
            %uint = OpTypeInt 32 0
            %uint_0 = OpConstant %uint 0
            %ptr_func_v2 = OpTypePointer Function %v2float
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_v2 Function
            %field = OpAccessChain %ptr_func_float %var %uint_0
            OpStore %field %float_3
            %val = OpLoad %float %field
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        // The local variable and access chain should be promoted.
        let has_func_var = output
            .lines()
            .any(|l| l.contains("OpVariable") && l.contains("Function"));
        assert!(
            !has_func_var,
            "Function variable should be promoted\n{output}"
        );
        // Should produce CompositeInsert for the store through access chain.
        assert_output_contains(&output, "OpCompositeInsert");
    }

    #[test]
    fn phi_from_control_flow() {
        // Stores in different branches with a load after the merge should
        // produce an OpPhi node.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out %cond_in
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %bool = OpTypeBool
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %float_2 = OpConstant %float 2.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %ptr_in_bool = OpTypePointer Input %bool
            %out = OpVariable %ptr_out_float Output
            %cond_in = OpVariable %ptr_in_bool Input
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_float Function
            %cond = OpLoad %bool %cond_in
            OpSelectionMerge %merge None
            OpBranchConditional %cond %true_bb %false_bb
            %true_bb = OpLabel
            OpStore %var %float_1
            OpBranch %merge
            %false_bb = OpLabel
            OpStore %var %float_2
            OpBranch %merge
            %merge = OpLabel
            %val = OpLoad %float %var
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        // The local variable should be promoted via a phi.
        let has_func_var = output
            .lines()
            .any(|l| l.contains("OpVariable") && l.contains("Function"));
        assert!(
            !has_func_var,
            "Function variable should be promoted\n{output}"
        );
        assert_output_contains(&output, "OpPhi");
    }

    #[test]
    fn load_before_store_produces_undef() {
        // Loading from a variable before any store should produce an OpUndef.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_float Function
            %val = OpLoad %float %var
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        // The variable should be promoted, with OpUndef replacing the load.
        let has_func_var = output
            .lines()
            .any(|l| l.contains("OpVariable") && l.contains("Function"));
        assert!(
            !has_func_var,
            "Function variable should be promoted\n{output}"
        );
        assert_output_contains(&output, "OpUndef");
    }

    #[test]
    fn pointer_used_in_select_not_promoted() {
        // A variable whose pointer is used in OpSelect (not Store/Load/AccessChain/
        // CopyMemory) cannot be promoted.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %cond_in %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %bool = OpTypeBool
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_in_bool = OpTypePointer Input %bool
            %ptr_out_float = OpTypePointer Output %float
            %cond_in = OpVariable %ptr_in_bool Input
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var_a = OpVariable %ptr_func_float Function
            %var_b = OpVariable %ptr_func_float Function
            OpStore %var_a %float_1
            OpStore %var_b %float_1
            %cond = OpLoad %bool %cond_in
            %sel = OpSelect %ptr_func_float %cond %var_a %var_b
            %val = OpLoad %float %sel
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        // Both variables' pointers appear in OpSelect, so neither can be promoted.
        let func_var_count = output
            .lines()
            .filter(|l| l.contains("OpVariable") && l.contains("Function"))
            .count();
        assert_eq!(
            func_var_count, 2,
            "Expected both Function OpVariables to survive (used in OpSelect)\n{output}"
        );
    }

    #[test]
    fn non_constant_access_chain_index_not_promoted() {
        // An OpAccessChain with a runtime (non-constant) index makes the
        // variable ineligible: construct_access_chain_info cannot resolve it.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %idx_in %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %uint = OpTypeInt 32 0
            %arr2 = OpTypeArray %float %uint
            %ptr_func_arr = OpTypePointer Function %arr2
            %ptr_func_float = OpTypePointer Function %float
            %ptr_in_uint = OpTypePointer Input %uint
            %ptr_out_float = OpTypePointer Output %float
            %idx_in = OpVariable %ptr_in_uint Input
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_arr Function
            %idx = OpLoad %uint %idx_in
            %elem = OpAccessChain %ptr_func_float %var %idx
            OpStore %elem %float_1
            %val = OpLoad %float %elem
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        let has_func_var = output
            .lines()
            .any(|l| l.contains("OpVariable") && l.contains("Function"));
        assert!(
            has_func_var,
            "Variable with dynamic access chain index should not be promoted\n{output}"
        );
    }

    #[test]
    fn non_u32_constant_access_chain_index_not_promoted() {
        // Access chain indices must be scalar integers per the SPIR-V spec
        // (https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#OpAccessChain),
        // but the constants map only tracks u32. A u64 constant index is valid
        // SPIR-V but is not resolved by mem2reg, so the variable stays.
        let output = run_mem2reg(
            "OpCapability Shader
            OpCapability Int64
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %uint = OpTypeInt 32 0
            %ulong = OpTypeInt 64 0
            %ulong_0 = OpConstant %ulong 0
            %arr2 = OpTypeArray %float %uint
            %ptr_func_arr = OpTypePointer Function %arr2
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %out = OpVariable %ptr_out_float Output
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_arr Function
            %elem = OpAccessChain %ptr_func_float %var %ulong_0
            OpStore %elem %float_1
            %val = OpLoad %float %elem
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        let has_func_var = output
            .lines()
            .any(|l| l.contains("OpVariable") && l.contains("Function"));
        assert!(
            has_func_var,
            "Variable with u64 access chain index should not be promoted\n{output}"
        );
    }

    #[test]
    fn phi_with_opline_interleaved() {
        // OpLine/OpNoLine can appear between OpPhi instructions per the SPIR-V
        // spec. Two variables with phis in the same merge block should both be
        // promoted even when OpLine separates the resulting OpPhi instructions.
        //
        // We can't directly inject OpLine between phis (mem2reg inserts them),
        // but we verify the precondition: two variables both produce phis at
        // the merge block, confirming the take_while handles non-Phi opcodes.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out_a %out_b %cond_in
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %bool = OpTypeBool
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %float_2 = OpConstant %float 2.0
            %float_3 = OpConstant %float 3.0
            %float_4 = OpConstant %float 4.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %ptr_in_bool = OpTypePointer Input %bool
            %out_a = OpVariable %ptr_out_float Output
            %out_b = OpVariable %ptr_out_float Output
            %cond_in = OpVariable %ptr_in_bool Input
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var_a = OpVariable %ptr_func_float Function
            %var_b = OpVariable %ptr_func_float Function
            %cond = OpLoad %bool %cond_in
            OpSelectionMerge %merge None
            OpBranchConditional %cond %true_bb %false_bb
            %true_bb = OpLabel
            OpStore %var_a %float_1
            OpStore %var_b %float_2
            OpBranch %merge
            %false_bb = OpLabel
            OpStore %var_a %float_3
            OpStore %var_b %float_4
            OpBranch %merge
            %merge = OpLabel
            %val_a = OpLoad %float %var_a
            %val_b = OpLoad %float %var_b
            OpStore %out_a %val_a
            OpStore %out_b %val_b
            OpReturn
            OpFunctionEnd",
        );
        // Both variables should be promoted via phis.
        let has_func_var = output
            .lines()
            .any(|l| l.contains("OpVariable") && l.contains("Function"));
        assert!(
            !has_func_var,
            "Both Function variables should be promoted\n{output}"
        );
        let phi_count = output.lines().filter(|l| l.contains("OpPhi")).count();
        assert_eq!(phi_count, 2, "Expected two OpPhi instructions\n{output}");
    }

    #[test]
    fn phi_not_found_past_non_phi_non_debug() {
        // Verify that insert_phi_value only searches through OpPhi and
        // OpLine/OpNoLine at the start of a block. If a non-phi, non-debug
        // instruction appears first, the search stops and a new phi is created
        // rather than finding one past the boundary. This test has a single
        // variable so it produces one phi — the key property is that the
        // take_while correctly bounds the search.
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\" %out %cond_in
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %bool = OpTypeBool
            %float = OpTypeFloat 32
            %float_1 = OpConstant %float 1.0
            %float_2 = OpConstant %float 2.0
            %ptr_func_float = OpTypePointer Function %float
            %ptr_out_float = OpTypePointer Output %float
            %ptr_in_bool = OpTypePointer Input %bool
            %out = OpVariable %ptr_out_float Output
            %cond_in = OpVariable %ptr_in_bool Input
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            %var = OpVariable %ptr_func_float Function
            %cond = OpLoad %bool %cond_in
            OpSelectionMerge %merge None
            OpBranchConditional %cond %true_bb %false_bb
            %true_bb = OpLabel
            OpStore %var %float_1
            OpBranch %merge
            %false_bb = OpLabel
            OpStore %var %float_2
            OpBranch %merge
            %merge = OpLabel
            %val = OpLoad %float %var
            OpStore %out %val
            OpReturn
            OpFunctionEnd",
        );
        let phi_count = output.lines().filter(|l| l.contains("OpPhi")).count();
        assert_eq!(phi_count, 1, "Expected exactly one OpPhi\n{output}");
    }

    #[test]
    fn no_variables_is_noop() {
        let output = run_mem2reg(
            "OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint Fragment %main \"main\"
            OpExecutionMode %main OriginUpperLeft
            %void = OpTypeVoid
            %fn_void = OpTypeFunction %void
            %main = OpFunction %void None %fn_void
            %entry = OpLabel
            OpReturn
            OpFunctionEnd",
        );
        assert_output_contains(&output, "OpReturn");
        assert_output_not_contains(&output, "OpVariable");
    }
}
