//! Simplify `OpCompositeExtract` pointing to `OpCompositeConstruct`s / `OpCompositeInsert`s.
//! Such constructions arise after inlining, when using multi-argument closures
//! (and other `Fn*` trait implementations). These composites can frequently be invalid,
//! containing pointers, `OpFunctionArgument`s, etc. After simplification, components
//! will become valid targets for `OpLoad`/`OpStore`.
use super::apply_rewrite_rules;
use rspirv::dr::{Function, Instruction};
use rspirv::spirv::Op;
use rustc_data_structures::fx::FxHashMap;

pub fn destructure_composites(function: &mut Function) {
    let mut rewrite_rules = FxHashMap::default();
    let reference: FxHashMap<_, _> = function
        .all_inst_iter()
        .filter_map(|inst| match inst.class.opcode {
            Op::CompositeConstruct => Some((inst.result_id.unwrap(), inst.clone())),
            Op::CompositeInsert if inst.operands.len() == 3 => {
                Some((inst.result_id.unwrap(), inst.clone()))
            }
            _ => None,
        })
        .collect();

    for inst in function.all_inst_iter_mut() {
        // multi-index extraction for nested tuples/structs
        if inst.class.opcode == Op::CompositeExtract && inst.operands.len() >= 2 {
            let mut current_id = inst.operands[0].unwrap_id_ref();
            let mut final_origin = None;

            // step through each index sequentially to resolve deeply nested extracts
            for index_operand in &inst.operands[1..] {
                let index = index_operand.unwrap_literal_bit32();

                let mut search_id = current_id;
                let mut resolved_id = None;

                while let Some(inst) = reference.get(&search_id) {
                    match inst.class.opcode {
                        Op::CompositeInsert => {
                            let insert_index = inst.operands[2].unwrap_literal_bit32();
                            if insert_index == index {
                                resolved_id = Some(inst.operands[0].unwrap_id_ref());
                                break;
                            }
                            // If not our index, continue down the insert chain
                            search_id = inst.operands[1].unwrap_id_ref();
                        }
                        Op::CompositeConstruct => {
                            resolved_id =
                                inst.operands.get(index as usize).map(|o| o.unwrap_id_ref());
                            break;
                        }
                        _ => unreachable!(),
                    }
                }

                if let Some(res) = resolved_id {
                    current_id = res;
                    final_origin = Some(res);
                } else {
                    final_origin = None;
                    break;
                }
            }

            if let Some(origin_id) = final_origin {
                rewrite_rules.insert(
                    inst.result_id.unwrap(),
                    rewrite_rules.get(&origin_id).map_or(origin_id, |id| *id),
                );
                *inst = Instruction::new(Op::Nop, None, None, vec![]);
            }
        }
    }

    // Transitive closure computation
    let mut closed_rewrite_rules = rewrite_rules.clone();
    for value in closed_rewrite_rules.values_mut() {
        while let Some(next) = rewrite_rules.get(value) {
            *value = *next;
        }
    }

    // Remove instructions replaced by NOPs, as well as unused composite values.
    for block in function.blocks.iter_mut() {
        block
            .instructions
            .retain(|inst| inst.class.opcode != Op::Nop);
    }
    apply_rewrite_rules(&closed_rewrite_rules, &mut function.blocks);
}
