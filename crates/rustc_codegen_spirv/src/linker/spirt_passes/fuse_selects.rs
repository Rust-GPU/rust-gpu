use either::Either;
use itertools::Itertools as _;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use spirt::cf::SelectionKind;
use spirt::func_at::FuncAt;
use spirt::transform::InnerInPlaceTransform;
use spirt::visit::InnerVisit;
use spirt::{Context, FuncDefBody, Node, NodeKind, Region, RegionDef, Value};
use std::mem;

use super::{ReplaceValueWith, VisitAllRegionsAndNodes};

/// Combine consecutive `Select`s in `func_def_body`.
pub(crate) fn fuse_selects_in_func(_cx: &Context, func_def_body: &mut FuncDefBody) {
    // Avoid having to support unstructured control-flow.
    if func_def_body.unstructured_cfg.is_some() {
        return;
    }

    // HACK(eddyb) this kind of random-access is easier than using `spirt::transform`.
    let mut all_regions = vec![];
    let mut divergent_nodes = FxHashSet::default();

    func_def_body.inner_visit_with(&mut VisitAllRegionsAndNodes {
        state: (),
        enter_region: |_: &mut (), func_at_region: FuncAt<'_, Region>| {
            all_regions.push(func_at_region.position);
        },
        exit_region: |_: &mut (), _| {},
        enter_node: |_: &mut (), _| {},
        exit_node: |_: &mut (), func_at_node: FuncAt<'_, Node>| {
            let node_def = func_at_node.def();
            let divergent = match &node_def.kind {
                NodeKind::ExitInvocation { .. } => true,
                NodeKind::Select(_) => node_def.child_regions.iter().all(|&case| {
                    (func_at_node.at(case).def().children.iter().last)
                        .is_some_and(|last_node| divergent_nodes.contains(&last_node))
                }),

                // FIXME(eddyb) handle other divergent cases as well.
                _ => false,
            };
            if divergent {
                divergent_nodes.insert(func_at_node.position);
            }
        },
    });

    // HACK(eddyb) this is only used when "factoring out" sole-convergent-cases.
    let mut replace_node_outputs = FxHashMap::default();

    // HACK(eddyb) reusing allocations for `Var` -> `Value` mappings.
    let mut reusable_var_replacements = FxHashMap::default();

    for region in all_regions {
        // HACK(eddyb) buffering the `Node`s to remove from this region,
        // as iterating and modifying a list at the same time isn't supported.
        let mut nodes_to_remove = SmallVec::<[_; 8]>::new();

        let mut func_at_children_iter = func_def_body.at_mut(region).at_children().into_iter();
        while let Some(func_at_child) = func_at_children_iter.next() {
            let base_node = func_at_child.position;
            let base_node_def = func_at_child.def();

            if replace_node_outputs.contains_key(&base_node) {
                continue;
            }

            if let NodeKind::Select(SelectionKind::BoolCond) = &base_node_def.kind {
                let base_cond = base_node_def.inputs[0];
                let base_cases = base_node_def.child_regions.clone();

                // Scan ahead for candidate `Select`s (with the same condition).
                let mut fusion_candidate_iter = func_at_children_iter.reborrow();
                while let Some(func_at_fusion_candidate) = fusion_candidate_iter.next() {
                    let fusion_candidate = func_at_fusion_candidate.position;
                    let mut func = func_at_fusion_candidate.at(());
                    let fusion_candidate_def = func.reborrow().at(fusion_candidate).def();
                    match &mut fusion_candidate_def.kind {
                        NodeKind::Select(SelectionKind::BoolCond)
                            if fusion_candidate_def.inputs[0] == base_cond =>
                        {
                            let cases_to_fuse = mem::take(&mut fusion_candidate_def.child_regions);
                            let fusion_candidate_outputs =
                                mem::take(&mut fusion_candidate_def.outputs);

                            // Concatenate the `Select`s' respective cases
                            // ("then" with "then", "else" with "else", etc.).
                            for (&base_case, &case_to_fuse) in base_cases.iter().zip(&cases_to_fuse)
                            {
                                // Replace uses of the outputs of the first `Select`,
                                // in the second one's case, with the specific values
                                // (e.g. `let y = if c { x } ...; if c { f(y) }`
                                // has to become `let y = if c { f(x); x } ...`).
                                let outputs_of_base_case = &func.regions[base_case].outputs;

                                // HACK(eddyb) due to no `func.vars` access through
                                // `ReplaceValueWith`, this is the next best thing.
                                let mut var_replacements = reusable_var_replacements;
                                assert_eq!(var_replacements.len(), 0);
                                var_replacements.extend(
                                    func.nodes[base_node]
                                        .outputs
                                        .iter()
                                        .copied()
                                        .zip_eq(outputs_of_base_case.iter().copied()),
                                );
                                func.reborrow()
                                    .at(case_to_fuse)
                                    .inner_in_place_transform_with(&mut ReplaceValueWith(
                                        |v| match v {
                                            Value::Const(_) => None,
                                            Value::Var(v) => var_replacements.get(&v).copied(),
                                        },
                                    ));
                                var_replacements.clear();
                                reusable_var_replacements = var_replacements;

                                let case_to_fuse_def =
                                    mem::take(func.reborrow().at(case_to_fuse).def());

                                assert_eq!(case_to_fuse_def.inputs.len(), 0);

                                let base_case_def = &mut func.regions[base_case];
                                base_case_def
                                    .children
                                    .append(case_to_fuse_def.children, func.nodes);
                                base_case_def.outputs.extend(case_to_fuse_def.outputs);
                            }

                            nodes_to_remove.push(fusion_candidate);

                            // Append the second `Select`'s outputs to the first's.
                            if !fusion_candidate_outputs.is_empty() {
                                let base_outputs = &mut func.nodes[base_node].outputs;
                                let fused_outputs_start =
                                    u32::try_from(base_outputs.len()).unwrap();
                                base_outputs.extend(
                                    fusion_candidate_outputs
                                        .into_iter()
                                        .zip(fused_outputs_start..)
                                        .map(|(var, def_idx)| {
                                            // HACK(eddyb) re-attach each `Var` on the fly.
                                            let var_decl = &mut func.vars[var];
                                            var_decl.def_parent = Either::Right(base_node);
                                            var_decl.def_idx = def_idx;

                                            var
                                        }),
                                );
                            }
                        }

                        _ => break,
                    }

                    // HACK(eddyb) some cases may have become divergent after
                    // fusion, and if only one case is left convergent, it can
                    // be factored out (to become unconditional).
                    let convergent_cases = base_cases.iter().copied().filter(|&case| {
                        !(func.reborrow().freeze().at(case).def().children.iter().last)
                            .is_some_and(|last_node| divergent_nodes.contains(&last_node))
                    });
                    if let Ok(convergent_case) = convergent_cases.exactly_one() {
                        let next_in_region = fusion_candidate_iter
                            .next()
                            .map(|func_at_next_node| func_at_next_node.position);
                        let mut func = func_at_children_iter.at(());

                        // Remove the now-unused outputs of the divergent cases,
                        // along with the output declarations of the `Select`.
                        for &other_case in &base_cases {
                            if other_case != convergent_case {
                                func.reborrow().at(other_case).def().outputs.clear();
                            }
                        }
                        let output_vars =
                            mem::take(&mut func.reborrow().at(base_node).def().outputs);

                        // FIXME(eddyb) deduplicate with near-identical code in
                        // `reduce` (where it handles "constant `scrutinee`").
                        let RegionDef {
                            inputs,
                            mut children,
                            outputs,
                        } = mem::take(func.reborrow().at(convergent_case).def());

                        assert_eq!(inputs.len(), 0);

                        // Move every child of the convergent case region, to
                        // just before `next_in_region`, in its parent `region`.
                        let region_children = &mut func.regions[region].children;
                        while let Some(case_child) = children.iter().first {
                            children.remove(case_child, func.nodes);
                            if let Some(next_in_region) = next_in_region {
                                region_children.insert_before(
                                    case_child,
                                    next_in_region,
                                    func.nodes,
                                );
                            } else {
                                region_children.insert_last(case_child, func.nodes);
                            }
                        }

                        // FIXME(eddyb) ideally it should be possible to rewrite
                        // uses of the `Select` outputs locally within `region`.
                        if !outputs.is_empty() {
                            replace_node_outputs.insert(base_node, (output_vars, outputs));
                        }

                        // HACK(eddyb) avoid iterator invalidation by restarting
                        // the rescan of the parent region, which shouldn't find
                        // any more `Select`s to fuse up to this point, anyway.
                        func_at_children_iter = func.at(region).at_children().into_iter();
                        break;
                    }
                }
            }
        }

        let region_children = &mut func_def_body.regions[region].children;
        for node in nodes_to_remove {
            region_children.remove(node, &mut func_def_body.nodes);
        }
    }

    // FIXME(eddyb) this shouldn't work at the level of nodes.
    if !replace_node_outputs.is_empty() {
        // HACK(eddyb) due to no `func.vars` access through
        // `ReplaceValueWith`, this is the next best thing.
        let mut var_replacements = reusable_var_replacements;
        assert_eq!(var_replacements.len(), 0);
        var_replacements.extend(
            replace_node_outputs
                .into_iter()
                .flat_map(|(_, (output_vars, outputs))| output_vars.into_iter().zip_eq(outputs)),
        );
        func_def_body.inner_in_place_transform_with(&mut ReplaceValueWith(|v| {
            let mut new_v = v;

            loop {
                if let Value::Var(v) = new_v
                    && let Some(replacement) = var_replacements.get(&v).copied()
                {
                    new_v = replacement;
                    continue;
                }
                break;
            }

            (new_v != v).then_some(new_v)
        }));
    }
}
