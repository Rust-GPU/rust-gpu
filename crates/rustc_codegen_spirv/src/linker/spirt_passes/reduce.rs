use itertools::{Either, Itertools};
use rustc_data_structures::fx::FxHashMap;
use smallvec::SmallVec;
use spirt::cf::SelectionKind;
use spirt::func_at::{FuncAt, FuncAtMut};
use spirt::transform::InnerInPlaceTransform;
use spirt::visit::InnerVisit;
use spirt::{
    Const, ConstDef, ConstKind, Context, DataInst, DataInstKind, EntityOrientedDenseMap,
    FuncDefBody, Node, NodeDef, NodeKind, Region, RegionDef, Type, TypeKind, Value, Var, VarDecl,
    VarKind, scalar, spv, vector,
};
use std::rc::Rc;
use std::{iter, mem};

use super::{ReplaceValueWith, VisitAllRegionsAndNodes};

use self::sealed::VarReplacer;
mod sealed {
    use spirt::{EntityOrientedDenseMap, Value, Var};

    // FIXME(eddyb) perhaps come up with a centralized abstraction for this
    // (in theory `VarDecl`s could indicate aliases, but that's a tradeoff).
    #[derive(Default)]
    pub struct VarReplacer {
        map: EntityOrientedDenseMap<Var, Value>,

        // HACK(eddyb) `EntityOrientedDenseMap` doesn't track its own size.
        count: usize,
    }

    impl VarReplacer {
        pub fn replacement_count(&self) -> usize {
            self.count
        }

        /// Insert a replacement from `old` to `new`, and return `true`,
        /// *iff* a replacement from `old` doesn't already exist *and*
        /// `maybe_mk_new()` returns `Some(new)`.
        pub fn maybe_add_replacement_with_if_new(
            &mut self,
            old: Var,
            maybe_mk_new: impl FnOnce() -> Option<Value>,
        ) -> bool {
            let mut is_new = false;
            if let entry @ None = self.map.entry(old)
                && let Some(new) = maybe_mk_new()
            {
                *entry = Some(new);
                is_new = true;
            }
            if is_new {
                self.count += 1;
            }
            is_new
        }

        pub fn apply_to_value(&self, v: &mut Value) -> bool {
            let mut any_changes = false;
            loop {
                if let Value::Var(var) = *v
                    && let Some(&new) = self.map.get(var)
                {
                    any_changes = true;
                    *v = new;
                    continue;
                }
                break;
            }
            any_changes
        }

        pub fn apply_to_values(&self, values: &mut [Value]) -> bool {
            let mut any_changes = false;
            for v in values {
                any_changes |= self.apply_to_value(v);
            }
            any_changes
        }
    }
}

// HACK(eddyb) these exist only because the relevant changes ended up not being
// necessary in the end, but they might still be useful in the future.
const ENABLE_PREDICATION_FOR_SELECT_PER_CASE_OUTPUTS: bool = false;
const ENABLE_REDUCES_PROBLEMATIC_WRT_QPTR_LEGALIZE: bool = false;

/// Apply "reduction rules" to `func_def_body`, replacing (pure) computations
/// with one of their inputs or a constant (e.g. `x + 0 => x` or `1 + 2 => 3`),
/// and at most only adding more `Select` outputs/`Loop` state (where necessary)
/// but never any new instructions (unlike e.g. LLVM's instcombine).
pub(crate) fn reduce_in_func(cx: &Context, func_def_body: &mut FuncDefBody) {
    let mut parent_map = ParentMap::new(func_def_body);

    let mut var_replacer = VarReplacer::default();

    let mut reduction_cache = FxHashMap::default();

    // HACK(eddyb) this is an annoying workaround for iterator invalidation
    // (SPIR-T iterators don't cope well with the underlying data changing).
    //
    // FIXME(eddyb) replace SPIR-T `FuncAtMut<EntityListIter<T>>` with some
    // kind of "list cursor", maybe even allowing removal during traversal.
    let mut flattened_visit_events = vec![];

    #[derive(Copy, Clone)]
    enum VisitEvent {
        EnterRegion(Region),
        ExitRegion(Region),
        EnterNode(Node),
        ExitNode(Node),
    }

    let mut iterations = 0;
    let start = std::time::Instant::now();
    loop {
        // FIXME(eddyb) do a "counting and/or hashing traversal" or similar,
        // after a few iterations (or track counts of all changes to the
        // function, but that's more intrusive), to detect a "bistable"
        // configuration, where `remove_unused_values_in_func` *exactly*
        // undoes everything done previously in each iteration, could
        // even call this a "Sisyphus detector".
        // NOTE(eddyb) the above comment mentions `remove_unused_values_in_func`,
        // which *does* run after `reduce`, but *not* as part of this loop, which
        // can easily create multiple unused copies, that are all removed later.
        //
        // FIXME(eddyb) fix the bad accidental loop... not-exactly-unrolling
        // (more like unfolding `+1` once per fix-point iteration).
        if iterations >= 64 {
            // FIXME(eddyb) maybe attach a SPIR-T diagnostic instead?
            eprintln!(
                "[WARNING] spirt_passes::reduce: giving up on fixpoint after {iterations} iterations (took {:?})",
                start.elapsed()
            );
            break;
        }
        iterations += 1;

        let old_var_replacement_count = var_replacer.replacement_count();

        // HACK(eddyb) auto-rebuild reified visit whenever empty, to allow easy
        // invalidation if needed (see also `flattened_visit_events` comment).
        if flattened_visit_events.is_empty() {
            let mut visitor = VisitAllRegionsAndNodes {
                state: flattened_visit_events,
                enter_region: |events: &mut Vec<_>, facr: FuncAt<'_, Region>| {
                    events.push(VisitEvent::EnterRegion(facr.position));
                },
                exit_region: |events: &mut Vec<_>, facr: FuncAt<'_, Region>| {
                    events.push(VisitEvent::ExitRegion(facr.position));
                },
                enter_node: |events: &mut Vec<_>, facn: FuncAt<'_, Node>| {
                    events.push(VisitEvent::EnterNode(facn.position));
                },
                exit_node: |events: &mut Vec<_>, facn: FuncAt<'_, Node>| {
                    events.push(VisitEvent::ExitNode(facn.position));
                },
            };
            func_def_body.inner_visit_with(&mut visitor);
            flattened_visit_events = visitor.state;
        }

        // HACK(eddyb) always start with only the complete reductions from the
        // last iteration (as they can offer access to auxiliary definitions,
        // which avoids synthesizing new ones needlessly).
        reduction_cache.retain(|_, result| matches!(result, Some(Ok(_))));

        // FIXME(eddyb) consider alternatives (to these loops) that could be
        // "demand-driven" (recursing into use->def, instead of processing defs).
        //
        // FIXME(eddyb) it would be nice if `FuncAtMut` could be wrapped to track
        // `any_changes` itself, with immutable access (`.freeze()`) not setting it.
        let mut any_changes = false;

        for &visit_event in &flattened_visit_events {
            match visit_event {
                VisitEvent::EnterRegion(region) => {
                    // HACK(eddyb) ignore regions that have been removed.
                    if region != func_def_body.body
                        && parent_map.region_parent.get(region).is_none()
                    {
                        // HACK(eddyb) also propagate the removal inwards.
                        // FIXME(eddyb) what is the cost of repeating this for
                        // the same region, i.e. doing noop removals?
                        let removed_region_def = mem::take(func_def_body.at_mut(region).def());
                        for func_at_child in func_def_body.at(removed_region_def.children) {
                            parent_map.node_parent.remove(func_at_child.position);
                        }
                    }
                }
                VisitEvent::ExitRegion(region) => {
                    // HACK(eddyb) ignore regions that have been removed.
                    if region != func_def_body.body
                        && parent_map.region_parent.get(region).is_none()
                    {
                        continue;
                    }

                    // FIXME(eddyb) run more of the reduction machinery,
                    // even if there is no operation applied to the value.
                    let region_def = func_def_body.at_mut(region).def();
                    any_changes |= var_replacer.apply_to_values(&mut region_def.outputs);
                    any_changes |= simplify_const_values_in_place(cx, &mut region_def.outputs);
                }
                VisitEvent::EnterNode(node) => {
                    let propagate_node_removal_inwards =
                        |func_def_body: &mut FuncDefBody, parent_map: &mut ParentMap| {
                            // FIXME(eddyb) what is the cost of repeating this for
                            // the same node, i.e. doing noop removals?
                            let removed_node_def = mem::replace(
                                func_def_body.at_mut(node).def(),
                                // HACK(eddyb) there isn't a good "dummy" to use here,
                                // but really the only thing we care about is no
                                // heap allocations remain around.
                                NodeDef {
                                    attrs: Default::default(),
                                    kind: NodeKind::Select(SelectionKind::BoolCond),
                                    inputs: Default::default(),
                                    child_regions: Default::default(),
                                    outputs: Default::default(),
                                },
                            );
                            for region in removed_node_def.child_regions {
                                parent_map.region_parent.remove(region);
                            }
                        };

                    // HACK(eddyb) ignore nodes that have been removed.
                    let Some(&parent_region) = parent_map.node_parent.get(node) else {
                        // HACK(eddyb) also propagate the removal inwards.
                        propagate_node_removal_inwards(func_def_body, &mut parent_map);

                        continue;
                    };

                    // HACK(eddyb) handy helper, as removing a node inherently
                    // requires substituting uses of its outputs - maybe that
                    // should be
                    let remove_node_replacing_outputs = |func_def_body: &mut FuncDefBody,
                                                         parent_map: &mut ParentMap,
                                                         var_replacer: &mut VarReplacer,
                                                         replacement_outputs: SmallVec<
                        [Value; 2],
                    >| {
                        let func = func_def_body.at_mut(());

                        for (&output_var, v) in
                            func.nodes[node].outputs.iter().zip_eq(replacement_outputs)
                        {
                            var_replacer.maybe_add_replacement_with_if_new(output_var, || Some(v));
                        }

                        func_def_body.regions[parent_region]
                            .children
                            .remove(node, &mut func_def_body.nodes);
                        parent_map.node_parent.remove(node);

                        // HACK(eddyb) emulate a second traversal of the
                        // outer loop, where `VisitEvent::EnterNode` reacts
                        // to the removed `parent_map.node_parent` entry.
                        propagate_node_removal_inwards(func_def_body, parent_map);
                    };

                    // HACK(eddyb) the `IntToBool` logic lower down doesn't
                    // understand case permutations.
                    let node_def = func_def_body.at_mut(node).def();
                    if let NodeKind::Select(SelectionKind::Switch { case_consts }) =
                        &mut node_def.kind
                        && let [a, b] = case_consts[..]
                        && [a.int_as_u32(), b.int_as_u32()] == [Some(1), Some(0)]
                    {
                        any_changes = true;

                        let cases = &mut node_def.child_regions;
                        assert_eq!(cases.len(), 3);
                        case_consts.swap(0, 1);
                        cases.swap(0, 1);
                    }

                    // FIXME(eddyb) run more of the reduction machinery,
                    // even if there is no operation applied to the value.
                    any_changes |= var_replacer.apply_to_values(&mut node_def.inputs);
                    any_changes |= simplify_const_values_in_place(cx, &mut node_def.inputs);

                    let func = func_def_body.at(());
                    let node_def = func.at(node).def();
                    match &node_def.kind {
                        NodeKind::Select(kind) => {
                            let scrutinee = node_def.inputs[0];
                            let cases = &node_def.child_regions;

                            // HACK(eddyb) minimum viable "`Select` by constant"
                            // simplification, e.g. `if false {A} else {B} -> B`.
                            if let Value::Const(scrutinee) = scrutinee {
                                let case_consts = match kind {
                                    SelectionKind::BoolCond => &[scalar::Const::TRUE][..],
                                    SelectionKind::Switch { case_consts } => case_consts,
                                };
                                let taken_case = scrutinee
                                    .as_scalar(cx)
                                    .filter(|s| case_consts.iter().all(|c| c.ty() == s.ty()))
                                    .map(|s| {
                                        cases[case_consts
                                            .iter()
                                            .position(|c| c == s)
                                            .unwrap_or(case_consts.len())]
                                    });
                                if let Some(taken_case) = taken_case {
                                    any_changes = true;

                                    let RegionDef {
                                        inputs,
                                        mut children,
                                        outputs,
                                    } = mem::take(func_def_body.at_mut(taken_case).def());

                                    assert_eq!(inputs.len(), 0);

                                    // Move every child of the taken case region, to just before
                                    // `node`, in its parent region (effectively replacing it).
                                    let parent_region_children =
                                        &mut func_def_body.regions[parent_region].children;
                                    while let Some(case_child) = children.iter().first {
                                        children.remove(case_child, &mut func_def_body.nodes);
                                        parent_region_children.insert_before(
                                            case_child,
                                            node,
                                            &mut func_def_body.nodes,
                                        );
                                        parent_map.node_parent[case_child] = parent_region;
                                    }

                                    remove_node_replacing_outputs(
                                        func_def_body,
                                        &mut parent_map,
                                        &mut var_replacer,
                                        outputs,
                                    );

                                    continue;
                                }
                            }

                            let scrutinee_redu_op = match kind {
                                // HACK(eddyb) turn `if c {A} else {B}` into `if !c {B} else {A}`,
                                // with the expectation that `!c` will undo some other negation,
                                // e.g. `if d { false } else { true }`.
                                // NOTE(eddyb) endless cycling is avoided in `try_reduce` by
                                // refusing to generate new e.g. `Node` outputs for
                                // `bool.not` (and only allow "true" reductions).
                                SelectionKind::BoolCond => {
                                    // FIXME(eddyb) try having an identity "reduction" op?
                                    Some(PureOp::BoolUnOp(scalar::BoolUnOp::Not))
                                }

                                // HACK(eddyb) turn `switch x { 0 => A, 1 => B, _ => ... }`
                                // into `if ... {B} else {A}`, when `x` ends up limited in `0..=1`,
                                // (such `switch`es come from e.g. `match`-ing enums w/ 2 variants)
                                // allowing us to bypass SPIR-T current (and temporary) lossiness
                                // wrt `_ => OpUnreachable` (i.e. we prove the default case can't
                                // be entered based on `x` not having values other than `0` or `1`)
                                //
                                // FIXME(eddyb) support more values than just `0..=1`.
                                SelectionKind::Switch { case_consts }
                                    if cases.len() == 3
                                        && case_consts.iter().enumerate().all(|(i, ct)| {
                                            ct.int_as_u32() == Some(u32::try_from(i).unwrap())
                                        }) =>
                                {
                                    Some(PureOp::IntToBool)
                                }

                                SelectionKind::Switch { .. } => None,
                            };

                            let (new_scrutinee_op, new_scrutinee, flipped) = scrutinee_redu_op
                                .and_then(|op| {
                                    let redu = Reducible {
                                        op,
                                        output_type: cx.intern(scalar::Type::Bool),
                                        input: scrutinee,
                                    };
                                    let reduced = redu.try_reduce_to_value_or_incomplete(
                                        cx,
                                        func_def_body.at_mut(()),
                                        &parent_map,
                                        &mut reduction_cache,
                                    );
                                    match reduced {
                                        Some(Ok(v)) => Some((op, v, false)),
                                        Some(Err(Incomplete(Reducible {
                                            op: PureOp::BoolUnOp(scalar::BoolUnOp::Not),
                                            input,
                                            ..
                                        }))) => Some((op, input, true)),
                                        _ => None,
                                    }
                                })
                                .map_or((None, scrutinee, false), |(op, v, flipped)| {
                                    (Some(op), v, flipped)
                                });

                            let func_at_node = func_def_body.at_mut(node);
                            let node_def = func_at_node.def();
                            match &mut node_def.kind {
                                NodeKind::Select(kind) => {
                                    node_def.inputs[0] = new_scrutinee;
                                    let cases = &mut node_def.child_regions;
                                    if let Some(op) = new_scrutinee_op {
                                        any_changes = true;
                                        let flipped = match (op, &*kind, &cases[..]) {
                                            (
                                                PureOp::IntToBool,
                                                SelectionKind::Switch { .. },
                                                &[case_0, case_1, _default],
                                            ) => {
                                                *kind = SelectionKind::BoolCond;
                                                *cases = [case_1, case_0].into_iter().collect();
                                                flipped
                                            }

                                            // FIXME(eddyb) the double flip is confusing.
                                            (
                                                PureOp::BoolUnOp(scalar::BoolUnOp::Not),
                                                SelectionKind::BoolCond,
                                                _,
                                            ) => !flipped,

                                            _ => unreachable!(),
                                        };
                                        if flipped {
                                            cases.swap(0, 1);
                                        }
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }

                        NodeKind::Loop { .. } | NodeKind::ExitInvocation { .. } => {}

                        DataInstKind::Scalar(_)
                        | DataInstKind::Vector(_)
                        | DataInstKind::FuncCall(_)
                        | DataInstKind::Mem(_)
                        | DataInstKind::QPtr(_)
                        | DataInstKind::ThunkBind(_)
                        | DataInstKind::SpvInst { .. }
                        | DataInstKind::SpvExtInst { .. } => {
                            let reduced = Reducible::try_from(func_def_body.at(node))
                                .ok()
                                .and_then(|redu| {
                                    redu.try_reduce_to_value_or_incomplete(
                                        cx,
                                        func_def_body.at_mut(()),
                                        &parent_map,
                                        &mut reduction_cache,
                                    )
                                });
                            match reduced {
                                None => {}
                                Some(Ok(v)) => {
                                    any_changes = true;
                                    remove_node_replacing_outputs(
                                        func_def_body,
                                        &mut parent_map,
                                        &mut var_replacer,
                                        [v].into_iter().collect(),
                                    );
                                }
                                Some(Err(Incomplete(redu))) => {
                                    if let Some((redu_kind, redu_inputs)) =
                                        redu.try_into_kind_and_inputs()
                                    {
                                        any_changes = true;

                                        let node_def = func_def_body.at_mut(node).def();
                                        (node_def.kind, node_def.inputs) = (redu_kind, redu_inputs);
                                    }
                                }
                            }
                        }
                    }
                }
                VisitEvent::ExitNode(node) => {
                    // HACK(eddyb) ignore nodes that have been removed.
                    if parent_map.node_parent.get(node).is_none() {
                        continue;
                    }

                    let node_def = &func_def_body.nodes[node];
                    match &node_def.kind {
                        NodeKind::Select(kind) => {
                            if ENABLE_PREDICATION_FOR_SELECT_PER_CASE_OUTPUTS {
                                // FIXME(eddyb) DRY.
                                let case_consts = match kind {
                                    SelectionKind::BoolCond => {
                                        &[scalar::Const::TRUE, scalar::Const::FALSE][..]
                                    }
                                    SelectionKind::Switch { case_consts } => case_consts,
                                };

                                // HACK(eddyb) it's not uncommon to have chained
                                // conditionals with the same condition, and end up
                                // with overly generic values, when more specific
                                // ones can be known, thanks to the condition.
                                // FIXME(eddyb) can this degrade `try_reduce_select`'s
                                // ability to reuse a previous `Select`'s outputs?
                                // (maybe this should be attempted only if merging
                                // the per-case outputs together has failed)
                                for (&case, &scrutinee_expected) in
                                    node_def.child_regions.iter().zip_eq(case_consts)
                                {
                                    for output_idx in 0..node_def.outputs.len() {
                                        let per_case_output =
                                            func_def_body.regions[case].outputs[output_idx];
                                        if let Some(per_case_output_assuming_case) =
                                            try_reduce_predicated(
                                                cx,
                                                &parent_map,
                                                func_def_body.at(()),
                                                per_case_output,
                                                node_def.inputs[0],
                                                scrutinee_expected,
                                            )
                                            && per_case_output_assuming_case != per_case_output
                                        {
                                            any_changes = true;
                                            func_def_body.regions[case].outputs[output_idx] =
                                                per_case_output_assuming_case;
                                        }
                                    }
                                }
                            }

                            let func = func_def_body.at(());
                            for (i, &output_var) in node_def.outputs.iter().enumerate() {
                                var_replacer.maybe_add_replacement_with_if_new(output_var, || {
                                    let per_case_value = node_def
                                        .child_regions
                                        .iter()
                                        .map(|&case| func.at(case).def().outputs[i]);
                                    try_reduce_select(
                                        cx,
                                        &parent_map,
                                        func,
                                        Some(node),
                                        kind,
                                        node_def.inputs[0],
                                        per_case_value,
                                    )
                                });
                            }
                        }

                        NodeKind::Loop { repeat_condition } => {
                            let body = node_def.child_regions[0];

                            let loop_state_count = node_def.inputs.len();

                            // FIXME(eddyb) run more of the reduction machinery,
                            // even if there is no operation applied to the value.
                            let repeat_condition = {
                                let mut v = *repeat_condition;
                                any_changes |= var_replacer.apply_to_value(&mut v);
                                v
                            };

                            for i in 0..loop_state_count {
                                let body_def = func_def_body.at(body).def();
                                let body_input = body_def.inputs[i];
                                let mut body_output = body_def.outputs[i];

                                // HACK(eddyb) take advantage of the loop body
                                // outputs being meant for the next iteration
                                // (only needed when `repeat_condition == true`).
                                if let Some(body_output_assuming_repeating) = try_reduce_predicated(
                                    cx,
                                    &parent_map,
                                    func_def_body.at(()),
                                    body_output,
                                    repeat_condition,
                                    scalar::Const::TRUE,
                                ) && body_output_assuming_repeating != body_output
                                {
                                    any_changes = true;
                                    func_def_body.at_mut(body).def().outputs[i] =
                                        body_output_assuming_repeating;
                                    body_output = body_output_assuming_repeating;
                                }

                                if body_output == Value::Var(body_input) {
                                    let initial_input = func_def_body.at(node).def().inputs[i];
                                    var_replacer
                                        .maybe_add_replacement_with_if_new(body_input, || {
                                            Some(initial_input)
                                        });
                                }
                            }

                            // HACK(eddyb) it's not uncommon for a loop body to
                            // end in a `Select` that simultaneously produces
                            // the repeat condition, loop body outputs (loop state),
                            // and its own separate outputs only used if the loop
                            // completes, i.e. on the last iteration, when the
                            // `repeat_condition` becomes `false`.
                            //
                            // FIXME(eddyb) how necessary is this after all?
                            if let Some(tail_node) =
                                func_def_body.at(body).def().children.iter().last
                            {
                                for i in 0..func_def_body.at(tail_node).def().outputs.len() {
                                    let tail_output = func_def_body.at(tail_node).def().outputs[i];
                                    let tail_output_used_by_loop = repeat_condition
                                        == Value::Var(tail_output)
                                        || func_def_body
                                            .at(body)
                                            .def()
                                            .outputs
                                            .contains(&Value::Var(tail_output));
                                    if tail_output_used_by_loop {
                                        continue;
                                    }

                                    // HACK(eddyb) ideally this could be handled
                                    // more generally, but that would require more
                                    // advanced/contextual predication systems
                                    // (region nesting might be sufficient?).
                                    let predicate = repeat_condition;
                                    let predicate_expected = scalar::Const::FALSE;
                                    let predicate_is_tail_node_output = match predicate {
                                        Value::Const(_) => None,
                                        Value::Var(v) => match func_def_body.vars[v].kind() {
                                            VarKind::NodeOutput {
                                                node: p_node,
                                                output_idx: p_idx,
                                            } if p_node == tail_node => Some(p_idx as usize),
                                            _ => None,
                                        },
                                    };
                                    if let Some(p_idx) = predicate_is_tail_node_output {
                                        let cases = &func_def_body.nodes[tail_node].child_regions;
                                        for &case in cases {
                                            let per_case_outputs =
                                                &func_def_body.at(case).def().outputs;
                                            let per_case_output = per_case_outputs[i];
                                            if let Some(per_case_output_assuming_exiting) =
                                                try_reduce_predicated(
                                                    cx,
                                                    &parent_map,
                                                    func_def_body.at(()),
                                                    per_case_output,
                                                    per_case_outputs[p_idx],
                                                    predicate_expected,
                                                )
                                                && per_case_output_assuming_exiting
                                                    != per_case_output
                                            {
                                                any_changes = true;
                                                func_def_body.regions[case].outputs[i] =
                                                    per_case_output_assuming_exiting;
                                            }
                                        }
                                    } else {
                                        let tail_output_assuming_exiting = try_reduce_predicated(
                                            cx,
                                            &parent_map,
                                            func_def_body.at(()),
                                            Value::Var(tail_output),
                                            predicate,
                                            predicate_expected,
                                        );
                                        if tail_output_assuming_exiting
                                            .is_some_and(|v| v != Value::Var(tail_output))
                                        {
                                            var_replacer.maybe_add_replacement_with_if_new(
                                                tail_output,
                                                || tail_output_assuming_exiting,
                                            );
                                        }
                                    }
                                }
                            }
                        }

                        NodeKind::ExitInvocation { .. }
                        | DataInstKind::Scalar(_)
                        | DataInstKind::Vector(_)
                        | DataInstKind::FuncCall(_)
                        | DataInstKind::Mem(_)
                        | DataInstKind::QPtr(_)
                        | DataInstKind::ThunkBind(_)
                        | DataInstKind::SpvInst { .. }
                        | DataInstKind::SpvExtInst { .. } => {}
                    }
                }
            }
        }

        if !any_changes && old_var_replacement_count == var_replacer.replacement_count() {
            break;
        }

        func_def_body.inner_in_place_transform_with(&mut ReplaceValueWith(|mut v| {
            if var_replacer.apply_to_value(&mut v) {
                // FIXME(eddyb) this has issues with loops, which can replace
                // the body inputs after the loop body is visited - maybe
                // the solution is tracking lengths in `flattened_visit_events`,
                // so traversal can restart to the start of the loop.
                if false {
                    // FIXME(eddyb) this is silly, but it should be impossible
                    // to reach this, now that `var_replacer` is eagerly used
                    // in the loop above.
                    unreachable!("replacements shouldn't be needed anymore");
                }
                any_changes = true;
                Some(v)
            } else {
                None
            }
        }));
    }
}

// FIXME(eddyb) maybe this kind of "parent map" should be provided by SPIR-T?
#[derive(Default)]
struct ParentMap {
    node_parent: EntityOrientedDenseMap<Node, Region>,
    region_parent: EntityOrientedDenseMap<Region, Node>,
}

impl ParentMap {
    fn new(func_def_body: &FuncDefBody) -> Self {
        let mut visitor = VisitAllRegionsAndNodes {
            state: Self::default(),
            enter_region: |this: &mut Self, func_at_region: FuncAt<'_, Region>| {
                for func_at_child_node in func_at_region.at_children() {
                    this.node_parent
                        .insert(func_at_child_node.position, func_at_region.position);
                }
            },
            exit_region: |_: &mut _, _| {},
            enter_node: |this: &mut Self, func_at_node: FuncAt<'_, Node>| {
                for &child_region in &func_at_node.def().child_regions {
                    this.region_parent
                        .insert(child_region, func_at_node.position);
                }
            },
            exit_node: |_: &mut _, _| {},
        };
        func_def_body.inner_visit_with(&mut visitor);
        visitor.state
    }
}

/// If possible, find a single `Value` from `cases` (or even `scrutinee`),
/// which would always be a valid result for `Select(kind, scrutinee, cases)`,
/// regardless of which case gets (dynamically) taken.
fn try_reduce_select(
    cx: &Context,
    parent_map: &ParentMap,
    // FIXME(eddyb) come up with a better convention for this!
    func: FuncAt<'_, ()>,
    // HACK(eddyb) `None` means this is actually an `OpSelect`.
    select_node: Option<Node>,
    kind: &SelectionKind,
    scrutinee: Value,
    cases: impl Iterator<Item = Value>,
) -> Option<Value> {
    let as_const = |v: Value| match v {
        Value::Const(ct) => Some(ct),
        Value::Var(_) => None,
    };

    // Rewrite `v` to assume `case_idx` was taken, i.e. replacing
    // `scrutinee` and values depending on it (on a best-effort basis).
    let specialize_for_case = |mut v: Value, case_idx: usize| {
        let var = match v {
            Value::Const(_) => return v,
            Value::Var(v) => v,
        };

        // FIXME(eddyb) might make sense to do actual recursion, and
        // even engage the full reduction machinery.
        if v == scrutinee {
            match kind {
                // FIXME(eddyb) is this wasteful? do bools need caching?
                SelectionKind::BoolCond => {
                    return Value::Const(
                        cx.intern([scalar::Const::TRUE, scalar::Const::FALSE][case_idx]),
                    );
                }
                SelectionKind::Switch { case_consts } => {
                    if let Some(&case_const) = case_consts.get(case_idx) {
                        return Value::Const(cx.intern(case_const));
                    }
                }
            }
        }

        // FIXME(eddyb) this overlaps a lot with `try_reduce_predicated`.
        if let VarKind::NodeOutput { node, output_idx } = func.vars[var].kind() {
            let v_def = func.at(node).def();
            match &v_def.kind {
                NodeKind::Select(v_def_kind) if v_def.inputs[0] == scrutinee => {
                    let equal_kind = match (v_def_kind, kind) {
                        (SelectionKind::BoolCond, SelectionKind::BoolCond) => true,
                        (
                            SelectionKind::Switch { case_consts: a },
                            SelectionKind::Switch { case_consts: b },
                        ) => a == b,
                        _ => false,
                    };
                    if equal_kind {
                        v = func.at(v_def.child_regions[case_idx]).def().outputs
                            [output_idx as usize];
                    }
                }
                _ => {}
            }
        }

        v
    };

    // Ignore `undef`s, as they can be legally substituted with any other value.
    let mut first_undef = None;
    let mut non_undef_cases = cases.enumerate().filter(|&(_, case)| {
        let is_undef = as_const(case).map(|ct| &cx[ct].kind) == Some(&ConstKind::Undef);
        if is_undef && first_undef.is_none() {
            first_undef = Some(case);
        }
        !is_undef
    });
    // FIXME(eddyb) false positive (no pre-existing tuple, only multi-value `match`ing).
    #[allow(clippy::tuple_array_conversions)]
    let merged_value = match (
        non_undef_cases.next(),
        non_undef_cases.next(),
        non_undef_cases.next(),
    ) {
        (None, ..) => first_undef?,

        // `Select(c: bool, f(true), f(false))` can be replaced with just `f(c)`,
        // if some suitable `f` can be found, such as:
        // - the identity function, i.e. `Select(c, true, false) -> c`
        // - the output of another `Select(c, _, _)` used in only one case,
        //   e.g.: `Select(c, Select(c, x, y), y) -> Select(c, x, y)`
        //
        // FIXME(eddyb) technically this can work for `switch`, too, but
        // that may require buffering the values before inspecting them.
        (Some((0, x)), Some((1, y)), None) if x != y && matches!(kind, SelectionKind::BoolCond) => {
            assert!(first_undef.is_none());

            let specific_cases = [x, y];
            ([scrutinee].into_iter().chain(specific_cases)).find(|&candidate| {
                // A general `candidate` which agrees with each individual case
                // can fully replace the original specific values, provided it
                // does dominate the `Select` itself (which is checked later).
                specific_cases.iter().enumerate().all(|(i, &specific)| {
                    specific == candidate
                        || specialize_for_case(specific, i) == specialize_for_case(candidate, i)
                })
            })?
        }

        (Some((x_idx, x)), y, z) => {
            if (y.into_iter().chain(z).chain(non_undef_cases)).all(|(_, v)| v == x) {
                // HACK(eddyb) if the same value appears in two different
                // cases, it's definitely dominating the whole `Select`.
                if y.is_some() {
                    return Some(x);
                }

                // HACK(eddyb) only one non-`undef` case, specializing to avoid
                // losing path-dependent information e.g. `Select(c, c, undef)`
                // can be replaced with `c` or `true` (as it's `c | undef`).
                //
                // FIXME(eddyb) there has to be a better way to do this, esp.
                // as the `Select(c: bool, f(true), f(false))` transformation
                // (see comment higher above) prefers generalizing, while using
                // specialization to guarantee viability of some general form
                // (maybe the solution is to keep the most general form, and
                // compute the per-case specialization, using the former only
                // when the latter doesn't overlap between cases).
                specialize_for_case(x, x_idx)
            } else {
                return None;
            }
        }
    };

    let is_valid_outside_select = |x: Value| {
        let select_node = match select_node {
            Some(select_node) => select_node,

            // HACK(eddyb) `select_node = None` indicates an
            // `OpSelect`, which is always dominated by its inputs.
            None => return Some(()),
        };

        // HACK(eddyb) the `scrutinee` has to dominate the `Select` itself.
        if x == scrutinee {
            return Some(());
        }

        // Constants are always valid.
        let x = match x {
            Value::Const(_) => return Some(()),
            Value::Var(x) => x,
        };

        // In general, `x` dominating the `Select` is what would
        // allow lifting an use of it outside the `Select`.
        let mut region_defining_x = match func.vars[x].kind() {
            VarKind::RegionInput { region, .. } => region,
            VarKind::NodeOutput { node, .. } => *parent_map.node_parent.get(node)?,
        };

        while let Some(&parent_of_region_defining_x) =
            parent_map.region_parent.get(region_defining_x)
        {
            // Fast-reject: if `x` is defined immediately inside one of
            // `select_node`'s cases, it's not a dominator.
            if parent_of_region_defining_x == select_node {
                return None;
            }

            // HACK(eddyb) due to SSA semantics (instead of RVSDG regions),
            // the body of a `Loop` dominates everything the `Loop` dominates.
            let parent_of_region_defining_x_def = func.at(parent_of_region_defining_x).def();
            match parent_of_region_defining_x_def.kind {
                NodeKind::Loop { .. }
                    if parent_of_region_defining_x_def.child_regions[0] == region_defining_x =>
                {
                    region_defining_x = parent_map
                        .node_parent
                        .get(parent_of_region_defining_x)
                        .copied()?;
                    continue;
                }
                _ => {}
            }

            break;
        }

        // Since we know `x` is used inside the `Select`, this only
        // needs to check that `x` is defined in a region that the
        // `Select` is nested in, as the only other possibility is
        // that the `x` is defined inside the `Select` - that is,
        // one of `x` and `Select` always dominates the other.
        //
        // FIXME(eddyb) this could be more efficient with some kind
        // of "region depth" precomputation but a potentially-slower
        // check doubles as a sanity check, for now.
        let mut region_containing_select = *parent_map.node_parent.get(select_node)?;
        loop {
            if region_containing_select == region_defining_x {
                return Some(());
            }
            region_containing_select = *parent_map
                .node_parent
                .get(*parent_map.region_parent.get(region_containing_select)?)?;
        }
    };

    is_valid_outside_select(merged_value).map(|()| merged_value)
}

/// Attempt to "simplify" `v` assuming `predicate == predicate_expected`
/// (i.e. disregarding values `v` can take in any other situations).
//
// FIXME(eddyb) how necessary is this after all?
fn try_reduce_predicated(
    cx: &Context,
    parent_map: &ParentMap,
    // FIXME(eddyb) come up with a better convention for this!
    func: FuncAt<'_, ()>,

    v: Value,
    predicate: Value,
    predicate_expected: scalar::Const,
) -> Option<Value> {
    if v == predicate {
        return Some(Value::Const(cx.intern(predicate_expected)));
    }

    let v = match v {
        Value::Const(_) => return Some(v),
        Value::Var(v) => v,
    };

    let predicate = match predicate {
        Value::Const(p) => {
            let p = p
                .as_scalar(cx)
                .filter(|p| p.ty() == predicate_expected.ty())?;
            return Some(if p == &predicate_expected {
                Value::Var(v)
            } else {
                // FIXME(eddyb) probably inefficient, return an `enum` instead?
                Value::Const(cx.intern(ConstDef {
                    attrs: Default::default(),
                    ty: func.at(v).decl().ty,
                    kind: ConstKind::Undef,
                }))
            });
        }
        Value::Var(p) => p,
    };

    match (func.at(v).decl().kind(), func.at(predicate).decl().kind()) {
        // FIXME(eddyb) this prefers recursing on `predicate`, but it's possible
        // `v` is derived from something that depends on `predicate`.
        (
            v_kind,
            VarKind::NodeOutput {
                node: p_node,
                output_idx: p_idx,
            },
        ) => {
            let p_node_def = func.at(p_node).def();
            match &p_node_def.kind {
                NodeKind::Select(kind) => {
                    // FIXME(eddyb) collecting only because of per-case `Option`.
                    let per_case_predicated: SmallVec<[_; 2]> = p_node_def
                        .child_regions
                        .iter()
                        .map(|&case| {
                            // FIXME(eddyb) ideally predication on `p_node_def.inputs[0]`
                            // would be also done here.
                            let per_case_outputs = &func.at(case).def().outputs;
                            let v = match v_kind {
                                VarKind::NodeOutput { node, output_idx } if node == p_node => {
                                    per_case_outputs[output_idx as usize]
                                }
                                _ => Value::Var(v),
                            };
                            try_reduce_predicated(
                                cx,
                                parent_map,
                                func,
                                v,
                                per_case_outputs[p_idx as usize],
                                predicate_expected,
                            )
                        })
                        .collect::<Option<_>>()?;
                    try_reduce_select(
                        cx,
                        parent_map,
                        func,
                        Some(p_node),
                        kind,
                        p_node_def.inputs[0],
                        per_case_predicated.iter().copied(),
                    )
                }
                _ => None,
            }
        }

        (
            VarKind::NodeOutput {
                node: v_node,
                output_idx: v_idx,
            },
            _,
        ) if ENABLE_PREDICATION_FOR_SELECT_PER_CASE_OUTPUTS => {
            let v_node_def = func.at(v_node).def();
            if let NodeKind::Select(kind) = &v_node_def.kind {
                // FIXME(eddyb) DRY.
                let case_consts = match kind {
                    SelectionKind::BoolCond => &[scalar::Const::TRUE, scalar::Const::FALSE][..],
                    SelectionKind::Switch { case_consts } => case_consts,
                };

                // FIXME(eddyb) collecting only because of per-case `Option`.
                let per_case_predicated: SmallVec<[_; 2]> = v_node_def
                    .child_regions
                    .iter()
                    .enumerate()
                    .map(|(case_idx, &case)| {
                        let per_case_outputs = &func.at(case).def().outputs;
                        // FIXME(eddyb) this is completely inscrutable, at
                        // least it should be traversing the DAG so it will
                        // terminate, but predication should be systematic.
                        let predicate = case_consts
                            .get(case_idx)
                            .and_then(|&scrutinee_expected| {
                                try_reduce_predicated(
                                    cx,
                                    parent_map,
                                    func,
                                    Value::Var(predicate),
                                    v_node_def.inputs[0],
                                    scrutinee_expected,
                                )
                            })
                            .unwrap_or(Value::Var(predicate));
                        try_reduce_predicated(
                            cx,
                            parent_map,
                            func,
                            per_case_outputs[v_idx as usize],
                            predicate,
                            predicate_expected,
                        )
                    })
                    .collect::<Option<_>>()?;
                try_reduce_select(
                    cx,
                    parent_map,
                    func,
                    Some(v_node),
                    kind,
                    v_node_def.inputs[0],
                    per_case_predicated.iter().copied(),
                )
            } else {
                let try_reduce_predicated = |v| {
                    try_reduce_predicated(
                        cx,
                        parent_map,
                        func,
                        v,
                        Value::Var(predicate),
                        predicate_expected,
                    )
                };

                let mut redu = Reducible::try_from(func.at(v_node)).ok()?;
                assert_eq!(v_idx, 0);

                loop {
                    if let Some(predicated_input) = try_reduce_predicated(redu.input) {
                        redu.input = predicated_input;
                    }
                    match redu.try_reduce_shallow(cx, |v| func.at(v).type_of(cx))? {
                        ReductionStep::Complete(v) => return try_reduce_predicated(v),
                        ReductionStep::Partial(redu_next) => redu = redu_next,
                    }
                }
            }
        }

        _ => None,
    }
}

/// Pure operation that transforms one `Value` into another `Value`.
//
// FIXME(eddyb) move this elsewhere? also, how should binops etc. be supported?
// (one approach could be having a "focus input" that can be dynamic, with the
// other inputs being `Const`s, i.e. partially applying all but one input)
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum PureOp {
    BoolUnOp(scalar::BoolUnOp),
    IntUnOp(scalar::IntUnOp),

    /// Integer binary operation with a constant RHS (e.g. `x + 123`).
    //
    // NOTE(eddyb) this also includes the cases when the LHS is also constant
    // (i.e. `reduce` handling constant folding that should be in SPIR-T itself).
    IntBinOpConstRhs(scalar::IntBinOp, Const),

    /// Boolean binary operation with a constant RHS (e.g. `x & true`).
    //
    // NOTE(eddyb) this also includes the cases when the LHS is also constant
    // (i.e. `reduce` handling constant folding that should be in SPIR-T itself).
    BoolBinOpConstRhs(scalar::BoolBinOp, Const),

    BitCast,
    // FIXME(eddyb) include all of `vector::Op` (or obsolete with `flow`).
    VectorExtract {
        elem_idx: u8,
    },

    /// Maps `0` to `false`, and `1` to `true`, but any other input values do
    /// not allow reduction, which is used to signal that the input couldn't
    /// be constrained to `0..=1` (and may take other values).
    //
    // HACK(eddyb) not a real operation, but a helper used to extract a `bool`
    // equivalent for an `OpSwitch`'s scrutinee.
    // FIXME(eddyb) proper SPIR-T range analysis should be implemented and such
    // a reduction not attempted at all if the range is larger than `0..=1`
    // (also, the actual operation can be replaced with `x == 1` or `x != 0`)
    IntToBool,

    // HACK(eddyb) these should be supported by `qptr` itself.
    // FIXME(eddyb) for now these are rewritten into bitcasts and not matched on.
    IntToPtr,
    PtrToInt,
    PtrEqAddr {
        addr: Const,
    },
}

impl TryFrom<(&DataInstKind, &[Option<Const>])> for PureOp {
    type Error = ();
    fn try_from((kind, const_inputs): (&DataInstKind, &[Option<Const>])) -> Result<Self, ()> {
        match kind {
            &DataInstKind::Scalar(scalar::Op::IntUnary(op)) => Ok(Self::IntUnOp(op)),
            &DataInstKind::Scalar(scalar::Op::IntBinary(op)) => {
                match op {
                    // FIXME(eddyb) these produce two outputs each.
                    scalar::IntBinOp::CarryingAdd
                    | scalar::IntBinOp::BorrowingSub
                    | scalar::IntBinOp::WideningMulU
                    | scalar::IntBinOp::WideningMulS => Err(()),

                    _ => Ok(Self::IntBinOpConstRhs(op, const_inputs[1].ok_or(())?)),
                }
            }
            &DataInstKind::Scalar(scalar::Op::BoolUnary(op)) => Ok(Self::BoolUnOp(op)),
            &DataInstKind::Scalar(scalar::Op::BoolBinary(op)) => {
                Ok(Self::BoolBinOpConstRhs(op, const_inputs[1].ok_or(())?))
            }

            &DataInstKind::Vector(vector::Op::Whole(vector::WholeOp::Extract { elem_idx })) => {
                Ok(Self::VectorExtract { elem_idx })
            }
            DataInstKind::SpvInst(spv_inst, lowering) => {
                if lowering.disaggregated_output.is_some()
                    || !lowering.disaggregated_inputs.is_empty()
                {
                    return Err(());
                }

                let wk = &super::SpvSpecWithExtras::get().well_known;

                let op = spv_inst.opcode;
                Ok(match spv_inst.imms[..] {
                    [] if op == wk.OpBitcast => Self::BitCast,

                    [] if op == wk.OpConvertUToPtr => Self::IntToPtr,
                    [] if op == wk.OpConvertPtrToU => Self::PtrToInt,

                    _ => return Err(()),
                })
            }
            _ => Err(()),
        }
    }
}

impl TryFrom<PureOp> for DataInstKind {
    type Error = ();
    fn try_from(op: PureOp) -> Result<Self, ()> {
        let wk = &super::SpvSpecWithExtras::get().well_known;

        let (opcode, imms) = match op {
            PureOp::IntUnOp(op) => return Ok(scalar::Op::from(op).into()),
            PureOp::IntBinOpConstRhs(op, _) => return Ok(scalar::Op::from(op).into()),
            PureOp::BoolUnOp(op) => return Ok(scalar::Op::from(op).into()),
            PureOp::BoolBinOpConstRhs(op, _) => return Ok(scalar::Op::from(op).into()),

            PureOp::BitCast => (wk.OpBitcast, iter::empty().collect()),
            PureOp::VectorExtract { elem_idx } => {
                return Ok(vector::Op::from(vector::WholeOp::Extract { elem_idx }).into());
            }

            PureOp::IntToPtr => (wk.OpConvertUToPtr, iter::empty().collect()),
            PureOp::PtrToInt => (wk.OpConvertPtrToU, iter::empty().collect()),

            // HACK(eddyb) this is the only reason this is `TryFrom` not `From`.
            PureOp::IntToBool | PureOp::PtrEqAddr { .. } => {
                return Err(());
            }
        };
        Ok(DataInstKind::SpvInst(
            spv::Inst { opcode, imms },
            spv::InstLowering::default(),
        ))
    }
}

/// Potentially-reducible application of a `PureOp` (`op`) to `input`.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct Reducible<V = Value> {
    op: PureOp,
    output_type: Type,
    input: V,
}

impl<V> Reducible<V> {
    fn with_input<V2>(self, new_input: V2) -> Reducible<V2> {
        Reducible {
            op: self.op,
            output_type: self.output_type,
            input: new_input,
        }
    }
}

impl TryFrom<FuncAt<'_, DataInst>> for Reducible {
    type Error = ();
    fn try_from(func_at_inst: FuncAt<'_, DataInst>) -> Result<Self, ()> {
        let inst_def = func_at_inst.def();
        let const_inputs = match inst_def.inputs[..] {
            [x] => [Some(x), None],
            [a, b] => [Some(a), Some(b)],
            _ => return Err(()),
        }
        .map(|v| match v? {
            Value::Const(ct) => Some(ct),
            Value::Var(_) => None,
        });
        let const_inputs = &const_inputs[..inst_def.inputs.len()];

        let op = PureOp::try_from((&inst_def.kind, const_inputs))?;

        // HACK(eddyb) all supported instructions should be single-output.
        assert_eq!(inst_def.outputs.len(), 1);

        let output_type = func_at_inst.at(inst_def.outputs[0]).decl().ty;
        match (op, &inst_def.inputs[..]) {
            (PureOp::IntBinOpConstRhs(..) | PureOp::BoolBinOpConstRhs(..), &[input, _])
            | (_, &[input]) => Ok(Self {
                op,
                output_type,
                input,
            }),
            _ => Err(()),
        }
    }
}

impl Reducible {
    // HACK(eddyb) `IntToBool` is the only reason this can return `None`.
    fn try_into_kind_and_inputs(self) -> Option<(DataInstKind, SmallVec<[Value; 2]>)> {
        let Self {
            op,
            output_type: _,
            input,
        } = self;

        Some((
            op.try_into().ok()?,
            match op {
                PureOp::IntBinOpConstRhs(_, rhs) | PureOp::BoolBinOpConstRhs(_, rhs) => {
                    [input, Value::Const(rhs)].into_iter().collect()
                }
                _ => [input].into_iter().collect(),
            },
        ))
    }

    // HACK(eddyb) `IntToBool` is the only reason this can return `None`.
    fn try_materialize_as_new_node_and_output(
        self,
        cx: &Context,
        func: FuncAtMut<'_, ()>,
    ) -> Option<(Node, Var)> {
        let (kind, inputs) = self.try_into_kind_and_inputs()?;

        let node = func.nodes.define(
            cx,
            NodeDef {
                attrs: Default::default(),
                kind,
                inputs,
                child_regions: [].into_iter().collect(),
                outputs: [].into_iter().collect(),
            }
            .into(),
        );

        // FIXME(eddyb) automate this (insertion cursor?).
        let output_var = func.vars.define(
            cx,
            VarDecl {
                attrs: Default::default(),
                ty: self.output_type,
                def_parent: Either::Right(node),
                def_idx: 0,
            },
        );
        func.nodes[node].outputs.push(output_var);

        Some((node, output_var))
    }
}

// HACK(eddyb) work around `OpSpecConstantOp` not being handled anywhere else.
fn simplify_const_values_in_place(cx: &Context, values: &mut [Value]) -> bool {
    let mut any_changes = false;
    for v in values {
        match v {
            Value::Const(ct) => {
                let new_ct = simplify_const(cx, *ct);
                if new_ct != *ct {
                    *ct = new_ct;
                    any_changes = true;
                }
            }
            Value::Var(_) => {}
        }
    }
    any_changes
}
fn simplify_const(cx: &Context, ct: Const) -> Const {
    Reducible::try_from((cx, ct))
        .ok()
        .and_then(|redu| redu.try_reduce_const(cx))
        .unwrap_or(ct)
}
impl TryFrom<(&Context, Const)> for Reducible<Const> {
    type Error = ();
    fn try_from((cx, ct): (&Context, Const)) -> Result<Self, ()> {
        let ct_def = &cx[ct];
        let ConstKind::SpvInst {
            spv_inst_and_const_inputs,
        } = &ct_def.kind
        else {
            return Err(());
        };
        let (spv_inst, inputs) = &**spv_inst_and_const_inputs;

        let wk = &super::SpvSpecWithExtras::get().well_known;

        let node_kind = match spv_inst.imms[..] {
            [spv::Imm::Short(_, opcode)] if spv_inst.opcode == wk.OpSpecConstantOp => {
                let (opcode, _, _) = spv::spec::Opcode::try_from_u16_with_name_and_def(
                    opcode.try_into().ok().ok_or(())?,
                )
                .ok_or(())?;

                let spv_inst = spv::Inst::from(opcode);
                spv_inst
                    .as_canonical_node_kind(
                        cx,
                        [ct_def.ty].into_iter(),
                        inputs.iter().map(|&input| cx[input].ty),
                    )
                    .unwrap_or_else(|| NodeKind::SpvInst(spv_inst, spv::InstLowering::default()))
            }
            _ => return Err(()),
        };

        let op = match inputs[..] {
            [x] => PureOp::try_from((&node_kind, &[Some(x)][..]))?,
            [a, b] => PureOp::try_from((&node_kind, &[Some(a), Some(b)][..]))?,
            _ => return Err(()),
        };

        match (op, &inputs[..]) {
            (PureOp::IntBinOpConstRhs(..) | PureOp::BoolBinOpConstRhs(..), &[input, _])
            | (_, &[input]) => Ok(Self {
                op,
                output_type: ct_def.ty,
                input,
            }),
            _ => Err(()),
        }
    }
}

impl Reducible<Const> {
    // FIXME(eddyb) in theory this should always return `Some`.
    fn try_reduce_const(&self, cx: &Context) -> Option<Const> {
        let wk = &super::SpvSpecWithExtras::get().well_known;

        // HACK(eddyb) precomputed here for easy reuse (does not allocate).
        let const_bitcast_spv_inst = spv::Inst {
            opcode: wk.OpSpecConstantOp,
            imms: [spv::Imm::Short(
                wk.LiteralSpecConstantOpInteger,
                wk.OpBitcast.as_u16().into(),
            )]
            .into_iter()
            .collect(),
        };

        let scalar_output_type = self.output_type.as_scalar(cx);
        let try_eval_scalar_op = |op: scalar::Op, inputs: &[scalar::Const]| {
            let output = op
                .try_eval(inputs, &[scalar_output_type?])
                .ok()?
                .into_iter()
                .exactly_one()
                .ok()?;

            Some(cx.intern(ConstDef {
                attrs: Default::default(),
                ty: self.output_type,
                kind: ConstKind::Scalar(output),
            }))
        };

        // FIXME(eddyb) measure the cost of these (repeated?) attempts.
        let input = simplify_const(cx, self.input);

        let input_def = &cx[input];
        match (self.op, &input_def.kind) {
            (_, ConstKind::Undef) => Some(cx.intern(ConstDef {
                attrs: input_def.attrs,
                ty: self.output_type,
                kind: input_def.kind.clone(),
            })),

            // FIXME(eddyb) these should be in SPIR-T itself.
            (PureOp::IntUnOp(op), &ConstKind::Scalar(ct)) => try_eval_scalar_op(op.into(), &[ct]),
            (PureOp::IntBinOpConstRhs(op, ct_rhs), &ConstKind::Scalar(ct_lhs)) => {
                let ct_rhs = *simplify_const(cx, ct_rhs).as_scalar(cx)?;
                try_eval_scalar_op(op.into(), &[ct_lhs, ct_rhs])
            }
            (PureOp::BoolUnOp(op), &ConstKind::Scalar(ct)) => try_eval_scalar_op(op.into(), &[ct]),
            (PureOp::BoolBinOpConstRhs(op, ct_rhs), &ConstKind::Scalar(ct_lhs)) => {
                let ct_rhs = *simplify_const(cx, ct_rhs).as_scalar(cx)?;
                try_eval_scalar_op(op.into(), &[ct_lhs, ct_rhs])
            }

            // FIXME(eddyb) consistently represent all SPIR-T constants with a
            // known bit-pattern.
            (PureOp::BitCast, _) if input_def.ty == self.output_type => Some(input),
            (
                PureOp::BitCast,
                ConstKind::SpvInst {
                    spv_inst_and_const_inputs,
                },
            ) => {
                let (spv_inst, const_inputs) = &**spv_inst_and_const_inputs;
                if *spv_inst == const_bitcast_spv_inst {
                    assert_eq!(const_inputs.len(), 1);
                    self.with_input(const_inputs[0]).try_reduce_const(cx)
                } else if spv_inst.opcode == wk.OpConstantNull {
                    Some(cx.intern(ConstDef {
                        attrs: input_def.attrs,
                        ty: self.output_type,
                        kind: ConstKind::Scalar(scalar::Const::from_bits(scalar_output_type?, 0)),
                    }))
                } else {
                    None
                }
            }
            (PureOp::BitCast, ConstKind::Scalar(ct)) if scalar_output_type.is_some() => {
                if ct.ty().bit_width() == scalar_output_type?.bit_width() {
                    Some(cx.intern(ConstDef {
                        attrs: input_def.attrs,
                        ty: self.output_type,
                        kind: ConstKind::Scalar(scalar::Const::try_from_bits(
                            scalar_output_type.unwrap(),
                            ct.bits(),
                        )?),
                    }))
                } else {
                    None
                }
            }
            (PureOp::BitCast, _) if ENABLE_REDUCES_PROBLEMATIC_WRT_QPTR_LEGALIZE => {
                let (spv_inst, const_inputs) = if input.as_scalar(cx).map(|ct| ct.bits()) == Some(0)
                {
                    (wk.OpConstantNull.into(), [].into_iter().collect())
                } else {
                    (const_bitcast_spv_inst, [input].into_iter().collect())
                };
                Some(cx.intern(ConstDef {
                    attrs: Default::default(),
                    ty: self.output_type,
                    kind: ConstKind::SpvInst {
                        spv_inst_and_const_inputs: Rc::new((spv_inst, const_inputs)),
                    },
                }))
            }

            (PureOp::VectorExtract { elem_idx }, ConstKind::Vector(ct)) => {
                Some(cx.intern(ct.get_elem(elem_idx.into())?))
            }

            (PureOp::IntToBool, ConstKind::Scalar(ct)) => {
                Some(cx.intern(scalar::Const::try_from_bits(scalar::Type::Bool, ct.bits())?))
            }
            (PureOp::PtrEqAddr { .. }, ConstKind::PtrToGlobalVar { .. }) => {
                Some(cx.intern(scalar::Const::FALSE))
            }
            (PureOp::PtrEqAddr { addr }, _) => {
                // HACK(eddyb) this effectively expands `PtrEqAddr`'s two-step
                // definition (maybe this is now unnecessary and `Eq` suffices?).
                Self {
                    op: PureOp::IntBinOpConstRhs(scalar::IntBinOp::Eq, addr),
                    input: Self {
                        op: PureOp::BitCast,
                        input,
                        output_type: cx[addr].ty,
                    }
                    .try_reduce_const(cx)?,
                    ..*self
                }
                .try_reduce_const(cx)
            }

            _ => None,
        }
    }
}

/// Outcome of a single step of a reduction (which may require more steps).
enum ReductionStep {
    Complete(Value),
    Partial(Reducible),
}

impl Reducible {
    // FIXME(eddyb) find better name? (this only inspects constant inputs, and
    // otherwise either returns a constant, or depends on the input uniformly)
    fn try_reduce_shallow(
        &self,
        cx: &Context,
        type_of_value: impl Fn(Value) -> Type,
    ) -> Option<ReductionStep> {
        // HACK(eddyb) aggressive typed rewriting, bypassing even `try_reduce_const`.
        match self.op {
            PureOp::BitCast if type_of_value(self.input) == self.output_type => {
                return Some(ReductionStep::Complete(self.input));
            }

            // HACK(eddyb) rewriting lossless ptr<->int conversions into bitcasts
            // encodes that losslessness and allows simpler reductions downstream.
            PureOp::IntToPtr
                if type_of_value(self.input).as_scalar(cx) == Some(super::QPTR_SIZED_UINT) =>
            {
                return Some(ReductionStep::Partial(Self {
                    op: PureOp::BitCast,
                    ..*self
                }));
            }
            PureOp::PtrToInt if self.output_type.as_scalar(cx) == Some(super::QPTR_SIZED_UINT) => {
                return Some(ReductionStep::Partial(Self {
                    op: PureOp::BitCast,
                    ..*self
                }));
            }

            _ => {}
        }

        if let Value::Const(ct) = self.input {
            return Some(ReductionStep::Complete(Value::Const(
                self.with_input(ct).try_reduce_const(cx)?,
            )));
        }

        // FIXME(eddyb) should this just be a method on `scalar::Const`?
        let as_bool = |x: &scalar::Const| match *x {
            scalar::Const::FALSE => Some(false),
            scalar::Const::TRUE => Some(true),
            _ => None,
        };

        match self.op {
            PureOp::IntBinOpConstRhs(op, ct_rhs) => {
                let ct_rhs = ct_rhs.as_scalar(cx)?;
                match (op, ct_rhs.bits()) {
                    (
                        scalar::IntBinOp::Add
                        | scalar::IntBinOp::Sub
                        | scalar::IntBinOp::ShrU
                        | scalar::IntBinOp::ShrS
                        | scalar::IntBinOp::Shl
                        | scalar::IntBinOp::Or
                        | scalar::IntBinOp::Xor,
                        0,
                    )
                    | (
                        scalar::IntBinOp::Mul | scalar::IntBinOp::DivU | scalar::IntBinOp::DivS,
                        1,
                    ) => Some(ReductionStep::Complete(self.input)),

                    (scalar::IntBinOp::And, mask)
                        if mask == (!0 >> (128 - ct_rhs.ty().bit_width())) =>
                    {
                        Some(ReductionStep::Complete(self.input))
                    }

                    (scalar::IntBinOp::GeU, 0) => Some(ReductionStep::Complete(Value::Const(
                        cx.intern(scalar::Const::TRUE),
                    ))),
                    (scalar::IntBinOp::LtU, 0) => Some(ReductionStep::Complete(Value::Const(
                        cx.intern(scalar::Const::FALSE),
                    ))),

                    (scalar::IntBinOp::DivU, d) if d.is_power_of_two() => {
                        Some(ReductionStep::Partial(Reducible {
                            op: PureOp::IntBinOpConstRhs(
                                scalar::IntBinOp::ShrU,
                                cx.intern(scalar::Const::from_u32(d.trailing_zeros())),
                            ),
                            ..*self
                        }))
                    }
                    (scalar::IntBinOp::ModU, d) if d.is_power_of_two() => {
                        Some(ReductionStep::Partial(Reducible {
                            op: PureOp::IntBinOpConstRhs(
                                scalar::IntBinOp::And,
                                cx.intern(scalar::Const::from_bits(ct_rhs.ty(), d - 1)),
                            ),
                            ..*self
                        }))
                    }

                    _ => None,
                }
            }

            PureOp::BoolBinOpConstRhs(op, ct_rhs) => {
                Some(match (op, as_bool(ct_rhs.as_scalar(cx)?)?) {
                    (scalar::BoolBinOp::Eq, false) | (scalar::BoolBinOp::Ne, true) => {
                        ReductionStep::Partial(Reducible {
                            op: PureOp::BoolUnOp(scalar::BoolUnOp::Not),
                            ..*self
                        })
                    }

                    (scalar::BoolBinOp::Eq | scalar::BoolBinOp::And, true)
                    | (scalar::BoolBinOp::Ne | scalar::BoolBinOp::Or, false) => {
                        ReductionStep::Complete(self.input)
                    }

                    (scalar::BoolBinOp::Or, true) | (scalar::BoolBinOp::And, false) => {
                        ReductionStep::Complete(Value::Const(ct_rhs))
                    }
                })
            }

            _ => None,
        }
    }
}

impl Reducible<FuncAt<'_, DataInst>> {
    // FIXME(eddyb) force the input to actually be itself some kind of pure op.
    fn try_reduce_output_of_data_inst(
        &self,
        cx: &Context,
        type_of_value: impl Fn(Value) -> Type,
        output_idx: u32,
    ) -> Option<ReductionStep> {
        // HACK(eddyb) semi-convienient matchable `Reducible | DataInstDef`
        let input_reducible_or_inst_def = {
            let func_at_input_inst = self.input;
            let input_inst_def = func_at_input_inst.def();
            Reducible::try_from(func_at_input_inst).ok().ok_or_else(|| {
                (
                    &input_inst_def.kind,
                    &input_inst_def.inputs[..],
                    &input_inst_def.outputs[..],
                )
            })
        };

        if input_reducible_or_inst_def.is_ok() {
            assert_eq!(output_idx, 0);
        }

        // NOTE(eddyb) do not destroy information left in e.g. comments.
        #[allow(clippy::match_same_arms)]
        match (self.op, input_reducible_or_inst_def) {
            // HACK(eddyb) workaround for `ptr.is_null()` using `ptr as usize == 0`,
            // and also `enum`s overlapping pointers with unrelated integers.
            (
                PureOp::IntBinOpConstRhs(scalar::IntBinOp::Eq, ct_addr),
                Ok(Reducible {
                    op: PureOp::BitCast,
                    input: ptr_input,
                    ..
                }),
            ) if matches!(cx[type_of_value(ptr_input)].kind, TypeKind::QPtr) => {
                return Some(ReductionStep::Partial(Reducible {
                    op: PureOp::PtrEqAddr { addr: ct_addr },
                    output_type: self.output_type,
                    input: ptr_input,
                }));
            }

            (
                PureOp::BoolUnOp(scalar::BoolUnOp::Not),
                Ok(Reducible {
                    op: PureOp::BoolUnOp(scalar::BoolUnOp::Not),
                    input,
                    ..
                }),
            ) => {
                return Some(ReductionStep::Complete(input));
            }

            (
                PureOp::IntUnOp(_)
                | PureOp::IntBinOpConstRhs(..)
                | PureOp::BoolUnOp(_)
                | PureOp::BoolBinOpConstRhs(..),
                _,
            ) => {
                // FIXME(eddyb) reduce compositions.
            }

            (
                PureOp::BitCast,
                Ok(Reducible {
                    op: PureOp::BitCast,
                    input,
                    ..
                }),
            ) => {
                return Some(ReductionStep::Partial(self.with_input(input)));
            }
            (PureOp::BitCast, _) => {}

            (
                PureOp::VectorExtract {
                    elem_idx: extract_idx,
                },
                Err((
                    &DataInstKind::Vector(vector::Op::Whole(vector::WholeOp::Insert {
                        elem_idx: insert_idx,
                    })),
                    &[new_elem, prev_vector],
                    _,
                )),
            ) => {
                return Some(if insert_idx == extract_idx {
                    ReductionStep::Complete(new_elem)
                } else {
                    ReductionStep::Partial(self.with_input(prev_vector))
                });
            }
            (PureOp::VectorExtract { .. }, _) => {}

            (PureOp::IntToBool, _) => {
                // FIXME(eddyb) look into what instructions might end up
                // being used to transform booleans into integers.
            }

            (PureOp::IntToPtr, _) => {}
            (PureOp::PtrToInt, _) => {}
            (
                PureOp::PtrEqAddr { .. },
                Err((
                    DataInstKind::Mem(spirt::mem::MemOp::FuncLocalVar(_))
                    | DataInstKind::QPtr(
                        // FIXME(eddyb) some of these might need an "inbounds" flag.
                        spirt::qptr::QPtrOp::HandleArrayIndex
                        | spirt::qptr::QPtrOp::BufferData
                        | spirt::qptr::QPtrOp::Offset(_)
                        | spirt::qptr::QPtrOp::DynOffset { .. },
                    ),
                    ..,
                )),
            ) => {
                return Some(ReductionStep::Complete(Value::Const(
                    cx.intern(scalar::Const::FALSE),
                )));
            }
            (
                PureOp::PtrEqAddr { addr },
                Ok(Reducible {
                    op: PureOp::BitCast,
                    input,
                    ..
                }),
            ) if type_of_value(input) == cx[addr].ty => {
                return Some(ReductionStep::Partial(Reducible {
                    op: PureOp::IntBinOpConstRhs(scalar::IntBinOp::Eq, addr),
                    output_type: self.output_type,
                    input,
                }));
            }
            (PureOp::PtrEqAddr { .. }, _) => {}
        }

        None
    }
}

// FIXME(eddyb) `Result<Value, Incomplete<Reducible>>` is isomorphic to the
// existing `ReductionStep`, only reason to do it this way is the difference
// between a single step and a composition of steps, but that's not great.
#[derive(Copy, Clone)]
struct Incomplete<T>(T);

impl Reducible {
    // FIXME(eddyb) make this into some kind of local `ReduceCx` method.
    fn try_reduce(
        self,
        cx: &Context,
        // FIXME(eddyb) come up with a better convention for this!
        func: FuncAtMut<'_, ()>,

        parent_map: &ParentMap,

        cache: &mut FxHashMap<Self, Option<Result<Value, Incomplete<Self>>>>,
    ) -> Option<Value> {
        self.try_reduce_to_value_or_incomplete(cx, func, parent_map, cache)?
            .ok()
    }

    // FIXME(eddyb) make this into some kind of local `ReduceCx` method.
    fn try_reduce_to_value_or_incomplete(
        self,
        cx: &Context,
        // FIXME(eddyb) come up with a better convention for this!
        mut func: FuncAtMut<'_, ()>,

        parent_map: &ParentMap,

        cache: &mut FxHashMap<Self, Option<Result<Value, Incomplete<Self>>>>,
    ) -> Option<Result<Value, Incomplete<Self>>> {
        if let Some(&cached) = cache.get(&self) {
            return cached;
        }

        let result = match self.try_reduce_uncached_step(cx, func.reborrow(), parent_map, cache) {
            // FIXME(eddyb) actually use a loop instead of recursing here,
            // but that can't easily handle caching every single step.
            Some(ReductionStep::Partial(redu)) => Some(
                redu.try_reduce_to_value_or_incomplete(cx, func, parent_map, cache)
                    .unwrap_or(Err(Incomplete(redu))),
            ),
            Some(ReductionStep::Complete(v)) => Some(Ok(v)),
            None => None,
        };

        cache.insert(self, result);

        result
    }

    // FIXME(eddyb) make this into some kind of local `ReduceCx` method.
    fn try_reduce_uncached_step(
        self,
        cx: &Context,
        // FIXME(eddyb) come up with a better convention for this!
        mut func: FuncAtMut<'_, ()>,

        parent_map: &ParentMap,

        cache: &mut FxHashMap<Self, Option<Result<Value, Incomplete<Self>>>>,
    ) -> Option<ReductionStep> {
        let as_const = |v: Value| match v {
            Value::Const(ct) => Some(ct),
            Value::Var(_) => None,
        };

        {
            let func = func.reborrow().freeze();
            if let Some(step) = self.try_reduce_shallow(cx, |v| func.at(v).type_of(cx)) {
                return Some(step);
            }
        }

        let input = match self.input {
            Value::Const(_) => return None,
            Value::Var(v) => v,
        };

        match func.vars[input].kind() {
            VarKind::RegionInput {
                region,
                input_idx: state_idx,
            } => {
                // HACK(eddyb) avoid generating `bool.not` instructions in loops,
                // which could lead to endless cycling (see `FlipIfElseCond`).
                if let PureOp::BoolUnOp(scalar::BoolUnOp::Not) = self.op {
                    return None;
                }

                let loop_node = *parent_map.region_parent.get(region)?;

                let input_from_initial_state =
                    func.reborrow().at(loop_node).def().inputs[state_idx as usize];
                let input_from_updated_state =
                    func.reborrow().at(region).def().outputs[state_idx as usize];

                let output_from_initial_state = self
                    .with_input(input_from_initial_state)
                    .try_reduce(cx, func.reborrow(), parent_map, cache)?;
                // HACK(eddyb) this is here because it can fail, see the comment
                // on `output_from_updated_state` for what's actually going on.
                let (output_from_updated_state_inst, output_from_updated_state) = self
                    .with_input(input_from_updated_state)
                    .try_materialize_as_new_node_and_output(cx, func.reborrow())?;

                // Now that the reduction succeeded for the initial state,
                // we can proceed with augmenting the loop with the extra state.
                func.reborrow()
                    .at(loop_node)
                    .def()
                    .inputs
                    .push(output_from_initial_state);

                let loop_state_vars = &mut func.regions[region].inputs;
                let new_loop_state_var = func.vars.define(
                    cx,
                    VarDecl {
                        attrs: Default::default(),
                        ty: self.output_type,
                        def_parent: Either::Left(region),
                        def_idx: loop_state_vars.len().try_into().unwrap(),
                    },
                );
                loop_state_vars.push(new_loop_state_var);

                // HACK(eddyb) generating the instruction wholesale again is not
                // the most efficient way to go about this, but avoiding getting
                // stuck in a loop while processing a loop is also important.
                //
                // FIXME(eddyb) attempt to replace this with early-inserting in
                // `cache` *then* returning.
                func.reborrow()
                    .at(region)
                    .def()
                    .outputs
                    .push(Value::Var(output_from_updated_state));

                func.regions[region]
                    .children
                    .insert_last(output_from_updated_state_inst, func.nodes);

                Some(ReductionStep::Complete(Value::Var(new_loop_state_var)))
            }
            VarKind::NodeOutput { node, output_idx } => {
                let node_def = &*func.reborrow().at(node).def();

                if let NodeKind::Select(_) = node_def.kind {
                    // FIXME(eddyb) remove all the cloning and undo additions of new
                    // outputs "upstream", if they end up unused (or let DCE do it?).
                    let cases = node_def.child_regions.clone();
                    let per_case_new_output: SmallVec<[_; 2]> = cases
                        .iter()
                        .map(|&case| {
                            let per_case_input =
                                func.reborrow().at(case).def().outputs[output_idx as usize];
                            self.with_input(per_case_input).try_reduce(
                                cx,
                                func.reborrow(),
                                parent_map,
                                cache,
                            )
                        })
                        .collect::<Option<_>>()?;

                    // Try to avoid introducing a new output, by reducing the merge
                    // of the per-case output values to a single value, if possible.
                    {
                        let func = func.reborrow().freeze();
                        let node_def = func.at(node).def();
                        let kind = match &node_def.kind {
                            NodeKind::Select(kind) => kind,
                            _ => unreachable!(),
                        };
                        if let Some(v) = try_reduce_select(
                            cx,
                            parent_map,
                            func,
                            Some(node),
                            kind,
                            node_def.inputs[0],
                            per_case_new_output.iter().copied(),
                        ) {
                            return Some(ReductionStep::Complete(v));
                        }

                        // HACK(eddyb) avoid adding boolean constants to the cases of
                        // any `if`-`else`s just to negate the original condition.
                        if let (SelectionKind::BoolCond, [t, e]) = (kind, &per_case_new_output[..])
                            && [t, e].map(|&x| as_const(x)?.as_scalar(cx))
                                == [Some(&scalar::Const::FALSE), Some(&scalar::Const::TRUE)]
                        {
                            return Some(ReductionStep::Partial(Reducible {
                                op: PureOp::BoolUnOp(scalar::BoolUnOp::Not),
                                output_type: self.output_type,
                                input: node_def.inputs[0],
                            }));
                        }
                    }

                    // HACK(eddyb) avoid generating e.g. `if ... { false } else { true }`,
                    // which could lead to endless cycling (see `FlipIfElseCond`).
                    if let PureOp::BoolUnOp(scalar::BoolUnOp::Not) = self.op {
                        return None;
                    }

                    // Merge the per-case output values into a new output.
                    let node_output_vars = &mut func.nodes[node].outputs;
                    let new_output_var = func.vars.define(
                        cx,
                        VarDecl {
                            attrs: Default::default(),
                            ty: self.output_type,
                            def_parent: Either::Right(node),
                            def_idx: node_output_vars.len().try_into().unwrap(),
                        },
                    );
                    node_output_vars.push(new_output_var);
                    for (&case, new_output) in cases.iter().zip_eq(per_case_new_output) {
                        let per_case_outputs = &mut func.regions[case].outputs;
                        per_case_outputs.push(new_output);
                        assert_eq!(per_case_outputs.len(), node_output_vars.len());
                    }
                    Some(ReductionStep::Complete(Value::Var(new_output_var)))
                } else {
                    // HACK(eddyb) special-casing `OpSelect` like this shouldn't even
                    // be needed, it should be replaced with `Select` nodes.
                    if let DataInstKind::SpvInst(spv_inst, lowering) = &node_def.kind {
                        let wk = &super::SpvSpecWithExtras::get().well_known;
                        if spv_inst.opcode == wk.OpSelect
                            && lowering.disaggregated_output.is_none()
                            && lowering.disaggregated_inputs.is_empty()
                        {
                            let select_cond = node_def.inputs[0];
                            let per_case_inputs = [node_def.inputs[1], node_def.inputs[2]];
                            let per_case_new_output = per_case_inputs.map(|per_case_input| {
                                self.with_input(per_case_input).try_reduce(
                                    cx,
                                    func.reborrow(),
                                    parent_map,
                                    cache,
                                )
                            });
                            if let [Some(t), Some(e)] = per_case_new_output {
                                if let Some(new_output) = try_reduce_select(
                                    cx,
                                    parent_map,
                                    func.reborrow().freeze(),
                                    None,
                                    &SelectionKind::BoolCond,
                                    select_cond,
                                    [t, e].into_iter(),
                                ) {
                                    return Some(ReductionStep::Complete(new_output));
                                }

                                // FIXME(eddyb) this should be part of `try_reduce_select`.
                                if [t, e].map(|x| as_const(x)?.as_scalar(cx))
                                    == [Some(&scalar::Const::FALSE), Some(&scalar::Const::TRUE)]
                                {
                                    return Some(ReductionStep::Partial(Reducible {
                                        op: PureOp::BoolUnOp(scalar::BoolUnOp::Not),
                                        output_type: self.output_type,
                                        input: select_cond,
                                    }));
                                }
                            }
                        }
                    }

                    let func = func.reborrow().freeze();
                    self.with_input(func.at(node))
                        .try_reduce_output_of_data_inst(cx, |v| func.at(v).type_of(cx), output_idx)
                }
            }
        }
    }
}
