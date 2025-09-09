use itertools::{Either, Itertools as _};
use rustc_data_structures::fx::FxHashMap;
use smallvec::SmallVec;
use spirt::cf::SelectionKind;
use spirt::func_at::{FuncAt, FuncAtMut};
use spirt::transform::InnerInPlaceTransform;
use spirt::visit::InnerVisit;
use spirt::{
    Const, ConstDef, ConstKind, Context, DataInst, DataInstDef, DataInstKind,
    EntityOrientedDenseMap, FuncDefBody, Node, NodeDef, NodeKind, Region, Type, Value, Var,
    VarDecl, VarKind, scalar, spv, vector,
};
use std::iter;

use super::{ReplaceValueWith, VisitAllRegionsAndNodes};

/// Apply "reduction rules" to `func_def_body`, replacing (pure) computations
/// with one of their inputs or a constant (e.g. `x + 0 => x` or `1 + 2 => 3`),
/// and at most only adding more `Select` outputs/`Loop` state (where necessary)
/// but never any new instructions (unlike e.g. LLVM's instcombine).
pub(crate) fn reduce_in_func(cx: &Context, func_def_body: &mut FuncDefBody) {
    let wk = &super::SpvSpecWithExtras::get().well_known;

    let parent_map = ParentMap::new(func_def_body);

    // FIXME(eddyb) perhaps come up with a centralized abstraction for this
    // (in theory `VarDecl`s could indicate aliases, but that's a tradeoff).
    let mut var_replacements: EntityOrientedDenseMap<Var, Value> = Default::default();

    // HACK(eddyb) `EntityOrientedDenseMap` doesn't track its own count.
    let mut var_replacement_count = 0;

    let mut reduction_cache = FxHashMap::default();

    // HACK(eddyb) this is an annoying workaround for iterator invalidation
    // (SPIR-T iterators don't cope well with the underlying data changing).
    //
    // FIXME(eddyb) replace SPIR-T `FuncAtMut<EntityListIter<T>>` with some
    // kind of "list cursor", maybe even allowing removal during traversal.
    let mut reduction_queue = vec![];

    #[derive(Copy, Clone)]
    enum ReductionTarget {
        /// Replace uses of a `DataInst` with a reduced `Value`.
        DataInst(DataInst),

        /// Replace an `OpSwitch` `Node` with an `if`-`else` one.
        //
        // HACK(eddyb) see comment in `handle_node` for more details.
        SwitchToIfElse(Node),
    }

    loop {
        let old_var_replacement_count = var_replacement_count;

        // HACK(eddyb) we want to transform `DataInstDef`s, while having the ability
        // to (mutably) traverse the function, but `in_place_transform_data_inst_def`
        // only gives us a `&mut DataInstDef` (without the `FuncAtMut` around it).
        //
        // HACK(eddyb) ignore the above, for now it's pretty bad due to iterator
        // invalidation (see comment on `let reduction_queue` too).
        let mut handle_node = |func_at_node: FuncAt<'_, Node>| match func_at_node.def() {
            NodeDef {
                attrs: _,
                kind: NodeKind::Select(kind),
                inputs,
                child_regions: cases,
                outputs,
            } => {
                // FIXME(eddyb) this should probably be ran in the queue loop
                // below, to more quickly benefit from previous reductions.
                for (i, &output_var) in outputs.iter().enumerate() {
                    if let entry @ None = var_replacements.entry(output_var) {
                        let per_case_value = cases
                            .iter()
                            .map(|&case| func_at_node.at(case).def().outputs[i]);
                        if let Some(reduced) = try_reduce_select(
                            cx,
                            func_at_node.at(()),
                            &parent_map,
                            func_at_node.position,
                            kind,
                            inputs[0],
                            per_case_value,
                        ) {
                            *entry = Some(reduced);

                            // FIXME(eddyb) manual tracking is fragile.
                            var_replacement_count += 1;
                        }
                    }
                }

                // HACK(eddyb) turn `switch x { 0 => A, 1 => B, _ => ... }`
                // into `if ... {B} else {A}`, when `x` ends up limited in `0..=1`,
                // (such `switch`es come from e.g. `match`-ing enums w/ 2 variants)
                // allowing us to bypass SPIR-T current (and temporary) lossiness
                // wrt `_ => OpUnreachable` (i.e. we prove the default case can't
                // be entered based on `x` not having values other than `0` or `1`)
                if let SelectionKind::Switch { case_consts } = kind
                    && cases.len() == 3
                {
                    let case_consts: &[_; 2] = case_consts[..].try_into().unwrap();

                    // FIMXE(eddyb) support more values than just `0..=1`.
                    if case_consts.map(|ct| ct.int_as_u32()) == [Some(0), Some(1)] {
                        let redu = Reducible {
                            op: PureOp::IntToBool,
                            output_type: cx.intern(scalar::Type::Bool),
                            input: inputs[0],
                        };
                        let redu_target = ReductionTarget::SwitchToIfElse(func_at_node.position);
                        reduction_queue.push((redu_target, redu));
                    }
                }
            }

            NodeDef {
                kind: NodeKind::Loop { .. },
                inputs,
                child_regions,
                ..
            } => {
                let body = child_regions[0];

                // FIXME(eddyb) this should probably be ran in the queue loop
                // below, to more quickly benefit from previous reductions.
                let body_def = func_at_node.at(body).def();
                for (&body_input_var, (&initial_input, &body_output)) in body_def
                    .inputs
                    .iter()
                    .zip_eq(inputs.iter().zip_eq(&body_def.outputs))
                {
                    if body_output == Value::Var(body_input_var)
                        && let entry @ None = var_replacements.entry(body_input_var)
                    {
                        *entry = Some(initial_input);

                        // FIXME(eddyb) manual tracking is fragile.
                        var_replacement_count += 1;
                    }
                }
            }

            &NodeDef {
                kind: NodeKind::ExitInvocation { .. },
                ..
            } => {}

            &DataInstDef {
                kind:
                    DataInstKind::Scalar(_)
                    | DataInstKind::Vector(_)
                    | DataInstKind::FuncCall(_)
                    | DataInstKind::Mem(_)
                    | DataInstKind::QPtr(_)
                    | DataInstKind::ThunkBind(_)
                    | DataInstKind::SpvInst { .. }
                    | DataInstKind::SpvExtInst { .. },
                ..
            } => {
                if let Ok(redu) = Reducible::try_from(func_at_node) {
                    let redu_target = ReductionTarget::DataInst(func_at_node.position);
                    reduction_queue.push((redu_target, redu));
                }
            }
        };
        func_def_body.inner_visit_with(&mut VisitAllRegionsAndNodes {
            state: (),
            visit_region: |_: &mut (), _| {},
            visit_node: |_: &mut (), func_at_node| {
                handle_node(func_at_node);
            },
        });

        // FIXME(eddyb) should this loop become the only loop, by having loop
        // reductions push the new instruction to `reduction_queue`? the problem
        // then is that it's not trivial to figure out what else might benefit
        // from another full scan, so perhaps the only solution is "demand-driven"
        // (recursing into use->def, instead of processing defs).
        let mut any_changes = false;
        for (redu_target, redu) in reduction_queue.drain(..) {
            if let Some(v) = redu.try_reduce(
                cx,
                func_def_body.at_mut(()),
                &var_replacements,
                &parent_map,
                &mut reduction_cache,
            ) {
                any_changes = true;
                match redu_target {
                    ReductionTarget::DataInst(inst) => {
                        let inst_def = func_def_body.at_mut(inst).def();
                        let newly_inserted =
                            var_replacements.insert(inst_def.outputs[0], v).is_none();
                        if newly_inserted {
                            // FIXME(eddyb) manual tracking is fragile.
                            var_replacement_count += 1;
                        }

                        // Replace the reduced `DataInstDef` itself with `OpNop`,
                        // removing the ability to use its "name" as a value.
                        //
                        // FIXME(eddyb) remove the instruction entirely.
                        *inst_def = DataInstDef {
                            attrs: Default::default(),
                            kind: DataInstKind::SpvInst(
                                wk.OpNop.into(),
                                spv::InstLowering::default(),
                            ),
                            inputs: [].into_iter().collect(),
                            child_regions: [].into_iter().collect(),
                            outputs: [].into_iter().collect(),
                        };
                    }

                    // HACK(eddyb) see comment in `handle_node` for more details.
                    ReductionTarget::SwitchToIfElse(node) => {
                        let node_def = func_def_body.at_mut(node).def();
                        match node_def.child_regions[..] {
                            [case_0, case_1, _default] => {
                                node_def.kind = NodeKind::Select(SelectionKind::BoolCond);
                                node_def.inputs[0] = v;
                                node_def.child_regions = [case_1, case_0].iter().copied().collect();
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
        }

        if !any_changes && old_var_replacement_count == var_replacement_count {
            break;
        }

        func_def_body.inner_in_place_transform_with(&mut ReplaceValueWith(|mut v| {
            let old = v;
            loop {
                match v {
                    Value::Const(_) => break,
                    Value::Var(var) => match var_replacements.get(var) {
                        Some(&new) => v = new,
                        None => break,
                    },
                }
            }
            if v != old {
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
            visit_region: |this: &mut Self, func_at_region: FuncAt<'_, Region>| {
                for func_at_child_node in func_at_region.at_children() {
                    this.node_parent
                        .insert(func_at_child_node.position, func_at_region.position);
                }
            },
            visit_node: |this: &mut Self, func_at_node: FuncAt<'_, Node>| {
                for &child_region in &func_at_node.def().child_regions {
                    this.region_parent
                        .insert(child_region, func_at_node.position);
                }
            },
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

    // FIXME(eddyb) come up with a better convention for this!
    func: FuncAt<'_, ()>,

    parent_map: &ParentMap,
    select_node: Node,
    // FIXME(eddyb) are these redundant with the `Node` above?
    kind: &SelectionKind,
    scrutinee: Value,
    cases: impl Iterator<Item = Value>,
) -> Option<Value> {
    let as_const = |v: Value| match v {
        Value::Const(ct) => Some(ct),
        Value::Var(_) => None,
    };

    // Ignore `undef`s, as they can be legally substituted with any other value.
    let mut first_undef = None;
    let mut non_undef_cases = cases.filter(|&case| {
        let is_undef = as_const(case).map(|ct| &cx[ct].kind) == Some(&ConstKind::Undef);
        if is_undef && first_undef.is_none() {
            first_undef = Some(case);
        }
        !is_undef
    });
    // FIXME(eddyb) false positive (no pre-existing tuple, only multi-value `match`ing).
    #[allow(clippy::tuple_array_conversions)]
    match (non_undef_cases.next(), non_undef_cases.next()) {
        (None, _) => first_undef,

        // `Select(c: bool, true, false)` can be replaced with just `c`.
        (Some(x), Some(y))
            if matches!(kind, SelectionKind::BoolCond)
                && [x, y].map(|v| as_const(v)?.as_scalar(cx))
                    == [Some(&scalar::Const::TRUE), Some(&scalar::Const::FALSE)] =>
        {
            assert!(non_undef_cases.next().is_none() && first_undef.is_none());

            Some(scrutinee)
        }

        (Some(x), y) => {
            if y.into_iter().chain(non_undef_cases).all(|z| z == x) {
                // HACK(eddyb) closure here serves as `try` block.
                let is_x_valid_outside_select = || {
                    // Constants are always valid.
                    let x = match x {
                        Value::Const(_) => return Some(()),
                        Value::Var(x) => x,
                    };

                    // HACK(eddyb) if the same value appears in two different
                    // cases, it's definitely dominating the whole `Select`.
                    if y.is_some() {
                        return Some(());
                    }

                    // In general, `x` dominating the `Select` is what would
                    // allow lifting an use of it outside the `Select`.
                    let region_defining_x = match func.vars[x].kind() {
                        VarKind::RegionInput { region, .. } => region,
                        VarKind::NodeOutput { node, .. } => *parent_map.node_parent.get(node)?,
                    };

                    // Fast-reject: if `x` is defined immediately inside one of
                    // `select_node`'s cases, it's not a dominator.
                    if parent_map.region_parent.get(region_defining_x) == Some(&select_node) {
                        return None;
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
                if is_x_valid_outside_select().is_some() {
                    return Some(x);
                }
            }

            None
        }
    }
}

/// Pure operation that transforms one `Value` into another `Value`.
//
// FIXME(eddyb) move this elsewhere? also, how should binops etc. be supported?
// (one approach could be having a "focus input" that can be dynamic, with the
// other inputs being `Const`s, i.e. partially applying all but one input)
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum PureOp {
    BitCast,
    // FIXME(eddyb) include all of `vector::Op` (or obsolete with `flow`).
    VectorExtract {
        elem_idx: u8,
    },

    /// Maps `0` to `false`, and `1` to `true`, but any other input values won't
    /// allow reduction, which is used to signal `0..=1` isn't being guaranteed.
    //
    // HACK(eddyb) not a real operation, but a helper used to extract a `bool`
    // equivalent for an `OpSwitch`'s scrutinee.
    // FIXME(eddyb) proper SPIR-T range analysis should be implemented and such
    // a reduction not attempted at all if the range is larger than `0..=1`
    // (also, the actual operation can be replaced with `x == 1` or `x != 0`)
    IntToBool,
}

impl TryFrom<&DataInstKind> for PureOp {
    type Error = ();
    fn try_from(kind: &DataInstKind) -> Result<Self, ()> {
        match kind {
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
            PureOp::BitCast => (wk.OpBitcast, iter::empty().collect()),
            PureOp::VectorExtract { elem_idx } => {
                return Ok(vector::Op::from(vector::WholeOp::Extract { elem_idx }).into());
            }

            // HACK(eddyb) this is the only reason this is `TryFrom` not `From`.
            PureOp::IntToBool => return Err(()),
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
        let op = PureOp::try_from(&inst_def.kind)?;

        // HACK(eddyb) all supported instructions should be single-output.
        assert_eq!(inst_def.outputs.len(), 1);

        let output_type = func_at_inst.at(inst_def.outputs[0]).decl().ty;
        if let [input] = inst_def.inputs[..] {
            return Ok(Self {
                op,
                output_type,
                input,
            });
        }
        Err(())
    }
}

impl Reducible {
    // HACK(eddyb) `IntToBool` is the only reason this can return `None`.
    fn try_materialize_as_new_node_and_output(
        self,
        cx: &Context,
        func: FuncAtMut<'_, ()>,
    ) -> Option<(Node, Var)> {
        let Self {
            op,
            output_type,
            input,
        } = self;

        let node = func.nodes.define(
            cx,
            NodeDef {
                attrs: Default::default(),
                kind: op.try_into().ok()?,
                inputs: [input].into_iter().collect(),
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
                ty: output_type,
                def_parent: Either::Right(node),
                def_idx: 0,
            },
        );
        func.nodes[node].outputs.push(output_var);

        Some((node, output_var))
    }
}

impl Reducible<Const> {
    // FIXME(eddyb) in theory this should always return `Some`.
    fn try_reduce_const(&self, cx: &Context) -> Option<Const> {
        let _wk = &super::SpvSpecWithExtras::get().well_known;

        let ct_def = &cx[self.input];
        match (self.op, &ct_def.kind) {
            (_, ConstKind::Undef) => Some(cx.intern(ConstDef {
                attrs: ct_def.attrs,
                ty: self.output_type,
                kind: ct_def.kind.clone(),
            })),

            (PureOp::BitCast, ConstKind::Scalar(ct)) => {
                let output_type = self.output_type.as_scalar(cx)?;
                if ct.ty().bit_width() == output_type.bit_width() {
                    Some(cx.intern(ConstDef {
                        attrs: ct_def.attrs,
                        ty: self.output_type,
                        kind: ConstKind::Scalar(scalar::Const::try_from_bits(
                            output_type,
                            ct.bits(),
                        )?),
                    }))
                } else {
                    None
                }
            }

            (PureOp::VectorExtract { elem_idx }, ConstKind::Vector(ct)) => {
                Some(cx.intern(ct.get_elem(elem_idx.into())?))
            }

            (PureOp::IntToBool, ConstKind::Scalar(ct)) => {
                Some(cx.intern(scalar::Const::try_from_bits(scalar::Type::Bool, ct.bits())?))
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

impl Reducible<&DataInstDef> {
    // FIXME(eddyb) force the input to actually be itself some kind of pure op.
    fn try_reduce_output_of_data_inst(
        &self,
        _cx: &Context,
        _output_idx: u32,
    ) -> Option<ReductionStep> {
        let _wk = &super::SpvSpecWithExtras::get().well_known;

        let input_inst_def = self.input;
        // NOTE(eddyb) do not destroy information left in e.g. comments.
        #[allow(clippy::match_same_arms)]
        match (self.op, &input_inst_def.kind) {
            (PureOp::BitCast, _) => {
                // FIXME(eddyb) reduce chains of bitcasts.
            }

            (
                PureOp::VectorExtract {
                    elem_idx: extract_idx,
                },
                &DataInstKind::Vector(vector::Op::Whole(vector::WholeOp::Insert {
                    elem_idx: insert_idx,
                })),
            ) => {
                let new_elem = input_inst_def.inputs[0];
                let prev_vector = input_inst_def.inputs[1];
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
        }

        None
    }
}

impl Reducible {
    // FIXME(eddyb) make this into some kind of local `ReduceCx` method.
    fn try_reduce(
        mut self,
        cx: &Context,
        // FIXME(eddyb) come up with a better convention for this!
        func: FuncAtMut<'_, ()>,

        var_replacements: &EntityOrientedDenseMap<Var, Value>,

        parent_map: &ParentMap,

        cache: &mut FxHashMap<Self, Option<Value>>,
    ) -> Option<Value> {
        // FIXME(eddyb) should we care about the cache *before* this loop below?

        // HACK(eddyb) eagerly apply `var_replacements`.
        // FIXME(eddyb) this could do the union-find trick of shortening chains
        // the first time they're encountered, but also, if this process was more
        // "demand-driven" (recursing into use->def, instead of processing defs),
        // it might not require any of this complication.
        loop {
            let replacement = match self.input {
                Value::Const(_) => None,
                Value::Var(v) => var_replacements.get(v),
            };
            match replacement {
                Some(&replacement) => self.input = replacement,
                None => break,
            }
        }

        if let Some(&cached) = cache.get(&self) {
            return cached;
        }

        let result = self.try_reduce_uncached(cx, func, var_replacements, parent_map, cache);

        cache.insert(self, result);

        result
    }

    // FIXME(eddyb) make this into some kind of local `ReduceCx` method.
    fn try_reduce_uncached(
        self,
        cx: &Context,
        // FIXME(eddyb) come up with a better convention for this!
        mut func: FuncAtMut<'_, ()>,

        var_replacements: &EntityOrientedDenseMap<Var, Value>,

        parent_map: &ParentMap,

        cache: &mut FxHashMap<Self, Option<Value>>,
    ) -> Option<Value> {
        let input = match self.input {
            Value::Const(ct) => return self.with_input(ct).try_reduce_const(cx).map(Value::Const),
            Value::Var(v) => v,
        };

        match func.vars[input].kind() {
            VarKind::RegionInput {
                region,
                input_idx: state_idx,
            } => {
                let loop_node = *parent_map.region_parent.get(region)?;

                let input_from_initial_state =
                    func.reborrow().at(loop_node).def().inputs[state_idx as usize];
                let input_from_updated_state =
                    func.reborrow().at(region).def().outputs[state_idx as usize];

                let output_from_initial_state = self
                    .with_input(input_from_initial_state)
                    .try_reduce(cx, func.reborrow(), var_replacements, parent_map, cache)?;
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

                Some(Value::Var(new_loop_state_var))
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
                                var_replacements,
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
                            func,
                            parent_map,
                            node,
                            kind,
                            node_def.inputs[0],
                            per_case_new_output.iter().copied(),
                        ) {
                            return Some(v);
                        }
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
                    for (&case, new_output) in cases.iter().zip(per_case_new_output) {
                        let per_case_outputs = &mut func.regions[case].outputs;
                        per_case_outputs.push(new_output);
                        assert_eq!(per_case_outputs.len(), node_output_vars.len());
                    }
                    Some(Value::Var(new_output_var))
                } else {
                    match self
                        .with_input(node_def)
                        .try_reduce_output_of_data_inst(cx, output_idx)?
                    {
                        ReductionStep::Complete(v) => Some(v),
                        // FIXME(eddyb) actually use a loop instead of recursing here.
                        ReductionStep::Partial(redu) => {
                            redu.try_reduce(cx, func, var_replacements, parent_map, cache)
                        }
                    }
                }
            }
        }
    }
}
