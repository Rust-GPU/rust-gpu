use itertools::{Either, Itertools as _};
use rustc_data_structures::fx::{FxHashMap, FxIndexSet};
use smallvec::SmallVec;
use spirt::func_at::{FuncAt, FuncAtMut};
use spirt::transform::{InnerInPlaceTransform as _, Transformed, Transformer};
use spirt::visit::InnerVisit as _;
use spirt::{
    Const, ConstDef, Context, DeclDef, EntityOrientedDenseMap, FuncDecl, Module, Node, NodeDef,
    NodeKind, Region, Type, TypeKind, Value, Var, VarDecl, mem::MemOp, scalar,
};
use std::cell::Cell;
use std::iter;
use std::num::{NonZeroI32, NonZeroU32};

pub trait Builder {
    type Value;
    type OutputDecl;

    fn type_of(&self, v: &Self::Value) -> Type;

    fn scalar_const(&self, ty: Type, ct: scalar::Const) -> Self::Value;

    fn op(
        &mut self,
        kind: impl Into<NodeKind>,
        inputs: impl IntoIterator<Item = Self::Value>,
        output_decls: impl IntoIterator<Item = Self::OutputDecl>,
    ) -> impl ExactSizeIterator<Item = Self::Value>;

    // FIXME(eddyb) figure out how to tell apart 1-output vs 2-output in naming.
    fn scalar_op(
        &mut self,
        kind: impl Into<scalar::Op>,
        inputs: impl IntoIterator<Item = Self::Value>,
        output_decl: Self::OutputDecl,
    ) -> Self::Value {
        let [output] = self
            .op(kind.into(), inputs, [output_decl])
            .collect_array()
            .unwrap();
        output
    }

    // HACK(eddyb) only necessary because `Builder` methods take `&mut self`.
    fn map_reduce<T, U>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        mut map: impl FnMut(&mut Self, T) -> U,
        mut reduce: impl FnMut(&mut Self, U, U) -> U,
    ) -> U {
        iter.into_iter()
            .fold(None, move |a, b| {
                let b = map(self, b);
                Some(match a {
                    Some(a) => reduce(self, a, b),
                    None => b,
                })
            })
            .unwrap()
    }

    // FIXME(eddyb) `OpSelect` is a problem (maybe SPIR-T should allow
    // a `Select` node with no child regions, and one input per case?).
    fn select(
        &mut self,
        cond: Self::Value,
        [a, b]: [Self::Value; 2],
        output_decl: Self::OutputDecl,
    ) -> Self::Value {
        let [output] = self
            .op(
                NodeKind::SpvInst(
                    super::SpvSpecWithExtras::get().well_known.OpSelect.into(),
                    Default::default(),
                ),
                [cond, a, b],
                [output_decl],
            )
            .collect_array()
            .unwrap();
        output
    }
}

// HACK(eddyb) this is only used to "probe" whether an expansion *could* succeed.
struct TypeOnlyBuilder;
impl Builder for TypeOnlyBuilder {
    type Value = Type;
    type OutputDecl = Type;

    fn type_of(&self, &ty: &Type) -> Type {
        ty
    }

    fn scalar_const(&self, ty: Type, _: scalar::Const) -> Type {
        ty
    }

    fn op(
        &mut self,
        _kind: impl Into<NodeKind>,
        _inputs: impl IntoIterator<Item = Type>,
        output_types: impl IntoIterator<Item = Type>,
    ) -> impl ExactSizeIterator<Item = Type> {
        output_types
            .into_iter()
            .collect::<SmallVec<[_; 2]>>()
            .into_iter()
    }
}

struct ConstBuilder<'a> {
    cx: &'a Context,
}
impl Builder for ConstBuilder<'_> {
    type Value = Result<Const, Type>;
    type OutputDecl = Type;

    fn type_of(&self, ct_or_ty: &Result<Const, Type>) -> Type {
        ct_or_ty.map(|ct| self.cx[ct].ty).unwrap_or_else(|ty| ty)
    }

    fn scalar_const(&self, ty: Type, ct: scalar::Const) -> Result<Const, Type> {
        Ok(self.cx.intern(ConstDef {
            attrs: Default::default(),
            ty,
            kind: ct.into(),
        }))
    }

    fn op(
        &mut self,
        kind: impl Into<NodeKind>,
        inputs: impl IntoIterator<Item = Result<Const, Type>>,
        output_types: impl IntoIterator<Item = Type>,
    ) -> impl ExactSizeIterator<Item = Result<Const, Type>> {
        let output_types: SmallVec<[_; 2]> = output_types.into_iter().collect();

        // HACK(eddyb) ad-hoc `try {...}` equivalent.
        let const_outputs = (|| {
            // FIXME(eddyb) support more kinds of const-foldable ops.
            match kind.into() {
                NodeKind::Scalar(op) => {
                    let inputs: SmallVec<[_; 2]> = inputs
                        .into_iter()
                        .map(|input| Some(*input.ok()?.as_scalar(self.cx)?))
                        .collect::<Option<_>>()?;
                    let output_types: SmallVec<[_; 2]> = output_types
                        .iter()
                        .map(|ty| ty.as_scalar(self.cx))
                        .collect::<Option<_>>()?;
                    Some(
                        op.try_eval(&inputs, &output_types)
                            .ok()?
                            .into_iter()
                            .map(|output| self.cx.intern(output))
                            .collect::<SmallVec<[_; 2]>>(),
                    )
                }
                // FIXME(eddyb) `OpSelect` is a problem (maybe SPIR-T should allow
                // a `Select` node with no child regions, and one input per case?).
                NodeKind::SpvInst(spv_inst, _)
                    if spv_inst.opcode == super::SpvSpecWithExtras::get().well_known.OpSelect =>
                {
                    let inputs: SmallVec<[_; 3]> = inputs
                        .into_iter()
                        .map(|input| Some(*input.ok()?.as_scalar(self.cx)?))
                        .collect::<Option<_>>()?;
                    match inputs[..] {
                        [scalar::Const::TRUE, x, _] | [scalar::Const::FALSE, _, x] => {
                            Some([self.cx.intern(x)].into_iter().collect())
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        })();

        if let Some(const_outputs) = &const_outputs {
            assert_eq!(output_types.len(), const_outputs.len());
        }

        output_types
            .into_iter()
            .enumerate()
            .map(move |(i, ty)| Ok(const_outputs.as_ref().ok_or(ty)?[i]))
    }
}

// FIXME(eddyb) use this to allow `Var` reuse.
#[derive(Copy, Clone)]
enum ReuseOrNew<R, N> {
    Reuse(R),
    New(N),
}

// FIXME(eddyb) reconsider this name/API, maybe centralize it?
#[derive(Copy, Clone)]
enum InsertAnchor {
    /// Inserts a `node` at the start of the region, then becomes `After(node)`
    /// (in order to preserve the relative order of multiple inserted nodes).
    First,

    /// Inserts each new `node` after the existing one, then becomes `After(node)`
    /// (in order to preserve the relative order of multiple inserted nodes).
    After(Node),

    /// Inserts each new node before the existing one, without any updates
    /// (the relative order of multiple inserted nodes is inherently preserved).
    Before(Node),

    /// Inserts each new node at the end of the region, without any updates
    /// (the relative order of multiple inserted nodes is inherently preserved).
    Last,
}

struct NodeBuilder<'a> {
    cx: &'a Context,
    func: FuncAtMut<'a, ()>,

    parent_region: Region,
    anchor: InsertAnchor,
    //
    // FIXME(eddyb) also track a "pending `NodeDef`", to allow automatically
    // reusing the `Node` being expanded, without allocating a new one.
}
impl NodeBuilder<'_> {
    fn insert_new_node_into_parent_region(&mut self, new_node: Node) {
        let nodes = &mut self.func.regions[self.parent_region].children;
        match self.anchor {
            InsertAnchor::First => {
                nodes.insert_first(new_node, self.func.nodes);
                self.anchor = InsertAnchor::After(new_node);
            }
            InsertAnchor::After(prev) => {
                nodes.insert_after(new_node, prev, self.func.nodes);
                self.anchor = InsertAnchor::After(new_node);
            }
            InsertAnchor::Before(next) => nodes.insert_before(new_node, next, self.func.nodes),
            InsertAnchor::Last => nodes.insert_last(new_node, self.func.nodes),
        }
    }

    // HACK(eddyb) `self.func.reborrow().freeze().at(position)` w/o `&mut self`.
    fn func_at<P: Copy>(&self, position: P) -> FuncAt<'_, P> {
        let FuncAtMut {
            regions,
            nodes,
            vars,
            position: (),
        } = &self.func;
        FuncAt {
            regions,
            nodes,
            vars,
            position,
        }
    }
}
impl Builder for NodeBuilder<'_> {
    type Value = Value;
    type OutputDecl = Type;

    fn type_of(&self, &v: &Value) -> Type {
        self.func_at(v).type_of(self.cx)
    }

    fn scalar_const(&self, ty: Type, ct: scalar::Const) -> Value {
        Value::Const(self.cx.intern(ConstDef {
            attrs: Default::default(),
            ty,
            kind: ct.into(),
        }))
    }

    fn op(
        &mut self,
        kind: impl Into<NodeKind>,
        inputs: impl IntoIterator<Item = Value>,
        output_types: impl IntoIterator<Item = Type>,
    ) -> impl ExactSizeIterator<Item = Value> {
        let kind = kind.into();
        let inputs: SmallVec<[_; 2]> = inputs.into_iter().collect();
        let output_types: SmallVec<[_; 2]> = output_types.into_iter().collect();

        // HACK(eddyb) reusing `ConstBuilder` for const-folding.
        // FIXME(eddyb) also simplify some ops (e.g. `x >> 0`).
        let const_outputs = ConstBuilder { cx: self.cx }
            .op(
                kind.clone(),
                inputs.iter().map(|&v| match v {
                    Value::Const(ct) => Ok(ct),
                    Value::Var(var) => Err(self.func.vars[var].ty),
                }),
                output_types.iter().copied(),
            )
            .map(|r| r.map(Value::Const))
            .collect::<Result<SmallVec<[_; 2]>, _>>();
        if let Ok(const_outputs) = const_outputs {
            return const_outputs.into_iter();
        }

        let node = self.func.nodes.define(
            self.cx,
            NodeDef {
                // FIXME(eddyb) strongly consider copying debuginfo from the initial
                // instigating node (that resulted in the `NodeBuilder` being created).
                attrs: Default::default(),
                kind,
                inputs,
                child_regions: [].into_iter().collect(),
                outputs: [].into_iter().collect(),
            }
            .into(),
        );

        self.insert_new_node_into_parent_region(node);

        output_types
            .into_iter()
            .enumerate()
            .map(|(output_idx, ty)| {
                let output_var = self.func.vars.define(
                    self.cx,
                    VarDecl {
                        attrs: Default::default(),
                        ty,
                        def_parent: Either::Right(node),
                        def_idx: output_idx.try_into().unwrap(),
                    },
                );
                self.func.nodes[node].outputs.push(output_var);

                Value::Var(output_var)
            })
            .collect::<SmallVec<[_; 2]>>()
            .into_iter()
    }
}

// FIXME(eddyb) this should be `Either<V, ...>`, or a new `enum`, ideally.
#[derive(Clone)]
pub struct MaybeExpanded<V> {
    whole: Option<V>,
    // FIXME(eddyb) make the size (or e.g. `SmallVec` vs `ArrayVec`) configurable?
    expanded: Option<(Type, SmallVec<[V; 4]>)>,
}

impl<V> MaybeExpanded<V> {
    fn expanded(ty: Type, components: impl IntoIterator<Item = V>) -> Self {
        MaybeExpanded {
            whole: None,
            expanded: Some((ty, components.into_iter().collect())),
        }
    }
}

// HACK(eddyb) this is a limited form of expansion that is entirely based on
// `maybe_expand_type` deciding whether values of a type need to be expanded.
pub trait TypeDrivenExpansion {
    type Error;

    fn maybe_expand_type(&self, ty: Type) -> Option<impl ExactSizeIterator<Item = Type>>;

    // FIXME(eddyb) don't force this to succeed.
    // FIXME(eddyb) consider folding this into `try_expand_op` somehow.
    fn decompose<V: Copy>(
        &self,
        ty: Type,
        value: V,
        bld: &mut impl Builder<Value = V, OutputDecl = Type>,
    ) -> impl ExactSizeIterator<Item = V>;

    // FIXME(eddyb) don't force this to succeed.
    // FIXME(eddyb) consider folding this into `try_expand_op` somehow.
    fn recompose<V: Copy>(
        &self,
        ty: Type,
        components: impl ExactSizeIterator<Item = V>,
        bld: &mut impl Builder<Value = V, OutputDecl = Type>,
    ) -> V;

    fn try_expand_op<V: Copy>(
        &self,
        kind: &NodeKind,
        inputs: impl ExactSizeIterator<Item = MaybeExpanded<V>>,
        output_types: impl ExactSizeIterator<Item = Type>,
        bld: &mut impl Builder<Value = V, OutputDecl = Type>,
    ) -> Result<impl ExactSizeIterator<Item = MaybeExpanded<V>>, Self::Error>;

    // FIXME(eddyb) find a better place for this than a default method.

    fn expand_module(&self, module: &mut Module)
    where
        Self: Sized,
    {
        let cx = &module.cx();
        // FIXME(eddyb) also expand e.g. global initializers?

        // FIXME(eddyb) reuse this collection work in some kind of "pass manager".
        let all_funcs = {
            let mut collector = super::ReachableUseCollector {
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

        // FIXME(eddyb) should this emit a warning, error, SPIR-T BUG diagnostic,
        // assert, etc.?
        let any_unstructured_cfg = all_funcs.iter().any(|&func| match &module.funcs[func].def {
            DeclDef::Imported(_) => false,
            DeclDef::Present(func_def_body) => func_def_body.unstructured_cfg.is_some(),
        });
        if any_unstructured_cfg {
            return;
        }

        for func in all_funcs {
            IntraFuncExpander {
                cx,
                expansion: self,

                node_parent: EntityOrientedDenseMap::new(),
                maybe_expanded_vars: EntityOrientedDenseMap::new(),
                recomposed_var_cache: FxHashMap::default(),
                parent_region: None,
            }
            .in_place_transform_func_decl(&mut module.funcs[func]);
        }
    }
}

struct IntraFuncExpander<'a, E> {
    cx: &'a Context,
    expansion: &'a E,

    // HACK(eddyb) this is only used to determine into which `Region` to inject
    // any new `Node`s needed to `recompose` an expanded value.
    node_parent: EntityOrientedDenseMap<Node, Region>,

    // FIXME(eddyb) rework this to allow `Var` reuse, maybe?
    // FIXME(eddyb) how wasteful is this being an `EntityOrientedDenseMap`?
    // (only done for the performance opportunities)
    // FIXME(eddyb) `MaybeExpanded` had to be used here because even unexpanded
    // variables can get replaced due to the lack of `Var` reuse.
    maybe_expanded_vars: EntityOrientedDenseMap<Var, MaybeExpanded<Value>>,

    // FIXME(eddyb) this should *really* just reuse the original `Var`.
    recomposed_var_cache: FxHashMap<Var, Var>,

    parent_region: Option<Region>,
}

impl<E: TypeDrivenExpansion> IntraFuncExpander<'_, E> {
    fn maybe_expanded_value(&self, func_at_value: FuncAt<'_, Value>) -> MaybeExpanded<Value> {
        let ty = func_at_value.type_of(self.cx);
        let v = func_at_value.position;
        let maybe_expanded = match v {
            // FIXME(eddyb) consider caching this? (across functions)
            Value::Const(ct) => self.expansion.maybe_expand_type(ty).and_then(|_| {
                Some(MaybeExpanded::expanded(
                    ty,
                    self.expansion
                        .decompose(ty, Ok(ct), &mut ConstBuilder { cx: self.cx })
                        .map(|r| r.map(Value::Const))
                        .collect::<Result<SmallVec<[_; 4]>, _>>()
                        .ok()?,
                ))
            }),
            Value::Var(var) => self.maybe_expanded_vars.get(var).cloned(),
        };
        maybe_expanded.unwrap_or(MaybeExpanded {
            whole: Some(v),
            expanded: None,
        })
    }

    fn expand_vars_as_needed_by_types(
        &mut self,
        func: FuncAtMut<'_, ()>,
        vars_def_parent: Either<Region, Node>,
    ) {
        let vars = vars_def_parent.either(
            |region| &mut func.regions[region].inputs,
            |node| &mut func.nodes[node].outputs,
        );
        let mut replacement_vars = None::<SmallVec<[_; 2]>>;
        for (i, &var) in vars.iter().enumerate() {
            let ty = func.vars[var].ty;
            if let Some(component_types) = self.expansion.maybe_expand_type(ty) {
                // FIXME(eddyb) try reusing at least one of the original `Var`s?
                let replacement_vars =
                    replacement_vars.get_or_insert_with(|| vars[..i].iter().copied().collect());

                let new_var_start = replacement_vars.len();
                replacement_vars.extend(component_types.map(|ty| {
                    func.vars.define(
                        self.cx,
                        VarDecl {
                            attrs: Default::default(),
                            ty,
                            def_parent: vars_def_parent,
                            // HACK(eddyb) correct value filled in by a loop below.
                            def_idx: !0,
                        },
                    )
                }));
                let component_vars = &replacement_vars[new_var_start..];

                self.maybe_expanded_vars.insert(
                    var,
                    MaybeExpanded::expanded(ty, component_vars.iter().map(|&v| Value::Var(v))),
                );
            } else if let Some(replacement_vars) = &mut replacement_vars {
                replacement_vars.push(var);
            }
        }
        if let Some(replacement_vars) = replacement_vars {
            for (i, &var) in replacement_vars.iter().enumerate() {
                func.vars[var].def_idx = i.try_into().unwrap();
            }
            *vars = replacement_vars;
        }
    }

    fn expand_values_as_needed_by_types(
        &self,
        func: FuncAtMut<'_, ()>,
        values: impl Fn(FuncAtMut<'_, ()>) -> &mut SmallVec<[Value; 2]>,
        anchor: InsertAnchor,
    ) {
        let mut bld = NodeBuilder {
            cx: self.cx,
            func,
            parent_region: self.parent_region.unwrap(),
            anchor,
        };

        let mut replacement_values = None::<SmallVec<[_; 2]>>;
        for i in 0..values(bld.func.reborrow()).len() {
            let old_value = values(bld.func.reborrow())[i];
            let mut value = self.maybe_expanded_value(bld.func.reborrow().freeze().at(old_value));
            if let Some(ty) = value.whole.as_ref().map(|v| bld.type_of(v))
                && self.expansion.maybe_expand_type(ty).is_some()
            {
                value.expanded = Some((
                    ty,
                    self.expansion
                        .decompose(ty, value.whole.take().unwrap(), &mut bld)
                        .collect(),
                ));
            }

            if value.whole != Some(old_value) {
                replacement_values
                    .get_or_insert_with(|| {
                        values(bld.func.reborrow())[..i].iter().copied().collect()
                    })
                    .extend(
                        value.whole.into_iter().chain(
                            value
                                .expanded
                                .into_iter()
                                .flat_map(|(_, components)| components),
                        ),
                    );
            } else if let Some(replacement_values) = &mut replacement_values {
                replacement_values.push(old_value);
            }
        }
        if let Some(replacement_values) = replacement_values {
            *values(bld.func.reborrow()) = replacement_values;
        }
    }
}

impl<E: TypeDrivenExpansion> Transformer for IntraFuncExpander<'_, E> {
    fn transform_value_use(&mut self, v: &Value) -> Transformed<Value> {
        match *v {
            Value::Const(_) => {}
            Value::Var(v) => {
                // FIXME(eddyb) this lacks access to the `FuncAtMut` to be able
                // to recompose the value on-demand.
                // FIXME(eddyb) in theory, this could handle `whole: Some(_)`.
                assert!(self.maybe_expanded_vars.get(v).is_none());
            }
        }
        Transformed::Unchanged
    }

    fn in_place_transform_func_decl(&mut self, func_decl: &mut FuncDecl) {
        func_decl.inner_in_place_transform_with(self);

        match &func_decl.def {
            DeclDef::Imported(_) => {
                // FIXME(eddyb) implement if/when it becomes relevant.
                unreachable!("spirt_passes::expand: did not expect function imports");
            }
            // HACK(eddyb) regenerate `params` and `ret_tys` from the function body.
            DeclDef::Present(func_def_body) => {
                assert!(func_def_body.unstructured_cfg.is_none());

                let body_def = func_def_body.at_body().def();
                func_decl.params = body_def
                    .inputs
                    .iter()
                    .map(|&input_var| {
                        let VarDecl { attrs, ty, .. } = func_def_body.vars[input_var];
                        spirt::FuncParam { attrs, ty }
                    })
                    .collect();
                func_decl.ret_types = body_def
                    .outputs
                    .iter()
                    .map(|&v| func_def_body.at(v).type_of(self.cx))
                    .collect();
            }
        }
    }

    fn in_place_transform_region_def(&mut self, func_at_region: FuncAtMut<'_, Region>) {
        let region = func_at_region.position;
        let outer_region = self.parent_region.replace(region);

        let mut func = func_at_region.at(());

        self.expand_vars_as_needed_by_types(func.reborrow(), Either::Left(region));

        func.reborrow()
            .at(region)
            .at_children()
            .into_iter()
            .inner_in_place_transform_with(self);

        self.expand_values_as_needed_by_types(
            func,
            |func| &mut func.regions[region].outputs,
            InsertAnchor::Last,
        );

        self.parent_region = outer_region;
    }

    fn in_place_transform_node_def(&mut self, mut func_at_node: FuncAtMut<'_, Node>) {
        let node = func_at_node.position;
        self.node_parent.insert(node, self.parent_region.unwrap());

        let func = func_at_node.reborrow().freeze().at(());
        let node_def = &func.nodes[node];
        let will_expand = match node_def.kind {
            NodeKind::Select(_) => {
                self.expand_vars_as_needed_by_types(
                    func_at_node.reborrow().at(()),
                    Either::Right(node),
                );
                false
            }
            NodeKind::Loop { .. } => {
                self.expand_values_as_needed_by_types(
                    func_at_node.reborrow().at(()),
                    |func| &mut func.nodes[node].inputs,
                    InsertAnchor::Before(node),
                );
                false
            }
            NodeKind::ExitInvocation(_) => false,
            NodeKind::FuncCall(_) => {
                self.expand_values_as_needed_by_types(
                    func_at_node.reborrow().at(()),
                    |func| &mut func.nodes[node].inputs,
                    InsertAnchor::Before(node),
                );
                self.expand_vars_as_needed_by_types(
                    func_at_node.reborrow().at(()),
                    Either::Right(node),
                );
                false
            }
            _ => {
                let inputs = node_def.inputs.iter().map(|&v| {
                    let ty = func.at(v).type_of(self.cx);
                    let expanded = match v {
                        Value::Const(_) => self
                            .expansion
                            .maybe_expand_type(ty)
                            .map(|components| (ty, components.collect())),
                        Value::Var(var) => self
                            .maybe_expanded_vars
                            .get(var)
                            .and_then(|MaybeExpanded { expanded, .. }| expanded.as_ref())
                            .map(|&(ty, ref components)| {
                                (
                                    ty,
                                    components
                                        .iter()
                                        .map(|&v| func.at(v).type_of(self.cx))
                                        .collect(),
                                )
                            }),
                    };

                    MaybeExpanded {
                        whole: expanded.is_none().then_some(ty),
                        expanded,
                    }
                });
                let output_types = node_def
                    .outputs
                    .iter()
                    .map(|&output_var| func.vars[output_var].ty);
                (inputs.clone().any(|input| input.expanded.is_some())
                    || output_types
                        .clone()
                        .any(|ty| self.expansion.maybe_expand_type(ty).is_some()))
                    && self
                        .expansion
                        .try_expand_op(&node_def.kind, inputs, output_types, &mut TypeOnlyBuilder)
                        .is_ok()
            }
        };

        if will_expand {
            let parent_region = self.parent_region.unwrap();

            let func = func_at_node.reborrow().freeze().at(());
            let node_def = &func.nodes[node];
            let kind = node_def.kind.clone();
            let inputs: SmallVec<[_; 2]> = node_def
                .inputs
                .iter()
                .map(|&v| self.maybe_expanded_value(func.at(v)))
                .collect();
            let output_types: SmallVec<[_; 2]> = node_def
                .outputs
                .iter()
                .map(|&output_var| func.vars[output_var].ty)
                .collect();
            // HACK(eddyb) reusing some aspects of `AutoExpandingBuilder`.
            let outputs: SmallVec<[_; 2]> = AutoExpandingBuilder {
                cx: self.cx,
                expansion: self.expansion,
                bld: &mut NodeBuilder {
                    cx: self.cx,
                    func: func_at_node.reborrow().at(()),
                    parent_region,
                    anchor: InsertAnchor::Before(node),
                },
            }
            .op(kind, inputs, output_types)
            .collect();

            let node_def = func_at_node.def();
            for (output_var, output) in node_def.outputs.drain(..).zip_eq(outputs) {
                self.maybe_expanded_vars.insert(output_var, output);
            }
            // HACK(eddyb) replace the node with a tombstone, but keep it as a
            // child of its original parent region, so it can be used as an
            // insertion anchor later on (if necessary).
            (node_def.kind, node_def.inputs) = (
                NodeKind::SpvInst(
                    spirt::spv::spec::Spec::get().well_known.OpNop.into(),
                    Default::default(),
                ),
                [].into_iter().collect(),
            );
            return;
        }

        for input_idx in 0..func_at_node.reborrow().def().inputs.len() {
            let input = func_at_node.reborrow().def().inputs[input_idx];
            if let Value::Var(var) = input
                && let Some(maybe_expanded_input) = self.maybe_expanded_vars.get(var)
            {
                if let Some(new_input) = maybe_expanded_input.whole {
                    func_at_node.reborrow().def().inputs[input_idx] = new_input;
                    continue;
                }

                let recomposed_var = *self.recomposed_var_cache.entry(var).or_insert_with(|| {
                    let func = func_at_node.reborrow().at(());
                    let var_decl = &func.vars[var];
                    let (parent_region, anchor) = var_decl.def_parent.either(
                        |region| (region, InsertAnchor::First),
                        |node| (self.node_parent[node], InsertAnchor::After(node)),
                    );
                    let &(ty, ref components) = maybe_expanded_input.expanded.as_ref().unwrap();
                    let recomposed = self.expansion.recompose(
                        ty,
                        components.iter().copied(),
                        &mut NodeBuilder {
                            cx: self.cx,
                            func,
                            parent_region,
                            anchor,
                        },
                    );
                    // FIXME(eddyb) could this be avoided with more
                    // associated types in `Builder`?
                    match recomposed {
                        Value::Const(_) => unreachable!(),
                        Value::Var(v) => v,
                    }
                });
                func_at_node.reborrow().def().inputs[input_idx] = Value::Var(recomposed_var);
            }
        }
        // FIXME(eddyb) should the output be eagerly decomposed?

        func_at_node.inner_in_place_transform_with(self);
    }
}

struct AutoExpandingBuilder<'a, E, B> {
    cx: &'a Context,
    expansion: &'a E,
    bld: &'a mut B,
}

impl<E: TypeDrivenExpansion, B: Builder<OutputDecl = Type>> Builder
    for AutoExpandingBuilder<'_, E, B>
where
    B::Value: Copy,
{
    type Value = MaybeExpanded<B::Value>;
    type OutputDecl = Type;

    fn type_of(&self, v: &MaybeExpanded<B::Value>) -> Type {
        v.expanded
            .as_ref()
            .map(|&(ty, _)| ty)
            .unwrap_or_else(|| self.bld.type_of(v.whole.as_ref().unwrap()))
    }

    fn scalar_const(&self, ty: Type, ct: scalar::Const) -> Self::Value {
        let expanded = self
            .expansion
            .maybe_expand_type(ty)
            .and_then(|component_types| {
                Some((
                    ty,
                    self.expansion
                        .decompose(
                            ty,
                            Ok(self.cx.intern(ct)),
                            &mut ConstBuilder { cx: self.cx },
                        )
                        .zip_eq(component_types)
                        .map(|(ct, ty)| {
                            Some(self.bld.scalar_const(ty, *ct.ok()?.as_scalar(self.cx)?))
                        })
                        .collect::<Option<_>>()?,
                ))
            });
        MaybeExpanded {
            whole: expanded.is_none().then(|| self.bld.scalar_const(ty, ct)),
            expanded,
        }
    }

    // FIXME(eddyb) also signal failure somehow?
    fn op(
        &mut self,
        kind: impl Into<NodeKind>,
        inputs: impl IntoIterator<Item = Self::Value>,
        output_types: impl IntoIterator<Item = Type>,
    ) -> impl ExactSizeIterator<Item = Self::Value> {
        let kind = kind.into();
        let mut inputs: SmallVec<[_; 2]> = inputs.into_iter().collect();
        let output_types: SmallVec<[_; 2]> = output_types.into_iter().collect();

        let needs_expansion = inputs.iter().any(|input| input.expanded.is_some())
            || output_types
                .iter()
                .any(|&ty| self.expansion.maybe_expand_type(ty).is_some());

        needs_expansion
            .then(|| {
                self.expansion
                    .try_expand_op(
                        &kind,
                        inputs.iter().cloned(),
                        output_types.iter().copied(),
                        self.bld,
                    )
                    .ok()
            })
            .flatten()
            .map(|outputs| outputs.collect::<SmallVec<[_; 2]>>())
            .unwrap_or_else(|| {
                for input in &mut inputs {
                    if let Some((ty, components)) = input.expanded.take() {
                        input.whole = Some(self.expansion.recompose(
                            ty,
                            components.into_iter(),
                            self.bld,
                        ));
                    }
                }
                self.bld
                    .op(
                        kind,
                        inputs.into_iter().map(|input| input.whole.unwrap()),
                        output_types.into_iter(),
                    )
                    .map(|output| MaybeExpanded {
                        whole: Some(output),
                        expanded: None,
                    })
                    .collect::<SmallVec<[_; 2]>>()
            })
            .into_iter()
    }
}

pub struct ExpandUnsupportedIntegers<'a> {
    cx: &'a Context,
    max_supported_width: scalar::IntWidth,

    cached_limb_type: Cell<Option<Type>>,
}

impl<'a> ExpandUnsupportedIntegers<'a> {
    pub fn for_max_supported_width(cx: &'a Context, max_supported_width: scalar::IntWidth) -> Self {
        Self {
            cx,
            max_supported_width,
            cached_limb_type: Cell::new(None),
        }
    }

    fn limb_scalar_type(&self) -> scalar::Type {
        scalar::Type::UInt(self.max_supported_width)
    }

    fn limb_type(&self) -> Type {
        self.cached_limb_type.get().unwrap_or_else(|| {
            let ty = self.cx.intern(self.limb_scalar_type());
            self.cached_limb_type.set(Some(ty));
            ty
        })
    }

    fn limb_count(&self, ty: scalar::Type) -> Option<NonZeroU32> {
        match ty {
            scalar::Type::SInt(w) | scalar::Type::UInt(w) => {
                (w.bits() > self.max_supported_width.bits()).then(|| {
                    let count = w.bits() / self.max_supported_width.bits();
                    assert_eq!(
                        count.checked_mul(self.max_supported_width.bits()).unwrap(),
                        w.bits()
                    );
                    NonZeroU32::new(count).unwrap()
                })
            }
            scalar::Type::Bool | scalar::Type::Float(_) => None,
        }
    }

    fn try_split_halves<V: Copy>(&self, x: MaybeExpanded<V>) -> Result<[MaybeExpanded<V>; 2], ()> {
        let (ty, limbs) = x.expanded.ok_or(())?;
        let scalar_type = ty.as_scalar(self.cx).ok_or(())?;
        let limb_count = self.limb_count(scalar_type).ok_or(())?.get();
        assert_eq!(limbs.len(), limb_count as usize);

        let half_limb_count = limb_count / 2;
        assert_eq!(limb_count, half_limb_count * 2);

        let half_scalar_type = match scalar_type {
            scalar::Type::UInt(w) => {
                scalar::Type::UInt(scalar::IntWidth::try_from_bits(w.bits() / 2).unwrap())
            }
            // FIXME(eddyb) figure out how to implement signed ops.
            _ => return Err(()),
        };
        let half_type = self.cx.intern(half_scalar_type);
        assert_eq!(
            self.limb_count(half_scalar_type).map_or(1, |c| c.get()),
            half_limb_count
        );

        let (lo, hi) = limbs.split_at(half_limb_count as usize);
        Ok([lo, hi].map(|half| match half[..] {
            [half] => MaybeExpanded {
                whole: Some(half),
                expanded: None,
            },
            _ => MaybeExpanded::expanded(half_type, half.iter().copied()),
        }))
    }

    // HACK(eddyb) this asserts instead of failing, by assuming `try_split_halves` success.
    fn join_halves<V: Copy>(&self, ty: Type, [lo, hi]: [MaybeExpanded<V>; 2]) -> MaybeExpanded<V> {
        MaybeExpanded::expanded(
            ty,
            [lo, hi].into_iter().flat_map(|x| {
                x.whole
                    .into_iter()
                    .chain(x.expanded.into_iter().flat_map(|(_, limbs)| limbs))
            }),
        )
    }
}

impl TypeDrivenExpansion for ExpandUnsupportedIntegers<'_> {
    type Error = ();

    fn maybe_expand_type(&self, ty: Type) -> Option<impl ExactSizeIterator<Item = Type>> {
        let limb_count = self.limb_count(ty.as_scalar(self.cx)?)?;
        let limb_type = self.limb_type();
        Some((0..limb_count.get()).map(move |_| limb_type))
    }

    fn decompose<V: Copy>(
        &self,
        ty: Type,
        value: V,
        bld: &mut impl Builder<Value = V, OutputDecl = Type>,
    ) -> impl ExactSizeIterator<Item = V> {
        let limb_type = self.limb_type();
        let limb_width = self.limb_scalar_type().bit_width();
        let limb_count = self.limb_count(ty.as_scalar(self.cx).unwrap()).unwrap();

        (0..limb_count.get()).map(move |limb_idx| {
            let limb = bld.scalar_op(
                scalar::IntBinOp::ShrU,
                [
                    value,
                    bld.scalar_const(
                        self.cx.intern(scalar::Type::U32),
                        scalar::Const::from_u32(limb_idx * limb_width),
                    ),
                ],
                ty,
            );
            bld.scalar_op(scalar::IntUnOp::TruncOrZeroExtend, [limb], limb_type)
        })
    }

    fn recompose<V: Copy>(
        &self,
        ty: Type,
        limbs: impl ExactSizeIterator<Item = V>,
        bld: &mut impl Builder<Value = V, OutputDecl = Type>,
    ) -> V {
        let limb_width = self.limb_scalar_type().bit_width();
        let limb_count = self.limb_count(ty.as_scalar(self.cx).unwrap()).unwrap();

        bld.map_reduce(
            limbs.zip_eq(0..limb_count.get()),
            move |bld, (limb, limb_idx)| {
                let zext_limb = bld.scalar_op(scalar::IntUnOp::TruncOrZeroExtend, [limb], ty);
                bld.scalar_op(
                    scalar::IntBinOp::Shl,
                    [
                        zext_limb,
                        bld.scalar_const(
                            self.cx.intern(scalar::Type::U32),
                            scalar::Const::from_u32(limb_idx * limb_width),
                        ),
                    ],
                    ty,
                )
            },
            |bld, a, b| bld.scalar_op(scalar::IntBinOp::Or, [a, b], ty),
        )
    }

    fn try_expand_op<V: Copy>(
        &self,
        kind: &NodeKind,
        inputs: impl ExactSizeIterator<Item = MaybeExpanded<V>>,
        output_types: impl ExactSizeIterator<Item = Type>,
        bld: &mut impl Builder<Value = V, OutputDecl = Type>,
    ) -> Result<impl ExactSizeIterator<Item = MaybeExpanded<V>>, Self::Error> {
        let limb_scalar_type = self.limb_scalar_type();
        let limb_type = self.limb_type();

        let outputs: SmallVec<[MaybeExpanded<V>; 2]> = match kind {
            NodeKind::Select(_)
            | NodeKind::Loop { .. }
            | NodeKind::ExitInvocation(_)
            | NodeKind::FuncCall(_) => unreachable!(),

            NodeKind::Scalar(
                scalar::Op::BoolUnary(_)
                | scalar::Op::BoolBinary(_)
                | scalar::Op::FloatUnary(_)
                | scalar::Op::FloatBinary(_),
            ) => return Err(()),
            &NodeKind::Scalar(scalar::Op::IntUnary(op)) => {
                let [x] = inputs.collect_array().ok_or(())?;
                let [output_type] = output_types.collect_array().ok_or(())?;
                [match op {
                    scalar::IntUnOp::Neg => {
                        // HACK(eddyb) apply `-x = !x + 1`.
                        let mut bld = AutoExpandingBuilder {
                            cx: self.cx,
                            expansion: self,
                            bld,
                        };
                        let not_x = bld.scalar_op(scalar::IntUnOp::Not, [x], output_type);
                        let zext_1 = bld.scalar_op(
                            scalar::IntUnOp::TruncOrZeroExtend,
                            [bld.scalar_const(
                                limb_type,
                                scalar::Const::from_bits(limb_scalar_type, 1),
                            )],
                            output_type,
                        );
                        bld.scalar_op(scalar::IntBinOp::Add, [not_x, zext_1], output_type)
                    }
                    scalar::IntUnOp::Not => MaybeExpanded::expanded(
                        output_type,
                        (x.expanded.ok_or(())?.1.into_iter())
                            .map(|limb| bld.scalar_op(op, [limb], limb_type)),
                    ),
                    scalar::IntUnOp::CountOnes => MaybeExpanded {
                        whole: Some(bld.map_reduce(
                            x.expanded.ok_or(())?.1,
                            |bld, limb| bld.scalar_op(op, [limb], output_type),
                            |bld, a, b| bld.scalar_op(scalar::IntBinOp::Add, [a, b], output_type),
                        )),
                        expanded: None,
                    },
                    scalar::IntUnOp::TruncOrZeroExtend => {
                        let mut zext_x = (x.whole)
                            .map(|x| {
                                // FIXME(eddyb) maybe rely on auto-simplification?
                                if bld.type_of(&x) == limb_type {
                                    x
                                } else {
                                    bld.scalar_op(op, [x], limb_type)
                                }
                            })
                            .into_iter()
                            .chain(x.expanded.into_iter().flat_map(|(_, limbs)| limbs))
                            .chain(iter::repeat(bld.scalar_const(
                                limb_type,
                                scalar::Const::from_bits(limb_scalar_type, 0),
                            )));
                        if let Some(count) =
                            self.limb_count(output_type.as_scalar(self.cx).ok_or(())?)
                        {
                            MaybeExpanded::expanded(output_type, zext_x.take(count.get() as usize))
                        } else {
                            let limb = zext_x.next().ok_or(())?;
                            // FIXME(eddyb) maybe rely on auto-simplification?
                            MaybeExpanded {
                                whole: Some(if output_type == limb_type {
                                    limb
                                } else {
                                    bld.scalar_op(op, [limb], output_type)
                                }),
                                expanded: None,
                            }
                        }
                    }
                    scalar::IntUnOp::TruncOrSignExtend => return Err(()),
                }]
                .into_iter()
                .collect()
            }
            &NodeKind::Scalar(scalar::Op::IntBinary(op)) => {
                let is_shift = matches!(
                    op,
                    scalar::IntBinOp::ShrU | scalar::IntBinOp::Shl | scalar::IntBinOp::ShrS
                );

                let [a, b] = inputs.collect_array().ok_or(())?;
                let &(a_type, ref a_limbs) = a.expanded.as_ref().ok_or(())?;
                let b_limbs = (!is_shift)
                    .then_some(b.expanded.as_ref().map(|(_, limbs)| limbs).ok_or(()))
                    .transpose()?;

                // FIXME(eddyb) is this putting too much effort into validation?
                if op.output_count() != output_types.len()
                    || b_limbs.is_some_and(|b_limbs| a_limbs.len() != b_limbs.len())
                {
                    return Err(());
                }
                let a_limbs = Some(a_limbs).into_iter().flatten().copied();
                let b_limbs = b_limbs.into_iter().flatten().copied();

                let output_type = output_types.dedup().exactly_one().ok().ok_or(())?;
                let limb_count = Some(output_type)
                    .filter(|ty| ty.as_scalar(self.cx) != Some(scalar::Type::Bool))
                    .into_iter()
                    .chain([a_type])
                    .chain((!is_shift).then(|| b.expanded.as_ref().unwrap().0))
                    .map(|ty| self.limb_count(ty.as_scalar(self.cx)?))
                    .dedup()
                    .exactly_one()
                    .ok()
                    .flatten()
                    .ok_or(())?;

                match op {
                    // FIXME(eddyb) deduplicate with `CarryingAdd`/`BorrowingSub`.
                    scalar::IntBinOp::Add | scalar::IntBinOp::Sub => {
                        let mut next_carry = None;
                        let r_limbs = (a_limbs.zip_eq(b_limbs).zip_eq(0..limb_count.get())).map(
                            |((a_limb, b_limb), limb_idx)| {
                                let rhses = [Some(b_limb), next_carry.take()].into_iter().flatten();
                                rhses.fold(a_limb, |lhs, rhs| {
                                    if limb_idx == limb_count.get() - 1 {
                                        return bld.scalar_op(op, [lhs, rhs], limb_type);
                                    }

                                    let op_with_carry = match op {
                                        scalar::IntBinOp::Add => scalar::IntBinOp::CarryingAdd,
                                        scalar::IntBinOp::Sub => scalar::IntBinOp::BorrowingSub,
                                        _ => unreachable!(),
                                    };
                                    let [r, carry] = bld
                                        .op(
                                            scalar::Op::from(op_with_carry),
                                            [lhs, rhs],
                                            [limb_type; 2],
                                        )
                                        .collect_array()
                                        .unwrap();

                                    next_carry = [next_carry, Some(carry)]
                                        .into_iter()
                                        .flatten()
                                        .reduce(|a, b| {
                                            bld.scalar_op(scalar::IntBinOp::Add, [a, b], limb_type)
                                        });

                                    r
                                })
                            },
                        );
                        [MaybeExpanded::expanded(output_type, r_limbs)]
                            .into_iter()
                            .collect()
                    }
                    scalar::IntBinOp::Mul => {
                        let mut bld = AutoExpandingBuilder {
                            cx: self.cx,
                            expansion: self,
                            bld,
                        };

                        // HACK(eddyb) the code below extracts `lo` and `hi`,
                        // such that `lo + 2^2N·hi` is equal to this expansion of `a · b`:
                        // `(al + 2^N·ah) · (bl + 2^N·bh) = al·bl + 2^N·(al·bh + ah·bl) + 2^2N·(ah·bh)`
                        // FIXME(eddyb) ^^ actually, `hi` is discarded, in this case.
                        let [al, ah] = self.try_split_halves(a)?;
                        let [bl, bh] = self.try_split_halves(b)?;
                        let half_type = [&al, &ah, &bl, &bh]
                            .into_iter()
                            .map(|x| bld.type_of(x))
                            .dedup()
                            .exactly_one()
                            .ok()
                            .unwrap();

                        // FIXME(eddyb) the unused `ah_bh` means that one of the
                        // 4 multiplications should actually be ignored.
                        let [[al_bl, al_bh], [ah_bl, ah_bh]] = [al, ah].map(|a| {
                            [&bl, &bh].map(|b| {
                                // FIXME(eddyb) replace `output_type` with `UInt`
                                // just to ensure we can get the right bits regardless.
                                bld.expansion.join_halves(
                                    output_type,
                                    bld.op(
                                        scalar::Op::from(scalar::IntBinOp::WideningMulU),
                                        [a.clone(), b.clone()],
                                        [half_type, half_type],
                                    )
                                    .collect_array()
                                    .unwrap(),
                                )
                            })
                        });

                        let mid = bld.scalar_op(scalar::IntBinOp::Add, [al_bh, ah_bl], output_type);
                        let [mid_lo, _] = bld.expansion.try_split_halves(mid)?;

                        let shifted_mid_lo = bld.expansion.join_halves(
                            output_type,
                            [
                                bld.scalar_const(
                                    half_type,
                                    scalar::Const::from_bits(
                                        half_type.as_scalar(bld.cx).ok_or(())?,
                                        0,
                                    ),
                                ),
                                mid_lo,
                            ],
                        );

                        [
                            bld.scalar_op(
                                scalar::IntBinOp::Add,
                                [al_bl, shifted_mid_lo],
                                output_type,
                            ),
                        ]
                        .into_iter()
                        .collect()
                    }
                    scalar::IntBinOp::DivU => return Err(()),
                    scalar::IntBinOp::DivS => return Err(()),
                    scalar::IntBinOp::ModU => return Err(()),
                    scalar::IntBinOp::RemS => return Err(()),
                    scalar::IntBinOp::ModS => return Err(()),
                    scalar::IntBinOp::ShrU | scalar::IntBinOp::Shl => {
                        let mut bld = AutoExpandingBuilder {
                            cx: self.cx,
                            expansion: self,
                            bld,
                        };

                        // FIXME(eddyb) maybe rename `a` and `b` to reflect the
                        // asymmetry of "value being shifted" vs "shift amount".
                        let a_scalar_type = a_type.as_scalar(self.cx).ok_or(())?;
                        let b_type = bld.type_of(&b);
                        let b_scalar_type = b_type.as_scalar(self.cx).ok_or(())?;

                        // HACK(eddyb) reduce `b` to at most `limb_type` width.
                        let b = if b_scalar_type.bit_width() > limb_scalar_type.bit_width() {
                            bld.scalar_op(scalar::IntUnOp::TruncOrZeroExtend, [b], limb_type)
                        } else {
                            b
                        };
                        let b_type = bld.type_of(&b);
                        let b_scalar_type = b_type.as_scalar(self.cx).ok_or(())?;

                        let a_bit_width_as_b_typed_const = bld.scalar_const(
                            b_type,
                            scalar::Const::try_from_bits(
                                b_scalar_type,
                                a_scalar_type.bit_width().into(),
                            )
                            .ok_or(())?,
                        );

                        let [al, ah] = self.try_split_halves(a)?;
                        let half_type = [&al, &ah]
                            .into_iter()
                            .map(|x| bld.type_of(x))
                            .dedup()
                            .exactly_one()
                            .ok()
                            .unwrap();

                        let half_0 = bld.scalar_const(
                            half_type,
                            scalar::Const::from_bits(half_type.as_scalar(self.cx).unwrap(), 0),
                        );

                        // HACK(eddyb) first handle the "intra-half" part of the
                        // shift (which, for non-zero `b_intra`, still involves
                        // a "cross-half" transfer of bits, in the shift direction),
                        // then shift the whole halves themselves.
                        let b_intra = bld.scalar_op(
                            scalar::IntBinOp::And,
                            [
                                b.clone(),
                                bld.scalar_const(
                                    b_type,
                                    scalar::Const::from_bits(
                                        b_scalar_type,
                                        (a_scalar_type.bit_width() - 1).into(),
                                    ),
                                ),
                            ],
                            b_type,
                        );
                        let [mut intra_lo, mut intra_hi] = [&al, &ah]
                            .map(|x| bld.scalar_op(op, [x.clone(), b_intra.clone()], half_type));

                        let (cross_dst, cross_src, cross_op) = match op {
                            scalar::IntBinOp::ShrU => (&mut intra_lo, &ah, scalar::IntBinOp::Shl),
                            scalar::IntBinOp::Shl => (&mut intra_hi, &al, scalar::IntBinOp::ShrU),
                            _ => unreachable!(),
                        };
                        let b_cross = bld.scalar_op(
                            scalar::IntBinOp::Sub,
                            [a_bit_width_as_b_typed_const.clone(), b_intra.clone()],
                            b_type,
                        );
                        {
                            // HACK(eddyb) this is effectively a "saturating shift",
                            // where a shift amount larger than (or equal to) the
                            // width of the integer being shifted, is defined to
                            // "shift away" all the bits, and produce `0`.
                            let cross_shift = bld.scalar_op(
                                cross_op,
                                [cross_src.clone(), b_cross.clone()],
                                half_type,
                            );

                            let b_cross_too_large = bld.scalar_op(
                                scalar::IntBinOp::GeU,
                                [b_cross, a_bit_width_as_b_typed_const.clone()],
                                self.cx.intern(scalar::Type::Bool),
                            );
                            let cross_saturated_shift = bld.select(
                                b_cross_too_large,
                                [half_0.clone(), cross_shift],
                                half_type,
                            );

                            *cross_dst = bld.scalar_op(
                                scalar::IntBinOp::Or,
                                [cross_dst.clone(), cross_saturated_shift],
                                half_type,
                            );
                        }

                        let b_inter = bld.scalar_op(
                            scalar::IntBinOp::GeU,
                            [b.clone(), a_bit_width_as_b_typed_const.clone()],
                            self.cx.intern(scalar::Type::Bool),
                        );

                        let [inter_lo, inter_hi] = match op {
                            // FIXME(eddyb) signed ("arithmetic") right shifts
                            // (once they are supported here) will want to use
                            // `intra_hi >> (a_scalar_type.bit_width() - 1)`,
                            // instead of `half_0`, for `inter_hi`.
                            scalar::IntBinOp::ShrU => [intra_hi.clone(), half_0],
                            scalar::IntBinOp::Shl => [half_0, intra_lo.clone()],
                            _ => unreachable!(),
                        };

                        let [lo, hi] = [
                            bld.select(b_inter.clone(), [inter_lo, intra_lo], half_type),
                            bld.select(b_inter, [inter_hi, intra_hi], half_type),
                        ];

                        [bld.expansion.join_halves(output_type, [lo, hi])]
                            .into_iter()
                            .collect()
                    }
                    scalar::IntBinOp::ShrS => return Err(()),
                    scalar::IntBinOp::Or | scalar::IntBinOp::Xor | scalar::IntBinOp::And => {
                        [MaybeExpanded::expanded(
                            output_type,
                            (a_limbs.zip_eq(b_limbs)).map(|(a_limb, b_limb)| {
                                bld.scalar_op(op, [a_limb, b_limb], limb_type)
                            }),
                        )]
                        .into_iter()
                        .collect()
                    }
                    scalar::IntBinOp::CarryingAdd | scalar::IntBinOp::BorrowingSub => {
                        let mut next_carry = None;

                        let r_limbs = a_limbs
                            .zip_eq(b_limbs)
                            .map(|(a_limb, b_limb)| {
                                let rhses = [Some(b_limb), next_carry.take()].into_iter().flatten();
                                rhses.fold(a_limb, |lhs, rhs| {
                                    let [r, carry] = bld
                                        .op(scalar::Op::from(op), [lhs, rhs], [limb_type; 2])
                                        .collect_array()
                                        .unwrap();

                                    next_carry = [next_carry, Some(carry)]
                                        .into_iter()
                                        .flatten()
                                        .reduce(|a, b| {
                                            bld.scalar_op(scalar::IntBinOp::Add, [a, b], limb_type)
                                        });

                                    r
                                })
                            })
                            .collect::<SmallVec<[_; 4]>>();

                        let mut bld = AutoExpandingBuilder {
                            cx: self.cx,
                            expansion: self,
                            bld,
                        };

                        let carry = bld.scalar_op(
                            scalar::IntUnOp::TruncOrZeroExtend,
                            [MaybeExpanded {
                                whole: Some(next_carry.unwrap()),
                                expanded: None,
                            }],
                            output_type,
                        );

                        [MaybeExpanded::expanded(output_type, r_limbs), carry]
                            .into_iter()
                            .collect()
                    }
                    scalar::IntBinOp::WideningMulU => return Err(()),
                    scalar::IntBinOp::WideningMulS => return Err(()),
                    scalar::IntBinOp::Eq | scalar::IntBinOp::Ne => {
                        let bool_type = output_type;
                        let r = bld.map_reduce(
                            a_limbs.zip_eq(b_limbs),
                            |bld, (a_limb, b_limb)| bld.scalar_op(op, [a_limb, b_limb], bool_type),
                            |bld, a, b| {
                                bld.scalar_op(
                                    match op {
                                        scalar::IntBinOp::Eq => scalar::BoolBinOp::And,
                                        scalar::IntBinOp::Ne => scalar::BoolBinOp::Or,
                                        _ => unreachable!(),
                                    },
                                    [a, b],
                                    bool_type,
                                )
                            },
                        );
                        [MaybeExpanded {
                            whole: Some(r),
                            expanded: None,
                        }]
                        .into_iter()
                        .collect()
                    }
                    scalar::IntBinOp::GtU
                    | scalar::IntBinOp::GtS
                    | scalar::IntBinOp::GeU
                    | scalar::IntBinOp::GeS
                    | scalar::IntBinOp::LtU
                    | scalar::IntBinOp::LtS
                    | scalar::IntBinOp::LeU
                    | scalar::IntBinOp::LeS => {
                        let bool_type = output_type;

                        let strict_cmp_op = match op {
                            scalar::IntBinOp::GtU | scalar::IntBinOp::GeU => scalar::IntBinOp::GtU,
                            scalar::IntBinOp::GtS | scalar::IntBinOp::GeS => scalar::IntBinOp::GtS,
                            scalar::IntBinOp::LtU | scalar::IntBinOp::LeU => scalar::IntBinOp::LtU,
                            scalar::IntBinOp::LtS | scalar::IntBinOp::LeS => scalar::IntBinOp::LtS,
                            _ => unreachable!(),
                        };

                        let cmp_op_to_unsigned = |op| match op {
                            scalar::IntBinOp::GtU | scalar::IntBinOp::GtS => scalar::IntBinOp::GtU,
                            scalar::IntBinOp::GeU | scalar::IntBinOp::GeS => scalar::IntBinOp::GeU,
                            scalar::IntBinOp::LtU | scalar::IntBinOp::LtS => scalar::IntBinOp::LtU,
                            scalar::IntBinOp::LeU | scalar::IntBinOp::LeS => scalar::IntBinOp::LeU,
                            _ => unreachable!(),
                        };

                        let r = (a_limbs.zip_eq(b_limbs).zip_eq(0..limb_count.get()))
                            .fold(None, |lo_cmp, ((a_limb, b_limb), limb_idx)| {
                                let is_topmost_limb = limb_idx == limb_count.get() - 1;
                                Some(if let Some(lo_cmp) = lo_cmp {
                                    let limb_strict_cmp_op = if is_topmost_limb {
                                        strict_cmp_op
                                    } else {
                                        cmp_op_to_unsigned(strict_cmp_op)
                                    };
                                    let a_strict_cmp_b = bld.scalar_op(
                                        limb_strict_cmp_op,
                                        [a_limb, b_limb],
                                        bool_type,
                                    );
                                    let hi_a_eq_b = bld.scalar_op(
                                        scalar::IntBinOp::Eq,
                                        [a_limb, b_limb],
                                        bool_type,
                                    );
                                    let hi_a_eq_b_and_lo_cmp = bld.scalar_op(
                                        scalar::BoolBinOp::And,
                                        [hi_a_eq_b, lo_cmp],
                                        bool_type,
                                    );
                                    bld.scalar_op(
                                        scalar::BoolBinOp::Or,
                                        [a_strict_cmp_b, hi_a_eq_b_and_lo_cmp],
                                        bool_type,
                                    )
                                } else {
                                    assert!(!is_topmost_limb);
                                    bld.scalar_op(
                                        cmp_op_to_unsigned(op),
                                        [a_limb, b_limb],
                                        bool_type,
                                    )
                                })
                            })
                            .unwrap();
                        [MaybeExpanded {
                            whole: Some(r),
                            expanded: None,
                        }]
                        .into_iter()
                        .collect()
                    }
                }
            }
            NodeKind::Vector(_) => {
                // FIXME(eddyb) also expand vector ops with too-wide elements.
                return Err(());
            }
            NodeKind::Mem(op) => match *op {
                MemOp::FuncLocalVar(_) => return Err(()),
                // FIXME(eddyb) is it worth deduping load and store below?
                MemOp::Load { offset } => {
                    let offset = offset.map_or(0, |o| o.get());

                    let [ptr] = inputs.collect_array().ok_or(())?;
                    let [output_type] = output_types.collect_array().ok_or(())?;
                    let ptr = ptr
                        .whole
                        .filter(|ptr| matches!(self.cx[bld.type_of(ptr)].kind, TypeKind::QPtr))
                        .ok_or(())?;

                    let limb_count = i32::try_from(
                        self.limb_count(output_type.as_scalar(self.cx).ok_or(())?)
                            .ok_or(())?
                            .get(),
                    )
                    .unwrap();
                    let limb_bytes = i32::try_from(limb_scalar_type.bit_width() / 8).unwrap();
                    offset.checked_add(limb_count * limb_bytes).ok_or(())?;

                    let loaded_limbs = (0..limb_count).map(|limb_idx| {
                        let limb_mem_idx = if super::SPIRT_MEM_LAYOUT_CONFIG.is_big_endian {
                            limb_count - 1 - limb_idx
                        } else {
                            limb_idx
                        };
                        bld.op(
                            MemOp::Load {
                                offset: NonZeroI32::new(
                                    offset.checked_add(limb_mem_idx * limb_bytes).unwrap(),
                                ),
                            },
                            [ptr],
                            [limb_type],
                        )
                        .exactly_one()
                        .ok()
                        .unwrap()
                    });
                    [MaybeExpanded::expanded(output_type, loaded_limbs)]
                        .into_iter()
                        .collect()
                }
                MemOp::Store { offset } => {
                    let offset = offset.map_or(0, |o| o.get());

                    let [ptr, stored_value] = inputs.collect_array().ok_or(())?;
                    let [] = output_types.collect_array().ok_or(())?;
                    let ptr = ptr
                        .whole
                        .filter(|ptr| matches!(self.cx[bld.type_of(ptr)].kind, TypeKind::QPtr))
                        .ok_or(())?;
                    let (stored_type, stored_limbs) = stored_value.expanded.ok_or(())?;

                    let limb_count = i32::try_from(
                        self.limb_count(stored_type.as_scalar(self.cx).ok_or(())?)
                            .ok_or(())?
                            .get(),
                    )
                    .unwrap();
                    let limb_bytes = i32::try_from(limb_scalar_type.bit_width() / 8).unwrap();
                    offset.checked_add(limb_count * limb_bytes).ok_or(())?;

                    if stored_limbs.len() != limb_count as usize {
                        return Err(());
                    }

                    for (limb_idx, stored_limb) in (0..limb_count).zip_eq(stored_limbs) {
                        let limb_mem_idx = if super::SPIRT_MEM_LAYOUT_CONFIG.is_big_endian {
                            limb_count - 1 - limb_idx
                        } else {
                            limb_idx
                        };
                        let [] = bld
                            .op(
                                MemOp::Store {
                                    offset: NonZeroI32::new(
                                        offset.checked_add(limb_mem_idx * limb_bytes).unwrap(),
                                    ),
                                },
                                [ptr, stored_limb],
                                [],
                            )
                            .collect_array()
                            .unwrap();
                    }

                    [].into_iter().collect()
                }
            },
            // FIXME(eddyb) `OpSelect` is a problem (maybe SPIR-T should allow
            // a `Select` node with no child regions, and one input per case?).
            NodeKind::SpvInst(spv_inst, _)
                if spv_inst.opcode == super::SpvSpecWithExtras::get().well_known.OpSelect =>
            {
                let [cond, a, b] = inputs.collect_array().ok_or(())?;
                let cond = cond.whole.ok_or(())?;
                let (a_type, a_limbs) = a.expanded.ok_or(())?;
                let (b_type, b_limbs) = b.expanded.ok_or(())?;

                let [output_type] = output_types.collect_array().ok_or(())?;

                let limb_count = [a_type, b_type, output_type]
                    .into_iter()
                    .map(|ty| self.limb_count(ty.as_scalar(self.cx)?))
                    .dedup()
                    .exactly_one()
                    .ok()
                    .flatten()
                    .ok_or(())?;

                // FIXME(eddyb) is this putting too much effort into validation?
                if a_limbs.len() != limb_count.get() as usize || a_limbs.len() != b_limbs.len() {
                    return Err(());
                }

                [MaybeExpanded::expanded(
                    output_type,
                    (a_limbs.into_iter().zip_eq(b_limbs))
                        .map(|(a_limb, b_limb)| bld.select(cond, [a_limb, b_limb], limb_type)),
                )]
                .into_iter()
                .collect()
            }
            NodeKind::SpvInst(spv_inst, _)
                if spv_inst.opcode == spirt::spv::spec::Spec::get().well_known.OpBitcast =>
            {
                let [x] = inputs.collect_array().ok_or(())?;
                let (x_type, x_limbs) = x.expanded.ok_or(())?;

                let [output_type] = output_types.collect_array().ok_or(())?;

                let limb_count = [x_type, output_type]
                    .into_iter()
                    .map(|ty| self.limb_count(ty.as_scalar(self.cx)?))
                    .dedup()
                    .exactly_one()
                    .ok()
                    .flatten()
                    .ok_or(())?;

                // FIXME(eddyb) is this putting too much effort into validation?
                if x_limbs.len() != limb_count.get() as usize {
                    return Err(());
                }

                [MaybeExpanded::expanded(output_type, x_limbs)]
                    .into_iter()
                    .collect()
            }
            NodeKind::QPtr(_)
            | NodeKind::ThunkBind(_)
            | NodeKind::SpvInst(..)
            | NodeKind::SpvExtInst { .. } => {
                return Err(());
            }
        };
        Ok(outputs.into_iter())
    }
}

// FIXME(eddyb) avoid duplication with `ConstBuilder`, if possible.
#[cfg(test)]
struct ScalarConstBuilder<'a> {
    cx: &'a Context,
}
#[cfg(test)]
impl Builder for ScalarConstBuilder<'_> {
    type Value = (Type, scalar::Const);
    type OutputDecl = Type;

    fn type_of(&self, &(ty, _): &(Type, scalar::Const)) -> Type {
        ty
    }

    fn scalar_const(&self, ty: Type, ct: scalar::Const) -> (Type, scalar::Const) {
        (ty, ct)
    }

    fn op(
        &mut self,
        kind: impl Into<NodeKind>,
        inputs: impl IntoIterator<Item = (Type, scalar::Const)>,
        output_types: impl IntoIterator<Item = Type>,
    ) -> impl ExactSizeIterator<Item = (Type, scalar::Const)> {
        let inputs = inputs
            .into_iter()
            .map(|(_, ct)| ct)
            .collect::<SmallVec<[_; 2]>>();
        let output_types = output_types.into_iter().collect::<SmallVec<[_; 2]>>();
        let outputs = match kind.into() {
            NodeKind::Scalar(op) => op
                .try_eval(
                    &inputs,
                    &output_types
                        .iter()
                        .map(|&ty| ty.as_scalar(self.cx).unwrap())
                        .collect::<SmallVec<[_; 2]>>(),
                )
                .ok()
                .unwrap(),
            // FIXME(eddyb) `OpSelect` is a problem (maybe SPIR-T should allow
            // a `Select` node with no child regions, and one input per case?).
            NodeKind::SpvInst(spv_inst, _)
                if spv_inst.opcode == super::SpvSpecWithExtras::get().well_known.OpSelect =>
            {
                [match inputs[..] {
                    [scalar::Const::TRUE, x, _] | [scalar::Const::FALSE, _, x] => x,
                    _ => unreachable!(),
                }]
                .into_iter()
                .collect()
            }
            _ => unreachable!(),
        };
        output_types.into_iter().zip_eq(outputs)
    }
}

#[test]
fn bruteforce_i16() {
    let cx = &spirt::Context::new();
    let u16_scalar_type = scalar::Type::UInt(scalar::IntWidth::I16);
    let u16_type = cx.intern(u16_scalar_type);

    let expansion = ExpandUnsupportedIntegers {
        cx,
        max_supported_width: scalar::IntWidth::I8,
        cached_limb_type: Cell::new(None),
    };
    let bruteforce_range = ((i8::MIN as i16 * 4)..=(i8::MAX as i16 * 4 + 3)).map(|x| x as u16);
    for a in bruteforce_range.clone() {
        for b in bruteforce_range.clone() {
            let [a_scalar, b_scalar] =
                [a, b].map(|x| scalar::Const::from_bits(u16_scalar_type, x.into()));
            for op in [
                scalar::IntBinOp::Add,
                scalar::IntBinOp::Sub,
                scalar::IntBinOp::Mul,
            ] {
                let expected = u32::from(match op {
                    scalar::IntBinOp::Add => a.wrapping_add(b),
                    scalar::IntBinOp::Sub => a.wrapping_sub(b),
                    scalar::IntBinOp::Mul => a.wrapping_mul(b),
                    _ => unreachable!(),
                });
                assert_eq!(
                    op.try_eval(a_scalar, b_scalar, &[u16_scalar_type])
                        .ok()
                        .unwrap()
                        .into_iter()
                        .exactly_one()
                        .ok()
                        .unwrap()
                        .int_as_u32()
                        .unwrap(),
                    expected
                );

                let bld = &mut ScalarConstBuilder { cx };
                let inputs = [a_scalar, b_scalar].map(|x| {
                    MaybeExpanded::expanded(
                        u16_type,
                        expansion.decompose(u16_type, bld.scalar_const(u16_type, x), bld),
                    )
                });
                let [found] = expansion
                    .try_expand_op(
                        &NodeKind::Scalar(op.into()),
                        inputs.into_iter(),
                        [u16_type].into_iter(),
                        bld,
                    )
                    .unwrap()
                    .collect::<SmallVec<[_; 2]>>()
                    .into_iter()
                    .map(|r| {
                        let (ty, components) = r.expanded.unwrap();
                        expansion
                            .recompose(ty, components.into_iter(), bld)
                            .1
                            .int_as_u32()
                            .unwrap()
                    })
                    .collect_array()
                    .unwrap();

                if found != expected {
                    panic!(
                        "{}({a:#x}, {b:#x}) mod 2¹⁶ = {expected:#x} but got {found:#x} instead",
                        op.name()
                    );
                }
            }
        }
    }
}
