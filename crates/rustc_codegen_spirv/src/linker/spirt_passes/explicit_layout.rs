//! SPIR-T passes related to "explicit layout decorations" (`Offset`/`ArrayStride`).

use either::Either;
use itertools::Itertools;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use spirt::func_at::{FuncAt, FuncAtMut};
use spirt::transform::{InnerInPlaceTransform, InnerTransform, Transformed, Transformer};
use spirt::visit::InnerVisit as _;
use spirt::{
    AddrSpace, Attr, AttrSetDef, Const, ConstDef, Context, DataInst, DataInstDef, DataInstKind,
    DeclDef, Diag, DiagLevel, Func, FuncDecl, GlobalVar, GlobalVarDecl, Module, Node, NodeKind,
    Region, Type, TypeDef, TypeKind, TypeOrConst, Value, VarDecl, spv,
};
use std::cmp::Ordering;
use std::collections::VecDeque;

/// Erase explicit layout decorations from struct/array types, when used with
/// storage classes that do not support them (as per the Vulkan spec).
//
// NOTE(eddyb) this is a stop-gap until `spirt::{mem,qptr}` can replace it.
pub fn erase_when_invalid(module: &mut Module) {
    let spv_spec = super::SpvSpecWithExtras::get();
    let wk = &spv_spec.well_known;

    let mut eraser = SelectiveEraser {
        cx: &module.cx(),
        wk,

        transformed_types: FxHashMap::default(),
        transformed_consts: FxHashMap::default(),
        seen_global_vars: FxHashSet::default(),
        global_var_queue: VecDeque::new(),
        seen_funcs: FxHashSet::default(),
        func_queue: VecDeque::new(),

        cached_erased_explicit_layout_types: FxHashMap::default(),
        cached_erased_explicit_layout_consts: FxHashMap::default(),

        parent_region: None,
    };

    // Seed the queues starting from the module exports.
    for exportee in module.exports.values_mut() {
        exportee
            .inner_transform_with(&mut eraser)
            .apply_to(exportee);
    }

    // Process the queues until they're all empty.
    while !eraser.global_var_queue.is_empty() || !eraser.func_queue.is_empty() {
        while let Some(gv) = eraser.global_var_queue.pop_front() {
            eraser.in_place_transform_global_var_decl(&mut module.global_vars[gv]);
        }
        while let Some(func) = eraser.func_queue.pop_front() {
            eraser.in_place_transform_func_decl(&mut module.funcs[func]);
        }
    }
}

// FIXME(eddyb) name could be better (avoiding overly verbose is a bit tricky).
struct SelectiveEraser<'a> {
    cx: &'a Context,
    wk: &'static super::SpvWellKnownWithExtras,

    // FIXME(eddyb) build some automation to avoid ever repeating these.
    transformed_types: FxHashMap<Type, Transformed<Type>>,
    transformed_consts: FxHashMap<Const, Transformed<Const>>,
    seen_global_vars: FxHashSet<GlobalVar>,
    global_var_queue: VecDeque<GlobalVar>,
    seen_funcs: FxHashSet<Func>,
    func_queue: VecDeque<Func>,

    // FIXME(eddyb) these overlap with some `transformed_*` fields above,
    // but they're contextually transformed additionally.
    // HACK(eddyb) these are now used via the `EraseExplicitLayout` newtype.
    cached_erased_explicit_layout_types: FxHashMap<Type, Transformed<Type>>,
    cached_erased_explicit_layout_consts: FxHashMap<Const, Transformed<Const>>,

    parent_region: Option<Region>,
}

impl Transformer for SelectiveEraser<'_> {
    // FIXME(eddyb) build some automation to avoid ever repeating these.
    fn transform_type_use(&mut self, ty: Type) -> Transformed<Type> {
        if let Some(&cached) = self.transformed_types.get(&ty) {
            return cached;
        }
        let transformed = self
            .transform_type_def(&self.cx[ty])
            .map(|ty_def| self.cx.intern(ty_def));
        self.transformed_types.insert(ty, transformed);
        transformed
    }
    fn transform_const_use(&mut self, ct: Const) -> Transformed<Const> {
        if let Some(&cached) = self.transformed_consts.get(&ct) {
            return cached;
        }
        let transformed = self
            .transform_const_def(&self.cx[ct])
            .map(|ct_def| self.cx.intern(ct_def));
        self.transformed_consts.insert(ct, transformed);
        transformed
    }

    fn transform_global_var_use(&mut self, gv: GlobalVar) -> Transformed<GlobalVar> {
        if self.seen_global_vars.insert(gv) {
            self.global_var_queue.push_back(gv);
        }
        Transformed::Unchanged
    }
    fn transform_func_use(&mut self, func: Func) -> Transformed<Func> {
        if self.seen_funcs.insert(func) {
            self.func_queue.push_back(func);
        }
        Transformed::Unchanged
    }

    // NOTE(eddyb) above methods are plumbing, erasure methods are below.

    fn transform_type_def(&mut self, ty_def: &TypeDef) -> Transformed<TypeDef> {
        let wk = self.wk;

        let needs_erasure_of_explicit_layout = match &ty_def.kind {
            TypeKind::SpvInst {
                spv_inst,
                type_and_const_inputs: _,
                value_lowering: _,
            } if spv_inst.opcode == wk.OpTypePointer => match spv_inst.imms[..] {
                [spv::Imm::Short(sc_kind, sc)] => {
                    assert_eq!(sc_kind, wk.StorageClass);
                    !self.addr_space_allows_explicit_layout(AddrSpace::SpvStorageClass(sc))
                }
                _ => unreachable!(),
            },

            _ => false,
        };
        if needs_erasure_of_explicit_layout {
            ty_def.inner_transform_with(&mut EraseExplicitLayout(self))
        } else {
            ty_def.inner_transform_with(self)
        }
    }

    fn in_place_transform_global_var_decl(&mut self, gv_decl: &mut GlobalVarDecl) {
        let needs_erasure_of_explicit_layout =
            !self.addr_space_allows_explicit_layout(gv_decl.addr_space);
        if needs_erasure_of_explicit_layout {
            // HACK(eddyb) bypass `EraseExplicitLayout::transform_type_use` checks,
            // specifically for the pointer-to-global type, while keeping them
            // for anything involving shapes or initializers.
            self.transform_type_use(gv_decl.type_of_ptr_to)
                .apply_to(&mut gv_decl.type_of_ptr_to);
            gv_decl.inner_in_place_transform_with(&mut EraseExplicitLayout(self));
        } else {
            gv_decl.inner_in_place_transform_with(self);
        }
    }

    fn in_place_transform_func_decl(&mut self, func_decl: &mut FuncDecl) {
        // HACK(eddyb) to catch any instructions having their input/output types
        // changed from under them, a separate visit has to be used before *any*
        // region input/node output declarations in the function body may change.
        if let DeclDef::Present(func_def_body) = &mut func_decl.def {
            let mut errors_to_attach = vec![];
            func_def_body.inner_visit_with(&mut super::VisitAllRegionsAndNodes {
                state: (),
                enter_region: |_: &mut _, _| {},
                exit_region: |_: &mut _, _| {},
                enter_node: |_: &mut _, func_at_node: FuncAt<'_, Node>| {
                    if let Err(e) = self.pre_check_node(func_at_node) {
                        errors_to_attach.push((func_at_node.position, e));
                    }
                },
                exit_node: |_: &mut _, _| {},
            });
            for (node, err) in errors_to_attach {
                func_def_body
                    .at_mut(node)
                    .def()
                    .attrs
                    .push_diag(self.cx, err);
            }
        }

        func_decl.inner_in_place_transform_with(self);
    }

    fn in_place_transform_region_def(&mut self, mut func_at_region: FuncAtMut<'_, Region>) {
        let old_parent_region = self.parent_region.replace(func_at_region.position);
        func_at_region.inner_in_place_transform_with(self);
        self.parent_region = old_parent_region;
    }

    fn in_place_transform_node_def(&mut self, mut func_at_data_inst: FuncAtMut<'_, Node>) {
        let cx = self.cx;
        let wk = self.wk;

        func_at_data_inst
            .reborrow()
            .inner_in_place_transform_with(self);

        let func_at_data_inst_frozen = func_at_data_inst.reborrow().freeze();
        let data_inst = func_at_data_inst_frozen.position;
        let data_inst_def = func_at_data_inst_frozen.def();
        let func = func_at_data_inst_frozen.at(());
        let type_of_val = |v: Value| func.at(v).type_of(cx);
        let pointee_type_of_ptr_val = |p: Value| match &cx[type_of_val(p)].kind {
            TypeKind::SpvInst {
                spv_inst,
                type_and_const_inputs,
                value_lowering: _,
            } if spv_inst.opcode == wk.OpTypePointer => match type_and_const_inputs[..] {
                [TypeOrConst::Type(elem_type)] => Some(elem_type),
                _ => unreachable!(),
            },
            _ => None,
        };

        let DataInstKind::SpvInst(spv_inst, lowering) = &data_inst_def.kind else {
            return;
        };

        // FIXME(eddyb) filter attributes into debuginfo and
        // semantic, and understand the semantic ones.
        let attrs = data_inst_def.attrs;

        if spv_inst.opcode == wk.OpLoad {
            if let Some(pointee_type) = pointee_type_of_ptr_val(data_inst_def.inputs[0])
                && let Some(aggregate_type) = &lowering.disaggregated_output
                && pointee_type != *aggregate_type
                && pointee_type == self.erase_explicit_layout_in_type(*aggregate_type)
            {
                let DataInstKind::SpvInst(_, lowering) = &mut func_at_data_inst.def().kind else {
                    unreachable!();
                };
                lowering.disaggregated_output = Some(pointee_type);
            }
        } else if spv_inst.opcode == wk.OpStore {
            if let Some(pointee_type) = pointee_type_of_ptr_val(data_inst_def.inputs[0])
                && let [
                    (disaggregated_range @ std::ops::Range { start: 1, end: _ }, aggregate_type),
                ] = &lowering.disaggregated_inputs[..]
                && (disaggregated_range.end as usize) == data_inst_def.inputs.len()
                && pointee_type != *aggregate_type
                && pointee_type == self.erase_explicit_layout_in_type(*aggregate_type)
            {
                let DataInstKind::SpvInst(_, lowering) = &mut func_at_data_inst.def().kind else {
                    unreachable!();
                };
                lowering.disaggregated_inputs[0].1 = pointee_type;
            }
        } else if spv_inst.opcode == wk.OpCopyMemory {
            let dst_ptr = data_inst_def.inputs[0];
            let src_ptr = data_inst_def.inputs[1];
            // FIXME(eddyb) leave a BUG diagnostic in the `None` case?
            let mismatched_dst_src_types = match [dst_ptr, src_ptr].map(pointee_type_of_ptr_val) {
                [Some(a), Some(b)] => {
                    Some([a, b]).filter(|&[a, b]| {
                        // FIXME(eddyb) there has to be a nicer way to write this??
                        fn equal<T: Eq>([a, b]: [T; 2]) -> bool {
                            a == b
                        }
                        !equal([a, b])
                            && equal([a, b].map(|ty| self.erase_explicit_layout_in_type(ty)))
                            && [a, b].iter().all(|&ty| {
                                matches!(
                                    &cx[ty].kind,
                                    TypeKind::SpvInst {
                                        value_lowering: spv::ValueLowering::Disaggregate(_),
                                        ..
                                    }
                                )
                            })
                            && equal([a, b].map(|ty| cx[ty].disaggregated_leaf_count()))
                    })
                }
                _ => None,
            };
            if let Some([dst_pointee_type, src_pointee_type]) = mismatched_dst_src_types {
                let is_memory_access_imm =
                    |imm| matches!(imm, &spv::Imm::Short(k, _) if k == wk.MemoryAccess);

                // HACK(eddyb) this relies on `MemoryAccess` being non-recursive
                // (in fact, its parameters seem to only be simple literals/IDs).
                let (dst_imms, src_imms) =
                    match (spv_inst.imms.iter().positions(is_memory_access_imm))
                        .collect::<SmallVec<[_; 2]>>()[..]
                    {
                        [] | [0] => (&spv_inst.imms[..], &spv_inst.imms[..]),
                        [0, i] => spv_inst.imms.split_at(i),
                        _ => unreachable!(),
                    };

                let [dst_imms, src_imms] =
                    [dst_imms, src_imms].map(|imms| imms.iter().copied().collect());

                let func = func_at_data_inst.at(());

                let parent_region_children =
                    &mut func.regions[self.parent_region.unwrap()].children;

                let load_inst = func.nodes.define(
                    cx,
                    DataInstDef {
                        attrs,
                        kind: DataInstKind::SpvInst(
                            spv::Inst {
                                opcode: wk.OpLoad,
                                imms: src_imms,
                            },
                            spv::InstLowering {
                                disaggregated_output: Some(src_pointee_type),
                                disaggregated_inputs: [].into_iter().collect(),
                            },
                        ),
                        inputs: [src_ptr].into_iter().collect(),
                        child_regions: [].into_iter().collect(),
                        outputs: [].into_iter().collect(),
                    }
                    .into(),
                );
                func.nodes[load_inst].outputs.extend(
                    src_pointee_type
                        .disaggregated_leaf_types(cx)
                        .enumerate()
                        .map(|(i, ty)| {
                            func.vars.define(
                                cx,
                                VarDecl {
                                    attrs: Default::default(),
                                    ty,
                                    def_parent: Either::Right(load_inst),
                                    def_idx: i.try_into().unwrap(),
                                },
                            )
                        }),
                );
                parent_region_children.insert_before(load_inst, data_inst, func.nodes);

                *func.nodes[data_inst] = DataInstDef {
                    attrs,
                    kind: DataInstKind::SpvInst(
                        spv::Inst {
                            opcode: wk.OpStore,
                            imms: dst_imms,
                        },
                        spv::InstLowering {
                            disaggregated_output: None,
                            disaggregated_inputs: [(
                                1..u32::try_from(
                                    1 + cx[dst_pointee_type].disaggregated_leaf_count(),
                                )
                                .unwrap(),
                                dst_pointee_type,
                            )]
                            .into_iter()
                            .collect(),
                        },
                    ),
                    inputs: [dst_ptr]
                        .into_iter()
                        .chain(
                            func.nodes[load_inst]
                                .outputs
                                .iter()
                                .copied()
                                .map(Value::Var),
                        )
                        .collect(),
                    child_regions: [].into_iter().collect(),
                    outputs: [].into_iter().collect(),
                };
            }
        }
    }
}

impl<'a> SelectiveEraser<'a> {
    fn addr_space_allows_explicit_layout(&self, addr_space: AddrSpace) -> bool {
        let wk = self.wk;

        // FIXME(eddyb) this might need to include `Workgroup`, specifically when
        // `WorkgroupMemoryExplicitLayoutKHR` is being relied upon.
        [
            wk.PushConstant,
            wk.Uniform,
            wk.StorageBuffer,
            wk.PhysicalStorageBuffer,
            // HACK(eddyb) not directly useful/needed, but avoiding (shallow)
            // layout erasure in function pointers can reduce diagnostic noise.
            wk.CodeSectionINTEL,
        ]
        .map(AddrSpace::SpvStorageClass)
        .contains(&addr_space)
    }

    fn erase_explicit_layout_in_type(&mut self, mut ty: Type) -> Type {
        EraseExplicitLayout(self)
            .transform_type_use(ty)
            .apply_to(&mut ty);
        ty
    }

    // HACK(eddyb) this runs on every `DataInst` in a function body, before the
    // declarations of any region input/node output, are ever changed, to catch
    // the cases that would need special handling, but lack it.
    fn pre_check_node(&mut self, func_at_inst: FuncAt<'_, DataInst>) -> Result<(), Diag> {
        let cx = self.cx;
        let wk = self.wk;

        let data_inst_def = func_at_inst.def();

        // FIXME(eddyb) consider preserving the actual type change in the error.
        let any_types_will_change = (data_inst_def
            .outputs
            .iter()
            .map(|&o| func_at_inst.at(o).decl().ty))
        .chain(
            data_inst_def
                .inputs
                .iter()
                .map(|&v| func_at_inst.at(v).type_of(cx)),
        )
        .any(|ty| {
            let mut new_ty = ty;
            self.transform_type_use(ty).apply_to(&mut new_ty);
            new_ty != ty
        });
        if !any_types_will_change {
            return Ok(());
        }

        let spv_inst = match &data_inst_def.kind {
            NodeKind::Select(_)
            | NodeKind::Loop { .. }
            | NodeKind::ExitInvocation(_)
            | DataInstKind::FuncCall(_)
            | DataInstKind::ThunkBind(_) => return Ok(()),

            DataInstKind::SpvInst(spv_inst, _)
                if [wk.OpLoad, wk.OpStore, wk.OpCopyMemory].contains(&spv_inst.opcode) =>
            {
                return Ok(());
            }
            DataInstKind::Scalar(_) | DataInstKind::Vector(_) => unreachable!(),
            DataInstKind::Mem(_) => {
                return Err(Diag::bug([
                    "unhandled pointer type change in unexpected `mem` instruction".into(),
                ]));
            }
            DataInstKind::QPtr(_) => {
                return Err(Diag::bug([
                    "unhandled pointer type change in unexpected `qptr` instruction".into(),
                ]));
            }
            &DataInstKind::SpvExtInst {
                ext_set,
                inst,
                lowering: _,
            } => {
                let ext_set = &cx[ext_set];
                return Err(Diag::bug([format!(
                    "unhandled pointer type change in extended SPIR-V \
                     (`{ext_set}` / #{inst}) instruction"
                )
                .into()]));
            }
            DataInstKind::SpvInst(spv_inst, _) => spv_inst,
        };

        let sigs = crate::spirv_type_constraints::instruction_signatures(
            rspirv::spirv::Op::from_u32(spv_inst.opcode.as_u16().into()).unwrap(),
        );
        let pointer_pointee_correlated_sigs: SmallVec<[_; 1]> = sigs
            .unwrap_or(&[])
            .iter()
            .filter(|sig| {
                use crate::spirv_type_constraints::{TyListPat, TyPat};

                #[derive(Copy, Clone, Default)]
                struct ConstrainedVars {
                    direct: bool,
                    in_pointee: bool,
                }
                impl std::ops::BitOr for ConstrainedVars {
                    type Output = Self;
                    fn bitor(self, rhs: Self) -> Self {
                        Self {
                            direct: self.direct | rhs.direct,
                            in_pointee: self.in_pointee | rhs.in_pointee,
                        }
                    }
                }
                impl ConstrainedVars {
                    fn collect_from(pat: &TyPat<'_>) -> Self {
                        match pat {
                            TyPat::Pointer(_, inner) => {
                                let Self { direct, in_pointee } = Self::collect_from(inner);
                                Self {
                                    direct: false,
                                    in_pointee: direct | in_pointee,
                                }
                            }

                            TyPat::Any | TyPat::Void => Self::default(),
                            TyPat::Var(_) => Self {
                                direct: true,
                                in_pointee: false,
                            },
                            TyPat::Either(a, b) => Self::collect_from(a) | Self::collect_from(b),
                            TyPat::Array(inner)
                            | TyPat::Vector(inner)
                            | TyPat::Vector4(inner)
                            | TyPat::Matrix(inner)
                            | TyPat::Image(inner)
                            | TyPat::Pipe(inner)
                            | TyPat::SampledImage(inner)
                            | TyPat::IndexComposite(inner) => Self::collect_from(inner),
                            TyPat::Struct(fields) => Self::collect_from_list_leaves(fields),
                            TyPat::Function(output, inputs) => {
                                Self::collect_from(output) | Self::collect_from_list_leaves(inputs)
                            }
                        }
                    }
                    fn collect_from_list_leaves(pat: &TyListPat<'_>) -> Self {
                        match pat {
                            TyListPat::Any | TyListPat::Nil | TyListPat::Var(_) => Self::default(),
                            TyListPat::Repeat(inner) => Self::collect_from_list_leaves(inner),
                            TyListPat::Cons { first, suffix } => {
                                Self::collect_from(first) | Self::collect_from_list_leaves(suffix)
                            }
                        }
                    }
                }

                let mut min_expected_inputs = 0;
                let mut constrained_vars = sig
                    .output_type
                    .map(ConstrainedVars::collect_from)
                    .unwrap_or_default();

                let mut inputs = sig.input_types;
                while let TyListPat::Cons { first, suffix } = inputs {
                    min_expected_inputs += 1;
                    constrained_vars = constrained_vars | ConstrainedVars::collect_from(first);

                    inputs = suffix;
                }

                if let (Ordering::Less, _) | (Ordering::Greater, TyListPat::Nil) =
                    (data_inst_def.inputs.len().cmp(&min_expected_inputs), inputs)
                {
                    return false;
                }

                constrained_vars.direct && constrained_vars.in_pointee
            })
            .collect();
        if !pointer_pointee_correlated_sigs.is_empty() {
            return Err(Diag::bug([format!(
                "unhandled pointer type change in `{}` SPIR-V instruction: \
                 {pointer_pointee_correlated_sigs:#?}",
                spv_inst.opcode.name()
            )
            .into()]));
        }
        Ok(())
    }
}

// HACK(eddyb) wrapper modifying `Transformer` behavior of `SelectiveEraser`.
struct EraseExplicitLayout<'a, 'b>(&'a mut SelectiveEraser<'b>);

impl Transformer for EraseExplicitLayout<'_, '_> {
    // FIXME(eddyb) build some automation to avoid ever repeating these.
    fn transform_type_use(&mut self, ty: Type) -> Transformed<Type> {
        if let Some(&cached) = self.0.cached_erased_explicit_layout_types.get(&ty) {
            return cached;
        }
        let transformed = self.transform_type_def(&self.0.cx[ty]).map(|new_ty_def| {
            let cx = self.0.cx;

            let new_ty: Type = cx.intern(new_ty_def);
            let new_ty_def = &cx[new_ty];
            let spv_value_lowering = match &new_ty_def.kind {
                TypeKind::Scalar(_)
                | TypeKind::Vector(_)
                | TypeKind::QPtr
                | TypeKind::Thunk
                | TypeKind::SpvStringLiteralForExtInst => &spv::ValueLowering::Direct,
                TypeKind::SpvInst { value_lowering, .. } => value_lowering,
            };

            match spv_value_lowering {
                // HACK(eddyb) this avoids having to ever generate `OpBitcast`s
                // (in Vulkan, only `PhysicalStorageBuffer` pointers could be
                // be an aggregate leaf, and refer to another aggregate type,
                // but they use explicit layout so they shouldn't reach this).
                spv::ValueLowering::Direct => {
                    let mut new_ty_def = TypeDef {
                        attrs: new_ty_def.attrs,
                        kind: new_ty_def.kind.clone(),
                    };
                    // FIXME(eddyb) also take into account any inner diagnostics.
                    let already_has_errors = new_ty_def
                        .attrs
                        .diags(cx)
                        .iter()
                        .any(|diag| matches!(diag.level, DiagLevel::Bug(_) | DiagLevel::Error));
                    if !already_has_errors {
                        new_ty_def.attrs.push_diag(
                            cx,
                            Diag::bug([
                                "unexpected (non-aggregate type) layout erasure (`".into(),
                                ty.into(),
                                "` -> `".into(),
                                new_ty.into(),
                                "`)".into(),
                            ]),
                        );
                    }
                    cx.intern(new_ty_def)
                }
                spv::ValueLowering::Disaggregate(_) => new_ty,
            }
        });
        self.0
            .cached_erased_explicit_layout_types
            .insert(ty, transformed);
        transformed
    }
    fn transform_const_use(&mut self, ct: Const) -> Transformed<Const> {
        if let Some(&cached) = self.0.cached_erased_explicit_layout_consts.get(&ct) {
            return cached;
        }
        let transformed = self.transform_const_def(&self.0.cx[ct]).map(|new_ct_def| {
            let cx = self.0.cx;

            let new_ct: Const = cx.intern(new_ct_def);
            let new_ct_def = &cx[new_ct];
            let mut new_ct_def = ConstDef {
                attrs: new_ct_def.attrs,
                ty: new_ct_def.ty,
                kind: new_ct_def.kind.clone(),
            };
            // FIXME(eddyb) also take into account any inner diagnostics.
            let already_has_errors = new_ct_def
                .attrs
                .diags(cx)
                .iter()
                .any(|diag| matches!(diag.level, DiagLevel::Bug(_) | DiagLevel::Error));
            if !already_has_errors {
                // HACK(eddyb) see also `transform_type_use` above and its similar
                // (but conditional) diagnostic (is caching consts even needed now?).
                new_ct_def.attrs.push_diag(
                    cx,
                    Diag::bug([
                        "unexpected (const) layout erasure (`".into(),
                        ct.into(),
                        "` -> `".into(),
                        new_ct.into(),
                        "`)".into(),
                    ]),
                );
            }
            cx.intern(new_ct_def)
        });
        self.0
            .cached_erased_explicit_layout_consts
            .insert(ct, transformed);
        transformed
    }

    fn transform_global_var_use(&mut self, gv: GlobalVar) -> Transformed<GlobalVar> {
        self.0.transform_global_var_use(gv)
    }
    fn transform_func_use(&mut self, func: Func) -> Transformed<Func> {
        self.0.transform_func_use(func)
    }

    // NOTE(eddyb) above methods are plumbing, erasure methods are below.

    fn transform_type_def(&mut self, ty_def: &TypeDef) -> Transformed<TypeDef> {
        let wk = self.0.wk;

        // HACK(eddyb) reconsider pointer types, based on *their* storage class
        // (e.g. implicit-layout pointers to explicit-layout pointers, even if
        // for Vulkan that's only possible by involving `PhysicalStorageBuffer`).
        match &ty_def.kind {
            TypeKind::SpvInst {
                spv_inst,
                type_and_const_inputs: _,
                value_lowering: _,
            } if spv_inst.opcode == wk.OpTypePointer => {
                return self.0.transform_type_def(ty_def);
            }
            _ => {}
        }

        let transformed = ty_def.inner_transform_with(self);

        let old_attrs = match &transformed {
            Transformed::Unchanged => ty_def.attrs,
            Transformed::Changed(new_ty_def) => new_ty_def.attrs,
        };

        let new_attrs = self.0.cx.intern(AttrSetDef {
            attrs: self.0.cx[old_attrs]
                .attrs
                .iter()
                .filter(|attr| {
                    // FIXME(eddyb) `rustfmt` breaks down for `matches!`.
                    #[allow(clippy::match_like_matches_macro)]
                    let is_explicit_layout_decoration = match attr {
                        Attr::SpvAnnotation(attr_spv_inst)
                            if (attr_spv_inst.opcode == wk.OpDecorate
                                && [wk.ArrayStride, wk.MatrixStride]
                                    .map(|d| spv::Imm::Short(wk.Decoration, d))
                                    .contains(&attr_spv_inst.imms[0]))
                                || (attr_spv_inst.opcode == wk.OpMemberDecorate
                                    && attr_spv_inst.imms[1]
                                        == spv::Imm::Short(wk.Decoration, wk.Offset)) =>
                        {
                            true
                        }

                        _ => false,
                    };
                    !is_explicit_layout_decoration
                })
                .cloned()
                .collect(),
        });

        if old_attrs == new_attrs {
            return transformed;
        }

        let mut ty_def = TypeDef {
            attrs: ty_def.attrs,
            kind: ty_def.kind.clone(),
        };
        transformed.apply_to(&mut ty_def);

        ty_def.attrs = new_attrs;
        Transformed::Changed(ty_def)
    }
}
