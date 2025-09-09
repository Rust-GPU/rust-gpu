//! SPIR-T passes related to "explicit layout decorations" (`Offset`/`ArrayStride`).

use either::Either;
use itertools::Itertools;
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use spirt::func_at::{FuncAt, FuncAtMut};
use spirt::transform::{InnerInPlaceTransform, InnerTransform, Transformed, Transformer};
use spirt::visit::InnerVisit as _;
use spirt::{
    AddrSpace, Attr, AttrSetDef, Const, ConstKind, Context, DataInst, DataInstDef, DataInstKind,
    DeclDef, Diag, Func, FuncDecl, GlobalVar, GlobalVarDecl, Module, Node, NodeKind,
    NodeOutputDecl, Type, TypeDef, TypeKind, TypeOrConst, Value, spv,
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

        parent_block: None,
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

    // HACK(eddyb) this is to allow `in_place_transform_data_inst_def` inject
    // new instructions into its parent block.
    parent_block: Option<Node>,
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
                visit_region: |_: &mut _, _| {},
                visit_node: |_: &mut _, func_at_node: FuncAt<'_, Node>| {
                    if let NodeKind::Block { insts } = func_at_node.def().kind {
                        for func_at_inst in func_at_node.at(insts) {
                            if let Err(e) = self.pre_check_data_inst(func_at_inst) {
                                errors_to_attach.push((func_at_inst.position, e));
                            }
                        }
                    }
                },
            });
            for (inst, err) in errors_to_attach {
                func_def_body
                    .at_mut(inst)
                    .def()
                    .attrs
                    .push_diag(self.cx, err);
            }
        }

        func_decl.inner_in_place_transform_with(self);
    }

    fn in_place_transform_node_def(&mut self, mut func_at_node: FuncAtMut<'_, Node>) {
        let old_parent_block = self.parent_block.take();
        if let NodeKind::Block { .. } = func_at_node.reborrow().def().kind {
            self.parent_block = Some(func_at_node.position);
        }
        func_at_node.inner_in_place_transform_with(self);
        self.parent_block = old_parent_block;
    }

    fn in_place_transform_data_inst_def(&mut self, mut func_at_data_inst: FuncAtMut<'_, DataInst>) {
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
            } if spv_inst.opcode == wk.OpTypePointer => match type_and_const_inputs[..] {
                [TypeOrConst::Type(elem_type)] => Some(elem_type),
                _ => unreachable!(),
            },
            _ => None,
        };

        let DataInstKind::SpvInst(spv_inst) = &data_inst_def.kind else {
            return;
        };

        // FIXME(eddyb) filter attributes into debuginfo and
        // semantic, and understand the semantic ones.
        let attrs = data_inst_def.attrs;

        let mk_bitcast_def = |in_value, out_type| DataInstDef {
            attrs,
            kind: DataInstKind::SpvInst(wk.OpBitcast.into()),
            inputs: [in_value].into_iter().collect(),
            child_regions: [].into_iter().collect(),
            outputs: [NodeOutputDecl {
                attrs: Default::default(),
                ty: out_type,
            }]
            .into_iter()
            .collect(),
        };

        if spv_inst.opcode == wk.OpLoad {
            let pointee_type = pointee_type_of_ptr_val(data_inst_def.inputs[0]);
            let value_type = data_inst_def.outputs[0].ty;
            // FIXME(eddyb) leave a BUG diagnostic in the `None` case?
            if pointee_type.is_some_and(|ty| {
                ty != value_type && ty == self.erase_explicit_layout_in_type(value_type)
            }) {
                let func = func_at_data_inst.at(());
                let NodeKind::Block { insts } = &mut func.nodes[self.parent_block.unwrap()].kind
                else {
                    unreachable!()
                };

                let fixed_load_inst = func.data_insts.define(
                    cx,
                    DataInstDef {
                        child_regions: [].into_iter().collect(),
                        outputs: [NodeOutputDecl {
                            attrs: Default::default(),
                            ty: pointee_type.unwrap(),
                        }]
                        .into_iter()
                        .collect(),
                        ..DataInstDef::clone(&func.data_insts[data_inst])
                    }
                    .into(),
                );
                insts.insert_before(fixed_load_inst, data_inst, func.data_insts);
                *func.data_insts[data_inst] = mk_bitcast_def(
                    Value::DataInstOutput {
                        inst: fixed_load_inst,
                        output_idx: 0,
                    },
                    value_type,
                );

                self.disaggregate_bitcast(func.at(data_inst));
            }
        } else if spv_inst.opcode == wk.OpStore {
            let pointee_type = pointee_type_of_ptr_val(data_inst_def.inputs[0]);
            let value_type = type_of_val(data_inst_def.inputs[1]);
            // FIXME(eddyb) leave a BUG diagnostic in the `None` case?
            if pointee_type.is_some_and(|ty| {
                ty != value_type && ty == self.erase_explicit_layout_in_type(value_type)
            }) {
                let func = func_at_data_inst.at(());
                let stored_value = &mut func.data_insts[data_inst].inputs[1];

                if let Value::Const(ct) = stored_value {
                    EraseExplicitLayout(self)
                        .transform_const_use(*ct)
                        .apply_to(ct);
                } else {
                    let original_stored_value = *stored_value;

                    let NodeKind::Block { insts } =
                        &mut func.nodes[self.parent_block.unwrap()].kind
                    else {
                        unreachable!()
                    };
                    let stored_value_cast_inst = func.data_insts.define(
                        cx,
                        mk_bitcast_def(original_stored_value, pointee_type.unwrap()).into(),
                    );
                    insts.insert_before(stored_value_cast_inst, data_inst, func.data_insts);
                    func.data_insts[data_inst].inputs[1] = Value::DataInstOutput {
                        inst: stored_value_cast_inst,
                        output_idx: 0,
                    };

                    self.disaggregate_bitcast(func.at(stored_value_cast_inst));
                }
            }
        } else if spv_inst.opcode == wk.OpCopyMemory {
            let dst_ptr = data_inst_def.inputs[0];
            let src_ptr = data_inst_def.inputs[1];
            let [dst_pointee_type, src_pointee_type] =
                [dst_ptr, src_ptr].map(pointee_type_of_ptr_val);
            // FIXME(eddyb) leave a BUG diagnostic in the `None` case?
            let mismatched_dst_src_types = match [dst_pointee_type, src_pointee_type] {
                [Some(a), Some(b)] => {
                    // FIXME(eddyb) there has to be a nicer way to write this??
                    fn equal<T: Eq>([a, b]: [T; 2]) -> bool {
                        a == b
                    }
                    !equal([a, b]) && equal([a, b].map(|ty| self.erase_explicit_layout_in_type(ty)))
                }
                _ => false,
            };
            if mismatched_dst_src_types {
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
                let NodeKind::Block { insts } = &mut func.nodes[self.parent_block.unwrap()].kind
                else {
                    unreachable!()
                };

                let load_inst = func.data_insts.define(
                    cx,
                    DataInstDef {
                        attrs,
                        kind: DataInstKind::SpvInst(spv::Inst {
                            opcode: wk.OpLoad,
                            imms: src_imms,
                        }),
                        inputs: [src_ptr].into_iter().collect(),
                        child_regions: [].into_iter().collect(),
                        outputs: [NodeOutputDecl {
                            attrs: Default::default(),
                            ty: src_pointee_type.unwrap(),
                        }]
                        .into_iter()
                        .collect(),
                    }
                    .into(),
                );
                insts.insert_before(load_inst, data_inst, func.data_insts);
                let cast_inst = func.data_insts.define(
                    cx,
                    mk_bitcast_def(
                        Value::DataInstOutput {
                            inst: load_inst,
                            output_idx: 0,
                        },
                        dst_pointee_type.unwrap(),
                    )
                    .into(),
                );
                insts.insert_before(cast_inst, data_inst, func.data_insts);

                *func.data_insts[data_inst] = DataInstDef {
                    attrs,
                    kind: DataInstKind::SpvInst(spv::Inst {
                        opcode: wk.OpStore,
                        imms: dst_imms,
                    }),
                    inputs: [
                        dst_ptr,
                        Value::DataInstOutput {
                            inst: cast_inst,
                            output_idx: 0,
                        },
                    ]
                    .into_iter()
                    .collect(),
                    child_regions: [].into_iter().collect(),
                    outputs: [].into_iter().collect(),
                };

                self.disaggregate_bitcast(func.at(cast_inst));
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

    // FIXME(eddyb) properly distinguish between zero-extension and sign-extension.
    fn const_as_u32(&self, ct: Const) -> Option<u32> {
        if let ConstKind::SpvInst {
            spv_inst_and_const_inputs,
        } = &self.cx[ct].kind
        {
            let (spv_inst, _const_inputs) = &**spv_inst_and_const_inputs;
            if spv_inst.opcode == self.wk.OpConstant && spv_inst.imms.len() == 1 {
                match spv_inst.imms[..] {
                    [spv::Imm::Short(_, x)] => return Some(x),
                    _ => unreachable!(),
                }
            }
        }
        None
    }

    fn aggregate_component_types(
        &self,
        ty: Type,
    ) -> Option<impl ExactSizeIterator<Item = Type> + Clone + 'a> {
        let cx = self.cx;
        let wk = self.wk;

        match &cx[ty].kind {
            TypeKind::SpvInst {
                spv_inst,
                type_and_const_inputs,
            } if spv_inst.opcode == wk.OpTypeStruct => {
                Some(Either::Left(type_and_const_inputs.iter().map(
                    |&ty_or_ct| match ty_or_ct {
                        TypeOrConst::Type(ty) => ty,
                        TypeOrConst::Const(_) => unreachable!(),
                    },
                )))
            }
            TypeKind::SpvInst {
                spv_inst,
                type_and_const_inputs,
            } if spv_inst.opcode == wk.OpTypeArray => {
                let [TypeOrConst::Type(elem_type), TypeOrConst::Const(count)] =
                    type_and_const_inputs[..]
                else {
                    unreachable!()
                };
                let count = self.const_as_u32(count)?;
                Some(Either::Right((0..count).map(move |_| elem_type)))
            }
            _ => None,
        }
    }

    fn erase_explicit_layout_in_type(&mut self, mut ty: Type) -> Type {
        EraseExplicitLayout(self)
            .transform_type_use(ty)
            .apply_to(&mut ty);
        ty
    }

    // HACK(eddyb) this expands an illegal `OpBitcast` of a struct/array, into
    // leaf values from the source aggregate that are then recomposed into the
    // target aggregate - this should go away when SPIR-T `disaggregate` lands.
    fn disaggregate_bitcast(&mut self, mut func_at_cast_inst: FuncAtMut<'_, DataInst>) {
        let cx = self.cx;
        let wk = self.wk;

        let cast_inst = func_at_cast_inst.position;
        let cast_def = func_at_cast_inst.reborrow().freeze().def().clone();

        // FIXME(eddyb) filter attributes into debuginfo and
        // semantic, and understand the semantic ones.
        let attrs = cast_def.attrs;

        assert!(cast_def.kind == DataInstKind::SpvInst(wk.OpBitcast.into()));
        let in_value = cast_def.inputs[0];
        let out_type = cast_def.outputs[0].ty;

        let mut func = func_at_cast_inst.reborrow();
        let in_type = func.reborrow().freeze().at(in_value).type_of(cx);

        // FIXME(eddyb) there has to be a nicer way to write this??
        fn equal<T: Eq>([a, b]: [T; 2]) -> bool {
            a == b
        }

        let [in_component_types, out_component_types] = Some([in_type, out_type])
            .filter(|&types| {
                !equal(types) && equal(types.map(|ty| self.erase_explicit_layout_in_type(ty)))
            })
            .map(|types| types.map(|ty| self.aggregate_component_types(ty)))
            .unwrap_or_default();

        // NOTE(eddyb) such sanity checks should always succeed, because of the
        // "in/out types are equal after erasure" check, earlier above.
        assert_eq!(
            in_component_types.as_ref().map(|iter| iter.len()),
            out_component_types.as_ref().map(|iter| iter.len()),
        );

        let [Some(in_component_types), Some(out_component_types)] =
            [in_component_types, out_component_types]
        else {
            return;
        };

        let components = (in_component_types.zip_eq(out_component_types).enumerate())
            .map(|(component_idx, (component_in_type, component_out_type))| {
                let component_idx = u32::try_from(component_idx).unwrap();

                let component_cast_types =
                    Some([component_in_type, component_out_type]).filter(|&types| !equal(types));
                if let Some(component_cast_types) = component_cast_types {
                    assert!(equal(
                        component_cast_types.map(|ty| self.erase_explicit_layout_in_type(ty))
                    ));
                }

                let component_extract_inst = func.data_insts.define(
                    cx,
                    DataInstDef {
                        attrs,
                        kind: DataInstKind::SpvInst(spv::Inst {
                            opcode: wk.OpCompositeExtract,
                            imms: [spv::Imm::Short(wk.LiteralInteger, component_idx)]
                                .into_iter()
                                .collect(),
                        }),
                        inputs: [in_value].into_iter().collect(),
                        child_regions: [].into_iter().collect(),
                        outputs: [NodeOutputDecl {
                            attrs: Default::default(),
                            ty: component_in_type,
                        }]
                        .into_iter()
                        .collect(),
                    }
                    .into(),
                );

                let NodeKind::Block { insts } = &mut func.nodes[self.parent_block.unwrap()].kind
                else {
                    unreachable!()
                };
                insts.insert_before(component_extract_inst, cast_inst, func.data_insts);

                let component_cast_inst = component_cast_types.map(|[_, component_out_type]| {
                    let inst = func.data_insts.define(
                        cx,
                        DataInstDef {
                            attrs,
                            kind: DataInstKind::SpvInst(wk.OpBitcast.into()),
                            inputs: [Value::DataInstOutput {
                                inst: component_extract_inst,
                                output_idx: 0,
                            }]
                            .into_iter()
                            .collect(),
                            child_regions: [].into_iter().collect(),
                            outputs: [NodeOutputDecl {
                                attrs: Default::default(),
                                ty: component_out_type,
                            }]
                            .into_iter()
                            .collect(),
                        }
                        .into(),
                    );
                    insts.insert_before(inst, cast_inst, func.data_insts);

                    inst
                });

                if let Some(component_cast_inst) = component_cast_inst {
                    self.disaggregate_bitcast(func.reborrow().at(component_cast_inst));
                }

                Value::DataInstOutput {
                    inst: component_cast_inst.unwrap_or(component_extract_inst),
                    output_idx: 0,
                }
            })
            .collect();

        *func.at(cast_inst).def() = DataInstDef {
            attrs,
            kind: DataInstKind::SpvInst(wk.OpCompositeConstruct.into()),
            inputs: components,
            child_regions: [].into_iter().collect(),
            outputs: [NodeOutputDecl {
                attrs: Default::default(),
                ty: out_type,
            }]
            .into_iter()
            .collect(),
        };
    }

    // HACK(eddyb) this runs on every `DataInst` in a function body, before the
    // declarations of any region input/node output, are ever changed, to catch
    // the cases that would need special handling, but lack it.
    fn pre_check_data_inst(&mut self, func_at_inst: FuncAt<'_, DataInst>) -> Result<(), Diag> {
        let cx = self.cx;
        let wk = self.wk;

        let data_inst_def = func_at_inst.def();

        // FIXME(eddyb) consider preserving the actual type change in the error.
        let any_types_will_change = (data_inst_def.outputs.iter().map(|o| o.ty))
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
            DataInstKind::FuncCall(_) => return Ok(()),

            DataInstKind::SpvInst(spv_inst)
                if [wk.OpLoad, wk.OpStore, wk.OpCopyMemory].contains(&spv_inst.opcode) =>
            {
                return Ok(());
            }

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
            &DataInstKind::SpvExtInst { ext_set, inst } => {
                let ext_set = &cx[ext_set];
                return Err(Diag::bug([format!(
                    "unhandled pointer type change in extended SPIR-V \
                     (`{ext_set}` / #{inst}) instruction"
                )
                .into()]));
            }

            DataInstKind::SpvInst(spv_inst) => spv_inst,
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
        let transformed = self
            .transform_type_def(&self.0.cx[ty])
            .map(|ty_def| self.0.cx.intern(ty_def));
        self.0
            .cached_erased_explicit_layout_types
            .insert(ty, transformed);
        transformed
    }
    fn transform_const_use(&mut self, ct: Const) -> Transformed<Const> {
        if let Some(&cached) = self.0.cached_erased_explicit_layout_consts.get(&ct) {
            return cached;
        }
        let transformed = self
            .transform_const_def(&self.0.cx[ct])
            .map(|ct_def| self.0.cx.intern(ct_def));
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
