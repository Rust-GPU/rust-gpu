//! SPIR-T pass adding `Aligned` memory operands to `PhysicalStorageBuffer` accesses.
//!
//! SPIR-V requires every load/store through a `PhysicalStorageBuffer` pointer to
//! have an `Aligned` memory operand, but codegen can't know which accesses those
//! are, as storage classes are only final after inference in the linker. Like
//! `explicit_layout`, this fixes up the module after the fact.

use super::SpvWellKnownWithExtras;
use rspirv::spirv::MemoryAccess;
use rustc_data_structures::fx::FxIndexSet;
use spirt::func_at::FuncAt;
use spirt::visit::InnerVisit as _;
use spirt::{
    Context, ControlNode, ControlNodeKind, DataInst, DataInstDef, DataInstFormDef, DataInstKind,
    DeclDef, FuncDefBody, Module, Type, TypeKind, TypeOrConst, Value, spv,
};

/// Add `Aligned` to every `OpLoad`/`OpStore` through a `PhysicalStorageBuffer`
/// pointer that lacks it, splitting `OpCopyMemory`s involving such pointers first.
pub fn align_accesses(module: &mut Module) {
    let cx = &module.cx();
    let wk = &super::SpvSpecWithExtras::get().well_known;

    let all_funcs = {
        let mut collector = super::ReachableUseCollector {
            cx,
            module,

            seen_types: FxIndexSet::default(),
            seen_consts: FxIndexSet::default(),
            seen_data_inst_forms: FxIndexSet::default(),
            seen_global_vars: FxIndexSet::default(),
            seen_funcs: FxIndexSet::default(),
        };
        for (export_key, &exportee) in &module.exports {
            export_key.inner_visit_with(&mut collector);
            exportee.inner_visit_with(&mut collector);
        }
        collector.seen_funcs
    };

    for func in all_funcs {
        let DeclDef::Present(func_def_body) = &mut module.funcs[func].def else {
            continue;
        };

        let mut accesses = vec![];
        func_def_body.inner_visit_with(&mut super::VisitAllControlRegionsAndNodes {
            state: (),
            visit_control_region: |_: &mut _, _| {},
            visit_control_node: |_: &mut _, func_at_node: FuncAt<'_, ControlNode>| {
                if let ControlNodeKind::Block { insts } = func_at_node.def().kind {
                    for func_at_inst in func_at_node.at(insts) {
                        if let DataInstKind::SpvInst(spv_inst) = &cx[func_at_inst.def().form].kind
                            && [wk.OpLoad, wk.OpStore, wk.OpCopyMemory].contains(&spv_inst.opcode)
                        {
                            accesses.push((func_at_node.position, func_at_inst.position));
                        }
                    }
                }
            },
        });

        for (block, inst) in accesses {
            if let Some(load_inst) = split_copy_memory(cx, wk, func_def_body, block, inst) {
                align_access(cx, wk, func_def_body, load_inst);
            }
            align_access(cx, wk, func_def_body, inst);
        }
    }
}

/// Replace `inst`, if it's an `OpCopyMemory` involving a `PhysicalStorageBuffer`
/// pointer, with an `OpStore` of a new `OpLoad` (inserted before it, and returned).
fn split_copy_memory(
    cx: &Context,
    wk: &SpvWellKnownWithExtras,
    func: &mut FuncDefBody,
    block: ControlNode,
    inst: DataInst,
) -> Option<DataInst> {
    let inst_def = &func.data_insts[inst];
    let attrs = inst_def.attrs;
    let DataInstKind::SpvInst(spv_inst) = &cx[inst_def.form].kind else {
        return None;
    };
    // Memory operands on a copy are written by hand in `asm!`, whose author
    // states the alignment themselves, so such copies are left alone.
    if spv_inst.opcode != wk.OpCopyMemory || !spv_inst.imms.is_empty() {
        return None;
    }
    let [dst_ptr, src_ptr] = inst_def.inputs[..] else {
        unreachable!()
    };
    let ptrs = [dst_ptr, src_ptr].map(|ptr| pointer_parts(cx, wk, func.at(ptr).type_of(cx)));
    let [Some((dst_sc, dst_pointee)), Some((src_sc, pointee))] = ptrs else {
        return None;
    };
    // So are copies between different types: `explicit_layout` already split
    // those that only differ in layout, and any others are invalid anyway.
    if ![dst_sc, src_sc].contains(&wk.PhysicalStorageBuffer) || dst_pointee != pointee {
        return None;
    }

    let mk_def = |opcode: spv::spec::Opcode, output_type, inputs: &[Value]| DataInstDef {
        attrs,
        form: cx.intern(DataInstFormDef {
            kind: DataInstKind::SpvInst(opcode.into()),
            output_type,
        }),
        inputs: inputs.iter().copied().collect(),
    };

    let load_inst = func
        .data_insts
        .define(cx, mk_def(wk.OpLoad, Some(pointee), &[src_ptr]).into());
    let ControlNodeKind::Block { insts } = &mut func.control_nodes[block].kind else {
        unreachable!()
    };
    insts.insert_before(load_inst, inst, &mut func.data_insts);
    *func.data_insts[inst] = mk_def(
        wk.OpStore,
        None,
        &[dst_ptr, Value::DataInstOutput(load_inst)],
    );
    Some(load_inst)
}

/// Add `Aligned` to `inst`, if it's an `OpLoad`/`OpStore` through a
/// `PhysicalStorageBuffer` pointer, and doesn't already have `Aligned`.
fn align_access(cx: &Context, wk: &SpvWellKnownWithExtras, func: &mut FuncDefBody, inst: DataInst) {
    let inst_def = &func.data_insts[inst];
    let form_def = &cx[inst_def.form];
    let spv_inst = match &form_def.kind {
        DataInstKind::SpvInst(spv_inst) if [wk.OpLoad, wk.OpStore].contains(&spv_inst.opcode) => {
            spv_inst
        }
        _ => return,
    };
    let Some(pointee) = physical_pointee(cx, wk, func.at(inst_def.inputs[0]).type_of(cx)) else {
        return;
    };

    let (mask, params) = match &spv_inst.imms[..] {
        [] => (0, &[][..]),
        &[spv::Imm::Short(kind, mask), ref params @ ..] if kind == wk.MemoryAccess => {
            (mask, params)
        }
        _ => unreachable!(),
    };
    let aligned = MemoryAccess::ALIGNED.bits();
    if mask & aligned != 0 {
        return;
    }
    // `Aligned` is the lowest bit with a parameter, so its literal comes first.
    let imms = [
        spv::Imm::Short(wk.MemoryAccess, mask | aligned),
        spv::Imm::Short(wk.LiteralInteger, largest_scalar_size(cx, wk, pointee)),
    ]
    .into_iter()
    .chain(params.iter().copied())
    .collect();

    func.data_insts[inst].form = cx.intern(DataInstFormDef {
        kind: DataInstKind::SpvInst(spv::Inst {
            opcode: spv_inst.opcode,
            imms,
        }),
        output_type: form_def.output_type,
    });
}

/// Size (in bytes) of the largest scalar in `ty`, which Vulkan requires accesses
/// through `PhysicalStorageBuffer` pointers to be aligned to (see
/// `VUID-StandaloneSpirv-PhysicalStorageBuffer64-04708`), so it's always sound.
fn largest_scalar_size(cx: &Context, wk: &SpvWellKnownWithExtras, ty: Type) -> u32 {
    let TypeKind::SpvInst {
        spv_inst,
        type_and_const_inputs,
    } = &cx[ty].kind
    else {
        return 1;
    };
    let opcode = spv_inst.opcode;
    if [wk.OpTypeInt, wk.OpTypeFloat].contains(&opcode) {
        let spv::Imm::Short(_, width) = spv_inst.imms[0] else {
            unreachable!()
        };
        width / 8
    } else if physical_pointee(cx, wk, ty).is_some() {
        8
    } else if [
        wk.OpTypeVector,
        wk.OpTypeMatrix,
        wk.OpTypeArray,
        wk.OpTypeRuntimeArray,
        wk.OpTypeStruct,
    ]
    .contains(&opcode)
    {
        (type_and_const_inputs.iter())
            .filter_map(|&ty_or_ct| match ty_or_ct {
                TypeOrConst::Type(ty) => Some(largest_scalar_size(cx, wk, ty)),
                TypeOrConst::Const(_) => None,
            })
            .max()
            .unwrap_or(1)
    } else {
        1
    }
}

fn physical_pointee(cx: &Context, wk: &SpvWellKnownWithExtras, ty: Type) -> Option<Type> {
    pointer_parts(cx, wk, ty)
        .and_then(|(sc, pointee)| (sc == wk.PhysicalStorageBuffer).then_some(pointee))
}

/// The storage class and pointee type of `ty`, if it's a pointer type.
fn pointer_parts(cx: &Context, wk: &SpvWellKnownWithExtras, ty: Type) -> Option<(u32, Type)> {
    match &cx[ty].kind {
        TypeKind::SpvInst {
            spv_inst,
            type_and_const_inputs,
        } if spv_inst.opcode == wk.OpTypePointer => {
            match (&spv_inst.imms[..], &type_and_const_inputs[..]) {
                (&[spv::Imm::Short(_, sc)], &[TypeOrConst::Type(pointee)]) => Some((sc, pointee)),
                _ => unreachable!(),
            }
        }
        _ => None,
    }
}
