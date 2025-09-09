// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::Builder;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{
    SpirvBlockCursor, SpirvConst, SpirvValue, SpirvValueExt, SpirvValueKind,
};
use crate::codegen_cx::CodegenCx;
use crate::custom_insts::CustomInst;
use crate::spirv_type::SpirvType;
use itertools::Itertools;
use rspirv::dr::{InsertPoint, Instruction, Operand};
use rspirv::spirv::{Capability, MemoryModel, MemorySemantics, Op, Scope, StorageClass, Word};
use rustc_abi::{Align, BackendRepr, Scalar, Size, WrappingRange};
use rustc_apfloat::{Float, Round, Status, ieee};
use rustc_codegen_ssa::MemFlags;
use rustc_codegen_ssa::common::{
    AtomicRmwBinOp, IntPredicate, RealPredicate, SynchronizationScope, TypeKind,
};
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    BackendTypes, BaseTypeCodegenMethods, BuilderMethods, ConstCodegenMethods,
    DerivedTypeCodegenMethods as _, LayoutTypeCodegenMethods, OverflowOp,
};
use rustc_middle::bug;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrs;
use rustc_middle::ty::layout::LayoutOf;
use rustc_middle::ty::{self, AtomicOrdering, Ty};
use rustc_span::Span;
use rustc_target::callconv::FnAbi;
use smallvec::SmallVec;
use std::iter::{self, empty};
use std::ops::{BitAnd, BitOr, BitXor, Not, RangeInclusive};

use crate::builder::format_args_decompiler::{CodegenPanic, DecodedFormatArgs};
use tracing::{Level, instrument, span};
use tracing::{trace, warn};

enum ConstValue {
    Unsigned(u128),
    Signed(i128),
    Bool(bool),
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    fn try_get_const_value(&self, val: SpirvValue) -> Option<ConstValue> {
        if let Some(const_val) = self.builder.lookup_const(val) {
            let x = match const_val {
                SpirvConst::Scalar(x) => x,
                _ => return None,
            };
            match self.lookup_type(val.ty) {
                SpirvType::Integer(bits, signed) => {
                    let size = Size::from_bits(bits);
                    // ensure the u128 constant didn't overflow and const-folding isn't hiding issues
                    if x == size.truncate(x) {
                        Some(if signed {
                            ConstValue::Signed(size.sign_extend(x))
                        } else {
                            ConstValue::Unsigned(size.truncate(x))
                        })
                    } else {
                        None
                    }
                }
                SpirvType::Bool => {
                    match x {
                        0 => Some(ConstValue::Bool(false)),
                        1 => Some(ConstValue::Bool(true)),
                        // ensure const-folding isn't hiding issues
                        _ => None,
                    }
                }
                _ => None,
            }
        } else {
            None
        }
    }
}

macro_rules! simple_op {
    (
        $func_name:ident
        $(, int: $inst_int:ident)?
        $(, uint: $inst_uint:ident)?
        $(, sint: $inst_sint:ident)?
        $(, float: $inst_float:ident)?
        $(, bool: $inst_bool:ident)?
        $(, fold_const {
            $(int($int_lhs:ident, $int_rhs:ident) => $fold_int:expr;)?
            $(uint($uint_lhs:ident, $uint_rhs:ident) => $fold_uint:expr;)?
            $(sint($sint_lhs:ident, $sint_rhs:ident) => $fold_sint:expr;)?
            $(bool($bool_lhs:ident, $bool_rhs:ident) => $fold_bool:expr;)?
        })?
    ) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            assert_ty_eq!(self, lhs.ty, rhs.ty);
            let result_type = lhs.ty;

            $(
                #[allow(unreachable_patterns, clippy::collapsible_match)]
                if let Some(const_lhs) = self.try_get_const_value(lhs)
                    && let Some(const_rhs) = self.try_get_const_value(rhs)
                {
                    let result = (|| Some(match (const_lhs, const_rhs) {
                        $(
                            (ConstValue::Unsigned($int_lhs), ConstValue::Unsigned($int_rhs)) => $fold_int,
                            (ConstValue::Signed($int_lhs), ConstValue::Signed($int_rhs)) => $fold_int as u128,
                        )?
                        $((ConstValue::Unsigned($uint_lhs), ConstValue::Unsigned($uint_rhs)) => $fold_uint,)?
                        $((ConstValue::Signed($sint_lhs), ConstValue::Signed($sint_rhs)) => $fold_sint as u128, )?
                        $((ConstValue::Bool($bool_lhs), ConstValue::Bool($bool_rhs)) => ($fold_bool).into(), )?
                        _ => return None,
                    }))();
                    if let Some(result) = result {
                        return self.const_uint_big(result_type, result);
                    }
                }
            )?

            match self.lookup_type(result_type) {
                $(SpirvType::Integer(_, _) => {
                    self.emit()
                        .$inst_int(result_type, None, lhs.def(self), rhs.def(self))
                })?
                $(SpirvType::Integer(_, false) => {
                    self.emit()
                        .$inst_uint(result_type, None, lhs.def(self), rhs.def(self))
                })?
                $(SpirvType::Integer(_, true) => {
                    self.emit()
                        .$inst_sint(result_type, None, lhs.def(self), rhs.def(self))
                })?
                $(SpirvType::Float(_) => {
                    self.emit()
                        .$inst_float(result_type, None, lhs.def(self), rhs.def(self))
                })?
                $(SpirvType::Bool => {
                    self.emit()
                        .$inst_bool(result_type, None, lhs.def(self), rhs.def(self))
                })?
                o => self.fatal(format!(
                    concat!(stringify!($func_name), "() not implemented for type {}"),
                    o.debug(result_type, self)
                )),
            }
            .unwrap()
            .with_type(result_type)
        }
    };
}

// shl and shr allow different types as their operands
macro_rules! simple_shift_op {
    (
        $func_name:ident
        $(, int: $inst_int:ident)?
        $(, uint: $inst_uint:ident)?
        $(, sint: $inst_sint:ident)?
        $(, fold_const {
            $(int($int_lhs:ident, $int_rhs:ident) => $fold_int:expr;)?
            $(uint($uint_lhs:ident, $uint_rhs:ident) => $fold_uint:expr;)?
            $(sint($sint_lhs:ident, $sint_rhs:ident) => $fold_sint:expr;)?
        })?
    ) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            let result_type = lhs.ty;

            $(
                #[allow(unreachable_patterns, clippy::collapsible_match)]
                if let Some(const_lhs) = self.try_get_const_value(lhs)
                    && let Some(const_rhs) = self.try_get_const_value(rhs)
                {
                    let result = (|| Some(match (const_lhs, const_rhs) {
                        $(
                            (ConstValue::Unsigned($int_lhs), ConstValue::Unsigned($int_rhs)) => $fold_int,
							(ConstValue::Unsigned($int_lhs), ConstValue::Signed($int_rhs)) => $fold_int,
							(ConstValue::Signed($int_lhs), ConstValue::Unsigned($int_rhs)) => $fold_int as u128,
							(ConstValue::Signed($int_lhs), ConstValue::Signed($int_rhs)) => $fold_int as u128,
						)?
						$(
							(ConstValue::Unsigned($uint_lhs), ConstValue::Unsigned($uint_rhs)) => $fold_uint,
                            (ConstValue::Unsigned($uint_lhs), ConstValue::Signed($uint_rhs)) => $fold_uint,
                        )?
                        $(
                            (ConstValue::Signed($sint_lhs), ConstValue::Unsigned($sint_rhs)) => $fold_sint as u128,
                            (ConstValue::Signed($sint_lhs), ConstValue::Signed($sint_rhs)) => $fold_sint as u128,
                        )?
                        _ => return None,
                    }))();
                    if let Some(result) = result {
                        return self.const_uint_big(result_type, result);
                    }
                }
            )?

            match self.lookup_type(result_type) {
                $(SpirvType::Integer(_, _) => {
                    self.emit()
                        .$inst_int(result_type, None, lhs.def(self), rhs.def(self))
                })?
                $(SpirvType::Integer(_, false) => {
                    self.emit()
                        .$inst_uint(result_type, None, lhs.def(self), rhs.def(self))
                })?
                $(SpirvType::Integer(_, true) => {
                    self.emit()
                        .$inst_sint(result_type, None, lhs.def(self), rhs.def(self))
                })?
                o => self.fatal(format!(
                    concat!(stringify!($func_name), "() not implemented for type {}"),
                    o.debug(result_type, self)
                )),
            }
            .unwrap()
            .with_type(result_type)
        }
    };
}

macro_rules! simple_uni_op {
    (
        $func_name:ident
        $(, int: $inst_int:ident)?
        $(, uint: $inst_uint:ident)?
        $(, sint: $inst_sint:ident)?
        $(, float: $inst_float:ident)?
        $(, bool: $inst_bool:ident)?
        $(, fold_const {
            $(int($int_val:ident) => $fold_int:expr;)?
            $(uint($uint_val:ident) => $fold_uint:expr;)?
            $(sint($sint_val:ident) => $fold_sint:expr;)?
            $(bool($bool_val:ident) => $fold_bool:expr;)?
        })?
    ) => {
        fn $func_name(&mut self, val: Self::Value) -> Self::Value {
            let result_type = val.ty;

            $(
                #[allow(unreachable_patterns, clippy::collapsible_match)]
                if let Some(const_val) = self.try_get_const_value(val) {
                    let result = (|| Some(match (const_val) {
                        $(
                            ConstValue::Unsigned($int_val) => $fold_int,
                            ConstValue::Signed($int_val) => $fold_int as u128,
                        )?
                        $(ConstValue::Unsigned($uint_val) => $fold_uint, )?
                        $(ConstValue::Signed($sint_val) => $fold_sint as u128, )?
                        $(ConstValue::Bool($bool_val) => ($fold_bool).into(), )?
                        _ => return None,
                    }))();
                    if let Some(result) = result {
                        return self.const_uint_big(result_type, result);
                    }
                }
            )?

            match self.lookup_type(result_type) {
                $(SpirvType::Integer(_, _) => {
                    self.emit()
                        .$inst_int(result_type, None, val.def(self))
                })?
                $(SpirvType::Integer(_, false) => {
                    self.emit()
                        .$inst_uint(result_type, None, val.def(self))
                })?
                $(SpirvType::Integer(_, true) => {
                    self.emit()
                        .$inst_sint(result_type, None, val.def(self))
                })?
                $(SpirvType::Float(_) => {
                    self.emit()
                        .$inst_float(result_type, None, val.def(self))
                })?
                $(SpirvType::Bool => {
                    self.emit()
                        .$inst_bool(result_type, None, val.def(self))
                })?
                o => self.fatal(format!(
                    concat!(stringify!($func_name), "() not implemented for type {}"),
                    o.debug(result_type, self)
                )),
            }
            .unwrap()
            .with_type(result_type)
        }
    };
}

fn memset_fill_u16(b: u8) -> u16 {
    b as u16 | ((b as u16) << 8)
}

fn memset_fill_u32(b: u8) -> u32 {
    b as u32 | ((b as u32) << 8) | ((b as u32) << 16) | ((b as u32) << 24)
}

fn memset_fill_u64(b: u8) -> u64 {
    b as u64
        | ((b as u64) << 8)
        | ((b as u64) << 16)
        | ((b as u64) << 24)
        | ((b as u64) << 32)
        | ((b as u64) << 40)
        | ((b as u64) << 48)
        | ((b as u64) << 56)
}

fn memset_dynamic_scalar(
    builder: &mut Builder<'_, '_>,
    fill_var: Word,
    byte_width: usize,
    is_float: bool,
) -> Word {
    let composite_type = SpirvType::simd_vector(
        builder,
        builder.span(),
        SpirvType::Integer(8, false),
        byte_width as u32,
    )
    .def(builder.span(), builder);
    let composite = builder
        .emit()
        .composite_construct(composite_type, None, iter::repeat_n(fill_var, byte_width))
        .unwrap();
    let result_type = if is_float {
        SpirvType::Float(byte_width as u32 * 8)
    } else {
        SpirvType::Integer(byte_width as u32 * 8, false)
    };
    builder
        .emit()
        .bitcast(result_type.def(builder.span(), builder), None, composite)
        .unwrap()
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    #[instrument(level = "trace", skip(self))]
    fn ordering_to_semantics_def(&mut self, ordering: AtomicOrdering) -> SpirvValue {
        let mut invalid_seq_cst = false;
        let semantics = match ordering {
            AtomicOrdering::Relaxed => MemorySemantics::NONE,
            AtomicOrdering::Acquire => MemorySemantics::MAKE_VISIBLE | MemorySemantics::ACQUIRE,
            AtomicOrdering::Release => MemorySemantics::MAKE_AVAILABLE | MemorySemantics::RELEASE,
            AtomicOrdering::AcqRel => {
                MemorySemantics::MAKE_AVAILABLE
                    | MemorySemantics::MAKE_VISIBLE
                    | MemorySemantics::ACQUIRE_RELEASE
            }
            AtomicOrdering::SeqCst => {
                let builder = self.emit();
                let memory_model = builder.module_ref().memory_model.as_ref().unwrap();
                if memory_model.operands[1].unwrap_memory_model() == MemoryModel::Vulkan {
                    invalid_seq_cst = true;
                }
                MemorySemantics::MAKE_AVAILABLE
                    | MemorySemantics::MAKE_VISIBLE
                    | MemorySemantics::SEQUENTIALLY_CONSISTENT
            }
        };
        let semantics = self.constant_u32(self.span(), semantics.bits());
        if invalid_seq_cst {
            self.zombie(
                semantics.def(self),
                "cannot use `AtomicOrdering::SeqCst` on Vulkan memory model \
                 (check if `AcqRel` fits your needs)",
            );
        }
        semantics
    }

    #[instrument(level = "trace", skip(self))]
    fn memset_const_pattern(&self, ty: &SpirvType<'tcx>, fill_byte: u8) -> Word {
        match *ty {
            SpirvType::Void => self.fatal("memset invalid on void pattern"),
            SpirvType::Bool => self.fatal("memset invalid on bool pattern"),
            SpirvType::Integer(width, false) => match width {
                8 => self.constant_u8(self.span(), fill_byte).def(self),
                16 => self
                    .constant_u16(self.span(), memset_fill_u16(fill_byte))
                    .def(self),
                32 => self
                    .constant_u32(self.span(), memset_fill_u32(fill_byte))
                    .def(self),
                64 => self
                    .constant_u64(self.span(), memset_fill_u64(fill_byte))
                    .def(self),
                _ => self.fatal(format!(
                    "memset on integer width {width} not implemented yet"
                )),
            },
            SpirvType::Integer(width, true) => match width {
                8 => self.constant_i8(self.span(), fill_byte as i8).def(self),
                16 => self
                    .constant_i16(self.span(), memset_fill_u16(fill_byte) as i16)
                    .def(self),
                32 => self
                    .constant_i32(self.span(), memset_fill_u32(fill_byte) as i32)
                    .def(self),
                64 => self
                    .constant_i64(self.span(), memset_fill_u64(fill_byte) as i64)
                    .def(self),
                _ => self.fatal(format!(
                    "memset on integer width {width} not implemented yet"
                )),
            },
            SpirvType::Float(width) => match width {
                32 => self
                    .constant_f32(self.span(), f32::from_bits(memset_fill_u32(fill_byte)))
                    .def(self),
                64 => self
                    .constant_f64(self.span(), f64::from_bits(memset_fill_u64(fill_byte)))
                    .def(self),
                _ => self.fatal(format!("memset on float width {width} not implemented yet")),
            },
            SpirvType::Adt { .. } => self.fatal("memset on structs not implemented yet"),
            SpirvType::Vector { element, count, .. } | SpirvType::Matrix { element, count } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                self.constant_composite(
                    ty.def(self.span(), self),
                    iter::repeat_n(elem_pat, count as usize),
                )
                .def(self)
            }
            SpirvType::Array {
                element,
                count,
                stride: _,
                is_physical: _,
            } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                let count = self.builder.lookup_const_scalar(count).unwrap() as usize;
                self.constant_composite(ty.def(self.span(), self), iter::repeat_n(elem_pat, count))
                    .def(self)
            }
            SpirvType::RuntimeArray { .. } => {
                self.fatal("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => self.fatal("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => self.fatal("memset on functions not implemented yet"),
            SpirvType::Image { .. } => self.fatal("cannot memset image"),
            SpirvType::Sampler => self.fatal("cannot memset sampler"),
            SpirvType::SampledImage { .. } => self.fatal("cannot memset sampled image"),
            SpirvType::InterfaceBlock { .. } => self.fatal("cannot memset interface block"),
            SpirvType::AccelerationStructureKhr => {
                self.fatal("cannot memset acceleration structure")
            }
            SpirvType::RayQueryKhr => self.fatal("cannot memset ray query"),
        }
    }

    #[instrument(level = "trace", skip(self))]
    fn memset_dynamic_pattern(&mut self, ty: &SpirvType<'tcx>, fill_var: Word) -> Word {
        match *ty {
            SpirvType::Void => self.fatal("memset invalid on void pattern"),
            SpirvType::Bool => self.fatal("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
                8 => fill_var,
                16 => memset_dynamic_scalar(self, fill_var, 2, false),
                32 => memset_dynamic_scalar(self, fill_var, 4, false),
                64 => memset_dynamic_scalar(self, fill_var, 8, false),
                _ => self.fatal(format!(
                    "memset on integer width {width} not implemented yet"
                )),
            },
            SpirvType::Float(width) => match width {
                32 => memset_dynamic_scalar(self, fill_var, 4, true),
                64 => memset_dynamic_scalar(self, fill_var, 8, true),
                _ => self.fatal(format!("memset on float width {width} not implemented yet")),
            },
            SpirvType::Adt { .. } => self.fatal("memset on structs not implemented yet"),
            SpirvType::Array {
                element,
                count,
                stride: _,
                is_physical: _,
            } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                let count = self.builder.lookup_const_scalar(count).unwrap() as usize;
                self.emit()
                    .composite_construct(
                        ty.def(self.span(), self),
                        None,
                        iter::repeat_n(elem_pat, count),
                    )
                    .unwrap()
            }
            SpirvType::Vector { element, count, .. } | SpirvType::Matrix { element, count } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                self.emit()
                    .composite_construct(
                        ty.def(self.span(), self),
                        None,
                        iter::repeat_n(elem_pat, count as usize),
                    )
                    .unwrap()
            }
            SpirvType::RuntimeArray { .. } => {
                self.fatal("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => self.fatal("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => self.fatal("memset on functions not implemented yet"),
            SpirvType::Image { .. } => self.fatal("cannot memset image"),
            SpirvType::Sampler => self.fatal("cannot memset sampler"),
            SpirvType::SampledImage { .. } => self.fatal("cannot memset sampled image"),
            SpirvType::InterfaceBlock { .. } => self.fatal("cannot memset interface block"),
            SpirvType::AccelerationStructureKhr => {
                self.fatal("cannot memset acceleration structure")
            }
            SpirvType::RayQueryKhr => self.fatal("cannot memset ray query"),
        }
    }

    #[instrument(level = "trace", skip(self))]
    fn memset_constant_size(&mut self, ptr: SpirvValue, pat: SpirvValue, size_bytes: u64) {
        let size_elem = self
            .lookup_type(pat.ty)
            .sizeof(self)
            .expect("Memset on unsized values not supported");
        let count = size_bytes / size_elem.bytes();
        if count == 1 {
            self.store(pat, ptr, Align::from_bytes(0).unwrap());
        } else {
            for index in 0..count {
                let const_index = self.constant_u32(self.span(), index as u32);
                let gep_ptr = self.inbounds_gep(pat.ty, ptr, &[const_index]);
                self.store(pat, gep_ptr, Align::from_bytes(0).unwrap());
            }
        }
    }

    // TODO: Test this is correct
    #[instrument(level = "trace", skip(self))]
    fn memset_dynamic_size(&mut self, ptr: SpirvValue, pat: SpirvValue, size_bytes: SpirvValue) {
        let size_elem = self
            .lookup_type(pat.ty)
            .sizeof(self)
            .expect("Unable to memset a dynamic sized object");
        let size_elem_const = self.constant_int(size_bytes.ty, size_elem.bytes().into());
        let zero = self.constant_int(size_bytes.ty, 0);
        let one = self.constant_int(size_bytes.ty, 1);
        let zero_align = Align::from_bytes(0).unwrap();

        let header_bb = self.append_sibling_block("memset_header");
        let body_bb = self.append_sibling_block("memset_body");
        let exit_bb = self.append_sibling_block("memset_exit");

        let count = self.udiv(size_bytes, size_elem_const);
        let index = self.alloca(self.lookup_type(count.ty).sizeof(self).unwrap(), zero_align);
        self.store(zero, index, zero_align);
        self.br(header_bb);

        self.switch_to_block(header_bb);
        let current_index = self.load(count.ty, index, zero_align);
        let cond = self.icmp(IntPredicate::IntULT, current_index, count);
        self.cond_br(cond, body_bb, exit_bb);

        self.switch_to_block(body_bb);
        let gep_ptr = self.gep(pat.ty, ptr, &[current_index]);
        self.store(pat, gep_ptr, zero_align);
        let current_index_plus_1 = self.add(current_index, one);
        self.store(current_index_plus_1, index, zero_align);
        self.br(header_bb);

        self.switch_to_block(exit_bb);
    }

    #[instrument(level = "trace", skip(self))]
    fn zombie_convert_ptr_to_u(&self, def: Word) {
        self.zombie(def, "cannot convert pointers to integers");
    }

    #[instrument(level = "trace", skip(self))]
    fn zombie_convert_u_to_ptr(&self, def: Word) {
        self.zombie(def, "cannot convert integers to pointers");
    }

    #[instrument(level = "trace", skip(self))]
    fn zombie_ptr_equal(&self, def: Word, inst: &str) {
        if !self.builder.has_capability(Capability::VariablePointers) {
            self.zombie(
                def,
                &format!("{inst} without OpCapability VariablePointers"),
            );
        }
    }

    /// Convenience wrapper for `adjust_pointer_for_sized_access`, falling back
    /// on choosing `ty` as the leaf's type (and casting `ptr` to a pointer to it).
    //
    // HACK(eddyb) temporary workaround for untyped pointers upstream.
    // FIXME(eddyb) replace with untyped memory SPIR-V + `qptr` or similar.
    #[instrument(level = "trace", skip(self), fields(ptr, ty = ?self.debug_type(ty)))]
    fn adjust_pointer_for_typed_access(
        &mut self,
        ptr: SpirvValue,
        ty: <Self as BackendTypes>::Type,
    ) -> (SpirvValue, <Self as BackendTypes>::Type) {
        self.lookup_type(ty)
            .sizeof(self)
            .filter(|_| {
                // HACK(eddyb) prefer pointer casts with `qptr`.
                self.codegen_args.linker_opts.infer_storage_classes
            })
            .and_then(|size| self.adjust_pointer_for_sized_access(ptr, size))
            .unwrap_or_else(|| (self.pointercast(ptr, self.type_ptr_to(ty)), ty))
    }

    /// If `ptr`'s pointee type contains any prefix field/element of size `size`,
    /// i.e. some leaf which can be used for all accesses of size `size`, return
    /// `ptr` adjusted to point to the innermost such leaf, and the leaf's type.
    //
    // FIXME(eddyb) technically this duplicates `pointercast`, but the main use
    // of `pointercast` is being replaced by this, and this can be more efficient.
    //
    // HACK(eddyb) temporary workaround for untyped pointers upstream.
    // FIXME(eddyb) replace with untyped memory SPIR-V + `qptr` or similar.
    #[instrument(level = "trace", skip(self))]
    fn adjust_pointer_for_sized_access(
        &mut self,
        ptr: SpirvValue,
        size: Size,
    ) -> Option<(SpirvValue, <Self as BackendTypes>::Type)> {
        let ptr = ptr.strip_ptrcasts();
        let mut leaf_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee, .. } => pointee?,
            other => self.fatal(format!("`ptr` is non-pointer type: {other:?}")),
        };

        trace!(
            "before nested adjust_pointer_for_sized_access. `leaf_ty`: {}",
            self.debug_type(leaf_ty)
        );

        let mut indices = SmallVec::<[_; 8]>::new();
        while let Some((inner_indices, inner_ty)) = self.recover_access_chain_from_offset(
            leaf_ty,
            Size::ZERO,
            Some(size)..=Some(size),
            None,
        ) {
            indices.extend(inner_indices);
            leaf_ty = inner_ty;
        }

        trace!(
            "after nested adjust_pointer_for_sized_access. `leaf_ty`: {}",
            self.debug_type(leaf_ty)
        );

        let leaf_ptr_ty = (self.lookup_type(leaf_ty).sizeof(self) == Some(size))
            .then(|| self.type_ptr_to(leaf_ty))?;
        let leaf_ptr = if indices.is_empty() {
            assert_ty_eq!(self, ptr.ty, leaf_ptr_ty);
            ptr
        } else {
            let indices = indices
                .into_iter()
                .map(|idx| self.constant_u32(self.span(), idx).def(self))
                .collect::<Vec<_>>();
            self.emit()
                .in_bounds_access_chain(leaf_ptr_ty, None, ptr.def(self), indices)
                .unwrap()
                .with_type(leaf_ptr_ty)
        };

        trace!(
            "adjust_pointer_for_sized_access returning {} {}",
            self.debug_type(leaf_ptr.ty),
            self.debug_type(leaf_ty)
        );
        Some((leaf_ptr, leaf_ty))
    }

    /// If possible, return the appropriate `OpAccessChain` indices for going
    /// from a pointer to `ty`, to a pointer to some leaf field/element having
    /// a size that fits `leaf_size_range` (and, optionally, the type `leaf_ty`),
    /// while adding `offset` bytes.
    ///
    /// That is, try to turn `((_: *T) as *u8).add(offset) as *Leaf` into a series
    /// of struct field and array/vector element accesses.
    #[instrument(level = "trace", skip(self), fields(ty = ?self.debug_type(ty), leaf_size_or_unsized_range, leaf_ty = ?leaf_ty))]
    fn recover_access_chain_from_offset(
        &self,
        mut ty: <Self as BackendTypes>::Type,
        mut offset: Size,
        // FIXME(eddyb) using `None` for "unsized" is a pretty bad design.
        leaf_size_or_unsized_range: RangeInclusive<Option<Size>>,
        leaf_ty: Option<<Self as BackendTypes>::Type>,
    ) -> Option<(SmallVec<[u32; 8]>, <Self as BackendTypes>::Type)> {
        assert_ne!(Some(ty), leaf_ty);
        if let Some(leaf_ty) = leaf_ty {
            trace!(
                "recovering access chain: leaf_ty: {:?}",
                self.debug_type(leaf_ty)
            );
        } else {
            trace!("recovering access chain: leaf_ty: None");
        }

        // HACK(eddyb) this has the correct ordering (`Sized(_) < Unsized`).
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
        enum MaybeSized {
            Sized(Size),
            Unsized,
        }
        let leaf_size_range = {
            let r = leaf_size_or_unsized_range;
            let [start, end] =
                [r.start(), r.end()].map(|x| x.map_or(MaybeSized::Unsized, MaybeSized::Sized));
            start..=end
        };

        trace!("leaf_size_range: {:?}", leaf_size_range);

        // NOTE(eddyb) `ty` and `ty_kind`/`ty_size` should be kept in sync.
        let mut ty_kind = self.lookup_type(ty);

        let mut indices = SmallVec::new();
        loop {
            let ty_size;
            match ty_kind {
                SpirvType::Adt {
                    field_types,
                    field_offsets,
                    ..
                } => {
                    trace!("recovering access chain from ADT");
                    let (i, field_ty, field_ty_kind, field_ty_size, offset_in_field) = field_offsets
                        .iter()
                        .enumerate()
                        .find_map(|(i, &field_offset)| {
                            if field_offset > offset {
                                return None;
                            }

                            // Grab the actual field type to be able to confirm that
                            // the leaf is somewhere inside the field.
                            let field_ty = field_types[i];
                            let field_ty_kind = self.lookup_type(field_ty);
                            let field_ty_size = field_ty_kind
                                .sizeof(self).map_or(MaybeSized::Unsized, MaybeSized::Sized);

                            let offset_in_field = offset - field_offset;
                            if MaybeSized::Sized(offset_in_field) < field_ty_size
                                // If the field is a zero sized type, check the
                                // expected size and type to get the correct entry
                                || offset_in_field == Size::ZERO
                                    && leaf_size_range.contains(&MaybeSized::Sized(Size::ZERO)) && leaf_ty == Some(field_ty)
                            {
                                Some((i, field_ty, field_ty_kind, field_ty_size, offset_in_field))
                            } else {
                                None
                            }
                        })?;

                    ty = field_ty;
                    trace!("setting ty = field_ty: {:?}", self.debug_type(field_ty));
                    ty_kind = field_ty_kind;
                    trace!("setting ty_kind = field_ty_kind: {:?}", field_ty_kind);
                    ty_size = field_ty_size;
                    trace!("setting ty_size = field_ty_size: {:?}", field_ty_size);

                    indices.push(i as u32);
                    offset = offset_in_field;
                    trace!("setting offset = offset_in_field: {:?}", offset_in_field);
                }
                SpirvType::Vector { element, .. }
                | SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element, .. }
                | SpirvType::Matrix { element, .. } => {
                    trace!("recovering access chain from Vector, Array, RuntimeArray, or Matrix");
                    ty = element;
                    trace!("setting ty = element: {:?}", self.debug_type(element));
                    ty_kind = self.lookup_type(ty);
                    trace!("looked up ty kind: {:?}", ty_kind);

                    let stride = ty_kind.sizeof(self)?;
                    ty_size = MaybeSized::Sized(stride);

                    if stride == Size::ZERO {
                        if offset != Size::ZERO {
                            trace!("zero-sized element with non-zero offset: {:?}", offset);
                            return None;
                        }

                        indices.push(0);
                        offset = Size::ZERO;
                    } else {
                        indices.push((offset.bytes() / stride.bytes()).try_into().ok()?);
                        offset = Size::from_bytes(offset.bytes() % stride.bytes());
                    }
                }
                _ => {
                    trace!("recovering access chain from SOMETHING ELSE, RETURNING NONE");
                    return None;
                }
            }

            // Avoid digging beyond the point the leaf could actually fit.
            if ty_size < *leaf_size_range.start() {
                trace!("avoiding digging beyond the point the leaf could actually fit");
                return None;
            }

            if offset == Size::ZERO
                && leaf_size_range.contains(&ty_size)
                && leaf_ty.is_none_or(|leaf_ty| leaf_ty == ty)
            {
                trace!("successful recovery leaf type: {:?}", self.debug_type(ty));
                trace!("successful recovery indices: {:?}", indices);
                return Some((indices, ty));
            }
        }
    }

    // NOTE(eddyb) see `ptr_offset_strided`, which this now forwards to.
    #[instrument(level = "trace", skip(self), fields(ty = ?self.debug_type(ty), ptr, combined_indices = ?combined_indices.iter().map(|x| (self.debug_type(x.ty), x.kind)).collect::<Vec<_>>(), is_inbounds))]
    fn maybe_inbounds_gep(
        &mut self,
        // Represents the type of the element that `ptr` is assumed to point to
        // *before* applying the *first* index (`ptr_base_index`). This type is used
        // primarily to calculate the size for the initial offset.
        ty: Word,
        //  The base pointer value for the GEP operation.
        ptr: SpirvValue,
        // A slice of indices used for the GEP calculation. The *first* index is treated
        // as the main offset from the base pointer `ptr`, scaled by the size of `ty`.
        // Subsequent indices navigate through nested aggregate types (structs, arrays,
        // vectors, etc.) starting from `ty`.
        combined_indices: &[SpirvValue],
        // If true, generate an `OpInBoundsAccessChain`, which has stricter requirements
        // but allows more optimization. If false, generate `OpAccessChain`.
        is_inbounds: bool,
    ) -> SpirvValue {
        // Separate the first index (used for the base offset) from the rest of the
        // indices (used for navigating within the aggregate type). `ptr_base_index` is
        // the index applied directly to `ptr`, effectively an offset multiplier based
        // on the size of `ty`. `indices` are the subsequent indices used to drill down
        // into fields or elements of `ty`.
        // https://llvm.org/docs/GetElementPtr.html
        // "An OpAccessChain instruction is the equivalent of an LLVM getelementptr instruction where the first index element is zero."
        // https://github.com/gpuweb/gpuweb/issues/33
        let (&ptr_base_index, indices) = combined_indices.split_first().unwrap();

        // HACK(eddyb) this effectively removes any real support for GEPs with
        // any `indices` (beyond `ptr_base_index`), which should now be the case
        // across `rustc_codegen_ssa` (see also comment inside `inbounds_gep`).
        if !indices.is_empty() {
            self.fatal(format!(
                "[RUST-GPU BUG] `inbounds_gep` or `gep` called \
                 with {} combined indices (expected only 1)",
                combined_indices.len(),
            ));
        }

        self.ptr_offset_strided(ptr, ty, ptr_base_index, is_inbounds)
    }

    /// Array-indexing-like pointer arithmetic, i.e. `(ptr: *T).offset(index)`
    /// (or `wrapping_offset` instead of `offset`, for `is_inbounds = false`),
    /// where `T` is given by `stride_elem_ty` (named so for extra clarity).
    ///
    /// This can produce legal SPIR-V by using 3 strategies:
    /// 1. noop, i.e. returning `ptr` unmodified, comparable to a `pointercast`
    ///    (but instead letting downstream users do any casts they might need,
    ///     themselves - also, upstream untyped pointers mean that no pointer
    ///     can be expected to have any specific pointee type)
    /// 2. `recover_access_chain_from_offset` for constant offsets
    ///    (e.g. from `ptradd`/`inbounds_ptradd` used to access `struct` fields)
    /// 3. merging onto an array `OpAccessChain` with the same `stride_elem_ty`
    ///    (possibly `&array[0]` from `pointercast` doing `*[T; N]` -> `*T`)
    ///
    /// Also, `pointercast` (used downstream, or as part of strategy 3.) helps
    /// with producing legal SPIR-V, as it allows deferring whole casts chains,
    /// and has a couple success modes of its own:
    /// - itself can also use `recover_access_chain_from_offset`, supporting
    ///   `struct`/array casts e.g. `*(T, U, ...)` -> `*T` / `*[T; N]` -> `*T`
    /// - even if a specific cast is unsupported, legal SPIR-V can still be
    ///   later obtained (thanks to `SpirvValueKind::LogicalPtrCast`), if all
    ///   uses of that cast rely on `pointercast` and/or `strip_ptrcasts`, e.g.:
    ///   - another `ptr_offset_strided` call (with a different offset)
    ///   - `adjust_pointer_for_typed_access`/`adjust_pointer_for_sized_access`
    ///     (themselves used by e.g. loads, stores, copies, etc.)
    //
    // FIXME(eddyb) maybe move the above `pointercast` section to its own docs?
    #[instrument(level = "trace", skip(self), fields(ptr, stride_elem_ty = ?self.debug_type(stride_elem_ty), index, is_inbounds))]
    fn ptr_offset_strided(
        &mut self,
        ptr: SpirvValue,
        stride_elem_ty: Word,
        index: SpirvValue,
        is_inbounds: bool,
    ) -> SpirvValue {
        // Precompute a constant `index * stride` (i.e. effective pointer offset)
        // if possible, as both strategies 1 and 2 rely on knowing this value.
        let const_offset = self.builder.lookup_const_scalar(index).and_then(|idx| {
            let idx_u64 = u64::try_from(idx).ok()?;
            let stride = self.lookup_type(stride_elem_ty).sizeof(self)?;
            Some(idx_u64 * stride)
        });

        // Strategy 1: do nothing for a `0` offset (and `stride_elem_ty` can be
        // safely ignored, because any typed uses will `pointercast` if needed).
        if const_offset == Some(Size::ZERO) {
            trace!("ptr_offset_strided: strategy 1 picked: offset 0 => noop");

            return ptr;
        }

        // Remove any (redundant) pointer casts applied to the input `ptr`,
        // to obtain the "most original" pointer (which ideally will be e.g.
        // a whole `OpVariable`, or the result of a previous `OpAccessChain`).
        let original_ptr = ptr.strip_ptrcasts();
        let original_pointee_ty = match self.lookup_type(original_ptr.ty) {
            SpirvType::Pointer { pointee, .. } => pointee,
            other => self.fatal(format!("pointer arithmetic on non-pointer type {other:?}")),
        };

        // Strategy 2: try recovering an `OpAccessChain` from a constant offset.
        if let (Some(original_pointee_ty), Some(const_offset)) = (original_pointee_ty, const_offset)
            && let Some((const_indices, leaf_pointee_ty)) = self.recover_access_chain_from_offset(
                original_pointee_ty,
                const_offset,
                Some(Size::ZERO)..=None,
                None,
            )
        {
            trace!(
                "ptr_offset_strided: strategy 2 picked: offset {const_offset:?} \
                     => access chain w/ {const_indices:?}"
            );

            let leaf_ptr_ty = self.type_ptr_to(leaf_pointee_ty);
            let original_ptr_id = original_ptr.def(self);
            let const_indices_ids = const_indices
                .into_iter()
                .map(|idx| self.constant_u32(self.span(), idx).def(self))
                .collect();

            return self.emit_access_chain(
                leaf_ptr_ty,
                original_ptr_id,
                None,
                const_indices_ids,
                is_inbounds,
            );
        }

        // Strategy 3: try merging onto an `OpAccessChain` of matching type,
        // and this starts by `pointercast`-ing to that type for two reasons:
        // - this is the only type a merged `OpAccessChain` could produce
        // - `pointercast` can itself produce a new `OpAccessChain` in the
        //   right circumstances (e.g. `&array[0]` for `*[T; N]` -> `*T`)
        let ptr = self.pointercast(ptr, self.type_ptr_to(stride_elem_ty));
        let ptr_id = ptr.def(self);

        let maybe_original_access_chain = {
            let builder = self.emit();
            let module = builder.module_ref();
            let current_func_blocks = builder
                .selected_function()
                .and_then(|func_idx| Some(&module.functions.get(func_idx)?.blocks[..]))
                .unwrap_or_default();

            // NOTE(eddyb) reverse search (`rfind`) used in the hopes that the
            // instruction producing the value with ID `ptr_id` is more likely to
            // have been added more recently to the function, even though there's
            // still a risk of this causing a whole-function traversal.
            //
            // FIXME(eddyb) consider tracking that via e.g. `SpirvValueKind`.
            current_func_blocks
                .iter()
                .flat_map(|b| &b.instructions)
                .rfind(|inst| inst.result_id == Some(ptr_id))
                .filter(|inst| {
                    matches!(inst.class.opcode, Op::AccessChain | Op::InBoundsAccessChain)
                })
                .map(|inst| {
                    let base_ptr = inst.operands[0].unwrap_id_ref();
                    let indices = inst.operands[1..]
                        .iter()
                        .map(|op| op.unwrap_id_ref())
                        .collect::<Vec<_>>();
                    (base_ptr, indices)
                })
        };
        if let Some((original_ptr, original_indices)) = maybe_original_access_chain {
            trace!("ptr_offset_strided: strategy 3 picked: merging access chains");

            let mut merged_indices = original_indices;

            let last_index_id = merged_indices.last_mut().unwrap();
            *last_index_id = self.add(last_index_id.with_type(index.ty), index).def(self);

            return self.emit_access_chain(ptr.ty, original_ptr, None, merged_indices, is_inbounds);
        }

        // None of the legalizing strategies above applied, so this operation
        // isn't really supported (and will error if actually used from a shader).
        //
        // FIXME(eddyb) supersede via SPIR-T pointer legalization (e.g. `qptr`).
        trace!("ptr_offset_strided: falling back to (illegal) `OpPtrAccessChain`");

        let result_ptr = if is_inbounds {
            self.emit()
                .in_bounds_ptr_access_chain(ptr.ty, None, ptr_id, index.def(self), vec![])
        } else {
            self.emit()
                .ptr_access_chain(ptr.ty, None, ptr_id, index.def(self), vec![])
        }
        .unwrap();
        self.zombie(
            result_ptr,
            "cannot offset a pointer to an arbitrary element",
        );
        result_ptr.with_type(ptr.ty)
    }

    #[instrument(
        level = "trace",
        skip(self),
        fields(
            result_type = ?self.debug_type(result_type),
             pointer,
             ptr_base_index,
             indices,
             is_inbounds
            )
        )]
    fn emit_access_chain(
        &mut self,
        result_type: <Self as BackendTypes>::Type,
        pointer: Word,
        ptr_base_index: Option<SpirvValue>,
        indices: Vec<Word>,
        is_inbounds: bool,
    ) -> SpirvValue {
        let mut builder = self.emit();

        let non_zero_ptr_base_index =
            ptr_base_index.filter(|&idx| self.builder.lookup_const_scalar(idx) != Some(0));
        if let Some(ptr_base_index) = non_zero_ptr_base_index {
            let result = if is_inbounds {
                builder.in_bounds_ptr_access_chain(
                    result_type,
                    None,
                    pointer,
                    ptr_base_index.def(self),
                    indices,
                )
            } else {
                builder.ptr_access_chain(
                    result_type,
                    None,
                    pointer,
                    ptr_base_index.def(self),
                    indices,
                )
            }
            .unwrap();
            self.zombie(result, "cannot offset a pointer to an arbitrary element");
            result
        } else {
            if is_inbounds {
                builder.in_bounds_access_chain(result_type, None, pointer, indices)
            } else {
                builder.access_chain(result_type, None, pointer, indices)
            }
            .unwrap()
        }
        .with_type(result_type)
    }

    #[instrument(level = "trace", skip(self))]
    fn fptoint_sat(
        &mut self,
        signed: bool,
        val: SpirvValue,
        dest_ty: <Self as BackendTypes>::Type,
    ) -> SpirvValue {
        // This uses the old llvm emulation to implement saturation

        let src_ty = self.cx.val_ty(val);
        let (float_ty, int_ty) = if self.cx.type_kind(src_ty) == TypeKind::Vector {
            assert_eq!(
                self.cx.vector_length(src_ty),
                self.cx.vector_length(dest_ty)
            );
            (self.cx.element_type(src_ty), self.cx.element_type(dest_ty))
        } else {
            (src_ty, dest_ty)
        };
        let int_width = self.cx().int_width(int_ty);
        let float_width = self.cx().float_width(float_ty);
        // LLVM's fpto[su]i returns undef when the input x is infinite, NaN, or does not fit into the
        // destination integer type after rounding towards zero. This `undef` value can cause UB in
        // safe code (see issue #10184), so we implement a saturating conversion on top of it:
        // Semantically, the mathematical value of the input is rounded towards zero to the next
        // mathematical integer, and then the result is clamped into the range of the destination
        // integer type. Positive and negative infinity are mapped to the maximum and minimum value of
        // the destination integer type. NaN is mapped to 0.
        //
        // Define f_min and f_max as the largest and smallest (finite) floats that are exactly equal to
        // a value representable in int_ty.
        // They are exactly equal to int_ty::{MIN,MAX} if float_ty has enough significand bits.
        // Otherwise, int_ty::MAX must be rounded towards zero, as it is one less than a power of two.
        // int_ty::MIN, however, is either zero or a negative power of two and is thus exactly
        // representable. Note that this only works if float_ty's exponent range is sufficiently large.
        // f16 or 256 bit integers would break this property. Right now the smallest float type is f32
        // with exponents ranging up to 127, which is barely enough for i128::MIN = -2^127.
        // On the other hand, f_max works even if int_ty::MAX is greater than float_ty::MAX. Because
        // we're rounding towards zero, we just get float_ty::MAX (which is always an integer).
        // This already happens today with u128::MAX = 2^128 - 1 > f32::MAX.
        let int_max = |signed: bool, int_width: u64| -> u128 {
            let shift_amount = 128 - int_width;
            if signed {
                i128::MAX as u128 >> shift_amount
            } else {
                u128::MAX >> shift_amount
            }
        };
        let int_min = |signed: bool, int_width: u64| -> i128 {
            if signed {
                i128::MIN >> (128 - int_width)
            } else {
                0
            }
        };

        let compute_clamp_bounds_single = |signed: bool, int_width: u64| -> (u128, u128) {
            let rounded_min =
                ieee::Single::from_i128_r(int_min(signed, int_width), Round::TowardZero);
            assert_eq!(rounded_min.status, Status::OK);
            let rounded_max =
                ieee::Single::from_u128_r(int_max(signed, int_width), Round::TowardZero);
            assert!(rounded_max.value.is_finite());
            (rounded_min.value.to_bits(), rounded_max.value.to_bits())
        };
        let compute_clamp_bounds_double = |signed: bool, int_width: u64| -> (u128, u128) {
            let rounded_min =
                ieee::Double::from_i128_r(int_min(signed, int_width), Round::TowardZero);
            assert_eq!(rounded_min.status, Status::OK);
            let rounded_max =
                ieee::Double::from_u128_r(int_max(signed, int_width), Round::TowardZero);
            assert!(rounded_max.value.is_finite());
            (rounded_min.value.to_bits(), rounded_max.value.to_bits())
        };
        // To implement saturation, we perform the following steps:
        //
        // 1. Cast x to an integer with fpto[su]i. This may result in undef.
        // 2. Compare x to f_min and f_max, and use the comparison results to select:
        //  a) int_ty::MIN if x < f_min or x is NaN
        //  b) int_ty::MAX if x > f_max
        //  c) the result of fpto[su]i otherwise
        // 3. If x is NaN, return 0.0, otherwise return the result of step 2.
        //
        // This avoids resulting undef because values in range [f_min, f_max] by definition fit into the
        // destination type. It creates an undef temporary, but *producing* undef is not UB. Our use of
        // undef does not introduce any non-determinism either.
        // More importantly, the above procedure correctly implements saturating conversion.
        // Proof (sketch):
        // If x is NaN, 0 is returned by definition.
        // Otherwise, x is finite or infinite and thus can be compared with f_min and f_max.
        // This yields three cases to consider:
        // (1) if x in [f_min, f_max], the result of fpto[su]i is returned, which agrees with
        //     saturating conversion for inputs in that range.
        // (2) if x > f_max, then x is larger than int_ty::MAX. This holds even if f_max is rounded
        //     (i.e., if f_max < int_ty::MAX) because in those cases, nextUp(f_max) is already larger
        //     than int_ty::MAX. Because x is larger than int_ty::MAX, the return value of int_ty::MAX
        //     is correct.
        // (3) if x < f_min, then x is smaller than int_ty::MIN. As shown earlier, f_min exactly equals
        //     int_ty::MIN and therefore the return value of int_ty::MIN is correct.
        // QED.

        let float_bits_to_llval = |bx: &mut Self, bits| {
            let bits_llval = match float_width {
                32 => bx.cx().const_u32(bits as u32),
                64 => bx.cx().const_u64(bits as u64),
                n => bug!("unsupported float width {}", n),
            };
            bx.bitcast(bits_llval, float_ty)
        };
        let (f_min, f_max) = match float_width {
            32 => compute_clamp_bounds_single(signed, int_width),
            64 => compute_clamp_bounds_double(signed, int_width),
            n => bug!("unsupported float width {}", n),
        };
        let f_min = float_bits_to_llval(self, f_min);
        let f_max = float_bits_to_llval(self, f_max);
        let int_max = self.cx().const_uint_big(int_ty, int_max(signed, int_width));
        let int_min = self
            .cx()
            .const_uint_big(int_ty, int_min(signed, int_width) as u128);
        let zero = self.cx().const_uint(int_ty, 0);

        // If we're working with vectors, constants must be "splatted": the constant is duplicated
        // into each lane of the vector.  The algorithm stays the same, we are just using the
        // same constant across all lanes.
        let maybe_splat = |bx: &mut Self, val| {
            if bx.cx().type_kind(dest_ty) == TypeKind::Vector {
                bx.vector_splat(bx.vector_length(dest_ty), val)
            } else {
                val
            }
        };
        let f_min = maybe_splat(self, f_min);
        let f_max = maybe_splat(self, f_max);
        let int_max = maybe_splat(self, int_max);
        let int_min = maybe_splat(self, int_min);
        let zero = maybe_splat(self, zero);

        // Step 1 ...
        let fptosui_result = if signed {
            self.fptosi(val, dest_ty)
        } else {
            self.fptoui(val, dest_ty)
        };
        let less_or_nan = self.fcmp(RealPredicate::RealULT, val, f_min);
        let greater = self.fcmp(RealPredicate::RealOGT, val, f_max);

        // Step 2: We use two comparisons and two selects, with %s1 being the
        // result:
        //     %less_or_nan = fcmp ult %x, %f_min
        //     %greater = fcmp olt %x, %f_max
        //     %s0 = select %less_or_nan, int_ty::MIN, %fptosi_result
        //     %s1 = select %greater, int_ty::MAX, %s0
        // Note that %less_or_nan uses an *unordered* comparison. This
        // comparison is true if the operands are not comparable (i.e., if x is
        // NaN). The unordered comparison ensures that s1 becomes int_ty::MIN if
        // x is NaN.
        //
        // Performance note: Unordered comparison can be lowered to a "flipped"
        // comparison and a negation, and the negation can be merged into the
        // select. Therefore, it not necessarily any more expensive than an
        // ordered ("normal") comparison. Whether these optimizations will be
        // performed is ultimately up to the backend, but at least x86 does
        // perform them.
        let s0 = self.select(less_or_nan, int_min, fptosui_result);
        let s1 = self.select(greater, int_max, s0);

        // Step 3: NaN replacement.
        // For unsigned types, the above step already yielded int_ty::MIN == 0 if x is NaN.
        // Therefore we only need to execute this step for signed integer types.
        if signed {
            // LLVM has no isNaN predicate, so we use (x == x) instead
            let cmp = self.fcmp(RealPredicate::RealOEQ, val, val);
            self.select(cmp, s1, zero)
        } else {
            s1
        }
    }

    // HACK(eddyb) helper shared by `typed_alloca` and `alloca`.
    #[instrument(level = "trace", skip(self), fields(ty = ?self.debug_type(ty)))]
    fn declare_func_local_var(
        &mut self,
        ty: <Self as BackendTypes>::Type,
        _align: Align,
    ) -> SpirvValue {
        let ptr_ty = self.type_ptr_to(ty);

        // "All OpVariable instructions in a function must be the first instructions in the first block."
        let mut builder = self.emit();
        builder.select_block(Some(0)).unwrap();
        let index = {
            let block = &builder.module_ref().functions[builder.selected_function().unwrap()]
                .blocks[builder.selected_block().unwrap()];
            block
                .instructions
                .iter()
                .enumerate()
                .find_map(|(index, inst)| {
                    if inst.class.opcode != Op::Variable {
                        Some(InsertPoint::FromBegin(index))
                    } else {
                        None
                    }
                })
                .unwrap_or(InsertPoint::End)
        };
        // TODO: rspirv doesn't have insert_variable function
        let result_id = builder.id();
        let inst = Instruction::new(
            Op::Variable,
            Some(ptr_ty),
            Some(result_id),
            vec![Operand::StorageClass(StorageClass::Function)],
        );
        builder.insert_into_block(index, inst).unwrap();
        result_id.with_type(ptr_ty)
    }
}

impl<'a, 'tcx> BuilderMethods<'a, 'tcx> for Builder<'a, 'tcx> {
    type CodegenCx = CodegenCx<'tcx>;

    #[instrument(level = "trace", skip(cx))]
    fn build(cx: &'a Self::CodegenCx, llbb: Self::BasicBlock) -> Self {
        Self {
            cx,
            current_block: llbb,
            current_span: Default::default(),
        }
    }

    fn cx(&self) -> &Self::CodegenCx {
        self.cx
    }

    fn llbb(&self) -> Self::BasicBlock {
        // FIXME(eddyb) `llbb` should be removed from `rustc_codegen_ssa::traits`.
        unreachable!("dead code within `rustc_codegen_ssa`")
    }

    fn set_span(&mut self, span: Span) {
        // HACK(eddyb) this is what `#[track_caller]` does, and we need it to be
        // able to point at e.g. a use of `panic!`, instead of its implementation,
        // but it should be more fine-grained and/or include macro backtraces in
        // debuginfo (so the decision to use them can be deferred).
        let span = span.ctxt().outer_expn().expansion_cause().unwrap_or(span);

        let old_span = self.current_span.replace(span);

        // FIXME(eddyb) enable this once cross-block interactions are figured out
        // (in particular, every block starts off with no debuginfo active).
        if false {
            // Avoid redundant debuginfo.
            if old_span == Some(span) {
                return;
            }
        }

        // HACK(eddyb) this is only to aid testing (and to not remove the old code).
        let use_custom_insts = true;

        if use_custom_insts {
            // FIXME(eddyb) this should be cached more efficiently.
            let void_ty = SpirvType::Void.def(rustc_span::DUMMY_SP, self);

            // We may not always have valid spans.
            // FIXME(eddyb) reduce the sources of this as much as possible.
            if span.is_dummy() {
                self.custom_inst(void_ty, CustomInst::ClearDebugSrcLoc);
            } else {
                let (file, line_col_range) = self.builder.file_line_col_range_for_debuginfo(span);
                let ((line_start, col_start), (line_end, col_end)) =
                    (line_col_range.start, line_col_range.end);

                self.custom_inst(
                    void_ty,
                    CustomInst::SetDebugSrcLoc {
                        file: Operand::IdRef(file.file_name_op_string_id),
                        line_start: Operand::IdRef(self.const_u32(line_start).def(self)),
                        line_end: Operand::IdRef(self.const_u32(line_end).def(self)),
                        col_start: Operand::IdRef(self.const_u32(col_start).def(self)),
                        col_end: Operand::IdRef(self.const_u32(col_end).def(self)),
                    },
                );
            }

            // HACK(eddyb) remove the previous instruction if made irrelevant.
            let mut builder = self.emit();
            if let (Some(func_idx), Some(block_idx)) =
                (builder.selected_function(), builder.selected_block())
            {
                let block = &mut builder.module_mut().functions[func_idx].blocks[block_idx];
                match &block.instructions[..] {
                    [.., a, b]
                        if a.class.opcode == b.class.opcode
                            && a.operands[..2] == b.operands[..2] =>
                    {
                        block.instructions.remove(block.instructions.len() - 2);
                    }
                    _ => {}
                }
            }
        } else {
            // We may not always have valid spans.
            // FIXME(eddyb) reduce the sources of this as much as possible.
            if span.is_dummy() {
                self.emit().no_line();
            } else {
                let (file, line_col_range) = self.builder.file_line_col_range_for_debuginfo(span);
                let (line, col) = line_col_range.start;

                self.emit().line(file.file_name_op_string_id, line, col);
            }
        }
    }

    // FIXME(eddyb) change `Self::Function` to be more like a function index.
    fn append_block(
        cx: &'a Self::CodegenCx,
        llfn: Self::Function,
        _name: &str,
    ) -> Self::BasicBlock {
        let mut builder = cx.builder.builder_for_fn(llfn);
        let id = builder.begin_block(None).unwrap();
        let index_in_builder = builder.selected_block().unwrap();
        SpirvBlockCursor {
            parent_fn: llfn,
            id,
            index_in_builder,
        }
    }

    fn append_sibling_block(&mut self, name: &str) -> Self::BasicBlock {
        Self::append_block(self.cx, self.current_block.parent_fn, name)
    }

    fn switch_to_block(&mut self, llbb: Self::BasicBlock) {
        // FIXME(eddyb) this could be more efficient by having an index in
        // `Self::BasicBlock`, not just a SPIR-V ID.
        *self = Self::build(self.cx, llbb);
    }

    fn ret_void(&mut self) {
        self.emit().ret().unwrap();
    }

    fn ret(&mut self, value: Self::Value) {
        let func_ret_ty = {
            let builder = self.emit();
            let func = &builder.module_ref().functions[builder.selected_function().unwrap()];
            func.def.as_ref().unwrap().result_type.unwrap()
        };

        // HACK(eddyb) temporary workaround for untyped pointers upstream.
        // FIXME(eddyb) replace with untyped memory SPIR-V + `qptr` or similar.
        let value = self.bitcast(value, func_ret_ty);

        self.emit().ret_value(value.def(self)).unwrap();
    }

    fn br(&mut self, dest: Self::BasicBlock) {
        self.emit().branch(dest.id).unwrap();
    }

    fn cond_br(
        &mut self,
        cond: Self::Value,
        then_llbb: Self::BasicBlock,
        else_llbb: Self::BasicBlock,
    ) {
        let cond = cond.def(self);

        // HACK(eddyb) constant-fold branches early on, as the `core` library is
        // starting to get a lot of `if cfg!(debug_assertions)` added to it.
        match self.builder.lookup_const_by_id(cond) {
            Some(SpirvConst::Scalar(1)) => self.br(then_llbb),
            Some(SpirvConst::Scalar(0)) => self.br(else_llbb),
            _ => {
                self.emit()
                    .branch_conditional(cond, then_llbb.id, else_llbb.id, empty())
                    .unwrap();
            }
        }
    }

    fn switch(
        &mut self,
        v: Self::Value,
        else_llbb: Self::BasicBlock,
        cases: impl ExactSizeIterator<Item = (u128, Self::BasicBlock)>,
    ) {
        fn construct_8(self_: &Builder<'_, '_>, signed: bool, v: u128) -> Operand {
            if v > u8::MAX as u128 {
                self_.fatal(format!(
                    "Switches to values above u8::MAX not supported: {v:?}"
                ))
            } else if signed {
                // this cast chain can probably be collapsed, but, whatever, be safe
                Operand::LiteralBit32(v as u8 as i8 as i32 as u32)
            } else {
                Operand::LiteralBit32(v as u8 as u32)
            }
        }
        fn construct_16(self_: &Builder<'_, '_>, signed: bool, v: u128) -> Operand {
            if v > u16::MAX as u128 {
                self_.fatal(format!(
                    "Switches to values above u16::MAX not supported: {v:?}"
                ))
            } else if signed {
                Operand::LiteralBit32(v as u16 as i16 as i32 as u32)
            } else {
                Operand::LiteralBit32(v as u16 as u32)
            }
        }
        fn construct_32(self_: &Builder<'_, '_>, _signed: bool, v: u128) -> Operand {
            if v > u32::MAX as u128 {
                self_.fatal(format!(
                    "Switches to values above u32::MAX not supported: {v:?}"
                ))
            } else {
                Operand::LiteralBit32(v as u32)
            }
        }
        fn construct_64(self_: &Builder<'_, '_>, _signed: bool, v: u128) -> Operand {
            if v > u64::MAX as u128 {
                self_.fatal(format!(
                    "Switches to values above u64::MAX not supported: {v:?}"
                ))
            } else {
                Operand::LiteralBit64(v as u64)
            }
        }
        // pass in signed into the closure to be able to unify closure types
        let (signed, construct_case) = match self.lookup_type(v.ty) {
            SpirvType::Integer(width, signed) => {
                let construct_case = match width {
                    8 => construct_8,
                    16 => construct_16,
                    32 => construct_32,
                    64 => construct_64,
                    other => self.fatal(format!(
                        "switch selector cannot have width {other} (only 8, 16, 32, and 64 bits allowed)"
                    )),
                };
                (signed, construct_case)
            }
            other => self.fatal(format!(
                "switch selector cannot have non-integer type {}",
                other.debug(v.ty, self)
            )),
        };
        let cases = cases
            .map(|(i, b)| (construct_case(self, signed, i), b.id))
            .collect::<Vec<_>>();
        self.emit()
            .switch(v.def(self), else_llbb.id, cases)
            .unwrap();
    }

    fn invoke(
        &mut self,
        llty: Self::Type,
        fn_attrs: Option<&CodegenFnAttrs>,
        fn_abi: Option<&FnAbi<'tcx, Ty<'tcx>>>,
        llfn: Self::Value,
        args: &[Self::Value],
        then: Self::BasicBlock,
        _catch: Self::BasicBlock,
        funclet: Option<&Self::Funclet>,
        instance: Option<ty::Instance<'tcx>>,
    ) -> Self::Value {
        // Exceptions don't exist, jump directly to then block
        let result = self.call(llty, fn_attrs, fn_abi, llfn, args, funclet, instance);
        self.emit().branch(then.id).unwrap();
        result
    }

    fn unreachable(&mut self) {
        self.emit().unreachable().unwrap();
    }

    // use wrapping_op everywhere, rustc will error out for us if the expr overflows
    simple_op! {
        add,
        int: i_add,
        fold_const {
            int(a, b) => a.checked_add(b)?;
        }
    }
    // FIXME(eddyb) try to annotate the SPIR-V for `fast` and `algebraic`.
    simple_op! {fadd, float: f_add}
    simple_op! {fadd_fast, float: f_add} // fast=normal
    simple_op! {fadd_algebraic, float: f_add} // algebraic=normal
    simple_op! {
        sub,
        int: i_sub,
        fold_const {
            int(a, b) => a.checked_sub(b)?;
        }
    }
    simple_op! {fsub, float: f_sub}
    simple_op! {fsub_fast, float: f_sub} // fast=normal
    simple_op! {fsub_algebraic, float: f_sub} // algebraic=normal
    simple_op! {
        mul,
        int: i_mul,
        fold_const {
            int(a, b) => a.checked_mul(b)?;
        }
    }
    simple_op! {fmul, float: f_mul}
    simple_op! {fmul_fast, float: f_mul} // fast=normal
    simple_op! {fmul_algebraic, float: f_mul} // algebraic=normal
    simple_op! {
        udiv,
        uint: u_div,
        fold_const {
            uint(a, b) => a.checked_div(b)?;
        }
    }
    // Note: exactudiv is UB when there's a remainder, so it's valid to implement as a normal div.
    // TODO: Can we take advantage of the UB and emit something else?
    simple_op! {
        exactudiv,
        uint: u_div,
        fold_const {
            uint(a, b) => a.checked_div(b)?;
        }
    }
    simple_op! {
        sdiv,
        sint: s_div,
        fold_const {
            sint(a, b) => a.checked_div(b)?;
        }
    }
    // Same note and TODO as exactudiv
    simple_op! {
        exactsdiv,
        int: s_div,
        fold_const {
            int(a, b) => a.checked_div(b)?;
        }
    }
    simple_op! {fdiv, float: f_div}
    simple_op! {fdiv_fast, float: f_div} // fast=normal
    simple_op! {fdiv_algebraic, float: f_div} // algebraic=normal
    simple_op! {
        urem,
        uint: u_mod,
        fold_const {
            uint(a, b) => a.checked_rem(b)?;
        }
    }
    simple_op! {
        srem,
        sint: s_rem,
        fold_const {
            sint(a, b) => a.checked_rem(b)?;
        }
    }
    simple_op! {frem, float: f_rem}
    simple_op! {frem_fast, float: f_rem} // fast=normal
    simple_op! {frem_algebraic, float: f_rem} // algebraic=normal
    simple_shift_op! {
        shl,
        int: shift_left_logical,
        fold_const {
            int(a, b) => a.checked_shl(b as u32)?;
        }
    }
    simple_shift_op! {
        lshr,
        uint: shift_right_logical,
        fold_const {
            uint(a, b) => a.checked_shr(b as u32)?;
        }
    }
    simple_shift_op! {
        ashr,
        sint: shift_right_arithmetic,
        fold_const {
            sint(a, b) => a.checked_shr(b as u32)?;
        }
    }
    simple_uni_op! {
        neg,
        int: s_negate,
        fold_const {
            int(a) => a.checked_neg()?;
        }
    }
    simple_uni_op! {fneg, float: f_negate}

    /// already unchecked by default
    fn unchecked_sadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.add(lhs, rhs)
    }
    /// already unchecked by default
    fn unchecked_uadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.add(lhs, rhs)
    }
    /// already unchecked by default
    fn unchecked_ssub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.sub(lhs, rhs)
    }
    /// already unchecked by default
    fn unchecked_usub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.sub(lhs, rhs)
    }
    /// already unchecked by default
    fn unchecked_smul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.mul(lhs, rhs)
    }
    /// already unchecked by default
    fn unchecked_umul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.mul(lhs, rhs)
    }

    simple_op! {
        and,
        int: bitwise_and,
        bool: logical_and,
        fold_const {
            int(a, b) => a.bitand(b);
            bool(a, b) => a.bitand(b);
        }
    }
    simple_op! {
        or,
        int: bitwise_or,
        bool: logical_or,
        fold_const {
            int(a, b) => a.bitor(b);
            bool(a, b) => a.bitor(b);
        }
    }
    simple_op! {
        xor,
        int: bitwise_xor,
        bool: logical_not_equal,
        fold_const {
            int(a, b) => a.bitxor(b);
            bool(a, b) => a.bitxor(b);
        }
    }
    simple_uni_op! {
        not,
        int: not,
        bool: logical_not,
        fold_const {
            int(a) => a.not();
            bool(a) => a.not();
        }
    }

    #[instrument(level = "trace", skip(self))]
    fn checked_binop(
        &mut self,
        oop: OverflowOp,
        ty: Ty<'tcx>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        let signed = match ty.kind() {
            ty::Int(_) => true,
            ty::Uint(_) => false,
            _ => self.fatal(format!(
                "unexpected {} type: {ty}",
                match oop {
                    OverflowOp::Add => "checked add",
                    OverflowOp::Sub => "checked sub",
                    OverflowOp::Mul => "checked mul",
                }
            )),
        };

        // HACK(eddyb) SPIR-V `OpIAddCarry`/`OpISubBorrow` are specifically for
        // unsigned overflow, so signed overflow still needs this custom logic.
        if signed && let OverflowOp::Add | OverflowOp::Sub = oop {
            let result = match oop {
                OverflowOp::Add => self.add(lhs, rhs),
                OverflowOp::Sub => self.sub(lhs, rhs),
                OverflowOp::Mul => unreachable!(),
            };

            // adopted partially from https://github.com/ziglang/zig/blob/master/src/codegen/spirv.zig

            // when adding, overflow could happen if
            // - rhs is positive and result < lhs; or
            // - rhs is negative and result > lhs
            // this is equivalent to (rhs < 0) == (result > lhs)
            //
            // when subtracting, overflow happens if
            // - rhs is positive and result > lhs; or
            // - rhs is negative and result < lhs
            // this is equivalent to (rhs < 0) == (result < lhs)
            let rhs_lt_zero = self.icmp(IntPredicate::IntSLT, rhs, self.constant_int(rhs.ty, 0));
            let result_gt_lhs = self.icmp(
                match oop {
                    OverflowOp::Add => IntPredicate::IntSGT,
                    OverflowOp::Sub => IntPredicate::IntSLT,
                    OverflowOp::Mul => unreachable!(),
                },
                result,
                lhs,
            );

            let overflowed = self.icmp(IntPredicate::IntEQ, rhs_lt_zero, result_gt_lhs);

            return (result, overflowed);
        }

        let result_type = self.layout_of(ty).spirv_type(self.span(), self);
        let pair_result_type = {
            let field_types = [result_type, result_type];
            let (field_offsets, size, align) =
                crate::abi::auto_struct_layout(self, &field_types, false);
            SpirvType::Adt {
                def_id: None,
                size,
                align,
                field_types: &field_types,
                field_offsets: &field_offsets,
                field_names: None,
            }
            .def(self.span(), self)
        };

        let lhs = lhs.def(self);
        let rhs = rhs.def(self);
        let pair_result = match oop {
            OverflowOp::Add => self
                .emit()
                .i_add_carry(pair_result_type, None, lhs, rhs)
                .unwrap(),
            OverflowOp::Sub => self
                .emit()
                .i_sub_borrow(pair_result_type, None, lhs, rhs)
                .unwrap(),
            OverflowOp::Mul => {
                if signed {
                    self.emit()
                        .s_mul_extended(pair_result_type, None, lhs, rhs)
                        .unwrap()
                } else {
                    self.emit()
                        .u_mul_extended(pair_result_type, None, lhs, rhs)
                        .unwrap()
                }
            }
        }
        .with_type(pair_result_type);
        let result_lo = self.extract_value(pair_result, 0);
        let result_hi = self.extract_value(pair_result, 1);

        // HACK(eddyb) SPIR-V lacks any `(T, T) -> (T, bool)` instructions,
        // so instead `result_hi` is compared with the value expected in the
        // non-overflow case (`0`, or `-1` for negative signed multiply result).
        let expected_nonoverflowing_hi = match (oop, signed) {
            (OverflowOp::Add | OverflowOp::Sub, _) | (OverflowOp::Mul, false) => {
                self.const_uint(result_type, 0)
            }
            (OverflowOp::Mul, true) => {
                // HACK(eddyb) `(x: iN) >> (N - 1)` will spread the sign bit
                // across all `N` bits of `iN`, and should be equivalent to
                // `if x < 0 { -1 } else { 0 }`, without needing compare+select).
                let result_width = u32::try_from(self.int_width(result_type)).unwrap();
                self.ashr(result_lo, self.const_u32(result_width - 1))
            }
        };
        let overflowed = self.icmp(IntPredicate::IntNE, result_hi, expected_nonoverflowing_hi);

        (result_lo, overflowed)
    }

    // rustc has the concept of an immediate vs. memory type - bools are compiled to LLVM bools as
    // immediates, but if they're behind a pointer, they're compiled to u8. The reason for this is
    // because LLVM is bad at bools behind pointers (something something u1 bitmasking on load).
    //
    // SPIR-V allows bools behind *some* pointers, and disallows others - specifically, it allows
    // bools behind the storage classes Workgroup, CrossWorkgroup, Private, Function, Input, and
    // Output. In other words, "For stuff the CPU can't see, bools are OK. For stuff the CPU *can*
    // see, no bools allowed". So, we always compile rust bools to SPIR-V bools instead of u8 as
    // rustc does, even if they're behind a pointer, and error if bools are in an interface (the
    // user should choose u8, u32, or something else instead). That means that immediate types and
    // memory types are the same, and no conversion needs to happen here.
    fn from_immediate(&mut self, val: Self::Value) -> Self::Value {
        val
    }

    fn to_immediate_scalar(&mut self, val: Self::Value, _scalar: Scalar) -> Self::Value {
        val
    }

    // HACK(eddyb) new method patched into `pqp_cg_ssa` (see `build.rs`).
    #[cfg(not(rustc_codegen_spirv_disable_pqp_cg_ssa))]
    fn typed_alloca(&mut self, ty: Self::Type, align: Align) -> Self::Value {
        self.declare_func_local_var(ty, align)
    }
    fn alloca(&mut self, size: Size, align: Align) -> Self::Value {
        self.declare_func_local_var(self.type_array(self.type_i8(), size.bytes()), align)
    }

    fn dynamic_alloca(&mut self, _len: Self::Value, _align: Align) -> Self::Value {
        self.fatal("dynamic alloca not supported yet")
    }

    fn load(&mut self, ty: Self::Type, ptr: Self::Value, _align: Align) -> Self::Value {
        // HACK(eddyb) prefer casting the pointer, if it's a constant, because
        // its success bypasses runtime `OpBitcast`s from potentially illegal
        // constants (sadly, nothing else might try to cast the pointer itself).
        if self.builder.lookup_const(ptr).is_some()
            && let Some(loaded_val) = self
                .const_bitcast(ptr, self.type_ptr_to(ty))
                .const_fold_load(self)
            && loaded_val.ty == ty
        {
            return loaded_val;
        }

        let (ptr, access_ty) = self.adjust_pointer_for_typed_access(ptr, ty);
        let loaded_val = ptr.const_fold_load(self).unwrap_or_else(|| {
            self.emit()
                .load(access_ty, None, ptr.def(self), None, empty())
                .unwrap()
                .with_type(access_ty)
        });
        self.bitcast(loaded_val, ty)
    }

    fn volatile_load(&mut self, ty: Self::Type, ptr: Self::Value) -> Self::Value {
        // TODO: Implement this
        let result = self.load(ty, ptr, Align::from_bytes(0).unwrap());
        self.zombie(result.def(self), "volatile load is not supported yet");
        result
    }

    fn atomic_load(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        order: AtomicOrdering,
        _size: Size,
    ) -> Self::Value {
        // HACK(eddyb) SPIR-V lacks pointer atomics, have to use integers instead.
        let atomic_ty = match self.lookup_type(ty) {
            SpirvType::Pointer { .. } => self.type_usize(),
            _ => ty,
        };
        let (ptr, access_ty) = self.adjust_pointer_for_typed_access(ptr, atomic_ty);

        // TODO: Default to device scope
        let memory = self.constant_u32(self.span(), Scope::Device as u32);
        let semantics = self.ordering_to_semantics_def(order);
        let result = self
            .emit()
            .atomic_load(
                access_ty,
                None,
                ptr.def(self),
                memory.def(self),
                semantics.def(self),
            )
            .unwrap()
            .with_type(access_ty);
        self.validate_atomic(access_ty, result.def(self));
        self.bitcast(result, ty)
    }

    fn load_operand(
        &mut self,
        place: PlaceRef<'tcx, Self::Value>,
    ) -> OperandRef<'tcx, Self::Value> {
        if place.layout.is_zst() {
            return OperandRef::zero_sized(place.layout);
        }

        let val = if place.val.llextra.is_some() {
            OperandValue::Ref(place.val)
        } else if self.cx.is_backend_immediate(place.layout) {
            let llval = self.load(
                place.layout.spirv_type(self.span(), self),
                place.val.llval,
                place.val.align,
            );
            OperandValue::Immediate(llval)
        } else if let BackendRepr::ScalarPair(a, b) = place.layout.backend_repr {
            let b_offset = a
                .primitive()
                .size(self)
                .align_to(b.primitive().align(self).abi);

            let mut load = |i, scalar: Scalar, align| {
                let llptr = if i == 0 {
                    place.val.llval
                } else {
                    self.inbounds_ptradd(place.val.llval, self.const_usize(b_offset.bytes()))
                };
                let load = self.load(
                    self.scalar_pair_element_backend_type(place.layout, i, false),
                    llptr,
                    align,
                );
                self.to_immediate_scalar(load, scalar)
            };

            OperandValue::Pair(
                load(0, a, place.val.align),
                load(1, b, place.val.align.restrict_for_offset(b_offset)),
            )
        } else {
            OperandValue::Ref(place.val)
        };
        OperandRef {
            val,
            layout: place.layout,
        }
    }

    /// Called for `Rvalue::Repeat` when the elem is neither a ZST nor optimizable using memset.
    fn write_operand_repeatedly(
        &mut self,
        cg_elem: OperandRef<'tcx, Self::Value>,
        count: u64,
        dest: PlaceRef<'tcx, Self::Value>,
    ) {
        let zero = self.const_usize(0);
        let start = dest.project_index(self, zero).val.llval;

        let elem_layout = dest.layout.field(self.cx(), 0);
        let elem_ty = elem_layout.spirv_type(self.span(), self);
        let align = dest.val.align.restrict_for_offset(elem_layout.size);

        for i in 0..count {
            let current = self.inbounds_gep(elem_ty, start, &[self.const_usize(i)]);
            cg_elem.val.store(
                self,
                PlaceRef::new_sized_aligned(current, cg_elem.layout, align),
            );
        }
    }

    // FIXME(eddyb) `assume` is not implemented atm, so all of its forms should
    // avoid computing its (potentially illegal) bool input in the first place.
    fn assume_integer_range(&mut self, _imm: Self::Value, _ty: Self::Type, _range: WrappingRange) {}
    fn assume_nonnull(&mut self, _val: Self::Value) {}

    fn range_metadata(&mut self, _load: Self::Value, _range: WrappingRange) {
        // ignore
    }

    fn nonnull_metadata(&mut self, _load: Self::Value) {
        // ignore
    }

    fn store(&mut self, val: Self::Value, ptr: Self::Value, _align: Align) -> Self::Value {
        let (ptr, access_ty) = self.adjust_pointer_for_typed_access(ptr, val.ty);
        let val = self.bitcast(val, access_ty);

        self.emit()
            .store(ptr.def(self), val.def(self), None, empty())
            .unwrap();
        // FIXME(eddyb) this is meant to be a handle the store instruction itself.
        val
    }

    fn store_with_flags(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        align: Align,
        flags: MemFlags,
    ) -> Self::Value {
        if flags != MemFlags::empty() {
            self.err(format!("store_with_flags is not supported yet: {flags:?}"));
        }
        self.store(val, ptr, align)
    }

    fn atomic_store(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        order: AtomicOrdering,
        _size: Size,
    ) {
        // HACK(eddyb) SPIR-V lacks pointer atomics, have to use integers instead.
        let atomic_ty = match self.lookup_type(val.ty) {
            SpirvType::Pointer { .. } => self.type_usize(),
            _ => val.ty,
        };
        let (ptr, access_ty) = self.adjust_pointer_for_typed_access(ptr, atomic_ty);
        let val = self.bitcast(val, access_ty);

        // TODO: Default to device scope
        let memory = self.constant_u32(self.span(), Scope::Device as u32);
        let semantics = self.ordering_to_semantics_def(order);
        self.validate_atomic(val.ty, ptr.def(self));
        self.emit()
            .atomic_store(
                ptr.def(self),
                memory.def(self),
                semantics.def(self),
                val.def(self),
            )
            .unwrap();
    }

    fn gep(&mut self, ty: Self::Type, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        self.maybe_inbounds_gep(ty, ptr, indices, false)
    }

    fn inbounds_gep(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        indices: &[Self::Value],
    ) -> Self::Value {
        // HACK(eddyb) effectively a backport of this `gep [0, i]` -> `gep [i]`
        // PR: https://github.com/rust-lang/rust/pull/134117 to even earlier
        // nightlies - and that PR happens to remove the last GEP that can be
        // emitted with any "structured" (struct/array) indices, beyond the
        // "first index" (which acts as `<*T>::offset` aka "pointer arithmetic").
        if let &[ptr_base_index, structured_index] = indices
            && self.builder.lookup_const_scalar(ptr_base_index) == Some(0)
            && let SpirvType::Array { element, .. } | SpirvType::RuntimeArray { element, .. } =
                self.lookup_type(ty)
        {
            return self.maybe_inbounds_gep(element, ptr, &[structured_index], true);
        }

        self.maybe_inbounds_gep(ty, ptr, indices, true)
    }

    // intcast has the logic for dealing with bools, so use that
    fn trunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }
    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, true)
    }
    fn fptoui_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.fptoint_sat(false, val, dest_ty)
    }

    fn fptosi_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.fptoint_sat(true, val, dest_ty)
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_f_to_u(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_f_to_s(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_u_to_f(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .convert_s_to_f(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            self.emit()
                .f_convert(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            // If casting a constant, directly create a constant of the target type.
            // This avoids creating intermediate types that might require additional
            // capabilities. For example, casting a f16 constant to f32 will directly
            // create a f32 constant, avoiding the need for Float16 capability if it is
            // not used elsewhere.
            if let Some(const_val) = self.builder.lookup_const_scalar(val)
                && let (SpirvType::Float(src_width), SpirvType::Float(dst_width)) =
                    (self.lookup_type(val.ty), self.lookup_type(dest_ty))
                && src_width < dst_width
            {
                // Convert the bit representation to the actual float value
                let float_val = match src_width {
                    32 => Some(f32::from_bits(const_val as u32) as f64),
                    64 => Some(f64::from_bits(const_val as u64)),
                    _ => None,
                };

                if let Some(val) = float_val {
                    return self.constant_float(dest_ty, val);
                }
            }

            // Regular conversion
            self.emit()
                .f_convert(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty)
        }
    }

    fn ptrtoint(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Pointer { .. } => (),
            other => self.fatal(format!(
                "ptrtoint called on non-pointer source type: {other:?}"
            )),
        }
        if val.ty == dest_ty {
            val
        } else {
            let result = self
                .emit()
                .convert_ptr_to_u(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty);
            self.zombie_convert_ptr_to_u(result.def(self));
            result
        }
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        match self.lookup_type(dest_ty) {
            SpirvType::Pointer { .. } => (),
            other => self.fatal(format!(
                "inttoptr called on non-pointer dest type: {other:?}"
            )),
        }
        if val.ty == dest_ty {
            val
        } else {
            let result = self
                .emit()
                .convert_u_to_ptr(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty);
            self.zombie_convert_u_to_ptr(result.def(self));
            result
        }
    }

    #[instrument(level = "trace", skip(self), fields(val_type = ?self.debug_type(val.ty), dest_ty = ?self.debug_type(dest_ty)))]
    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if val.ty == dest_ty {
            val
        } else {
            let val_ty_kind = self.lookup_type(val.ty);
            let dest_ty_kind = self.lookup_type(dest_ty);

            // HACK(eddyb) account for bitcasts from/to aggregates not being legal
            // in SPIR-V, but still being used to paper over untyped pointers,
            // by unpacking/repacking newtype-shaped aggregates as-needed.
            let unpack_newtype = |ty, kind| {
                let span = span!(Level::DEBUG, "unpack_newtype");
                let _guard = span.enter();

                if !matches!(kind, SpirvType::Adt { .. } | SpirvType::Array { .. }) {
                    return None;
                }
                let size = kind.sizeof(self)?;
                let mut leaf_ty = ty;

                // FIXME(eddyb) this isn't efficient, `recover_access_chain_from_offset`
                // could instead be doing all the extra digging itself.
                let mut indices = SmallVec::<[_; 8]>::new();
                while let Some((inner_indices, inner_ty)) = self.recover_access_chain_from_offset(
                    leaf_ty,
                    Size::ZERO,
                    Some(size)..=Some(size),
                    None,
                ) {
                    indices.extend(inner_indices);
                    leaf_ty = inner_ty;
                }

                (!indices.is_empty()).then_some((indices, leaf_ty))
            };
            // Unpack input newtypes, and bitcast the leaf inside, instead.
            if let Some((indices, in_leaf_ty)) = unpack_newtype(val.ty, val_ty_kind) {
                let in_leaf = self
                    .emit()
                    .composite_extract(in_leaf_ty, None, val.def(self), indices)
                    .unwrap()
                    .with_type(in_leaf_ty);

                trace!(
                    "unpacked newtype. val: {} -> in_leaf_ty: {}",
                    self.debug_type(val.ty),
                    self.debug_type(in_leaf_ty),
                );
                return self.bitcast(in_leaf, dest_ty);
            }

            // Repack output newtypes, after bitcasting the leaf inside, instead.
            if let Some((indices, out_leaf_ty)) = unpack_newtype(dest_ty, dest_ty_kind) {
                trace!(
                    "unpacked newtype: dest: {} -> out_leaf_ty: {}",
                    self.debug_type(dest_ty),
                    self.debug_type(out_leaf_ty),
                );
                let out_leaf = self.bitcast(val, out_leaf_ty);
                let out_agg_undef = self.undef(dest_ty);
                trace!("returning composite insert");
                return self
                    .emit()
                    .composite_insert(
                        dest_ty,
                        None,
                        out_leaf.def(self),
                        out_agg_undef.def(self),
                        indices,
                    )
                    .unwrap()
                    .with_type(dest_ty);
            }

            let val_is_ptr = matches!(val_ty_kind, SpirvType::Pointer { .. });
            let dest_is_ptr = matches!(dest_ty_kind, SpirvType::Pointer { .. });

            // Reuse the pointer-specific logic in `pointercast` for `*T -> *U`.
            if val_is_ptr && dest_is_ptr {
                trace!("val and dest are both pointers");
                return self.pointercast(val, dest_ty);
            }

            trace!(
                "before emitting: val ty: {} -> dest ty: {}",
                self.debug_type(val.ty),
                self.debug_type(dest_ty)
            );

            let result = self
                .emit()
                .bitcast(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty);

            if val_is_ptr || dest_is_ptr {
                self.zombie(
                    result.def(self),
                    &format!(
                        "cannot cast between pointer and non-pointer types\
                         \nfrom `{}`\
                         \n  to `{}`",
                        self.debug_type(val.ty),
                        self.debug_type(dest_ty)
                    ),
                );
            }

            result
        }
    }

    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
        if val.ty == dest_ty {
            // I guess?
            return val;
        }

        // If casting a constant, directly create a constant of the target type. This
        // avoids creating intermediate types that might require additional
        // capabilities. For example, casting a u8 constant to u32 will directly create
        // a u32 constant, avoiding the need for Int8 capability if it is not used
        // elsewhere.
        if let Some(const_val) = self.builder.lookup_const_scalar(val) {
            let src_ty = self.lookup_type(val.ty);
            let dst_ty_spv = self.lookup_type(dest_ty);

            // Try to optimize the constant cast
            let optimized_result = match (src_ty, dst_ty_spv) {
                // Integer to integer cast
                (SpirvType::Integer(src_width, _), SpirvType::Integer(dst_width, _)) => {
                    // Only optimize if we're widening. This avoids creating the source
                    // type when it's safe to do so. For narrowing casts (e.g., u32 as
                    // u8), we need the proper truncation behavior that the regular cast
                    // provides.
                    if src_width < dst_width {
                        Some(self.constant_int(dest_ty, const_val))
                    } else {
                        None
                    }
                }
                // Bool to integer cast - const_val will be 0 or 1
                (SpirvType::Bool, SpirvType::Integer(_, _)) => {
                    Some(self.constant_int(dest_ty, const_val))
                }
                // Integer to bool cast - compare with zero
                (SpirvType::Integer(_, _), SpirvType::Bool) => {
                    Some(self.constant_bool(self.span(), const_val != 0))
                }
                _ => None,
            };

            if let Some(result) = optimized_result {
                return result;
            }
        }

        match (self.lookup_type(val.ty), self.lookup_type(dest_ty)) {
            // sign change
            (
                SpirvType::Integer(val_width, val_signedness),
                SpirvType::Integer(dest_width, dest_signedness),
            ) if val_width == dest_width && val_signedness != dest_signedness => self
                .emit()
                .bitcast(dest_ty, None, val.def(self))
                .unwrap()
                .with_type(dest_ty),
            // width change, and optional sign change
            (SpirvType::Integer(_, _), SpirvType::Integer(_, dest_signedness)) => {
                // spir-v spec doesn't seem to say that signedness needs to match the operands, only that the signedness
                // of the destination type must match the instruction's signedness.
                if dest_signedness {
                    self.emit().s_convert(dest_ty, None, val.def(self))
                } else {
                    self.emit().u_convert(dest_ty, None, val.def(self))
                }
                .unwrap()
                .with_type(dest_ty)
            }
            // bools are ints in llvm, so we have to implement this here
            (SpirvType::Bool, SpirvType::Integer(_, _)) => {
                // spir-v doesn't have a direct conversion instruction
                let if_true = self.constant_int(dest_ty, 1);
                let if_false = self.constant_int(dest_ty, 0);
                self.emit()
                    .select(
                        dest_ty,
                        None,
                        val.def(self),
                        if_true.def(self),
                        if_false.def(self),
                    )
                    .unwrap()
                    .with_type(dest_ty)
            }
            (SpirvType::Integer(_, _), SpirvType::Bool) => {
                // spir-v doesn't have a direct conversion instruction, glslang emits OpINotEqual
                let zero = self.constant_int(val.ty, 0);
                self.emit()
                    .i_not_equal(dest_ty, None, val.def(self), zero.def(self))
                    .unwrap()
                    .with_type(dest_ty)
            }
            (val_ty, dest_ty_spv) => self.fatal(format!(
                "TODO: intcast not implemented yet: val={val:?} val.ty={val_ty:?} dest_ty={dest_ty_spv:?} is_signed={is_signed}"
            )),
        }
    }

    #[instrument(level = "trace", skip(self), fields(ptr, ptr_ty = ?self.debug_type(ptr.ty), dest_ty = ?self.debug_type(dest_ty)))]
    fn pointercast(&mut self, ptr: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if ptr.ty == dest_ty {
            trace!("ptr.ty == dest_ty");
            return ptr;
        }

        // Strip a previous `pointercast`, to reveal the original pointer type.
        let ptr = ptr.strip_ptrcasts();

        trace!(
            "ptr type after stripping pointer casts: {}",
            self.debug_type(ptr.ty),
        );

        if ptr.ty == dest_ty {
            return ptr;
        }

        // HACK(eddyb) this is basically a `try` block.
        let try_recover_access_chain = || {
            let ptr_pointee = match self.lookup_type(ptr.ty) {
                SpirvType::Pointer { pointee, .. } => pointee?,
                other => self.fatal(format!(
                    "pointercast called on non-pointer source type: {other:?}"
                )),
            };
            let dest_pointee = match self.lookup_type(dest_ty) {
                SpirvType::Pointer { pointee, .. } => pointee?,
                other => self.fatal(format!(
                    "pointercast called on non-pointer dest type: {other:?}"
                )),
            };
            let dest_pointee_size = self.lookup_type(dest_pointee).sizeof(self);
            self.recover_access_chain_from_offset(
                ptr_pointee,
                Size::ZERO,
                dest_pointee_size..=dest_pointee_size,
                Some(dest_pointee),
            )
        };

        if let Some((indices, _)) = try_recover_access_chain() {
            trace!("`recover_access_chain_from_offset` returned something");
            let indices = indices
                .into_iter()
                .map(|idx| self.constant_u32(self.span(), idx).def(self))
                .collect::<Vec<_>>();
            self.emit()
                .in_bounds_access_chain(dest_ty, None, ptr.def(self), indices)
                .unwrap()
                .with_type(dest_ty)
        } else {
            trace!("`recover_access_chain_from_offset` returned `None`");

            // Prefer constant(-folding) pointer casts, over runtime casts,
            // once all recovery options have been exhasuted (see above).
            // HACK(eddyb) this also reuses the special-casing in `const_bitcast`,
            // which relies on combining a pointee type with untyped const data.
            if self.builder.lookup_const(ptr).is_some() {
                trace!("const_bitcast");
                return self.const_bitcast(ptr, dest_ty);
            }

            // Defer the cast so that it has a chance to be avoided.
            let ptr_id = ptr.def(self);
            let bitcast_result_id = self.emit().bitcast(dest_ty, None, ptr_id).unwrap();

            self.zombie(
                bitcast_result_id,
                &format!(
                    "cannot cast between pointer types\
                         \nfrom `{}`\
                         \n  to `{}`",
                    self.debug_type(ptr.ty),
                    self.debug_type(dest_ty)
                ),
            );

            SpirvValue {
                zombie_waiting_for_span: false,
                kind: SpirvValueKind::Def {
                    id: bitcast_result_id,
                    original_ptr_before_casts: Some(SpirvValue {
                        zombie_waiting_for_span: ptr.zombie_waiting_for_span,
                        kind: ptr_id,
                        ty: ptr.ty,
                    }),
                },
                ty: dest_ty,
            }
        }
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, mut rhs: Self::Value) -> Self::Value {
        // Note: the signedness of the opcode doesn't have to match the signedness of the operands.
        use IntPredicate::*;

        if lhs.ty != rhs.ty
            && [lhs, rhs].map(|v| matches!(self.lookup_type(v.ty), SpirvType::Pointer { .. }))
                == [true, true]
        {
            // HACK(eddyb) temporary workaround for untyped pointers upstream.
            // FIXME(eddyb) replace with untyped memory SPIR-V + `qptr` or similar.
            rhs = self.pointercast(rhs, lhs.ty);
        }

        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self.span(), self);

        if let Some(const_lhs) = self.try_get_const_value(lhs)
            && let Some(const_rhs) = self.try_get_const_value(rhs)
        {
            let const_result = match self.lookup_type(lhs.ty) {
                SpirvType::Integer(_, _) => match (const_lhs, const_rhs, op) {
                    (ConstValue::Unsigned(lhs), ConstValue::Unsigned(rhs), IntEQ) => {
                        Some(lhs.eq(&rhs))
                    }
                    (ConstValue::Signed(lhs), ConstValue::Signed(rhs), IntEQ) => Some(lhs.eq(&rhs)),
                    (ConstValue::Unsigned(lhs), ConstValue::Unsigned(rhs), IntNE) => {
                        Some(lhs.ne(&rhs))
                    }
                    (ConstValue::Signed(lhs), ConstValue::Signed(rhs), IntNE) => Some(lhs.ne(&rhs)),
                    (ConstValue::Unsigned(lhs), ConstValue::Unsigned(rhs), IntUGT) => {
                        Some(lhs.gt(&rhs))
                    }
                    (ConstValue::Unsigned(lhs), ConstValue::Unsigned(rhs), IntUGE) => {
                        Some(lhs.ge(&rhs))
                    }
                    (ConstValue::Unsigned(lhs), ConstValue::Unsigned(rhs), IntULT) => {
                        Some(lhs.lt(&rhs))
                    }
                    (ConstValue::Unsigned(lhs), ConstValue::Unsigned(rhs), IntULE) => {
                        Some(lhs.le(&rhs))
                    }
                    (ConstValue::Signed(lhs), ConstValue::Signed(rhs), IntUGT) => {
                        Some(lhs.gt(&rhs))
                    }
                    (ConstValue::Signed(lhs), ConstValue::Signed(rhs), IntUGE) => {
                        Some(lhs.ge(&rhs))
                    }
                    (ConstValue::Signed(lhs), ConstValue::Signed(rhs), IntULT) => {
                        Some(lhs.lt(&rhs))
                    }
                    (ConstValue::Signed(lhs), ConstValue::Signed(rhs), IntULE) => {
                        Some(lhs.le(&rhs))
                    }
                    (_, _, _) => None,
                },
                SpirvType::Bool => match (const_lhs, const_rhs, op) {
                    (ConstValue::Bool(lhs), ConstValue::Bool(rhs), IntEQ) => Some(lhs.eq(&rhs)),
                    (ConstValue::Bool(lhs), ConstValue::Bool(rhs), IntNE) => Some(lhs.ne(&rhs)),
                    (ConstValue::Bool(lhs), ConstValue::Bool(rhs), IntUGT) => Some(lhs.gt(&rhs)),
                    (ConstValue::Bool(lhs), ConstValue::Bool(rhs), IntUGE) => Some(lhs.ge(&rhs)),
                    (ConstValue::Bool(lhs), ConstValue::Bool(rhs), IntULT) => Some(lhs.lt(&rhs)),
                    (ConstValue::Bool(lhs), ConstValue::Bool(rhs), IntULE) => Some(lhs.le(&rhs)),
                    (_, _, _) => None,
                },
                _ => None,
            };
            if let Some(result) = const_result {
                return self.const_bool(result);
            }
        }

        match self.lookup_type(lhs.ty) {
            SpirvType::Integer(_, _) => match op {
                IntEQ => self.emit().i_equal(b, None, lhs.def(self), rhs.def(self)),
                IntNE => self
                    .emit()
                    .i_not_equal(b, None, lhs.def(self), rhs.def(self)),
                IntUGT => self
                    .emit()
                    .u_greater_than(b, None, lhs.def(self), rhs.def(self)),
                IntUGE => self
                    .emit()
                    .u_greater_than_equal(b, None, lhs.def(self), rhs.def(self)),
                IntULT => self
                    .emit()
                    .u_less_than(b, None, lhs.def(self), rhs.def(self)),
                IntULE => self
                    .emit()
                    .u_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
                IntSGT => self
                    .emit()
                    .s_greater_than(b, None, lhs.def(self), rhs.def(self)),
                IntSGE => self
                    .emit()
                    .s_greater_than_equal(b, None, lhs.def(self), rhs.def(self)),
                IntSLT => self
                    .emit()
                    .s_less_than(b, None, lhs.def(self), rhs.def(self)),
                IntSLE => self
                    .emit()
                    .s_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
            },
            SpirvType::Pointer { .. } => match op {
                // FIXME(eddyb) consider always emitting `OpPtrEqual` w/ `qptr`.
                IntEQ => {
                    if self.emit().version().unwrap() > (1, 3) {
                        self.emit()
                            .ptr_equal(b, None, lhs.def(self), rhs.def(self))
                            .inspect(|&result| {
                                self.zombie_ptr_equal(result, "OpPtrEqual");
                            })
                    } else {
                        let int_ty = self.type_usize();
                        let lhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, lhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(lhs);
                        let rhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, rhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(rhs);
                        self.emit().i_equal(b, None, lhs, rhs)
                    }
                }
                // FIXME(eddyb) consider always emitting `OpPtrNotEqual` w/ `qptr`.
                IntNE => {
                    if self.emit().version().unwrap() > (1, 3) {
                        self.emit()
                            .ptr_not_equal(b, None, lhs.def(self), rhs.def(self))
                            .inspect(|&result| {
                                self.zombie_ptr_equal(result, "OpPtrNotEqual");
                            })
                    } else {
                        let int_ty = self.type_usize();
                        let lhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, lhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(lhs);
                        let rhs = self
                            .emit()
                            .convert_ptr_to_u(int_ty, None, rhs.def(self))
                            .unwrap();
                        self.zombie_convert_ptr_to_u(rhs);
                        self.emit().i_not_equal(b, None, lhs, rhs)
                    }
                }
                IntUGT => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_greater_than(b, None, lhs, rhs)
                }
                IntUGE => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_greater_than_equal(b, None, lhs, rhs)
                }
                IntULT => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_less_than(b, None, lhs, rhs)
                }
                IntULE => {
                    let int_ty = self.type_usize();
                    let lhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, lhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(lhs);
                    let rhs = self
                        .emit()
                        .convert_ptr_to_u(int_ty, None, rhs.def(self))
                        .unwrap();
                    self.zombie_convert_ptr_to_u(rhs);
                    self.emit().u_less_than_equal(b, None, lhs, rhs)
                }
                IntSGT => self.fatal("TODO: pointer operator IntSGT not implemented yet"),
                IntSGE => self.fatal("TODO: pointer operator IntSGE not implemented yet"),
                IntSLT => self.fatal("TODO: pointer operator IntSLT not implemented yet"),
                IntSLE => self.fatal("TODO: pointer operator IntSLE not implemented yet"),
            },
            SpirvType::Bool => match op {
                IntEQ => self
                    .emit()
                    .logical_equal(b, None, lhs.def(self), rhs.def(self)),
                IntNE => self
                    .emit()
                    .logical_not_equal(b, None, lhs.def(self), rhs.def(self)),
                // x > y  =>  x && !y
                IntUGT => {
                    // intel-compute-runtime doesn't like OpLogicalNot
                    let true_ = self.constant_bool(self.span(), true);
                    let rhs = self
                        .emit()
                        .logical_not_equal(b, None, rhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_and(b, None, lhs.def(self), rhs)
                }
                // x >= y  =>  x || !y
                IntUGE => {
                    let true_ = self.constant_bool(self.span(), true);
                    let rhs = self
                        .emit()
                        .logical_not_equal(b, None, rhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_or(b, None, lhs.def(self), rhs)
                }
                // x < y  =>  !x && y
                IntULT => {
                    // intel-compute-runtime doesn't like OpLogicalNot
                    let true_ = self.constant_bool(self.span(), true);
                    let lhs = self
                        .emit()
                        .logical_not_equal(b, None, lhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_and(b, None, lhs, rhs.def(self))
                }
                // x <= y  =>  !x || y
                IntULE => {
                    // intel-compute-runtime doesn't like OpLogicalNot
                    let true_ = self.constant_bool(self.span(), true);
                    let lhs = self
                        .emit()
                        .logical_not_equal(b, None, lhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_or(b, None, lhs, rhs.def(self))
                }
                IntSGT => self.fatal("TODO: boolean operator IntSGT not implemented yet"),
                IntSGE => self.fatal("TODO: boolean operator IntSGE not implemented yet"),
                IntSLT => self.fatal("TODO: boolean operator IntSLT not implemented yet"),
                IntSLE => self.fatal("TODO: boolean operator IntSLE not implemented yet"),
            },
            other => self.fatal(format!(
                "Int comparison not implemented on {}",
                other.debug(lhs.ty, self)
            )),
        }
        .unwrap()
        .with_type(b)
    }

    fn fcmp(&mut self, op: RealPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        use RealPredicate::*;
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self.span(), self);
        match op {
            RealPredicateFalse => return self.cx.constant_bool(self.span(), false),
            RealPredicateTrue => return self.cx.constant_bool(self.span(), true),
            RealOEQ => self
                .emit()
                .f_ord_equal(b, None, lhs.def(self), rhs.def(self)),
            RealOGT => self
                .emit()
                .f_ord_greater_than(b, None, lhs.def(self), rhs.def(self)),
            RealOGE => self
                .emit()
                .f_ord_greater_than_equal(b, None, lhs.def(self), rhs.def(self)),
            RealOLT => self
                .emit()
                .f_ord_less_than(b, None, lhs.def(self), rhs.def(self)),
            RealOLE => self
                .emit()
                .f_ord_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
            RealONE => self
                .emit()
                .f_ord_not_equal(b, None, lhs.def(self), rhs.def(self)),
            RealORD => self.emit().ordered(b, None, lhs.def(self), rhs.def(self)),
            RealUNO => self.emit().unordered(b, None, lhs.def(self), rhs.def(self)),
            RealUEQ => self
                .emit()
                .f_unord_equal(b, None, lhs.def(self), rhs.def(self)),
            RealUGT => self
                .emit()
                .f_unord_greater_than(b, None, lhs.def(self), rhs.def(self)),
            RealUGE => {
                self.emit()
                    .f_unord_greater_than_equal(b, None, lhs.def(self), rhs.def(self))
            }
            RealULT => self
                .emit()
                .f_unord_less_than(b, None, lhs.def(self), rhs.def(self)),
            RealULE => self
                .emit()
                .f_unord_less_than_equal(b, None, lhs.def(self), rhs.def(self)),
            RealUNE => self
                .emit()
                .f_unord_not_equal(b, None, lhs.def(self), rhs.def(self)),
        }
        .unwrap()
        .with_type(b)
    }

    #[instrument(level = "trace", skip(self))]
    fn memcpy(
        &mut self,
        dst: Self::Value,
        _dst_align: Align,
        src: Self::Value,
        _src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        if flags != MemFlags::empty() {
            self.err(format!(
                "memcpy with mem flags is not supported yet: {flags:?}"
            ));
        }
        let const_size = self
            .builder
            .lookup_const_scalar(size)
            .and_then(|size| Some(Size::from_bytes(u64::try_from(size).ok()?)));
        if const_size == Some(Size::ZERO) {
            // Nothing to do!
            return;
        }

        let typed_copy_dst_src = const_size.and_then(|const_size| {
            trace!(
                "adjusting pointers: src: {} -> dst: {}",
                self.debug_type(src.ty),
                self.debug_type(dst.ty),
            );
            let dst_adj = self.adjust_pointer_for_sized_access(dst, const_size);
            let src_adj = self.adjust_pointer_for_sized_access(src, const_size);
            match (dst_adj, src_adj) {
                (Some((dst, dst_access_ty)), Some((src, src_access_ty)))
                    if dst_access_ty == src_access_ty =>
                {
                    trace!("BOTH adjusted memcpy");
                    Some((dst, src))
                }

                // HACK(eddyb) fill in missing `dst`/`src` with the other side
                // (including `src` having the wrong type but being constant,
                // which can still reach the special-casing in `const_bitcast`).
                (Some((dst, access_ty)), _)
                    if src_adj.is_none() || self.builder.lookup_const(src).is_some() =>
                {
                    trace!(
                        "DESTINATION adjusted memcpy calling pointercast: \
                         dst ty: {}, access ty: {}",
                        self.debug_type(dst.ty),
                        self.debug_type(access_ty)
                    );
                    Some((dst, self.pointercast(src, self.type_ptr_to(access_ty))))
                }
                (None, Some((src, access_ty))) => {
                    trace!(
                        "SOURCE adjusted memcpy calling pointercast: \
                         dst ty: {} -> access ty: {}, src ty: {}",
                        self.debug_type(dst.ty),
                        self.debug_type(access_ty),
                        self.debug_type(src.ty)
                    );
                    Some((self.pointercast(dst, self.type_ptr_to(access_ty)), src))
                }

                // HACK(eddyb) unreachable, but only via guard expression above.
                (Some(_), None) => unreachable!(),

                // HACK(eddyb) favor the source when they disagree.
                (Some((dst, _)), Some((src, src_access_ty))) => {
                    Some((self.pointercast(dst, self.type_ptr_to(src_access_ty)), src))
                }

                (None, None) => None,
            }
        });

        if let Some((dst, src)) = typed_copy_dst_src {
            if let Some(const_value) = src.const_fold_load(self) {
                trace!("storing const value");
                self.store(const_value, dst, Align::from_bytes(0).unwrap());
            } else {
                trace!("copying memory using OpCopyMemory");
                self.emit()
                    .copy_memory(dst.def(self), src.def(self), None, None, empty())
                    .unwrap();
            }
        } else {
            self.emit()
                .copy_memory_sized(
                    dst.def(self),
                    src.def(self),
                    size.def(self),
                    None,
                    None,
                    empty(),
                )
                .unwrap();
            self.zombie(dst.def(self), "cannot memcpy dynamically sized data");
        }
    }

    fn memmove(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        self.memcpy(dst, dst_align, src, src_align, size, flags);
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        align: Align,
        flags: MemFlags,
    ) {
        if flags != MemFlags::empty() {
            self.err(format!(
                "memset with mem flags is not supported yet: {flags:?}"
            ));
        }

        let const_size = self
            .builder
            .lookup_const_scalar(size)
            .and_then(|size| Some(Size::from_bytes(u64::try_from(size).ok()?)));

        let elem_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee, .. } => pointee.unwrap_or_else(|| {
                // FIXME(eddyb) dedup w/ `type_padding_filler`.
                let unit = self.integer_approximate_align_prefer_legal(align);
                // FIXME(eddyb) what does this imply for runtime size?
                if let Some(size) = const_size {
                    let unit_size = unit.size().bytes();
                    assert_eq!(size.bytes() % unit_size, 0);
                }
                self.type_from_integer(unit)
            }),
            _ => self.fatal(format!(
                "memset called on non-pointer type: {}",
                self.debug_type(ptr.ty)
            )),
        };
        let elem_ty_spv = self.lookup_type(elem_ty);
        let pat = match self.builder.lookup_const_scalar(fill_byte) {
            Some(fill_byte) => self.memset_const_pattern(&elem_ty_spv, fill_byte as u8),
            None => self.memset_dynamic_pattern(&elem_ty_spv, fill_byte.def(self)),
        }
        .with_type(elem_ty);
        match const_size {
            Some(size) => self.memset_constant_size(ptr, pat, size.bytes()),
            None => self.memset_dynamic_size(ptr, pat, size),
        }
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        assert_ty_eq!(self, then_val.ty, else_val.ty);
        let result_type = then_val.ty;

        if let Some(ConstValue::Bool(b)) = self.try_get_const_value(cond) {
            // as we directly return the values, it'll preserve their constness as well
            return if b { then_val } else { else_val };
        }

        self.emit()
            .select(
                result_type,
                None,
                cond.def(self),
                then_val.def(self),
                else_val.def(self),
            )
            .unwrap()
            .with_type(result_type)
    }

    fn va_arg(&mut self, _list: Self::Value, _ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn extract_element(&mut self, vec: Self::Value, idx: Self::Value) -> Self::Value {
        let result_type = match self.lookup_type(vec.ty) {
            SpirvType::Vector { element, .. } => element,
            other => self.fatal(format!("extract_element not implemented on type {other:?}")),
        };
        match self.builder.lookup_const_scalar(idx) {
            Some(const_index) => self.emit().composite_extract(
                result_type,
                None,
                vec.def(self),
                [const_index as u32].iter().cloned(),
            ),
            None => {
                self.emit()
                    .vector_extract_dynamic(result_type, None, vec.def(self), idx.def(self))
            }
        }
        .unwrap()
        .with_type(result_type)
    }

    fn vector_splat(&mut self, num_elts: usize, elt: Self::Value) -> Self::Value {
        let result_type =
            SpirvType::simd_vector(self, self.span(), self.lookup_type(elt.ty), num_elts as u32)
                .def(self.span(), self);
        if self.builder.lookup_const(elt).is_some() {
            self.constant_composite(result_type, iter::repeat_n(elt.def(self), num_elts))
        } else {
            self.emit()
                .composite_construct(result_type, None, iter::repeat_n(elt.def(self), num_elts))
                .unwrap()
                .with_type(result_type)
        }
    }

    fn extract_value(&mut self, agg_val: Self::Value, idx: u64) -> Self::Value {
        let result_type = match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types, .. } => field_types[idx as usize],
            SpirvType::Array { element, .. }
            | SpirvType::Vector { element, .. }
            | SpirvType::Matrix { element, .. } => element,
            other => self.fatal(format!(
                "extract_value not implemented on type {}",
                other.debug(agg_val.ty, self)
            )),
        };
        self.emit()
            .composite_extract(
                result_type,
                None,
                agg_val.def(self),
                [idx as u32].iter().cloned(),
            )
            .unwrap()
            .with_type(result_type)
    }

    #[instrument(level = "trace", skip(self))]
    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        let field_type = match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types, .. } => field_types[idx as usize],
            SpirvType::Array { element, .. }
            | SpirvType::Vector { element, .. }
            | SpirvType::Matrix { element, .. } => element,
            other => self.fatal(format!("insert_value not implemented on type {other:?}")),
        };

        // HACK(eddyb) temporary workaround for untyped pointers upstream.
        // FIXME(eddyb) replace with untyped memory SPIR-V + `qptr` or similar.
        let elt = self.bitcast(elt, field_type);

        self.emit()
            .composite_insert(
                agg_val.ty,
                None,
                elt.def(self),
                agg_val.def(self),
                [idx as u32].iter().cloned(),
            )
            .unwrap()
            .with_type(agg_val.ty)
    }

    fn set_personality_fn(&mut self, _personality: Self::Function) {
        todo!()
    }

    // These are used by everyone except msvc
    fn cleanup_landing_pad(&mut self, _pers_fn: Self::Function) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn filter_landing_pad(&mut self, _pers_fn: Self::Function) {
        todo!()
    }

    fn resume(&mut self, _exn0: Self::Value, _exn1: Self::Value) {
        todo!()
    }

    // These are used only by msvc
    fn cleanup_pad(
        &mut self,
        _parent: Option<Self::Value>,
        _args: &[Self::Value],
    ) -> Self::Funclet {
        todo!()
    }

    fn cleanup_ret(&mut self, _funclet: &Self::Funclet, _unwind: Option<Self::BasicBlock>) {
        todo!()
    }

    fn catch_pad(&mut self, _parent: Self::Value, _args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn catch_switch(
        &mut self,
        _parent: Option<Self::Value>,
        _unwind: Option<Self::BasicBlock>,
        _handlers: &[Self::BasicBlock],
    ) -> Self::Value {
        todo!()
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: Self::Value,
        cmp: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        _weak: bool,
    ) -> (Self::Value, Self::Value) {
        assert_ty_eq!(self, cmp.ty, src.ty);
        let ty = src.ty;

        // HACK(eddyb) SPIR-V lacks pointer atomics, have to use integers instead.
        let atomic_ty = match self.lookup_type(ty) {
            SpirvType::Pointer { .. } => self.type_usize(),
            _ => ty,
        };
        let (dst, access_ty) = self.adjust_pointer_for_typed_access(dst, atomic_ty);
        let cmp = self.bitcast(cmp, access_ty);
        let src = self.bitcast(src, access_ty);

        self.validate_atomic(access_ty, dst.def(self));
        // TODO: Default to device scope
        let memory = self.constant_u32(self.span(), Scope::Device as u32);
        let semantics_equal = self.ordering_to_semantics_def(order);
        let semantics_unequal = self.ordering_to_semantics_def(failure_order);
        // Note: OpAtomicCompareExchangeWeak is deprecated, and has the same semantics
        let result = self
            .emit()
            .atomic_compare_exchange(
                access_ty,
                None,
                dst.def(self),
                memory.def(self),
                semantics_equal.def(self),
                semantics_unequal.def(self),
                src.def(self),
                cmp.def(self),
            )
            .unwrap()
            .with_type(access_ty);

        let val = self.bitcast(result, ty);
        let success = self.icmp(IntPredicate::IntEQ, result, cmp);

        (val, success)
    }

    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: Self::Value,
        src: Self::Value,
        order: AtomicOrdering,
    ) -> Self::Value {
        let ty = src.ty;

        // HACK(eddyb) SPIR-V lacks pointer atomics, have to use integers instead.
        let atomic_ty = match self.lookup_type(ty) {
            SpirvType::Pointer { .. } => self.type_usize(),
            _ => ty,
        };
        let (dst, access_ty) = self.adjust_pointer_for_typed_access(dst, atomic_ty);
        let src = self.bitcast(src, access_ty);

        self.validate_atomic(access_ty, dst.def(self));
        // TODO: Default to device scope
        let memory = self
            .constant_u32(self.span(), Scope::Device as u32)
            .def(self);
        let semantics = self.ordering_to_semantics_def(order).def(self);
        use AtomicRmwBinOp::*;
        let result = match op {
            AtomicXchg => self.emit().atomic_exchange(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicAdd => self.emit().atomic_i_add(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicSub => self.emit().atomic_i_sub(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicAnd => self.emit().atomic_and(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicNand => self.fatal("atomic nand is not supported"),
            AtomicOr => self.emit().atomic_or(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicXor => self.emit().atomic_xor(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicMax => self.emit().atomic_s_max(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicMin => self.emit().atomic_s_min(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicUMax => self.emit().atomic_u_max(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
            AtomicUMin => self.emit().atomic_u_min(
                access_ty,
                None,
                dst.def(self),
                memory,
                semantics,
                src.def(self),
            ),
        }
        .unwrap()
        .with_type(access_ty);
        self.bitcast(result, ty)
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, _scope: SynchronizationScope) {
        // Ignore sync scope (it only has "single thread" and "cross thread")
        // TODO: Default to device scope
        let memory = self
            .constant_u32(self.span(), Scope::Device as u32)
            .def(self);
        let semantics = self.ordering_to_semantics_def(order).def(self);
        self.emit().memory_barrier(memory, semantics).unwrap();
    }

    fn set_invariant_load(&mut self, _load: Self::Value) {
        // ignore
    }

    /// Called for `StorageLive`
    fn lifetime_start(&mut self, _ptr: Self::Value, _size: Size) {
        // ignore
    }

    /// Called for `StorageDead`
    fn lifetime_end(&mut self, _ptr: Self::Value, _size: Size) {
        // ignore
    }

    fn call(
        &mut self,
        callee_ty: Self::Type,
        _fn_attrs: Option<&CodegenFnAttrs>,
        fn_abi: Option<&FnAbi<'tcx, Ty<'tcx>>>,
        callee: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
        instance: Option<ty::Instance<'tcx>>,
    ) -> Self::Value {
        let span = tracing::span!(tracing::Level::DEBUG, "call");
        let _enter = span.enter();

        if funclet.is_some() {
            self.fatal("TODO: Funclets are not supported");
        }

        let mut func_call_opcode = Op::FunctionCall;
        let (callee_val, result_type, argument_types) = match self.lookup_type(callee.ty) {
            // FIXME(eddyb) avoid using `SpirvType::Pointer` for function ptrs.
            SpirvType::Pointer {
                pointee: Some(pointee),
                ..
            } => match self.lookup_type(pointee) {
                SpirvType::Function {
                    return_type,
                    arguments,
                } => {
                    let direct_callee = self.builder.lookup_const(callee).and_then(
                        |const_callee| match const_callee {
                            SpirvConst::PtrToFunc {
                                func_id,
                                mangled_func_name: _,
                            } => {
                                assert_ty_eq!(self, callee_ty, pointee);
                                Some(func_id)
                            }
                            _ => None,
                        },
                    );
                    (
                        direct_callee.unwrap_or_else(|| {
                            // Truly indirect call.
                            let fn_ptr_val = callee.def(self);
                            self.zombie(fn_ptr_val, "indirect calls are not supported in SPIR-V");
                            func_call_opcode = Op::FunctionPointerCallINTEL;
                            fn_ptr_val
                        }),
                        return_type,
                        arguments,
                    )
                }
                _ => bug!(
                    "call expected `fn` pointer to point to function type, got `{}`",
                    self.debug_type(pointee)
                ),
            },

            _ => bug!(
                "call expected `fn` pointer type, got `{}`",
                self.debug_type(callee.ty)
            ),
        };

        // HACK(eddyb) temporary workaround for untyped pointers upstream.
        // FIXME(eddyb) replace with untyped memory SPIR-V + `qptr` or similar.
        let args: SmallVec<[_; 8]> = args
            .iter()
            .zip_eq(argument_types)
            .map(|(&arg, &expected_type)| self.bitcast(arg, expected_type))
            .collect();
        let args = &args[..];

        // FIXME(eddyb) should the maps exist at all, now that the `DefId` is known
        // at `call` time, and presumably its high-level details can be looked up?
        let instance_def_id = instance.map(|instance| instance.def_id());

        let libm_intrinsic =
            instance_def_id.and_then(|def_id| self.libm_intrinsics.borrow().get(&def_id).copied());
        let buffer_load_intrinsic = instance_def_id
            .is_some_and(|def_id| self.buffer_load_intrinsics.borrow().contains(&def_id));
        let buffer_store_intrinsic = instance_def_id
            .is_some_and(|def_id| self.buffer_store_intrinsics.borrow().contains(&def_id));
        let is_panic_entry_point = instance_def_id
            .is_some_and(|def_id| self.panic_entry_points.borrow().contains(&def_id));
        let from_trait_impl =
            instance_def_id.and_then(|def_id| self.from_trait_impls.borrow().get(&def_id).copied());

        if let Some(libm_intrinsic) = libm_intrinsic {
            let result = self.call_libm_intrinsic(libm_intrinsic, result_type, args);
            if result_type != result.ty {
                bug!(
                    "Mismatched libm result type for {:?}: expected {}, got {}",
                    libm_intrinsic,
                    self.debug_type(result_type),
                    self.debug_type(result.ty),
                );
            }
            return result;
        }

        if is_panic_entry_point {
            return DecodedFormatArgs::try_decode_and_remove_format_args(self, args)
                .codegen_panic(self, result_type);
        }
        if buffer_load_intrinsic {
            return self.codegen_buffer_load_intrinsic(fn_abi, result_type, args);
        }
        if buffer_store_intrinsic {
            self.codegen_buffer_store_intrinsic(fn_abi, args);
            let void_ty = SpirvType::Void.def(rustc_span::DUMMY_SP, self);
            return self.undef(void_ty);
        }

        if let Some((source_ty, target_ty)) = from_trait_impl {
            // Optimize From::from calls with constant arguments to avoid creating intermediate types.
            // Since From is only implemented for safe conversions (widening conversions that preserve
            // the numeric value), we can directly create a constant of the target type for primitive
            // numeric types.
            if let [arg] = args
                && let Some(const_val) = self.builder.lookup_const_scalar(*arg)
            {
                use rustc_middle::ty::FloatTy;
                let optimized_result = match (source_ty.kind(), target_ty.kind()) {
                    // Integer widening conversions
                    (ty::Uint(_), ty::Uint(_)) | (ty::Int(_), ty::Int(_)) => {
                        Some(self.constant_int(result_type, const_val))
                    }
                    // Float widening conversions
                    // TODO(@LegNeato): Handle more float types
                    (ty::Float(FloatTy::F32), ty::Float(FloatTy::F64)) => {
                        let float_val = f32::from_bits(const_val as u32) as f64;
                        Some(self.constant_float(result_type, float_val))
                    }
                    // No optimization for narrowing conversions or unsupported types
                    _ => None,
                };

                if let Some(result) = optimized_result {
                    return result;
                }
            }
        }

        // Default: emit a regular function call
        let operands = [callee_val]
            .into_iter()
            .chain(args.iter().map(|arg| arg.def(self)))
            .map(Operand::IdRef)
            .collect();

        let mut builder = self.emit();
        let result_id = builder.id();
        builder
            .insert_into_block(
                InsertPoint::End,
                Instruction::new(
                    func_call_opcode,
                    Some(result_type),
                    Some(result_id),
                    operands,
                ),
            )
            .unwrap();
        result_id.with_type(result_type)
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }

    fn apply_attrs_to_cleanup_callsite(&mut self, _llret: Self::Value) {
        // Ignore
    }
}
