// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::Builder;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{BuilderCursor, SpirvConst, SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::codegen_cx::CodegenCx;
use crate::custom_insts::{CustomInst, CustomOp};
use crate::spirv_type::SpirvType;
use itertools::Itertools;
use rspirv::dr::{InsertPoint, Instruction, Operand};
use rspirv::spirv::{Capability, MemoryModel, MemorySemantics, Op, Scope, StorageClass, Word};
use rustc_apfloat::{Float, Round, Status, ieee};
use rustc_codegen_ssa::MemFlags;
use rustc_codegen_ssa::common::{
    AtomicOrdering, AtomicRmwBinOp, IntPredicate, RealPredicate, SynchronizationScope, TypeKind,
};
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    BackendTypes, BaseTypeCodegenMethods, BuilderMethods, ConstCodegenMethods,
    LayoutTypeCodegenMethods, OverflowOp,
};
use rustc_data_structures::fx::FxHashSet;
use rustc_middle::bug;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrs;
use rustc_middle::ty::layout::LayoutOf;
use rustc_middle::ty::{self, Ty};
use rustc_span::Span;
use rustc_target::abi::call::FnAbi;
use rustc_target::abi::{Align, BackendRepr, Scalar, Size, WrappingRange};
use smallvec::SmallVec;
use std::borrow::Cow;
use std::cell::Cell;
use std::iter::{self, empty};
use std::ops::RangeInclusive;

macro_rules! simple_op {
    (
        $func_name:ident, $inst_name:ident
        $(, fold_const {
            $(int($fold_int_lhs:ident, $fold_int_rhs:ident) => $fold_int:expr)?
        })?
    ) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            assert_ty_eq!(self, lhs.ty, rhs.ty);
            let result_type = lhs.ty;

            $(if let Some(const_lhs) = self.builder.lookup_const(lhs) {
                if let Some(const_rhs) = self.builder.lookup_const(rhs) {
                    match self.lookup_type(result_type) {
                        $(SpirvType::Integer(bits, signed) => {
                            let size = Size::from_bits(bits);
                            let as_u128 = |const_val| {
                                let x = match const_val {
                                    SpirvConst::Scalar(x) => x,
                                    _ => return None,
                                };
                                Some(if signed {
                                    size.sign_extend(x) as u128
                                } else {
                                    size.truncate(x)
                                })
                            };
                            if let Some($fold_int_lhs) = as_u128(const_lhs) {
                                if let Some($fold_int_rhs) = as_u128(const_rhs) {
                                    return self.const_uint_big(result_type, $fold_int);
                                }
                            }
                        })?
                        _ => {}
                    }
                }
            })?

            self.emit()
                .$inst_name(result_type, None, lhs.def(self), rhs.def(self))
                .unwrap()
                .with_type(result_type)
        }
    };
}

// shl and shr allow different types as their operands
macro_rules! simple_op_unchecked_type {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
            self.emit()
                .$inst_name(lhs.ty, None, lhs.def(self), rhs.def(self))
                .unwrap()
                .with_type(lhs.ty)
        }
    };
}

macro_rules! simple_uni_op {
    ($func_name:ident, $inst_name:ident) => {
        fn $func_name(&mut self, val: Self::Value) -> Self::Value {
            self.emit()
                .$inst_name(val.ty, None, val.def(self))
                .unwrap()
                .with_type(val.ty)
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
    builder: &Builder<'_, '_>,
    fill_var: Word,
    byte_width: usize,
    is_float: bool,
) -> Word {
    let composite_type = SpirvType::Vector {
        element: SpirvType::Integer(8, false).def(builder.span(), builder),
        count: byte_width as u32,
    }
    .def(builder.span(), builder);
    let composite = builder
        .emit()
        .composite_construct(
            composite_type,
            None,
            iter::repeat(fill_var).take(byte_width),
        )
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
    fn ordering_to_semantics_def(&self, ordering: AtomicOrdering) -> SpirvValue {
        let mut invalid_seq_cst = false;
        let semantics = match ordering {
            AtomicOrdering::Unordered | AtomicOrdering::Relaxed => MemorySemantics::NONE,
            // Note: rustc currently has AtomicOrdering::Consume commented out, if it ever becomes
            // uncommented, it should be MakeVisible | Acquire.
            AtomicOrdering::Acquire => MemorySemantics::MAKE_VISIBLE | MemorySemantics::ACQUIRE,
            AtomicOrdering::Release => MemorySemantics::MAKE_AVAILABLE | MemorySemantics::RELEASE,
            AtomicOrdering::AcquireRelease => {
                MemorySemantics::MAKE_AVAILABLE
                    | MemorySemantics::MAKE_VISIBLE
                    | MemorySemantics::ACQUIRE_RELEASE
            }
            AtomicOrdering::SequentiallyConsistent => {
                let emit = self.emit();
                let memory_model = emit.module_ref().memory_model.as_ref().unwrap();
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
                "cannot use AtomicOrdering=SequentiallyConsistent on Vulkan memory model \
                 (check if AcquireRelease fits your needs)",
            );
        }
        semantics
    }

    fn memset_const_pattern(&self, ty: &SpirvType<'tcx>, fill_byte: u8) -> Word {
        match *ty {
            SpirvType::Void => self.fatal("memset invalid on void pattern"),
            SpirvType::Bool => self.fatal("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
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
            SpirvType::Vector { element, count } | SpirvType::Matrix { element, count } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                self.constant_composite(
                    ty.def(self.span(), self),
                    iter::repeat(elem_pat).take(count as usize),
                )
                .def(self)
            }
            SpirvType::Array { element, count } => {
                let elem_pat = self.memset_const_pattern(&self.lookup_type(element), fill_byte);
                let count = self.builder.lookup_const_scalar(count).unwrap() as usize;
                self.constant_composite(
                    ty.def(self.span(), self),
                    iter::repeat(elem_pat).take(count),
                )
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

    fn memset_dynamic_pattern(&self, ty: &SpirvType<'tcx>, fill_var: Word) -> Word {
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
            SpirvType::Array { element, count } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                let count = self.builder.lookup_const_scalar(count).unwrap() as usize;
                self.emit()
                    .composite_construct(
                        ty.def(self.span(), self),
                        None,
                        iter::repeat(elem_pat).take(count),
                    )
                    .unwrap()
            }
            SpirvType::Vector { element, count } | SpirvType::Matrix { element, count } => {
                let elem_pat = self.memset_dynamic_pattern(&self.lookup_type(element), fill_var);
                self.emit()
                    .composite_construct(
                        ty.def(self.span(), self),
                        None,
                        iter::repeat(elem_pat).take(count as usize),
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
                let gep_ptr = self.gep(pat.ty, ptr, &[const_index]);
                self.store(pat, gep_ptr, Align::from_bytes(0).unwrap());
            }
        }
    }

    // TODO: Test this is correct
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

    fn zombie_convert_ptr_to_u(&self, def: Word) {
        self.zombie(def, "cannot convert pointers to integers");
    }

    fn zombie_convert_u_to_ptr(&self, def: Word) {
        self.zombie(def, "cannot convert integers to pointers");
    }

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
    fn adjust_pointer_for_typed_access(
        &mut self,
        ptr: SpirvValue,
        ty: <Self as BackendTypes>::Type,
    ) -> (SpirvValue, <Self as BackendTypes>::Type) {
        self.lookup_type(ty)
            .sizeof(self)
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
    fn adjust_pointer_for_sized_access(
        &mut self,
        ptr: SpirvValue,
        size: Size,
    ) -> Option<(SpirvValue, <Self as BackendTypes>::Type)> {
        let ptr = ptr.strip_ptrcasts();
        let mut leaf_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => pointee,
            SpirvType::Adt { .. } => return None,
            other => self.fatal(format!("adjust_pointer for non-pointer type: {other:?}")),
        };

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
        Some((leaf_ptr, leaf_ty))
    }

    /// If possible, return the appropriate `OpAccessChain` indices for going
    /// from a pointer to `ty`, to a pointer to some leaf field/element having
    /// a size that fits `leaf_size_range` (and, optionally, the type `leaf_ty`),
    /// while adding `offset` bytes.
    ///
    /// That is, try to turn `((_: *T) as *u8).add(offset) as *Leaf` into a series
    /// of struct field and array/vector element accesses.
    fn recover_access_chain_from_offset(
        &self,
        mut ty: <Self as BackendTypes>::Type,
        mut offset: Size,
        // FIXME(eddyb) using `None` for "unsized" is a pretty bad design.
        leaf_size_or_unsized_range: RangeInclusive<Option<Size>>,
        leaf_ty: Option<<Self as BackendTypes>::Type>,
    ) -> Option<(SmallVec<[u32; 8]>, <Self as BackendTypes>::Type)> {
        assert_ne!(Some(ty), leaf_ty);

        // HACK(eddyb) this has the correct ordering (`Sized(_) < Unsized`).
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
                    ty_kind = field_ty_kind;
                    ty_size = field_ty_size;

                    indices.push(i as u32);
                    offset = offset_in_field;
                }
                SpirvType::Vector { element, .. }
                | SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element }
                | SpirvType::Matrix { element, .. } => {
                    ty = element;
                    ty_kind = self.lookup_type(ty);

                    let stride = ty_kind.sizeof(self)?;
                    ty_size = MaybeSized::Sized(stride);

                    if stride.bytes() == 0 {
                        indices.push(0);
                        offset = Size::from_bytes(0);
                    } else {
                        indices.push((offset.bytes() / stride.bytes()).try_into().ok()?);
                        offset = Size::from_bytes(offset.bytes() % stride.bytes());
                    }
                }
                _ => return None,
            }

            // Avoid digging beyond the point the leaf could actually fit.
            if ty_size < *leaf_size_range.start() {
                return None;
            }

            if offset == Size::ZERO
                && leaf_size_range.contains(&ty_size)
                && leaf_ty.map_or(true, |leaf_ty| leaf_ty == ty)
            {
                return Some((indices, ty));
            }
        }
    }

    fn maybe_inbounds_gep(
        &mut self,
        ty: Word,
        ptr: SpirvValue,
        combined_indices: &[SpirvValue],
        is_inbounds: bool,
    ) -> SpirvValue {
        let (&ptr_base_index, indices) = combined_indices.split_first().unwrap();

        // The first index is an offset to the pointer, the rest are actual members.
        // https://llvm.org/docs/GetElementPtr.html
        // "An OpAccessChain instruction is the equivalent of an LLVM getelementptr instruction where the first index element is zero."
        // https://github.com/gpuweb/gpuweb/issues/33
        let mut result_pointee_type = ty;
        let indices: Vec<_> = indices
            .iter()
            .map(|index| {
                result_pointee_type = match self.lookup_type(result_pointee_type) {
                    SpirvType::Array { count, element }
                        if self.builder.lookup_const_scalar(count) == Some(0) =>
                    {
                        self.fatal(format!(
                            "evaluation of constant value failed: cannot index into [{}; 0]",
                            self.debug_type(element)
                        ))
                    }
                    SpirvType::Array { element, .. } | SpirvType::RuntimeArray { element } => {
                        element
                    }
                    _ => self.fatal(format!(
                        "GEP not implemented for type {}",
                        self.debug_type(result_pointee_type)
                    )),
                };
                index.def(self)
            })
            .collect();

        // Special-case field accesses through a `pointercast`, to access the
        // right field in the original type, for the `Logical` addressing model.
        let ptr = ptr.strip_ptrcasts();
        let ptr_id = ptr.def(self);
        let original_pointee_ty = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => pointee,
            other => self.fatal(format!("gep called on non-pointer type: {other:?}")),
        };

        // HACK(eddyb) `struct_gep` itself is falling out of use, as it's being
        // replaced upstream by `ptr_add` (aka `inbounds_gep` with byte offsets).
        //
        // FIXME(eddyb) get rid of everything other than:
        // - constant byte offset (`ptr_add`?)
        // - dynamic indexing of a single array
        let const_ptr_offset = self
            .builder
            .lookup_const_scalar(ptr_base_index)
            .and_then(|idx| Some(u64::try_from(idx).ok()? * self.lookup_type(ty).sizeof(self)?));
        if let Some(const_ptr_offset) = const_ptr_offset {
            if let Some((base_indices, base_pointee_ty)) = self.recover_access_chain_from_offset(
                original_pointee_ty,
                const_ptr_offset,
                Some(Size::ZERO)..=None,
                None,
            ) {
                // FIXME(eddyb) this condition is pretty limiting, but
                // eventually it shouldn't matter if GEPs are going away.
                if ty == base_pointee_ty || indices.is_empty() {
                    let result_pointee_type = if indices.is_empty() {
                        base_pointee_ty
                    } else {
                        result_pointee_type
                    };
                    let indices = base_indices
                        .into_iter()
                        .map(|idx| self.constant_u32(self.span(), idx).def(self))
                        .chain(indices)
                        .collect();
                    return self.emit_access_chain(
                        self.type_ptr_to(result_pointee_type),
                        ptr_id,
                        None,
                        indices,
                        is_inbounds,
                    );
                }
            }
        }

        let result_type = self.type_ptr_to(result_pointee_type);

        // Check if `ptr_id` is defined by an `OpAccessChain`, and if it is,
        // grab its base pointer and indices.
        //
        // FIXME(eddyb) this could get ridiculously expensive, at the very least
        // it could use `.rev()`, hoping the base pointer was recently defined?
        let maybe_original_access_chain = if ty == original_pointee_ty {
            let emit = self.emit();
            let module = emit.module_ref();
            let func = &module.functions[emit.selected_function().unwrap()];
            let base_ptr_and_combined_indices = func
                .all_inst_iter()
                .find(|inst| inst.result_id == Some(ptr_id))
                .and_then(|ptr_def_inst| {
                    if matches!(
                        ptr_def_inst.class.opcode,
                        Op::AccessChain | Op::InBoundsAccessChain
                    ) {
                        let base_ptr = ptr_def_inst.operands[0].unwrap_id_ref();
                        let indices = ptr_def_inst.operands[1..]
                            .iter()
                            .map(|op| op.unwrap_id_ref())
                            .collect::<Vec<_>>();
                        Some((base_ptr, indices))
                    } else {
                        None
                    }
                });
            base_ptr_and_combined_indices
        } else {
            None
        };
        if let Some((original_ptr, mut original_indices)) = maybe_original_access_chain {
            // Transform the following:
            // OpAccessChain original_ptr [a, b, c]
            // OpPtrAccessChain ptr base [d, e, f]
            // into
            // OpAccessChain original_ptr [a, b, c + base, d, e, f]
            // to remove the need for OpPtrAccessChain
            let last = original_indices.last_mut().unwrap();
            *last = self
                .add(last.with_type(ptr_base_index.ty), ptr_base_index)
                .def(self);
            original_indices.extend(indices);
            return self.emit_access_chain(
                result_type,
                original_ptr,
                None,
                original_indices,
                is_inbounds,
            );
        }

        // HACK(eddyb) temporary workaround for untyped pointers upstream.
        // FIXME(eddyb) replace with untyped memory SPIR-V + `qptr` or similar.
        let ptr = self.pointercast(ptr, self.type_ptr_to(ty));
        let ptr_id = ptr.def(self);

        self.emit_access_chain(
            result_type,
            ptr_id,
            Some(ptr_base_index),
            indices,
            is_inbounds,
        )
    }

    fn emit_access_chain(
        &self,
        result_type: <Self as BackendTypes>::Type,
        pointer: Word,
        ptr_base_index: Option<SpirvValue>,
        indices: Vec<Word>,
        is_inbounds: bool,
    ) -> SpirvValue {
        let mut emit = self.emit();

        let non_zero_ptr_base_index =
            ptr_base_index.filter(|&idx| self.builder.lookup_const_scalar(idx) != Some(0));
        if let Some(ptr_base_index) = non_zero_ptr_base_index {
            let result = if is_inbounds {
                emit.in_bounds_ptr_access_chain(
                    result_type,
                    None,
                    pointer,
                    ptr_base_index.def(self),
                    indices,
                )
            } else {
                emit.ptr_access_chain(
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
                emit.in_bounds_access_chain(result_type, None, pointer, indices)
            } else {
                emit.access_chain(result_type, None, pointer, indices)
            }
            .unwrap()
        }
        .with_type(result_type)
    }

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
        let inst = Instruction::new(Op::Variable, Some(ptr_ty), Some(result_id), vec![
            Operand::StorageClass(StorageClass::Function),
        ]);
        builder.insert_into_block(index, inst).unwrap();
        result_id.with_type(ptr_ty)
    }
}

impl<'a, 'tcx> BuilderMethods<'a, 'tcx> for Builder<'a, 'tcx> {
    type CodegenCx = CodegenCx<'tcx>;

    fn build(cx: &'a Self::CodegenCx, llbb: Self::BasicBlock) -> Self {
        let cursor = cx.builder.select_block_by_id(llbb);
        // FIXME(eddyb) change `Self::Function` to be more like a function index.
        let current_fn = {
            let emit = cx.emit_with_cursor(cursor);
            let selected_function = emit.selected_function().unwrap();
            let selected_function = &emit.module_ref().functions[selected_function];
            let def_inst = selected_function.def.as_ref().unwrap();
            let def = def_inst.result_id.unwrap();
            let ty = def_inst.operands[1].unwrap_id_ref();
            def.with_type(ty)
        };
        Self {
            cx,
            cursor,
            current_fn,
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

                self.custom_inst(void_ty, CustomInst::SetDebugSrcLoc {
                    file: Operand::IdRef(file.file_name_op_string_id),
                    line_start: Operand::IdRef(self.const_u32(line_start).def(self)),
                    line_end: Operand::IdRef(self.const_u32(line_end).def(self)),
                    col_start: Operand::IdRef(self.const_u32(col_start).def(self)),
                    col_end: Operand::IdRef(self.const_u32(col_end).def(self)),
                });
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
        let cursor_fn = cx.builder.select_function_by_id(llfn.def_cx(cx));
        cx.emit_with_cursor(cursor_fn).begin_block(None).unwrap()
    }

    fn append_sibling_block(&mut self, _name: &str) -> Self::BasicBlock {
        self.emit_with_cursor(BuilderCursor {
            function: self.cursor.function,
            block: None,
        })
        .begin_block(None)
        .unwrap()
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
        self.emit().branch(dest).unwrap();
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
                    .branch_conditional(cond, then_llbb, else_llbb, empty())
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
            .map(|(i, b)| (construct_case(self, signed, i), b))
            .collect::<Vec<_>>();
        self.emit().switch(v.def(self), else_llbb, cases).unwrap();
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
        self.emit().branch(then).unwrap();
        result
    }

    fn unreachable(&mut self) {
        self.emit().unreachable().unwrap();
    }

    simple_op! {
        add, i_add,
        fold_const {
            int(a, b) => a.wrapping_add(b)
        }
    }
    // FIXME(eddyb) try to annotate the SPIR-V for `fast` and `algebraic`.
    simple_op! {fadd, f_add}
    simple_op! {fadd_fast, f_add} // fast=normal
    simple_op! {fadd_algebraic, f_add} // algebraic=normal
    simple_op! {sub, i_sub}
    simple_op! {fsub, f_sub}
    simple_op! {fsub_fast, f_sub} // fast=normal
    simple_op! {fsub_algebraic, f_sub} // algebraic=normal
    simple_op! {
        mul, i_mul,
        // HACK(eddyb) `rustc_codegen_ssa` relies on `Builder` methods doing
        // on-the-fly constant-folding, for e.g. intrinsics that copy memory.
        fold_const {
            int(a, b) => a.wrapping_mul(b)
        }
    }
    simple_op! {fmul, f_mul}
    simple_op! {fmul_fast, f_mul} // fast=normal
    simple_op! {fmul_algebraic, f_mul} // algebraic=normal
    simple_op! {udiv, u_div}
    // Note: exactudiv is UB when there's a remainder, so it's valid to implement as a normal div.
    // TODO: Can we take advantage of the UB and emit something else?
    simple_op! {exactudiv, u_div}
    simple_op! {sdiv, s_div}
    // Same note and TODO as exactudiv
    simple_op! {exactsdiv, s_div}
    simple_op! {fdiv, f_div}
    simple_op! {fdiv_fast, f_div} // fast=normal
    simple_op! {fdiv_algebraic, f_div} // algebraic=normal
    simple_op! {urem, u_mod}
    simple_op! {srem, s_rem}
    simple_op! {frem, f_rem}
    simple_op! {frem_fast, f_rem} // fast=normal
    simple_op! {frem_algebraic, f_rem} // algebraic=normal
    simple_op_unchecked_type! {shl, shift_left_logical}
    simple_op_unchecked_type! {lshr, shift_right_logical}
    simple_op_unchecked_type! {ashr, shift_right_arithmetic}
    simple_op! {unchecked_sadd, i_add} // already unchecked by default
    simple_op! {unchecked_uadd, i_add} // already unchecked by default
    simple_op! {unchecked_ssub, i_sub} // already unchecked by default
    simple_op! {unchecked_usub, i_sub} // already unchecked by default
    simple_op! {unchecked_smul, i_mul} // already unchecked by default
    simple_op! {unchecked_umul, i_mul} // already unchecked by default
    simple_uni_op! {neg, s_negate}
    simple_uni_op! {fneg, f_negate}

    fn and(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => {
                self.emit()
                    .bitwise_and(ty, None, lhs.def(self), rhs.def(self))
            }
            SpirvType::Bool => self
                .emit()
                .logical_and(ty, None, lhs.def(self), rhs.def(self)),
            o => self.fatal(format!(
                "and() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => {
                self.emit()
                    .bitwise_or(ty, None, lhs.def(self), rhs.def(self))
            }
            SpirvType::Bool => self
                .emit()
                .logical_or(ty, None, lhs.def(self), rhs.def(self)),
            o => self.fatal(format!(
                "or() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let ty = lhs.ty;
        match self.lookup_type(ty) {
            SpirvType::Integer(_, _) => {
                self.emit()
                    .bitwise_xor(ty, None, lhs.def(self), rhs.def(self))
            }
            SpirvType::Bool => {
                self.emit()
                    .logical_not_equal(ty, None, lhs.def(self), rhs.def(self))
            }
            o => self.fatal(format!(
                "xor() not implemented for type {}",
                o.debug(ty, self)
            )),
        }
        .unwrap()
        .with_type(ty)
    }
    fn not(&mut self, val: Self::Value) -> Self::Value {
        match self.lookup_type(val.ty) {
            SpirvType::Integer(_, _) => self.emit().not(val.ty, None, val.def(self)),
            SpirvType::Bool => {
                let true_ = self.constant_bool(self.span(), true);
                // intel-compute-runtime doesn't like OpLogicalNot
                self.emit()
                    .logical_not_equal(val.ty, None, val.def(self), true_.def(self))
            }
            o => self.fatal(format!(
                "not() not implemented for type {}",
                o.debug(val.ty, self)
            )),
        }
        .unwrap()
        .with_type(val.ty)
    }

    fn checked_binop(
        &mut self,
        oop: OverflowOp,
        ty: Ty<'_>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        // adopted partially from https://github.com/ziglang/zig/blob/master/src/codegen/spirv.zig
        let is_add = match oop {
            OverflowOp::Add => true,
            OverflowOp::Sub => false,
            OverflowOp::Mul => {
                // NOTE(eddyb) this needs to be `undef`, not `false`/`true`, because
                // we don't want the user's boolean constants to keep the zombie alive.
                let bool = SpirvType::Bool.def(self.span(), self);
                let overflowed = self.undef(bool);

                let result = (self.mul(lhs, rhs), overflowed);
                self.zombie(result.1.def(self), "checked mul is not supported yet");
                return result;
            }
        };
        let signed = match ty.kind() {
            ty::Int(_) => true,
            ty::Uint(_) => false,
            other => self.fatal(format!("Unexpected {} type: {other:#?}", match oop {
                OverflowOp::Add => "checked add",
                OverflowOp::Sub => "checked sub",
                OverflowOp::Mul => "checked mul",
            })),
        };

        let result = if is_add {
            self.add(lhs, rhs)
        } else {
            self.sub(lhs, rhs)
        };

        let overflowed = if signed {
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
                if is_add {
                    IntPredicate::IntSGT
                } else {
                    IntPredicate::IntSLT
                },
                result,
                lhs,
            );
            self.icmp(IntPredicate::IntEQ, rhs_lt_zero, result_gt_lhs)
        } else {
            // for unsigned addition, overflow occurred if the result is less than any of the operands.
            // for subtraction, overflow occurred if the result is greater.
            self.icmp(
                if is_add {
                    IntPredicate::IntULT
                } else {
                    IntPredicate::IntUGT
                },
                result,
                lhs,
            )
        };

        (result, overflowed)
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
        let (ptr, access_ty) = self.adjust_pointer_for_typed_access(ptr, ty);

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
            OperandValue::Immediate(self.to_immediate(llval, place.layout))
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
        let (ptr, access_ty) = self.adjust_pointer_for_typed_access(ptr, val.ty);
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
                return self.bitcast(in_leaf, dest_ty);
            }
            // Repack output newtypes, after bitcasting the leaf inside, instead.
            if let Some((indices, out_leaf_ty)) = unpack_newtype(dest_ty, dest_ty_kind) {
                let out_leaf = self.bitcast(val, out_leaf_ty);
                let out_agg_undef = self.undef(dest_ty);
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
                return self.pointercast(val, dest_ty);
            }

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

    fn pointercast(&mut self, ptr: Self::Value, dest_ty: Self::Type) -> Self::Value {
        // HACK(eddyb) reuse the special-casing in `const_bitcast`, which relies
        // on adding a pointer type to an untyped pointer (to some const data).
        if let SpirvValueKind::IllegalConst(_) = ptr.kind {
            return self.const_bitcast(ptr, dest_ty);
        }

        if ptr.ty == dest_ty {
            return ptr;
        }

        // Strip a previous `pointercast`, to reveal the original pointer type.
        let ptr = ptr.strip_ptrcasts();

        if ptr.ty == dest_ty {
            return ptr;
        }

        let ptr_pointee = match self.lookup_type(ptr.ty) {
            SpirvType::Pointer { pointee } => pointee,
            other => self.fatal(format!(
                "pointercast called on non-pointer source type: {other:?}"
            )),
        };
        let dest_pointee = match self.lookup_type(dest_ty) {
            SpirvType::Pointer { pointee } => pointee,
            other => self.fatal(format!(
                "pointercast called on non-pointer dest type: {other:?}"
            )),
        };
        let dest_pointee_size = self.lookup_type(dest_pointee).sizeof(self);

        if let Some((indices, _)) = self.recover_access_chain_from_offset(
            ptr_pointee,
            Size::ZERO,
            dest_pointee_size..=dest_pointee_size,
            Some(dest_pointee),
        ) {
            let indices = indices
                .into_iter()
                .map(|idx| self.constant_u32(self.span(), idx).def(self))
                .collect::<Vec<_>>();
            self.emit()
                .in_bounds_access_chain(dest_ty, None, ptr.def(self), indices)
                .unwrap()
                .with_type(dest_ty)
        } else {
            // Defer the cast so that it has a chance to be avoided.
            let original_ptr = ptr.def(self);
            SpirvValue {
                kind: SpirvValueKind::LogicalPtrCast {
                    original_ptr,
                    original_ptr_ty: ptr.ty,
                    bitcast_result_id: self.emit().bitcast(dest_ty, None, original_ptr).unwrap(),
                },
                ty: dest_ty,
            }
        }
    }

    fn icmp(&mut self, op: IntPredicate, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        // Note: the signedness of the opcode doesn't have to match the signedness of the operands.
        use IntPredicate::*;
        assert_ty_eq!(self, lhs.ty, rhs.ty);
        let b = SpirvType::Bool.def(self.span(), self);
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
                IntULE => {
                    let true_ = self.constant_bool(self.span(), true);
                    let lhs = self
                        .emit()
                        .logical_not_equal(b, None, lhs.def(self), true_.def(self))
                        .unwrap();
                    self.emit().logical_and(b, None, lhs, rhs.def(self))
                }
                // x <= y  =>  !x || y
                IntULT => {
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
            let dst_adj = self.adjust_pointer_for_sized_access(dst, const_size);
            let src_adj = self.adjust_pointer_for_sized_access(src, const_size);
            match (dst_adj, src_adj) {
                // HACK(eddyb) fill in missing `dst`/`src` with the other side.
                (Some((dst, access_ty)), None) => {
                    Some((dst, self.pointercast(src, self.type_ptr_to(access_ty))))
                }
                (None, Some((src, access_ty))) => {
                    Some((self.pointercast(dst, self.type_ptr_to(access_ty)), src))
                }
                (Some((dst, dst_access_ty)), Some((src, src_access_ty)))
                    if dst_access_ty == src_access_ty =>
                {
                    Some((dst, src))
                }
                (None, None) | (Some(_), Some(_)) => None,
            }
        });

        if let Some((dst, src)) = typed_copy_dst_src {
            if let Some(const_value) = src.const_fold_load(self) {
                self.store(const_value, dst, Align::from_bytes(0).unwrap());
            } else {
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
        _align: Align,
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
            SpirvType::Pointer { pointee } => pointee,
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
        let result_type = SpirvType::Vector {
            element: elt.ty,
            count: num_elts as u32,
        }
        .def(self.span(), self);
        if self.builder.lookup_const(elt).is_some() {
            self.constant_composite(result_type, iter::repeat(elt.def(self)).take(num_elts))
        } else {
            self.emit()
                .composite_construct(
                    result_type,
                    None,
                    iter::repeat(elt.def(self)).take(num_elts),
                )
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

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        let field_type = match self.lookup_type(agg_val.ty) {
            SpirvType::Adt { field_types, .. } => field_types[idx as usize],
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

    fn set_personality_fn(&mut self, _personality: Self::Value) {
        todo!()
    }

    // These are used by everyone except msvc
    fn cleanup_landing_pad(&mut self, _pers_fn: Self::Value) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn filter_landing_pad(&mut self, _pers_fn: Self::Value) -> (Self::Value, Self::Value) {
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

        let (dst, access_ty) = self.adjust_pointer_for_typed_access(dst, ty);
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
        let success = self.icmp(IntPredicate::IntEQ, val, cmp);

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

        let (dst, access_ty) = self.adjust_pointer_for_typed_access(dst, ty);
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
        _fn_abi: Option<&FnAbi<'tcx, Ty<'tcx>>>,
        callee: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
        instance: Option<ty::Instance<'tcx>>,
    ) -> Self::Value {
        if funclet.is_some() {
            self.fatal("TODO: Funclets are not supported");
        }

        // NOTE(eddyb) see the comment on `SpirvValueKind::FnAddr`, this should
        // be fixed upstream, so we never see any "function pointer" values being
        // created just to perform direct calls.
        let (callee_val, result_type, argument_types) = match self.lookup_type(callee.ty) {
            // HACK(eddyb) this seems to be needed, but it's not what `get_fn_addr`
            // produces, are these coming from inside `rustc_codegen_spirv`?
            SpirvType::Function {
                return_type,
                arguments,
            } => {
                assert_ty_eq!(self, callee_ty, callee.ty);
                (callee.def(self), return_type, arguments)
            }

            SpirvType::Pointer { pointee } => match self.lookup_type(pointee) {
                SpirvType::Function {
                    return_type,
                    arguments,
                } => (
                    if let SpirvValueKind::FnAddr { function } = callee.kind {
                        assert_ty_eq!(self, callee_ty, pointee);
                        function
                    }
                    // Truly indirect call.
                    else {
                        let fn_ptr_val = callee.def(self);
                        self.zombie(fn_ptr_val, "indirect calls are not supported in SPIR-V");
                        fn_ptr_val
                    },
                    return_type,
                    arguments,
                ),
                _ => bug!(
                    "call expected `fn` pointer to point to function type, got `{}`",
                    self.debug_type(pointee)
                ),
            },

            _ => bug!(
                "call expected function or `fn` pointer type, got `{}`",
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
            .and_then(|def_id| self.buffer_load_intrinsics.borrow().get(&def_id).copied());
        let buffer_store_intrinsic = instance_def_id
            .and_then(|def_id| self.buffer_store_intrinsics.borrow().get(&def_id).copied());
        let is_panic_entry_point = instance_def_id
            .is_some_and(|def_id| self.panic_entry_points.borrow().contains(&def_id));

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
            result
        } else if is_panic_entry_point {
            // HACK(eddyb) Rust 2021 `panic!` always uses `format_args!`, even
            // in the simple case that used to pass a `&str` constant, which
            // would not remain reachable in the SPIR-V - but `format_args!` is
            // more complex and neither immediate (`fmt::Arguments` is too big)
            // nor simplified in MIR (e.g. promoted to a constant) in any way,
            // so we have to try and remove the `fmt::Arguments::new` call here.
            #[derive(Default)]
            struct DecodedFormatArgs<'tcx> {
                /// If fully constant, the `pieces: &'a [&'static str]` input
                /// of `fmt::Arguments<'a>` (i.e. the strings between args).
                const_pieces: Option<SmallVec<[String; 2]>>,

                /// Original references for `fmt::Arguments<'a>` dynamic arguments,
                /// i.e. the `&'a T` passed to `fmt::rt::Argument::<'a>::new_*`,
                /// tracking the type `T` and `char` formatting specifier.
                ///
                /// E.g. for `format_args!("{a} {b:x}")` they'll be:
                /// * `&a` with `typeof a` and ' ',
                /// * `&b` with `typeof b` and 'x'
                ref_arg_ids_with_ty_and_spec: SmallVec<[(Word, Ty<'tcx>, char); 2]>,
            }
            struct FormatArgsNotRecognized(String);

            // HACK(eddyb) this is basically a `try` block.
            let try_decode_and_remove_format_args = || {
                let mut decoded_format_args = DecodedFormatArgs::default();

                let const_u32_as_usize = |ct_id| match self.builder.lookup_const_by_id(ct_id)? {
                    SpirvConst::Scalar(x) => Some(u32::try_from(x).ok()? as usize),
                    _ => None,
                };
                let const_slice_as_elem_ids = |ptr_id: Word, len: usize| {
                    if let SpirvConst::PtrTo { pointee } =
                        self.builder.lookup_const_by_id(ptr_id)?
                    {
                        if let SpirvConst::Composite(elems) =
                            self.builder.lookup_const_by_id(pointee)?
                        {
                            if elems.len() == len {
                                return Some(elems);
                            }
                        }
                    }
                    None
                };
                let const_str_as_utf8 = |&[str_ptr_id, str_len_id]: &[Word; 2]| {
                    let str_len = const_u32_as_usize(str_len_id)?;
                    let piece_str_bytes = const_slice_as_elem_ids(str_ptr_id, str_len)?
                        .iter()
                        .map(|&id| u8::try_from(const_u32_as_usize(id)?).ok())
                        .collect::<Option<Vec<u8>>>()?;
                    String::from_utf8(piece_str_bytes).ok()
                };

                // HACK(eddyb) some entry-points only take a `&str`, not `fmt::Arguments`.
                if let [
                    SpirvValue {
                        kind: SpirvValueKind::Def(a_id),
                        ..
                    },
                    SpirvValue {
                        kind: SpirvValueKind::Def(b_id),
                        ..
                    },
                    ref other_args @ ..,
                ] = args[..]
                {
                    // Optional `&'static panic::Location<'static>`.
                    if other_args.len() <= 1 {
                        if let Some(const_msg) = const_str_as_utf8(&[a_id, b_id]) {
                            decoded_format_args.const_pieces =
                                Some([const_msg].into_iter().collect());
                            return Ok(decoded_format_args);
                        }
                    }
                }

                let format_args_id = match *args {
                    // HACK(eddyb) `panic_nounwind_fmt` takes an extra argument.
                    [
                        SpirvValue {
                            kind: SpirvValueKind::Def(format_args_id),
                            ..
                        },
                        _, // `&'static panic::Location<'static>`
                    ]
                    | [
                        SpirvValue {
                            kind: SpirvValueKind::Def(format_args_id),
                            ..
                        },
                        _, // `force_no_backtrace: bool`
                        _, // `&'static panic::Location<'static>`
                    ] => format_args_id,

                    _ => {
                        return Err(FormatArgsNotRecognized(
                            "panic entry-point call args".into(),
                        ));
                    }
                };

                let custom_ext_inst_set_import = self.ext_inst.borrow_mut().import_custom(self);

                // HACK(eddyb) we can remove SSA instructions even when they have
                // side-effects, *as long as* they are "local" enough and cannot
                // be observed from outside this current invocation - because the
                // the abort, any SSA definitions or local variable writes can't
                // be actually used anywhere else (other than *before* the abort).
                let mut builder = self.emit();
                let func_idx = builder.selected_function().unwrap();
                let block_idx = builder.selected_block().unwrap();
                let func = &mut builder.module_mut().functions[func_idx];

                // HACK(eddyb) this is used to check that all `Op{Store,Load}`s
                // that may get removed, operate on local `OpVariable`s,
                // i.e. are not externally observable.
                let local_var_ids: FxHashSet<_> = func.blocks[0]
                    .instructions
                    .iter()
                    .take_while(|inst| inst.class.opcode == Op::Variable)
                    .map(|inst| inst.result_id.unwrap())
                    .collect();
                let require_local_var = |ptr_id, var| {
                    Some(())
                        .filter(|()| local_var_ids.contains(&ptr_id))
                        .ok_or_else(|| FormatArgsNotRecognized(format!("{var} storage not local")))
                };

                let mut non_debug_insts = func.blocks[block_idx]
                    .instructions
                    .iter()
                    .enumerate()
                    .filter(|(_, inst)| {
                        let is_standard_debug = [Op::Line, Op::NoLine].contains(&inst.class.opcode);
                        let is_custom_debug = inst.class.opcode == Op::ExtInst
                            && inst.operands[0].unwrap_id_ref() == custom_ext_inst_set_import
                            && CustomOp::decode_from_ext_inst(inst).is_debuginfo();
                        !(is_standard_debug || is_custom_debug)
                    });

                // HACK(eddyb) to aid in pattern-matching, relevant instructions
                // are decoded to values of this `enum`. For instructions that
                // produce results, the result ID is the first `ID` value.
                #[derive(Debug)]
                enum Inst<ID> {
                    Bitcast(ID, ID),
                    CompositeExtract(ID, ID, u32),
                    InBoundsAccessChain(ID, ID, u32),
                    Store(ID, ID),
                    Load(ID, ID),
                    CopyMemory(ID, ID),
                    Call(ID, ID, SmallVec<[ID; 4]>),

                    // HACK(eddyb) this only exists for better error reporting,
                    // as `Result<Inst<...>, Op>` would only report one `Op`.
                    Unsupported(
                        // HACK(eddyb) only exists for `fmt::Debug` in case of error.
                        #[allow(dead_code)] Op,
                    ),
                }

                let taken_inst_idx_range = Cell::new(func.blocks[block_idx].instructions.len())..;

                // Take `count` instructions, advancing backwards, but returning
                // instructions in their original order (and decoded to `Inst`s).
                let mut try_rev_take = |count| {
                    let maybe_rev_insts = (0..count).map(|_| {
                        let (i, inst) = non_debug_insts.next_back()?;
                        taken_inst_idx_range.start.set(i);

                        // HACK(eddyb) avoid the logic below that assumes only ID operands
                        if inst.class.opcode == Op::CompositeExtract {
                            if let (Some(r), &[Operand::IdRef(x), Operand::LiteralBit32(i)]) =
                                (inst.result_id, &inst.operands[..])
                            {
                                return Some(Inst::CompositeExtract(r, x, i));
                            }
                        }

                        // HACK(eddyb) all instructions accepted below
                        // are expected to take no more than 4 operands,
                        // and this is easier to use than an iterator.
                        let id_operands = inst
                            .operands
                            .iter()
                            .map(|operand| operand.id_ref_any())
                            .collect::<Option<SmallVec<[_; 4]>>>()?;

                        // Decode the instruction into one of our `Inst`s.
                        Some(
                            match (inst.class.opcode, inst.result_id, &id_operands[..]) {
                                (Op::Bitcast, Some(r), &[x]) => Inst::Bitcast(r, x),
                                (Op::InBoundsAccessChain, Some(r), &[p, i]) => {
                                    if let Some(SpirvConst::Scalar(i)) =
                                        self.builder.lookup_const_by_id(i)
                                    {
                                        Inst::InBoundsAccessChain(r, p, i as u32)
                                    } else {
                                        Inst::Unsupported(inst.class.opcode)
                                    }
                                }
                                (Op::Store, None, &[p, v]) => Inst::Store(p, v),
                                (Op::Load, Some(r), &[p]) => Inst::Load(r, p),
                                (Op::CopyMemory, None, &[a, b]) => Inst::CopyMemory(a, b),
                                (Op::FunctionCall, Some(r), [f, args @ ..]) => {
                                    Inst::Call(r, *f, args.iter().copied().collect())
                                }
                                _ => Inst::Unsupported(inst.class.opcode),
                            },
                        )
                    });
                    let mut insts = maybe_rev_insts.collect::<Option<SmallVec<[_; 4]>>>()?;
                    insts.reverse();
                    Some(insts)
                };
                let fmt_args_new_call_insts = try_rev_take(3).ok_or_else(|| {
                    FormatArgsNotRecognized(
                        "fmt::Arguments::new call: ran out of instructions".into(),
                    )
                })?;
                let ((pieces_slice_ptr_id, pieces_len), (rt_args_slice_ptr_id, rt_args_count)) =
                    match fmt_args_new_call_insts[..] {
                        [
                            Inst::Call(call_ret_id, callee_id, ref call_args),
                            Inst::Store(st_dst_id, st_val_id),
                            Inst::Load(ld_val_id, ld_src_id),
                        ] if call_ret_id == st_val_id
                            && st_dst_id == ld_src_id
                            && ld_val_id == format_args_id =>
                        {
                            require_local_var(st_dst_id, "fmt::Arguments::new destination")?;

                            let Some(&(pieces_len, rt_args_count)) =
                                self.fmt_args_new_fn_ids.borrow().get(&callee_id)
                            else {
                                return Err(FormatArgsNotRecognized(
                                    "fmt::Arguments::new callee not registered".into(),
                                ));
                            };

                            match call_args[..] {
                                // `<core::fmt::Arguments>::new_v1`
                                [pieces_slice_ptr_id, rt_args_slice_ptr_id] => (
                                    (pieces_slice_ptr_id, pieces_len),
                                    (Some(rt_args_slice_ptr_id), rt_args_count),
                                ),

                                // `<core::fmt::Arguments>::new_const`
                                [pieces_slice_ptr_id] if rt_args_count == 0 => {
                                    ((pieces_slice_ptr_id, pieces_len), (None, rt_args_count))
                                }

                                _ => {
                                    return Err(FormatArgsNotRecognized(
                                        "fmt::Arguments::new call args".into(),
                                    ));
                                }
                            }
                        }
                        _ => {
                            // HACK(eddyb) this gathers more context before reporting.
                            let mut insts = fmt_args_new_call_insts;
                            insts.reverse();
                            while let Some(extra_inst) = try_rev_take(1) {
                                insts.extend(extra_inst);
                                if insts.len() >= 32 {
                                    break;
                                }
                            }
                            insts.reverse();

                            return Err(FormatArgsNotRecognized(format!(
                                "fmt::Arguments::new call sequence ({insts:?})",
                            )));
                        }
                    };

                // HACK(eddyb) this is the worst part: if we do have runtime
                // arguments (from e.g. new `assert!`s being added to `core`),
                // we have to confirm their many instructions for removal.
                if rt_args_count > 0 {
                    let rt_args_array_ptr_id = rt_args_slice_ptr_id.unwrap();

                    // Each runtime argument has 4 instructions to call one of
                    // the `fmt::rt::Argument::new_*` functions (and temporarily
                    // store its result), and 4 instructions to copy it into
                    // the appropriate slot in the array. The groups of 4 and 4
                    // instructions, for all runtime args, are each separate.
                    let copies_to_rt_args_array =
                        try_rev_take(rt_args_count * 4).ok_or_else(|| {
                            FormatArgsNotRecognized(
                                "[fmt::rt::Argument; N] copies: ran out of instructions".into(),
                            )
                        })?;
                    let copies_to_rt_args_array = copies_to_rt_args_array.chunks(4);
                    let rt_arg_new_calls = try_rev_take(rt_args_count * 4).ok_or_else(|| {
                        FormatArgsNotRecognized(
                            "fmt::rt::Argument::new calls: ran out of instructions".into(),
                        )
                    })?;
                    let rt_arg_new_calls = rt_arg_new_calls.chunks(4);

                    for (rt_arg_idx, (rt_arg_new_call_insts, copy_to_rt_args_array_insts)) in
                        rt_arg_new_calls.zip(copies_to_rt_args_array).enumerate()
                    {
                        let call_ret_slot_ptr = match rt_arg_new_call_insts[..] {
                            [
                                Inst::Call(call_ret_id, callee_id, ref call_args),
                                Inst::InBoundsAccessChain(tmp_slot_field_ptr, tmp_slot_ptr, 0),
                                Inst::CompositeExtract(field, wrapper_newtype, 0),
                                Inst::Store(st_dst_ptr, st_val),
                            ] if wrapper_newtype == call_ret_id
                                && (st_dst_ptr, st_val) == (tmp_slot_field_ptr, field) =>
                            {
                                self.fmt_rt_arg_new_fn_ids_to_ty_and_spec
                                    .borrow()
                                    .get(&callee_id)
                                    .and_then(|&(ty, spec)| match call_args[..] {
                                        [x] => {
                                            decoded_format_args
                                                .ref_arg_ids_with_ty_and_spec
                                                .push((x, ty, spec));
                                            Some(tmp_slot_ptr)
                                        }
                                        _ => None,
                                    })
                            }
                            _ => None,
                        }
                        .ok_or_else(|| {
                            FormatArgsNotRecognized(format!(
                                "fmt::rt::Argument::new call sequence ({rt_arg_new_call_insts:?})"
                            ))
                        })?;

                        match copy_to_rt_args_array_insts[..] {
                            [
                                Inst::InBoundsAccessChain(array_slot, array_base, array_idx),
                                Inst::InBoundsAccessChain(dst_field_ptr, dst_base_ptr, 0),
                                Inst::InBoundsAccessChain(src_field_ptr, src_base_ptr, 0),
                                Inst::CopyMemory(copy_dst, copy_src),
                            ] if array_base == rt_args_array_ptr_id
                                && array_idx as usize == rt_arg_idx
                                && dst_base_ptr == array_slot
                                && src_base_ptr == call_ret_slot_ptr
                                && (copy_dst, copy_src) == (dst_field_ptr, src_field_ptr) => {}
                            _ => {
                                return Err(FormatArgsNotRecognized(format!(
                                    "[fmt::rt::Argument; N] copies sequence ({copy_to_rt_args_array_insts:?})"
                                )));
                            }
                        }
                    }
                }

                // If the `pieces: &[&str]` slice needs a bitcast, it'll be here.
                let pieces_slice_ptr_id = match try_rev_take(1).as_deref() {
                    Some(&[Inst::Bitcast(out_id, in_id)]) if out_id == pieces_slice_ptr_id => in_id,
                    _ => pieces_slice_ptr_id,
                };
                decoded_format_args.const_pieces =
                    const_slice_as_elem_ids(pieces_slice_ptr_id, pieces_len).and_then(
                        |piece_ids| {
                            piece_ids
                                .iter()
                                .map(|&piece_id| {
                                    match self.builder.lookup_const_by_id(piece_id)? {
                                        SpirvConst::Composite(piece) => {
                                            const_str_as_utf8(piece.try_into().ok()?)
                                        }
                                        _ => None,
                                    }
                                })
                                .collect::<Option<_>>()
                        },
                    );

                // Keep all instructions up to (but not including) the last one
                // confirmed above to be the first instruction of `format_args!`.
                func.blocks[block_idx]
                    .instructions
                    .truncate(taken_inst_idx_range.start.get());

                Ok(decoded_format_args)
            };

            let mut debug_printf_args = SmallVec::<[_; 2]>::new();
            let message = match try_decode_and_remove_format_args() {
                Ok(DecodedFormatArgs {
                    const_pieces,
                    ref_arg_ids_with_ty_and_spec,
                }) => {
                    match const_pieces {
                        Some(const_pieces) => {
                            const_pieces
                                .into_iter()
                                .map(|s| Cow::Owned(s.replace('%', "%%")))
                                .interleave(ref_arg_ids_with_ty_and_spec.iter().map(
                                    |&(ref_id, ty, spec)| {
                                        use rustc_target::abi::{
                                            Float::*, Integer::*, Primitive::*,
                                        };

                                        let layout = self.layout_of(ty);

                                        let scalar = match layout.backend_repr {
                                            BackendRepr::Scalar(scalar) => Some(scalar.primitive()),
                                            _ => None,
                                        };
                                        let debug_printf_fmt = match (spec, scalar) {
                                            // FIXME(eddyb) support more of these,
                                            // potentially recursing to print ADTs.
                                            (' ' | '?', Some(Int(I32, false))) => "%u",
                                            ('x', Some(Int(I32, false))) => "%x",
                                            (' ' | '?', Some(Int(I32, true))) => "%i",
                                            (' ' | '?', Some(Float(F32))) => "%f",

                                            _ => "",
                                        };

                                        if debug_printf_fmt.is_empty() {
                                            return Cow::Owned(
                                                format!("{{/* unprintable {ty} */:{spec}}}")
                                                    .replace('%', "%%"),
                                            );
                                        }

                                        let spirv_type = layout.spirv_type(self.span(), self);
                                        debug_printf_args.push(
                                            self.emit()
                                                .load(spirv_type, None, ref_id, None, [])
                                                .unwrap()
                                                .with_type(spirv_type),
                                        );
                                        Cow::Borrowed(debug_printf_fmt)
                                    },
                                ))
                                .collect::<String>()
                        }
                        None => "<unknown message>".into(),
                    }
                }

                Err(FormatArgsNotRecognized(step)) => {
                    if let Some(current_span) = self.current_span {
                        let mut warn = self.tcx.dcx().struct_span_warn(
                            current_span,
                            "failed to find and remove `format_args!` construction for this `panic!`",
                        );

                        warn.note(
                            "compilation may later fail due to leftover `format_args!` internals",
                        );

                        if self.tcx.sess.opts.unstable_opts.inline_mir != Some(false) {
                            warn.note("missing `-Zinline-mir=off` flag (should've been set by `spirv-builder`)")
                                .help("check `.cargo` and environment variables for potential overrides")
                                .help("(or, if not using `spirv-builder` at all, add the flag manually)");
                        } else {
                            warn.note(format!("[RUST-GPU BUG] bailed from {step}"));
                        }

                        warn.emit();
                    }
                    "<unknown message> (failed to find/decode `format_args!` expansion)".into()
                }
            };

            // HACK(eddyb) redirect any possible panic call to an abort, to avoid
            // needing to materialize `&core::panic::Location` or `format_args!`.
            self.abort_with_kind_and_message_debug_printf("panic", message, debug_printf_args);
            self.undef(result_type)
        } else if let Some(mode) = buffer_load_intrinsic {
            self.codegen_buffer_load_intrinsic(result_type, args, mode)
        } else if let Some(mode) = buffer_store_intrinsic {
            self.codegen_buffer_store_intrinsic(args, mode);

            let void_ty = SpirvType::Void.def(rustc_span::DUMMY_SP, self);
            SpirvValue {
                kind: SpirvValueKind::IllegalTypeUsed(void_ty),
                ty: void_ty,
            }
        } else {
            let args = args.iter().map(|arg| arg.def(self)).collect::<Vec<_>>();
            self.emit()
                .function_call(result_type, None, callee_val, args)
                .unwrap()
                .with_type(result_type)
        }
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }

    fn apply_attrs_to_cleanup_callsite(&mut self, _llret: Self::Value) {
        // Ignore
    }
}
