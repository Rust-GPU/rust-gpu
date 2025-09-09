// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::CodegenCx;
use crate::builder_spirv::{SpirvConst, SpirvValue, SpirvValueKind};
use crate::spirv_type::SpirvType;
use itertools::Itertools as _;
use rspirv::spirv::Word;
use rustc_abi::{self as abi, AddressSpace, Float, HasDataLayout, Integer, Primitive, Size};
use rustc_codegen_ssa::traits::{
    BaseTypeCodegenMethods as _, ConstCodegenMethods, MiscCodegenMethods,
};
use rustc_middle::mir::interpret::{AllocError, ConstAllocation, GlobalAlloc, Scalar, alloc_range};
use rustc_span::{DUMMY_SP, Span};

impl<'tcx> CodegenCx<'tcx> {
    pub fn def_constant(&self, ty: Word, val: SpirvConst<'_, 'tcx>) -> SpirvValue {
        self.builder.def_constant_cx(ty, val, self)
    }

    pub fn constant_u8(&self, span: Span, val: u8) -> SpirvValue {
        self.constant_int_from_native_unsigned(span, val)
    }

    pub fn constant_i8(&self, span: Span, val: i8) -> SpirvValue {
        self.constant_int_from_native_signed(span, val)
    }

    pub fn constant_i16(&self, span: Span, val: i16) -> SpirvValue {
        self.constant_int_from_native_signed(span, val)
    }

    pub fn constant_u16(&self, span: Span, val: u16) -> SpirvValue {
        self.constant_int_from_native_unsigned(span, val)
    }

    pub fn constant_i32(&self, span: Span, val: i32) -> SpirvValue {
        self.constant_int_from_native_signed(span, val)
    }

    pub fn constant_u32(&self, span: Span, val: u32) -> SpirvValue {
        self.constant_int_from_native_unsigned(span, val)
    }

    pub fn constant_i64(&self, span: Span, val: i64) -> SpirvValue {
        self.constant_int_from_native_signed(span, val)
    }

    pub fn constant_u64(&self, span: Span, val: u64) -> SpirvValue {
        self.constant_int_from_native_unsigned(span, val)
    }

    fn constant_int_from_native_unsigned(&self, span: Span, val: impl Into<u128>) -> SpirvValue {
        let size = Size::from_bytes(std::mem::size_of_val(&val));
        let ty = SpirvType::Integer(size.bits() as u32, false).def(span, self);
        self.constant_int(ty, val.into())
    }

    fn constant_int_from_native_signed(&self, span: Span, val: impl Into<i128>) -> SpirvValue {
        let size = Size::from_bytes(std::mem::size_of_val(&val));
        let ty = SpirvType::Integer(size.bits() as u32, true).def(span, self);
        self.constant_int(ty, val.into() as u128)
    }

    pub fn constant_int(&self, ty: Word, val: u128) -> SpirvValue {
        self.def_constant(ty, SpirvConst::Scalar(val))
    }

    pub fn constant_f32(&self, span: Span, val: f32) -> SpirvValue {
        let ty = SpirvType::Float(32).def(span, self);
        self.def_constant(ty, SpirvConst::Scalar(val.to_bits().into()))
    }

    pub fn constant_f64(&self, span: Span, val: f64) -> SpirvValue {
        let ty = SpirvType::Float(64).def(span, self);
        self.def_constant(ty, SpirvConst::Scalar(val.to_bits().into()))
    }

    pub fn constant_float(&self, ty: Word, val: f64) -> SpirvValue {
        match self.lookup_type(ty) {
            // FIXME(eddyb) use `rustc_apfloat` to support all float sizes.
            SpirvType::Float(32) => {
                self.def_constant(ty, SpirvConst::Scalar((val as f32).to_bits().into()))
            }
            SpirvType::Float(64) => self.def_constant(ty, SpirvConst::Scalar(val.to_bits().into())),
            other => self.tcx.dcx().fatal(format!(
                "constant_float does not support type {}",
                other.debug(ty, self)
            )),
        }
    }

    pub fn constant_bool(&self, span: Span, val: bool) -> SpirvValue {
        let ty = SpirvType::Bool.def(span, self);
        self.def_constant(ty, SpirvConst::Scalar(val as u128))
    }

    pub fn constant_composite(&self, ty: Word, fields: impl Iterator<Item = Word>) -> SpirvValue {
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        self.def_constant(ty, SpirvConst::Composite(&fields.collect::<Vec<_>>()))
    }

    pub fn constant_null(&self, ty: Word) -> SpirvValue {
        self.def_constant(ty, SpirvConst::Null)
    }

    pub fn undef(&self, ty: Word) -> SpirvValue {
        self.def_constant(ty, SpirvConst::Undef)
    }

    // FIXME(eddyb) this is much less efficient than e.g. LLVM's byte strings.
    pub fn const_bytes(&self, bytes: &[u8]) -> SpirvValue {
        // FIXME(eddyb) `type_iN` methods return *unsigned* integer types,
        // which is probably desirable, and it also suggests that maybe the
        // SPIR-T-like `sN`/`uN` convention should be used for SPIR-V types,
        // instead of Rust-like `iN`/`uN`, to avoid some of the ambiguity.
        let u8_ty = self.type_i8();

        self.constant_composite(
            self.type_array(u8_ty, bytes.len().try_into().unwrap()),
            bytes.iter().map(|&b| self.const_u8(b).def_cx(self)),
        )
    }
}

impl ConstCodegenMethods for CodegenCx<'_> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        self.constant_null(t)
    }
    fn const_undef(&self, ty: Self::Type) -> Self::Value {
        self.undef(ty)
    }
    fn const_poison(&self, ty: Self::Type) -> Self::Value {
        // No distinction between undef and poison.
        self.const_undef(ty)
    }
    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        self.constant_int(t, i as u128)
    }
    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        self.constant_int(t, i.into())
    }
    fn const_uint_big(&self, t: Self::Type, i: u128) -> Self::Value {
        self.constant_int(t, i)
    }
    fn const_bool(&self, val: bool) -> Self::Value {
        self.constant_bool(DUMMY_SP, val)
    }
    fn const_i8(&self, i: i8) -> Self::Value {
        self.constant_i8(DUMMY_SP, i)
    }
    fn const_i16(&self, i: i16) -> Self::Value {
        self.constant_i16(DUMMY_SP, i)
    }
    fn const_i32(&self, i: i32) -> Self::Value {
        self.constant_i32(DUMMY_SP, i)
    }
    fn const_u32(&self, i: u32) -> Self::Value {
        self.constant_u32(DUMMY_SP, i)
    }
    fn const_u64(&self, i: u64) -> Self::Value {
        self.constant_u64(DUMMY_SP, i)
    }
    fn const_u128(&self, i: u128) -> Self::Value {
        let ty = SpirvType::Integer(128, false).def(DUMMY_SP, self);
        self.const_uint_big(ty, i)
    }
    fn const_usize(&self, i: u64) -> Self::Value {
        let ptr_size = self.tcx.data_layout.pointer_size.bits() as u32;
        let t = SpirvType::Integer(ptr_size, false).def(DUMMY_SP, self);
        self.constant_int(t, i.into())
    }
    fn const_u8(&self, i: u8) -> Self::Value {
        self.constant_u8(DUMMY_SP, i)
    }
    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        self.constant_float(t, val)
    }

    // FIXME(eddyb) remove this upstream (obsoleted by `const_data_from_alloc`).
    fn const_str(&self, _s: &str) -> (Self::Value, Self::Value) {
        unreachable!("`const_str` is never called by `rustc_codegen_ssa`");
    }
    fn const_struct(&self, elts: &[Self::Value], packed: bool) -> Self::Value {
        // Presumably this will get bitcasted to the right type?
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        let field_types = elts.iter().map(|f| f.ty).collect::<Vec<_>>();
        let (field_offsets, size, align) =
            crate::abi::auto_struct_layout(self, &field_types, packed);
        let struct_ty = SpirvType::Adt {
            def_id: None,
            size,
            align,
            field_types: &field_types,
            field_offsets: &field_offsets,
            field_names: None,
        }
        .def(DUMMY_SP, self);
        self.constant_composite(struct_ty, elts.iter().map(|f| f.def_cx(self)))
    }
    fn const_vector(&self, elts: &[Self::Value]) -> Self::Value {
        let vector_ty = SpirvType::simd_vector(
            self,
            DUMMY_SP,
            self.lookup_type(elts[0].ty),
            elts.len() as u32,
        )
        .def(DUMMY_SP, self);
        self.constant_composite(vector_ty, elts.iter().map(|elt| elt.def_cx(self)))
    }

    fn const_to_opt_uint(&self, v: Self::Value) -> Option<u64> {
        self.builder.lookup_const_scalar(v)?.try_into().ok()
    }
    // FIXME(eddyb) what's the purpose of the `sign_ext` argument, and can it
    // differ from the signedness of `v`?
    fn const_to_opt_u128(&self, v: Self::Value, _sign_ext: bool) -> Option<u128> {
        self.builder.lookup_const_scalar(v)
    }

    fn scalar_to_backend(
        &self,
        scalar: Scalar,
        layout: abi::Scalar,
        ty: Self::Type,
    ) -> Self::Value {
        match scalar {
            Scalar::Int(int) => {
                assert_eq!(int.size(), layout.primitive().size(self));
                let data = int.to_uint(int.size());

                if let Primitive::Pointer(_) = layout.primitive() {
                    self.const_bitcast(self.const_usize(data.try_into().unwrap()), ty)
                } else {
                    self.def_constant(ty, SpirvConst::Scalar(data))
                }
            }
            Scalar::Ptr(ptr, _) => {
                let (prov, offset) = ptr.into_parts();
                let alloc_id = prov.alloc_id();
                let (base_addr, _base_addr_space) = match self.tcx.global_alloc(alloc_id) {
                    GlobalAlloc::Memory(alloc) => {
                        // For ZSTs directly codegen an aligned pointer.
                        // This avoids generating a zero-sized constant value and actually needing a
                        // real address at runtime.
                        //
                        // FIXME(eddyb) see `prefer_int_const_ptrs_over_zst_statics`.
                        if alloc.inner().len() == 0 && self.prefer_int_const_ptrs_over_zst_statics()
                        {
                            assert_eq!(offset.bytes(), 0);
                            return self
                                .const_bitcast(self.const_usize(alloc.inner().align.bytes()), ty);
                        }

                        (self.static_addr_of_alloc(alloc, None), AddressSpace::DATA)
                    }
                    GlobalAlloc::Function { instance } => (
                        self.get_fn_addr(instance),
                        self.data_layout().instruction_address_space,
                    ),
                    GlobalAlloc::VTable(vty, dyn_ty) => {
                        let alloc = self
                            .tcx
                            .global_alloc(self.tcx.vtable_allocation((
                                vty,
                                dyn_ty.principal().map(|principal| {
                                    self.tcx.instantiate_bound_regions_with_erased(principal)
                                }),
                            )))
                            .unwrap_memory();
                        (self.static_addr_of_alloc(alloc, None), AddressSpace::DATA)
                    }
                    GlobalAlloc::Static(def_id) => {
                        assert!(self.tcx.is_static(def_id));
                        assert!(!self.tcx.is_thread_local_static(def_id));
                        (self.get_static(def_id), AddressSpace::DATA)
                    }
                };
                self.const_bitcast(self.const_ptr_byte_offset(base_addr, offset), ty)
            }
        }
    }

    fn const_data_from_alloc(&self, alloc: ConstAllocation<'_>) -> Self::Value {
        // HACK(eddyb) the `ConstCodegenMethods` trait no longer guarantees the
        // lifetime that `alloc` is interned for, but since it *is* interned,
        // we can cheaply recover it (see also the `ty::Lift` infrastructure).
        let alloc = self.tcx.lift(alloc).unwrap();

        SpirvValue {
            zombie_waiting_for_span: false,
            kind: SpirvValueKind::ConstDataFromAlloc {
                // HACK(eddyb) this isn't just `alloc` because that would require
                // adding a `'tcx` parameter to `SpirvValue`.
                alloc_id: self.tcx.reserve_and_set_memory_alloc(alloc),
            },
            ty: SpirvType::Void.def(DUMMY_SP, self),
        }
    }

    fn const_ptr_byte_offset(&self, ptr: Self::Value, offset: Size) -> Self::Value {
        let ptr = ptr.strip_ptrcasts();

        if offset == Size::ZERO {
            return ptr;
        }

        self.def_constant(
            ptr.ty,
            SpirvConst::PtrByteOffset {
                ptr: ptr.def_cx(self),
                offset,
            },
        )
    }
}

impl<'tcx> CodegenCx<'tcx> {
    /// `&ZST` constants can be represented in two possible ways:
    /// - integer pointer e.g. `const_bitcast(const_usize(1))` for `&()`
    ///   (in the general case, `ptr::dangling::<ZST>()`, i.e. a similar cast,
    ///   except the `1` has to be replaced by the actual `align_of::<ZST>()`)
    /// - pointer to some `static S: ZST` equivalent, i.e. `static_addr_of`
    ///   (which is wasteful/unnecessary for a ZST, as far as Rust is concerned)
    ///
    /// Currently the choice is hardcoded to the latter, instead of the Rust
    /// preference for integer pointers, but this function mainly exists to
    /// allow the logic for integer pointers to be kept, even if inactive.
    //
    // FIXME(eddyb) maybe gate this choice on whether `qptr` is enabled?
    fn prefer_int_const_ptrs_over_zst_statics(&self) -> bool {
        let _cx = self;

        false
    }

    pub fn const_bitcast(&self, val: SpirvValue, ty: Word) -> SpirvValue {
        if val.ty == ty {
            return val;
        }

        // FIXME(eddyb) also strip other kinds of bitcasts (if useful at all?).
        let val = val.strip_ptrcasts();

        if val.ty == ty {
            return val;
        }

        let val_ct_def = self.builder.lookup_const(val);

        // FIXME(eddyb) constant-fold scalar -> scalar as well, not just nulls.
        if let Some(SpirvConst::Scalar(0)) = val_ct_def
            && let SpirvType::Pointer { .. } = self.lookup_type(ty)
        {
            return self.const_null(ty);
        }

        // HACK(eddyb) special-case `const_data_from_alloc` + `static_addr_of`
        // as the old `from_const_alloc` (now `OperandRef::from_const_alloc`).
        // FIXME(eddyb) replace this with `qptr` handling of constant data.
        if let Some(SpirvConst::PtrTo {
            pointee: _,
            pointee_alloc,
        }) = val_ct_def
            && pointee_alloc.inner().mutability.is_not()
            && let SpirvType::Pointer { pointee, .. } = self.lookup_type(ty)
            && let Some(init) = self.try_read_from_const_alloc(pointee_alloc, pointee)
        {
            return self.def_constant(
                ty,
                SpirvConst::PtrTo {
                    pointee: init.def_cx(self),
                    pointee_alloc,
                },
            );
        }

        self.def_constant(ty, SpirvConst::BitCast(val.def_cx(self)))
    }

    // This function comes from `ty::layout`'s `layout_of_uncached`,
    // where it's named `scalar_unit`.
    pub fn primitive_to_scalar(&self, value: Primitive) -> abi::Scalar {
        let bits = value.size(self.data_layout()).bits();
        assert!(bits <= 128);
        abi::Scalar::Initialized {
            value,
            valid_range: abi::WrappingRange {
                start: 0,
                end: (!0 >> (128 - bits)),
            },
        }
    }

    /// Attempt to read a whole constant of type `ty` from `alloc`, but only
    /// returning that constant if its size covers the entirety of `alloc`.
    //
    // FIXME(eddyb) should this use something like `Result<_, PartialRead>`?
    // FIXME(eddyb) replace this with `qptr` handling of constant data.
    pub fn try_read_from_const_alloc(
        &self,
        alloc: ConstAllocation<'tcx>,
        ty: Word,
    ) -> Option<SpirvValue> {
        let (result, read_size) = self.read_from_const_alloc_at(alloc, ty, Size::ZERO);
        (read_size == alloc.inner().size()).then_some(result)
    }

    // HACK(eddyb) the `Size` returned is the equivalent of `size_of_val` on
    // the returned constant, i.e. `ty.sizeof()` can be either `Some(read_size)`,
    // or `None` - i.e. unsized, in which case only the returned `Size` records
    // how much was read from `alloc` to build the returned constant value.
    // FIXME(eddyb) replace this with `qptr` handling of constant data.
    #[tracing::instrument(level = "trace", skip(self), fields(ty = ?self.debug_type(ty), offset))]
    fn read_from_const_alloc_at(
        &self,
        alloc: ConstAllocation<'tcx>,
        ty: Word,
        offset: Size,
    ) -> (SpirvValue, Size) {
        let ty_def = self.lookup_type(ty);
        match ty_def {
            SpirvType::Bool
            | SpirvType::Integer(..)
            | SpirvType::Float(_)
            | SpirvType::Pointer { .. } => {
                let size = ty_def.sizeof(self).unwrap();
                let primitive = match ty_def {
                    SpirvType::Bool => Primitive::Int(Integer::fit_unsigned(0), false),
                    SpirvType::Integer(int_size, int_signedness) => Primitive::Int(
                        match int_size {
                            8 => Integer::I8,
                            16 => Integer::I16,
                            32 => Integer::I32,
                            64 => Integer::I64,
                            128 => Integer::I128,
                            other => {
                                self.tcx
                                    .dcx()
                                    .fatal(format!("invalid size for integer: {other}"));
                            }
                        },
                        int_signedness,
                    ),
                    SpirvType::Float(float_size) => Primitive::Float(match float_size {
                        16 => Float::F16,
                        32 => Float::F32,
                        64 => Float::F64,
                        128 => Float::F128,
                        other => {
                            self.tcx
                                .dcx()
                                .fatal(format!("invalid size for float: {other}"));
                        }
                    }),
                    SpirvType::Pointer { .. } => Primitive::Pointer(AddressSpace::DATA),
                    _ => unreachable!(),
                };

                let range = alloc_range(offset, size);
                let read_provenance = matches!(primitive, Primitive::Pointer(_));

                let mut primitive = primitive;
                let mut read_result = alloc.inner().read_scalar(self, range, read_provenance);

                // HACK(eddyb) while reading a pointer as an integer will fail,
                // the pointer itself can be read as a pointer, and then passed
                // to `scalar_to_backend`, which will `const_bitcast` it to `ty`.
                if read_result.is_err()
                    && !read_provenance
                    && let read_ptr_result @ Ok(Scalar::Ptr(ptr, _)) = alloc
                        .inner()
                        .read_scalar(self, range, /* read_provenance */ true)
                {
                    let (prov, _offset) = ptr.into_parts();
                    primitive = Primitive::Pointer(
                        self.tcx.global_alloc(prov.alloc_id()).address_space(self),
                    );
                    read_result = read_ptr_result;
                }

                let scalar_or_zombie = match read_result {
                    Ok(scalar) => {
                        Ok(self.scalar_to_backend(scalar, self.primitive_to_scalar(primitive), ty))
                    }

                    // FIXME(eddyb) could some of these use e.g. `const_bitcast`?
                    // (or, in general, assembling one constant out of several)
                    Err(err) => match err {
                        // The scalar is only `undef` if the entire byte range
                        // it covers is completely uninitialized - all other
                        // failure modes of `read_scalar` are various errors.
                        AllocError::InvalidUninitBytes(_) => {
                            let uninit_range = alloc
                                .inner()
                                .init_mask()
                                .is_range_initialized(range)
                                .unwrap_err();
                            let uninit_size = {
                                let [start, end] = [uninit_range.start, uninit_range.end()]
                                    .map(|x| x.clamp(range.start, range.end()));
                                end - start
                            };
                            if uninit_size == size {
                                Ok(self.undef(ty))
                            } else {
                                Err(format!(
                                    "overlaps {} uninitialized bytes",
                                    uninit_size.bytes()
                                ))
                            }
                        }
                        AllocError::ReadPointerAsInt(_) => Err("overlaps pointer bytes".into()),
                        AllocError::ReadPartialPointer(_) => {
                            Err("partially overlaps another pointer".into())
                        }

                        // HACK(eddyb) these should never happen when using
                        // `read_scalar`, but better not outright crash.
                        AllocError::ScalarSizeMismatch(_)
                        | AllocError::OverwritePartialPointer(_) => {
                            Err(format!("unrecognized `AllocError::{err:?}`"))
                        }
                    },
                };
                let result = scalar_or_zombie.unwrap_or_else(|reason| {
                    let result = self.undef(ty);
                    self.zombie_no_span(
                        result.def_cx(self),
                        &format!("unsupported `{}` constant: {reason}", self.debug_type(ty),),
                    );
                    result
                });
                (result, size)
            }
            SpirvType::Adt {
                field_types,
                field_offsets,
                ..
            } => {
                // HACK(eddyb) this accounts for unsized `struct`s, and allows
                // detecting gaps *only* at the end of the type, but is cheap.
                let mut tail_read_range = ..Size::ZERO;
                let result = self.constant_composite(
                    ty,
                    field_types
                        .iter()
                        .zip_eq(field_offsets.iter())
                        .map(|(&f_ty, &f_offset)| {
                            let (f, f_size) =
                                self.read_from_const_alloc_at(alloc, f_ty, offset + f_offset);
                            tail_read_range.end =
                                tail_read_range.end.max(offset + f_offset + f_size);
                            f.def_cx(self)
                        }),
                );

                let ty_size = ty_def.sizeof(self);

                // HACK(eddyb) catch non-padding holes in e.g. `enum` values.
                if let Some(ty_size) = ty_size
                    && let Some(tail_gap) = (ty_size.bytes())
                        .checked_sub(tail_read_range.end.align_to(ty_def.alignof(self)).bytes())
                    && tail_gap > 0
                {
                    self.zombie_no_span(
                        result.def_cx(self),
                        &format!(
                            "undersized `{}` constant (at least {tail_gap} bytes may be missing)",
                            self.debug_type(ty)
                        ),
                    );
                }

                (result, ty_size.unwrap_or(tail_read_range.end))
            }
            SpirvType::Vector { element, .. }
            | SpirvType::Matrix { element, .. }
            | SpirvType::Array { element, .. }
            | SpirvType::RuntimeArray { element, .. } => {
                let stride = match ty_def {
                    SpirvType::Array { stride, .. } | SpirvType::RuntimeArray { stride, .. } => {
                        stride
                    }
                    _ => self.lookup_type(element).sizeof(self).unwrap(),
                };

                let count = match ty_def {
                    SpirvType::Vector { count, .. } | SpirvType::Matrix { count, .. } => {
                        u64::from(count)
                    }
                    SpirvType::Array { count, .. } => {
                        u64::try_from(self.builder.lookup_const_scalar(count).unwrap()).unwrap()
                    }
                    SpirvType::RuntimeArray { .. } => (alloc.inner().size() - offset)
                        .bytes()
                        .checked_div(stride.bytes())
                        .unwrap_or(0),
                    _ => unreachable!(),
                };

                let result = self.constant_composite(
                    ty,
                    (0..count).map(|i| {
                        let (e, e_size) =
                            self.read_from_const_alloc_at(alloc, element, offset + i * stride);
                        assert_eq!(e_size, stride);
                        e.def_cx(self)
                    }),
                );

                // HACK(eddyb) `align_to` can only cause an increase for `Vector`,
                // because its `size`/`align` are rounded up to a power of two
                // (for now, at least, even if eventually that should go away).
                let read_size = (count * stride).align_to(ty_def.alignof(self));

                if let Some(ty_size) = ty_def.sizeof(self) {
                    assert_eq!(read_size, ty_size);
                }

                if let SpirvType::RuntimeArray { .. } = ty_def {
                    // FIXME(eddyb) values of this type should never be created,
                    // the only reasonable encoding of e.g. `&str` consts should
                    // be `&[u8; N]` consts, with the `static_addr_of` pointer
                    // (*not* the value it points to) cast to `&str`, afterwards.
                    self.zombie_no_span(
                        result.def_cx(self),
                        &format!("unsupported unsized `{}` constant", self.debug_type(ty)),
                    );
                }

                (result, read_size)
            }

            SpirvType::Void
            | SpirvType::Function { .. }
            | SpirvType::Image { .. }
            | SpirvType::Sampler
            | SpirvType::SampledImage { .. }
            | SpirvType::InterfaceBlock { .. }
            | SpirvType::AccelerationStructureKhr
            | SpirvType::RayQueryKhr => {
                let result = self.undef(ty);
                self.zombie_no_span(
                    result.def_cx(self),
                    &format!(
                        "cannot reinterpret Rust constant data as a `{}` value",
                        self.debug_type(ty)
                    ),
                );
                (result, ty_def.sizeof(self).unwrap_or(Size::ZERO))
            }
        }
    }

    // HACK(eddyb) this copies `rustc_codegen_llvm::consts::const_alloc_to_llvm`,
    // almost exactly, with only references to LLVM (in names and comments), and
    // types (`CodegenCx`, `Value`, etc.), adjusted to fit `rustc_codegen_spirv`.
    //
    // FIXME(eddyb) this should be in `rustc_codegen_ssa`, perhaps requiring new
    // helpers/heuristics around the chunks of plain bytes/undef, but removing
    // the need for backends to implement `alloc.provenance().ptrs()` lowering.
    //
    pub(crate) fn const_alloc_to_backend(
        &self,
        alloc: ConstAllocation<'_>,
        is_static: bool,
    ) -> SpirvValue {
        // HACK(eddyb) `use`s from `rustc_codegen_llvm::consts`.
        use rustc_abi::{Scalar, WrappingRange};
        use rustc_middle::mir::interpret::{
            Allocation, InitChunk, Pointer, Scalar as InterpScalar, read_target_uint,
        };
        use std::ops::Range;

        let alloc = alloc.inner();
        // We expect that callers of const_alloc_to_backend will instead directly codegen a pointer or
        // integer for any &ZST where the ZST is a constant (i.e. not a static). We should never be
        // producing empty global vars as they're just adding noise to binaries and forcing less
        // optimal codegen.
        //
        // Statics have a guaranteed meaningful address so it's less clear that we want to do
        // something like this; it's also harder.
        //
        // FIXME(eddyb) see `prefer_int_const_ptrs_over_zst_statics`.
        if !is_static && self.prefer_int_const_ptrs_over_zst_statics() {
            assert!(alloc.len() != 0);
        }
        let mut llvals = Vec::with_capacity(alloc.provenance().ptrs().len() + 1);
        let dl = self.data_layout();
        let pointer_size = dl.pointer_size.bytes() as usize;

        // Note: this function may call `inspect_with_uninit_and_ptr_outside_interpreter`, so `range`
        // must be within the bounds of `alloc` and not contain or overlap a pointer provenance.
        fn append_chunks_of_init_and_uninit_bytes(
            llvals: &mut Vec<SpirvValue>,
            cx: &CodegenCx<'_>,
            alloc: &Allocation,
            range: Range<usize>,
        ) {
            let chunks = alloc.init_mask().range_as_init_chunks(range.clone().into());

            let chunk_to_llval = move |chunk| match chunk {
                InitChunk::Init(range) => {
                    let range = (range.start.bytes() as usize)..(range.end.bytes() as usize);
                    let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(range);
                    cx.const_bytes(bytes)
                }
                InitChunk::Uninit(range) => {
                    let len = range.end.bytes() - range.start.bytes();
                    cx.const_undef(cx.type_array(cx.type_i8(), len))
                }
            };

            // Generating partially-uninit consts is limited to small numbers of chunks,
            // to avoid the cost of generating large complex const expressions.
            // For example, `[(u32, u8); 1024 * 1024]` contains uninit padding in each element, and
            // would result in `{ [5 x i8] zeroinitializer, [3 x i8] undef, ...repeat 1M times... }`.
            let max = cx.sess().opts.unstable_opts.uninit_const_chunk_threshold;
            let allow_uninit_chunks = chunks.clone().take(max.saturating_add(1)).count() <= max;

            if allow_uninit_chunks {
                llvals.extend(chunks.map(chunk_to_llval));
            } else {
                // If this allocation contains any uninit bytes, codegen as if it was initialized
                // (using some arbitrary value for uninit bytes).
                let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(range);
                llvals.push(cx.const_bytes(bytes));
            }
        }

        let mut next_offset = 0;
        for &(offset, prov) in alloc.provenance().ptrs().iter() {
            let offset = offset.bytes();
            assert_eq!(offset as usize as u64, offset);
            let offset = offset as usize;
            if offset > next_offset {
                // This `inspect` is okay since we have checked that there is no provenance, it
                // is within the bounds of the allocation, and it doesn't affect interpreter execution
                // (we inspect the result after interpreter execution).
                append_chunks_of_init_and_uninit_bytes(
                    &mut llvals,
                    self,
                    alloc,
                    next_offset..offset,
                );
            }
            let ptr_offset = read_target_uint(
                dl.endian,
                // This `inspect` is okay since it is within the bounds of the allocation, it doesn't
                // affect interpreter execution (we inspect the result after interpreter execution),
                // and we properly interpret the provenance as a relocation pointer offset.
                alloc.inspect_with_uninit_and_ptr_outside_interpreter(
                    offset..(offset + pointer_size),
                ),
            )
            .expect("const_alloc_to_backend: could not read relocation pointer")
                as u64;

            let address_space = self.tcx.global_alloc(prov.alloc_id()).address_space(self);

            llvals.push(self.scalar_to_backend(
                InterpScalar::from_pointer(
                    Pointer::new(prov, Size::from_bytes(ptr_offset)),
                    &self.tcx,
                ),
                Scalar::Initialized {
                    value: Primitive::Pointer(address_space),
                    valid_range: WrappingRange::full(dl.pointer_size),
                },
                self.type_ptr_ext(address_space),
            ));
            next_offset = offset + pointer_size;
        }
        if alloc.len() >= next_offset {
            let range = next_offset..alloc.len();
            // This `inspect` is okay since we have check that it is after all provenance, it is
            // within the bounds of the allocation, and it doesn't affect interpreter execution (we
            // inspect the result after interpreter execution).
            append_chunks_of_init_and_uninit_bytes(&mut llvals, self, alloc, range);
        }

        // NOTE(eddyb) original comment below, but similar reasoning might apply
        // to other backends - also, the struct is only a concatenation gadget.
        //
        // Avoid wrapping in a struct if there is only a single value. This ensures
        // that LLVM is able to perform the string merging optimization if the constant
        // is a valid C string. LLVM only considers bare arrays for this optimization,
        // not arrays wrapped in a struct. LLVM handles this at:
        // https://github.com/rust-lang/llvm-project/blob/acaea3d2bb8f351b740db7ebce7d7a40b9e21488/llvm/lib/Target/TargetLoweringObjectFile.cpp#L249-L280
        if let &[data] = &*llvals {
            data
        } else {
            self.const_struct(&llvals, true)
        }
    }
}
