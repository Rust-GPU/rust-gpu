// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{SpirvConst, SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_abi::{self as abi, AddressSpace, Float, HasDataLayout, Integer, Primitive, Size};
use rustc_codegen_ssa::traits::{ConstCodegenMethods, MiscCodegenMethods, StaticCodegenMethods};
use rustc_middle::bug;
use rustc_middle::mir::interpret::{ConstAllocation, GlobalAlloc, Scalar, alloc_range};
use rustc_middle::ty::layout::LayoutOf;
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

    fn const_str(&self, s: &str) -> (Self::Value, Self::Value) {
        let len = s.len();
        let str_ty = self
            .layout_of(self.tcx.types.str_)
            .spirv_type(DUMMY_SP, self);
        (
            self.def_constant(
                self.type_ptr_to(str_ty),
                SpirvConst::PtrTo {
                    pointee: self
                        .constant_composite(
                            str_ty,
                            s.bytes().map(|b| self.const_u8(b).def_cx(self)),
                        )
                        .def_cx(self),
                },
            ),
            self.const_usize(len as u64),
        )
    }
    fn const_struct(&self, elts: &[Self::Value], _packed: bool) -> Self::Value {
        // Presumably this will get bitcasted to the right type?
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        let field_types = elts.iter().map(|f| f.ty).collect::<Vec<_>>();
        let (field_offsets, size, align) = crate::abi::auto_struct_layout(self, &field_types);
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
        let vector_ty = SpirvType::Vector {
            element: elts[0].ty,
            count: elts.len() as u32,
        }
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
                    if data == 0 {
                        self.constant_null(ty)
                    } else {
                        let result = self.undef(ty);
                        self.zombie_no_span(
                            result.def_cx(self),
                            "pointer has non-null integer address",
                        );
                        result
                    }
                } else {
                    self.def_constant(ty, SpirvConst::Scalar(data))
                }
            }
            Scalar::Ptr(ptr, _) => {
                let (prov, offset) = ptr.into_parts();
                let alloc_id = prov.alloc_id();
                let (base_addr, _base_addr_space) = match self.tcx.global_alloc(alloc_id) {
                    GlobalAlloc::Memory(alloc) => {
                        let pointee = match self.lookup_type(ty) {
                            SpirvType::Pointer { pointee } => pointee,
                            other => self.tcx.dcx().fatal(format!(
                                "GlobalAlloc::Memory type not implemented: {}",
                                other.debug(ty, self)
                            )),
                        };
                        let init = self.create_const_alloc(alloc, pointee);
                        let value = self.static_addr_of(init, alloc.inner().align, None);
                        (value, AddressSpace::DATA)
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
                        let pointee = match self.lookup_type(ty) {
                            SpirvType::Pointer { pointee } => pointee,
                            other => self.tcx.dcx().fatal(format!(
                                "GlobalAlloc::VTable type not implemented: {}",
                                other.debug(ty, self)
                            )),
                        };
                        let init = self.create_const_alloc(alloc, pointee);
                        let value = self.static_addr_of(init, alloc.inner().align, None);
                        (value, AddressSpace::DATA)
                    }
                    GlobalAlloc::Static(def_id) => {
                        assert!(self.tcx.is_static(def_id));
                        assert!(!self.tcx.is_thread_local_static(def_id));
                        (self.get_static(def_id), AddressSpace::DATA)
                    }
                };
                let value = if offset.bytes() == 0 {
                    base_addr
                } else {
                    self.tcx
                        .dcx()
                        .fatal("Non-zero scalar_to_backend ptr.offset not supported")
                    // let offset = self.constant_bit64(ptr.offset.bytes());
                    // self.gep(base_addr, once(offset))
                };
                if let Primitive::Pointer(_) = layout.primitive() {
                    assert_ty_eq!(self, value.ty, ty);
                    value
                } else {
                    self.tcx
                        .dcx()
                        .fatal("Non-pointer-typed scalar_to_backend Scalar::Ptr not supported");
                    // unsafe { llvm::LLVMConstPtrToInt(llval, llty) }
                }
            }
        }
    }

    // HACK(eddyb) this uses a symbolic `ConstDataFromAlloc`, to allow deferring
    // the actual value generation until after a pointer to this value is cast
    // to its final type (e.g. that will be loaded as).
    // FIXME(eddyb) replace this with `qptr` handling of constant data.
    fn const_data_from_alloc(&self, alloc: ConstAllocation<'_>) -> Self::Value {
        // HACK(eddyb) the `ConstCodegenMethods` trait no longer guarantees the
        // lifetime that `alloc` is interned for, but since it *is* interned,
        // we can cheaply recover it (see also the `ty::Lift` infrastructure).
        let alloc = self.tcx.lift(alloc).unwrap();

        let void_type = SpirvType::Void.def(DUMMY_SP, self);
        self.def_constant(void_type, SpirvConst::ConstDataFromAlloc(alloc))
    }

    fn const_ptr_byte_offset(&self, val: Self::Value, offset: Size) -> Self::Value {
        if offset == Size::ZERO {
            val
        } else {
            // FIXME(eddyb) implement via `OpSpecConstantOp`.
            // FIXME(eddyb) this zombies the original value without creating a new one.
            let result = val;
            self.zombie_no_span(result.def_cx(self), "const_ptr_byte_offset");
            result
        }
    }
}

impl<'tcx> CodegenCx<'tcx> {
    pub fn const_bitcast(&self, val: SpirvValue, ty: Word) -> SpirvValue {
        // HACK(eddyb) special-case `const_data_from_alloc` + `static_addr_of`
        // as the old `from_const_alloc` (now `OperandRef::from_const_alloc`).
        if let SpirvValueKind::IllegalConst(_) = val.kind {
            if let Some(SpirvConst::PtrTo { pointee }) = self.builder.lookup_const(val) {
                if let Some(SpirvConst::ConstDataFromAlloc(alloc)) =
                    self.builder.lookup_const_by_id(pointee)
                {
                    if let SpirvType::Pointer { pointee } = self.lookup_type(ty) {
                        let mut offset = Size::ZERO;
                        let init = self.read_from_const_alloc(alloc, &mut offset, pointee);
                        return self.static_addr_of(init, alloc.inner().align, None);
                    }
                }
            }
        }

        if val.ty == ty {
            val
        } else {
            // FIXME(eddyb) implement via `OpSpecConstantOp`.
            // FIXME(eddyb) this zombies the original value without creating a new one.
            let result = val.def_cx(self).with_type(ty);
            self.zombie_no_span(result.def_cx(self), "const_bitcast");
            result
        }
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

    pub fn create_const_alloc(&self, alloc: ConstAllocation<'tcx>, ty: Word) -> SpirvValue {
        tracing::trace!(
            "Creating const alloc of type {} with {} bytes",
            self.debug_type(ty),
            alloc.inner().len()
        );
        let mut offset = Size::ZERO;
        let result = self.read_from_const_alloc(alloc, &mut offset, ty);
        assert_eq!(
            offset.bytes_usize(),
            alloc.inner().len(),
            "create_const_alloc must consume all bytes of an Allocation"
        );
        tracing::trace!("Done creating alloc of type {}", self.debug_type(ty));
        result
    }

    fn read_from_const_alloc(
        &self,
        alloc: ConstAllocation<'tcx>,
        offset: &mut Size,
        ty: Word,
    ) -> SpirvValue {
        let ty_concrete = self.lookup_type(ty);
        *offset = offset.align_to(ty_concrete.alignof(self));
        // these print statements are really useful for debugging, so leave them easily available
        // println!("const at {}: {}", offset.bytes(), self.debug_type(ty));
        match ty_concrete {
            SpirvType::Void => self
                .tcx
                .dcx()
                .fatal("cannot create const alloc of type void"),
            SpirvType::Bool
            | SpirvType::Integer(..)
            | SpirvType::Float(_)
            | SpirvType::Pointer { .. } => {
                let size = ty_concrete.sizeof(self).unwrap();
                let primitive = match ty_concrete {
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
                    unsupported_spirv_type => bug!(
                        "invalid spirv type internal to create_alloc_const2: {:?}",
                        unsupported_spirv_type
                    ),
                };
                // alloc_id is not needed by read_scalar, so we just use 0. If the context
                // refers to a pointer, read_scalar will find the actual alloc_id. It
                // only uses the input alloc_id in the case that the scalar is uninitialized
                // as part of the error output
                // tldr, the pointer here is only needed for the offset
                let value = match alloc.inner().read_scalar(
                    self,
                    alloc_range(*offset, size),
                    matches!(primitive, Primitive::Pointer(_)),
                ) {
                    Ok(scalar) => {
                        self.scalar_to_backend(scalar, self.primitive_to_scalar(primitive), ty)
                    }
                    _ => self.undef(ty),
                };
                *offset += size;
                value
            }
            SpirvType::Adt {
                size,
                field_types,
                field_offsets,
                ..
            } => {
                let base = *offset;
                let mut values = Vec::with_capacity(field_types.len());
                let mut occupied_spaces = Vec::with_capacity(field_types.len());
                for (&ty, &field_offset) in field_types.iter().zip(field_offsets.iter()) {
                    let total_offset_start = base + field_offset;
                    let mut total_offset_end = total_offset_start;
                    values.push(
                        self.read_from_const_alloc(alloc, &mut total_offset_end, ty)
                            .def_cx(self),
                    );
                    occupied_spaces.push(total_offset_start..total_offset_end);
                }
                if let Some(size) = size {
                    *offset += size;
                } else {
                    assert_eq!(
                        offset.bytes_usize(),
                        alloc.inner().len(),
                        "create_const_alloc must consume all bytes of an Allocation after an unsized struct"
                    );
                }
                self.constant_composite(ty, values.into_iter())
            }
            SpirvType::Array { element, count } => {
                let count = self.builder.lookup_const_scalar(count).unwrap() as usize;
                let values = (0..count).map(|_| {
                    self.read_from_const_alloc(alloc, offset, element)
                        .def_cx(self)
                });
                self.constant_composite(ty, values)
            }
            SpirvType::Vector { element, count } => {
                let total_size = ty_concrete
                    .sizeof(self)
                    .expect("create_const_alloc: Vectors must be sized");
                let final_offset = *offset + total_size;
                let values = (0..count).map(|_| {
                    self.read_from_const_alloc(alloc, offset, element)
                        .def_cx(self)
                });
                let result = self.constant_composite(ty, values);
                assert!(*offset <= final_offset);
                // Vectors sometimes have padding at the end (e.g. vec3), skip over it.
                *offset = final_offset;
                result
            }
            SpirvType::Matrix { element, count } => {
                let total_size = ty_concrete
                    .sizeof(self)
                    .expect("create_const_alloc: Matrices must be sized");
                let final_offset = *offset + total_size;
                let values = (0..count).map(|_| {
                    self.read_from_const_alloc(alloc, offset, element)
                        .def_cx(self)
                });
                let result = self.constant_composite(ty, values);
                assert!(*offset <= final_offset);
                // Matrices sometimes have padding at the end (e.g. Mat4x3), skip over it.
                *offset = final_offset;
                result
            }
            SpirvType::RuntimeArray { element } => {
                let mut values = Vec::new();
                while offset.bytes_usize() != alloc.inner().len() {
                    values.push(
                        self.read_from_const_alloc(alloc, offset, element)
                            .def_cx(self),
                    );
                }
                let result = self.constant_composite(ty, values.into_iter());
                // TODO: Figure out how to do this. Compiling the below crashes both clspv *and* llvm-spirv:
                /*
                __constant struct A {
                    float x;
                    int y[];
                } a = {1, {2, 3, 4}};

                __kernel void foo(__global int* data, __constant int* c) {
                __constant struct A* asdf = &a;
                *data = *c + asdf->y[*c];
                }
                */
                // NOTE(eddyb) the above description is a bit outdated, it's now
                // clear `OpTypeRuntimeArray` does not belong in user code, and
                // is only for dynamically-sized SSBOs and descriptor indexing,
                // and a general solution looks similar to `union` handling, but
                // for the length of a fixed-length array.
                self.zombie_no_span(result.def_cx(self), "constant `OpTypeRuntimeArray` value");
                result
            }
            SpirvType::Function { .. } => self
                .tcx
                .dcx()
                .fatal("TODO: SpirvType::Function not supported yet in create_const_alloc"),
            SpirvType::Image { .. } => self.tcx.dcx().fatal("cannot create a constant image value"),
            SpirvType::Sampler => self
                .tcx
                .dcx()
                .fatal("cannot create a constant sampler value"),
            SpirvType::SampledImage { .. } => self
                .tcx
                .dcx()
                .fatal("cannot create a constant sampled image value"),
            SpirvType::InterfaceBlock { .. } => self
                .tcx
                .dcx()
                .fatal("cannot create a constant interface block value"),
            SpirvType::AccelerationStructureKhr => self
                .tcx
                .dcx()
                .fatal("cannot create a constant acceleration structure"),
            SpirvType::RayQueryKhr => self.tcx.dcx().fatal("cannot create a constant ray query"),
        }
    }
}
