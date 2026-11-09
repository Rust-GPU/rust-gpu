// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::spirv_type::SpirvType;
use rspirv::spirv::Word;
use rustc_abi::{AddressSpace, Reg};
use rustc_codegen_ssa::traits::{BaseTypeCodegenMethods, LayoutTypeCodegenMethods};
use rustc_middle::ty::Ty;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, LayoutError, LayoutOfHelpers, TyAndLayout,
};
use rustc_span::{DUMMY_SP, Span, Spanned, bug, span_bug};
use rustc_target::callconv::{CastTarget, FnAbi};

impl<'tcx> LayoutOfHelpers<'tcx> for CodegenCx<'tcx> {
    type LayoutOfResult = TyAndLayout<'tcx>;

    #[inline]
    fn handle_layout_err(&self, err: LayoutError<'tcx>, span: Span, ty: Ty<'tcx>) -> ! {
        if let LayoutError::SizeOverflow(_) = err {
            self.tcx.dcx().span_fatal(span, err.to_string())
        } else {
            span_bug!(span, "failed to get layout for `{}`: {}", ty, err)
        }
    }
}

impl<'tcx> FnAbiOfHelpers<'tcx> for CodegenCx<'tcx> {
    type FnAbiOfResult = &'tcx FnAbi<'tcx, Ty<'tcx>>;

    #[inline]
    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> ! {
        if let FnAbiError::Layout(LayoutError::SizeOverflow(_)) = err {
            self.tcx.dcx().emit_fatal(Spanned { span, node: err })
        } else {
            match fn_abi_request {
                FnAbiRequest::OfFnPtr { sig, extra_args } => {
                    span_bug!(
                        span,
                        "`fn_abi_of_fn_ptr({sig}, {extra_args:?})` failed: {err:?}",
                    );
                }
                FnAbiRequest::OfInstance {
                    instance,
                    extra_args,
                } => {
                    span_bug!(
                        span,
                        "`fn_abi_of_instance({instance}, {extra_args:?})` failed: {err:?}",
                    );
                }
            }
        }
    }
}

impl<'tcx> LayoutTypeCodegenMethods<'tcx> for CodegenCx<'tcx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        layout.spirv_type(DUMMY_SP, self)
    }

    fn cast_backend_type(&self, ty: &CastTarget) -> Self::Type {
        bug!(
            "cast_backend_type({:?}): query hooks should've made `PassMode::Cast` impossible",
            ty
        )
    }

    fn fn_decl_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::FunctionSignature {
        fn_abi.spirv_type(DUMMY_SP, self)
    }

    fn fn_ptr_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        self.type_ptr_to(self.fn_decl_backend_type(fn_abi))
    }

    fn reg_backend_type(&self, ty: &Reg) -> Self::Type {
        bug!(
            "reg_backend_type({:?}): query hooks should've made `PassMode::Cast` impossible",
            ty
        )
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        layout.spirv_type(DUMMY_SP, self)
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout<'tcx>,
        index: usize,
        _immediate: bool,
    ) -> Self::Type {
        crate::abi::scalar_pair_element_backend_type(self, DUMMY_SP, layout, index)
    }
}

impl<'tcx> CodegenCx<'tcx> {
    pub fn type_usize(&self) -> Word {
        let ptr_size = self.tcx.data_layout.pointer_size().bits() as u32;
        SpirvType::Integer(ptr_size, false).def(DUMMY_SP, self)
    }
}

impl BaseTypeCodegenMethods for CodegenCx<'_> {
    fn type_i8(&self) -> Self::Type {
        SpirvType::Integer(8, false).def(DUMMY_SP, self)
    }
    fn type_i16(&self) -> Self::Type {
        SpirvType::Integer(16, false).def(DUMMY_SP, self)
    }
    fn type_i32(&self) -> Self::Type {
        SpirvType::Integer(32, false).def(DUMMY_SP, self)
    }
    fn type_i64(&self) -> Self::Type {
        SpirvType::Integer(64, false).def(DUMMY_SP, self)
    }
    fn type_i128(&self) -> Self::Type {
        SpirvType::Integer(128, false).def(DUMMY_SP, self)
    }
    fn type_isize(&self) -> Self::Type {
        let ptr_size = self.tcx.data_layout.pointer_size().bits() as u32;
        SpirvType::Integer(ptr_size, false).def(DUMMY_SP, self)
    }

    fn type_f16(&self) -> Self::Type {
        SpirvType::Float(16).def(DUMMY_SP, self)
    }

    fn type_f16b(&self) -> Self::Type {
        bug!("f16b not supported")
    }

    fn type_f32(&self) -> Self::Type {
        SpirvType::Float(32).def(DUMMY_SP, self)
    }
    fn type_f64(&self) -> Self::Type {
        SpirvType::Float(64).def(DUMMY_SP, self)
    }
    fn type_f128(&self) -> Self::Type {
        SpirvType::Float(128).def(DUMMY_SP, self)
    }
    fn type_ppcf128(&self) -> Self::Type {
        panic!("ppcf128 is unsupported")
    }

    fn type_func(&self, args: &[Self::Type], ret: Self::Type) -> Self::FunctionSignature {
        SpirvType::Function {
            return_type: ret,
            arguments: args,
        }
        .def(DUMMY_SP, self)
    }
    fn type_ptr(&self) -> Self::Type {
        self.type_ptr_to(SpirvType::Void.def(DUMMY_SP, self))
    }
    fn type_ptr_ext(&self, address_space: AddressSpace) -> Self::Type {
        self.type_ptr_to_ext(SpirvType::Void.def(DUMMY_SP, self), address_space)
    }

    /// Returns the number of elements in `self` if it is a LLVM vector type.
    fn vector_length(&self, ty: Self::Type) -> usize {
        match self.lookup_type(ty) {
            SpirvType::Vector { count, .. } => count as usize,
            ty => self
                .tcx
                .dcx()
                .fatal(format!("vector_length called on non-vector type: {ty:?}")),
        }
    }

    /// Retrieves the bit width of the integer type `self`.
    fn int_width(&self, ty: Self::Type) -> u64 {
        match self.lookup_type(ty) {
            SpirvType::Integer(width, _) => width as u64,
            ty => self
                .tcx
                .dcx()
                .fatal(format!("int_width called on non-integer type: {ty:?}")),
        }
    }

    fn val_ty(&self, v: Self::Value) -> Self::Type {
        v.ty
    }
}
