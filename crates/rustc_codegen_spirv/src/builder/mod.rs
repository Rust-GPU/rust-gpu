mod builder_methods;
mod byte_addressable_buffer;
mod ext_inst;
mod intrinsics;
pub mod libm_intrinsics;
mod spirv_asm;

pub use ext_inst::ExtInst;
use rustc_span::DUMMY_SP;
pub use spirv_asm::InstructionTable;

// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use crate::abi::ConvSpirvType;
use crate::builder_spirv::{BuilderCursor, SpirvValue, SpirvValueExt};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::{SpirvType, StorageClassKind};
use rspirv::spirv::{StorageClass, Word};
use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiBuilderMethods, BackendTypes, BuilderMethods,
    CoverageInfoBuilderMethods, DebugInfoBuilderMethods, StaticBuilderMethods,
    TypeMembershipCodegenMethods,
};
use rustc_errors::{Diag, DiagMessage};
use rustc_middle::mir::coverage::CoverageKind;
use rustc_middle::span_bug;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, HasTypingEnv, LayoutError,
    LayoutOfHelpers, TyAndLayout,
};
use rustc_middle::ty::{Instance, Ty, TyCtxt, TypingEnv};
use rustc_span::Span;
use rustc_span::def_id::DefId;
use rustc_target::abi::call::{ArgAbi, FnAbi, PassMode};
use rustc_target::abi::{HasDataLayout, Size, TargetDataLayout};
use rustc_target::spec::{HasTargetSpec, Target};
use std::ops::{Deref, Range};

pub struct Builder<'a, 'tcx> {
    cx: &'a CodegenCx<'tcx>,
    cursor: BuilderCursor,
    current_fn: <Self as BackendTypes>::Function,
    current_span: Option<Span>,
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    /// See comment on `BuilderCursor`
    pub fn emit(&self) -> std::cell::RefMut<'_, rspirv::dr::Builder> {
        self.emit_with_cursor(self.cursor)
    }

    pub fn zombie(&self, word: Word, reason: &str) {
        if let Some(current_span) = self.current_span {
            self.zombie_with_span(word, current_span, reason);
        } else {
            self.zombie_no_span(word, reason);
        }
    }

    pub fn validate_atomic(&self, ty: Word, to_zombie: Word) {
        if !self.i8_i16_atomics_allowed {
            match self.lookup_type(ty) {
                SpirvType::Integer(width, _) if width < 32 => {
                    self.zombie(to_zombie, "atomic on i8 or i16 when disallowed by runtime");
                }
                _ => (),
            }
        }
    }

    #[track_caller]
    pub fn struct_err(&self, msg: impl Into<DiagMessage>) -> Diag<'_> {
        if let Some(current_span) = self.current_span {
            self.tcx.dcx().struct_span_err(current_span, msg)
        } else {
            self.tcx.dcx().struct_err(msg)
        }
    }

    #[track_caller]
    pub fn err(&self, msg: impl Into<DiagMessage>) {
        if let Some(current_span) = self.current_span {
            self.tcx.dcx().span_err(current_span, msg);
        } else {
            self.tcx.dcx().err(msg);
        }
    }

    #[track_caller]
    pub fn fatal(&self, msg: impl Into<DiagMessage>) -> ! {
        if let Some(current_span) = self.current_span {
            self.tcx.dcx().span_fatal(current_span, msg)
        } else {
            self.tcx.dcx().fatal(msg)
        }
    }

    pub fn span(&self) -> Span {
        self.current_span.unwrap_or(DUMMY_SP)
    }

    // HACK(eddyb) like the `CodegenCx` method but with `self.span()` awareness.
    pub fn type_ptr_to(&self, ty: Word) -> Word {
        SpirvType::Pointer {
            pointee: ty,
            storage_class: StorageClassKind::Inferred,
        }
        .def(self.span(), self)
    }

    pub fn type_ptr_to_with_storage_class(
        &self,
        ty: Word,
        storage_class: StorageClassKind,
    ) -> Word {
        SpirvType::Pointer {
            pointee: ty,
            storage_class,
        }
        .def(self.span(), self)
    }

    // TODO: Definitely add tests to make sure this impl is right.
    fn rotate(&mut self, value: SpirvValue, shift: SpirvValue, is_left: bool) -> SpirvValue {
        let width = match self.lookup_type(shift.ty) {
            SpirvType::Integer(width, _) => width,
            other => self.fatal(format!(
                "cannot rotate non-integer type: {}",
                other.debug(shift.ty, self)
            )),
        };
        let int_size = self.constant_int(shift.ty, width.into());
        let mask = self.constant_int(shift.ty, (width - 1).into());
        let zero = self.constant_int(shift.ty, 0);
        let bool = SpirvType::Bool.def(self.span(), self);
        // https://stackoverflow.com/a/10134877
        let mask_shift = self.and(shift, mask);
        let sub = self.sub(int_size, mask_shift);
        let (lhs, rhs) = if is_left {
            (self.shl(value, mask_shift), self.lshr(value, sub))
        } else {
            (self.lshr(value, mask_shift), self.shl(value, sub))
        };
        let or = self.or(lhs, rhs);
        // "The result is undefined if Shift is greater than or equal to the bit width of the components of Base."
        // So we need to check for zero shift, and don't use the shift result if it is.
        let mask_is_zero = self
            .emit()
            .i_equal(bool, None, mask_shift.def(self), zero.def(self))
            .unwrap()
            .with_type(bool);
        self.select(mask_is_zero, value, or)
    }
}

// Important: This lets us use CodegenCx methods on Builder
impl<'a, 'tcx> Deref for Builder<'a, 'tcx> {
    type Target = CodegenCx<'tcx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

impl<'a, 'tcx> CoverageInfoBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn add_coverage(&mut self, _instance: Instance<'tcx>, _kind: &CoverageKind) {}
}

impl<'a, 'tcx> DebugInfoBuilderMethods for Builder<'a, 'tcx> {
    fn dbg_var_addr(
        &mut self,
        _dbg_var: Self::DIVariable,
        _scope_metadata: Self::DILocation,
        _variable_alloca: Self::Value,
        _direct_offset: Size,
        // NB: each offset implies a deref (i.e. they're steps in a pointer chain).
        _indirect_offsets: &[Size],
        _fragment: Option<Range<Size>>,
    ) {
        todo!()
    }

    fn set_dbg_loc(&mut self, _: Self::DILocation) {
        todo!()
    }

    fn clear_dbg_loc(&mut self) {
        todo!()
    }

    fn get_dbg_loc(&self) -> Option<Self::DILocation> {
        None
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        todo!()
    }

    fn set_var_name(&mut self, _value: Self::Value, _name: &str) {
        todo!()
    }
}

impl<'a, 'tcx> ArgAbiBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn store_fn_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        fn next(bx: &Builder<'_, '_>, idx: &mut usize) -> SpirvValue {
            let val = bx.function_parameter_values.borrow()[&bx.current_fn.def(bx)][*idx];
            *idx += 1;
            val
        }
        match arg_abi.mode {
            PassMode::Ignore => {}
            PassMode::Direct(_) => {
                self.store_arg(arg_abi, next(self, idx), dst);
            }
            PassMode::Pair(..) => {
                OperandValue::Pair(next(self, idx), next(self, idx)).store(self, dst);
            }
            PassMode::Cast { .. } | PassMode::Indirect { .. } => span_bug!(
                self.span(),
                "query hooks should've made this `PassMode` impossible: {:#?}",
                arg_abi
            ),
        }
    }

    fn store_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        val: Self::Value,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        match arg_abi.mode {
            PassMode::Ignore => {}
            PassMode::Direct(_) | PassMode::Pair(..) => {
                OperandRef::from_immediate_or_packed_pair(self, val, arg_abi.layout)
                    .val
                    .store(self, dst);
            }
            PassMode::Cast { .. } | PassMode::Indirect { .. } => span_bug!(
                self.span(),
                "query hooks should've made this `PassMode` impossible: {:#?}",
                arg_abi
            ),
        }
    }

    fn arg_memory_ty(&self, arg_abi: &ArgAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        arg_abi.layout.spirv_type(self.span(), self)
    }
}

impl<'a, 'tcx> AbiBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn get_param(&mut self, index: usize) -> Self::Value {
        self.function_parameter_values.borrow()[&self.current_fn.def(self)][index]
    }
}

impl<'a, 'tcx> StaticBuilderMethods for Builder<'a, 'tcx> {
    fn get_static(&mut self, def_id: DefId) -> Self::Value {
        self.cx.get_static(def_id)
    }
}

impl<'a, 'tcx> BackendTypes for Builder<'a, 'tcx> {
    type Value = <CodegenCx<'tcx> as BackendTypes>::Value;
    type Metadata = <CodegenCx<'tcx> as BackendTypes>::Metadata;
    type Function = <CodegenCx<'tcx> as BackendTypes>::Function;

    type BasicBlock = <CodegenCx<'tcx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'tcx> as BackendTypes>::Type;
    type Funclet = <CodegenCx<'tcx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'tcx> as BackendTypes>::DIScope;
    type DIVariable = <CodegenCx<'tcx> as BackendTypes>::DIVariable;
    type DILocation = <CodegenCx<'tcx> as BackendTypes>::DILocation;
}

impl<'a, 'tcx> HasTypingEnv<'tcx> for Builder<'a, 'tcx> {
    fn typing_env(&self) -> TypingEnv<'tcx> {
        self.cx.typing_env()
    }
}

impl<'a, 'tcx> HasTargetSpec for Builder<'a, 'tcx> {
    fn target_spec(&self) -> &Target {
        self.cx.target_spec()
    }
}

impl<'a, 'tcx> HasTyCtxt<'tcx> for Builder<'a, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.cx.tcx
    }
}

impl<'a, 'tcx> HasDataLayout for Builder<'a, 'tcx> {
    fn data_layout(&self) -> &TargetDataLayout {
        self.cx.data_layout()
    }
}

impl<'tcx> LayoutOfHelpers<'tcx> for Builder<'_, 'tcx> {
    type LayoutOfResult = TyAndLayout<'tcx>;

    #[inline]
    fn handle_layout_err(&self, err: LayoutError<'tcx>, span: Span, ty: Ty<'tcx>) -> ! {
        self.cx.handle_layout_err(err, span, ty)
    }
}

impl<'tcx> FnAbiOfHelpers<'tcx> for Builder<'_, 'tcx> {
    type FnAbiOfResult = &'tcx FnAbi<'tcx, Ty<'tcx>>;

    #[inline]
    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> ! {
        self.cx.handle_fn_abi_err(err, span, fn_abi_request)
    }
}

impl<'tcx> TypeMembershipCodegenMethods<'tcx> for CodegenCx<'tcx> {}
