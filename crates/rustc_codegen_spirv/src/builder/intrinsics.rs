// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::Builder;
use crate::abi::ConvSpirvType;
use crate::builder_spirv::{SpirvValue, SpirvValueExt};
use crate::codegen_cx::CodegenCx;
use crate::custom_insts::CustomInst;
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::GLOp;
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{BuilderMethods, IntrinsicCallBuilderMethods};
use rustc_middle::ty::layout::LayoutOf;
use rustc_middle::ty::{FnDef, Instance, Ty, TyKind, TypingEnv};
use rustc_middle::{bug, ty};
use rustc_span::Span;
use rustc_span::sym;
use rustc_target::abi::call::{FnAbi, PassMode};
use std::assert_matches::assert_matches;

fn int_type_width_signed(ty: Ty<'_>, cx: &CodegenCx<'_>) -> Option<(u64, bool)> {
    match ty.kind() {
        TyKind::Int(t) => Some((
            t.bit_width()
                .unwrap_or(cx.tcx.sess.target.pointer_width as u64),
            true,
        )),
        TyKind::Uint(t) => Some((
            t.bit_width()
                .unwrap_or(cx.tcx.sess.target.pointer_width as u64),
            false,
        )),
        _ => None,
    }
}

impl Builder<'_, '_> {
    pub fn copysign(&mut self, val: SpirvValue, sign: SpirvValue) -> SpirvValue {
        let width = match self.lookup_type(val.ty) {
            SpirvType::Float(width) => width,
            other => bug!(
                "copysign must have float argument, not {}",
                other.debug(val.ty, self)
            ),
        };
        let int_ty = SpirvType::Integer(width, false).def(self.span(), self);
        let (mask_sign, mask_value) = match width {
            32 => (
                self.constant_u32(self.span(), 1 << 31),
                self.constant_u32(self.span(), u32::MAX >> 1),
            ),
            64 => (
                self.constant_u64(self.span(), 1 << 63),
                self.constant_u64(self.span(), u64::MAX >> 1),
            ),
            _ => bug!("copysign must have width 32 or 64, not {}", width),
        };
        let val_bits = self.bitcast(val, int_ty);
        let sign_bits = self.bitcast(sign, int_ty);
        let val_masked = self.and(val_bits, mask_value);
        let sign_masked = self.and(sign_bits, mask_sign);
        let result_bits = self.or(val_masked, sign_masked);
        self.bitcast(result_bits, val.ty)
    }
}

impl<'a, 'tcx> IntrinsicCallBuilderMethods<'tcx> for Builder<'a, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, Self::Value>],
        llresult: Self::Value,
        _span: Span,
    ) -> Result<(), ty::Instance<'tcx>> {
        let callee_ty = instance.ty(self.tcx, TypingEnv::fully_monomorphized());

        let (def_id, fn_args) = match *callee_ty.kind() {
            FnDef(def_id, fn_args) => (def_id, fn_args),
            _ => bug!("expected fn item type, found {}", callee_ty),
        };

        let sig = callee_ty.fn_sig(self.tcx);
        let sig = self
            .tcx
            .normalize_erasing_late_bound_regions(TypingEnv::fully_monomorphized(), sig);
        let arg_tys = sig.inputs();
        let name = self.tcx.item_name(def_id);

        let ret_ty = self.layout_of(sig.output()).spirv_type(self.span(), self);
        let result = PlaceRef::new_sized(llresult, fn_abi.ret.layout);

        let value = match name {
            sym::likely | sym::unlikely => {
                // Ignore these for now.
                args[0].immediate()
            }

            sym::breakpoint => {
                self.abort();
                assert!(fn_abi.ret.is_ignore());
                return Ok(());
            }

            sym::volatile_load | sym::unaligned_volatile_load => {
                let ptr = args[0].immediate();
                let layout = self.layout_of(fn_args.type_at(0));
                let load = self.volatile_load(layout.spirv_type(self.span(), self), ptr);
                if !result.layout.is_zst() {
                    self.store(load, result.val.llval, result.val.align);
                }
                return Ok(());
            }

            sym::prefetch_read_data
            | sym::prefetch_write_data
            | sym::prefetch_read_instruction
            | sym::prefetch_write_instruction => {
                // ignore
                assert!(fn_abi.ret.is_ignore());
                return Ok(());
            }

            sym::saturating_add => {
                assert_eq!(arg_tys[0], arg_tys[1]);
                let result = match arg_tys[0].kind() {
                    TyKind::Int(_) | TyKind::Uint(_) => {
                        self.add(args[0].immediate(), args[1].immediate())
                    }
                    TyKind::Float(_) => self.fadd(args[0].immediate(), args[1].immediate()),
                    other => self.fatal(format!(
                        "Unimplemented saturating_add intrinsic type: {other:#?}"
                    )),
                };
                // TODO: Implement this
                self.zombie(result.def(self), "saturating_add is not implemented yet");
                result
            }
            sym::saturating_sub => {
                assert_eq!(arg_tys[0], arg_tys[1]);
                let result = match &arg_tys[0].kind() {
                    TyKind::Int(_) | TyKind::Uint(_) => {
                        self.sub(args[0].immediate(), args[1].immediate())
                    }
                    TyKind::Float(_) => self.fsub(args[0].immediate(), args[1].immediate()),
                    other => self.fatal(format!(
                        "Unimplemented saturating_sub intrinsic type: {other:#?}"
                    )),
                };
                // TODO: Implement this
                self.zombie(result.def(self), "saturating_sub is not implemented yet");
                result
            }

            sym::sqrtf32 | sym::sqrtf64 => self.gl_op(GLOp::Sqrt, ret_ty, [args[0].immediate()]),
            sym::powif32 | sym::powif64 => {
                let float = self.sitofp(args[1].immediate(), args[0].immediate().ty);
                self.gl_op(GLOp::Pow, ret_ty, [args[0].immediate(), float])
            }
            sym::sinf32 | sym::sinf64 => self.gl_op(GLOp::Sin, ret_ty, [args[0].immediate()]),
            sym::cosf32 | sym::cosf64 => self.gl_op(GLOp::Cos, ret_ty, [args[0].immediate()]),
            sym::powf32 | sym::powf64 => self.gl_op(GLOp::Pow, ret_ty, [
                args[0].immediate(),
                args[1].immediate(),
            ]),
            sym::expf32 | sym::expf64 => self.gl_op(GLOp::Exp, ret_ty, [args[0].immediate()]),
            sym::exp2f32 | sym::exp2f64 => self.gl_op(GLOp::Exp2, ret_ty, [args[0].immediate()]),
            sym::logf32 | sym::logf64 => self.gl_op(GLOp::Log, ret_ty, [args[0].immediate()]),
            sym::log2f32 | sym::log2f64 => self.gl_op(GLOp::Log2, ret_ty, [args[0].immediate()]),
            sym::log10f32 | sym::log10f64 => {
                // spir-v glsl doesn't have log10, so,
                // log10(x) == (1 / ln(10)) * ln(x)
                let mul = self.constant_float(args[0].immediate().ty, 1.0 / 10.0f64.ln());
                let ln = self.gl_op(GLOp::Log, ret_ty, [args[0].immediate()]);
                self.fmul(mul, ln)
            }
            sym::fmaf32 | sym::fmaf64 => self.gl_op(GLOp::Fma, ret_ty, [
                args[0].immediate(),
                args[1].immediate(),
                args[2].immediate(),
            ]),
            sym::fabsf32 | sym::fabsf64 => self.gl_op(GLOp::FAbs, ret_ty, [args[0].immediate()]),
            sym::minnumf32 | sym::minnumf64 => self.gl_op(GLOp::FMin, ret_ty, [
                args[0].immediate(),
                args[1].immediate(),
            ]),
            sym::maxnumf32 | sym::maxnumf64 => self.gl_op(GLOp::FMax, ret_ty, [
                args[0].immediate(),
                args[1].immediate(),
            ]),
            sym::copysignf32 | sym::copysignf64 => {
                let val = args[0].immediate();
                let sign = args[1].immediate();
                self.copysign(val, sign)
            }
            sym::floorf32 | sym::floorf64 => self.gl_op(GLOp::Floor, ret_ty, [args[0].immediate()]),
            sym::ceilf32 | sym::ceilf64 => self.gl_op(GLOp::Ceil, ret_ty, [args[0].immediate()]),
            sym::truncf32 | sym::truncf64 => self.gl_op(GLOp::Trunc, ret_ty, [args[0].immediate()]),
            // TODO: Correctness of all these rounds
            sym::rintf32 | sym::rintf64 => self.gl_op(GLOp::Round, ret_ty, [args[0].immediate()]),
            sym::nearbyintf32 | sym::nearbyintf64 | sym::roundf32 | sym::roundf64 => {
                self.gl_op(GLOp::Round, ret_ty, [args[0].immediate()])
            }

            sym::rotate_left | sym::rotate_right => {
                let is_left = name == sym::rotate_left;
                let val = args[0].immediate();
                let shift = args[1].immediate();
                self.rotate(val, shift, is_left)
            }

            sym::ctlz => self.count_leading_trailing_zeros(args[0].immediate(), false, false),
            sym::ctlz_nonzero => {
                self.count_leading_trailing_zeros(args[0].immediate(), false, true)
            }
            sym::cttz => self.count_leading_trailing_zeros(args[0].immediate(), true, false),
            sym::cttz_nonzero => self.count_leading_trailing_zeros(args[0].immediate(), true, true),

            sym::ctpop => self.count_ones(args[0].immediate()),
            sym::bitreverse => self.bit_reverse(args[0].immediate()),
            sym::bswap => {
                // https://github.com/KhronosGroup/SPIRV-LLVM/pull/221/files
                // TODO: Definitely add tests to make sure this impl is right.
                let arg = args[0].immediate();
                let (width, is_signed) = int_type_width_signed(arg_tys[0], self)
                    .expect("bswap must have an integer argument");

                // Cast to unsigned type for byte-swapping
                let unsigned_ty: u32 =
                    SpirvType::Integer(width.try_into().unwrap(), false).def(self.span(), self);
                let unsigned_arg = if is_signed {
                    self.bitcast(arg, unsigned_ty)
                } else {
                    arg
                };

                let swapped = match width {
                    8 => unsigned_arg,
                    16 => {
                        let offset8 = self.constant_u16(self.span(), 8);
                        let tmp1 = self.shl(unsigned_arg, offset8);
                        let tmp2 = self.lshr(unsigned_arg, offset8);
                        self.or(tmp1, tmp2)
                    }
                    32 => {
                        let offset8 = self.constant_u32(self.span(), 8);
                        let offset24 = self.constant_u32(self.span(), 24);
                        let mask16 = self.constant_u32(self.span(), 0xFF00);
                        let mask24 = self.constant_u32(self.span(), 0xFF0000);
                        let tmp4 = self.shl(unsigned_arg, offset24);
                        let tmp3 = self.shl(unsigned_arg, offset8);
                        let tmp2 = self.lshr(unsigned_arg, offset8);
                        let tmp1 = self.lshr(unsigned_arg, offset24);
                        let tmp3 = self.and(tmp3, mask24);
                        let tmp2 = self.and(tmp2, mask16);
                        let res1 = self.or(tmp1, tmp2);
                        let res2 = self.or(tmp3, tmp4);
                        self.or(res1, res2)
                    }
                    64 => {
                        let offset8 = self.constant_u64(self.span(), 8);
                        let offset24 = self.constant_u64(self.span(), 24);
                        let offset40 = self.constant_u64(self.span(), 40);
                        let offset56 = self.constant_u64(self.span(), 56);
                        let mask16 = self.constant_u64(self.span(), 0xff00);
                        let mask24 = self.constant_u64(self.span(), 0xff0000);
                        let mask32 = self.constant_u64(self.span(), 0xff000000);
                        let mask40 = self.constant_u64(self.span(), 0xff00000000);
                        let mask48 = self.constant_u64(self.span(), 0xff0000000000);
                        let mask56 = self.constant_u64(self.span(), 0xff000000000000);
                        let tmp8 = self.shl(unsigned_arg, offset56);
                        let tmp7 = self.shl(unsigned_arg, offset40);
                        let tmp6 = self.shl(unsigned_arg, offset24);
                        let tmp5 = self.shl(unsigned_arg, offset8);
                        let tmp4 = self.lshr(unsigned_arg, offset8);
                        let tmp3 = self.lshr(unsigned_arg, offset24);
                        let tmp2 = self.lshr(unsigned_arg, offset40);
                        let tmp1 = self.lshr(unsigned_arg, offset56);
                        let tmp7 = self.and(tmp7, mask56);
                        let tmp6 = self.and(tmp6, mask48);
                        let tmp5 = self.and(tmp5, mask40);
                        let tmp4 = self.and(tmp4, mask32);
                        let tmp3 = self.and(tmp3, mask24);
                        let tmp2 = self.and(tmp2, mask16);
                        let res1 = self.or(tmp8, tmp7);
                        let res2 = self.or(tmp6, tmp5);
                        let res3 = self.or(tmp4, tmp3);
                        let res4 = self.or(tmp2, tmp1);
                        let res1 = self.or(res1, res2);
                        let res3 = self.or(res3, res4);
                        self.or(res1, res3)
                    }
                    other => {
                        let undef = self.undef(ret_ty);
                        self.zombie(
                            undef.def(self),
                            &format!("bswap not implemented for int width {other}"),
                        );
                        undef
                    }
                };

                // Cast back to the original signed type if necessary
                if is_signed {
                    self.bitcast(swapped, arg.ty)
                } else {
                    swapped
                }
            }

            sym::compare_bytes => {
                let undef = self.undef(ret_ty);
                self.zombie(undef.def(self), "memcmp not implemented");
                undef
            }

            _ => {
                // Call the fallback body instead of generating the intrinsic code
                return Err(ty::Instance::new(instance.def_id(), instance.args));
            }
        };

        if !fn_abi.ret.is_ignore() {
            assert_matches!(fn_abi.ret.mode, PassMode::Direct(_) | PassMode::Pair(..));
            OperandRef::from_immediate_or_packed_pair(self, value, result.layout)
                .val
                .store(self, result);
        }
        Ok(())
    }

    fn abort(&mut self) {
        self.abort_with_kind_and_message_debug_printf("abort", "intrinsics::abort() called", []);
    }

    fn assume(&mut self, _val: Self::Value) {
        // TODO: llvm.assume
    }

    fn expect(&mut self, cond: Self::Value, _expected: bool) -> Self::Value {
        // TODO: llvm.expect
        cond
    }

    fn type_test(&mut self, _pointer: Self::Value, _typeid: Self::Metadata) -> Self::Value {
        todo!()
    }

    fn type_checked_load(
        &mut self,
        _llvtable: Self::Value,
        _vtable_byte_offset: u64,
        _typeid: Self::Metadata,
    ) -> Self::Value {
        todo!()
    }

    fn va_start(&mut self, _val: Self::Value) -> Self::Value {
        todo!()
    }

    fn va_end(&mut self, _val: Self::Value) -> Self::Value {
        todo!()
    }
}

impl Builder<'_, '_> {
    pub fn count_ones(&self, arg: SpirvValue) -> SpirvValue {
        let ty = arg.ty;
        match self.cx.lookup_type(ty) {
            SpirvType::Integer(bits, signed) => {
                let u32 = SpirvType::Integer(32, false).def(self.span(), self);

                match bits {
                    8 | 16 => {
                        let arg = arg.def(self);
                        let arg = if signed {
                            let unsigned =
                                SpirvType::Integer(bits, false).def(self.span(), self);
                            self.emit().bitcast(unsigned, None, arg).unwrap()
                        } else {
                            arg
                        };
                        let arg = self.emit().u_convert(u32, None, arg).unwrap();
                        self.emit().bit_count(u32, None, arg).unwrap()
                    }
                    32 => self.emit().bit_count(u32, None, arg.def(self)).unwrap(),
                    64 => {
                        let u32_32 = self.constant_u32(self.span(), 32).def(self);
                        let arg = arg.def(self);
                        let lower = self.emit().s_convert(u32, None, arg).unwrap();
                        let higher = self
                            .emit()
                            .shift_left_logical(ty, None, arg, u32_32)
                            .unwrap();
                        let higher = self.emit().s_convert(u32, None, higher).unwrap();

                        let lower_bits = self.emit().bit_count(u32, None, lower).unwrap();
                        let higher_bits = self.emit().bit_count(u32, None, higher).unwrap();
                        self.emit().i_add(u32, None, lower_bits, higher_bits).unwrap()
                    }
                    _ => {
                        let undef = self.undef(ty).def(self);
                        self.zombie(undef, &format!(
                            "counting leading / trailing zeros on unsupported {ty:?} bit integer type"
                        ));
                        undef
                    }
                }
                    .with_type(u32)
            }
            _ => self.fatal("count_ones on a non-integer type"),
        }
    }
    pub fn bit_reverse(&self, arg: SpirvValue) -> SpirvValue {
        let ty = arg.ty;
        match self.cx.lookup_type(ty) {
            SpirvType::Integer(bits, signed) => {
                let u32 = SpirvType::Integer(32, false).def(self.span(), self);
                let uint = SpirvType::Integer(bits, false).def(self.span(), self);

                match (bits, signed) {
                    (8 | 16, signed) => {
                        let arg = arg.def(self);
                        let arg = if signed {
                            self.emit().bitcast(uint, None, arg).unwrap()
                        } else {
                            arg
                        };
                        let arg = self.emit().u_convert(u32, None, arg).unwrap();

                        let reverse = self.emit().bit_reverse(u32, None, arg).unwrap();
                        let shift = self.constant_u32(self.span(), 32 - bits).def(self);
                        let reverse = self.emit().shift_right_logical(u32, None, reverse, shift).unwrap();
                        let reverse = self.emit().u_convert(uint, None, reverse).unwrap();
                        if signed {
                            self.emit().bitcast(ty, None, reverse).unwrap()
                        } else {
                            reverse
                        }
                    }
                    (32, false) => self.emit().bit_reverse(u32, None, arg.def(self)).unwrap(),
                    (32, true) => {
                        let arg = self.emit().bitcast(u32, None, arg.def(self)).unwrap();
                        let reverse = self.emit().bit_reverse(u32, None, arg).unwrap();
                        self.emit().bitcast(ty, None, reverse).unwrap()
                    },
                    (64, signed) => {
                        let u32_32 = self.constant_u32(self.span(), 32).def(self);
                        let arg = arg.def(self);
                        let lower = self.emit().s_convert(u32, None, arg).unwrap();
                        let higher = self
                            .emit()
                            .shift_left_logical(ty, None, arg, u32_32)
                            .unwrap();
                        let higher = self.emit().s_convert(u32, None, higher).unwrap();

                        // note that higher and lower have swapped
                        let higher_bits = self.emit().bit_reverse(u32, None, lower).unwrap();
                        let lower_bits = self.emit().bit_reverse(u32, None, higher).unwrap();

                        let higher_bits = self.emit().u_convert(uint, None, higher_bits).unwrap();
                        let shift = self.constant_u32(self.span(), 32).def(self);
                        let higher_bits = self.emit().shift_right_logical(uint, None, higher_bits, shift).unwrap();
                        let lower_bits = self.emit().u_convert(uint, None, lower_bits).unwrap();

                        let result = self.emit().bitwise_or(ty, None, lower_bits, higher_bits).unwrap();
                        if signed {
                            self.emit().bitcast(ty, None, result).unwrap()
                        } else {
                            result
                        }
                    }
                    _ => {
                        let undef = self.undef(ty).def(self);
                        self.zombie(undef, &format!(
                            "counting leading / trailing zeros on unsupported {ty:?} bit integer type"
                        ));
                        undef
                    }
                }
                    .with_type(ty)
            }
            _ => self.fatal("count_ones on a non-integer type"),
        }
    }

    pub fn count_leading_trailing_zeros(
        &self,
        arg: SpirvValue,
        trailing: bool,
        non_zero: bool,
    ) -> SpirvValue {
        let ty = arg.ty;
        match self.cx.lookup_type(ty) {
            SpirvType::Integer(bits, signed) => {
                let bool = SpirvType::Bool.def(self.span(), self);
                let u32 = SpirvType::Integer(32, false).def(self.span(), self);

                let gl_op = if trailing {
                    // rust is always unsigned
                    GLOp::FindILsb
                } else {
                    GLOp::FindUMsb
                };

                let glsl = self.ext_inst.borrow_mut().import_glsl(self);
                let find_xsb = |arg| {
                    self.emit()
                        .ext_inst(u32, None, glsl, gl_op as u32, [Operand::IdRef(arg)])
                        .unwrap()
                };

                let converted = match bits {
                    8 | 16 => {
                        if trailing {
                            let arg = self.emit().s_convert(u32, None, arg.def(self)).unwrap();
                            find_xsb(arg)
                        } else {
                            let arg = arg.def(self);
                            let arg = if signed {
                                let unsigned =
                                    SpirvType::Integer(bits, false).def(self.span(), self);
                                self.emit().bitcast(unsigned, None, arg).unwrap()
                            } else {
                                arg
                            };
                            let arg = self.emit().u_convert(u32, None, arg).unwrap();
                            let xsb = find_xsb(arg);
                            let subtrahend = self.constant_u32(self.span(), 32 - bits).def(self);
                            self.emit().i_sub(u32, None, xsb, subtrahend).unwrap()
                        }
                    }
                    32 => find_xsb(arg.def(self)),
                    64 => {
                        let u32_0 = self.constant_int(u32, 0).def(self);
                        let u32_32 = self.constant_u32(self.span(), 32).def(self);

                        let arg = arg.def(self);
                        let lower = self.emit().s_convert(u32, None, arg).unwrap();
                        let higher = self
                            .emit()
                            .shift_left_logical(ty, None, arg, u32_32)
                            .unwrap();
                        let higher = self.emit().s_convert(u32, None, higher).unwrap();

                        let lower_bits = find_xsb(lower);
                        let higher_bits = find_xsb(higher);

                        if trailing {
                            let use_lower = self.emit().i_equal(bool, None, higher, u32_0).unwrap();
                            let lower_bits =
                                self.emit().i_add(u32, None, lower_bits, u32_32).unwrap();
                            self.emit()
                                .select(u32, None, use_lower, lower_bits, higher_bits)
                                .unwrap()
                        } else {
                            let use_higher = self.emit().i_equal(bool, None, lower, u32_0).unwrap();
                            let higher_bits =
                                self.emit().i_add(u32, None, higher_bits, u32_32).unwrap();
                            self.emit()
                                .select(u32, None, use_higher, higher_bits, lower_bits)
                                .unwrap()
                        }
                    }
                    _ => {
                        let undef = self.undef(ty).def(self);
                        self.zombie(undef, &format!(
                            "counting leading / trailing zeros on unsupported {ty:?} bit integer type"
                        ));
                        undef
                    }
                };

                if non_zero {
                    converted
                } else {
                    let int_0 = self.constant_int(ty, 0).def(self);
                    let int_bits = self.constant_int(u32, bits as u128).def(self);
                    let is_0 = self
                        .emit()
                        .i_equal(bool, None, arg.def(self), int_0)
                        .unwrap();
                    self.emit()
                        .select(u32, None, is_0, int_bits, converted)
                        .unwrap()
                }
                .with_type(u32)
            }
            _ => self.fatal("counting leading / trailing zeros on a non-integer type"),
        }
    }

    pub fn abort_with_kind_and_message_debug_printf(
        &mut self,
        kind: &str,
        message_debug_printf_fmt_str: impl Into<String>,
        message_debug_printf_args: impl IntoIterator<Item = SpirvValue>,
    ) {
        // FIXME(eddyb) this should be cached more efficiently.
        let void_ty = SpirvType::Void.def(rustc_span::DUMMY_SP, self);

        // HACK(eddyb) there is no `abort` or `trap` instruction in SPIR-V,
        // so the best thing we can do is use our own custom instruction.
        let kind_id = self.emit().string(kind);
        let message_debug_printf_fmt_str_id = self.emit().string(message_debug_printf_fmt_str);
        self.custom_inst(void_ty, CustomInst::Abort {
            kind: Operand::IdRef(kind_id),
            message_debug_printf: [message_debug_printf_fmt_str_id]
                .into_iter()
                .chain(
                    message_debug_printf_args
                        .into_iter()
                        .map(|arg| arg.def(self)),
                )
                .map(Operand::IdRef)
                .collect(),
        });
        self.unreachable();

        // HACK(eddyb) we still need an active block in case the user of this
        // `Builder` will continue to emit instructions after the `.abort()`.
        let post_abort_dead_bb = self.append_sibling_block("post_abort_dead");
        self.switch_to_block(post_abort_dead_bb);
    }
}
