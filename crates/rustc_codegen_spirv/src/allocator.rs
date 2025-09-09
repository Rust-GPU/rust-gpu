// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

// FIXME(eddyb) this should be implemented once for all backends, it doesn't
// need backend-specific logic, just a way to create modules and functions.

use crate::builder::Builder;
use crate::builder_spirv::{SpirvConst, SpirvFunctionCursor, SpirvValue};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use rspirv::spirv::{FunctionControl, LinkageType, StorageClass, Word};
use rustc_abi::HasDataLayout as _;
use rustc_ast::expand::allocator::{
    ALLOCATOR_METHODS, AllocatorKind, AllocatorTy, NO_ALLOC_SHIM_IS_UNSTABLE,
    alloc_error_handler_name, default_fn_name, global_fn_name,
};
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods as _, BaseTypeCodegenMethods as _, BuilderMethods as _,
    ConstCodegenMethods as _,
};
use rustc_session::config::OomStrategy;
use rustc_span::DUMMY_SP;
use rustc_symbol_mangling::mangle_internal_symbol;

pub(crate) fn codegen(
    cx: &CodegenCx<'_>,
    kind: AllocatorKind,
    alloc_error_handler_kind: AllocatorKind,
) {
    let usize = cx.type_usize();
    let i8 = cx.type_i8();
    let i8p = cx.type_ptr_to(i8);

    if kind == AllocatorKind::Default {
        for method in ALLOCATOR_METHODS {
            let mut args = Vec::with_capacity(method.inputs.len());
            for input in method.inputs.iter() {
                match input.ty {
                    AllocatorTy::Layout => {
                        args.push(usize); // size
                        args.push(usize); // align
                    }
                    AllocatorTy::Ptr => args.push(i8p),
                    AllocatorTy::Usize => args.push(usize),

                    AllocatorTy::ResultPtr | AllocatorTy::Unit => panic!("invalid allocator arg"),
                }
            }
            let output = match method.output {
                AllocatorTy::ResultPtr => Some(i8p),
                AllocatorTy::Unit => None,

                AllocatorTy::Layout | AllocatorTy::Usize | AllocatorTy::Ptr => {
                    panic!("invalid allocator output")
                }
            };

            let from_name = mangle_internal_symbol(cx.tcx, &global_fn_name(method.name));
            let to_name = mangle_internal_symbol(cx.tcx, &default_fn_name(method.name));

            create_wrapper_function(cx, &from_name, Some(&to_name), &args, output, false);
        }
    }

    // rust alloc error handler
    create_wrapper_function(
        cx,
        &mangle_internal_symbol(cx.tcx, "__rust_alloc_error_handler"),
        Some(&mangle_internal_symbol(
            cx.tcx,
            alloc_error_handler_name(alloc_error_handler_kind),
        )),
        &[usize, usize], // size, align
        None,
        true,
    );

    let define_global = |name: &str, init: SpirvValue| {
        let init_val_id = init.def_cx(cx);
        let ptr_ty = cx.type_ptr_to(init.ty);
        let global_id =
            cx.emit_global()
                .variable(ptr_ty, None, StorageClass::Private, Some(init_val_id));
        cx.set_linkage(global_id, name.to_string(), LinkageType::Export);
    };

    // __rust_alloc_error_handler_should_panic
    define_global(
        &mangle_internal_symbol(cx.tcx, OomStrategy::SYMBOL),
        cx.const_u8(cx.tcx.sess.opts.unstable_opts.oom.should_panic()),
    );

    // __rust_no_alloc_shim_is_unstable_v2
    create_wrapper_function(
        cx,
        &mangle_internal_symbol(cx.tcx, NO_ALLOC_SHIM_IS_UNSTABLE),
        None,
        &[],
        None,
        false,
    );
}

fn create_wrapper_function(
    cx: &CodegenCx<'_>,
    from_name: &str,
    to_name: Option<&str>,
    // NOTE(eddyb) these are SPIR-V type IDs.
    args: &[Word],
    output: Option<Word>,
    _no_return: bool,
) {
    let ret_ty = output.unwrap_or_else(|| SpirvType::Void.def(DUMMY_SP, cx));
    let fn_ty = cx.type_func(args, ret_ty);
    let decl_fn = |name: &str, linkage_type| {
        let mut emit = cx.emit_global();
        let fn_id = emit
            .begin_function(ret_ty, None, FunctionControl::NONE, fn_ty)
            .unwrap();
        for &ty in args {
            emit.function_parameter(ty).unwrap();
        }
        let index_in_builder = emit.selected_function().unwrap();
        emit.end_function().unwrap();
        drop(emit);
        cx.set_linkage(fn_id, name.to_string(), linkage_type);
        SpirvFunctionCursor {
            ty: fn_ty,
            id: fn_id,
            index_in_builder,
        }
    };
    let wrapper = decl_fn(from_name, LinkageType::Export);
    let mut bx = Builder::build(cx, Builder::append_block(cx, wrapper, ""));

    if let Some(to_name) = to_name {
        let callee = decl_fn(to_name, LinkageType::Import);
        let call_args = (0..args.len()).map(|i| bx.get_param(i)).collect::<Vec<_>>();
        let ret = bx.call(
            callee.ty,
            None,
            None,
            cx.def_constant(
                cx.type_ptr_to_ext(callee.ty, cx.data_layout().instruction_address_space),
                SpirvConst::PtrToFunc { func_id: callee.id },
            ),
            &call_args,
            None,
            None,
        );
        if output.is_some() {
            bx.ret(ret);
        } else {
            bx.ret_void();
        }
    } else {
        assert!(output.is_none());
        bx.ret_void()
    }
}
