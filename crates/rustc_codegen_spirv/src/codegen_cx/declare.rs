// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::AggregatedSpirvAttributes;
use crate::builder_spirv::{
    SpirvConst, SpirvFunctionCursor, SpirvValue, SpirvValueExt, SpirvValueKind,
};
use crate::custom_decorations::{CustomDecoration, SrcLocDecoration};
use crate::spirv_type::SpirvType;
use itertools::Itertools;
use rspirv::spirv::{FunctionControl, LinkageType, StorageClass, Word};
use rustc_abi::{AddressSpace, Align};
use rustc_attr_data_structures::InlineAttr;
use rustc_codegen_ssa::traits::{PreDefineCodegenMethods, StaticCodegenMethods};
use rustc_middle::bug;
use rustc_middle::middle::codegen_fn_attrs::{CodegenFnAttrFlags, CodegenFnAttrs};
use rustc_middle::mir::interpret::ConstAllocation;
use rustc_middle::mir::mono::{Linkage, MonoItem, Visibility};
use rustc_middle::ty::layout::{FnAbiOf, LayoutOf};
use rustc_middle::ty::{self, Instance, TypeVisitableExt, TypingEnv};
use rustc_span::Span;
use rustc_span::def_id::DefId;

fn attrs_to_spirv(attrs: &CodegenFnAttrs) -> FunctionControl {
    let mut control = FunctionControl::NONE;
    match attrs.inline {
        InlineAttr::None => (),
        InlineAttr::Hint | InlineAttr::Always | InlineAttr::Force { .. } => {
            control.insert(FunctionControl::INLINE);
        }
        InlineAttr::Never => control.insert(FunctionControl::DONT_INLINE),
    }
    if attrs.flags.contains(CodegenFnAttrFlags::FFI_PURE) {
        control.insert(FunctionControl::PURE);
    }
    if attrs.flags.contains(CodegenFnAttrFlags::FFI_CONST) {
        control.insert(FunctionControl::CONST);
    }
    control
}

impl<'tcx> CodegenCx<'tcx> {
    /// Returns a function if it already exists, or declares a header if it doesn't.
    pub fn get_fn_ext(&self, instance: Instance<'tcx>) -> SpirvFunctionCursor {
        assert!(!instance.args.has_infer());
        assert!(!instance.args.has_escaping_bound_vars());

        if let Some(&func) = self.fn_instances.borrow().get(&instance) {
            return func;
        }

        // Because we've already declared everything with predefine_fn, if we hit this branch, we're guaranteed to be
        // importing this function from elsewhere. So, slap an extern on it.
        let linkage = Some(LinkageType::Import);
        let llfn = self.declare_fn_ext(instance, linkage);

        self.fn_instances.borrow_mut().insert(instance, llfn);

        llfn
    }

    // The call graph of how this is reachable is a little tangled, so:
    // MiscCodegenMethods::get_fn -> get_fn_ext -> declare_fn_ext
    // MiscCodegenMethods::get_fn_addr -> get_fn_ext -> declare_fn_ext
    // PreDefineCodegenMethods::predefine_fn -> declare_fn_ext
    fn declare_fn_ext(
        &self,
        instance: Instance<'tcx>,
        linkage: Option<LinkageType>,
    ) -> SpirvFunctionCursor {
        let def_id = instance.def_id();

        // HACK(eddyb) this is a bit roundabout, but the easiest way to get a
        // fully absolute path that contains at least as much information as
        // `instance.to_string()` (at least with `-C symbol-mangling-version=v0`).
        // While we could use the mangled symbol instead, like we do for linkage,
        // `OpName` is more of a debugging aid, so not having to separately
        // demangle the SPIR-V can help. However, if some tools assume `OpName`
        // is always a valid identifier, we may have to offer the mangled name
        // (as some sort of opt-in, or toggled based on the platform, etc.).
        let symbol_name = self.tcx.symbol_name(instance).name;
        let demangled_symbol_name = format!("{:#}", rustc_demangle::demangle(symbol_name));

        let codegen_fn_attrs = self.tcx.codegen_fn_attrs(def_id);
        let mut control = attrs_to_spirv(codegen_fn_attrs);
        if codegen_fn_attrs.inline == InlineAttr::None && instance.def.requires_inline(self.tcx) {
            control.insert(FunctionControl::INLINE);
        }

        // HACK(eddyb) this function is intentionally not inlined, despite the
        // ability to disable MIR inlining (and it's unlikely LLVM inlining is
        // unwanted, since it just returns two constants).
        if demangled_symbol_name.starts_with("core::alloc::layout::size_align::<") {
            control.insert(FunctionControl::INLINE);
        }

        // HACK(eddyb) some tiny free functions in `alloc::raw_vec` really should
        // get inlined but don't because they are merely generic and our inliner
        // doesn't have any heuristics based on size/complexity of the callee.
        if demangled_symbol_name.starts_with("alloc::raw_vec::")
            && demangled_symbol_name.contains("_cap")
        {
            control.insert(FunctionControl::INLINE);
        }

        let fn_abi = self.fn_abi_of_instance(instance, ty::List::empty());
        let span = self.tcx.def_span(def_id);
        let function_type = fn_abi.spirv_type(span, self);
        let (return_type, argument_types) = match self.lookup_type(function_type) {
            SpirvType::Function {
                return_type,
                arguments,
            } => (return_type, arguments),
            other => bug!("fn_abi type {}", other.debug(function_type, self)),
        };

        let declared = {
            let mut emit = self.emit_global();
            let id = emit
                .begin_function(return_type, None, control, function_type)
                .unwrap();
            let index_in_builder = emit.selected_function().unwrap();

            // FIXME(eddyb) omitting `OpFunctionParameter` on imports might be
            // illegal, this probably shouldn't be conditional at all.
            if linkage != Some(LinkageType::Import) {
                for &ty in argument_types {
                    emit.function_parameter(ty).unwrap();
                }
            }
            emit.end_function().unwrap();
            SpirvFunctionCursor {
                ty: function_type,
                id,
                index_in_builder,
            }
        };
        let fn_id = declared.id;

        // HACK(eddyb) this is a temporary workaround due to our use of `rspirv`,
        // which prevents us from attaching `OpLine`s to `OpFunction` definitions,
        // but we can use our custom `SrcLocDecoration` instead.
        let src_loc_inst = SrcLocDecoration::from_rustc_span(
            self.tcx.def_ident_span(def_id).unwrap_or(span),
            &self.builder,
        )
        .map(|src_loc| src_loc.encode_to_inst(fn_id));
        self.emit_global()
            .module_mut()
            .annotations
            .extend(src_loc_inst);

        self.emit_global().name(fn_id, &demangled_symbol_name);

        if let Some(linkage) = linkage {
            self.set_linkage(fn_id, symbol_name.to_owned(), linkage);
        }

        let attrs = AggregatedSpirvAttributes::parse(self, self.tcx.get_attrs_unchecked(def_id));
        if let Some(entry) = attrs.entry.map(|attr| attr.value) {
            // HACK(eddyb) early insert to let `shader_entry_stub` call this
            // very function via `get_fn_addr`.
            self.fn_instances.borrow_mut().insert(instance, declared);

            let entry_name = entry
                .name
                .as_ref()
                .map_or_else(|| instance.to_string(), ToString::to_string);
            self.entry_stub(instance, fn_abi, entry_name, entry);
        }

        // FIXME(eddyb) should the maps exist at all, now that the `DefId` is known
        // at `call` time, and presumably its high-level details can be looked up?
        if attrs.buffer_load_intrinsic.is_some() {
            self.buffer_load_intrinsics.borrow_mut().insert(def_id);
        }
        if attrs.buffer_store_intrinsic.is_some() {
            self.buffer_store_intrinsics.borrow_mut().insert(def_id);
        }

        // Check for usage of `libm` intrinsics outside of `libm` itself
        if self.tcx.crate_name(def_id.krate) == self.sym.libm && !def_id.is_local() {
            let item_name = self.tcx.item_name(def_id);
            if let Some(&intrinsic) = self.sym.libm_intrinsics.get(&item_name) {
                self.libm_intrinsics.borrow_mut().insert(def_id, intrinsic);
            } else {
                let message = format!("missing libm intrinsic {symbol_name}, which is {instance}");
                self.tcx.dcx().err(message);
            }
        }

        // Check if this is a From trait implementation
        if let Some(impl_def_id) = self.tcx.impl_of_method(def_id)
            && let Some(trait_ref) = self.tcx.impl_trait_ref(impl_def_id)
        {
            let trait_def_id = trait_ref.skip_binder().def_id;

            // Check if this is the From trait.
            let trait_path = self.tcx.def_path_str(trait_def_id);
            if matches!(
                trait_path.as_str(),
                "core::convert::From" | "std::convert::From"
            ) {
                // Extract the source and target types from the trait substitutions
                let trait_args = trait_ref.skip_binder().args;
                if let (Some(target_ty), Some(source_ty)) =
                    (trait_args.types().nth(0), trait_args.types().nth(1))
                {
                    self.from_trait_impls
                        .borrow_mut()
                        .insert(def_id, (source_ty, target_ty));
                }
            }
        }

        if [
            self.tcx.lang_items().panic_fn(),
            self.tcx.lang_items().panic_fmt(),
            self.tcx.lang_items().panic_nounwind(),
        ]
        .contains(&Some(def_id))
        {
            self.panic_entry_points.borrow_mut().insert(def_id);
        }

        // HACK(eddyb) there is no good way to identify these definitions
        // (e.g. no `#[lang = "..."]` attribute), but this works well enough.
        if let Some("panic_nounwind_fmt" | "panic_explicit") =
            demangled_symbol_name.strip_prefix("core::panicking::")
        {
            self.panic_entry_points.borrow_mut().insert(def_id);
        }
        if let Some(pieces_len) = demangled_symbol_name
            .strip_prefix("<core::fmt::Arguments>::new_const::<")
            .and_then(|s| s.strip_suffix(">"))
        {
            self.fmt_args_new_fn_ids
                .borrow_mut()
                .insert(fn_id, (pieces_len.parse().unwrap(), 0));
        }
        if let Some(generics) = demangled_symbol_name
            .strip_prefix("<core::fmt::Arguments>::new_v1::<")
            .and_then(|s| s.strip_suffix(">"))
        {
            let (pieces_len, rt_args_len) = generics.split_once(", ").unwrap();
            self.fmt_args_new_fn_ids.borrow_mut().insert(
                fn_id,
                (pieces_len.parse().unwrap(), rt_args_len.parse().unwrap()),
            );
        }
        if demangled_symbol_name == "<core::fmt::Arguments>::new_v1_formatted" {
            // HACK(eddyb) `!0` used as a placeholder value to indicate "dynamic".
            self.fmt_args_new_fn_ids
                .borrow_mut()
                .insert(fn_id, (!0, !0));
        }

        // HACK(eddyb) there is no good way to identify these definitions
        // (e.g. no `#[lang = "..."]` attribute), but this works well enough.
        if let Some(suffix) = demangled_symbol_name.strip_prefix("<core::fmt::rt::Argument>::new_")
        {
            let spec = suffix.split_once("::<").and_then(|(method_suffix, _)| {
                Some(match method_suffix {
                    "display" => ' ',
                    "debug" => '?',
                    "octal" => 'o',
                    "lower_hex" => 'x',
                    "upper_hex" => 'X',
                    "pointer" => 'p',
                    "binary" => 'b',
                    "lower_exp" => 'e',
                    "upper_exp" => 'E',
                    _ => return None,
                })
            });
            if let Some(spec) = spec
                && let Some((ty,)) = instance.args.types().collect_tuple()
            {
                self.fmt_rt_arg_new_fn_ids_to_ty_and_spec
                    .borrow_mut()
                    .insert(fn_id, (ty, spec));
            }
        }

        declared
    }

    pub fn get_static(&self, def_id: DefId) -> SpirvValue {
        if let Some(&g) = self.statics.borrow().get(&def_id) {
            return g;
        }

        let defined_in_current_codegen_unit = self
            .codegen_unit
            .items()
            .contains_key(&MonoItem::Static(def_id));
        assert!(
            !defined_in_current_codegen_unit,
            "get_static() should always hit the cache for statics defined in the same CGU, but did not for `{def_id:?}`"
        );

        let instance = Instance::mono(self.tcx, def_id);
        let ty = instance.ty(self.tcx, TypingEnv::fully_monomorphized());
        let sym = self.tcx.symbol_name(instance).name;
        let span = self.tcx.def_span(def_id);
        let g = self.declare_global(span, self.layout_of(ty).spirv_type(span, self));
        self.statics.borrow_mut().insert(def_id, g);
        self.set_linkage(g.def_cx(self), sym.to_string(), LinkageType::Import);
        g
    }

    fn declare_global(&self, span: Span, ty: Word) -> SpirvValue {
        let ptr_ty = SpirvType::Pointer {
            pointee: Some(ty),
            addr_space: AddressSpace::DATA,
        }
        .def(span, self);
        // FIXME(eddyb) figure out what the correct storage class is.
        let result = self
            .emit_global()
            .variable(ptr_ty, None, StorageClass::Private, None)
            .with_type(ptr_ty);
        // TODO: These should be StorageClass::Private, so just zombie for now.
        // FIXME(eddyb) why zombie? this looks like it should just work nowadays.
        self.zombie_with_span(result.def_cx(self), span, "globals are not supported yet");
        result
    }
}

impl<'tcx> PreDefineCodegenMethods<'tcx> for CodegenCx<'tcx> {
    fn predefine_static(
        &mut self,
        def_id: DefId,
        linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let instance = Instance::mono(self.tcx, def_id);
        let ty = instance.ty(self.tcx, TypingEnv::fully_monomorphized());
        let span = self.tcx.def_span(def_id);
        let spvty = self.layout_of(ty).spirv_type(span, self);
        let linkage = match linkage {
            Linkage::External => Some(LinkageType::Export),
            Linkage::Internal => None,
            other => {
                self.tcx.dcx().err(format!(
                    "TODO: Linkage type {other:?} not supported yet for static var symbol {symbol_name}"
                ));
                None
            }
        };

        let g = self.declare_global(span, spvty);

        self.statics.borrow_mut().insert(def_id, g);
        if let Some(linkage) = linkage {
            self.set_linkage(g.def_cx(self), symbol_name.to_string(), linkage);
        }
    }

    fn predefine_fn(
        &mut self,
        instance: Instance<'tcx>,
        linkage: Linkage,
        _visibility: Visibility,
        symbol_name: &str,
    ) {
        let linkage2 = match linkage {
            // super sketchy hack: memcpy, memmove, memset, memcmp, and bcmp in the
            // compiler_builtins crate use the WeakAny linkage type. Treat it as actually External
            // linkage because we know there's only one of them.
            Linkage::External | Linkage::WeakAny => Some(LinkageType::Export),
            Linkage::Internal => None,
            other => {
                self.tcx.dcx().err(format!(
                    "TODO: Linkage type {other:?} not supported yet for function symbol {symbol_name}"
                ));
                None
            }
        };
        let declared = self.declare_fn_ext(instance, linkage2);

        self.fn_instances.borrow_mut().insert(instance, declared);
    }
}

impl<'tcx> CodegenCx<'tcx> {
    // FIXME(eddyb) remove after https://github.com/rust-lang/rust/pull/142960
    // (or a similar PR relying on `ConstAllocation<'tcx>`) lands upstream.
    pub(crate) fn static_addr_of_alloc(
        &self,
        alloc: ConstAllocation<'tcx>,
        _kind: Option<&str>,
    ) -> SpirvValue {
        // FIXME(eddyb) do not ignore `alloc.align` or `alloc.mutability`!
        let init = self.const_alloc_to_backend(alloc, /*static*/ false);
        self.def_constant(
            self.type_ptr_to(init.ty),
            SpirvConst::PtrTo {
                pointee: init.def_cx(self),
                pointee_alloc: alloc,
            },
        )
    }
}

impl<'tcx> StaticCodegenMethods for CodegenCx<'tcx> {
    fn static_addr_of(&self, cv: Self::Value, align: Align, kind: Option<&str>) -> Self::Value {
        let SpirvValueKind::ConstDataFromAlloc { alloc_id } = cv.kind else {
            bug!("expected result of `const_data_from_alloc` in `static_addr_of`");
        };
        let alloc = self.tcx.global_alloc(alloc_id).unwrap_memory();
        assert_eq!(align, alloc.inner().align);
        self.static_addr_of_alloc(alloc, kind)
    }

    fn codegen_static(&mut self, def_id: DefId) {
        let g = self.get_static(def_id);

        let alloc = match self.tcx.eval_static_initializer(def_id) {
            Ok(alloc) => alloc,
            // Error has already been reported
            Err(_) => return,
        };

        // FIXME(eddyb) get the value type from the variable itself and/or take
        // it from initializer, if any is present.
        let value_ty = match self.lookup_type(g.ty) {
            SpirvType::Pointer {
                pointee: Some(pointee),
                ..
            } => pointee,
            other => self.tcx.dcx().fatal(format!(
                "global had non-pointer type {}",
                other.debug(g.ty, self)
            )),
        };
        let v = self.try_read_from_const_alloc(alloc, value_ty).unwrap();
        assert_ty_eq!(self, value_ty, v.ty);
        self.builder
            .set_global_initializer(g.def_cx(self), v.def_cx(self));

        let attrs = self.tcx.codegen_fn_attrs(def_id);

        let alloc = alloc.inner();
        let align_override =
            Some(alloc.align).filter(|&align| align != self.lookup_type(value_ty).alignof(self));
        if let Some(_align) = align_override {
            // FIXME(eddyb) implement, or at least error.
        }

        if attrs.flags.contains(CodegenFnAttrFlags::THREAD_LOCAL) {
            // FIXME(eddyb) implement, or at least error.
        }

        if let Some(_section) = attrs.link_section {
            // FIXME(eddyb) implement, or at least error.
        }

        if attrs.flags.contains(CodegenFnAttrFlags::USED_COMPILER) {
            // `USED` and `USED_LINKER` can't be used together.
            assert!(!attrs.flags.contains(CodegenFnAttrFlags::USED_LINKER));

            // The semantics of #[used] in Rust only require the symbol to make it into the
            // object file. It is explicitly allowed for the linker to strip the symbol if it
            // is dead, which means we are allowed to use `llvm.compiler.used` instead of
            // `llvm.used` here.
            //
            // Additionally, https://reviews.llvm.org/D97448 in LLVM 13 started emitting unique
            // sections with SHF_GNU_RETAIN flag for llvm.used symbols, which may trigger bugs
            // in the handling of `.init_array` (the static constructor list) in versions of
            // the gold linker (prior to the one released with binutils 2.36).
            //
            // That said, we only ever emit these when `#[used(compiler)]` is explicitly
            // requested. This is to avoid similar breakage on other targets, in particular
            // MachO targets have *their* static constructor lists broken if `llvm.compiler.used`
            // is emitted rather than `llvm.used`. However, that check happens when assigning
            // the `CodegenFnAttrFlags` in the `codegen_fn_attrs` query, so we don't need to
            // take care of it here.
            self.add_compiler_used_global(g);
        }
        if attrs.flags.contains(CodegenFnAttrFlags::USED_LINKER) {
            // `USED` and `USED_LINKER` can't be used together.
            assert!(!attrs.flags.contains(CodegenFnAttrFlags::USED_COMPILER));

            self.add_used_global(g);
        }
    }
}

impl CodegenCx<'_> {
    /// Mark the given global value as "used", to prevent the compiler and linker from potentially
    /// removing a static variable that may otherwise appear unused.
    fn add_used_global(&self, global: SpirvValue) {
        // TODO: Ignore for now.
        let _unused = (self, global);
    }

    /// Same as `add_used_global`, but only prevent the compiler from potentially removing an
    /// otherwise unused symbol. The linker is still permitted to drop it.
    ///
    /// This corresponds to the semantics of the `#[used]` attribute.
    fn add_compiler_used_global(&self, global: SpirvValue) {
        // TODO: Ignore for now.
        let _unused = (self, global);
    }
}
