// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use super::CodegenCx;
use crate::abi::ConvSpirvType;
use crate::attr::{AggregatedSpirvAttributes, Entry, Spanned, SpecConstant};
use crate::builder::Builder;
use crate::builder_spirv::{SpirvFunctionCursor, SpirvValue, SpirvValueExt};
use crate::spirv_type::SpirvType;
use rspirv::dr::Operand;
use rspirv::spirv::{
    BuiltIn, Decoration, Dim, ExecutionModel, FunctionControl, StorageClass, Word,
};
use rustc_abi::FieldsShape;
use rustc_codegen_ssa::traits::{BaseTypeCodegenMethods, BuilderMethods, MiscCodegenMethods as _};
use rustc_data_structures::fx::FxHashMap;
use rustc_errors::MultiSpan;
use rustc_hir as hir;
use rustc_middle::span_bug;
use rustc_middle::ty::layout::{LayoutOf, TyAndLayout};
use rustc_middle::ty::{self, Instance, Ty};
use rustc_span::{DUMMY_SP, Span};
use rustc_target::callconv::{ArgAbi, FnAbi, PassMode};
use std::assert_matches::assert_matches;

/// Various information about an entry-point parameter, which can only be deduced
/// (and/or checked) in all cases by using the original reference/value Rust type
/// (e.g. `&mut T` vs `&T` vs `T`).
///
/// This is in contrast to other information about "shader interface variables",
/// that can rely on merely the SPIR-V type and/or `#[spirv(...)]` attributes.
///
/// See also `entry_param_deduce_from_rust_ref_or_value` (which computes this).
struct EntryParamDeducedFromRustRefOrValue<'tcx> {
    /// The type/layout for the data to pass onto the entry-point parameter,
    /// either by-value (only for `Input`) or behind some kind of reference.
    ///
    /// That is, the original parameter type is (given `T = value_layout.ty`):
    /// * `T` (iff `storage_class` is `Input`)
    /// * `&T` (all shader interface storage classes other than `Input`/`Output`)
    /// * `&mut T` (only writable storage classes)
    value_layout: TyAndLayout<'tcx>,

    /// The SPIR-V storage class to declare the shader interface variable in,
    /// either deduced from the type (e.g. opaque handles use `UniformConstant`),
    /// provided via `#[spirv(...)]` attributes, or an `Input`/`Output` default.
    //
    // HACK(eddyb) this can be `Err(SpecConstant)` to indicate this is actually
    // an `OpSpecConstant` being exposed as if it were an `Input`
    storage_class: Result<StorageClass, SpecConstant>,

    /// Whether this entry-point parameter doesn't allow writes to the underlying
    /// shader interface variable (i.e. is by-value, or `&T` where `T: Freeze`).
    ///
    /// For some storage classes, this can be mapped to `NonWritable` decorations
    /// (only `StorageBuffer` for now, with few others, if any, plausible at all).
    read_only: bool,
}

impl<'tcx> CodegenCx<'tcx> {
    // Entry points declare their "interface" (all uniforms, inputs, outputs, etc.) as parameters.
    // spir-v uses globals to declare the interface. So, we need to generate a lil stub for the
    // "real" main that collects all those global variables and calls the user-defined main
    // function.
    pub fn entry_stub(
        &self,
        entry_instance: Instance<'tcx>,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        name: String,
        entry: Entry,
    ) {
        let entry_def_id = entry_instance.def_id();
        let span = self
            .tcx
            .def_ident_span(entry_def_id)
            .unwrap_or_else(|| self.tcx.def_span(entry_def_id));
        let hir_params = {
            let fn_local_def_id = if let Some(id) = entry_def_id.as_local() {
                id
            } else {
                self.tcx
                    .dcx()
                    .span_err(span, format!("cannot declare {name} as an entry point"));
                return;
            };
            self.tcx.hir_body_owned_by(fn_local_def_id).params
        };
        for (arg_abi, hir_param) in fn_abi.args.iter().zip(hir_params) {
            match arg_abi.mode {
                PassMode::Direct(_) | PassMode::Ignore => {}
                PassMode::Pair(..) => {
                    // FIXME(eddyb) implement `ScalarPair` `Input`s, or change
                    // the `FnAbi` readjustment to only use `PassMode::Pair` for
                    // pointers to `!Sized` types, but not other `ScalarPair`s.
                    if !matches!(arg_abi.layout.ty.kind(), ty::Ref(..)) {
                        self.tcx.dcx().span_err(
                            hir_param.ty_span,
                            format!(
                                "entry point parameter type not yet supported \
                                 (`{}` has `ScalarPair` ABI but is not a `&T`)",
                                arg_abi.layout.ty
                            ),
                        );
                    }
                }
                _ => span_bug!(
                    hir_param.ty_span,
                    "query hooks should've made this `PassMode` impossible: {:#?}",
                    arg_abi
                ),
            }
        }
        if fn_abi.ret.layout.ty.is_unit() {
            assert_matches!(fn_abi.ret.mode, PassMode::Ignore);
        } else {
            self.tcx.dcx().span_err(
                span,
                format!(
                    "entry point should return `()`, not `{}`",
                    fn_abi.ret.layout.ty
                ),
            );
        }

        // let execution_model = entry.execution_model;
        let stub = self.shader_entry_stub(
            span,
            entry_instance,
            fn_abi,
            hir_params,
            name,
            entry.execution_model,
        );
        let mut emit = self.emit_global();
        entry
            .execution_modes
            .iter()
            .for_each(|(execution_mode, execution_mode_extra)| {
                emit.execution_mode(stub.id, *execution_mode, execution_mode_extra);
            });
    }

    fn shader_entry_stub(
        &self,
        span: Span,
        entry_instance: Instance<'tcx>,
        entry_fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        hir_params: &[hir::Param<'tcx>],
        name: String,
        execution_model: ExecutionModel,
    ) -> SpirvFunctionCursor {
        let stub_fn = {
            let void = SpirvType::Void.def(span, self);
            let fn_void_void = SpirvType::Function {
                return_type: void,
                arguments: &[],
            }
            .def(span, self);
            let mut emit = self.emit_global();
            let id = emit
                .begin_function(void, None, FunctionControl::NONE, fn_void_void)
                .unwrap();
            let index_in_builder = emit.selected_function().unwrap();
            emit.end_function().unwrap();
            SpirvFunctionCursor {
                ty: fn_void_void,
                id,
                index_in_builder,
            }
        };

        let mut op_entry_point_interface_operands = vec![];

        let mut bx = Builder::build(self, Builder::append_block(self, stub_fn, ""));
        let mut call_args = vec![];
        let mut decoration_locations = FxHashMap::default();
        for (entry_arg_abi, hir_param) in entry_fn_abi.args.iter().zip(hir_params) {
            bx.set_span(hir_param.span);
            self.declare_shader_interface_for_param(
                execution_model,
                entry_arg_abi,
                hir_param,
                &mut op_entry_point_interface_operands,
                &mut bx,
                &mut call_args,
                &mut decoration_locations,
            );
        }
        bx.set_span(span);
        bx.call(
            self.get_fn(entry_instance).ty,
            None,
            Some(entry_fn_abi),
            self.get_fn_addr(entry_instance),
            &call_args,
            None,
            None,
        );
        bx.ret_void();

        self.emit_global().entry_point(
            execution_model,
            stub_fn.id,
            name,
            op_entry_point_interface_operands,
        );
        stub_fn
    }

    /// Attempt to compute `EntryParamDeducedFromRustRefOrValue` (see its docs)
    /// from `ref_or_value_layout` (and potentially some of `attrs`).
    ///
    // FIXME(eddyb) document this by itself.
    fn entry_param_deduce_from_rust_ref_or_value(
        &self,
        ref_or_value_layout: TyAndLayout<'tcx>,
        hir_param: &hir::Param<'tcx>,
        attrs: &AggregatedSpirvAttributes,
    ) -> EntryParamDeducedFromRustRefOrValue<'tcx> {
        // FIXME(eddyb) attribute validation should be done ahead of time.
        // FIXME(eddyb) also check the type for compatibility with being
        // part of the interface, including potentially `Sync`ness etc.
        // FIXME(eddyb) really need to require `T: Sync` for references
        // (especially relevant with interior mutability!).
        let (value_layout, explicit_mutbl, is_ref) = match *ref_or_value_layout.ty.kind() {
            ty::Ref(_, pointee_ty, mutbl) => (self.layout_of(pointee_ty), mutbl, true),
            _ => (ref_or_value_layout, hir::Mutability::Not, false),
        };
        let effective_mutbl = match explicit_mutbl {
            // NOTE(eddyb) `T: !Freeze` used to detect "`T` has interior mutability"
            // (i.e. "`&T` is a shared+mutable reference"), more specifically `T`
            // containing `UnsafeCell` (but not behind any indirection), which
            // includes many safe abstractions (e.g. `Cell`, `RefCell`, `Atomic*`).
            hir::Mutability::Not
                if is_ref
                    && !value_layout
                        .ty
                        .is_freeze(self.tcx, ty::TypingEnv::fully_monomorphized()) =>
            {
                hir::Mutability::Mut
            }
            _ => explicit_mutbl,
        };
        // FIXME(eddyb) this should maybe be an extension method on `StorageClass`?
        let expected_mutbl_for = |storage_class| match storage_class {
            StorageClass::UniformConstant
            | StorageClass::Input
            | StorageClass::Uniform
            | StorageClass::PushConstant => hir::Mutability::Not,

            // FIXME(eddyb) further categorize this by which want interior
            // mutability (+`Sync`!), likely almost all of them, and which
            // can be per-lane-owning `&mut T`.
            _ => hir::Mutability::Mut,
        };
        let value_spirv_type = value_layout.spirv_type(hir_param.ty_span, self);
        // Some types automatically specify a storage class. Compute that here.
        let element_ty = match self.lookup_type(value_spirv_type) {
            SpirvType::Array { element, .. } | SpirvType::RuntimeArray { element } => {
                self.lookup_type(element)
            }
            ty => ty,
        };
        let deduced_storage_class_from_ty = match element_ty {
            SpirvType::Image { .. }
            | SpirvType::Sampler
            | SpirvType::SampledImage { .. }
            | SpirvType::AccelerationStructureKhr { .. } => {
                if is_ref {
                    Some(StorageClass::UniformConstant)
                } else {
                    self.tcx.dcx().span_err(
                        hir_param.ty_span,
                        format!(
                            "entry parameter type must be by-reference: `&{}`",
                            value_layout.ty,
                        ),
                    );
                    None
                }
            }
            _ => None,
        };
        // Storage classes can be specified via attribute. Compute that here, and emit diagnostics.
        let attr_storage_class = attrs.storage_class.map(|storage_class_attr| {
            let storage_class = storage_class_attr.value;

            if !is_ref {
                self.tcx.dcx().span_fatal(
                    hir_param.ty_span,
                    format!(
                        "invalid entry param type `{}` for storage class `{storage_class:?}` \
                         (expected `&{}T`)",
                        value_layout.ty,
                        expected_mutbl_for(storage_class).prefix_str()
                    ),
                )
            }

            match deduced_storage_class_from_ty {
                Some(deduced) if storage_class == deduced => self.tcx.dcx().span_warn(
                    storage_class_attr.span,
                    "redundant storage class attribute, storage class is deduced from type",
                ),
                Some(deduced) => {
                    self.tcx
                        .dcx()
                        .struct_span_err(hir_param.span, "storage class mismatch")
                        .with_span_label(
                            storage_class_attr.span,
                            format!("`{storage_class:?}` specified in attribute"),
                        )
                        .with_span_label(
                            hir_param.ty_span,
                            format!("`{deduced:?}` deduced from type"),
                        )
                        .with_help(format!(
                            "remove storage class attribute to use `{deduced:?}` as storage class"
                        ))
                        .emit();
                }
                None => (),
            }

            storage_class
        });
        // If storage class was not deduced nor specified, compute the default (i.e. input/output)
        let storage_class = deduced_storage_class_from_ty
            .or(attr_storage_class)
            .unwrap_or_else(|| match (is_ref, explicit_mutbl) {
                (false, _) => StorageClass::Input,
                (true, hir::Mutability::Mut) => StorageClass::Output,
                (true, hir::Mutability::Not) => self.tcx.dcx().span_fatal(
                    hir_param.ty_span,
                    format!(
                        "invalid entry param type `{}` (expected `{}` or `&mut {1}`)",
                        ref_or_value_layout.ty, value_layout.ty
                    ),
                ),
            });

        // Validate reference mutability against the *final* storage class.
        let read_only = effective_mutbl == hir::Mutability::Not;
        if is_ref {
            // FIXME(eddyb) named booleans make uses a bit more readable.
            let ref_is_read_only = read_only;
            let storage_class_requires_read_only =
                expected_mutbl_for(storage_class) == hir::Mutability::Not;
            if !ref_is_read_only && storage_class_requires_read_only {
                let mut err = self.tcx.dcx().struct_span_err(
                    hir_param.ty_span,
                    format!(
                        "entry-point requires {}...",
                        match explicit_mutbl {
                            hir::Mutability::Not => "interior mutability",
                            hir::Mutability::Mut => "a mutable reference",
                        }
                    ),
                );
                {
                    let note_message =
                        format!("...but storage class `{storage_class:?}` is read-only");
                    let (note_label_span, note_label) =
                        if let Some(storage_class_attr) = attrs.storage_class {
                            (
                                storage_class_attr.span,
                                format!("`{storage_class:?}` specified in attribute"),
                            )
                        } else {
                            (
                                hir_param.ty_span,
                                format!("`{storage_class:?}` deduced from type"),
                            )
                        };
                    // HACK(eddyb) have to use `MultiSpan` directly for labels,
                    // as there's no `span_label` equivalent for `span_note`s.
                    let mut note_multi_span: MultiSpan = vec![note_label_span].into();
                    note_multi_span.push_span_label(note_label_span, note_label);
                    err.span_note(note_multi_span, note_message);
                }
                err.emit();
            }
        }

        // HACK(eddyb) only handle `attrs.spec_constant` after everything above
        // would've assumed it was actually an implicitly-`Input`.
        let mut storage_class = Ok(storage_class);
        if let Some(spec_constant) = attrs.spec_constant {
            let ty = ref_or_value_layout;
            let valid_array_count = match ty.fields {
                FieldsShape::Array { count, .. } => {
                    let element = ty.field(self, 0);
                    (element.ty == self.tcx.types.u32).then_some(u32::try_from(count).ok())
                }
                FieldsShape::Primitive => (ty.ty == self.tcx.types.u32).then_some(None),
                _ => None,
            };

            if let Some(array_count) = valid_array_count {
                if let Some(storage_class) = attrs.storage_class {
                    self.tcx.dcx().span_err(
                        storage_class.span,
                        "`#[spirv(spec_constant)]` cannot have a storage class",
                    );
                } else {
                    assert_eq!(storage_class, Ok(StorageClass::Input));
                    assert!(!is_ref);
                    storage_class = Err(SpecConstant {
                        array_count,
                        ..spec_constant.value
                    });
                }
            } else {
                self.tcx.dcx().span_err(
                    hir_param.ty_span,
                    format!(
                        "unsupported `#[spirv(spec_constant)]` type `{}` (expected `u32` or `[u32; N]`)",
                        ref_or_value_layout.ty
                    ),
                );
            }
        }

        EntryParamDeducedFromRustRefOrValue {
            value_layout,
            storage_class,
            read_only,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn declare_shader_interface_for_param(
        &self,
        execution_model: ExecutionModel,
        entry_arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        hir_param: &hir::Param<'tcx>,
        op_entry_point_interface_operands: &mut Vec<Word>,
        bx: &mut Builder<'_, 'tcx>,
        call_args: &mut Vec<SpirvValue>,
        decoration_locations: &mut FxHashMap<StorageClass, u32>,
    ) {
        let attrs = AggregatedSpirvAttributes::parse(self, self.tcx.hir_attrs(hir_param.hir_id));

        let EntryParamDeducedFromRustRefOrValue {
            value_layout,
            storage_class,
            read_only,
        } = self.entry_param_deduce_from_rust_ref_or_value(entry_arg_abi.layout, hir_param, &attrs);
        let value_spirv_type = value_layout.spirv_type(hir_param.ty_span, self);

        let (var_id, spec_const_id) = match storage_class {
            // Pre-allocate the module-scoped `OpVariable` *Result* ID.
            Ok(_) => (
                Ok(self.emit_global().id()),
                Err("entry-point interface variable is not a `#[spirv(spec_constant)]`"),
            ),
            Err(SpecConstant {
                id,
                default,
                array_count,
            }) => {
                let u32_ty = SpirvType::Integer(32, false).def(DUMMY_SP, self);
                let single = |id: u32| {
                    let mut emit = self.emit_global();
                    let spec_const_id = emit.spec_constant_bit32(u32_ty, default.unwrap_or(0));
                    emit.decorate(
                        spec_const_id,
                        Decoration::SpecId,
                        [Operand::LiteralBit32(id)],
                    );
                    spec_const_id
                };
                let param_word = if let Some(array_count) = array_count {
                    let array = (0..array_count).map(|i| single(id + i)).collect::<Vec<_>>();
                    let array_ty = SpirvType::Array {
                        element: u32_ty,
                        count: self.constant_u32(DUMMY_SP, array_count),
                    }
                    .def(DUMMY_SP, self);
                    bx.emit()
                        .composite_construct(array_ty, None, array)
                        .unwrap()
                } else {
                    single(id)
                };
                (
                    Err("`#[spirv(spec_constant)]` is not an entry-point interface variable"),
                    Ok(param_word),
                )
            }
        };

        // Emit decorations deduced from the reference/value Rust type.
        if read_only {
            // NOTE(eddyb) it appears only `StorageBuffer`s simultaneously:
            // - allow `NonWritable` decorations on shader interface variables
            // - default to writable (i.e. the decoration actually has an effect)
            if storage_class == Ok(StorageClass::StorageBuffer) {
                self.emit_global()
                    .decorate(var_id.unwrap(), Decoration::NonWritable, []);
            }
        }

        // Certain storage classes require an `OpTypeStruct` decorated with `Block`,
        // which we represent with `SpirvType::InterfaceBlock` (see its doc comment).
        // This "interface block" construct is also required for "runtime arrays".
        let is_unsized = self.lookup_type(value_spirv_type).sizeof(self).is_none();
        let is_pair = matches!(entry_arg_abi.mode, PassMode::Pair(..));
        let is_unsized_with_len = is_pair && is_unsized;
        // HACK(eddyb) sanity check because we get the same information in two
        // very different ways, and going out of sync could cause subtle issues.
        assert_eq!(
            is_unsized_with_len,
            value_layout.is_unsized(),
            "`{}` param mismatch in call ABI (is_pair={is_pair}) + \
             SPIR-V type (is_unsized={is_unsized}) \
             vs layout:\n{value_layout:#?}",
            entry_arg_abi.layout.ty
        );
        if is_pair && !is_unsized {
            // If PassMode is Pair, then we need to fill in the second part of the pair with a
            // value. We currently only do that with unsized types, so if a type is a pair for some
            // other reason (e.g. a tuple), we bail.
            self.tcx
                .dcx()
                .span_fatal(hir_param.ty_span, "pair type not supported yet")
        }
        // FIXME(eddyb) should this talk about "typed buffers" instead of "interface blocks"?
        // FIXME(eddyb) should we talk about "descriptor indexing" or
        // actually use more reasonable terms like "resource arrays"?
        let needs_interface_block_and_supports_descriptor_indexing = matches!(
            storage_class,
            Ok(StorageClass::Uniform | StorageClass::StorageBuffer)
        );
        let needs_interface_block = needs_interface_block_and_supports_descriptor_indexing
            || storage_class == Ok(StorageClass::PushConstant);
        // NOTE(eddyb) `#[spirv(typed_buffer)]` adds `SpirvType::InterfaceBlock`s
        // which must bypass the automated ones (i.e. the user is taking control).
        let has_explicit_interface_block = needs_interface_block_and_supports_descriptor_indexing
            && {
                // Peel off arrays first (used for "descriptor indexing").
                let outermost_or_array_element = match self.lookup_type(value_spirv_type) {
                    SpirvType::Array { element, .. } | SpirvType::RuntimeArray { element } => {
                        element
                    }
                    _ => value_spirv_type,
                };
                matches!(
                    self.lookup_type(outermost_or_array_element),
                    SpirvType::InterfaceBlock { .. }
                )
            };
        let var_ptr_spirv_type;
        let (value_ptr, value_len) = if needs_interface_block && !has_explicit_interface_block {
            let var_spirv_type = SpirvType::InterfaceBlock {
                inner_type: value_spirv_type,
            }
            .def(hir_param.span, self);
            var_ptr_spirv_type = self.type_ptr_to(var_spirv_type);

            let zero_u32 = self.constant_u32(hir_param.span, 0).def_cx(self);
            let value_ptr_spirv_type = self.type_ptr_to(value_spirv_type);
            let value_ptr = bx
                .emit()
                .in_bounds_access_chain(
                    value_ptr_spirv_type,
                    None,
                    var_id.unwrap(),
                    [zero_u32].iter().cloned(),
                )
                .unwrap()
                .with_type(value_ptr_spirv_type);

            let value_len = if is_unsized_with_len {
                match self.lookup_type(value_spirv_type) {
                    SpirvType::RuntimeArray { .. } => {}
                    _ => {
                        self.tcx.dcx().span_err(
                            hir_param.ty_span,
                            "only plain slices are supported as unsized types",
                        );
                    }
                }

                // FIXME(eddyb) shouldn't this be `usize`?
                let len_spirv_type = self.type_isize();
                let len = bx
                    .emit()
                    .array_length(len_spirv_type, None, var_id.unwrap(), 0)
                    .unwrap();

                Some(len.with_type(len_spirv_type))
            } else {
                if is_unsized {
                    // It's OK to use a RuntimeArray<u32> and not have a length parameter, but
                    // it's just nicer ergonomics to use a slice.
                    self.tcx
                        .dcx()
                        .span_warn(hir_param.ty_span, "use &[T] instead of &RuntimeArray<T>");
                }
                None
            };

            (Ok(value_ptr), value_len)
        } else {
            var_ptr_spirv_type = self.type_ptr_to(value_spirv_type);

            // FIXME(eddyb) should we talk about "descriptor indexing" or
            // actually use more reasonable terms like "resource arrays"?
            let unsized_is_descriptor_indexing =
                needs_interface_block_and_supports_descriptor_indexing
                    || storage_class == Ok(StorageClass::UniformConstant);
            if unsized_is_descriptor_indexing {
                match self.lookup_type(value_spirv_type) {
                    SpirvType::RuntimeArray { .. } => {
                        if is_unsized_with_len {
                            self.tcx.dcx().span_err(
                                hir_param.ty_span,
                                "descriptor indexing must use &RuntimeArray<T>, not &[T]",
                            );
                        }
                    }
                    _ => {
                        if is_unsized {
                            self.tcx.dcx().span_err(
                                hir_param.ty_span,
                                "only RuntimeArray is supported, not other unsized types",
                            );
                        }
                    }
                }
            } else {
                // FIXME(eddyb) determine, based on the type, what kind of type
                // this is, to narrow it further to e.g. "buffer in a non-buffer
                // storage class" or "storage class expects fixed data sizes".
                if is_unsized {
                    self.tcx.dcx().span_fatal(
                        hir_param.ty_span,
                        format!(
                            "unsized types are not supported for {}",
                            match storage_class {
                                Ok(storage_class) => format!("storage class {storage_class:?}"),
                                Err(SpecConstant { .. }) => "`#[spirv(spec_constant)]`".into(),
                            },
                        ),
                    );
                }
            }

            let value_len = if is_pair {
                // We've already emitted an error, fill in a placeholder value
                Some(bx.undef(self.type_isize()))
            } else {
                None
            };

            (
                var_id.map(|var_id| var_id.with_type(var_ptr_spirv_type)),
                value_len,
            )
        };

        // Compute call argument(s) to match what the Rust entry `fn` expects,
        // starting from the `value_ptr` pointing to a `value_spirv_type`
        // (e.g. `Input` doesn't use indirection, so we have to load from it).
        if let ty::Ref(..) = entry_arg_abi.layout.ty.kind() {
            match entry_arg_abi.mode {
                PassMode::Direct(_) => {
                    assert_eq!(value_len, None);
                    call_args.push(value_ptr.unwrap());
                }
                PassMode::Pair(..) => call_args.extend([value_ptr.unwrap(), value_len.unwrap()]),
                PassMode::Ignore => (),
                _ => unreachable!(),
            }
        } else {
            match entry_arg_abi.mode {
                PassMode::Ignore => {}
                PassMode::Direct(_) => {
                    let value = match storage_class {
                        Ok(_) => {
                            assert_eq!(storage_class, Ok(StorageClass::Input));
                            bx.load(
                                entry_arg_abi.layout.spirv_type(hir_param.ty_span, bx),
                                value_ptr.unwrap(),
                                entry_arg_abi.layout.align.abi,
                            )
                        }
                        Err(SpecConstant { .. }) => {
                            spec_const_id.unwrap().with_type(value_spirv_type)
                        }
                    };
                    call_args.push(value);
                    assert_eq!(value_len, None);
                }
                _ => unreachable!(),
            }
        }

        // FIXME(eddyb) check whether the storage class is compatible with the
        // specific shader stage of this entry-point, and any decorations
        // (e.g. Vulkan has specific rules for builtin storage classes).

        // Emit `OpName` in the simple case of a pattern that's just a variable
        // name (e.g. "foo" for `foo: Vec3`). While `OpName` is *not* supposed
        // to be semantic, OpenGL and some tooling rely on it for reflection.
        if let hir::PatKind::Binding(_, _, ident, _) = &hir_param.pat.kind {
            self.emit_global()
                .name(var_id.or(spec_const_id).unwrap(), ident.to_string());
        }

        // location assignment
        // Note(@firestar99): UniformConstant are things like `SampledImage`, `StorageImage`, `Sampler` and
        // `Acceleration structure`. Almost always they are assigned a `descriptor_set` and binding, thus never end up
        // here being assigned locations. I think this is one of those occasions where spirv allows us to assign
        // locations, but the "client API" Vulkan doesn't describe any use-case for them, or at least none I'm aware of.
        // A quick scour through the spec revealed that `VK_KHR_dynamic_rendering_local_read` may need this, and while
        // we don't support it yet (I assume), I'll just keep it here in case it becomes useful in the future.
        let has_location = matches!(
            storage_class,
            Ok(StorageClass::Input | StorageClass::Output | StorageClass::UniformConstant)
        );
        let mut assign_location = |var_id: Result<Word, &str>, explicit: Option<u32>| {
            let storage_class = storage_class.unwrap();
            let location = decoration_locations
                .entry(storage_class)
                .or_insert_with(|| 0);
            if let Some(explicit) = explicit {
                *location = explicit;
            }
            self.emit_global().decorate(
                var_id.unwrap(),
                Decoration::Location,
                std::iter::once(Operand::LiteralBit32(*location)),
            );
            let mut spirv_type = self.lookup_type(value_spirv_type);

            // These shader types and storage classes skip the outer array or pointer of the declaration when computing
            // the location layout, see bug at https://github.com/Rust-GPU/rust-gpu/issues/500.
            //
            // The match statment follows the rules at:
            // https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-iointerfaces-matching
            #[allow(clippy::match_same_arms)]
            let can_skip_outer_array =
                match (execution_model, storage_class, attrs.per_primitive_ext) {
                    // > if the input is declared in a tessellation control or geometry shader...
                    (
                        ExecutionModel::TessellationControl | ExecutionModel::Geometry,
                        StorageClass::Input,
                        _,
                    ) => true,
                    // > if the maintenance4 feature is enabled, they are declared as OpTypeVector variables, and the
                    // > output has a Component Count value higher than that of the input but the same Component Type
                    // Irrelevant: This allows a vertex shader to output a Vec4 and a fragment shader to accept a vector
                    // type with fewer components, like Vec3, Vec2 (or f32?). Which has no influence on locations.
                    // > if the output is declared in a mesh shader...
                    (ExecutionModel::MeshEXT | ExecutionModel::MeshNV, StorageClass::Output, _) => {
                        true
                    }
                    // > if the input is decorated with PerVertexKHR, and is declared in a fragment shader...
                    (ExecutionModel::Fragment, StorageClass::Input, Some(_)) => true,
                    // > if in any other case...
                    (_, _, _) => false,
                };
            if can_skip_outer_array {
                spirv_type = match spirv_type {
                    SpirvType::Array { element, .. }
                    | SpirvType::RuntimeArray { element, .. }
                    | SpirvType::Pointer {
                        pointee: element, ..
                    } => self.lookup_type(element),
                    e => e,
                };
            }

            if let Some(location_size) = spirv_type.location_size(self) {
                *location += location_size;
            } else {
                *location += 1;
                self.tcx.dcx().span_err(
                    hir_param.ty_span,
                    "Type not supported in Input or Output declarations",
                );
            }
        };

        // Emit `OpDecorate`s based on attributes.
        let mut decoration_supersedes_location = false;
        if let Some(builtin) = attrs.builtin {
            if let Err(SpecConstant { .. }) = storage_class {
                self.tcx.dcx().span_fatal(
                    builtin.span,
                    format!(
                        "`#[spirv(spec_constant)]` cannot be `{:?}` builtin",
                        builtin.value
                    ),
                );
            }
            self.emit_global().decorate(
                var_id.unwrap(),
                Decoration::BuiltIn,
                std::iter::once(Operand::BuiltIn(builtin.value)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(descriptor_set) = attrs.descriptor_set {
            if let Err(SpecConstant { .. }) = storage_class {
                self.tcx.dcx().span_fatal(
                    descriptor_set.span,
                    "`#[spirv(descriptor_set = ...)]` cannot apply to `#[spirv(spec_constant)]`",
                );
            }
            self.emit_global().decorate(
                var_id.unwrap(),
                Decoration::DescriptorSet,
                std::iter::once(Operand::LiteralBit32(descriptor_set.value)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(binding) = attrs.binding {
            if let Err(SpecConstant { .. }) = storage_class {
                self.tcx.dcx().span_fatal(
                    binding.span,
                    "`#[spirv(binding = ...)]` cannot apply to `#[spirv(spec_constant)]`",
                );
            }
            self.emit_global().decorate(
                var_id.unwrap(),
                Decoration::Binding,
                std::iter::once(Operand::LiteralBit32(binding.value)),
            );
            decoration_supersedes_location = true;
        }
        if let Some(location) = attrs.location {
            if let Err(SpecConstant { .. }) = storage_class {
                self.tcx.dcx().span_fatal(
                    location.span,
                    "`#[spirv(location = ...)]` cannot apply to `#[spirv(spec_constant)]`",
                );
            }
            if attrs.descriptor_set.is_some() {
                self.tcx.dcx().span_fatal(
                    location.span,
                    "`#[spirv(location = ...)]` cannot be combined with `#[spirv(descriptor_set = ...)]`",
                );
            }
            if attrs.binding.is_some() {
                self.tcx.dcx().span_fatal(
                    location.span,
                    "`#[spirv(location = ...)]` cannot be combined with `#[spirv(binding = ...)]`",
                );
            }
            if !has_location {
                self.tcx.dcx().span_fatal(
                    location.span,
                    "`#[spirv(location = ...)]` can only be used on Inputs (declared as plain values, eg. `Vec4`) \
                     or Outputs (declared as mut ref, eg. `&mut Vec4`)",
                );
            }
            assign_location(var_id, Some(location.value));
            decoration_supersedes_location = true;
        }
        if let Some(flat) = attrs.flat {
            if let Err(SpecConstant { .. }) = storage_class {
                self.tcx.dcx().span_fatal(
                    flat.span,
                    "`#[spirv(flat)]` cannot apply to `#[spirv(spec_constant)]`",
                );
            }
            self.emit_global()
                .decorate(var_id.unwrap(), Decoration::Flat, std::iter::empty());
        }
        if let Some(invariant) = attrs.invariant {
            if storage_class != Ok(StorageClass::Output) {
                self.tcx.dcx().span_fatal(
                    invariant.span,
                    "`#[spirv(invariant)]` is only valid on Output variables",
                );
            }
            self.emit_global()
                .decorate(var_id.unwrap(), Decoration::Invariant, std::iter::empty());
        }
        if let Some(per_primitive_ext) = attrs.per_primitive_ext {
            match execution_model {
                ExecutionModel::Fragment => {
                    if storage_class != Ok(StorageClass::Input) {
                        self.tcx.dcx().span_fatal(
                            per_primitive_ext.span,
                            "`#[spirv(per_primitive_ext)]` in fragment shaders is only valid on Input variables",
                        );
                    }
                }
                ExecutionModel::MeshNV | ExecutionModel::MeshEXT => {
                    if storage_class != Ok(StorageClass::Output) {
                        self.tcx.dcx().span_fatal(
                            per_primitive_ext.span,
                            "`#[spirv(per_primitive_ext)]` in mesh shaders is only valid on Output variables",
                        );
                    }
                }
                _ => {
                    self.tcx.dcx().span_fatal(
                        per_primitive_ext.span,
                        "`#[spirv(per_primitive_ext)]` is only valid in fragment or mesh shaders",
                    );
                }
            }

            self.emit_global().decorate(
                var_id.unwrap(),
                Decoration::PerPrimitiveEXT,
                std::iter::empty(),
            );
        }

        let is_subpass_input = match self.lookup_type(value_spirv_type) {
            SpirvType::Image {
                dim: Dim::DimSubpassData,
                ..
            } => true,
            SpirvType::RuntimeArray { element: elt, .. }
            | SpirvType::Array { element: elt, .. } => {
                matches!(
                    self.lookup_type(elt),
                    SpirvType::Image {
                        dim: Dim::DimSubpassData,
                        ..
                    }
                )
            }
            _ => false,
        };
        if let Some(attachment_index) = attrs.input_attachment_index {
            if is_subpass_input {
                self.emit_global().decorate(
                    var_id.unwrap(),
                    Decoration::InputAttachmentIndex,
                    std::iter::once(Operand::LiteralBit32(attachment_index.value)),
                );
            } else {
                self.tcx.dcx().span_err(
                    attachment_index.span,
                    "#[spirv(input_attachment_index)] is only valid on Image types with dim = SubpassData"
                );
            }
            decoration_supersedes_location = true;
        } else if is_subpass_input {
            self.tcx.dcx().span_err(
                hir_param.ty_span,
                "Image types with dim = SubpassData require #[spirv(input_attachment_index)] decoration",
            );
        }

        // Check builtin-specific type requirements.
        if let Some(builtin) = attrs.builtin {
            self.check_builtin_type(hir_param.ty_span, value_layout.ty, builtin);
        }

        if let Ok(storage_class) = storage_class {
            self.check_for_bad_types(
                execution_model,
                hir_param.ty_span,
                var_ptr_spirv_type,
                storage_class,
                attrs.builtin.is_some(),
                attrs.flat,
            );
        }

        // Assign locations from left to right, incrementing each storage class
        // individually.
        // TODO: Is this right for UniformConstant? Do they share locations with
        // input/outpus?
        if !decoration_supersedes_location && has_location {
            assign_location(var_id, None);
        }

        match storage_class {
            Ok(storage_class) => {
                let var = var_id.unwrap();

                // Emit the `OpVariable` with its *Result* ID set to `var_id`.
                self.emit_global()
                    .variable(var_ptr_spirv_type, Some(var), storage_class, None);

                // Record this `OpVariable` as needing to be added (if applicable),
                // to the *Interface* operands of the `OpEntryPoint` instruction.
                if self.emit_global().version().unwrap() > (1, 3) {
                    // SPIR-V >= v1.4 includes all OpVariables in the interface.
                    op_entry_point_interface_operands.push(var);
                } else {
                    // SPIR-V <= v1.3 only includes Input and Output in the interface.
                    if storage_class == StorageClass::Input || storage_class == StorageClass::Output
                    {
                        op_entry_point_interface_operands.push(var);
                    }
                }
            }
            Err(not_var) => {
                // Emitted earlier.
                let SpecConstant { .. } = not_var;
            }
        }
    }

    // Booleans are only allowed in some storage classes. Error if they're in others.
    // Integers and `f64`s must be decorated with `#[spirv(flat)]`.
    fn check_for_bad_types(
        &self,
        execution_model: ExecutionModel,
        span: Span,
        ty: Word,
        storage_class: StorageClass,
        is_builtin: bool,
        flat_attr: Option<Spanned<()>>,
    ) {
        // private and function are allowed here, but they can't happen.
        if matches!(
            storage_class,
            StorageClass::Workgroup | StorageClass::CrossWorkgroup
        ) {
            return;
        }

        let mut has_bool = false;
        let mut type_must_be_flat = false;
        recurse(self, ty, &mut has_bool, &mut type_must_be_flat);

        // SPIR-V technically allows all input/output variables to be booleans, not just builtins,
        // but has a note:
        // > Khronos Issue #363: OpTypeBool can be used in the Input and Output storage classes,
        //   but the client APIs still only allow built-in Boolean variables (e.g. FrontFacing),
        //   not user variables.
        // spirv-val disallows non-builtin inputs/outputs, so we do too, I guess.
        if has_bool
            && !(is_builtin && matches!(storage_class, StorageClass::Input | StorageClass::Output))
        {
            self.tcx
                .dcx()
                .span_err(span, "entry-point parameter cannot contain `bool`s");
        }

        // Enforce Vulkan validation rules around `Flat` as accurately as possible,
        // i.e. "interpolation control" can only be used "within" the rasterization
        // pipeline (roughly: `vertex (outputs) -> ... -> (inputs for) fragment`),
        // but not at the "outer" interface (vertex inputs/fragment outputs).
        // Also, fragment inputs *require* it for some ("uninterpolatable") types.
        // FIXME(eddyb) maybe this kind of `enum` could be placed elsewhere?
        enum Force {
            Disallow,
            Require,
        }
        #[allow(clippy::match_same_arms)]
        let flat_forced = match (execution_model, storage_class) {
            // VUID-StandaloneSpirv-Flat-06202
            // > The `Flat`, `NoPerspective`, `Sample`, and `Centroid` decorations **must**
            // > not be used on variables with the `Input` storage class in a vertex shader
            (ExecutionModel::Vertex, StorageClass::Input) => Some(Force::Disallow),

            // VUID-StandaloneSpirv-Flat-04744
            // > Any variable with integer or double-precision floating-point type and
            // > with `Input` storage class in a fragment shader, **must** be decorated `Flat`
            (ExecutionModel::Fragment, StorageClass::Input) if type_must_be_flat => {
                // FIXME(eddyb) shouldn't this be automatic then? (maybe with a warning?)
                Some(Force::Require)
            }

            // VUID-StandaloneSpirv-Flat-06201
            // > The `Flat`, `NoPerspective`, `Sample`, and `Centroid` decorations **must**
            // > not be used on variables with the `Output` storage class in a fragment shader
            (ExecutionModel::Fragment, StorageClass::Output) => Some(Force::Disallow),

            // VUID-StandaloneSpirv-Flat-04670
            // > The `Flat`, `NoPerspective`, `Sample`, and `Centroid` decorations **must**
            // > only be used on variables with the `Output` or `Input` storage class
            (_, StorageClass::Input | StorageClass::Output) => None,
            _ => Some(Force::Disallow),
        };

        let flat_mismatch = match (flat_forced, flat_attr) {
            (Some(Force::Disallow), Some(flat_attr)) => Some((flat_attr.span, "cannot")),
            // FIXME(eddyb) it would be useful to show the type that required it.
            (Some(Force::Require), None) => Some((span, "must")),
            _ => None,
        };
        if let Some((span, must_or_cannot)) = flat_mismatch {
            self.tcx.dcx().span_err(
                span,
                format!(
                    "`{execution_model:?}` entry-point `{storage_class:?}` parameter \
                     {must_or_cannot} be decorated with `#[spirv(flat)]`"
                ),
            );
        }

        fn recurse(cx: &CodegenCx<'_>, ty: Word, has_bool: &mut bool, must_be_flat: &mut bool) {
            match cx.lookup_type(ty) {
                SpirvType::Bool => *has_bool = true,
                SpirvType::Integer(_, _) | SpirvType::Float(64) => *must_be_flat = true,
                SpirvType::Adt { field_types, .. } => {
                    for &f in field_types {
                        recurse(cx, f, has_bool, must_be_flat);
                    }
                }
                SpirvType::Vector { element, .. }
                | SpirvType::Matrix { element, .. }
                | SpirvType::Array { element, .. }
                | SpirvType::RuntimeArray { element }
                | SpirvType::Pointer { pointee: element }
                | SpirvType::InterfaceBlock {
                    inner_type: element,
                } => recurse(cx, element, has_bool, must_be_flat),
                SpirvType::Function {
                    return_type,
                    arguments,
                } => {
                    recurse(cx, return_type, has_bool, must_be_flat);
                    for &a in arguments {
                        recurse(cx, a, has_bool, must_be_flat);
                    }
                }
                _ => (),
            }
        }
    }

    /// Check that builtin variables have the correct type.
    fn check_builtin_type(&self, span: Span, rust_ty: Ty<'tcx>, builtin: Spanned<BuiltIn>) {
        // LocalInvocationIndex must be a u32.
        if builtin.value == BuiltIn::LocalInvocationIndex && rust_ty != self.tcx.types.u32 {
            self.tcx.dcx().span_err(
                span,
                format!("`#[spirv(local_invocation_index)]` must be a `u32`, not `{rust_ty}`"),
            );
        }
    }
}
