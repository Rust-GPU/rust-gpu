//! This file is responsible for translation from rustc tys (`TyAndLayout`) to spir-v types. It's
//! surprisingly difficult.

use crate::attr::{AggregatedSpirvAttributes, IntrinsicType};
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use itertools::Itertools;
use rspirv::spirv::{Dim, ImageFormat, Word};
use rustc_abi::ExternAbi as Abi;
use rustc_abi::{
    AddressSpace, Align, BackendRepr, FieldIdx, FieldsShape, Primitive, Scalar, Size, VariantIdx,
    Variants,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_errors::ErrorGuaranteed;
use rustc_index::Idx;
use rustc_middle::query::Providers;
use rustc_middle::ty::layout::{FnAbiOf, LayoutOf, TyAndLayout};
use rustc_middle::ty::{
    self, Const, CoroutineArgs, CoroutineArgsExt as _, FloatTy, IntTy, PolyFnSig, Ty, TyCtxt,
    TyKind, UintTy,
};
use rustc_middle::ty::{GenericArgsRef, ScalarInt};
use rustc_middle::{bug, span_bug};
use rustc_span::DUMMY_SP;
use rustc_span::def_id::DefId;
use rustc_span::{Span, Symbol};
use rustc_target::callconv::{ArgAbi, ArgAttributes, FnAbi, PassMode};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::fmt;

pub(crate) fn provide(providers: &mut Providers) {
    // This is a lil weird: so, we obviously don't support C ABIs at all. However, libcore does declare some extern
    // C functions:
    // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/library/core/src/slice/cmp.rs#L119
    // However, those functions will be implemented by compiler-builtins:
    // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/library/core/src/lib.rs#L23-L27
    // This theoretically then should be fine to leave as C, but, there's no backend hook for
    // `FnAbi::adjust_for_cabi`, causing it to panic:
    // https://github.com/rust-lang/rust/blob/5fae56971d8487088c0099c82c0a5ce1638b5f62/compiler/rustc_target/src/abi/call/mod.rs#L603
    // So, treat any `extern "C"` functions as `extern "unadjusted"`, to be able to compile libcore with arch=spirv.
    providers.fn_sig = |tcx, def_id| {
        // We can't capture the old fn_sig and just call that, because fn_sig is a `fn`, not a `Fn`, i.e. it can't
        // capture variables. Fortunately, the defaults are exposed (thanks rustdoc), so use that instead.
        let result = (rustc_interface::DEFAULT_QUERY_PROVIDERS.fn_sig)(tcx, def_id);
        result.map_bound(|outer| {
            outer.map_bound(|mut inner| {
                if let Abi::C { .. } = inner.abi {
                    inner.abi = Abi::Unadjusted;
                }
                inner
            })
        })
    };

    // For the Rust ABI, `FnAbi` adjustments are backend-agnostic, but they will
    // use features like `PassMode::Cast`, that are incompatible with SPIR-V.
    // By hooking the queries computing `FnAbi`s, we can recompute the `FnAbi`
    // from the return/args layouts, to e.g. prefer using `PassMode::Direct`.
    fn readjust_fn_abi<'tcx>(
        tcx: TyCtxt<'tcx>,
        fn_abi: &'tcx FnAbi<'tcx, Ty<'tcx>>,
    ) -> &'tcx FnAbi<'tcx, Ty<'tcx>> {
        let readjust_arg_abi = |arg: &ArgAbi<'tcx, Ty<'tcx>>| {
            let mut arg = ArgAbi::new(&tcx, arg.layout, |_, _, _| ArgAttributes::new());
            // FIXME: this is bad! https://github.com/rust-lang/rust/issues/115666
            // <https://github.com/rust-lang/rust/commit/eaaa03faf77b157907894a4207d8378ecaec7b45>
            arg.make_direct_deprecated();

            // FIXME(eddyb) detect `#[rust_gpu::vector::v1]` more specifically,
            // to avoid affecting anything should actually be passed as a pair.
            if let PassMode::Pair(..) = arg.mode {
                // HACK(eddyb) this avoids breaking e.g. `&[T]` pairs.
                if let TyKind::Adt(..) = arg.layout.ty.kind() {
                    arg.mode = PassMode::Direct(ArgAttributes::new());
                }
            }

            // Avoid pointlessly passing ZSTs, just like the official Rust ABI.
            if arg.layout.is_zst() {
                arg.mode = PassMode::Ignore;
            }

            arg
        };
        tcx.arena.alloc(FnAbi {
            args: fn_abi.args.iter().map(readjust_arg_abi).collect(),
            ret: readjust_arg_abi(&fn_abi.ret),

            // FIXME(eddyb) validate some of these, and report errors - however,
            // we can't just emit errors from here, since we have no `Span`, so
            // we should have instead a check on MIR for e.g. C variadic calls.
            c_variadic: fn_abi.c_variadic,
            fixed_count: fn_abi.fixed_count,
            conv: fn_abi.conv,
            can_unwind: fn_abi.can_unwind,
        })
    }
    providers.fn_abi_of_fn_ptr = |tcx, key| {
        let result = (rustc_interface::DEFAULT_QUERY_PROVIDERS.fn_abi_of_fn_ptr)(tcx, key);
        Ok(readjust_fn_abi(tcx, result?))
    };
    providers.fn_abi_of_instance = |tcx, key| {
        let result = (rustc_interface::DEFAULT_QUERY_PROVIDERS.fn_abi_of_instance)(tcx, key);
        Ok(readjust_fn_abi(tcx, result?))
    };

    // HACK(eddyb) work around https://github.com/rust-lang/rust/pull/132173
    // (and further changes from https://github.com/rust-lang/rust/pull/132843)
    // starting to ban SIMD ABI misuse (or at least starting to warn about it).
    //
    // FIXME(eddyb) same as the FIXME comment on `check_well_formed`:
    // need to migrate away from `#[repr(simd)]` ASAP.
    providers.check_mono_item = |_, _| {};
}

/// If a struct contains a pointer to itself, even indirectly, then doing a naiive recursive walk
/// of the fields will result in an infinite loop. Because pointers are the only thing that are
/// allowed to be recursive, keep track of what pointers we've translated, or are currently in the
/// progress of translating, and break the recursion that way. This struct manages that state
/// tracking.
#[derive(Default)]
pub struct PointeeCycleDetector<'tcx> {
    // FIXME(eddyb) this should not be responsible for caching the resulting ID!
    map: RefCell<FxHashMap<PointeeTy<'tcx>, CycleDetectionState<Word>>>,
}

enum CycleDetectionState<T> {
    InProgress,
    Done(T),
}

impl<'tcx> PointeeCycleDetector<'tcx> {
    fn begin(
        &self,
        cx: &CodegenCx<'tcx>,
        span: Span,
        pointee: PointeeTy<'tcx>,
        addr_space: AddressSpace,
    ) -> Option<Word> {
        match self.map.borrow_mut().entry(pointee) {
            // State: This is the first time we've seen this type. Record that we're beginning to translate this type,
            // and start doing the translation.
            Entry::Vacant(entry) => {
                entry.insert(CycleDetectionState::InProgress);
                None
            }
            Entry::Occupied(mut entry) => match *entry.get() {
                // State: This is the second time we've seen this type, and we're already translating this type. If we
                // were to try to translate the type now, we'd get a stack overflow, due to continually recursing. So,
                // emit an OpTypePointer, and use that ID. (This is the juicy part of this algorithm)
                CycleDetectionState::InProgress => {
                    let id = SpirvType::Pointer {
                        pointee: None,
                        addr_space,
                    }
                    .def(span, cx);
                    entry.insert(CycleDetectionState::Done(id));
                    Some(id)
                }
                // State: This is the third or more time we've seen this type, and we've already emitted an
                // OpTypePointer. Just use the ID we've already emitted.
                CycleDetectionState::Done(id) => Some(id),
            },
        }
    }

    fn end(
        &self,
        cx: &CodegenCx<'tcx>,
        span: Span,
        pointee: PointeeTy<'tcx>,
        pointee_spv: Word,
        addr_space: AddressSpace,
    ) -> Word {
        match self.map.borrow_mut().entry(pointee) {
            // We should have hit begin() on this type already, which always inserts an entry.
            Entry::Vacant(_) => {
                span_bug!(span, "PointeeCycleDetector::end should always have entry")
            }
            Entry::Occupied(mut entry) => match *entry.get() {
                // State: There have been no recursive references to this type while defining it, and so no
                // OpTypePointer has been emitted. This is the most common case.
                CycleDetectionState::InProgress => {
                    let id = SpirvType::Pointer {
                        pointee: Some(pointee_spv),
                        addr_space,
                    }
                    .def(span, cx);
                    entry.insert(CycleDetectionState::Done(id));
                    id
                }
                // State: There was a recursive reference to this type, and so an OpTypePointer has been emitted.
                // Make sure to use the same ID.
                CycleDetectionState::Done(id) => id,
            },
        }
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
enum PointeeTy<'tcx> {
    Ty(TyAndLayout<'tcx>),
    Fn(PolyFnSig<'tcx>),
}

impl fmt::Display for PointeeTy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PointeeTy::Ty(ty) => write!(f, "{}", ty.ty),
            PointeeTy::Fn(ty) => write!(f, "{ty}"),
        }
    }
}
/// Various type-like things can be converted to a spirv type - normal types, function types, etc. - and this trait
/// provides a uniform way of translating them.
pub trait ConvSpirvType<'tcx> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word;
}

impl<'tcx> ConvSpirvType<'tcx> for PointeeTy<'tcx> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        match *self {
            PointeeTy::Ty(ty) => ty.spirv_type(span, cx),
            PointeeTy::Fn(ty) => cx
                .fn_abi_of_fn_ptr(ty, ty::List::empty())
                .spirv_type(span, cx),
        }
    }
}

impl<'tcx> ConvSpirvType<'tcx> for FnAbi<'tcx, Ty<'tcx>> {
    fn spirv_type(&self, span: Span, cx: &CodegenCx<'tcx>) -> Word {
        // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
        let mut argument_types = Vec::new();

        let return_type = match self.ret.mode {
            PassMode::Ignore => SpirvType::Void.def(span, cx),
            PassMode::Direct(_) | PassMode::Pair(..) => self.ret.layout.spirv_type(span, cx),
            PassMode::Cast { .. } | PassMode::Indirect { .. } => span_bug!(
                span,
                "query hooks should've made this `PassMode` impossible: {:#?}",
                self.ret
            ),
        };

        for arg in self.args.iter() {
            let arg_type = match arg.mode {
                PassMode::Ignore => continue,
                PassMode::Direct(_) => arg.layout.spirv_type(span, cx),
                PassMode::Pair(_, _) => {
                    argument_types.push(scalar_pair_element_backend_type(cx, span, arg.layout, 0));
                    argument_types.push(scalar_pair_element_backend_type(cx, span, arg.layout, 1));
                    continue;
                }
                PassMode::Cast { .. } | PassMode::Indirect { .. } => span_bug!(
                    span,
                    "query hooks should've made this `PassMode` impossible: {:#?}",
                    arg
                ),
            };
            argument_types.push(arg_type);
        }

        SpirvType::Function {
            return_type,
            arguments: &argument_types,
        }
        .def(span, cx)
    }
}

impl<'tcx> ConvSpirvType<'tcx> for TyAndLayout<'tcx> {
    fn spirv_type(&self, mut span: Span, cx: &CodegenCx<'tcx>) -> Word {
        if let TyKind::Adt(adt, args) = *self.ty.kind() {
            if span == DUMMY_SP {
                span = cx.tcx.def_span(adt.did());
            }

            let attrs = AggregatedSpirvAttributes::parse(cx, cx.tcx.get_attrs_unchecked(adt.did()));

            if let Some(intrinsic_type_attr) = attrs.intrinsic_type.map(|attr| attr.value)
                && let Ok(spirv_type) =
                    trans_intrinsic_type(cx, span, *self, args, intrinsic_type_attr)
            {
                return spirv_type;
            }
        }

        // Note: ty.layout is orthogonal to ty.ty, e.g. `ManuallyDrop<Result<isize, isize>>` has abi
        // `ScalarPair`.
        // There's a few layers that we go through here. First we inspect layout.backend_repr, then if relevant, layout.fields, etc.
        match self.backend_repr {
            _ if self.uninhabited => SpirvType::Adt {
                def_id: def_id_for_spirv_type_adt(*self),
                size: Some(Size::ZERO),
                align: Align::from_bytes(0).unwrap(),
                field_types: &[],
                field_offsets: &[],
                field_names: None,
            }
            .def_with_name(cx, span, TyLayoutNameKey::from(*self)),
            BackendRepr::Scalar(scalar) => trans_scalar(cx, span, *self, scalar, Size::ZERO),
            BackendRepr::ScalarPair(a, b) => {
                // NOTE(eddyb) unlike `BackendRepr::Scalar`'s simpler newtype-unpacking
                // behavior, `BackendRepr::ScalarPair` can be composed in two ways:
                // * two `BackendRepr::Scalar` fields (and any number of ZST fields),
                //   gets handled the same as a `struct { a, b }`, further below
                // * an `BackendRepr::ScalarPair` field (and any number of ZST fields),
                //   which requires more work to allow taking a reference to
                //   that field, and there are two potential approaches:
                //   1. wrapping that field's SPIR-V type in a single-field
                //      `OpTypeStruct` - this has the disadvantage that GEPs
                //      would have to inject an extra `0` field index, and other
                //      field-related operations would also need additional work
                //   2. reusing that field's SPIR-V type, instead of defining
                //      a new one, offering the `(a, b)` shape `rustc_codegen_ssa`
                //      expects, while letting noop pointercasts access the sole
                //      `BackendRepr::ScalarPair` field - this is the approach taken here
                let mut non_zst_fields = (0..self.fields.count())
                    .map(|i| (i, self.field(cx, i)))
                    .filter(|(_, field)| !field.is_zst());
                let sole_non_zst_field = match (non_zst_fields.next(), non_zst_fields.next()) {
                    (Some(field), None) => Some(field),
                    _ => None,
                };
                if let Some((i, field)) = sole_non_zst_field {
                    // Only unpack a newtype if the field and the newtype line up
                    // perfectly, in every way that could potentially affect ABI.
                    if self.fields.offset(i) == Size::ZERO
                        && field.size == self.size
                        && field.align.abi == self.align.abi
                        && field.backend_repr.eq_up_to_validity(&self.backend_repr)
                    {
                        return field.spirv_type(span, cx);
                    }
                }

                // Note: We can't use auto_struct_layout here because the spirv types here might be undefined due to
                // recursive pointer types.
                let a_offset = Size::ZERO;
                let b_offset = a.primitive().size(cx).align_to(b.primitive().align(cx).abi);
                let a = trans_scalar(cx, span, *self, a, a_offset);
                let b = trans_scalar(cx, span, *self, b, b_offset);
                let size = if self.is_unsized() {
                    None
                } else {
                    Some(self.size)
                };
                // FIXME(eddyb) use `ArrayVec` here.
                let mut field_names = Vec::new();
                if let TyKind::Adt(adt, _) = self.ty.kind()
                    && let Variants::Single { index } = self.variants
                {
                    for i in self.fields.index_by_increasing_offset() {
                        let field = &adt.variants()[index].fields[FieldIdx::new(i)];
                        field_names.push(field.name);
                    }
                }
                SpirvType::Adt {
                    def_id: def_id_for_spirv_type_adt(*self),
                    size,
                    align: self.align.abi,
                    field_types: &[a, b],
                    field_offsets: &[a_offset, b_offset],
                    field_names: if field_names.len() == 2 {
                        Some(&field_names)
                    } else {
                        None
                    },
                }
                .def_with_name(cx, span, TyLayoutNameKey::from(*self))
            }
            BackendRepr::SimdVector { element, count } => {
                let elem_spirv = trans_scalar(cx, span, *self, element, Size::ZERO);
                SpirvType::Vector {
                    element: elem_spirv,
                    count: count as u32,
                    size: self.size,
                    align: self.align.abi,
                }
                .def(span, cx)
            }
            BackendRepr::Memory { sized: _ } => trans_aggregate(cx, span, *self),
        }
    }
}

/// Only pub for `LayoutTypeCodegenMethods::scalar_pair_element_backend_type`. Think about what you're
/// doing before calling this.
pub fn scalar_pair_element_backend_type<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    index: usize,
) -> Word {
    let [a, b] = match ty.layout.backend_repr() {
        BackendRepr::ScalarPair(a, b) => [a, b],
        other => span_bug!(
            span,
            "scalar_pair_element_backend_type invalid abi: {:?}",
            other
        ),
    };
    let offset = match index {
        0 => Size::ZERO,
        1 => a.primitive().size(cx).align_to(b.primitive().align(cx).abi),
        _ => unreachable!(),
    };
    trans_scalar(cx, span, ty, [a, b][index], offset)
}

/// A "scalar" is a basic building block: bools, ints, floats, pointers. (i.e. not something complex like a struct)
/// A "scalar pair" is a bit of a strange concept: if there is a `fn f(x: (u32, u32))`, then what's preferred for
/// performance is to compile that ABI to `f(x_1: u32, x_2: u32)`, i.e. splitting out the pair into their own arguments,
/// and pretending that they're one unit. So, there's quite a bit of special handling around these scalar pairs to enable
/// scenarios like that.
/// I say it's "preferred", but spirv doesn't really care - only CPU ABIs really care here. However, following rustc's
/// lead and doing what they want makes things go smoothly, so we'll implement it here too.
fn trans_scalar<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    scalar: Scalar,
    offset: Size,
) -> Word {
    if scalar.is_bool() {
        return SpirvType::Bool.def(span, cx);
    }

    match scalar.primitive() {
        Primitive::Int(int_kind, signedness) => {
            SpirvType::Integer(int_kind.size().bits() as u32, signedness).def(span, cx)
        }
        Primitive::Float(float_kind) => {
            SpirvType::Float(float_kind.size().bits() as u32).def(span, cx)
        }
        Primitive::Pointer(addr_space) => {
            let pointee_ty = dig_scalar_pointee(cx, ty, offset);
            // Pointers can be recursive. So, record what we're currently translating, and if we're already translating
            // the same type, emit an OpTypePointer and use that ID.
            if let Some(predefined_result) = cx
                .type_cache
                .pointee_cycle_detector
                .begin(cx, span, pointee_ty, addr_space)
            {
                predefined_result
            } else {
                let pointee = pointee_ty.spirv_type(span, cx);
                cx.type_cache
                    .pointee_cycle_detector
                    .end(cx, span, pointee_ty, pointee, addr_space)
            }
        }
    }
}

// This is a really weird function, strap in...
// So, rustc_codegen_ssa is designed around scalar pointers being opaque, you shouldn't know the type behind the
// pointer. Unfortunately, that's impossible for us, we need to know the underlying pointee type for various reasons. In
// some cases, this is pretty easy - if it's a TyKind::Ref, then the pointee will be the pointee of the ref (with
// handling for wide pointers, etc.). Unfortunately, there's some pretty advanced processing going on in cx.layout_of:
// for example, `ManuallyDrop<Result<ptr, ptr>>` has abi `ScalarPair`. This means that to figure out the pointee type,
// we have to replicate the logic of cx.layout_of. Part of that is digging into types that are aggregates: for example,
// ManuallyDrop<T> has a single field of type T. We "dig into" that field, and recurse, trying to find a base case that
// we can handle, like TyKind::Ref.
// If the above didn't make sense, please poke Ashley, it's probably easier to explain via conversation.
fn dig_scalar_pointee<'tcx>(
    cx: &CodegenCx<'tcx>,
    layout: TyAndLayout<'tcx>,
    offset: Size,
) -> PointeeTy<'tcx> {
    if let FieldsShape::Primitive = layout.fields {
        assert_eq!(offset, Size::ZERO);
        let pointee = match *layout.ty.kind() {
            TyKind::Ref(_, pointee_ty, _) | TyKind::RawPtr(pointee_ty, _) => {
                PointeeTy::Ty(cx.layout_of(pointee_ty))
            }
            TyKind::FnPtr(sig_tys, hdr) => PointeeTy::Fn(sig_tys.with(hdr)),
            _ => bug!("Pointer is not `&T`, `*T` or `fn` pointer: {:#?}", layout),
        };
        return pointee;
    }

    let all_fields = (match &layout.variants {
        Variants::Empty => 0..0,
        Variants::Multiple { variants, .. } => 0..variants.len(),
        Variants::Single { index } => {
            let i = index.as_usize();
            i..i + 1
        }
    })
    .flat_map(|variant_idx| {
        let variant = layout.for_variant(cx, VariantIdx::new(variant_idx));
        (0..variant.fields.count()).map(move |field_idx| {
            (
                variant.field(cx, field_idx),
                variant.fields.offset(field_idx),
            )
        })
    });

    let mut pointee = None;
    for (field, field_offset) in all_fields {
        if field.is_zst() {
            continue;
        }
        if (field_offset..field_offset + field.size).contains(&offset) {
            let new_pointee = dig_scalar_pointee(cx, field, offset - field_offset);
            match pointee {
                Some(old_pointee) if old_pointee != new_pointee => {
                    cx.tcx.dcx().fatal(format!(
                        "dig_scalar_pointee: unsupported Pointer with different \
                         pointee types ({old_pointee:?} vs {new_pointee:?}) at offset {offset:?} in {layout:#?}"
                    ));
                }
                _ => pointee = Some(new_pointee),
            }
        }
    }
    pointee.unwrap_or_else(|| {
        bug!(
            "field containing Pointer scalar at offset {:?} not found in {:#?}",
            offset,
            layout
        )
    })
}

// FIXME(eddyb) all `ty: TyAndLayout` variables should be `layout: TyAndLayout`,
// the type is really more "Layout with Ty" (`.ty` field + `Deref`s to `Layout`).
fn trans_aggregate<'tcx>(cx: &CodegenCx<'tcx>, span: Span, ty: TyAndLayout<'tcx>) -> Word {
    fn create_zst<'tcx>(cx: &CodegenCx<'tcx>, span: Span, ty: TyAndLayout<'tcx>) -> Word {
        assert_eq!(ty.size, Size::ZERO);
        SpirvType::Adt {
            def_id: def_id_for_spirv_type_adt(ty),
            size: Some(Size::ZERO),
            align: ty.align.abi,
            field_types: &[],
            field_offsets: &[],
            field_names: None,
        }
        .def_with_name(cx, span, TyLayoutNameKey::from(ty))
    }
    match ty.fields {
        FieldsShape::Primitive => span_bug!(
            span,
            "trans_aggregate called for FieldsShape::Primitive layout {:#?}",
            ty
        ),
        FieldsShape::Union(_) => {
            assert!(!ty.is_unsized(), "{ty:#?}");

            // Represent the `union` with its sole non-ZST case, which should work
            // for at least `MaybeUninit<T>` (which is an union of `T` and `()`).
            // NOTE(eddyb) even if long-term this may become a byte array, that
            // only works for "data types" and not "opaque handles" (images etc.).
            let mut non_zst_cases = (0..ty.fields.count())
                .map(|i| (FieldIdx::from_usize(i), ty.field(cx, i)))
                .filter(|(_, case)| !case.is_zst());
            match (non_zst_cases.next(), non_zst_cases.next()) {
                (None, _) if ty.is_1zst() => create_zst(cx, span, ty),
                (Some(sole_non_zst_case), None) => {
                    let (case_idx, case) = sole_non_zst_case;
                    if ty.align != case.align {
                        // HACK(eddyb) mismatched alignment requires a wrapper `struct`.
                        trans_struct_or_union(cx, span, ty, Some(case_idx))
                    } else {
                        assert_eq!(ty.size, case.size);
                        case.spirv_type(span, cx)
                    }
                }

                // FIXME(eddyb) can't risk using the largest case when smaller
                // non-ZST cases exist, at least not without additional effort
                // of guaranteeing that the largest case subsumes all smaller
                // cases (at the very least in terms of padding).
                (nzc0, nzc1) => {
                    let non_zst_cases = nzc0.into_iter().chain(nzc1).chain(non_zst_cases);

                    // FIXME(eddyb) in theory, the `sole_non_zst_case` logic
                    // above, which allows generating a wrapper `struct` for
                    // mismatched alignments, could also be employed here,
                    // but for now that information (case index) is erased.
                    let non_zst_cases = non_zst_cases.map(|(_, case)| case);

                    // HACK(eddyb) still try to find a viable candidate case to
                    // subsume all others (at least through simple heuristics),
                    // as some `union`s are commonly used similarly to `transmute`
                    // (e.g. `core::ptr::metadata::PtrRepr`) and therefore have
                    // multiple cases of the same size (and very similar types).
                    let fully_filling_cases = non_zst_cases.filter(|case| {
                        let leaf_scalars_total_unpadded_size = match case.backend_repr {
                            _ if case.uninhabited => Size::ZERO,
                            BackendRepr::Scalar(s) => s.size(cx),
                            BackendRepr::SimdVector { element, count } => element.size(cx) * count,
                            BackendRepr::ScalarPair(a, b) => a.size(cx) + b.size(cx),
                            // FIXME(eddyb) support by recursing? but also this
                            // "require no padding" approach isn't general enough.
                            BackendRepr::Memory { .. } => return false,
                        };
                        case.size == leaf_scalars_total_unpadded_size
                            && ty.size == case.size
                            && ty.align.abi == case.align.abi
                    });

                    // FIXME(eddyb) this picks just the first `union` case that
                    // happens to fit the whole `union` size with no padding,
                    // which might work but isn't a good general approach.
                    let filler_type_from_case = fully_filling_cases
                        .map(|case| case.spirv_type(span, cx))
                        .next();

                    let filler_type = filler_type_from_case
                        .unwrap_or_else(|| cx.type_padding_filler(ty.size, ty.align.abi));

                    if true {
                        filler_type
                    } else {
                        // HACK(eddyb) this only exists to give a name to the `union`.
                        SpirvType::Adt {
                            def_id: def_id_for_spirv_type_adt(ty),
                            size: Some(ty.size),
                            align: ty.align.abi,
                            field_types: &[filler_type],
                            field_offsets: &[Size::ZERO],
                            field_names: None,
                        }
                        .def_with_name(cx, span, TyLayoutNameKey::from(ty))
                    }
                }
            }
        }
        FieldsShape::Array { stride, count } => {
            let element_type = ty.field(cx, 0).spirv_type(span, cx);
            if ty.is_unsized() {
                // There's a potential for this array to be sized, but the element to be unsized, e.g. `[[u8]; 5]`.
                // However, I think rust disallows all these cases, so assert this here.
                assert_eq!(count, 0);
                SpirvType::RuntimeArray {
                    element: element_type,
                    stride,
                    // FIXME(eddyb) why even have this distinction? (may break `qptr`)
                    is_physical: cx.lookup_type(element_type).physical_size(cx) == Some(stride),
                }
                .def(span, cx)
            } else if count == 0 {
                // spir-v doesn't support zero-sized arrays
                create_zst(cx, span, ty)
            } else {
                SpirvType::Array {
                    element: element_type,
                    count: cx.constant_u32(span, count as u32),
                    stride,
                    // FIXME(eddyb) why even have this distinction? (may break `qptr`)
                    is_physical: cx.lookup_type(element_type).physical_size(cx) == Some(stride),
                }
                .def(span, cx)
            }
        }
        FieldsShape::Arbitrary {
            offsets: _,
            memory_index: _,
        } => trans_struct_or_union(cx, span, ty, None),
    }
}

// returns (field_offsets, size, align)
pub fn auto_struct_layout(
    cx: &CodegenCx<'_>,
    field_types: &[Word],
    packed: bool,
) -> (Vec<Size>, Option<Size>, Align) {
    // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
    let mut field_offsets = Vec::with_capacity(field_types.len());
    let mut offset = Some(Size::ZERO);
    let mut max_align = Align::ONE;
    for &field_type in field_types {
        let spirv_type = cx.lookup_type(field_type);
        let field_size = spirv_type.sizeof(cx);
        let field_align = if packed {
            Align::ONE
        } else {
            spirv_type.alignof(cx)
        };
        let this_offset = offset
            .expect("Unsized values can only be the last field in a struct")
            .align_to(field_align);

        field_offsets.push(this_offset);
        if field_align > max_align {
            max_align = field_align;
        }
        offset = field_size.map(|size| this_offset + size);
    }
    (field_offsets, offset, max_align)
}

// see struct_llfields in librustc_codegen_llvm for implementation hints
fn trans_struct_or_union<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    union_case: Option<FieldIdx>,
) -> Word {
    let size = if ty.is_unsized() { None } else { Some(ty.size) };
    let align = ty.align.abi;
    // FIXME(eddyb) use `AccumulateVec`s just like `rustc` itself does.
    let mut field_types = Vec::new();
    let mut field_offsets = Vec::new();
    let mut field_names = Vec::new();
    for i in ty.fields.index_by_increasing_offset() {
        if let Some(expected_field_idx) = union_case
            && i != expected_field_idx.as_usize()
        {
            continue;
        }

        let field_ty = ty.field(cx, i);
        field_types.push(field_ty.spirv_type(span, cx));
        let offset = ty.fields.offset(i);
        field_offsets.push(offset);
        if let Variants::Single { index } = ty.variants {
            if let TyKind::Adt(adt, _) = ty.ty.kind() {
                let field = &adt.variants()[index].fields[FieldIdx::new(i)];
                field_names.push(field.name);
            } else {
                // FIXME(eddyb) this looks like something that should exist in rustc.
                field_names.push(Symbol::intern(&format!("{i}")));
            }
        } else {
            if let TyKind::Adt(_, _) = ty.ty.kind() {
            } else {
                span_bug!(span, "Variants::Multiple not TyKind::Adt");
            }
            if i == 0 {
                field_names.push(cx.sym.discriminant);
            } else {
                cx.tcx.dcx().fatal("Variants::Multiple has multiple fields")
            }

            assert_eq!(i, ty.fields.count() - 1);

            // HACK(eddyb) fill the space before/after the discriminant, so
            // that by-value usage of the resulting `OpTypeStruct` doesn't treat
            // variant data as padding (and therefore `undef`).
            // FIXME(eddyb) this is worse than taking advantage of known leaves
            // within the variants, especially with a single non-ZST variant.
            for (pad_start, pad_end) in [(Size::ZERO, offset), (offset + field_ty.size, ty.size)] {
                let pad_align = align
                    .restrict_for_offset(pad_start)
                    .restrict_for_offset(pad_end);
                let pad_size = pad_end - pad_start;
                if pad_size != Size::ZERO {
                    field_types.push(cx.type_padding_filler(pad_size, pad_align));
                    field_offsets.push(pad_start);
                    field_names.push(Symbol::intern(&format!(
                        "_pad_{}_to_{}",
                        pad_start.bytes(),
                        pad_end.bytes()
                    )));
                }
            }
        };
    }
    SpirvType::Adt {
        def_id: def_id_for_spirv_type_adt(ty),
        size,
        align,
        field_types: &field_types,
        field_offsets: &field_offsets,
        field_names: Some(&field_names),
    }
    .def_with_name(cx, span, TyLayoutNameKey::from(ty))
}

/// Grab a `DefId` from the type if possible to avoid too much deduplication,
/// which could result in one SPIR-V `OpType*` having many names
/// (not in itself an issue, but it makes error reporting harder).
fn def_id_for_spirv_type_adt(layout: TyAndLayout<'_>) -> Option<DefId> {
    match *layout.ty.kind() {
        TyKind::Adt(def, _) => Some(def.did()),
        TyKind::Foreign(def_id) | TyKind::Closure(def_id, _) | TyKind::Coroutine(def_id, ..) => {
            Some(def_id)
        }
        _ => None,
    }
}

fn span_for_spirv_type_adt(cx: &CodegenCx<'_>, layout: TyAndLayout<'_>) -> Option<Span> {
    def_id_for_spirv_type_adt(layout).map(|did| cx.tcx.def_span(did))
}

/// Minimal and cheaply comparable/hashable subset of the information contained
/// in `TyLayout` that can be used to generate a name (assuming a nominal type).
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct TyLayoutNameKey<'tcx> {
    ty: Ty<'tcx>,
    variant: Option<VariantIdx>,
}

impl<'tcx> From<TyAndLayout<'tcx>> for TyLayoutNameKey<'tcx> {
    fn from(layout: TyAndLayout<'tcx>) -> Self {
        TyLayoutNameKey {
            ty: layout.ty,
            variant: match layout.variants {
                Variants::Single { index } => Some(index),
                _ => None,
            },
        }
    }
}

impl fmt::Display for TyLayoutNameKey<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty)?;
        if let (TyKind::Adt(def, _), Some(index)) = (self.ty.kind(), self.variant)
            && def.is_enum()
            && !def.variants().is_empty()
        {
            write!(f, "::{}", def.variants()[index].name)?;
        }
        if let (TyKind::Coroutine(_, _), Some(index)) = (self.ty.kind(), self.variant) {
            write!(f, "::{}", CoroutineArgs::variant_name(index))?;
        }
        Ok(())
    }
}

fn trans_intrinsic_type<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    args: GenericArgsRef<'tcx>,
    intrinsic_type_attr: IntrinsicType,
) -> Result<Word, ErrorGuaranteed> {
    match intrinsic_type_attr {
        IntrinsicType::GenericImageType => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                return Err(cx
                    .tcx
                    .dcx()
                    .err("#[spirv(generic_image)] type must have size 4"));
            }

            // fn type_from_variant_discriminant<'tcx, P: FromPrimitive>(
            //     cx: &CodegenCx<'tcx>,
            //     const_: Const<'tcx>,
            // ) -> P {
            //     let adt_def = const_.ty.ty_adt_def().unwrap();
            //     assert!(adt_def.is_enum());
            //     let destructured = cx.tcx.destructure_const(TypingEnv::fully_monomorphized().and(const_));
            //     let idx = destructured.variant.unwrap();
            //     let value = const_.ty.discriminant_for_variant(cx.tcx, idx).unwrap().val as u64;
            //     <_>::from_u64(value).unwrap()
            // }

            let sampled_type = match args.type_at(0).kind() {
                TyKind::Int(int) => match int {
                    IntTy::Isize => {
                        SpirvType::Integer(cx.tcx.data_layout.pointer_size.bits() as u32, true)
                            .def(span, cx)
                    }
                    IntTy::I8 => SpirvType::Integer(8, true).def(span, cx),
                    IntTy::I16 => SpirvType::Integer(16, true).def(span, cx),
                    IntTy::I32 => SpirvType::Integer(32, true).def(span, cx),
                    IntTy::I64 => SpirvType::Integer(64, true).def(span, cx),
                    IntTy::I128 => SpirvType::Integer(128, true).def(span, cx),
                },
                TyKind::Uint(uint) => match uint {
                    UintTy::Usize => {
                        SpirvType::Integer(cx.tcx.data_layout.pointer_size.bits() as u32, false)
                            .def(span, cx)
                    }
                    UintTy::U8 => SpirvType::Integer(8, false).def(span, cx),
                    UintTy::U16 => SpirvType::Integer(16, false).def(span, cx),
                    UintTy::U32 => SpirvType::Integer(32, false).def(span, cx),
                    UintTy::U64 => SpirvType::Integer(64, false).def(span, cx),
                    UintTy::U128 => SpirvType::Integer(128, false).def(span, cx),
                },
                TyKind::Float(FloatTy::F32) => SpirvType::Float(32).def(span, cx),
                TyKind::Float(FloatTy::F64) => SpirvType::Float(64).def(span, cx),
                _ => {
                    return Err(cx
                        .tcx
                        .dcx()
                        .span_err(span, "Invalid sampled type to `Image`."));
                }
            };

            // let dim: spirv::Dim = type_from_variant_discriminant(cx, args.const_at(1));
            // let depth: u32 = type_from_variant_discriminant(cx, args.const_at(2));
            // let arrayed: u32 = type_from_variant_discriminant(cx, args.const_at(3));
            // let multisampled: u32 = type_from_variant_discriminant(cx, args.const_at(4));
            // let sampled: u32 = type_from_variant_discriminant(cx, args.const_at(5));
            // let image_format: spirv::ImageFormat =
            //     type_from_variant_discriminant(cx, args.const_at(6));

            trait FromScalarInt: Sized {
                fn from_scalar_int(n: ScalarInt) -> Option<Self>;
            }

            impl FromScalarInt for u32 {
                fn from_scalar_int(n: ScalarInt) -> Option<Self> {
                    Some(n.try_to_bits(Size::from_bits(32)).ok()?.try_into().unwrap())
                }
            }

            impl FromScalarInt for Dim {
                fn from_scalar_int(n: ScalarInt) -> Option<Self> {
                    Dim::from_u32(u32::from_scalar_int(n)?)
                }
            }

            impl FromScalarInt for ImageFormat {
                fn from_scalar_int(n: ScalarInt) -> Option<Self> {
                    ImageFormat::from_u32(u32::from_scalar_int(n)?)
                }
            }

            fn const_int_value<'tcx, P: FromScalarInt>(
                cx: &CodegenCx<'tcx>,
                const_: Const<'tcx>,
            ) -> Result<P, ErrorGuaranteed> {
                let ty::Value {
                    ty: const_ty,
                    valtree: const_val,
                } = const_.to_value();
                assert!(const_ty.is_integral());
                const_val
                    .try_to_scalar_int()
                    .and_then(P::from_scalar_int)
                    .ok_or_else(|| {
                        cx.tcx
                            .dcx()
                            .err(format!("invalid value for Image const generic: {const_}"))
                    })
            }

            let dim = const_int_value(cx, args.const_at(1))?;
            let depth = const_int_value(cx, args.const_at(2))?;
            let arrayed = const_int_value(cx, args.const_at(3))?;
            let multisampled = const_int_value(cx, args.const_at(4))?;
            let sampled = const_int_value(cx, args.const_at(5))?;
            let image_format = const_int_value(cx, args.const_at(6))?;

            let ty = SpirvType::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
            };
            Ok(ty.def(span, cx))
        }
        IntrinsicType::Sampler => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                return Err(cx.tcx.dcx().err("#[spirv(sampler)] type must have size 4"));
            }
            Ok(SpirvType::Sampler.def(span, cx))
        }
        IntrinsicType::AccelerationStructureKhr => {
            Ok(SpirvType::AccelerationStructureKhr.def(span, cx))
        }
        IntrinsicType::RayQueryKhr => Ok(SpirvType::RayQueryKhr.def(span, cx)),
        IntrinsicType::SampledImage => {
            // see SpirvType::sizeof
            if ty.size != Size::from_bytes(4) {
                return Err(cx
                    .tcx
                    .dcx()
                    .err("#[spirv(sampled_image)] type must have size 4"));
            }

            // We use a generic to indicate the underlying image type of the sampled image.
            // The spirv type of it will be generated by querying the type of the first generic.
            if let Some(image_ty) = args.types().next() {
                // TODO: enforce that the generic param is an image type?
                let image_type = cx.layout_of(image_ty).spirv_type(span, cx);
                Ok(SpirvType::SampledImage { image_type }.def(span, cx))
            } else {
                Err(cx
                    .tcx
                    .dcx()
                    .err("#[spirv(sampled_image)] type must have a generic image type"))
            }
        }
        IntrinsicType::RuntimeArray => {
            if ty.size != Size::from_bytes(4) {
                return Err(cx
                    .tcx
                    .dcx()
                    .err("#[spirv(runtime_array)] type must have size 4"));
            }

            // We use a generic param to indicate the underlying element type.
            // The SPIR-V element type will be generated from the first generic param.
            if let Some(elem_ty) = args.types().next() {
                let elem = cx.layout_of(elem_ty);
                let elem_spirv_type = elem.spirv_type(span, cx);
                Ok(SpirvType::RuntimeArray {
                    element: elem_spirv_type,
                    stride: elem.size,
                    // FIXME(eddyb) why even have this distinction? (may break `qptr`)
                    is_physical: cx.lookup_type(elem_spirv_type).physical_size(cx)
                        == Some(elem.size),
                }
                .def(span, cx))
            } else {
                Err(cx
                    .tcx
                    .dcx()
                    .err("#[spirv(runtime_array)] type must have a generic element type"))
            }
        }
        IntrinsicType::TypedBuffer => {
            if ty.size != Size::from_bytes(4) {
                return Err(cx
                    .tcx
                    .sess
                    .dcx()
                    .err("#[spirv(typed_buffer)] type must have size 4"));
            }

            // We use a generic param to indicate the underlying data type.
            // The SPIR-V data type will be generated from the first generic param.
            if let Some(data_ty) = args.types().next() {
                // HACK(eddyb) this should be a *pointer* to an "interface block",
                // but SPIR-V screwed up and used no explicit indirection for the
                // descriptor indexing case, and instead made a `RuntimeArray` of
                // `InterfaceBlock`s be an "array of typed buffer resources".
                Ok(SpirvType::InterfaceBlock {
                    inner_type: cx.layout_of(data_ty).spirv_type(span, cx),
                }
                .def(span, cx))
            } else {
                Err(cx
                    .tcx
                    .sess
                    .dcx()
                    .err("#[spirv(typed_buffer)] type must have a generic data type"))
            }
        }
        IntrinsicType::Matrix => {
            let span = span_for_spirv_type_adt(cx, ty).unwrap();
            let err_attr_name = "`#[spirv(matrix)]`";
            let (element, count) = trans_glam_like_struct(cx, span, ty, args, err_attr_name)?;
            match cx.lookup_type(element) {
                SpirvType::Vector { .. } => (),
                ty => {
                    return Err(cx
                        .tcx
                        .dcx()
                        .struct_span_err(
                            span,
                            format!("{err_attr_name} type fields must all be vectors"),
                        )
                        .with_note(format!("field type is {}", ty.debug(element, cx)))
                        .emit());
                }
            }
            Ok(SpirvType::Matrix { element, count }.def(span, cx))
        }
        IntrinsicType::Vector => {
            let span = span_for_spirv_type_adt(cx, ty).unwrap();
            let err_attr_name = "`#[spirv(vector)]`";
            let (element, count) = trans_glam_like_struct(cx, span, ty, args, err_attr_name)?;
            match cx.lookup_type(element) {
                SpirvType::Bool | SpirvType::Float { .. } | SpirvType::Integer { .. } => (),
                ty => {
                    return Err(cx
                        .tcx
                        .dcx()
                        .struct_span_err(
                            span,
                            format!(
                                "{err_attr_name} type fields must all be floats, integers or bools"
                            ),
                        )
                        .with_note(format!("field type is {}", ty.debug(element, cx)))
                        .emit());
                }
            }
            Ok(SpirvType::Vector {
                element,
                count,
                size: ty.size,
                align: ty.align.abi,
            }
            .def(span, cx))
        }
    }
}

/// A struct with multiple fields of the same kind.
/// Used for `#[spirv(vector)]` and `#[spirv(matrix)]`.
fn trans_glam_like_struct<'tcx>(
    cx: &CodegenCx<'tcx>,
    span: Span,
    ty: TyAndLayout<'tcx>,
    args: GenericArgsRef<'tcx>,
    err_attr_name: &str,
) -> Result<(Word, u32), ErrorGuaranteed> {
    let tcx = cx.tcx;
    if let Some(adt) = ty.ty.ty_adt_def()
        && adt.is_struct()
    {
        let (count, element) = adt
            .non_enum_variant()
            .fields
            .iter()
            .map(|f| f.ty(tcx, args))
            .dedup_with_count()
            .exactly_one()
            .map_err(|_e| {
                tcx.dcx().span_err(
                    span,
                    format!("{err_attr_name} member types must all be the same"),
                )
            })?;

        let element = cx.layout_of(element);
        let element_word = element.spirv_type(span, cx);
        let count = u32::try_from(count)
            .ok()
            .filter(|count| 2 <= *count && *count <= 4)
            .ok_or_else(|| {
                tcx.dcx()
                    .span_err(span, format!("{err_attr_name} must have 2, 3 or 4 members"))
            })?;

        for i in 0..ty.fields.count() {
            let expected = element.size.checked_mul(i as u64, cx).unwrap();
            let actual = ty.fields.offset(i);
            if actual != expected {
                let name: &str = adt
                    .non_enum_variant()
                    .fields
                    .get(FieldIdx::from(i))
                    .unwrap()
                    .name
                    .as_str();
                tcx.dcx().span_fatal(
                    span,
                    format!(
                        "Unexpected layout for {err_attr_name} annotated struct: \
                    Expected member `{name}` at offset {expected:?}, but was at {actual:?}"
                    ),
                )
            }
        }

        Ok((element_word, count))
    } else {
        Err(tcx
            .dcx()
            .span_err(span, format!("{err_attr_name} type must be a struct")))
    }
}
