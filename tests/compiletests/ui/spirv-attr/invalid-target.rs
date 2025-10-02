// Tests that `#[rust_gpu::spirv(...)]` attributes cannot be applied to the wrong "targets"
// (i.e. various kinds of definitions and other syntactic categories).

// build-fail

#![feature(
    extern_types,
    type_alias_impl_trait, // HACK(eddyb) this comment prevents rustfmt
    stmt_expr_attributes,
    trait_alias
)]

// NOTE(eddyb) in the interest of keeping this test manageable, only one of
// each of the following categories of `#[rust_gpu::spirv(...)]` attributes is used:
// * entry: `vertex`
// * storage class: `uniform`
// * builtin: `position`

// NOTE(eddyb) accounting for the number of errors this test actually produces:
// * 437 errors, all "attribute is only valid on" (see `invalid-target.stderr`)
// * 41 uses of `#[rust_gpu::spirv(...)]` in this test
// * at most 11 attributes per `#[rust_gpu::spirv(...)]`, so an upper bound of `41*11 = 451`
// * the difference between 451 and 437 is 14, i.e. valid attributes, made up of:
//   * 4 on `_Struct`
//   * 4 on functions, i.e. 1 on each of:
//     * `_inherent_method`
//     * `_trait_method_with_default`,
//     * `_trait_method` (in `impl _Trait for ()`)
//     * `_fn`
//   * 6 on `_entry_param`

// NOTE(firestar99) Using the custom-made `#[spirv_recursive_for_testing(...)]` which expands recursively,
// unlike #[spirv()] which tries to expand as little code as possible for compile performance reasons.

use spirv_std::macros::spirv_recursive_for_testing;

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
macro_rules! _macro {
    () => {};
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
extern crate spirv_std as _;

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
use spirv_std as _;

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
mod _mod {}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
extern "C" {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    type _ForeignTy;

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    static _FOREIGN_STATIC: ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _foreign_fn();
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
static _STATIC: () = ();

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
const _CONST: () = ();

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
type _TyAlias = ();

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
type _OpaqueTy = impl Copy;

fn _opaque_ty_definer() -> _OpaqueTy {
    ()
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
enum _Enum {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    _Variant {
        #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        )]
        _field: (),
    },
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
union _Union {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    _field: (),
}

#[spirv_recursive_for_testing(
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
struct _Struct {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    _field: (),
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
impl _Struct {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    const _INHERENT_ASSOC_CONST: () = ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _inherent_method() {}
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
trait _TraitAlias = Copy;

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
trait _Trait {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    type _AssocTy;

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    const _TRAIT_ASSOC_CONST: ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _trait_method();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _trait_method_with_default() {}
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    vertex, // fn-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
impl _Trait for () {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    type _AssocTy = ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    const _TRAIT_ASSOC_CONST: () = ();

    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    fn _trait_method() {}
}

#[spirv_recursive_for_testing(
    sampler, block, sampled_image, generic_image_type, // struct-only
    uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
)]
fn _fn(
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
    )]
    _entry_param: (),
) {
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )]
    let _statement = ();

    let _closure = #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        )]
    || {};

    (
        #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        )]
        (1, 2, 3)
        // expression
    );

    match () {
        #[spirv(
            sampler, block, sampled_image, generic_image_type, // struct-only
            vertex, // fn-only
            uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
        )]
        _arm => {}
    }
}

#[spirv_recursive_for_testing()]
fn _fn_with_generics<
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )] '_lifetime_param,
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )] _TyParam,
    #[spirv(
        sampler, block, sampled_image, generic_image_type, // struct-only
        vertex, // fn-only
        uniform, position, descriptor_set = 0, binding = 0, flat, invariant, // param-only
    )] const _CONST_PARAM: usize,
>() {
}
