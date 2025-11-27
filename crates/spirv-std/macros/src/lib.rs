// FIXME(eddyb) update/review these lints.
//
// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_same_arms,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::todo,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v0.4
// crate-specific exceptions:
// #![allow()]
#![doc = include_str!("../README.md")]

mod debug_printf;
mod image;
mod sample_param_permutations;

use crate::debug_printf::{DebugPrintfInput, debug_printf_inner};
use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Ident, Span, TokenTree};
use quote::{ToTokens, TokenStreamExt, format_ident, quote};
use spirv_std_types::spirv_attr_version::spirv_attr_with_version;

/// A macro for creating SPIR-V `OpTypeImage` types. Always produces a
/// `spirv_std::image::Image<...>` type.
///
/// The grammar for the macro is as follows:
///
/// ```rust,ignore
/// Image!(
///     <dimensionality>,
///     <type=...|format=...>,
///     [sampled[=<true|false>],]
///     [multisampled[=<true|false>],]
///     [arrayed[=<true|false>],]
///     [depth[=<true|false>],]
/// )
/// ```
///
/// `=true` can be omitted as shorthand - e.g. `sampled` is short for `sampled=true`.
///
/// A basic example looks like this:
/// ```rust,ignore
/// #[spirv(vertex)]
/// fn main(#[spirv(descriptor_set = 0, binding = 0)] image: &Image!(2D, type=f32, sampled)) {}
/// ```
///
/// ## Arguments
///
/// - `dimensionality` — Dimensionality of an image.
///   Accepted values: `1D`, `2D`, `3D`, `rect`, `cube`, `subpass`.
/// - `type` — The sampled type of an image, mutually exclusive with `format`,
///   when set the image format is unknown.
///   Accepted values: `f32`, `f64`, `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`.
/// - `format` — The image format of the image, mutually exclusive with `type`.
///   Accepted values: Snake case versions of [`ImageFormat`] variants, e.g. `rgba32f`,
///   `rgba8_snorm`.
/// - `sampled` — Whether it is known that the image will be used with a sampler.
///   Accepted values: `true` or `false`. Default: `unknown`.
/// - `multisampled` — Whether the image contains multisampled content.
///   Accepted values: `true` or `false`. Default: `false`.
/// - `arrayed` — Whether the image contains arrayed content.
///   Accepted values: `true` or `false`. Default: `false`.
/// - `depth` — Whether it is known that the image is a depth image.
///   Accepted values: `true` or `false`. Default: `unknown`.
///
/// [`ImageFormat`]: spirv_std_types::image_params::ImageFormat
///
/// Keep in mind that `sampled` here is a different concept than the `SampledImage` type:
/// `sampled=true` means that this image requires a sampler to be able to access, while the
/// `SampledImage` type bundles that sampler together with the image into a single type (e.g.
/// `sampler2D` in GLSL, vs. `texture2D`).
#[proc_macro]
// The `Image` is supposed to be used in the type position, which
// uses `PascalCase`.
#[allow(nonstandard_style)]
pub fn Image(item: TokenStream) -> TokenStream {
    let output = syn::parse_macro_input!(item as image::ImageType).into_token_stream();

    output.into()
}

/// Replaces all (nested) occurrences of the `#[spirv(..)]` attribute with
/// `#[cfg_attr(target_arch="spirv", rust_gpu::spirv(..))]`.
#[proc_macro_attribute]
pub fn spirv(attr: TokenStream, item: TokenStream) -> TokenStream {
    let spirv = format_ident!("{}", &spirv_attr_with_version());

    // prepend with #[rust_gpu::spirv(..)]
    let attr: proc_macro2::TokenStream = attr.into();
    let mut tokens = quote! { #[cfg_attr(target_arch="spirv", rust_gpu::#spirv(#attr))] };

    let item: proc_macro2::TokenStream = item.into();
    for tt in item {
        match tt {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                let mut group_tokens = proc_macro2::TokenStream::new();
                let mut last_token_hashtag = false;
                for tt in group.stream() {
                    let is_token_hashtag =
                        matches!(&tt, TokenTree::Punct(punct) if punct.as_char() == '#');
                    match tt {
                        TokenTree::Group(group)
                            if group.delimiter() == Delimiter::Bracket
                                && last_token_hashtag
                                && matches!(group.stream().into_iter().next(), Some(TokenTree::Ident(ident)) if ident == "spirv") =>
                        {
                            // group matches [spirv ...]
                            // group stream doesn't include the brackets
                            let inner = group
                                .stream()
                                .into_iter()
                                .skip(1)
                                .collect::<proc_macro2::TokenStream>();
                            group_tokens.extend(
                                quote! { [cfg_attr(target_arch="spirv", rust_gpu::#spirv #inner)] },
                            );
                        }
                        _ => group_tokens.append(tt),
                    }
                    last_token_hashtag = is_token_hashtag;
                }
                let mut out = Group::new(Delimiter::Parenthesis, group_tokens);
                out.set_span(group.span());
                tokens.append(out);
            }
            _ => tokens.append(tt),
        }
    }
    tokens.into()
}

/// For testing only! Is not reexported in `spirv-std`, but reachable via
/// `spirv_std::macros::spirv_recursive_for_testing`.
///
/// May be more expensive than plain `spirv`, since we're checking a lot more symbols. So I've opted to
/// have this be a separate macro, instead of modifying the standard `spirv` one.
#[proc_macro_attribute]
pub fn spirv_recursive_for_testing(attr: TokenStream, item: TokenStream) -> TokenStream {
    fn recurse(spirv: &Ident, stream: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        let mut last_token_hashtag = false;
        stream.into_iter().map(|tt| {
            let mut is_token_hashtag = false;
            let out = match tt {
                TokenTree::Group(group)
                if group.delimiter() == Delimiter::Bracket
                    && last_token_hashtag
                    && matches!(group.stream().into_iter().next(), Some(TokenTree::Ident(ident)) if ident == "spirv") =>
                    {
                        // group matches [spirv ...]
                        // group stream doesn't include the brackets
                        let inner = group
                            .stream()
                            .into_iter()
                            .skip(1)
                            .collect::<proc_macro2::TokenStream>();
                        quote! { [cfg_attr(target_arch="spirv", rust_gpu::#spirv #inner)] }
                    },
                TokenTree::Group(group) => {
                    let mut out = Group::new(group.delimiter(), recurse(spirv, group.stream()));
                    out.set_span(group.span());
                    TokenTree::Group(out).into()
                },
                TokenTree::Punct(punct) => {
                    is_token_hashtag = punct.as_char() == '#';
                    TokenTree::Punct(punct).into()
                }
                tt => tt.into(),
            };
            last_token_hashtag = is_token_hashtag;
            out
        }).collect()
    }

    let attr: proc_macro2::TokenStream = attr.into();
    let item: proc_macro2::TokenStream = item.into();

    // prepend with #[rust_gpu::spirv(..)]
    let spirv = format_ident!("{}", &spirv_attr_with_version());
    let inner = recurse(&spirv, item);
    quote! { #[cfg_attr(target_arch="spirv", rust_gpu::#spirv(#attr))] #inner }.into()
}

/// Marks a function as runnable only on the GPU, and will panic on
/// CPU platforms.
#[proc_macro_attribute]
pub fn gpu_only(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = syn::parse_macro_input!(item as syn::ItemFn);

    let fn_name = sig.ident.clone();

    let sig_cpu = syn::Signature {
        abi: None,
        ..sig.clone()
    };

    let output = quote::quote! {
        // Don't warn on unused arguments on the CPU side.
        #[cfg(not(target_arch="spirv"))]
        #[allow(unused_variables)]
        #(#attrs)* #vis #sig_cpu {
            unimplemented!(
                concat!("`", stringify!(#fn_name), "` is only available on SPIR-V platforms.")
            )
        }

        #[cfg(target_arch="spirv")]
        #(#attrs)* #vis #sig {
            #block
        }
    };

    output.into()
}

/// Print a formatted string using the debug printf extension.
///
/// Examples:
///
/// ```rust,ignore
/// debug_printf!("uv: %v2f\n", uv);
/// debug_printf!("pos.x: %f, pos.z: %f, int: %i\n", pos.x, pos.z, int);
/// ```
///
/// See <https://github.com/KhronosGroup/Vulkan-ValidationLayers/blob/main/docs/debug_printf.md#debug-printf-format-string> for formatting rules.
#[proc_macro]
pub fn debug_printf(input: TokenStream) -> TokenStream {
    debug_printf_inner(syn::parse_macro_input!(input as DebugPrintfInput))
}

/// Similar to `debug_printf` but appends a newline to the format string.
#[proc_macro]
pub fn debug_printfln(input: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(input as DebugPrintfInput);
    input.format_string.push('\n');
    debug_printf_inner(input)
}

/// Generates permutations of an `ImageWithMethods` implementation containing sampling functions
/// that have asm instruction ending with a placeholder `$PARAMS` operand. The last parameter
/// of each function must be named `params`, its type will be rewritten. Relevant generic
/// arguments are added to the impl generics.
/// See `SAMPLE_PARAM_GENERICS` for a list of names you cannot use as generic arguments.
#[proc_macro_attribute]
#[doc(hidden)]
pub fn gen_sample_param_permutations(_attr: TokenStream, item: TokenStream) -> TokenStream {
    sample_param_permutations::gen_sample_param_permutations(item)
}

pub(crate) fn spirv_std_crate_symbol() -> syn::Result<proc_macro2::TokenStream> {
    match proc_macro_crate::crate_name("spirv-std") {
        Ok(proc_macro_crate::FoundCrate::Itself) => Ok(quote! {crate}),
        Ok(proc_macro_crate::FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            Ok(quote! {::#ident})
        }
        Err(err) => Err(syn::Error::new(Span::call_site(), err)),
    }
}
