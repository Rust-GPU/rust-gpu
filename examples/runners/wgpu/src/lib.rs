// FIXME(eddyb) update/review these lints.
//
// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
//#![deny(unsafe_code)]  // quite a bit of unsafe with wgpu still, would be nice to remove
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

use clap::Parser;
use clap::ValueEnum;
use std::borrow::Cow;
use strum::{Display, EnumString};

// NOTE(eddyb) while this could theoretically work on the web, it needs more work.
#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
mod compute;

mod graphics;

#[derive(Debug, EnumString, Display, PartialEq, Eq, Copy, Clone, ValueEnum)]
pub enum RustGPUShader {
    Simplest,
    Sky,
    Compute,
    Mouse,
}

struct CompiledShaderModules {
    named_spv_modules: Vec<(Option<String>, wgpu::ShaderModuleDescriptorSpirV<'static>)>,
}

impl CompiledShaderModules {
    fn spv_module_for_entry_point<'a>(
        &'a self,
        wanted_entry: &str,
    ) -> wgpu::ShaderModuleDescriptorSpirV<'a> {
        for (name, spv_module) in &self.named_spv_modules {
            if name.as_ref().is_none_or(|name| name == wanted_entry) {
                return wgpu::ShaderModuleDescriptorSpirV {
                    label: name.as_deref(),
                    source: Cow::Borrowed(&spv_module.source),
                };
            }
        }
        unreachable!(
            "{wanted_entry:?} not found in modules {:?}",
            self.named_spv_modules
                .iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>()
        );
    }
}

fn maybe_watch(
    options: &Options,
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))] on_watch: Option<
        Box<dyn FnMut(CompiledShaderModules) + Send + 'static>,
    >,
) -> CompiledShaderModules {
    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        use spirv_builder::{CompileResult, MetadataPrintout, SpirvBuilder};
        use std::path::PathBuf;

        let crate_name = match options.shader {
            RustGPUShader::Simplest => "simplest-shader",
            RustGPUShader::Sky => "sky-shader",
            RustGPUShader::Compute => "compute-shader",
            RustGPUShader::Mouse => "mouse-shader",
        };
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let crate_path = [manifest_dir, "..", "..", "shaders", crate_name]
            .iter()
            .copied()
            .collect::<PathBuf>();

        let has_debug_printf = options.force_spirv_passthru;

        let builder = SpirvBuilder::new(crate_path, "spirv-unknown-vulkan1.1")
            .print_metadata(MetadataPrintout::None)
            .shader_panic_strategy(if has_debug_printf {
                spirv_builder::ShaderPanicStrategy::DebugPrintfThenExit {
                    print_inputs: true,
                    print_backtrace: true,
                }
            } else {
                spirv_builder::ShaderPanicStrategy::SilentExit
            });

        fn handle_compile_result(compile_result: CompileResult) -> CompiledShaderModules {
            let load_spv_module = |path| {
                let data = std::fs::read(path).unwrap();
                // FIXME(eddyb) this reallocates all the data pointlessly, there is
                // not a good reason to use `ShaderModuleDescriptorSpirV` specifically.
                let spirv = Cow::Owned(wgpu::util::make_spirv_raw(&data).into_owned());
                wgpu::ShaderModuleDescriptorSpirV {
                    label: None,
                    source: spirv,
                }
            };
            CompiledShaderModules {
                named_spv_modules: match compile_result.module {
                    spirv_builder::ModuleResult::SingleModule(path) => {
                        vec![(None, load_spv_module(path))]
                    }
                    spirv_builder::ModuleResult::MultiModule(modules) => modules
                        .into_iter()
                        .map(|(name, path)| (Some(name), load_spv_module(path)))
                        .collect(),
                },
            }
        }

        if let Some(mut f) = on_watch {
            let mut watcher = builder.watch().unwrap();
            let first_compile = watcher.recv().unwrap();

            let shader_modules = handle_compile_result(first_compile);
            std::thread::spawn(move || {
                loop {
                    let compile_result = watcher.recv().unwrap();
                    let modules = handle_compile_result(compile_result);
                    f(modules);
                }
            });
            shader_modules
        } else {
            handle_compile_result(builder.build().unwrap())
        }
    }
    #[cfg(any(target_os = "android", target_arch = "wasm32"))]
    {
        let module = match options.shader {
            RustGPUShader::Simplest => {
                wgpu::include_spirv_raw!(env!("simplest_shader.spv"))
            }
            RustGPUShader::Sky => wgpu::include_spirv_raw!(env!("sky_shader.spv")),
            RustGPUShader::Compute => wgpu::include_spirv_raw!(env!("compute_shader.spv")),
            RustGPUShader::Mouse => wgpu::include_spirv_raw!(env!("mouse_shader.spv")),
        };
        let spirv = match module {
            wgpu::ShaderModuleDescriptorPassthrough::SpirV(spirv) => spirv,
            _ => panic!("not spirv"),
        };
        CompiledShaderModules {
            named_spv_modules: vec![(None, spirv)],
        }
    }
}

#[derive(Parser, Clone)]
#[command()]
pub struct Options {
    /// which shader to run
    #[cfg_attr(not(target_arch = "wasm32"), arg(short, long, default_value = "sky"))]
    #[cfg_attr(target_arch = "wasm32", arg(short, long, default_value = "mouse"))]
    shader: RustGPUShader,

    #[arg(long)]
    force_spirv_passthru: bool,

    #[structopt(long)]
    emulate_push_constants_with_storage_buffer: bool,
}

#[cfg_attr(target_os = "android", unsafe(export_name = "android_main"))]
pub fn main(
    #[cfg(target_os = "android")] android_app: winit::platform::android::activity::AndroidApp,
) {
    let mut options = Options::parse();

    #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
    {
        if options.shader == RustGPUShader::Compute {
            return compute::start(&options);
        }
    }

    // HACK(eddyb) force push constant emulation using (read-only) SSBOs, on
    // wasm->WebGPU, as push constants are currently not supported.
    // FIXME(eddyb) could push constant support be automatically detected at runtime?
    if cfg!(target_arch = "wasm32") {
        options.emulate_push_constants_with_storage_buffer = true;
    }

    graphics::start(
        #[cfg(target_os = "android")]
        android_app,
        &options,
    );
}
