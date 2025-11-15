// FIXME(eddyb) update/review these lints.
//
// BEGIN - Embark standard lints v0.4
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
//#![deny(unsafe_code)] // impractical in this crate dealing with unsafe `ash`
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

use crate::device::MyDevice;
use crate::graphics::{MyRenderPipelineManager, MyRenderer};
use crate::swapchain::MySwapchainManager;
use anyhow::{Context, anyhow};
use ash::util::read_spv;
use clap::{Parser, ValueEnum};
use raw_window_handle::HasDisplayHandle as _;
use shared::ShaderConstants;
use spirv_builder::{MetadataPrintout, SpirvBuilder};
use std::{
    fs::File,
    path::PathBuf,
    sync::mpsc::{TryRecvError, sync_channel},
    thread,
};
use winit::event_loop::ActiveEventLoop;
use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
};

pub mod device;
pub mod graphics;
pub mod single_command_buffer;
pub mod swapchain;

// This runner currently doesn't run the `compute` shader example.
#[derive(Debug, PartialEq, Eq, Copy, Clone, ValueEnum)]
pub enum RustGPUShader {
    Simplest,
    Sky,
    Mouse,
}

impl RustGPUShader {
    fn crate_name(&self) -> &'static str {
        match self {
            RustGPUShader::Simplest => "simplest-shader",
            RustGPUShader::Sky => "sky-shader",
            RustGPUShader::Mouse => "mouse-shader",
        }
    }
}

#[derive(Debug, Parser)]
#[command()]
pub struct Options {
    #[arg(short, long, default_value = "sky")]
    shader: RustGPUShader,

    /// Use Vulkan debug layer (requires Vulkan SDK installed)
    #[arg(short, long)]
    debug_layer: bool,
}

pub fn main() -> anyhow::Result<()> {
    let options = Options::parse();
    let shader_code = compile_shaders(&options.shader)?;

    // runtime setup
    let event_loop = EventLoop::new()?;
    // FIXME(eddyb) incomplete `winit` upgrade, follow the guides in:
    // https://github.com/rust-windowing/winit/releases/tag/v0.30.0
    #[allow(deprecated)]
    let window = event_loop.create_window(
        winit::window::Window::default_attributes()
            .with_title("Rust GPU - ash")
            .with_inner_size(winit::dpi::LogicalSize::new(
                f64::from(1280),
                f64::from(720),
            )),
    )?;

    let extensions = ash_window::enumerate_required_extensions(window.display_handle()?.as_raw())?;
    let device = MyDevice::new(extensions, &options)?;
    let mut swapchain = MySwapchainManager::new(device.clone(), window)?;
    let mut renderer = MyRenderer::new(MyRenderPipelineManager::new(
        device.clone(),
        swapchain.surface_format.format,
        shader_code,
    )?)?;

    let mut recompiling_shaders = false;
    let start = std::time::Instant::now();
    let (compiler_sender, compiler_receiver) = sync_channel::<Vec<u32>>(1);
    let mut event_handler =
        move |event: Event<_>, event_loop_window_target: &ActiveEventLoop| match event {
            Event::AboutToWait => {
                match compiler_receiver.try_recv() {
                    Err(TryRecvError::Empty) => (),
                    Ok(shader_code) => {
                        recompiling_shaders = false;
                        renderer.pipeline.set_shader_code(shader_code);
                    }
                    Err(TryRecvError::Disconnected) => {
                        return Err(anyhow!("compiler receiver disconnected unexpectedly"));
                    }
                };

                swapchain.render(|frame| {
                    let extent = frame.extent;
                    let push_constants = ShaderConstants {
                        width: extent.width,
                        height: extent.height,
                        time: start.elapsed().as_secs_f32(),

                        // FIXME(eddyb) implement mouse support for the ash runner.
                        cursor_x: 0.0,
                        cursor_y: 0.0,
                        drag_start_x: 0.0,
                        drag_start_y: 0.0,
                        drag_end_x: 0.0,
                        drag_end_y: 0.0,
                        mouse_button_pressed: 0,
                        mouse_button_press_time: [f32::NEG_INFINITY; 3],
                    };

                    renderer.render_frame(frame, push_constants)?;
                    Ok(())
                })
            }
            Event::WindowEvent { event, .. } => {
                match event {
                    WindowEvent::KeyboardInput {
                        event:
                            winit::event::KeyEvent {
                                logical_key: winit::keyboard::Key::Named(key),
                                state: winit::event::ElementState::Pressed,
                                ..
                            },
                        ..
                    } => match key {
                        winit::keyboard::NamedKey::Escape => event_loop_window_target.exit(),
                        winit::keyboard::NamedKey::F5 => {
                            if !recompiling_shaders {
                                recompiling_shaders = true;
                                let compiler_sender = compiler_sender.clone();
                                thread::spawn(move || {
                                    let shader_code = compile_shaders(&options.shader)
                                        .context("Compiling shaders failed")
                                        .unwrap();
                                    compiler_sender.try_send(shader_code).unwrap();
                                });
                            }
                        }
                        winit::keyboard::NamedKey::ArrowUp
                        | winit::keyboard::NamedKey::ArrowDown => {
                            let mut factor = renderer.pipeline.get_sky_fs_sun_intensity_factor();
                            factor = if key == winit::keyboard::NamedKey::ArrowUp {
                                factor.saturating_add(1)
                            } else {
                                factor.saturating_sub(1)
                            };
                            renderer.pipeline.set_sky_fs_sun_intensity_factor(factor);
                        }
                        _ => {}
                    },
                    WindowEvent::Resized(_) => {
                        swapchain.should_recreate();
                    }
                    WindowEvent::CloseRequested => event_loop_window_target.exit(),
                    _ => {}
                }

                Ok(())
            }
            _ => {
                event_loop_window_target.set_control_flow(ControlFlow::Poll);
                Ok(())
            }
        };

    // FIXME(eddyb) incomplete `winit` upgrade, follow the guides in:
    // https://github.com/rust-windowing/winit/releases/tag/v0.30.0
    #[allow(deprecated)]
    event_loop.run(move |event, event_loop_window_target| {
        event_handler(event, event_loop_window_target).unwrap();
    })?;
    Ok(())
}

pub fn compile_shaders(shader: &RustGPUShader) -> anyhow::Result<Vec<u32>> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let crate_path = [manifest_dir, "..", "..", "shaders", shader.crate_name()]
        .iter()
        .copied()
        .collect::<PathBuf>();

    let compile_result = SpirvBuilder::new(crate_path, "spirv-unknown-vulkan1.3")
        .print_metadata(MetadataPrintout::None)
        .shader_panic_strategy(spirv_builder::ShaderPanicStrategy::DebugPrintfThenExit {
            print_inputs: true,
            print_backtrace: true,
        })
        .build()?;
    let spv_path = compile_result.module.unwrap_single();

    // Assert that we always have these two shaders
    let shaders = &compile_result.entry_points;
    assert_eq!(shaders.len(), 2);
    assert!(shaders.contains(&"main_vs".to_string()));
    assert!(shaders.contains(&"main_fs".to_string()));

    Ok(read_spv(&mut File::open(spv_path)?)?)
}
