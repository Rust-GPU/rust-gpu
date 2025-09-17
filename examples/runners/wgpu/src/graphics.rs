use crate::{CompiledShaderModules, Options, maybe_watch};
use wgpu::ShaderModuleDescriptorPassthrough;

use shared::ShaderConstants;
use std::slice;
use winit::{
    event::{ElementState, Event, MouseButton, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::Window,
};

#[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
mod shaders {
    // The usual usecase of code generation is always building in build.rs, and so the codegen
    // always happens. However, we want to both test code generation (on android) and runtime
    // compilation (on desktop), so manually fill in what would have been codegenned for desktop.
    #[allow(non_upper_case_globals)]
    pub const main_fs: &str = "main_fs";
    #[allow(non_upper_case_globals)]
    pub const main_vs: &str = "main_vs";
}
#[cfg(any(target_os = "android", target_arch = "wasm32"))]
mod shaders {
    include!(concat!(env!("OUT_DIR"), "/entry_points.rs"));
}

fn mouse_button_index(button: MouseButton) -> usize {
    match button {
        MouseButton::Left => 0,
        MouseButton::Middle => 1,
        MouseButton::Right => 2,
        MouseButton::Back => 3,
        MouseButton::Forward => 4,
        MouseButton::Other(i) => 5 + (i as usize),
    }
}

async fn run(
    options: Options,
    event_loop: EventLoop<CompiledShaderModules>,
    window: Window,
    compiled_shader_modules: CompiledShaderModules,
) {
    // FIXME(eddyb) should this just use `wgpu::Backends::PRIMARY`?
    // (that also enables the DirectX 12 backend, not sure we want that one?)
    let backends = wgpu::Backends::from_env()
        .unwrap_or(wgpu::Backends::VULKAN | wgpu::Backends::METAL | wgpu::Backends::BROWSER_WEBGPU);
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends,
        ..Default::default()
    });

    // HACK(eddyb) marker error type for lazily-created surfaces (e.g. on Android).
    struct SurfaceCreationPending {
        preferred_format: wgpu::TextureFormat,
    }

    // Wait for Resumed event on Android; the surface is only otherwise needed
    // early to find an adapter that can render to this surface.
    let initial_surface = if cfg!(target_os = "android") {
        Err(SurfaceCreationPending {
            preferred_format: wgpu::TextureFormat::Rgba8UnormSrgb,
        })
    } else {
        Ok(instance
            .create_surface(&window)
            .expect("Failed to create surface from window"))
    };

    let adapter = wgpu::util::initialize_adapter_from_env_or_default(
        &instance,
        // Request an adapter which can render to our surface
        initial_surface.as_ref().ok(),
    )
    .await
    .expect("Failed to find an appropriate adapter");

    let mut required_features = wgpu::Features::PUSH_CONSTANTS;
    if options.force_spirv_passthru {
        required_features |= wgpu::Features::SPIRV_SHADER_PASSTHROUGH;
    }
    let required_limits = wgpu::Limits {
        max_push_constant_size: 128,
        ..Default::default()
    };

    // Create the logical device and command queue
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor {
            label: None,
            required_features,
            required_limits,
            memory_hints: wgpu::MemoryHints::Performance,
            trace: Default::default(),
        })
        .await
        .expect("Failed to create device");

    fn auto_configure_surface<'a>(
        adapter: &wgpu::Adapter,
        device: &wgpu::Device,
        surface: wgpu::Surface<'a>,
        size: winit::dpi::PhysicalSize<u32>,
    ) -> (wgpu::Surface<'a>, wgpu::SurfaceConfiguration) {
        let mut surface_config = surface
            .get_default_config(adapter, size.width, size.height)
            .unwrap_or_else(|| {
                panic!(
                    "Missing formats/present modes in surface capabilities: {:#?}",
                    surface.get_capabilities(adapter)
                )
            });

        // HACK(eddyb) this (alongside `.add_srgb_suffix()` calls elsewhere)
        // forces sRGB output, even on WebGPU (which handles it differently).
        surface_config
            .view_formats
            .push(surface_config.format.add_srgb_suffix());

        // FIXME(eddyb) should this be toggled by a CLI arg?
        // NOTE(eddyb) VSync was disabled in the past, but without VSync,
        // especially for simpler shaders, you can easily hit thousands
        // of frames per second, stressing GPUs for no reason.
        surface_config.present_mode = wgpu::PresentMode::AutoVsync;

        surface.configure(device, &surface_config);

        (surface, surface_config)
    }
    let mut surface_with_config = initial_surface
        .map(|surface| auto_configure_surface(&adapter, &device, surface, window.inner_size()));

    // Describe the pipeline layout and build the initial pipeline.
    let push_constants_or_rossbo_emulation = {
        const PUSH_CONSTANTS_SIZE: usize = std::mem::size_of::<ShaderConstants>();
        let stages = wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT;

        if !options.emulate_push_constants_with_storage_buffer {
            Ok(wgpu::PushConstantRange {
                stages,
                range: 0..PUSH_CONSTANTS_SIZE as u32,
            })
        } else {
            let buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: None,
                size: PUSH_CONSTANTS_SIZE as u64,
                usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
            let binding0 = wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: stages,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Storage { read_only: true },
                    has_dynamic_offset: false,
                    min_binding_size: Some((PUSH_CONSTANTS_SIZE as u64).try_into().unwrap()),
                },
                count: None,
            };
            let bind_group_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: None,
                    entries: &[binding0],
                });
            let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: None,
                layout: &bind_group_layout,
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: buffer.as_entire_binding(),
                }],
            });
            Err((buffer, bind_group_layout, bind_group))
        }
    };
    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: push_constants_or_rossbo_emulation
            .as_ref()
            .err()
            .map(|(_, layout, _)| layout)
            .as_slice(),
        push_constant_ranges: push_constants_or_rossbo_emulation
            .as_ref()
            .map_or(&[], slice::from_ref),
    });

    let mut render_pipeline = create_pipeline(
        &options,
        &device,
        &pipeline_layout,
        surface_with_config.as_ref().map_or_else(
            |pending| pending.preferred_format,
            |(_, surface_config)| surface_config.format.add_srgb_suffix(),
        ),
        compiled_shader_modules,
    );

    let start = web_time::Instant::now();

    let (mut cursor_x, mut cursor_y) = (0.0, 0.0);
    let (mut drag_start_x, mut drag_start_y) = (0.0, 0.0);
    let (mut drag_end_x, mut drag_end_y) = (0.0, 0.0);
    let mut mouse_button_pressed = 0;
    let mut mouse_button_press_since_last_frame = 0;
    let mut mouse_button_press_time = [f32::NEG_INFINITY; 3];

    // FIXME(eddyb) incomplete `winit` upgrade, follow the guides in:
    // https://github.com/rust-windowing/winit/releases/tag/v0.30.0
    #[allow(deprecated)]
    event_loop.run(|event, event_loop_window_target| {
        // Have the closure take ownership of the resources.
        // `event_loop.run` never returns, therefore we must do this to ensure
        // the resources are properly cleaned up.
        let _ = (&instance, &adapter, &pipeline_layout);
        let render_pipeline = &mut render_pipeline;

        event_loop_window_target.set_control_flow(ControlFlow::Wait);
        match event {
            Event::Resumed => {
                // Avoid holding onto to multiple surfaces at the same time
                // (as it's undetected and can confusingly break e.g. Wayland).
                //
                // FIXME(eddyb) create the window and `wgpu::Surface` on either
                // `Event::NewEvents(StartCause::Init)`, or `Event::Resumed`,
                // which is becoming recommended on (almost) all platforms, see:
                // - https://github.com/rust-windowing/winit/releases/tag/v0.30.0
                // - https://github.com/gfx-rs/wgpu/blob/v23/examples/src/framework.rs#L139-L161
                //   (note wasm being handled differently due to its `<canvas>`)
                if let Ok((_, surface_config)) = &surface_with_config {
                    // HACK(eddyb) can't move out of `surface_with_config` as
                    // it's a closure capture, and also the `Err(_)` variant
                    // has a payload so that needs to be filled with something.
                    let filler = Err(SurfaceCreationPending {
                        preferred_format: surface_config.format,
                    });
                    drop(std::mem::replace(&mut surface_with_config, filler));
                }

                let new_surface = instance.create_surface(&window)
                    .expect("Failed to create surface from window (after resume)");
                surface_with_config = Ok(auto_configure_surface(
                    &adapter,
                    &device,
                    new_surface,
                    window.inner_size(),
                ));
            }
            Event::Suspended => {
                if let Ok((_, surface_config)) = &surface_with_config {
                    surface_with_config = Err(SurfaceCreationPending {
                        preferred_format: surface_config.format,
                    });
                }
            }
            Event::WindowEvent {
                event: WindowEvent::Resized(size),
                ..
            } => {
                if size.width != 0 && size.height != 0 {
                    // Recreate the swap chain with the new size
                    if let Ok((surface, surface_config)) = &mut surface_with_config {
                        surface_config.width = size.width;
                        surface_config.height = size.height;
                        surface.configure(&device, surface_config);
                    }
                }
            }
            Event::WindowEvent {
                event: WindowEvent::RedrawRequested,
                ..
            } => {
                // FIXME(eddyb) only the mouse shader *really* needs this, could
                // avoid doing wasteful rendering by special-casing each shader?
                // (with VSync enabled this can't be *too* bad, thankfully)
                // FIXME(eddyb) is this the best way to do continuous redraws in
                // `winit`? (or should we stop using `ControlFlow::Wait`? etc.)
                window.request_redraw();

                if let Ok((surface, surface_config)) = &mut surface_with_config {
                    let output = match surface.get_current_texture() {
                        Ok(surface) => surface,
                        Err(err) => {
                            eprintln!("get_current_texture error: {err:?}");
                            match err {
                                wgpu::SurfaceError::Lost => {
                                    surface.configure(&device, surface_config);
                                }
                                wgpu::SurfaceError::OutOfMemory => {
                                    event_loop_window_target.exit();
                                }
                                _ => (),
                            }
                            return;
                        }
                    };
                    let output_view = output.texture.create_view(&wgpu::TextureViewDescriptor {
                        format: Some(surface_config.format.add_srgb_suffix()),
                        ..wgpu::TextureViewDescriptor::default()
                    });
                    let mut encoder = device
                        .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
                    {
                        let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                            label: None,
                            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                                view: &output_view,
                                depth_slice: None,
                                resolve_target: None,
                                ops: wgpu::Operations {
                                    load: wgpu::LoadOp::Clear(wgpu::Color::GREEN),
                                    store: wgpu::StoreOp::Store,
                                },
                            })],
                            depth_stencil_attachment: None,
                            ..Default::default()
                        });

                        let time = start.elapsed().as_secs_f32();
                        for (i, press_time) in mouse_button_press_time.iter_mut().enumerate() {
                            if (mouse_button_press_since_last_frame & (1 << i)) != 0 {
                                *press_time = time;
                            }
                        }
                        mouse_button_press_since_last_frame = 0;

                        let push_constants = ShaderConstants {
                            width: window.inner_size().width,
                            height: window.inner_size().height,
                            time,
                            cursor_x,
                            cursor_y,
                            drag_start_x,
                            drag_start_y,
                            drag_end_x,
                            drag_end_y,
                            mouse_button_pressed,
                            mouse_button_press_time,
                        };

                        rpass.set_pipeline(render_pipeline);
                        let (push_constant_offset, push_constant_bytes) =
                            (0, bytemuck::bytes_of(&push_constants));
                        match &push_constants_or_rossbo_emulation {
                            Ok(_) => rpass.set_push_constants(
                                wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                                push_constant_offset as u32,
                                push_constant_bytes,
                            ),
                            Err((buffer, _, bind_group)) => {
                                queue.write_buffer(
                                    buffer,
                                    push_constant_offset,
                                    push_constant_bytes,
                                );
                                rpass.set_bind_group(0, bind_group, &[]);
                            }
                        }
                        rpass.draw(0..3, 0..1);
                    }

                    queue.submit(Some(encoder.finish()));
                    output.present();
                }
            }
            Event::WindowEvent {
                event:
                    WindowEvent::CloseRequested
                    | WindowEvent::KeyboardInput {
                        event:
                            winit::event::KeyEvent {
                                logical_key:
                                    winit::keyboard::Key::Named(winit::keyboard::NamedKey::Escape),
                                state: winit::event::ElementState::Pressed,
                                ..
                            },
                        ..
                    },
                ..
            } => event_loop_window_target.exit(),
            Event::WindowEvent {
                event: WindowEvent::MouseInput { state, button, .. },
                ..
            } => {
                let mask = 1 << mouse_button_index(button);
                match state {
                    ElementState::Pressed => {
                        mouse_button_pressed |= mask;
                        mouse_button_press_since_last_frame |= mask;

                        if button == MouseButton::Left {
                            drag_start_x = cursor_x;
                            drag_start_y = cursor_y;
                            drag_end_x = cursor_x;
                            drag_end_y = cursor_y;
                        }
                    }
                    ElementState::Released => mouse_button_pressed &= !mask,
                }
            }
            Event::WindowEvent {
                event: WindowEvent::CursorMoved { position, .. },
                ..
            } => {
                cursor_x = position.x as f32;
                cursor_y = position.y as f32;
                if (mouse_button_pressed & (1 << mouse_button_index(MouseButton::Left))) != 0 {
                    drag_end_x = cursor_x;
                    drag_end_y = cursor_y;
                }
            }
            Event::UserEvent(new_module) => {
                *render_pipeline = create_pipeline(
                    &options,
                    &device,
                    &pipeline_layout,
                    surface_with_config.as_ref().map_or_else(
                        |pending| pending.preferred_format,
                        |(_, surface_config)| surface_config.format,
                    ),
                    new_module,
                );
                window.request_redraw();
                event_loop_window_target.set_control_flow(ControlFlow::Poll);
            }
            _ => {}
        }
    }).unwrap();
}

fn create_pipeline(
    options: &Options,
    device: &wgpu::Device,
    pipeline_layout: &wgpu::PipelineLayout,
    surface_format: wgpu::TextureFormat,
    mut compiled_shader_modules: CompiledShaderModules,
) -> wgpu::RenderPipeline {
    if options.emulate_push_constants_with_storage_buffer {
        let (ds, b) = (0, 0);

        for (_, shader_module_descr) in &mut compiled_shader_modules.named_spv_modules {
            let w = shader_module_descr.source.to_mut();
            assert_eq!((w[0], w[4]), (0x07230203, 0));
            let mut last_op_decorate_start = None;
            let mut i = 5;
            while i < w.len() {
                let (op, len) = (w[i] & 0xffff, w[i] >> 16);
                match op {
                    71 => last_op_decorate_start = Some(i),
                    32 if w[i + 2] == 9 => w[i + 2] = 12,
                    59 if w[i + 3] == 9 => {
                        w[i + 3] = 12;
                        let id = w[i + 2];
                        let j = last_op_decorate_start.expect("no OpDecorate?");
                        w.splice(
                            j..j,
                            [0x4_0047, id, 34, ds, 0x4_0047, id, 33, b, 0x3_0047, id, 24],
                        );
                        i += 11;
                    }
                    54 => break,
                    _ => {}
                }
                i += len as usize;
            }
        }
    }

    // FIXME(eddyb) automate this decision by default.
    let create_module = |module| {
        if options.force_spirv_passthru {
            unsafe {
                device.create_shader_module_passthrough(ShaderModuleDescriptorPassthrough::SpirV(
                    module,
                ))
            }
        } else {
            let wgpu::ShaderModuleDescriptorSpirV { label, source } = module;
            device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label,
                source: wgpu::ShaderSource::SpirV(source),
            })
        }
    };

    let vs_entry_point = shaders::main_vs;
    let fs_entry_point = shaders::main_fs;

    let vs_module_descr = compiled_shader_modules.spv_module_for_entry_point(vs_entry_point);
    let fs_module_descr = compiled_shader_modules.spv_module_for_entry_point(fs_entry_point);

    // HACK(eddyb) avoid calling `device.create_shader_module` twice unnecessarily.
    let vs_fs_same_module = std::ptr::eq(&vs_module_descr.source[..], &fs_module_descr.source[..]);

    let vs_module = &create_module(vs_module_descr);
    let fs_module;
    let fs_module = if vs_fs_same_module {
        vs_module
    } else {
        fs_module = create_module(fs_module_descr);
        &fs_module
    };

    device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        cache: None,
        label: None,
        layout: Some(pipeline_layout),
        vertex: wgpu::VertexState {
            module: vs_module,
            entry_point: Some(vs_entry_point),
            buffers: &[],
            compilation_options: Default::default(),
        },
        primitive: wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: None,
            unclipped_depth: false,
            polygon_mode: wgpu::PolygonMode::Fill,
            conservative: false,
        },
        depth_stencil: None,
        multisample: wgpu::MultisampleState {
            count: 1,
            mask: !0,
            alpha_to_coverage_enabled: false,
        },
        fragment: Some(wgpu::FragmentState {
            compilation_options: Default::default(),
            module: fs_module,
            entry_point: Some(fs_entry_point),
            targets: &[Some(wgpu::ColorTargetState {
                format: surface_format,
                blend: None,
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        multiview: None,
    })
}

#[allow(clippy::match_wild_err_arm)]
pub fn start(
    #[cfg(target_os = "android")] android_app: winit::platform::android::activity::AndroidApp,
    options: &Options,
) {
    let mut event_loop_builder = EventLoop::with_user_event();
    cfg_if::cfg_if! {
        if #[cfg(target_os = "android")] {
            android_logger::init_once(
                android_logger::Config::default()
                    .with_max_level("info".parse().unwrap()),
            );

            use winit::platform::android::EventLoopBuilderExtAndroid;
            event_loop_builder.with_android_app(android_app);
        } else if #[cfg(target_arch = "wasm32")] {
            std::panic::set_hook(Box::new(console_error_panic_hook::hook));
            console_log::init().expect("could not initialize logger");
        } else {
            env_logger::init();
        }
    }
    let event_loop = event_loop_builder.build().unwrap();

    // Build the shader before we pop open a window, since it might take a while.
    let initial_shader = maybe_watch(
        options,
        #[cfg(not(any(target_os = "android", target_arch = "wasm32")))]
        {
            let proxy = event_loop.create_proxy();
            Some(Box::new(move |res| match proxy.send_event(res) {
                Ok(it) => it,
                // ShaderModuleDescriptor is not `Debug`, so can't use unwrap/expect
                Err(_err) => panic!("Event loop dead"),
            }))
        },
    );

    // FIXME(eddyb) incomplete `winit` upgrade, follow the guides in:
    // https://github.com/rust-windowing/winit/releases/tag/v0.30.0
    #[allow(deprecated)]
    let window = event_loop
        .create_window(
            winit::window::Window::default_attributes()
                .with_title("Rust GPU - wgpu")
                .with_inner_size(winit::dpi::LogicalSize::new(1280.0, 720.0)),
        )
        .unwrap();

    cfg_if::cfg_if! {
        if #[cfg(target_arch = "wasm32")] {
            use winit::platform::web::WindowExtWebSys;
            // On wasm, append the canvas to the document body
            web_sys::window()
                .and_then(|dom_window| {
                    dom_window
                        .document()?
                        .body()?
                        .append_child(&web_sys::Element::from(window.canvas()?))
                        .ok()
                })
                .expect("couldn't append canvas to document body");
            wasm_bindgen_futures::spawn_local(run(
                options.clone(),
                event_loop,
                window,
                initial_shader,
            ));
        } else {
            futures::executor::block_on(run(
                options.clone(),
                event_loop,
                window,
                initial_shader,
            ));
        }
    }
}
