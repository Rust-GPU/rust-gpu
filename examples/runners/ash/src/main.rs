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

use ash::{ext, khr, util::read_spv, vk};
use clap::{Parser, ValueEnum};
use raw_window_handle::{HasDisplayHandle as _, HasWindowHandle as _};
use shared::ShaderConstants;
use spirv_builder::{MetadataPrintout, SpirvBuilder};
use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    fs::File,
    os::raw::c_char,
    path::PathBuf,
    sync::mpsc::{TryRecvError, TrySendError, sync_channel},
    thread,
};
use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
};

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

pub fn main() {
    let options = Options::parse();
    let shader_data = compile_shaders(&options.shader).unwrap();

    // runtime setup
    let event_loop = EventLoop::new().unwrap();
    // FIXME(eddyb) incomplete `winit` upgrade, follow the guides in:
    // https://github.com/rust-windowing/winit/releases/tag/v0.30.0
    #[allow(deprecated)]
    let window = event_loop
        .create_window(
            winit::window::Window::default_attributes()
                .with_title("Rust GPU - ash")
                .with_inner_size(winit::dpi::LogicalSize::new(
                    f64::from(1280),
                    f64::from(720),
                )),
        )
        .unwrap();
    let mut ctx = RenderCtx::new(RenderBase::new(window, &options), shader_data);
    ctx.rebuild_pipeline();
    let (compiler_sender, compiler_receiver) = sync_channel::<Vec<u32>>(1);

    // FIXME(eddyb) incomplete `winit` upgrade, follow the guides in:
    // https://github.com/rust-windowing/winit/releases/tag/v0.30.0
    #[allow(deprecated)]
    event_loop
        .run(move |event, event_loop_window_target| match event {
            Event::AboutToWait => {
                match compiler_receiver.try_recv() {
                    Err(TryRecvError::Empty) => {
                        if ctx.rendering_paused {
                            let vk::Extent2D { width, height } = ctx.base.surface_resolution();
                            if height > 0 && width > 0 {
                                ctx.recreate_swapchain();
                                ctx.render();
                            }
                        } else {
                            ctx.render();
                        }
                    }
                    Ok(shader_data) => {
                        ctx.recompiling_shaders = false;
                        ctx.shader_code = shader_data;
                        ctx.rebuild_pipeline();
                    }
                    Err(TryRecvError::Disconnected) => {
                        panic!("compiler receiver disconnected unexpectedly");
                    }
                };
            }
            Event::WindowEvent { event, .. } => match event {
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
                        if !ctx.recompiling_shaders {
                            ctx.recompiling_shaders = true;
                            let compiler_sender = compiler_sender.clone();
                            thread::spawn(move || {
                                if let Err(TrySendError::Disconnected(_)) = compiler_sender
                                    .try_send(compile_shaders(&options.shader).unwrap())
                                {
                                    panic!("compiler sender disconnected unexpectedly");
                                };
                            });
                        }
                    }
                    winit::keyboard::NamedKey::ArrowUp | winit::keyboard::NamedKey::ArrowDown => {
                        let factor =
                            &mut ctx.sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor;
                        *factor = if key == winit::keyboard::NamedKey::ArrowUp {
                            factor.saturating_add(1)
                        } else {
                            factor.saturating_sub(1)
                        };

                        // HACK(eddyb) to see any changes, re-specializing the
                        // shader module is needed (e.g. during pipeline rebuild).
                        ctx.rebuild_pipeline();
                    }
                    _ => {}
                },
                WindowEvent::Resized(_) => {
                    ctx.recreate_swapchain();
                }
                WindowEvent::CloseRequested => event_loop_window_target.exit(),
                _ => {}
            },
            _ => event_loop_window_target.set_control_flow(ControlFlow::Poll),
        })
        .unwrap();
}

pub fn compile_shaders(shader: &RustGPUShader) -> anyhow::Result<Vec<u32>> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let crate_path = [manifest_dir, "..", "..", "shaders", shader.crate_name()]
        .iter()
        .copied()
        .collect::<PathBuf>();

    let compile_result = SpirvBuilder::new(crate_path, "spirv-unknown-vulkan1.1")
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

pub struct RenderBase {
    pub window: winit::window::Window,

    pub entry: ash::Entry,

    pub instance: ash::Instance,
    pub device: ash::Device,
    pub swapchain_loader: khr::swapchain::Device,

    pub debug_utils_loader: Option<ext::debug_utils::Instance>,
    pub debug_call_back: Option<vk::DebugUtilsMessengerEXT>,

    pub pdevice: vk::PhysicalDevice,
    pub queue_family_index: u32,
    pub present_queue: vk::Queue,

    pub surface: vk::SurfaceKHR,
    pub surface_loader: khr::surface::Instance,
    pub surface_format: vk::SurfaceFormatKHR,
}

impl RenderBase {
    pub fn new(window: winit::window::Window, options: &Options) -> Self {
        cfg_if::cfg_if! {
            if #[cfg(target_os = "macos")] {
                let entry = ash_molten::load();
            } else {
                let entry = unsafe{ash::Entry::load()}.unwrap();
            }
        }

        let instance: ash::Instance = {
            let app_name = CString::new("VulkanTriangle").unwrap();

            let layer_names = if options.debug_layer {
                vec![CString::new("VK_LAYER_KHRONOS_validation").unwrap()]
            } else {
                vec![]
            };
            let layers_names_raw: Vec<*const c_char> = layer_names
                .iter()
                .map(|raw_name| raw_name.as_ptr())
                .collect();

            let mut extension_names_raw =
                ash_window::enumerate_required_extensions(window.display_handle().unwrap().into())
                    .unwrap()
                    .to_vec();
            if options.debug_layer {
                extension_names_raw.push(ext::debug_utils::NAME.as_ptr());
            }

            let appinfo = vk::ApplicationInfo::default()
                .application_name(&app_name)
                .application_version(0)
                .engine_name(&app_name)
                .engine_version(0)
                .api_version(vk::make_api_version(0, 1, 2, 0));

            let instance_create_info = vk::InstanceCreateInfo::default()
                .application_info(&appinfo)
                .enabled_layer_names(&layers_names_raw)
                .enabled_extension_names(&extension_names_raw);

            unsafe {
                entry
                    .create_instance(&instance_create_info, None)
                    .expect("Instance creation error")
            }
        };

        let surface = unsafe {
            ash_window::create_surface(
                &entry,
                &instance,
                window.display_handle().unwrap().into(),
                window.window_handle().unwrap().into(),
                None,
            )
            .unwrap()
        };

        let (debug_utils_loader, debug_call_back) = if options.debug_layer {
            let debug_utils_loader = ext::debug_utils::Instance::new(&entry, &instance);
            let debug_call_back = {
                let debug_info = vk::DebugUtilsMessengerCreateInfoEXT::default()
                    .message_severity(
                        vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
                            | vk::DebugUtilsMessageSeverityFlagsEXT::WARNING
                            | vk::DebugUtilsMessageSeverityFlagsEXT::INFO,
                    )
                    .message_type(
                        vk::DebugUtilsMessageTypeFlagsEXT::GENERAL
                            | vk::DebugUtilsMessageTypeFlagsEXT::VALIDATION
                            | vk::DebugUtilsMessageTypeFlagsEXT::PERFORMANCE,
                    )
                    .pfn_user_callback(Some(vulkan_debug_callback));

                unsafe {
                    debug_utils_loader
                        .create_debug_utils_messenger(&debug_info, None)
                        .unwrap()
                }
            };

            (Some(debug_utils_loader), Some(debug_call_back))
        } else {
            (None, None)
        };

        let surface_loader = khr::surface::Instance::new(&entry, &instance);

        let (pdevice, queue_family_index) = unsafe {
            instance
                .enumerate_physical_devices()
                .expect("Physical device error")
                .iter()
                .find_map(|pdevice| {
                    instance
                        .get_physical_device_queue_family_properties(*pdevice)
                        .iter()
                        .enumerate()
                        .find_map(|(index, info)| {
                            if info.queue_flags.contains(vk::QueueFlags::GRAPHICS)
                                && surface_loader
                                    .get_physical_device_surface_support(
                                        *pdevice,
                                        index as u32,
                                        surface,
                                    )
                                    .unwrap()
                            {
                                Some((*pdevice, index as u32))
                            } else {
                                None
                            }
                        })
                })
                .expect("Couldn't find suitable device.")
        };

        let device: ash::Device = {
            let device_extension_names_raw = [
                khr::swapchain::NAME.as_ptr(),
                khr::shader_non_semantic_info::NAME.as_ptr(),
            ];
            let features = vk::PhysicalDeviceFeatures {
                shader_clip_distance: 1,
                ..Default::default()
            };
            let priorities = [1.0];
            let queue_info = [vk::DeviceQueueCreateInfo::default()
                .queue_family_index(queue_family_index)
                .queue_priorities(&priorities)];

            let mut vulkan_memory_model_features =
                vk::PhysicalDeviceVulkanMemoryModelFeatures::default().vulkan_memory_model(true);

            let device_create_info = vk::DeviceCreateInfo::default()
                .push_next(&mut vulkan_memory_model_features)
                .queue_create_infos(&queue_info)
                .enabled_extension_names(&device_extension_names_raw)
                .enabled_features(&features);
            unsafe {
                instance
                    .create_device(pdevice, &device_create_info, None)
                    .unwrap()
            }
        };

        let swapchain_loader = khr::swapchain::Device::new(&instance, &device);

        let present_queue = unsafe { device.get_device_queue(queue_family_index, 0) };

        let surface_format = {
            let acceptable_formats = {
                [
                    vk::Format::R8G8B8_SRGB,
                    vk::Format::B8G8R8_SRGB,
                    vk::Format::R8G8B8A8_SRGB,
                    vk::Format::B8G8R8A8_SRGB,
                    vk::Format::A8B8G8R8_SRGB_PACK32,
                ]
            };
            unsafe {
                *surface_loader
                    .get_physical_device_surface_formats(pdevice, surface)
                    .unwrap()
                    .iter()
                    .find(|sfmt| acceptable_formats.contains(&sfmt.format))
                    .expect("Unable to find suitable surface format.")
            }
        };

        Self {
            window,
            entry,
            instance,
            device,
            swapchain_loader,
            debug_utils_loader,
            debug_call_back,
            pdevice,
            queue_family_index,
            present_queue,
            surface,
            surface_loader,
            surface_format,
        }
    }

    pub fn surface_resolution(&self) -> vk::Extent2D {
        let surface_capabilities = self.surface_capabilities();
        match surface_capabilities.current_extent.width {
            std::u32::MAX => {
                let window_inner = self.window.inner_size();
                vk::Extent2D {
                    width: window_inner.width,
                    height: window_inner.height,
                }
            }
            _ => surface_capabilities.current_extent,
        }
    }

    pub fn surface_capabilities(&self) -> vk::SurfaceCapabilitiesKHR {
        unsafe {
            self.surface_loader
                .get_physical_device_surface_capabilities(self.pdevice, self.surface)
                .unwrap()
        }
    }

    pub fn create_swapchain(&self) -> (vk::SwapchainKHR, vk::Extent2D) {
        let surface_capabilities = self.surface_capabilities();
        let mut desired_image_count = surface_capabilities.min_image_count + 1;
        if surface_capabilities.max_image_count > 0
            && desired_image_count > surface_capabilities.max_image_count
        {
            desired_image_count = surface_capabilities.max_image_count;
        }
        let pre_transform = if surface_capabilities
            .supported_transforms
            .contains(vk::SurfaceTransformFlagsKHR::IDENTITY)
        {
            vk::SurfaceTransformFlagsKHR::IDENTITY
        } else {
            surface_capabilities.current_transform
        };
        let present_mode = unsafe {
            self.surface_loader
                .get_physical_device_surface_present_modes(self.pdevice, self.surface)
                .unwrap()
                .iter()
                .cloned()
                .find(|&mode| mode == vk::PresentModeKHR::MAILBOX)
                .unwrap_or(vk::PresentModeKHR::FIFO)
        };
        let extent = self.surface_resolution();
        let swapchain_create_info = vk::SwapchainCreateInfoKHR::default()
            .surface(self.surface)
            .min_image_count(desired_image_count)
            .image_color_space(self.surface_format.color_space)
            .image_format(self.surface_format.format)
            .image_extent(extent)
            .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
            .image_sharing_mode(vk::SharingMode::EXCLUSIVE)
            .pre_transform(pre_transform)
            .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE)
            .present_mode(present_mode)
            .clipped(true)
            .image_array_layers(1);
        let swapchain = unsafe {
            self.swapchain_loader
                .create_swapchain(&swapchain_create_info, None)
                .unwrap()
        };
        (swapchain, extent)
    }

    pub fn create_image_views(&self, swapchain: vk::SwapchainKHR) -> Vec<vk::ImageView> {
        unsafe {
            self.swapchain_loader
                .get_swapchain_images(swapchain)
                .unwrap()
                .iter()
                .map(|&image| {
                    let create_view_info = vk::ImageViewCreateInfo::default()
                        .view_type(vk::ImageViewType::TYPE_2D)
                        .format(self.surface_format.format)
                        .components(vk::ComponentMapping {
                            r: vk::ComponentSwizzle::R,
                            g: vk::ComponentSwizzle::G,
                            b: vk::ComponentSwizzle::B,
                            a: vk::ComponentSwizzle::A,
                        })
                        .subresource_range(vk::ImageSubresourceRange {
                            aspect_mask: vk::ImageAspectFlags::COLOR,
                            base_mip_level: 0,
                            level_count: 1,
                            base_array_layer: 0,
                            layer_count: 1,
                        })
                        .image(image);
                    self.device
                        .create_image_view(&create_view_info, None)
                        .unwrap()
                })
                .collect()
        }
    }

    pub fn create_framebuffers(
        &self,
        image_views: &[vk::ImageView],
        render_pass: vk::RenderPass,
        extent: vk::Extent2D,
    ) -> Vec<vk::Framebuffer> {
        image_views
            .iter()
            .map(|&present_image_view| {
                let framebuffer_attachments = [present_image_view];
                unsafe {
                    self.device
                        .create_framebuffer(
                            &vk::FramebufferCreateInfo::default()
                                .render_pass(render_pass)
                                .attachments(&framebuffer_attachments)
                                .width(extent.width)
                                .height(extent.height)
                                .layers(1),
                            None,
                        )
                        .unwrap()
                }
            })
            .collect()
    }

    pub fn create_render_pass(&self) -> vk::RenderPass {
        let renderpass_attachments = [vk::AttachmentDescription {
            format: self.surface_format.format,
            samples: vk::SampleCountFlags::TYPE_1,
            load_op: vk::AttachmentLoadOp::CLEAR,
            store_op: vk::AttachmentStoreOp::STORE,
            final_layout: vk::ImageLayout::PRESENT_SRC_KHR,
            ..Default::default()
        }];
        let color_attachment_refs = [vk::AttachmentReference {
            attachment: 0,
            layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
        }];
        let dependencies = [vk::SubpassDependency {
            src_subpass: vk::SUBPASS_EXTERNAL,
            src_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ
                | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
            dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            ..Default::default()
        }];
        let subpasses = [vk::SubpassDescription::default()
            .color_attachments(&color_attachment_refs)
            .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)];
        let renderpass_create_info = vk::RenderPassCreateInfo::default()
            .attachments(&renderpass_attachments)
            .subpasses(&subpasses)
            .dependencies(&dependencies);
        unsafe {
            self.device
                .create_render_pass(&renderpass_create_info, None)
                .unwrap()
        }
    }

    pub fn create_render_sync(&self) -> RenderSync {
        RenderSync::new(self)
    }
}

impl Drop for RenderBase {
    fn drop(&mut self) {
        unsafe {
            self.device.destroy_device(None);
            self.surface_loader.destroy_surface(self.surface, None);
            if let Some((debug_utils, call_back)) =
                Option::zip(self.debug_utils_loader.take(), self.debug_call_back.take())
            {
                debug_utils.destroy_debug_utils_messenger(call_back, None);
            }
            self.instance.destroy_instance(None);
        }
    }
}

pub struct RenderCtx {
    pub base: RenderBase,
    pub sync: RenderSync,

    pub swapchain: vk::SwapchainKHR,
    pub extent: vk::Extent2D,
    pub image_views: Vec<vk::ImageView>,
    pub render_pass: vk::RenderPass,
    pub framebuffers: Vec<vk::Framebuffer>,
    pub commands: RenderCommandPool,
    pub viewports: Box<[vk::Viewport]>,
    pub scissors: Box<[vk::Rect2D]>,
    pub shader_code: Vec<u32>,
    pub pipeline: Option<Pipeline>,

    pub rendering_paused: bool,
    pub recompiling_shaders: bool,
    pub start: std::time::Instant,

    // Only used for sky-shader.
    // NOTE(eddyb) this acts like an integration test for specialization constants.
    pub sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor: u32,
}

impl RenderCtx {
    pub fn new(base: RenderBase, shader_code: Vec<u32>) -> Self {
        let sync = RenderSync::new(&base);

        let (swapchain, extent) = base.create_swapchain();
        let image_views = base.create_image_views(swapchain);
        let render_pass = base.create_render_pass();
        let framebuffers = base.create_framebuffers(&image_views, render_pass, extent);
        let commands = RenderCommandPool::new(&base);
        let (viewports, scissors) = {
            (
                Box::new([vk::Viewport {
                    x: 0.0,
                    y: extent.height as f32,
                    width: extent.width as f32,
                    height: -(extent.height as f32),
                    min_depth: 0.0,
                    max_depth: 1.0,
                }]),
                Box::new([vk::Rect2D {
                    offset: vk::Offset2D { x: 0, y: 0 },
                    extent,
                }]),
            )
        };

        Self {
            sync,
            base,
            swapchain,
            extent,
            image_views,
            commands,
            render_pass,
            framebuffers,
            viewports,
            scissors,
            shader_code,
            pipeline: None,
            rendering_paused: false,
            recompiling_shaders: false,
            start: std::time::Instant::now(),

            sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor: 100,
        }
    }

    /// Update shaders and rebuild the pipeline
    pub fn rebuild_pipeline(&mut self) {
        unsafe {
            self.cleanup_pipeline();

            let shader_module = self
                .base
                .device
                .create_shader_module(
                    &vk::ShaderModuleCreateInfo::default().code(&self.shader_code),
                    None,
                )
                .unwrap();

            let pipeline_layout = self
                .base
                .device
                .create_pipeline_layout(
                    &vk::PipelineLayoutCreateInfo::default().push_constant_ranges(&[
                        vk::PushConstantRange::default()
                            .offset(0)
                            .size(size_of::<ShaderConstants>() as u32)
                            .stage_flags(vk::ShaderStageFlags::ALL),
                    ]),
                    None,
                )
                .unwrap();

            let mut pipelines =
				self.base
					.device
					.create_graphics_pipelines(vk::PipelineCache::null(), &[vk::GraphicsPipelineCreateInfo::default()
						.stages(
							&[
								vk::PipelineShaderStageCreateInfo {
									module: shader_module,
									p_name: c"main_vs".as_ptr(),
									stage: vk::ShaderStageFlags::VERTEX,
									..Default::default()
								},
								vk::PipelineShaderStageCreateInfo {
									module: shader_module,
									p_name: c"main_fs".as_ptr(),
									stage: vk::ShaderStageFlags::FRAGMENT,
									// NOTE(eddyb) this acts like an integration test for specialization constants.
									p_specialization_info: &vk::SpecializationInfo::default()
										.map_entries(&[vk::SpecializationMapEntry::default()
											.constant_id(0x5007)
											.offset(0)
											.size(4)])
										.data(&u32::to_le_bytes(
											self.sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor,
										)),
									..Default::default()
								},
							],
						)
						.vertex_input_state(&vk::PipelineVertexInputStateCreateInfo::default())
						.input_assembly_state(&vk::PipelineInputAssemblyStateCreateInfo {
							topology: vk::PrimitiveTopology::TRIANGLE_LIST,
							..Default::default()
						})
						.rasterization_state(&vk::PipelineRasterizationStateCreateInfo {
							front_face: vk::FrontFace::COUNTER_CLOCKWISE,
							line_width: 1.0,
							..Default::default()
						})
						.multisample_state(&vk::PipelineMultisampleStateCreateInfo {
                            rasterization_samples: vk::SampleCountFlags::TYPE_1,
                            ..Default::default()
                        })
						.depth_stencil_state(&vk::PipelineDepthStencilStateCreateInfo::default())
						.color_blend_state(
							&vk::PipelineColorBlendStateCreateInfo::default()
								.attachments(
									&[vk::PipelineColorBlendAttachmentState {
										blend_enable: 0,
										src_color_blend_factor: vk::BlendFactor::SRC_COLOR,
										dst_color_blend_factor: vk::BlendFactor::ONE_MINUS_DST_COLOR,
										color_blend_op: vk::BlendOp::ADD,
										src_alpha_blend_factor: vk::BlendFactor::ZERO,
										dst_alpha_blend_factor: vk::BlendFactor::ZERO,
										alpha_blend_op: vk::BlendOp::ADD,
										color_write_mask: vk::ColorComponentFlags::RGBA,
									}],
								),
						)
						.dynamic_state(
							&vk::PipelineDynamicStateCreateInfo::default()
								.dynamic_states(&[vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR]),
						)
						.viewport_state(
							&vk::PipelineViewportStateCreateInfo::default()
								.scissor_count(1)
								.viewport_count(1),
						)
						.layout(pipeline_layout)
						.render_pass(self.render_pass)], None)
					.expect("Unable to create graphics pipeline");

            // A single `pipeline_info` results in a single pipeline.
            assert_eq!(pipelines.len(), 1);
            self.pipeline = pipelines.pop().map(|pipeline| Pipeline {
                pipeline,
                pipeline_layout,
            });

            // shader modules are allowed to be deleted after the pipeline has been created
            self.base.device.destroy_shader_module(shader_module, None);
        }
    }

    pub unsafe fn cleanup_pipeline(&mut self) {
        unsafe {
            self.base.device.device_wait_idle().unwrap();
            if let Some(pipeline) = self.pipeline.take() {
                self.base.device.destroy_pipeline(pipeline.pipeline, None);
                self.base
                    .device
                    .destroy_pipeline_layout(pipeline.pipeline_layout, None);
            }
        }
    }

    /// Destroys the swapchain, as well as the renderpass and frame and command buffers
    pub fn cleanup_swapchain(&mut self) {
        unsafe {
            self.base.device.device_wait_idle().unwrap();
            // framebuffers
            for framebuffer in self.framebuffers.drain(..) {
                self.base.device.destroy_framebuffer(framebuffer, None);
            }
            // image views
            for image_view in self.image_views.drain(..) {
                self.base.device.destroy_image_view(image_view, None);
            }
            // swapchain
            self.base
                .swapchain_loader
                .destroy_swapchain(self.swapchain, None);
        }
    }

    /// Recreates the swapchain, but does not recreate the pipelines because they use dynamic state.
    pub fn recreate_swapchain(&mut self) {
        let surface_resolution = self.base.surface_resolution();

        if surface_resolution.width == 0 || surface_resolution.height == 0 {
            self.rendering_paused = true;
            return;
        } else if self.rendering_paused {
            self.rendering_paused = false;
        };

        self.cleanup_swapchain();

        let (swapchain, extent) = self.base.create_swapchain();
        self.swapchain = swapchain;
        self.extent = extent;
        self.image_views = self.base.create_image_views(self.swapchain);
        self.framebuffers =
            self.base
                .create_framebuffers(&self.image_views, self.render_pass, extent);
        self.viewports = Box::new([vk::Viewport {
            x: 0.0,
            y: extent.height as f32,
            width: extent.width as f32,
            height: -(extent.height as f32),
            min_depth: 0.0,
            max_depth: 1.0,
        }]);
        self.scissors = Box::new([vk::Rect2D {
            offset: vk::Offset2D { x: 0, y: 0 },
            extent,
        }]);
    }

    pub fn render(&mut self) {
        let present_index = unsafe {
            match self.base.swapchain_loader.acquire_next_image(
                self.swapchain,
                u64::MAX,
                self.sync.present_complete_semaphore,
                vk::Fence::null(),
            ) {
                Ok((idx, _)) => idx,
                Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                    self.recreate_swapchain();
                    return;
                }
                Err(err) => panic!("failed to acquire next image: {err:?}"),
            }
        };

        let framebuffer = self.framebuffers[present_index as usize];
        let clear_values = [vk::ClearValue {
            color: vk::ClearColorValue {
                float32: [0.0, 1.0, 0.0, 0.0],
            },
        }];

        self.draw(self.pipeline.as_ref().unwrap(), framebuffer, &clear_values);

        let wait_semaphors = [self.sync.rendering_complete_semaphore];
        let swapchains = [self.swapchain];
        let image_indices = [present_index];
        let present_info = vk::PresentInfoKHR::default()
            .wait_semaphores(&wait_semaphors)
            .swapchains(&swapchains)
            .image_indices(&image_indices);
        unsafe {
            match self
                .base
                .swapchain_loader
                .queue_present(self.base.present_queue, &present_info)
            {
                Err(vk::Result::ERROR_OUT_OF_DATE_KHR) | Ok(true) => self.recreate_swapchain(),
                Ok(false) => {}
                Err(err) => panic!("failed to present queue: {err:?}"),
            };
        }
    }

    pub fn draw(
        &self,
        pipeline: &Pipeline,
        framebuffer: vk::Framebuffer,
        clear_values: &[vk::ClearValue],
    ) {
        let render_pass_begin_info = vk::RenderPassBeginInfo::default()
            .render_pass(self.render_pass)
            .framebuffer(framebuffer)
            .render_area(vk::Rect2D {
                offset: vk::Offset2D { x: 0, y: 0 },
                extent: self.scissors[0].extent,
            })
            .clear_values(clear_values);
        self.record_submit_commandbuffer(
            &[vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT],
            |device, draw_command_buffer| unsafe {
                device.cmd_begin_render_pass(
                    draw_command_buffer,
                    &render_pass_begin_info,
                    vk::SubpassContents::INLINE,
                );
                device.cmd_bind_pipeline(
                    draw_command_buffer,
                    vk::PipelineBindPoint::GRAPHICS,
                    pipeline.pipeline,
                );
                device.cmd_set_viewport(draw_command_buffer, 0, &self.viewports);
                device.cmd_set_scissor(draw_command_buffer, 0, &self.scissors);

                let push_constants = ShaderConstants {
                    width: self.scissors[0].extent.width,
                    height: self.scissors[0].extent.height,
                    time: self.start.elapsed().as_secs_f32(),

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
                device.cmd_push_constants(
                    draw_command_buffer,
                    pipeline.pipeline_layout,
                    ash::vk::ShaderStageFlags::ALL,
                    0,
                    bytemuck::bytes_of(&push_constants),
                );

                device.cmd_draw(draw_command_buffer, 3, 1, 0, 0);
                device.cmd_end_render_pass(draw_command_buffer);
            },
        );
    }

    /// Helper function for submitting command buffers. Immediately waits for the fence before the command buffer
    /// is executed. That way we can delay the waiting for the fences by 1 frame which is good for performance.
    /// Make sure to create the fence in a signaled state on the first use.
    pub fn record_submit_commandbuffer<F: FnOnce(&ash::Device, vk::CommandBuffer)>(
        &self,
        wait_mask: &[vk::PipelineStageFlags],
        f: F,
    ) {
        unsafe {
            self.base
                .device
                .wait_for_fences(&[self.sync.draw_commands_reuse_fence], true, u64::MAX)
                .expect("Wait for fence failed.");

            self.base
                .device
                .reset_fences(&[self.sync.draw_commands_reuse_fence])
                .expect("Reset fences failed.");

            // As we only have a single command buffer, we can simply reset the entire pool instead of just the buffer.
            // Doing this is a little bit faster, see
            // https://arm-software.github.io/vulkan_best_practice_for_mobile_developers/samples/performance/command_buffer_usage/command_buffer_usage_tutorial.html#resetting-the-command-pool
            self.base
                .device
                .reset_command_pool(self.commands.pool, vk::CommandPoolResetFlags::empty())
                .expect("Reset command pool failed.");

            let command_buffer_begin_info = vk::CommandBufferBeginInfo::default()
                .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);

            self.base
                .device
                .begin_command_buffer(
                    self.commands.draw_command_buffer,
                    &command_buffer_begin_info,
                )
                .expect("Begin commandbuffer");

            f(&self.base.device, self.commands.draw_command_buffer);

            self.base
                .device
                .end_command_buffer(self.commands.draw_command_buffer)
                .expect("End commandbuffer");

            let command_buffers = vec![self.commands.draw_command_buffer];
            let wait_semaphores = &[self.sync.present_complete_semaphore];
            let signal_semaphores = &[self.sync.rendering_complete_semaphore];
            let submit_info = vk::SubmitInfo::default()
                .wait_semaphores(wait_semaphores)
                .wait_dst_stage_mask(wait_mask)
                .command_buffers(&command_buffers)
                .signal_semaphores(signal_semaphores);

            self.base
                .device
                .queue_submit(
                    self.base.present_queue,
                    &[submit_info],
                    self.sync.draw_commands_reuse_fence,
                )
                .expect("queue submit failed.");
        }
    }
}

impl Drop for RenderCtx {
    fn drop(&mut self) {
        unsafe {
            self.base.device.device_wait_idle().unwrap();
            self.base
                .device
                .destroy_semaphore(self.sync.present_complete_semaphore, None);
            self.base
                .device
                .destroy_semaphore(self.sync.rendering_complete_semaphore, None);
            self.base
                .device
                .destroy_fence(self.sync.draw_commands_reuse_fence, None);
            self.base
                .device
                .free_command_buffers(self.commands.pool, &[self.commands.draw_command_buffer]);
            self.base.device.destroy_render_pass(self.render_pass, None);
            self.cleanup_pipeline();
            self.cleanup_swapchain();
            self.base
                .device
                .destroy_command_pool(self.commands.pool, None);
        }
    }
}

pub struct RenderSync {
    pub present_complete_semaphore: vk::Semaphore,
    pub rendering_complete_semaphore: vk::Semaphore,
    pub draw_commands_reuse_fence: vk::Fence,
}

impl RenderSync {
    pub fn new(base: &RenderBase) -> Self {
        let fence_create_info =
            vk::FenceCreateInfo::default().flags(vk::FenceCreateFlags::SIGNALED);

        let semaphore_create_info = vk::SemaphoreCreateInfo::default();

        unsafe {
            let draw_commands_reuse_fence = base
                .device
                .create_fence(&fence_create_info, None)
                .expect("Create fence failed.");
            let present_complete_semaphore = base
                .device
                .create_semaphore(&semaphore_create_info, None)
                .unwrap();
            let rendering_complete_semaphore = base
                .device
                .create_semaphore(&semaphore_create_info, None)
                .unwrap();

            Self {
                present_complete_semaphore,
                rendering_complete_semaphore,
                draw_commands_reuse_fence,
            }
        }
    }
}

pub struct RenderCommandPool {
    pub pool: vk::CommandPool,
    pub draw_command_buffer: vk::CommandBuffer,
}

impl RenderCommandPool {
    pub fn new(base: &RenderBase) -> Self {
        let pool = {
            let pool_create_info =
                vk::CommandPoolCreateInfo::default().queue_family_index(base.queue_family_index);

            unsafe {
                base.device
                    .create_command_pool(&pool_create_info, None)
                    .unwrap()
            }
        };

        let command_buffers = {
            let command_buffer_allocate_info = vk::CommandBufferAllocateInfo::default()
                .command_buffer_count(1)
                .command_pool(pool)
                .level(vk::CommandBufferLevel::PRIMARY);

            unsafe {
                base.device
                    .allocate_command_buffers(&command_buffer_allocate_info)
                    .unwrap()
            }
        };

        Self {
            pool,
            draw_command_buffer: command_buffers[0],
        }
    }
}

pub struct Pipeline {
    pub pipeline: vk::Pipeline,
    pub pipeline_layout: vk::PipelineLayout,
}

unsafe extern "system" fn vulkan_debug_callback(
    message_severity: vk::DebugUtilsMessageSeverityFlagsEXT,
    _message_type: vk::DebugUtilsMessageTypeFlagsEXT,
    p_callback_data: *const vk::DebugUtilsMessengerCallbackDataEXT<'_>,
    _user_data: *mut std::os::raw::c_void,
) -> vk::Bool32 {
    let callback_data = unsafe { *p_callback_data };
    let message_id_name = if callback_data.p_message_id_name.is_null() {
        Cow::from("")
    } else {
        unsafe { CStr::from_ptr(callback_data.p_message_id_name).to_string_lossy() }
    };
    let message = if callback_data.p_message.is_null() {
        Cow::from("")
    } else {
        unsafe { CStr::from_ptr(callback_data.p_message).to_string_lossy() }
    };
    println!("{message_severity:?}: [{message_id_name}] : {message}");
    vk::FALSE
}
