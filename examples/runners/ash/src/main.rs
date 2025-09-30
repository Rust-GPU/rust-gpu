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

use anyhow::{Context, anyhow};
use ash::{ext, khr, util::read_spv, vk};
use clap::{Parser, ValueEnum};
use raw_window_handle::{HasDisplayHandle as _, HasWindowHandle as _};
use shared::ShaderConstants;
use spirv_builder::{MetadataPrintout, SpirvBuilder};
use std::ffi::c_char;
use std::ops::Deref;
use std::sync::Arc;
use std::{
    borrow::Cow,
    ffi::CStr,
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

/// Central struct containing the Vulkan instance and device, among others
pub struct MyDevice {
    pub entry: ash::Entry,
    pub instance: ash::Instance,
    pub physical_device: vk::PhysicalDevice,
    pub device: ash::Device,
    pub main_queue_family: u32,
    pub main_queue: vk::Queue,
    pub debug_ext_instance: ext::debug_utils::Instance,
    pub debug_ext_device: ext::debug_utils::Device,
    pub surface_ext: khr::surface::Instance,
    pub swapchain_ext: khr::swapchain::Device,
    debug_callback: vk::DebugUtilsMessengerEXT,
}

impl Deref for MyDevice {
    type Target = ash::Device;

    fn deref(&self) -> &Self::Target {
        &self.device
    }
}

impl MyDevice {
    pub fn new(extension_names: &[*const c_char], options: &Options) -> anyhow::Result<Arc<Self>> {
        unsafe {
            cfg_if::cfg_if! {
                if #[cfg(target_os = "macos")] {
                    let entry = ash_molten::load();
                } else {
                    let entry = ash::Entry::load()?;
                }
            }

            let instance = {
                let layer_names: &'static [_] = if options.debug_layer {
                    const { &[c"VK_LAYER_KHRONOS_validation".as_ptr()] }
                } else {
                    &[]
                };

                let mut extension_names_raw = extension_names.to_vec();
                extension_names_raw.push(ext::debug_utils::NAME.as_ptr());

                let app_name = c"VulkanTriangle";
                entry
                    .create_instance(
                        &vk::InstanceCreateInfo::default()
                            .application_info(
                                &vk::ApplicationInfo::default()
                                    .application_name(app_name)
                                    .application_version(0)
                                    .engine_name(app_name)
                                    .engine_version(0)
                                    .api_version(vk::make_api_version(0, 1, 3, 0)),
                            )
                            .enabled_layer_names(layer_names)
                            .enabled_extension_names(&extension_names_raw),
                        None,
                    )
                    .context("create_instance")?
            };

            let debug_instance = ext::debug_utils::Instance::new(&entry, &instance);
            let debug_callback = {
                debug_instance.create_debug_utils_messenger(
                    &vk::DebugUtilsMessengerCreateInfoEXT::default()
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
                        .pfn_user_callback(Some(vulkan_debug_callback)),
                    None,
                )?
            };

            let physical_device = {
                instance
                    .enumerate_physical_devices()?
                    .into_iter()
                    .min_by_key(|phy| {
                        match instance.get_physical_device_properties(*phy).device_type {
                            vk::PhysicalDeviceType::DISCRETE_GPU => 1,
                            vk::PhysicalDeviceType::VIRTUAL_GPU => 2,
                            vk::PhysicalDeviceType::INTEGRATED_GPU => 3,
                            vk::PhysicalDeviceType::CPU => 4,
                            _ => 5,
                        }
                    })
                    .ok_or(anyhow!("No physical devices available"))?
            };

            let main_queue_family = {
                instance
                    .get_physical_device_queue_family_properties(physical_device)
                    .into_iter()
                    .enumerate()
                    .find(|(_, prop)| prop.queue_flags.contains(vk::QueueFlags::GRAPHICS))
                    .ok_or(anyhow!(
                        "No graphics + compute queues on physical device available"
                    ))?
                    .0 as u32
            };

            let device = instance
                .create_device(
                    physical_device,
                    &vk::DeviceCreateInfo::default()
                        .push_next(
                            &mut vk::PhysicalDeviceVulkanMemoryModelFeatures::default()
                                .vulkan_memory_model(true),
                        )
                        .push_next(
                            &mut vk::PhysicalDeviceVulkan13Features::default()
                                .synchronization2(true)
                                .dynamic_rendering(true),
                        )
                        .queue_create_infos(&[vk::DeviceQueueCreateInfo::default()
                            .queue_family_index(main_queue_family)
                            .queue_priorities(&[1.0])])
                        .enabled_extension_names(&[
                            khr::swapchain::NAME.as_ptr(),
                            khr::shader_non_semantic_info::NAME.as_ptr(),
                        ]),
                    None,
                )
                .context("create_device")?;
            let main_queue = device.get_device_queue(main_queue_family, 0);

            Ok(Arc::new(Self {
                debug_ext_device: ext::debug_utils::Device::new(&instance, &device),
                surface_ext: khr::surface::Instance::new(&entry, &instance),
                swapchain_ext: khr::swapchain::Device::new(&instance, &device),
                entry,
                instance,
                physical_device,
                device,
                main_queue_family,
                main_queue,
                debug_ext_instance: debug_instance,
                debug_callback,
            }))
        }
    }
}

impl Drop for MyDevice {
    fn drop(&mut self) {
        unsafe {
            self.debug_ext_instance
                .destroy_debug_utils_messenger(self.debug_callback, None);
            self.device.destroy_device(None);
            self.instance.destroy_instance(None);
        }
    }
}

/// A binary semaphore for swapchain operations
pub struct SwapchainSync {
    acquire_semaphore: vk::Semaphore,
    render_semaphore: vk::Semaphore,
    render_fence: vk::Fence,
}

impl SwapchainSync {
    pub unsafe fn new(device: &MyDevice) -> anyhow::Result<Self> {
        unsafe {
            let signaled_fence =
                vk::FenceCreateInfo::default().flags(vk::FenceCreateFlags::SIGNALED);
            Ok(Self {
                acquire_semaphore: device
                    .create_semaphore(&vk::SemaphoreCreateInfo::default(), None)?,
                render_semaphore: device
                    .create_semaphore(&vk::SemaphoreCreateInfo::default(), None)?,
                render_fence: device.create_fence(&signaled_fence, None)?,
            })
        }
    }

    pub unsafe fn destroy(&self, device: &MyDevice) {
        unsafe {
            device.destroy_semaphore(self.acquire_semaphore, None);
            device.destroy_semaphore(self.render_semaphore, None);
            device.destroy_fence(self.render_fence, None);
        }
    }
}

/// Takes care of all things swapchain related
///
/// Intentionally kept simple and does not offer support for multiple frames in flight
pub struct MySwapchainManager {
    pub device: Arc<MyDevice>,
    pub window: winit::window::Window,
    pub surface: vk::SurfaceKHR,
    pub surface_format: vk::SurfaceFormatKHR,
    pub surface_capabilities: vk::SurfaceCapabilitiesKHR,
    pub present_mode: vk::PresentModeKHR,
    pub image_count: u32,
    pub pre_transform: vk::SurfaceTransformFlagsKHR,

    // state below
    active: Option<ActiveSwapchain>,
    should_recreate: bool,
    sync: SwapchainSync,
}

struct ActiveSwapchain {
    extent: vk::Extent2D,
    swapchain: vk::SwapchainKHR,
    images: Vec<(vk::Image, vk::ImageView)>,
}

impl MySwapchainManager {
    pub fn new(device: Arc<MyDevice>, window: winit::window::Window) -> anyhow::Result<Self> {
        unsafe {
            let surface_ext = &device.surface_ext;

            let surface = ash_window::create_surface(
                &device.entry,
                &device.instance,
                window.display_handle().unwrap().into(),
                window.window_handle().unwrap().into(),
                None,
            )
            .context("create_surface")?;

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
                *surface_ext
                    .get_physical_device_surface_formats(device.physical_device, surface)?
                    .iter()
                    .find(|sfmt| acceptable_formats.contains(&sfmt.format))
                    .context("Unable to find suitable surface format.")?
            };

            let surface_capabilities = surface_ext
                .get_physical_device_surface_capabilities(device.physical_device, surface)?;
            let pre_transform = surface_capabilities.current_transform;

            let present_mode = surface_ext
                .get_physical_device_surface_present_modes(device.physical_device, surface)?
                .iter()
                .cloned()
                // Mailbox is preferred
                .find(|&mode| mode == vk::PresentModeKHR::MAILBOX)
                // FIFO is guaranteed to be available
                .unwrap_or(vk::PresentModeKHR::FIFO);

            let image_count = {
                let mut image_count = match present_mode {
                    // tripple buffering in mailbox mode:: one presenting, one ready and one drawing
                    vk::PresentModeKHR::MAILBOX => 3,
                    // double buffering in fifo mode: one presenting, one drawing
                    vk::PresentModeKHR::FIFO => 2,
                    _ => unreachable!(),
                };
                if surface_capabilities.max_image_count != 0 {
                    image_count = image_count.min(surface_capabilities.max_image_count);
                }
                image_count.max(surface_capabilities.min_image_count)
            };

            let sync = SwapchainSync::new(&device)?;
            Ok(Self {
                device,
                window,
                surface,
                surface_format,
                surface_capabilities,
                present_mode,
                image_count,
                pre_transform,

                active: None,
                should_recreate: true,
                sync,
            })
        }
    }

    #[inline]
    fn should_recreate(&mut self) {
        self.should_recreate = true;
    }

    /// After this function is called, `Self.active` is initialized
    unsafe fn recreate_swapchain(&mut self) -> anyhow::Result<()> {
        unsafe {
            let device = &self.device;
            let swapchain_ext = &device.swapchain_ext;
            let surface_ext = &self.device.surface_ext;
            let format = self.surface_format.format;

            let extent = {
                let window_size = self.window.inner_size();
                let capabilities = surface_ext.get_physical_device_surface_capabilities(
                    self.device.physical_device,
                    self.surface,
                )?;
                let min = capabilities.min_image_extent;
                let max = capabilities.max_image_extent;
                vk::Extent2D {
                    width: u32::clamp(window_size.width, min.width, max.width),
                    height: u32::clamp(window_size.height, min.height, max.height),
                }
            };

            let old = self.active.take();
            if let Some(old) = old.as_ref() {
                old.destroy_image_views(device);
            }

            let swapchain = swapchain_ext
                .create_swapchain(
                    &vk::SwapchainCreateInfoKHR::default()
                        .surface(self.surface)
                        .min_image_count(self.image_count)
                        .image_color_space(self.surface_format.color_space)
                        .image_format(format)
                        .image_extent(extent)
                        .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
                        .image_sharing_mode(vk::SharingMode::EXCLUSIVE)
                        .pre_transform(self.pre_transform)
                        .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE)
                        .present_mode(self.present_mode)
                        .clipped(true)
                        .image_array_layers(1)
                        .old_swapchain(
                            old.as_ref()
                                .map_or(vk::SwapchainKHR::null(), |old| old.swapchain),
                        ),
                    None,
                )
                .context("create_swapchain")?;

            if let Some(old) = old.as_ref() {
                old.destroy_swapchain(device);
            }

            let images = device.swapchain_ext.get_swapchain_images(swapchain)?;
            let images = images
                .into_iter()
                .map(|image| {
                    let image_view = device.create_image_view(
                        &vk::ImageViewCreateInfo::default()
                            .image(image)
                            .view_type(vk::ImageViewType::TYPE_2D)
                            .format(format)
                            .components(vk::ComponentMapping::default()) // identity
                            .subresource_range(vk::ImageSubresourceRange {
                                aspect_mask: vk::ImageAspectFlags::COLOR,
                                base_mip_level: 0,
                                level_count: 1,
                                base_array_layer: 0,
                                layer_count: 1,
                            }),
                        None,
                    )?;
                    Ok::<_, anyhow::Error>((image, image_view))
                })
                .collect::<Result<Vec<_>, _>>()?;

            self.active = Some(ActiveSwapchain {
                swapchain,
                images,
                extent,
            });
            Ok(())
        }
    }
}

impl ActiveSwapchain {
    /// We must destroy the image views we own, but not the images, those are owned by the swapchain.
    unsafe fn destroy_image_views(&self, device: &MyDevice) {
        unsafe {
            for (_, image_view) in &self.images {
                device.destroy_image_view(*image_view, None);
            }
        }
    }

    /// Destroying the swapchain destroys* the images, so image views must be destroyed beforehand.
    unsafe fn destroy_swapchain(&self, device: &MyDevice) {
        unsafe { device.swapchain_ext.destroy_swapchain(self.swapchain, None) }
    }
}

impl Drop for MySwapchainManager {
    fn drop(&mut self) {
        unsafe {
            self.sync.destroy(&self.device);
            if let Some(active) = self.active.as_ref() {
                active.destroy_image_views(&self.device);
                active.destroy_swapchain(&self.device);
            }
            self.device.surface_ext.destroy_surface(self.surface, None);
        }
    }
}

/// Metadata on drawing a single frame on some image
pub struct DrawFrame {
    /// the size of the image
    pub extent: vk::Extent2D,
    /// the [`vk::Image`] to draw to
    pub image: vk::Image,
    /// the [`vk::ImageImage`] to draw to, created from `image`
    pub image_view: vk::ImageView,
    /// the `acquire_image` semaphore that must be waited for before draw commands are executed
    pub acquire_semaphore: vk::Semaphore,
    /// the `draw_finished` semaphore that must be signaled when drawing to the image has finished
    pub draw_finished_semaphore: vk::Semaphore,
    /// the `draw_finished` fence that must be signaled when drawing to the image has finished
    pub draw_finished_fence: vk::Fence,
}

impl MySwapchainManager {
    pub fn render(
        &mut self,
        f: impl FnOnce(DrawFrame) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        unsafe {
            self.device
                .wait_for_fences(&[self.sync.render_fence], true, !0)?;
            self.device.reset_fences(&[self.sync.render_fence])?;

            const RECREATE_ATTEMPTS: u32 = 10;
            for _ in 0..RECREATE_ATTEMPTS {
                if self.should_recreate {
                    self.should_recreate = false;

                    // *In theory*, recreating the swapchain allows you to present any acquired images from the old
                    // swapchain. Which (iirc) requires you to wait for all images to be presented before destroying
                    // the old swapchain. But we just use the [`ash::Device::device_wait_idle`] "hack" to wait for all
                    // previous images to finish before immediately destroying the swapchain.
                    self.device.device_wait_idle()?;
                    self.recreate_swapchain()?;
                }

                let active = self.active.as_ref().unwrap();
                let swapchain_ext = &self.device.swapchain_ext;
                match swapchain_ext.acquire_next_image(
                    active.swapchain,
                    !0,
                    self.sync.acquire_semaphore,
                    vk::Fence::null(),
                ) {
                    Ok((id, suboptimal)) => {
                        if suboptimal {
                            self.should_recreate = true;
                        }
                        let (image, image_view) = active.images[id as usize];
                        f(DrawFrame {
                            extent: active.extent,
                            image,
                            image_view,
                            acquire_semaphore: self.sync.acquire_semaphore,
                            draw_finished_semaphore: self.sync.render_semaphore,
                            draw_finished_fence: self.sync.render_fence,
                        })?;

                        let suboptimal = swapchain_ext.queue_present(
                            self.device.main_queue,
                            &vk::PresentInfoKHR::default()
                                .swapchains(&[active.swapchain])
                                .image_indices(&[id])
                                .wait_semaphores(&[self.sync.render_semaphore]),
                        )?;
                        if suboptimal {
                            self.should_recreate = true;
                        }
                        return Ok(());
                    }
                    Err(vk::Result::ERROR_OUT_OF_DATE_KHR) => {
                        // retry
                        self.should_recreate = true;
                    }
                    Err(e) => {
                        return Err(e.into());
                    }
                }
            }
            panic!(
                "looped {} times trying to acquire swapchain image and failed repeatedly!",
                RECREATE_ATTEMPTS
            );
        }
    }
}

/// Manages the creation and recreation of [`MyRenderPipeline`], whenever new shader code ([`Self::set_shader_code`])
/// is submitted or the spec constant is changed ([`Self::set_sky_fs_sun_intensity_factor`])
pub struct MyRenderPipelineManager {
    pub device: Arc<MyDevice>,
    color_out_format: vk::Format,
    shader_code: Vec<u32>,
    pipeline: Option<MyRenderPipeline>,

    // Only used for sky-shader.
    // NOTE(eddyb) this acts like an integration test for specialization constants.
    sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor: u32,
    should_recreate: bool,
}

pub struct MyRenderPipeline {
    pub pipeline: vk::Pipeline,
    pub pipeline_layout: vk::PipelineLayout,
}

impl MyRenderPipelineManager {
    pub fn new(
        device: Arc<MyDevice>,
        color_out_format: vk::Format,
        shader_code: Vec<u32>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            device,
            color_out_format,
            shader_code,
            pipeline: None,
            sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor: 100,
            should_recreate: true,
        })
    }

    #[inline]
    pub fn set_sky_fs_sun_intensity_factor(&mut self, factor: u32) {
        self.sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor = factor;
        self.should_recreate();
    }

    #[inline]
    pub fn get_sky_fs_sun_intensity_factor(&self) -> u32 {
        self.sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor
    }

    #[inline]
    pub fn set_shader_code(&mut self, shader_code: Vec<u32>) {
        self.shader_code = shader_code;
        self.should_recreate();
    }

    #[inline]
    pub fn should_recreate(&mut self) {
        self.should_recreate = true;
    }

    pub fn get_pipeline(&mut self) -> anyhow::Result<&MyRenderPipeline> {
        if self.should_recreate {
            self.rebuild_pipeline()?;
        }
        Ok(self.pipeline.as_ref().unwrap())
    }

    /// Update shaders and rebuild the pipeline
    fn rebuild_pipeline(&mut self) -> anyhow::Result<()> {
        unsafe {
            self.destroy_pipeline()?;

            let shader_module = self.device.create_shader_module(
                &vk::ShaderModuleCreateInfo::default().code(&self.shader_code),
                None,
            )?;

            let pipeline_layout = self.device.create_pipeline_layout(
                &vk::PipelineLayoutCreateInfo::default().push_constant_ranges(&[
                    vk::PushConstantRange::default()
                        .offset(0)
                        .size(size_of::<ShaderConstants>() as u32)
                        .stage_flags(vk::ShaderStageFlags::ALL),
                ]),
                None,
            )?;

            let mut pipelines =
				self
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
                        .push_next(&mut vk::PipelineRenderingCreateInfo::default().color_attachment_formats(&[self.color_out_format]))
                    ], None).map_err(|(_, e)| e)
					.context("Unable to create graphics pipeline")?;

            // A single `pipeline_info` results in a single pipeline.
            assert_eq!(pipelines.len(), 1);
            self.pipeline = pipelines.pop().map(|pipeline| MyRenderPipeline {
                pipeline,
                pipeline_layout,
            });

            // shader modules are allowed to be deleted after the pipeline has been created
            self.device.destroy_shader_module(shader_module, None);
            Ok(())
        }
    }

    unsafe fn destroy_pipeline(&mut self) -> anyhow::Result<()> {
        unsafe {
            if let Some(pipeline) = self.pipeline.take() {
                // Figuring out when the pipeline stops being used is hard, so we take this shortcut
                self.device.device_wait_idle()?;

                self.device.destroy_pipeline(pipeline.pipeline, None);
                self.device
                    .destroy_pipeline_layout(pipeline.pipeline_layout, None);
            }
            Ok(())
        }
    }
}

impl MyRenderPipeline {
    pub fn render(
        &self,
        device: &MyDevice,
        cmd: vk::CommandBuffer,
        color_out: vk::ImageView,
        extent: vk::Extent2D,
        push_constants: ShaderConstants,
    ) -> anyhow::Result<()> {
        unsafe {
            let render_area = vk::Rect2D {
                offset: vk::Offset2D::default(),
                extent,
            };

            device.cmd_begin_rendering(
                cmd,
                &vk::RenderingInfo::default()
                    .render_area(render_area)
                    .layer_count(1)
                    .color_attachments(&[vk::RenderingAttachmentInfo::default()
                        .image_view(color_out)
                        .load_op(vk::AttachmentLoadOp::CLEAR)
                        .store_op(vk::AttachmentStoreOp::STORE)
                        .clear_value(vk::ClearValue {
                            color: vk::ClearColorValue {
                                float32: [0.0, 1.0, 0.0, 0.0],
                            },
                        })
                        .image_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)]),
            );
            device.cmd_bind_pipeline(cmd, vk::PipelineBindPoint::GRAPHICS, self.pipeline);
            device.cmd_set_viewport(
                cmd,
                0,
                &[vk::Viewport {
                    // contains a y-flip
                    x: 0.0,
                    y: extent.height as f32,
                    width: extent.width as f32,
                    height: -(extent.height as f32),
                    min_depth: 0.0,
                    max_depth: 1.0,
                }],
            );
            device.cmd_set_scissor(cmd, 0, &[render_area]);
            device.cmd_push_constants(
                cmd,
                self.pipeline_layout,
                vk::ShaderStageFlags::ALL,
                0,
                bytemuck::bytes_of(&push_constants),
            );
            device.cmd_draw(cmd, 3, 1, 0, 0);
            device.cmd_end_rendering(cmd);
            Ok(())
        }
    }
}

impl Drop for MyRenderPipelineManager {
    fn drop(&mut self) {
        unsafe {
            self.destroy_pipeline().ok();
        }
    }
}

/// The renderer manages our command buffer and submits the commands, using [`MyRenderPipeline`] for drawing.
pub struct MyRenderer {
    pub device: Arc<MyDevice>,
    pub pipeline: MyRenderPipelineManager,
    pub command: SingleCommandBuffer,
}

impl MyRenderer {
    pub fn new(pipeline: MyRenderPipelineManager) -> anyhow::Result<Self> {
        Ok(Self {
            command: SingleCommandBuffer::new(pipeline.device.clone())?,
            device: pipeline.device.clone(),
            pipeline,
        })
    }

    pub fn render_frame(
        &mut self,
        frame: DrawFrame,
        push_constants: ShaderConstants,
    ) -> anyhow::Result<()> {
        unsafe {
            let device = &self.device;
            let pipeline = self.pipeline.get_pipeline()?;
            let cmd = self.command.cmd;

            device.reset_command_pool(self.command.pool, vk::CommandPoolResetFlags::default())?;

            {
                device.begin_command_buffer(
                    cmd,
                    &vk::CommandBufferBeginInfo::default()
                        .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT),
                )?;
                device.cmd_pipeline_barrier2(
                    cmd,
                    &vk::DependencyInfo::default().image_memory_barriers(&[
                        vk::ImageMemoryBarrier2::default()
                            .image(frame.image)
                            .src_access_mask(vk::AccessFlags2::NONE)
                            .src_stage_mask(vk::PipelineStageFlags2::ALL_COMMANDS)
                            .old_layout(vk::ImageLayout::UNDEFINED)
                            .dst_access_mask(vk::AccessFlags2::COLOR_ATTACHMENT_WRITE)
                            .dst_stage_mask(vk::PipelineStageFlags2::COLOR_ATTACHMENT_OUTPUT)
                            .new_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                            .subresource_range(
                                vk::ImageSubresourceRange::default()
                                    .aspect_mask(vk::ImageAspectFlags::COLOR)
                                    .base_mip_level(0)
                                    .level_count(1)
                                    .base_array_layer(0)
                                    .layer_count(1),
                            ),
                    ]),
                );
                pipeline.render(&device, cmd, frame.image_view, frame.extent, push_constants)?;
                device.cmd_pipeline_barrier2(
                    cmd,
                    &vk::DependencyInfo::default().image_memory_barriers(&[
                        vk::ImageMemoryBarrier2::default()
                            .image(frame.image)
                            .src_access_mask(vk::AccessFlags2::COLOR_ATTACHMENT_WRITE)
                            .src_stage_mask(vk::PipelineStageFlags2::COLOR_ATTACHMENT_OUTPUT)
                            .old_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                            .dst_access_mask(vk::AccessFlags2::NONE)
                            .dst_stage_mask(vk::PipelineStageFlags2::ALL_COMMANDS)
                            .new_layout(vk::ImageLayout::PRESENT_SRC_KHR)
                            .subresource_range(
                                vk::ImageSubresourceRange::default()
                                    .aspect_mask(vk::ImageAspectFlags::COLOR)
                                    .base_mip_level(0)
                                    .level_count(1)
                                    .base_array_layer(0)
                                    .layer_count(1),
                            ),
                    ]),
                );
                device.end_command_buffer(cmd)?;
            }

            device.queue_submit2(
                device.main_queue,
                &[vk::SubmitInfo2::default()
                    .wait_semaphore_infos(&[vk::SemaphoreSubmitInfo::default()
                        .semaphore(frame.acquire_semaphore)
                        .stage_mask(vk::PipelineStageFlags2::TOP_OF_PIPE)])
                    .command_buffer_infos(&[
                        vk::CommandBufferSubmitInfo::default().command_buffer(cmd)
                    ])
                    .signal_semaphore_infos(&[vk::SemaphoreSubmitInfo::default()
                        .semaphore(frame.draw_finished_semaphore)
                        .stage_mask(vk::PipelineStageFlags2::BOTTOM_OF_PIPE)])],
                frame.draw_finished_fence,
            )?;
            Ok(())
        }
    }
}

/// A single command buffer with a pool
pub struct SingleCommandBuffer {
    pub device: Arc<MyDevice>,
    pub pool: vk::CommandPool,
    pub cmd: vk::CommandBuffer,
}

impl SingleCommandBuffer {
    pub fn new(device: Arc<MyDevice>) -> anyhow::Result<Self> {
        unsafe {
            let pool = device.device.create_command_pool(
                &vk::CommandPoolCreateInfo::default().queue_family_index(device.main_queue_family),
                None,
            )?;

            let command_buffers = device.device.allocate_command_buffers(
                &vk::CommandBufferAllocateInfo::default()
                    .command_buffer_count(1)
                    .command_pool(pool)
                    .level(vk::CommandBufferLevel::PRIMARY),
            )?;
            assert_eq!(command_buffers.len(), 1);
            let cmd = command_buffers[0];

            Ok(Self { device, pool, cmd })
        }
    }
}

impl Drop for SingleCommandBuffer {
    fn drop(&mut self) {
        unsafe {
            let device = &self.device;
            device.free_command_buffers(self.pool, &[self.cmd]);
            device.destroy_command_pool(self.pool, None);
        }
    }
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
