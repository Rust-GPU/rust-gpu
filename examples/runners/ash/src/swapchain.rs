use crate::device::MyDevice;
use anyhow::Context;
use ash::vk;
use raw_window_handle::{HasDisplayHandle, HasWindowHandle};
use std::sync::Arc;

/// A binary semaphore for swapchain operations
struct SwapchainSync {
    acquire_semaphore: vk::Semaphore,
    render_semaphore: vk::Semaphore,
    render_fence: vk::Fence,
}

impl SwapchainSync {
    unsafe fn new(device: &MyDevice) -> anyhow::Result<Self> {
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

    unsafe fn destroy(&self, device: &MyDevice) {
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
    pub fn should_recreate(&mut self) {
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
    /// the [`vk::Image`] to draw to, created from `image`
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
                "looped {RECREATE_ATTEMPTS} times trying to acquire swapchain image and failed repeatedly!"
            );
        }
    }
}
