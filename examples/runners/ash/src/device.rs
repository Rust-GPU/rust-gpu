use crate::Options;
use anyhow::{Context, anyhow};
use ash::{ext, khr, vk};
use std::borrow::Cow;
use std::ffi::{CStr, c_char};
use std::ops::Deref;
use std::sync::Arc;

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
            let entry = ash::Entry::load()?;

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
                            &mut vk::PhysicalDeviceVulkan12Features::default()
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

unsafe extern "system" fn vulkan_debug_callback(
    message_severity: vk::DebugUtilsMessageSeverityFlagsEXT,
    _message_type: vk::DebugUtilsMessageTypeFlagsEXT,
    p_callback_data: *const vk::DebugUtilsMessengerCallbackDataEXT<'_>,
    _user_data: *mut std::os::raw::c_void,
) -> vk::Bool32 {
    unsafe {
        let callback_data = *p_callback_data;
        let message_id_name = callback_data
            .message_id_name_as_c_str()
            .map_or(Cow::Borrowed(""), CStr::to_string_lossy);
        let message = callback_data
            .message_as_c_str()
            .map_or(Cow::Borrowed(""), CStr::to_string_lossy);

        println!("{message_severity:?}: [{message_id_name}] : {message}");
        vk::FALSE
    }
}
