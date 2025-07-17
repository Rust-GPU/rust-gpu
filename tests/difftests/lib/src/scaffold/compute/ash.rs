use super::backend::{BufferConfig, BufferUsage, ComputeBackend};
use anyhow::{Context, Result};
use ash::vk;
use ash::vk::DescriptorType;
use std::ffi::CString;

pub struct AshBackend {
    instance: ash::Instance,
    device: ash::Device,
    queue: vk::Queue,
    command_pool: vk::CommandPool,
    memory_properties: vk::PhysicalDeviceMemoryProperties,
    _entry: ash::Entry,
}

impl AshBackend {
    fn find_memory_type(
        &self,
        type_filter: u32,
        properties: vk::MemoryPropertyFlags,
    ) -> Option<u32> {
        (0..self.memory_properties.memory_type_count).find(|&i| {
            (type_filter & (1 << i)) != 0
                && self.memory_properties.memory_types[i as usize]
                    .property_flags
                    .contains(properties)
        })
    }

    fn create_buffer(&self, config: &BufferConfig) -> Result<(vk::Buffer, vk::DeviceMemory)> {
        unsafe {
            let usage = match config.usage {
                BufferUsage::Storage | BufferUsage::StorageReadOnly => {
                    vk::BufferUsageFlags::STORAGE_BUFFER
                }
                BufferUsage::Uniform => vk::BufferUsageFlags::UNIFORM_BUFFER,
            };
            let buffer = self
                .device
                .create_buffer(
                    &vk::BufferCreateInfo::default()
                        .size(config.size)
                        .usage(
                            usage
                                | vk::BufferUsageFlags::TRANSFER_SRC
                                | vk::BufferUsageFlags::TRANSFER_DST,
                        )
                        .sharing_mode(vk::SharingMode::EXCLUSIVE),
                    None,
                )
                .context("Failed to create buffer")?;

            let memory_requirements = self.device.get_buffer_memory_requirements(buffer);
            let memory_type_index = self
                .find_memory_type(
                    memory_requirements.memory_type_bits,
                    vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
                )
                .context("Failed to find suitable memory type")?;
            let memory = self
                .device
                .allocate_memory(
                    &vk::MemoryAllocateInfo::default()
                        .allocation_size(memory_requirements.size)
                        .memory_type_index(memory_type_index),
                    None,
                )
                .context("Failed to allocate memory")?;
            self.device
                .bind_buffer_memory(buffer, memory, 0)
                .context("Failed to bind buffer memory")?;

            // Initialize buffer if initial data provided
            if let Some(data) = &config.initial_data {
                let mapped_ptr = self
                    .device
                    .map_memory(memory, 0, config.size, vk::MemoryMapFlags::empty())
                    .context("Failed to map memory")?;

                std::ptr::copy_nonoverlapping(data.as_ptr(), mapped_ptr.cast::<u8>(), data.len());

                self.device.unmap_memory(memory);
            }

            Ok((buffer, memory))
        }
    }
}

impl ComputeBackend for AshBackend {
    fn init() -> Result<Self> {
        unsafe {
            let entry = ash::Entry::load().context("Failed to load Vulkan entry")?;

            // Create instance
            let instance = entry
                .create_instance(
                    &vk::InstanceCreateInfo::default().application_info(
                        &vk::ApplicationInfo::default()
                            .application_name(c"difftest")
                            .application_version(vk::make_api_version(0, 1, 0, 0))
                            .engine_name(c"difftest")
                            .engine_version(vk::make_api_version(0, 1, 0, 0))
                            .api_version(vk::API_VERSION_1_2),
                    ),
                    None,
                )
                .context("Failed to create Vulkan instance")?;

            // Select physical device
            let physical_devices = instance
                .enumerate_physical_devices()
                .context("Failed to enumerate physical devices")?;
            let physical_device = *physical_devices
                .first()
                .context("No Vulkan devices found")?;
            let memory_properties = instance.get_physical_device_memory_properties(physical_device);

            // Find compute queue family
            let queue_family_properties =
                instance.get_physical_device_queue_family_properties(physical_device);
            let queue_family_index = queue_family_properties
                .iter()
                .enumerate()
                .find(|(_, props)| props.queue_flags.contains(vk::QueueFlags::COMPUTE))
                .map(|(index, _)| index as u32)
                .context("No compute queue family found")?;

            // Create device
            let device = instance
                .create_device(
                    physical_device,
                    &vk::DeviceCreateInfo::default()
                        .queue_create_infos(&[vk::DeviceQueueCreateInfo::default()
                            .queue_family_index(queue_family_index)
                            .queue_priorities(&[1.0])])
                        .enabled_features(&vk::PhysicalDeviceFeatures::default()),
                    None,
                )
                .context("Failed to create Vulkan device")?;
            let queue = device.get_device_queue(queue_family_index, 0);

            // Create command pool
            let command_pool = device
                .create_command_pool(
                    &vk::CommandPoolCreateInfo::default().queue_family_index(queue_family_index),
                    None,
                )
                .context("Failed to create command pool")?;

            Ok(Self {
                instance,
                device,
                queue,
                command_pool,
                memory_properties,
                _entry: entry,
            })
        }
    }

    fn run_compute(
        &self,
        spirv_bytes: &[u8],
        entry_point: &str,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
    ) -> Result<Vec<Vec<u8>>> {
        unsafe {
            // Create descriptor set layout
            let bindings = buffers
                .iter()
                .enumerate()
                .map(|(i, buffer)| {
                    vk::DescriptorSetLayoutBinding::default()
                        .binding(i as u32)
                        .descriptor_type(buffer_usage_to_descriptor_type(buffer.usage))
                        .descriptor_count(1)
                        .stage_flags(vk::ShaderStageFlags::COMPUTE)
                })
                .collect::<Vec<_>>();
            let descriptor_set_layout = self
                .device
                .create_descriptor_set_layout(
                    &vk::DescriptorSetLayoutCreateInfo::default().bindings(&bindings),
                    None,
                )
                .context("Failed to create descriptor set layout")?;

            // Create pipeline layout
            let pipeline_layout = self
                .device
                .create_pipeline_layout(
                    &vk::PipelineLayoutCreateInfo::default().set_layouts(&[descriptor_set_layout]),
                    None,
                )
                .context("Failed to create pipeline layout")?;

            // Create compute pipeline
            let pipeline = {
                let spirv_u32: Vec<u32> = spirv_bytes
                    .chunks_exact(4)
                    .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
                    .collect();
                let shader_module = self
                    .device
                    .create_shader_module(
                        &vk::ShaderModuleCreateInfo::default().code(&spirv_u32),
                        None,
                    )
                    .context("Failed to create shader module")?;
                let pipeline = self
                    .device
                    .create_compute_pipelines(
                        vk::PipelineCache::null(),
                        &[vk::ComputePipelineCreateInfo::default()
                            .stage(
                                vk::PipelineShaderStageCreateInfo::default()
                                    .stage(vk::ShaderStageFlags::COMPUTE)
                                    .module(shader_module)
                                    .name(&CString::new(entry_point)?),
                            )
                            .layout(pipeline_layout)],
                        None,
                    )
                    .map_err(|(_, e)| e)
                    .context("Failed to create compute pipeline")?[0];
                self.device.destroy_shader_module(shader_module, None);
                pipeline
            };

            // Create buffers
            let mut vk_buffers = Vec::new();
            let mut buffer_memories = Vec::new();
            for buffer_config in &buffers {
                let (buffer, memory) = self.create_buffer(buffer_config)?;
                vk_buffers.push(buffer);
                buffer_memories.push(memory);
            }

            // Allocate descriptor set
            let count_descriptor_types = |desc_type: DescriptorType| vk::DescriptorPoolSize {
                ty: desc_type,
                descriptor_count: buffers
                    .iter()
                    .filter(|buffer| buffer_usage_to_descriptor_type(buffer.usage) == desc_type)
                    .count() as u32,
            };
            let pool_sizes = [
                count_descriptor_types(DescriptorType::STORAGE_BUFFER),
                count_descriptor_types(DescriptorType::UNIFORM_BUFFER),
            ]
            .into_iter()
            .filter(|a| a.descriptor_count != 0)
            .collect::<Vec<_>>();
            let descriptor_pool = self
                .device
                .create_descriptor_pool(
                    &vk::DescriptorPoolCreateInfo::default()
                        .pool_sizes(&pool_sizes)
                        .max_sets(1),
                    None,
                )
                .context("Failed to create descriptor pool")?;
            let descriptor_set = self
                .device
                .allocate_descriptor_sets(
                    &vk::DescriptorSetAllocateInfo::default()
                        .descriptor_pool(descriptor_pool)
                        .set_layouts(&[descriptor_set_layout]),
                )
                .context("Failed to allocate descriptor sets")?[0];

            // Update descriptor sets
            let buffer_infos: Vec<vk::DescriptorBufferInfo> = vk_buffers
                .iter()
                .zip(&buffers)
                .map(|(buffer, config)| {
                    vk::DescriptorBufferInfo::default()
                        .buffer(*buffer)
                        .offset(0)
                        .range(config.size)
                })
                .collect();
            let descriptor_writes: Vec<vk::WriteDescriptorSet<'_>> = buffer_infos
                .iter()
                .zip(&buffers)
                .enumerate()
                .map(|(i, (buffer_info, config))| {
                    vk::WriteDescriptorSet::default()
                        .dst_set(descriptor_set)
                        .dst_binding(i as u32)
                        .descriptor_type(buffer_usage_to_descriptor_type(config.usage))
                        .buffer_info(std::slice::from_ref(buffer_info))
                })
                .collect();
            self.device.update_descriptor_sets(&descriptor_writes, &[]);

            // Allocate command buffer
            let command_buffer = self
                .device
                .allocate_command_buffers(
                    &vk::CommandBufferAllocateInfo::default()
                        .command_pool(self.command_pool)
                        .level(vk::CommandBufferLevel::PRIMARY)
                        .command_buffer_count(1),
                )
                .context("Failed to allocate command buffer")?[0];

            // Record command buffer
            self.device
                .begin_command_buffer(
                    command_buffer,
                    &vk::CommandBufferBeginInfo::default()
                        .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT),
                )
                .context("Failed to begin command buffer")?;
            self.device
                .cmd_bind_pipeline(command_buffer, vk::PipelineBindPoint::COMPUTE, pipeline);
            self.device.cmd_bind_descriptor_sets(
                command_buffer,
                vk::PipelineBindPoint::COMPUTE,
                pipeline_layout,
                0,
                &[descriptor_set],
                &[],
            );
            self.device
                .cmd_dispatch(command_buffer, dispatch[0], dispatch[1], dispatch[2]);
            self.device
                .end_command_buffer(command_buffer)
                .context("Failed to end command buffer")?;

            // Submit command buffer
            self.device
                .queue_submit(
                    self.queue,
                    &[vk::SubmitInfo::default().command_buffers(&[command_buffer])],
                    vk::Fence::null(),
                )
                .context("Failed to submit queue")?;

            // Wait for completion
            self.device
                .queue_wait_idle(self.queue)
                .context("Failed to wait for queue")?;

            // Read buffer results
            let mut results = Vec::new();
            for (memory, config) in buffer_memories.iter().zip(&buffers) {
                let mut data = vec![0u8; config.size as usize];

                let mapped_ptr = self
                    .device
                    .map_memory(*memory, 0, config.size, vk::MemoryMapFlags::empty())
                    .context("Failed to map memory for reading")?;

                std::ptr::copy_nonoverlapping(
                    mapped_ptr as *const u8,
                    data.as_mut_ptr(),
                    config.size as usize,
                );

                self.device.unmap_memory(*memory);

                results.push(data);
            }

            // Clean up
            self.device
                .free_command_buffers(self.command_pool, &[command_buffer]);
            for (buffer, memory) in vk_buffers.iter().zip(&buffer_memories) {
                self.device.destroy_buffer(*buffer, None);
                self.device.free_memory(*memory, None);
            }
            self.device.destroy_descriptor_pool(descriptor_pool, None);
            self.device.destroy_pipeline(pipeline, None);
            self.device.destroy_pipeline_layout(pipeline_layout, None);
            self.device
                .destroy_descriptor_set_layout(descriptor_set_layout, None);

            Ok(results)
        }
    }
}

impl Drop for AshBackend {
    fn drop(&mut self) {
        unsafe {
            self.device.destroy_command_pool(self.command_pool, None);
            self.device.destroy_device(None);
            self.instance.destroy_instance(None);
        }
    }
}

fn buffer_usage_to_descriptor_type(usage: BufferUsage) -> DescriptorType {
    match usage {
        BufferUsage::Storage | BufferUsage::StorageReadOnly => DescriptorType::STORAGE_BUFFER,
        BufferUsage::Uniform => DescriptorType::UNIFORM_BUFFER,
    }
}
