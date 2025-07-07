use super::backend::{BufferConfig, BufferUsage, ComputeBackend};
use anyhow::{Context, Result, bail};
use ash::vk;
use std::ffi::{CStr, CString};

pub struct AshBackend {
    instance: ash::Instance,
    device: ash::Device,
    queue: vk::Queue,
    command_pool: vk::CommandPool,
    descriptor_pool: vk::DescriptorPool,
    memory_properties: vk::PhysicalDeviceMemoryProperties,
    _entry: ash::Entry,
}

impl AshBackend {
    fn find_memory_type(
        &self,
        type_filter: u32,
        properties: vk::MemoryPropertyFlags,
    ) -> Option<u32> {
        for i in 0..self.memory_properties.memory_type_count {
            if (type_filter & (1 << i)) != 0
                && self.memory_properties.memory_types[i as usize]
                    .property_flags
                    .contains(properties)
            {
                return Some(i);
            }
        }
        None
    }

    fn create_buffer(&self, config: &BufferConfig) -> Result<(vk::Buffer, vk::DeviceMemory)> {
        unsafe {
            let usage = match config.usage {
                BufferUsage::Storage => vk::BufferUsageFlags::STORAGE_BUFFER,
                BufferUsage::StorageReadOnly => vk::BufferUsageFlags::STORAGE_BUFFER,
                BufferUsage::Uniform => vk::BufferUsageFlags::UNIFORM_BUFFER,
            };

            let buffer_create_info = vk::BufferCreateInfo::default()
                .size(config.size)
                .usage(
                    usage | vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST,
                )
                .sharing_mode(vk::SharingMode::EXCLUSIVE);

            let buffer = self
                .device
                .create_buffer(&buffer_create_info, None)
                .context("Failed to create buffer")?;

            let memory_requirements = self.device.get_buffer_memory_requirements(buffer);

            let memory_type_index = self
                .find_memory_type(
                    memory_requirements.memory_type_bits,
                    vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
                )
                .context("Failed to find suitable memory type")?;

            let allocate_info = vk::MemoryAllocateInfo::default()
                .allocation_size(memory_requirements.size)
                .memory_type_index(memory_type_index);

            let memory = self
                .device
                .allocate_memory(&allocate_info, None)
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

                std::ptr::copy_nonoverlapping(data.as_ptr(), mapped_ptr as *mut u8, data.len());

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
            let app_info = vk::ApplicationInfo::default()
                .application_name(CStr::from_bytes_with_nul_unchecked(b"difftest\0"))
                .application_version(vk::make_api_version(0, 1, 0, 0))
                .engine_name(CStr::from_bytes_with_nul_unchecked(b"difftest\0"))
                .engine_version(vk::make_api_version(0, 1, 0, 0))
                .api_version(vk::API_VERSION_1_2);

            let instance_create_info =
                vk::InstanceCreateInfo::default().application_info(&app_info);

            let instance = entry
                .create_instance(&instance_create_info, None)
                .context("Failed to create Vulkan instance")?;

            // Select physical device
            let physical_devices = instance
                .enumerate_physical_devices()
                .context("Failed to enumerate physical devices")?;

            if physical_devices.is_empty() {
                bail!("No Vulkan devices found");
            }

            let physical_device = physical_devices[0];
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
            let priorities = [1.0];
            let queue_create_info = vk::DeviceQueueCreateInfo::default()
                .queue_family_index(queue_family_index)
                .queue_priorities(&priorities);

            let device_features = vk::PhysicalDeviceFeatures::default();

            let queue_create_infos = [queue_create_info];
            let device_create_info = vk::DeviceCreateInfo::default()
                .queue_create_infos(&queue_create_infos)
                .enabled_features(&device_features);

            let device = instance
                .create_device(physical_device, &device_create_info, None)
                .context("Failed to create Vulkan device")?;

            let queue = device.get_device_queue(queue_family_index, 0);

            // Create command pool
            let command_pool_create_info = vk::CommandPoolCreateInfo::default()
                .queue_family_index(queue_family_index)
                .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER);

            let command_pool = device
                .create_command_pool(&command_pool_create_info, None)
                .context("Failed to create command pool")?;

            // Create descriptor pool
            let descriptor_pool_sizes = vec![
                vk::DescriptorPoolSize {
                    ty: vk::DescriptorType::STORAGE_BUFFER,
                    descriptor_count: 16,
                },
                vk::DescriptorPoolSize {
                    ty: vk::DescriptorType::UNIFORM_BUFFER,
                    descriptor_count: 16,
                },
            ];

            let descriptor_pool_create_info = vk::DescriptorPoolCreateInfo::default()
                .pool_sizes(&descriptor_pool_sizes)
                .max_sets(16);

            let descriptor_pool = device
                .create_descriptor_pool(&descriptor_pool_create_info, None)
                .context("Failed to create descriptor pool")?;

            Ok(Self {
                instance,
                device,
                queue,
                command_pool,
                descriptor_pool,
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
            // Create shader module
            let spirv_u32: Vec<u32> = spirv_bytes
                .chunks_exact(4)
                .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
                .collect();

            let shader_module_create_info = vk::ShaderModuleCreateInfo::default().code(&spirv_u32);

            let shader_module = self
                .device
                .create_shader_module(&shader_module_create_info, None)
                .context("Failed to create shader module")?;

            // Create descriptor set layout
            let mut layout_bindings = Vec::new();
            for (i, buffer) in buffers.iter().enumerate() {
                let descriptor_type = match buffer.usage {
                    BufferUsage::Storage | BufferUsage::StorageReadOnly => {
                        vk::DescriptorType::STORAGE_BUFFER
                    }
                    BufferUsage::Uniform => vk::DescriptorType::UNIFORM_BUFFER,
                };
                let binding = vk::DescriptorSetLayoutBinding::default()
                    .binding(i as u32)
                    .descriptor_type(descriptor_type)
                    .descriptor_count(1)
                    .stage_flags(vk::ShaderStageFlags::COMPUTE);
                layout_bindings.push(binding);
            }

            let descriptor_set_layout_create_info =
                vk::DescriptorSetLayoutCreateInfo::default().bindings(&layout_bindings);

            let descriptor_set_layout = self
                .device
                .create_descriptor_set_layout(&descriptor_set_layout_create_info, None)
                .context("Failed to create descriptor set layout")?;

            // Create pipeline layout
            let set_layouts = [descriptor_set_layout];
            let pipeline_layout_create_info =
                vk::PipelineLayoutCreateInfo::default().set_layouts(&set_layouts);

            let pipeline_layout = self
                .device
                .create_pipeline_layout(&pipeline_layout_create_info, None)
                .context("Failed to create pipeline layout")?;

            // Create compute pipeline
            let entry_point_cstring = CString::new(entry_point)?;
            let stage_create_info = vk::PipelineShaderStageCreateInfo::default()
                .stage(vk::ShaderStageFlags::COMPUTE)
                .module(shader_module)
                .name(&entry_point_cstring);

            let compute_pipeline_create_info = vk::ComputePipelineCreateInfo::default()
                .stage(stage_create_info)
                .layout(pipeline_layout);

            let pipelines = self
                .device
                .create_compute_pipelines(
                    vk::PipelineCache::null(),
                    &[compute_pipeline_create_info],
                    None,
                )
                .map_err(|(_, e)| e)
                .context("Failed to create compute pipeline")?;

            let pipeline = pipelines[0];

            // Create buffers
            let mut vk_buffers = Vec::new();
            let mut buffer_memories = Vec::new();

            for buffer_config in &buffers {
                let (buffer, memory) = self.create_buffer(buffer_config)?;
                vk_buffers.push(buffer);
                buffer_memories.push(memory);
            }

            // Allocate descriptor set
            let set_layouts = [descriptor_set_layout];
            let descriptor_set_allocate_info = vk::DescriptorSetAllocateInfo::default()
                .descriptor_pool(self.descriptor_pool)
                .set_layouts(&set_layouts);

            let descriptor_sets = self
                .device
                .allocate_descriptor_sets(&descriptor_set_allocate_info)
                .context("Failed to allocate descriptor sets")?;

            let descriptor_set = descriptor_sets[0];

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
                    let descriptor_type = match config.usage {
                        BufferUsage::Storage | BufferUsage::StorageReadOnly => {
                            vk::DescriptorType::STORAGE_BUFFER
                        }
                        BufferUsage::Uniform => vk::DescriptorType::UNIFORM_BUFFER,
                    };

                    vk::WriteDescriptorSet::default()
                        .dst_set(descriptor_set)
                        .dst_binding(i as u32)
                        .descriptor_type(descriptor_type)
                        .buffer_info(std::slice::from_ref(buffer_info))
                })
                .collect();

            self.device.update_descriptor_sets(&descriptor_writes, &[]);

            // Allocate command buffer
            let command_buffer_allocate_info = vk::CommandBufferAllocateInfo::default()
                .command_pool(self.command_pool)
                .level(vk::CommandBufferLevel::PRIMARY)
                .command_buffer_count(1);

            let command_buffers = self
                .device
                .allocate_command_buffers(&command_buffer_allocate_info)
                .context("Failed to allocate command buffer")?;

            let command_buffer = command_buffers[0];

            // Begin command buffer
            let begin_info = vk::CommandBufferBeginInfo::default()
                .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);

            self.device
                .begin_command_buffer(command_buffer, &begin_info)
                .context("Failed to begin command buffer")?;

            // Bind pipeline and descriptor set
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

            // Dispatch compute
            self.device
                .cmd_dispatch(command_buffer, dispatch[0], dispatch[1], dispatch[2]);

            // End command buffer
            self.device
                .end_command_buffer(command_buffer)
                .context("Failed to end command buffer")?;

            // Submit command buffer
            let command_buffers = [command_buffer];
            let submit_info = vk::SubmitInfo::default().command_buffers(&command_buffers);

            self.device
                .queue_submit(self.queue, &[submit_info], vk::Fence::null())
                .context("Failed to submit queue")?;

            // Wait for completion
            self.device
                .queue_wait_idle(self.queue)
                .context("Failed to wait for queue")?;

            // Read buffer results
            let mut results = Vec::new();
            for (_i, (memory, config)) in buffer_memories.iter().zip(&buffers).enumerate() {
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
            self.device.destroy_pipeline(pipeline, None);
            self.device.destroy_pipeline_layout(pipeline_layout, None);
            self.device
                .destroy_descriptor_set_layout(descriptor_set_layout, None);
            self.device.destroy_shader_module(shader_module, None);

            Ok(results)
        }
    }
}

impl Drop for AshBackend {
    fn drop(&mut self) {
        unsafe {
            self.device
                .destroy_descriptor_pool(self.descriptor_pool, None);
            self.device.destroy_command_pool(self.command_pool, None);
            self.device.destroy_device(None);
            self.instance.destroy_instance(None);
        }
    }
}
