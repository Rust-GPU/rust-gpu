use super::backend::{BufferConfig, BufferUsage, ComputeBackend};
use anyhow::{Context, Result};
use std::sync::Arc;
use vulkano::{
    VulkanLibrary,
    buffer::{Buffer, BufferCreateInfo, BufferUsage as VkBufferUsage},
    command_buffer::{
        AutoCommandBufferBuilder, CommandBufferUsage, allocator::StandardCommandBufferAllocator,
    },
    descriptor_set::{
        DescriptorSet, WriteDescriptorSet, allocator::StandardDescriptorSetAllocator,
    },
    device::{
        Device, DeviceCreateInfo, DeviceFeatures, Queue, QueueCreateInfo, QueueFlags,
        physical::PhysicalDeviceType,
    },
    instance::{Instance, InstanceCreateFlags, InstanceCreateInfo},
    memory::allocator::{AllocationCreateInfo, MemoryTypeFilter, StandardMemoryAllocator},
    pipeline::{
        ComputePipeline, Pipeline, PipelineLayout, PipelineShaderStageCreateInfo,
        compute::ComputePipelineCreateInfo, layout::PipelineDescriptorSetLayoutCreateInfo,
    },
    shader::{ShaderModule, ShaderModuleCreateInfo},
    sync::{self, GpuFuture},
};

pub struct VulkanoBackend {
    device: Arc<Device>,
    queue: Arc<Queue>,
    memory_allocator: Arc<StandardMemoryAllocator>,
    command_buffer_allocator: Arc<StandardCommandBufferAllocator>,
    descriptor_set_allocator: Arc<StandardDescriptorSetAllocator>,
}

impl ComputeBackend for VulkanoBackend {
    fn init() -> Result<Self> {
        let library = VulkanLibrary::new()?;

        // Use the library's supported API version
        let api_version = library.api_version();
        eprintln!(
            "Vulkan library API version: {}.{}.{}",
            api_version.major, api_version.minor, api_version.patch
        );

        let instance = Instance::new(library, InstanceCreateInfo {
            flags: InstanceCreateFlags::ENUMERATE_PORTABILITY,
            ..Default::default()
        })?;

        // Pick a physical device
        let physical_device = instance
            .enumerate_physical_devices()?
            .min_by_key(|p| match p.properties().device_type {
                PhysicalDeviceType::DiscreteGpu => 0,
                PhysicalDeviceType::IntegratedGpu => 1,
                PhysicalDeviceType::VirtualGpu => 2,
                PhysicalDeviceType::Cpu => 3,
                PhysicalDeviceType::Other => 4,
                _ => 5,
            })
            .context("No suitable physical device found")?;

        // Find a compute queue
        let queue_family_index = physical_device
            .queue_family_properties()
            .iter()
            .enumerate()
            .position(|(_, q)| q.queue_flags.intersects(QueueFlags::COMPUTE))
            .context("No compute queue family found")? as u32;

        // Check if vulkan_memory_model is supported
        let supported_features = physical_device.supported_features();
        let mut enabled_features = DeviceFeatures::empty();
        if supported_features.vulkan_memory_model {
            enabled_features.vulkan_memory_model = true;
        }

        let (device, mut queues) = Device::new(physical_device, DeviceCreateInfo {
            queue_create_infos: vec![QueueCreateInfo {
                queue_family_index,
                ..Default::default()
            }],
            enabled_features,
            ..Default::default()
        })?;

        let queue = queues.next().context("No queue returned")?;

        let memory_allocator = Arc::new(StandardMemoryAllocator::new_default(device.clone()));
        let command_buffer_allocator = Arc::new(StandardCommandBufferAllocator::new(
            device.clone(),
            Default::default(),
        ));
        let descriptor_set_allocator = Arc::new(StandardDescriptorSetAllocator::new(
            device.clone(),
            Default::default(),
        ));

        Ok(Self {
            device,
            queue,
            memory_allocator,
            command_buffer_allocator,
            descriptor_set_allocator,
        })
    }

    fn run_compute(
        &self,
        spirv_bytes: &[u8],
        entry_point: &str,
        dispatch: [u32; 3],
        buffers: Vec<BufferConfig>,
    ) -> Result<Vec<Vec<u8>>> {
        // Convert bytes to u32 words
        if spirv_bytes.len() % 4 != 0 {
            anyhow::bail!("SPIR-V binary length is not a multiple of 4");
        }
        let spirv_words: Vec<u32> = spirv_bytes
            .chunks_exact(4)
            .map(|chunk| u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
            .collect();

        // Create shader module
        let shader = unsafe {
            ShaderModule::new(
                self.device.clone(),
                ShaderModuleCreateInfo::new(&spirv_words),
            )?
        };

        // Get the entry point
        let entry_point = shader
            .entry_point(entry_point)
            .context("Entry point not found in shader module")?;

        // Create pipeline
        let stage = PipelineShaderStageCreateInfo::new(entry_point);
        let layout = PipelineLayout::new(
            self.device.clone(),
            PipelineDescriptorSetLayoutCreateInfo::from_stages([&stage])
                .into_pipeline_layout_create_info(self.device.clone())?,
        )?;

        let compute_pipeline = ComputePipeline::new(
            self.device.clone(),
            None,
            ComputePipelineCreateInfo::stage_layout(stage, layout),
        )?;

        // Create buffers
        let mut gpu_buffers = Vec::new();
        for buffer_config in buffers.iter() {
            let usage = match buffer_config.usage {
                BufferUsage::Storage => VkBufferUsage::STORAGE_BUFFER,
                BufferUsage::StorageReadOnly => VkBufferUsage::STORAGE_BUFFER,
                BufferUsage::Uniform => VkBufferUsage::UNIFORM_BUFFER,
            };

            let buffer = if let Some(initial_data) = &buffer_config.initial_data {
                Buffer::from_iter(
                    self.memory_allocator.clone(),
                    BufferCreateInfo {
                        usage: usage | VkBufferUsage::TRANSFER_SRC | VkBufferUsage::TRANSFER_DST,
                        ..Default::default()
                    },
                    AllocationCreateInfo {
                        memory_type_filter: MemoryTypeFilter::PREFER_DEVICE
                            | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                        ..Default::default()
                    },
                    initial_data.iter().cloned(),
                )?
            } else {
                // Zero initialize
                let zeros = vec![0u8; buffer_config.size as usize];
                Buffer::from_iter(
                    self.memory_allocator.clone(),
                    BufferCreateInfo {
                        usage: usage | VkBufferUsage::TRANSFER_SRC | VkBufferUsage::TRANSFER_DST,
                        ..Default::default()
                    },
                    AllocationCreateInfo {
                        memory_type_filter: MemoryTypeFilter::PREFER_DEVICE
                            | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                        ..Default::default()
                    },
                    zeros,
                )?
            };

            gpu_buffers.push(buffer);
        }

        // Create descriptor set
        let layout = compute_pipeline.layout().set_layouts()[0].clone();
        let mut writes = Vec::new();
        for (i, buffer) in gpu_buffers.iter().enumerate() {
            writes.push(WriteDescriptorSet::buffer(i as u32, buffer.clone()));
        }

        let descriptor_set =
            DescriptorSet::new(self.descriptor_set_allocator.clone(), layout, writes, [])?;

        // Create command buffer
        let mut builder = AutoCommandBufferBuilder::primary(
            self.command_buffer_allocator.clone(),
            self.queue.queue_family_index(),
            CommandBufferUsage::OneTimeSubmit,
        )?;

        unsafe {
            builder
                .bind_pipeline_compute(compute_pipeline.clone())?
                .bind_descriptor_sets(
                    vulkano::pipeline::PipelineBindPoint::Compute,
                    compute_pipeline.layout().clone(),
                    0,
                    descriptor_set,
                )?
                .dispatch(dispatch)?;
        }

        let command_buffer = builder.build()?;

        // Execute
        let future = sync::now(self.device.clone())
            .then_execute(self.queue.clone(), command_buffer)?
            .then_signal_fence_and_flush()?;

        future.wait(None)?;

        // Read back results
        let mut results = Vec::new();
        for (i, buffer_config) in buffers.iter().enumerate() {
            if matches!(
                buffer_config.usage,
                BufferUsage::Storage | BufferUsage::StorageReadOnly
            ) {
                let content_guard = gpu_buffers[i].read()?;
                results.push(content_guard.to_vec());
            } else {
                results.push(Vec::new());
            }
        }

        Ok(results)
    }
}
