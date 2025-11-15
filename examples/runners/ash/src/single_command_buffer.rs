use crate::device::MyDevice;
use ash::vk;
use std::sync::Arc;

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
