use glam::UVec3;

#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDeviceSize.html>"]
pub type DeviceSize = u64;
#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDeviceAddress.html>"]
pub type DeviceAddress = u64;

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDrawIndirectCommand.html>"]
pub struct DrawIndirectCommand {
    pub vertex_count: u32,
    pub instance_count: u32,
    pub first_vertex: u32,
    pub first_instance: u32,
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDrawIndexedIndirectCommand.html>"]
pub struct DrawIndexedIndirectCommand {
    pub index_count: u32,
    pub instance_count: u32,
    pub first_index: u32,
    pub vertex_offset: i32,
    pub first_instance: u32,
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDispatchIndirectCommand.html>"]
pub struct DispatchIndirectCommand {
    pub x: u32,
    pub y: u32,
    pub z: u32,
}

impl From<UVec3> for DispatchIndirectCommand {
    fn from(v: UVec3) -> Self {
        Self {
            x: v.x,
            y: v.y,
            z: v.z,
        }
    }
}

impl From<DispatchIndirectCommand> for UVec3 {
    fn from(v: DispatchIndirectCommand) -> Self {
        Self {
            x: v.x,
            y: v.y,
            z: v.z,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDrawMeshTasksIndirectCommandEXT.html>"]
pub struct DrawMeshTasksIndirectCommandEXT {
    pub group_count_x: u32,
    pub group_count_y: u32,
    pub group_count_z: u32,
}

impl From<UVec3> for DrawMeshTasksIndirectCommandEXT {
    fn from(v: UVec3) -> Self {
        Self {
            group_count_x: v.x,
            group_count_y: v.y,
            group_count_z: v.z,
        }
    }
}

impl From<DrawMeshTasksIndirectCommandEXT> for UVec3 {
    fn from(v: DrawMeshTasksIndirectCommandEXT) -> Self {
        Self {
            x: v.group_count_x,
            y: v.group_count_y,
            z: v.group_count_z,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkTraceRaysIndirectCommandKHR.html>"]
pub struct TraceRaysIndirectCommandKHR {
    pub width: u32,
    pub height: u32,
    pub depth: u32,
}

impl From<UVec3> for TraceRaysIndirectCommandKHR {
    fn from(v: UVec3) -> Self {
        Self {
            width: v.x,
            height: v.y,
            depth: v.z,
        }
    }
}

impl From<TraceRaysIndirectCommandKHR> for UVec3 {
    fn from(v: TraceRaysIndirectCommandKHR) -> Self {
        Self {
            x: v.width,
            y: v.height,
            z: v.depth,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
#[doc = "<https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkTraceRaysIndirectCommand2KHR.html>"]
#[must_use]
pub struct TraceRaysIndirectCommand2KHR {
    pub raygen_shader_record_address: DeviceAddress,
    pub raygen_shader_record_size: DeviceSize,
    pub miss_shader_binding_table_address: DeviceAddress,
    pub miss_shader_binding_table_size: DeviceSize,
    pub miss_shader_binding_table_stride: DeviceSize,
    pub hit_shader_binding_table_address: DeviceAddress,
    pub hit_shader_binding_table_size: DeviceSize,
    pub hit_shader_binding_table_stride: DeviceSize,
    pub callable_shader_binding_table_address: DeviceAddress,
    pub callable_shader_binding_table_size: DeviceSize,
    pub callable_shader_binding_table_stride: DeviceSize,
    pub width: u32,
    pub height: u32,
    pub depth: u32,
}
