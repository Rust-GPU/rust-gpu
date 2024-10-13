//! Indirect command structs from vulkan

use glam::UVec3;

/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDeviceSize.html>
pub type DeviceSize = u64;

/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDeviceAddress.html>
pub type DeviceAddress = u64;

/// Structure specifying an indirect drawing command
///
/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDrawIndirectCommand.html>
#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct DrawIndirectCommand {
    /// vertexCount is the number of vertices to draw.
    pub vertex_count: u32,
    /// instanceCount is the number of instances to draw.
    pub instance_count: u32,
    /// firstVertex is the index of the first vertex to draw.
    pub first_vertex: u32,
    /// firstInstance is the instance ID of the first instance to draw.
    pub first_instance: u32,
}

/// Structure specifying an indexed indirect drawing command
///
/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDrawIndexedIndirectCommand.html>
#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct DrawIndexedIndirectCommand {
    /// indexCount is the number of vertices to draw.
    pub index_count: u32,
    /// instanceCount is the number of instances to draw.
    pub instance_count: u32,
    /// firstIndex is the base index within the index buffer.
    pub first_index: u32,
    /// vertexOffset is the value added to the vertex index before indexing into the vertex buffer.
    pub vertex_offset: i32,
    /// firstInstance is the instance ID of the first instance to draw.
    pub first_instance: u32,
}

/// Structure specifying an indirect dispatching command
///
/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDispatchIndirectCommand.html>
#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct DispatchIndirectCommand {
    /// x is the number of local workgroups to dispatch in the X dimension.
    pub x: u32,
    /// y is the number of local workgroups to dispatch in the Y dimension.
    pub y: u32,
    /// z is the number of local workgroups to dispatch in the Z dimension.
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

/// Structure specifying a mesh tasks draw indirect command
///
/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkDrawMeshTasksIndirectCommandEXT.html>
#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct DrawMeshTasksIndirectCommandEXT {
    /// groupCountX is the number of local workgroups to dispatch in the X dimension.
    pub group_count_x: u32,
    /// groupCountY is the number of local workgroups to dispatch in the Y dimension.
    pub group_count_y: u32,
    /// groupCountZ is the number of local workgroups to dispatch in the Z dimension.
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

/// Structure specifying the parameters of an indirect ray tracing command
///
/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkTraceRaysIndirectCommandKHR.html>
#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct TraceRaysIndirectCommandKHR {
    /// width is the width of the ray trace query dimensions.
    pub width: u32,
    /// height is height of the ray trace query dimensions.
    pub height: u32,
    /// depth is depth of the ray trace query dimensions.
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

/// Structure specifying the parameters of an indirect trace ray command with indirect shader binding tables
///
/// <https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkTraceRaysIndirectCommand2KHR.html>
#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
#[must_use]
pub struct TraceRaysIndirectCommand2KHR {
    /// raygenShaderRecordAddress is a `VkDeviceAddress` of the ray generation shader binding table record used by this command.
    pub raygen_shader_record_address: DeviceAddress,
    /// raygenShaderRecordSize is a `VkDeviceSize` number of bytes corresponding to the ray generation shader binding table record at base address raygenShaderRecordAddress.
    pub raygen_shader_record_size: DeviceSize,
    /// missShaderBindingTableAddress is a `VkDeviceAddress` of the first record in the miss shader binding table used by this command.
    pub miss_shader_binding_table_address: DeviceAddress,
    /// missShaderBindingTableSize is a `VkDeviceSize` number of bytes corresponding to the total size of the miss shader binding table at missShaderBindingTableAddress that may be accessed by this command.
    pub miss_shader_binding_table_size: DeviceSize,
    /// missShaderBindingTableStride is a `VkDeviceSize` number of bytes between records of the miss shader binding table.
    pub miss_shader_binding_table_stride: DeviceSize,
    /// hitShaderBindingTableAddress is a `VkDeviceAddress` of the first record in the hit shader binding table used by this command.
    pub hit_shader_binding_table_address: DeviceAddress,
    /// hitShaderBindingTableSize is a `VkDeviceSize` number of bytes corresponding to the total size of the hit shader binding table at hitShaderBindingTableAddress that may be accessed by this command.
    pub hit_shader_binding_table_size: DeviceSize,
    /// hitShaderBindingTableStride is a `VkDeviceSize` number of bytes between records of the hit shader binding table.
    pub hit_shader_binding_table_stride: DeviceSize,
    /// callableShaderBindingTableAddress is a `VkDeviceAddress` of the first record in the callable shader binding table used by this command.
    pub callable_shader_binding_table_address: DeviceAddress,
    /// callableShaderBindingTableSize is a `VkDeviceSize` number of bytes corresponding to the total size of the callable shader binding table at callableShaderBindingTableAddress that may be accessed by this command.
    pub callable_shader_binding_table_size: DeviceSize,
    /// callableShaderBindingTableStride is a `VkDeviceSize` number of bytes between records of the callable shader binding table.
    pub callable_shader_binding_table_stride: DeviceSize,
    /// width is the width of the ray trace query dimensions.
    pub width: u32,
    /// height is height of the ray trace query dimensions.
    pub height: u32,
    /// depth is depth of the ray trace query dimensions.
    pub depth: u32,
}
