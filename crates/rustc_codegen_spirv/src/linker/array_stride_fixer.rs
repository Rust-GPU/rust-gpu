//! Fix ArrayStride decorations for newer SPIR-V versions.
//!
//! Newer SPIR-V versions forbid explicit layouts (ArrayStride decorations) in certain
//! storage classes (Function, Private, Workgroup), but allow them in others
//! (StorageBuffer, Uniform). This module removes ArrayStride decorations from
//! array types that are used in contexts where they're forbidden.

use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Capability, Decoration, Op, StorageClass, Word};
use rustc_data_structures::fx::FxHashSet;

/// Check if a storage class allows explicit layout decorations based on SPIR-V version and capabilities.
/// This matches the logic from SPIRV-Tools validate_decorations.cpp AllowsLayout function.
fn allows_layout(
    storage_class: StorageClass,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) -> bool {
    match storage_class {
        // Always explicitly laid out
        StorageClass::StorageBuffer
        | StorageClass::Uniform
        | StorageClass::PhysicalStorageBuffer
        | StorageClass::PushConstant => true,

        // Never allows layout
        StorageClass::UniformConstant => false,

        // Requires explicit capability
        StorageClass::Workgroup => has_workgroup_layout_capability,

        // Only forbidden in SPIR-V 1.4+
        StorageClass::Function | StorageClass::Private => spirv_version < (1, 4),

        // Block is used generally and mesh shaders use Offset
        StorageClass::Input | StorageClass::Output => true,

        // TODO: Some storage classes in ray tracing use explicit layout
        // decorations, but it is not well documented which. For now treat other
        // storage classes as allowed to be laid out.
        _ => true,
    }
}

/// Remove ArrayStride decorations from array types used in storage classes where
/// newer SPIR-V versions forbid explicit layouts.
pub fn fix_array_stride_decorations(module: &mut Module) {
    // Get SPIR-V version from module header
    let spirv_version = module
        .header
        .as_ref()
        .map(|h| h.version())
        .unwrap_or((1, 0)); // Default to 1.0 if no header

    // Check for WorkgroupMemoryExplicitLayoutKHR capability
    let has_workgroup_layout_capability = module.capabilities.iter().any(|inst| {
        inst.class.opcode == Op::Capability
            && inst.operands.first()
                == Some(&Operand::Capability(
                    Capability::WorkgroupMemoryExplicitLayoutKHR,
                ))
    });

    // Find all array types that have ArrayStride decorations
    let mut array_types_with_stride = FxHashSet::default();
    for inst in &module.annotations {
        if inst.class.opcode == Op::Decorate
            && inst.operands.len() >= 2
            && inst.operands[1] == Operand::Decoration(Decoration::ArrayStride)
        {
            let target_id = inst.operands[0].unwrap_id_ref();
            array_types_with_stride.insert(target_id);
        }
    }

    // Check each array type with ArrayStride to see if it's used in forbidden contexts
    let mut array_types_to_fix = FxHashSet::default();
    for &array_type_id in &array_types_with_stride {
        if is_array_type_used_in_forbidden_storage_class(
            array_type_id,
            module,
            spirv_version,
            has_workgroup_layout_capability,
        ) {
            array_types_to_fix.insert(array_type_id);
        }
    }

    // Remove ArrayStride decorations for the problematic types
    module.annotations.retain(|inst| {
        if inst.class.opcode == Op::Decorate
            && inst.operands.len() >= 2
            && inst.operands[1] == Operand::Decoration(Decoration::ArrayStride)
        {
            let target_id = inst.operands[0].unwrap_id_ref();
            !array_types_to_fix.contains(&target_id)
        } else {
            true
        }
    });
}

/// Check if an array type is used in any variable with a forbidden storage class
fn is_array_type_used_in_forbidden_storage_class(
    array_type_id: Word,
    module: &Module,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) -> bool {
    // Check global variables
    for inst in &module.types_global_values {
        if inst.class.opcode == Op::Variable && inst.operands.len() >= 1 {
            let storage_class = inst.operands[0].unwrap_storage_class();

            // Check if this storage class forbids explicit layouts
            if !allows_layout(
                storage_class,
                spirv_version,
                has_workgroup_layout_capability,
            ) {
                // Check if this variable's type hierarchy contains the array type
                if let Some(var_type_id) = inst.result_type {
                    if type_hierarchy_contains_array_type(var_type_id, array_type_id, module) {
                        return true;
                    }
                }
            }
        }
    }

    // Check function-local variables
    for function in &module.functions {
        for block in &function.blocks {
            for inst in &block.instructions {
                if inst.class.opcode == Op::Variable && inst.operands.len() >= 1 {
                    let storage_class = inst.operands[0].unwrap_storage_class();

                    // Check if this storage class forbids explicit layouts
                    if !allows_layout(
                        storage_class,
                        spirv_version,
                        has_workgroup_layout_capability,
                    ) {
                        // Check if this variable's type hierarchy contains the array type
                        if let Some(var_type_id) = inst.result_type {
                            if type_hierarchy_contains_array_type(
                                var_type_id,
                                array_type_id,
                                module,
                            ) {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }

    false
}

/// Check if a type hierarchy contains a specific array type
fn type_hierarchy_contains_array_type(
    type_id: Word,
    target_array_type_id: Word,
    module: &Module,
) -> bool {
    if type_id == target_array_type_id {
        return true;
    }

    // Find the type definition
    if let Some(type_inst) = module
        .types_global_values
        .iter()
        .find(|inst| inst.result_id == Some(type_id))
    {
        match type_inst.class.opcode {
            Op::TypeArray | Op::TypeRuntimeArray => {
                // Check element type recursively
                if !type_inst.operands.is_empty() {
                    let element_type = type_inst.operands[0].unwrap_id_ref();
                    return type_hierarchy_contains_array_type(
                        element_type,
                        target_array_type_id,
                        module,
                    );
                }
            }
            Op::TypeStruct => {
                // Check all field types
                for operand in &type_inst.operands {
                    if let Ok(field_type) = operand.id_ref_any().ok_or(()) {
                        if type_hierarchy_contains_array_type(
                            field_type,
                            target_array_type_id,
                            module,
                        ) {
                            return true;
                        }
                    }
                }
            }
            Op::TypePointer => {
                // Follow pointer to pointee type
                if type_inst.operands.len() >= 2 {
                    let pointee_type = type_inst.operands[1].unwrap_id_ref();
                    return type_hierarchy_contains_array_type(
                        pointee_type,
                        target_array_type_id,
                        module,
                    );
                }
            }
            _ => {}
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use rspirv::dr::Module;

    // Helper function to assemble SPIR-V from text
    fn assemble_spirv(spirv: &str) -> Vec<u8> {
        use spirv_tools::assembler::{self, Assembler};

        let assembler = assembler::create(None);
        let spv_binary = assembler
            .assemble(spirv, assembler::AssemblerOptions::default())
            .expect("Failed to assemble test SPIR-V");
        let contents: &[u8] = spv_binary.as_ref();
        contents.to_vec()
    }

    // Helper function to load SPIR-V binary into Module
    fn load_spirv(bytes: &[u8]) -> Module {
        use rspirv::dr::Loader;

        let mut loader = Loader::new();
        rspirv::binary::parse_bytes(bytes, &mut loader).unwrap();
        loader.module()
    }

    // Helper function to count ArrayStride decorations
    fn count_array_stride_decorations(module: &Module) -> usize {
        module
            .annotations
            .iter()
            .filter(|inst| {
                inst.class.opcode == Op::Decorate
                    && inst.operands.len() >= 2
                    && inst.operands[1] == Operand::Decoration(Decoration::ArrayStride)
            })
            .count()
    }

    #[test]
    fn test_removes_array_stride_from_workgroup_arrays() {
        let spirv = r#"
            OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; Type declarations
            %void = OpTypeVoid
            %func_ty = OpTypeFunction %void
            %u32 = OpTypeInt 32 0
            %u32_256 = OpConstant %u32 256
            %array_ty = OpTypeArray %u32 %u32_256
            
            ; Pointer types for workgroup storage (forbidden in newer SPIR-V)
            %ptr_workgroup = OpTypePointer Workgroup %array_ty
            
            ; Variables in workgroup storage class
            %workgroup_var = OpVariable %ptr_workgroup Workgroup
            
            ; ArrayStride decoration that should be removed
            OpDecorate %array_ty ArrayStride 4
            
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            OpReturn
            OpFunctionEnd
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        assert_eq!(count_array_stride_decorations(&module), 1);

        fix_array_stride_decorations(&mut module);

        // ArrayStride should be removed from arrays used in Workgroup storage
        assert_eq!(count_array_stride_decorations(&module), 0);
    }

    #[test]
    fn test_keeps_array_stride_for_workgroup_with_capability() {
        let spirv = r#"
            OpCapability Shader
            OpCapability WorkgroupMemoryExplicitLayoutKHR
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; Type declarations
            %void = OpTypeVoid
            %func_ty = OpTypeFunction %void
            %u32 = OpTypeInt 32 0
            %u32_256 = OpConstant %u32 256
            %array_ty = OpTypeArray %u32 %u32_256
            
            ; Pointer types for workgroup storage (allowed with capability)
            %ptr_workgroup = OpTypePointer Workgroup %array_ty
            
            ; Variables in workgroup storage class
            %workgroup_var = OpVariable %ptr_workgroup Workgroup
            
            ; ArrayStride decoration that should be kept with capability
            OpDecorate %array_ty ArrayStride 4
            
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            OpReturn
            OpFunctionEnd
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        assert_eq!(count_array_stride_decorations(&module), 1);

        fix_array_stride_decorations(&mut module);

        // ArrayStride should be kept when WorkgroupMemoryExplicitLayoutKHR capability is present
        assert_eq!(count_array_stride_decorations(&module), 1);
    }

    #[test]
    fn test_keeps_array_stride_for_storage_buffer_arrays() {
        let spirv = r#"
            OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; Type declarations
            %void = OpTypeVoid
            %func_ty = OpTypeFunction %void
            %u32 = OpTypeInt 32 0
            %u32_256 = OpConstant %u32 256
            %array_ty = OpTypeArray %u32 %u32_256
            
            ; Pointer types for storage buffer (always allowed)
            %ptr_storage_buffer = OpTypePointer StorageBuffer %array_ty
            
            ; Variables in storage buffer storage class
            %storage_buffer_var = OpVariable %ptr_storage_buffer StorageBuffer
            
            ; ArrayStride decoration that should be kept
            OpDecorate %array_ty ArrayStride 4
            OpDecorate %storage_buffer_var DescriptorSet 0
            OpDecorate %storage_buffer_var Binding 0
            
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            OpReturn
            OpFunctionEnd
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        assert_eq!(count_array_stride_decorations(&module), 1);

        fix_array_stride_decorations(&mut module);

        // ArrayStride should be kept for StorageBuffer storage class
        assert_eq!(count_array_stride_decorations(&module), 1);
    }

    #[test]
    fn test_handles_runtime_arrays_in_workgroup() {
        let spirv = r#"
            OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; Type declarations
            %void = OpTypeVoid
            %func_ty = OpTypeFunction %void
            %u32 = OpTypeInt 32 0
            %runtime_array_ty = OpTypeRuntimeArray %u32
            
            ; Pointer types for workgroup storage (forbidden)
            %ptr_workgroup = OpTypePointer Workgroup %runtime_array_ty
            
            ; Variables in workgroup storage class
            %workgroup_var = OpVariable %ptr_workgroup Workgroup
            
            ; ArrayStride decoration that should be removed
            OpDecorate %runtime_array_ty ArrayStride 4
            
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            OpReturn
            OpFunctionEnd
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        assert_eq!(count_array_stride_decorations(&module), 1);

        fix_array_stride_decorations(&mut module);

        // ArrayStride should be removed from runtime arrays in Workgroup storage
        assert_eq!(count_array_stride_decorations(&module), 0);
    }

    #[test]
    fn test_mixed_storage_classes_removes_problematic_arrays() {
        let spirv = r#"
            OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; Type declarations
            %void = OpTypeVoid
            %func_ty = OpTypeFunction %void
            %u32 = OpTypeInt 32 0
            %u32_256 = OpConstant %u32 256
            %forbidden_array_ty = OpTypeArray %u32 %u32_256
            %allowed_array_ty = OpTypeArray %u32 %u32_256
            
            ; Pointer types for different storage classes
            %ptr_workgroup = OpTypePointer Workgroup %forbidden_array_ty
            %ptr_storage_buffer = OpTypePointer StorageBuffer %allowed_array_ty
            
            ; Variables in different storage classes
            %workgroup_var = OpVariable %ptr_workgroup Workgroup
            %storage_buffer_var = OpVariable %ptr_storage_buffer StorageBuffer
            
            ; ArrayStride decorations
            OpDecorate %forbidden_array_ty ArrayStride 4
            OpDecorate %allowed_array_ty ArrayStride 4
            OpDecorate %storage_buffer_var DescriptorSet 0
            OpDecorate %storage_buffer_var Binding 0
            
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            OpReturn
            OpFunctionEnd
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        assert_eq!(count_array_stride_decorations(&module), 2);

        fix_array_stride_decorations(&mut module);

        // Only the Workgroup array should have its ArrayStride removed
        assert_eq!(count_array_stride_decorations(&module), 1);
    }

    #[test]
    fn test_nested_structs_and_arrays_in_function_storage() {
        let spirv = r#"
            OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; ArrayStride decoration that should be removed in SPIR-V 1.4+
            OpDecorate %inner_array_ty ArrayStride 16
            
            ; Type declarations
            %void = OpTypeVoid
            %float = OpTypeFloat 32
            %u32 = OpTypeInt 32 0
            %u32_4 = OpConstant %u32 4
            %inner_array_ty = OpTypeArray %float %u32_4
            %inner_struct_ty = OpTypeStruct %inner_array_ty
            %outer_struct_ty = OpTypeStruct %inner_struct_ty
            
            ; Pointer types for function storage (forbidden in SPIR-V 1.4+)
            %ptr_function = OpTypePointer Function %outer_struct_ty
            %func_ty = OpTypeFunction %void
            
            ; Function variable inside function
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            %function_var = OpVariable %ptr_function Function
            OpReturn
            OpFunctionEnd
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        // Force SPIR-V 1.4 for this test
        if let Some(ref mut header) = module.header {
            header.set_version(1, 4);
        }

        assert_eq!(count_array_stride_decorations(&module), 1);

        fix_array_stride_decorations(&mut module);

        // ArrayStride should be removed in SPIR-V 1.4+ for Function storage
        assert_eq!(count_array_stride_decorations(&module), 0);
    }

    #[test]
    fn test_function_storage_spirv_13_keeps_decorations() {
        let spirv = r#"
            OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; Type declarations
            %void = OpTypeVoid
            %func_ty = OpTypeFunction %void
            %u32 = OpTypeInt 32 0
            %u32_256 = OpConstant %u32 256
            %array_ty = OpTypeArray %u32 %u32_256
            
            ; Pointer types for function storage
            %ptr_function = OpTypePointer Function %array_ty
            
            ; Function variable  
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            %function_var = OpVariable %ptr_function Function
            OpReturn
            OpFunctionEnd
            
            ; ArrayStride decoration that should be kept in SPIR-V 1.3
            OpDecorate %array_ty ArrayStride 4
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        // Force SPIR-V 1.3 for this test
        if let Some(ref mut header) = module.header {
            header.set_version(1, 3);
        }

        assert_eq!(count_array_stride_decorations(&module), 1);

        fix_array_stride_decorations(&mut module);

        // ArrayStride should be kept in SPIR-V 1.3 for Function storage
        assert_eq!(count_array_stride_decorations(&module), 1);
    }

    #[test]
    fn test_private_storage_spirv_14_removes_decorations() {
        let spirv = r#"
            OpCapability Shader
            OpMemoryModel Logical GLSL450
            OpEntryPoint GLCompute %main "main"
            OpExecutionMode %main LocalSize 1 1 1
            
            ; Type declarations
            %void = OpTypeVoid
            %func_ty = OpTypeFunction %void
            %u32 = OpTypeInt 32 0
            %u32_256 = OpConstant %u32 256
            %array_ty = OpTypeArray %u32 %u32_256
            
            ; Pointer types for private storage
            %ptr_private = OpTypePointer Private %array_ty
            
            ; Variables in private storage class
            %private_var = OpVariable %ptr_private Private
            
            ; ArrayStride decoration that should be removed in SPIR-V 1.4+
            OpDecorate %array_ty ArrayStride 4
            
            %main = OpFunction %void None %func_ty
            %entry = OpLabel
            OpReturn
            OpFunctionEnd
        "#;

        let bytes = assemble_spirv(spirv);
        let mut module = load_spirv(&bytes);

        // Force SPIR-V 1.4 for this test
        if let Some(ref mut header) = module.header {
            header.set_version(1, 4);
        }

        assert_eq!(count_array_stride_decorations(&module), 1);

        fix_array_stride_decorations(&mut module);

        // ArrayStride should be removed in SPIR-V 1.4+ for Private storage
        assert_eq!(count_array_stride_decorations(&module), 0);
    }
}
