//! Fix `ArrayStride` decorations for newer SPIR-V versions.
//!
//! Newer SPIR-V versions forbid explicit layouts (`ArrayStride` decorations) in certain
//! storage classes (Function, Private, Workgroup), but allow them in others
//! (`StorageBuffer`, Uniform). This module removes `ArrayStride` decorations from
//! array types that are used in contexts where they're forbidden.

use rspirv::dr::{Module, Operand};
use rspirv::spirv::{Capability, Decoration, Op, StorageClass, Word};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};

/// Describes how an array type is used across different storage classes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayUsagePattern {
    /// Array is only used in storage classes that require explicit layout
    LayoutRequired,
    /// Array is only used in storage classes that forbid explicit layout
    LayoutForbidden,
    /// Array is used in both types of storage classes (needs specialization)
    MixedUsage,
    /// Array is not used in any variables (orphaned)
    Unused,
}

/// Context information about array type usage
#[derive(Debug, Clone)]
pub struct ArrayStorageContext {
    /// Which storage classes this array type is used in
    pub storage_classes: FxHashSet<StorageClass>,
    /// Whether this array allows or forbids layout in its contexts
    pub usage_pattern: ArrayUsagePattern,
}

/// Check if a storage class allows explicit layout decorations based on SPIR-V version and capabilities.
/// This matches the logic from SPIRV-Tools `validate_decorations.cpp` `AllowsLayout` function.
fn allows_layout(
    storage_class: StorageClass,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) -> bool {
    match storage_class {
        // Never allows layout
        StorageClass::UniformConstant => false,

        // Requires explicit capability
        StorageClass::Workgroup => has_workgroup_layout_capability,

        // Only forbidden in SPIR-V 1.4+
        StorageClass::Function | StorageClass::Private => spirv_version < (1, 4),

        // All other storage classes allow layout by default
        _ => true,
    }
}

/// Comprehensive fix for `ArrayStride` decorations with optional type deduplication
pub fn fix_array_stride_decorations_with_deduplication(
    module: &mut Module,
    use_context_aware_deduplication: bool,
) {
    // Get SPIR-V version from module header
    let spirv_version = module.header.as_ref().map_or((1, 0), |h| h.version()); // Default to 1.0 if no header

    // Check for WorkgroupMemoryExplicitLayoutKHR capability
    let has_workgroup_layout_capability = module.capabilities.iter().any(|inst| {
        inst.class.opcode == Op::Capability
            && inst.operands.first()
                == Some(&Operand::Capability(
                    Capability::WorkgroupMemoryExplicitLayoutKHR,
                ))
    });

    // Analyze storage class contexts for all array types
    let array_contexts =
        analyze_array_storage_contexts(module, spirv_version, has_workgroup_layout_capability);

    // Handle mixed usage arrays by creating specialized versions
    let specializations = create_specialized_array_types(module, &array_contexts);

    // Update references to use appropriate specialized types
    if !specializations.is_empty() {
        update_references_for_specialized_arrays(
            module,
            &specializations,
            spirv_version,
            has_workgroup_layout_capability,
        );
    }

    // Apply context-aware type deduplication if requested
    if use_context_aware_deduplication {
        crate::linker::duplicates::remove_duplicate_types_with_array_context(
            module,
            Some(&array_contexts),
        );
    }

    // Remove ArrayStride decorations from arrays used in forbidden contexts
    remove_array_stride_decorations_for_forbidden_contexts(module, &array_contexts);
}

/// Remove `ArrayStride` decorations from arrays used in layout-forbidden storage classes
fn remove_array_stride_decorations_for_forbidden_contexts(
    module: &mut Module,
    array_contexts: &FxHashMap<Word, ArrayStorageContext>,
) {
    // Find array types that should have their ArrayStride decorations removed
    // Remove from arrays used in forbidden contexts OR mixed usage that includes forbidden contexts
    let arrays_to_remove_stride: FxHashSet<Word> = array_contexts
        .iter()
        .filter_map(|(&id, context)| {
            match context.usage_pattern {
                // Always remove from arrays used only in forbidden contexts
                ArrayUsagePattern::LayoutForbidden => Some(id),
                // For mixed usage, remove if it includes forbidden contexts that would cause validation errors
                ArrayUsagePattern::MixedUsage => {
                    // If the array is used in any context that forbids layout, remove the decoration
                    // This is a conservative approach that prevents validation errors
                    let has_forbidden_context = context.storage_classes.iter().any(|&sc| {
                        !allows_layout(sc, (1, 4), false) // Use SPIR-V 1.4 rules for conservative check
                    });

                    if has_forbidden_context {
                        Some(id)
                    } else {
                        None
                    }
                }
                ArrayUsagePattern::LayoutRequired | ArrayUsagePattern::Unused => None,
            }
        })
        .collect();

    // Remove ArrayStride decorations for layout-forbidden arrays
    module.annotations.retain(|inst| {
        if inst.class.opcode == Op::Decorate
            && inst.operands.len() >= 2
            && inst.operands[1] == Operand::Decoration(Decoration::ArrayStride)
        {
            let target_id = inst.operands[0].unwrap_id_ref();
            !arrays_to_remove_stride.contains(&target_id)
        } else {
            true
        }
    });
}

/// Analyze storage class contexts for all array types in the module
pub fn analyze_array_storage_contexts(
    module: &Module,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) -> FxHashMap<Word, ArrayStorageContext> {
    let mut array_contexts: FxHashMap<Word, ArrayStorageContext> = FxHashMap::default();

    // Find all array and runtime array types
    let mut array_types = FxHashSet::default();
    for inst in &module.types_global_values {
        if matches!(inst.class.opcode, Op::TypeArray | Op::TypeRuntimeArray) {
            if let Some(result_id) = inst.result_id {
                array_types.insert(result_id);
                array_contexts.insert(result_id, ArrayStorageContext {
                    storage_classes: FxHashSet::default(),
                    usage_pattern: ArrayUsagePattern::Unused,
                });
            }
        }
    }

    // Analyze global variables
    for inst in &module.types_global_values {
        if inst.class.opcode == Op::Variable && !inst.operands.is_empty() {
            let storage_class = inst.operands[0].unwrap_storage_class();

            if let Some(var_type_id) = inst.result_type {
                // Check if this variable's type hierarchy contains any array types
                for &array_type_id in &array_types {
                    if type_hierarchy_contains_array_type(var_type_id, array_type_id, module) {
                        if let Some(context) = array_contexts.get_mut(&array_type_id) {
                            context.storage_classes.insert(storage_class);
                        }
                    }
                }
            }
        }
    }

    // Analyze function-local variables
    for function in &module.functions {
        for block in &function.blocks {
            for inst in &block.instructions {
                if inst.class.opcode == Op::Variable && !inst.operands.is_empty() {
                    let storage_class = inst.operands[0].unwrap_storage_class();

                    if let Some(var_type_id) = inst.result_type {
                        // Check if this variable's type hierarchy contains any array types
                        for &array_type_id in &array_types {
                            if type_hierarchy_contains_array_type(
                                var_type_id,
                                array_type_id,
                                module,
                            ) {
                                if let Some(context) = array_contexts.get_mut(&array_type_id) {
                                    context.storage_classes.insert(storage_class);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Determine usage patterns
    for context in array_contexts.values_mut() {
        if context.storage_classes.is_empty() {
            context.usage_pattern = ArrayUsagePattern::Unused;
        } else {
            let mut requires_layout = false;
            let mut forbids_layout = false;

            for &storage_class in &context.storage_classes {
                if allows_layout(
                    storage_class,
                    spirv_version,
                    has_workgroup_layout_capability,
                ) {
                    requires_layout = true;
                } else {
                    forbids_layout = true;
                }
            }

            context.usage_pattern = match (requires_layout, forbids_layout) {
                (true, true) => ArrayUsagePattern::MixedUsage,
                (true, false) => ArrayUsagePattern::LayoutRequired,
                (false, true) => ArrayUsagePattern::LayoutForbidden,
                (false, false) => ArrayUsagePattern::Unused, // Should not happen
            };
        }
    }

    array_contexts
}

/// Create specialized array types for mixed usage scenarios
fn create_specialized_array_types(
    module: &mut Module,
    array_contexts: &FxHashMap<Word, ArrayStorageContext>,
) -> FxHashMap<Word, (Word, Word)> {
    let mut specializations = FxHashMap::default(); // original_id -> (layout_required_id, layout_forbidden_id)

    // Find arrays that need specialization (mixed usage)
    let arrays_to_specialize: Vec<Word> = array_contexts
        .iter()
        .filter_map(|(&id, context)| {
            if context.usage_pattern == ArrayUsagePattern::MixedUsage {
                Some(id)
            } else {
                None
            }
        })
        .collect();

    if arrays_to_specialize.is_empty() {
        return specializations;
    }

    // Generate new IDs for specialized types
    let mut next_id = module.header.as_ref().map_or(1, |h| h.bound);

    for &original_id in &arrays_to_specialize {
        let layout_required_id = next_id;
        next_id += 1;
        let layout_forbidden_id = next_id;
        next_id += 1;

        specializations.insert(original_id, (layout_required_id, layout_forbidden_id));
    }

    // Update the module header bound
    if let Some(ref mut header) = module.header {
        header.bound = next_id;
    }

    // Create specialized array type definitions
    let mut new_type_instructions = Vec::new();

    for &original_id in &arrays_to_specialize {
        if let Some((layout_required_id, layout_forbidden_id)) = specializations.get(&original_id) {
            // Find the original array type instruction
            if let Some(original_inst) = module
                .types_global_values
                .iter()
                .find(|inst| inst.result_id == Some(original_id))
                .cloned()
            {
                // Create layout-required variant (keeps ArrayStride decorations)
                let mut layout_required_inst = original_inst.clone();
                layout_required_inst.result_id = Some(*layout_required_id);
                new_type_instructions.push(layout_required_inst);

                // Create layout-forbidden variant (will have ArrayStride decorations removed later)
                let mut layout_forbidden_inst = original_inst.clone();
                layout_forbidden_inst.result_id = Some(*layout_forbidden_id);
                new_type_instructions.push(layout_forbidden_inst);
            }
        }
    }

    // IMPORTANT: Do not add the specialized arrays to the end - this would create forward references
    // Instead, we need to insert them in the correct position to maintain SPIR-V type ordering

    // Find the insertion point: after the last original array type that needs specialization
    // This ensures all specialized arrays are defined before any types that might reference them
    let mut insertion_point = 0;
    for (i, inst) in module.types_global_values.iter().enumerate() {
        if let Some(result_id) = inst.result_id {
            if arrays_to_specialize.contains(&result_id) {
                insertion_point = i + 1;
            }
        }
    }

    // Insert the specialized array types at the calculated position
    // This maintains the invariant that referenced types appear before referencing types
    for (i, new_inst) in new_type_instructions.into_iter().enumerate() {
        module
            .types_global_values
            .insert(insertion_point + i, new_inst);
    }

    specializations
}

/// Update all references to specialized array types based on storage class context
fn update_references_for_specialized_arrays(
    module: &mut Module,
    specializations: &FxHashMap<Word, (Word, Word)>,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) {
    // Update struct types that contain specialized arrays
    // This is safe now because all specialized arrays have been properly positioned in the types section
    for inst in &mut module.types_global_values {
        if inst.class.opcode == Op::TypeStruct {
            for operand in &mut inst.operands {
                if let Some(referenced_id) = operand.id_ref_any() {
                    if let Some(&(layout_required_id, _layout_forbidden_id)) =
                        specializations.get(&referenced_id)
                    {
                        // For struct types, we use the layout-required variant since structs
                        // can be used in both layout-required and layout-forbidden contexts
                        *operand = Operand::IdRef(layout_required_id);
                    }
                }
            }
        }
    }

    // Collect all existing pointer types that reference specialized arrays FIRST
    let mut existing_pointers_to_specialize = Vec::new();
    for inst in &module.types_global_values {
        if inst.class.opcode == Op::TypePointer && inst.operands.len() >= 2 {
            let pointee_type = inst.operands[1].unwrap_id_ref();
            if specializations.contains_key(&pointee_type) {
                existing_pointers_to_specialize.push(inst.clone());
            }
        }
    }

    // Create ALL specialized pointer types from the collected existing ones
    let mut next_id = module.header.as_ref().map_or(1, |h| h.bound);
    let mut new_pointer_instructions = Vec::new();
    let mut pointer_type_mappings = FxHashMap::default(); // old_pointer_id -> new_pointer_id

    // Create new pointer types for each storage class context
    for inst in &existing_pointers_to_specialize {
        let storage_class = inst.operands[0].unwrap_storage_class();
        let pointee_type = inst.operands[1].unwrap_id_ref();

        if let Some(&(layout_required_id, layout_forbidden_id)) = specializations.get(&pointee_type)
        {
            let allows_layout_for_sc = allows_layout(
                storage_class,
                spirv_version,
                has_workgroup_layout_capability,
            );

            // Create new pointer type pointing to appropriate specialized array
            let target_array_id = if allows_layout_for_sc {
                layout_required_id
            } else {
                layout_forbidden_id
            };

            let mut new_pointer_inst = inst.clone();
            new_pointer_inst.result_id = Some(next_id);
            new_pointer_inst.operands[1] = Operand::IdRef(target_array_id);
            new_pointer_instructions.push(new_pointer_inst);

            // Map old pointer to new pointer
            if let Some(old_pointer_id) = inst.result_id {
                pointer_type_mappings.insert(old_pointer_id, next_id);
            }
            next_id += 1;
        }
    }

    // Update module header bound to account for the new pointer types
    if let Some(ref mut header) = module.header {
        header.bound = next_id;
    }

    // Insert new pointer type instructions in the correct position
    // They must come after the specialized arrays they reference, but before any variables that use them

    // Find the last specialized array position to ensure pointers come after their pointee types
    let mut pointer_insertion_point = 0;
    for (i, inst) in module.types_global_values.iter().enumerate() {
        if let Some(result_id) = inst.result_id {
            // Check if this is one of our specialized arrays
            if specializations
                .values()
                .any(|&(req_id, forb_id)| result_id == req_id || result_id == forb_id)
            {
                pointer_insertion_point = i + 1;
            }
        }
    }

    // Insert the new pointer types at the calculated position
    // This ensures they appear after specialized arrays but before variables
    for (i, new_pointer_inst) in new_pointer_instructions.into_iter().enumerate() {
        module
            .types_global_values
            .insert(pointer_insertion_point + i, new_pointer_inst);
    }

    // Update ALL references to old pointer types throughout the entire module
    // This includes variables, function parameters, and all instructions

    // Update global variables and function types
    for inst in &mut module.types_global_values {
        match inst.class.opcode {
            Op::Variable => {
                if let Some(var_type_id) = inst.result_type {
                    if let Some(&new_pointer_id) = pointer_type_mappings.get(&var_type_id) {
                        inst.result_type = Some(new_pointer_id);
                    }
                }
            }
            Op::TypeFunction => {
                // Update function type operands (return type and parameter types)
                for operand in &mut inst.operands {
                    if let Some(referenced_id) = operand.id_ref_any() {
                        if let Some(&new_pointer_id) = pointer_type_mappings.get(&referenced_id) {
                            *operand = Operand::IdRef(new_pointer_id);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Update function signatures and local variables
    for function in &mut module.functions {
        // Update function parameters
        for param in &mut function.parameters {
            if let Some(param_type_id) = param.result_type {
                if let Some(&new_pointer_id) = pointer_type_mappings.get(&param_type_id) {
                    param.result_type = Some(new_pointer_id);
                }
            }
        }

        // Update all instructions in function bodies
        for block in &mut function.blocks {
            for inst in &mut block.instructions {
                // Update result type
                if let Some(result_type_id) = inst.result_type {
                    if let Some(&new_pointer_id) = pointer_type_mappings.get(&result_type_id) {
                        inst.result_type = Some(new_pointer_id);
                    }
                }

                // Update operand references
                for operand in &mut inst.operands {
                    if let Some(referenced_id) = operand.id_ref_any() {
                        if let Some(&new_pointer_id) = pointer_type_mappings.get(&referenced_id) {
                            *operand = Operand::IdRef(new_pointer_id);
                        }
                    }
                }
            }
        }
    }

    // Remove old pointer type instructions that reference specialized arrays
    module.types_global_values.retain(|inst| {
        if inst.class.opcode == Op::TypePointer && inst.operands.len() >= 2 {
            let pointee_type = inst.operands[1].unwrap_id_ref();
            !specializations.contains_key(&pointee_type)
        } else {
            true
        }
    });

    // Remove original array type instructions that were specialized
    let arrays_to_remove: FxHashSet<Word> = specializations.keys().cloned().collect();
    module.types_global_values.retain(|inst| {
        if let Some(result_id) = inst.result_id {
            !arrays_to_remove.contains(&result_id)
        } else {
            true
        }
    });

    // STEP 8: Copy ArrayStride decorations from original arrays to layout-required variants
    // and remove them from layout-forbidden variants
    let mut decorations_to_add = Vec::new();
    let layout_forbidden_arrays: FxHashSet<Word> = specializations
        .values()
        .map(|&(_, layout_forbidden_id)| layout_forbidden_id)
        .collect();

    // Find existing ArrayStride decorations on original arrays and copy them to layout-required variants
    for inst in &module.annotations {
        if inst.class.opcode == Op::Decorate
            && inst.operands.len() >= 2
            && inst.operands[1] == Operand::Decoration(Decoration::ArrayStride)
        {
            let target_id = inst.operands[0].unwrap_id_ref();
            if let Some(&(layout_required_id, _)) = specializations.get(&target_id) {
                // Copy the decoration to the layout-required variant
                let mut new_decoration = inst.clone();
                new_decoration.operands[0] = Operand::IdRef(layout_required_id);
                decorations_to_add.push(new_decoration);
            }
        }
    }

    // Add the copied decorations
    module.annotations.extend(decorations_to_add);

    // Remove ArrayStride decorations from layout-forbidden arrays and original arrays
    let arrays_to_remove_decorations: FxHashSet<Word> = layout_forbidden_arrays
        .iter()
        .cloned()
        .chain(specializations.keys().cloned()) // Also remove from original arrays
        .collect();

    module.annotations.retain(|inst| {
        if inst.class.opcode == Op::Decorate
            && inst.operands.len() >= 2
            && inst.operands[1] == Operand::Decoration(Decoration::ArrayStride)
        {
            let target_id = inst.operands[0].unwrap_id_ref();
            !arrays_to_remove_decorations.contains(&target_id)
        } else {
            true
        }
    });
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
