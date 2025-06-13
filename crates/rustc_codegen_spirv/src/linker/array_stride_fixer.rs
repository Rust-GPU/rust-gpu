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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    /// Array types that this array contains as elements (for nested arrays)
    pub element_arrays: FxHashSet<Word>,
    /// Array types that contain this array as an element
    pub parent_arrays: FxHashSet<Word>,
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

/// Comprehensive fix for `ArrayStride` decorations with staged processing architecture
pub fn fix_array_stride_decorations_with_deduplication(module: &mut Module) {
    let spirv_version = module.header.as_ref().map_or((1, 0), |h| h.version());
    let has_workgroup_layout_capability = module.capabilities.iter().any(|inst| {
        inst.class.opcode == Op::Capability
            && inst.operands.first()
                == Some(&Operand::Capability(
                    Capability::WorkgroupMemoryExplicitLayoutKHR,
                ))
    });

    // Analyze all array usage patterns and dependencies
    let array_contexts =
        analyze_array_storage_contexts(module, spirv_version, has_workgroup_layout_capability);

    // Create specialized array types when necessary (mixed usage scenarios)
    let specializations = create_specialized_array_types(
        module,
        &array_contexts,
        spirv_version,
        has_workgroup_layout_capability,
    );

    // Update references with full context awareness
    if !specializations.is_empty() {
        update_references_for_specialized_arrays(
            module,
            &specializations,
            &array_contexts,
            spirv_version,
            has_workgroup_layout_capability,
        );
    }

    // Remove decorations from layout-forbidden contexts
    remove_array_stride_decorations_for_forbidden_contexts(
        module,
        &array_contexts,
        spirv_version,
        has_workgroup_layout_capability,
    );

    // Final cleanup and deduplication.
    // Always run the context-aware variant so that arrays used in differing
    // storage-class contexts are not incorrectly merged.
    crate::linker::duplicates::remove_duplicate_types_with_array_context(
        module,
        Some(&array_contexts),
    );
}

/// Remove `ArrayStride` decorations from arrays used in layout-forbidden storage classes
fn remove_array_stride_decorations_for_forbidden_contexts(
    module: &mut Module,
    array_contexts: &FxHashMap<Word, ArrayStorageContext>,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
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
                        !allows_layout(sc, spirv_version, has_workgroup_layout_capability)
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
fn analyze_array_storage_contexts(
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
                    element_arrays: FxHashSet::default(),
                    parent_arrays: FxHashSet::default(),
                });
            }
        }
    }

    // Build parent-child relationships between array types
    for inst in &module.types_global_values {
        if matches!(inst.class.opcode, Op::TypeArray | Op::TypeRuntimeArray) {
            if let Some(parent_id) = inst.result_id {
                if !inst.operands.is_empty() {
                    let element_type = inst.operands[0].unwrap_id_ref();
                    // If the element type is also an array, record the parent-child relationship
                    if array_types.contains(&element_type) {
                        if let Some(parent_context) = array_contexts.get_mut(&parent_id) {
                            parent_context.element_arrays.insert(element_type);
                        }
                        if let Some(element_context) = array_contexts.get_mut(&element_type) {
                            element_context.parent_arrays.insert(parent_id);
                        }
                    }
                }
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

    // Determine usage patterns with enhanced logic for nested arrays
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

    // Propagate context from parent arrays to child arrays for better consistency
    // If a parent array is pure workgroup, its child arrays should inherit this context
    let mut changed = true;
    while changed {
        changed = false;
        let array_ids: Vec<Word> = array_contexts.keys().copied().collect();

        for &array_id in &array_ids {
            let (parent_arrays, current_pattern) = {
                let context = &array_contexts[&array_id];
                (context.parent_arrays.clone(), context.usage_pattern)
            };

            // If this array has parent arrays that are pure workgroup, and this array
            // doesn't have mixed usage, then it should also be pure workgroup
            if current_pattern != ArrayUsagePattern::LayoutForbidden
                && current_pattern != ArrayUsagePattern::MixedUsage
            {
                let has_pure_workgroup_parent = parent_arrays.iter().any(|&parent_id| {
                    matches!(
                        array_contexts.get(&parent_id).map(|ctx| ctx.usage_pattern),
                        Some(ArrayUsagePattern::LayoutForbidden)
                    )
                });

                if has_pure_workgroup_parent {
                    if let Some(context) = array_contexts.get_mut(&array_id) {
                        // Only inherit if this array doesn't have its own conflicting storage classes
                        let has_layout_required_usage = context.storage_classes.iter().any(|&sc| {
                            allows_layout(sc, spirv_version, has_workgroup_layout_capability)
                        });

                        if !has_layout_required_usage {
                            context.usage_pattern = ArrayUsagePattern::LayoutForbidden;
                            // Also inherit the workgroup storage class if not already present
                            context.storage_classes.insert(StorageClass::Workgroup);
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    array_contexts
}

/// Create specialized array types for mixed usage scenarios
fn create_specialized_array_types(
    module: &mut Module,
    array_contexts: &FxHashMap<Word, ArrayStorageContext>,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) -> FxHashMap<Word, (Word, Word)> {
    let mut specializations = FxHashMap::default(); // original_id -> (layout_required_id, layout_forbidden_id)

    // Find arrays that need specialization (true mixed usage only)
    // Be more conservative - only specialize arrays that truly have conflicting requirements
    let arrays_to_specialize: Vec<Word> = array_contexts
        .iter()
        .filter_map(|(&id, context)| {
            // Only specialize if the array has BOTH layout-required AND layout-forbidden usage
            // AND it's not just inheriting context from parents (check own storage classes)
            if context.usage_pattern == ArrayUsagePattern::MixedUsage {
                let mut has_layout_required = false;
                let mut has_layout_forbidden = false;

                // Check actual storage classes, not inherited patterns
                for &storage_class in &context.storage_classes {
                    if allows_layout(
                        storage_class,
                        spirv_version,
                        has_workgroup_layout_capability,
                    ) {
                        has_layout_required = true;
                    } else {
                        has_layout_forbidden = true;
                    }
                }

                // Only specialize if there's a true conflict in this array's own usage
                if has_layout_required && has_layout_forbidden {
                    Some(id)
                } else {
                    None
                }
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

    // Insert specialized arrays right after their corresponding original arrays
    // This maintains proper SPIR-V type ordering
    let mut new_types_global_values = Vec::new();

    for inst in &module.types_global_values {
        new_types_global_values.push(inst.clone());

        // If this is an array that was specialized, add the specialized versions right after
        if let Some(result_id) = inst.result_id {
            if let Some(&(layout_required_id, layout_forbidden_id)) =
                specializations.get(&result_id)
            {
                // Find and add the specialized versions
                for new_inst in &new_type_instructions {
                    if new_inst.result_id == Some(layout_required_id) {
                        new_types_global_values.push(new_inst.clone());
                        break;
                    }
                }
                for new_inst in &new_type_instructions {
                    if new_inst.result_id == Some(layout_forbidden_id) {
                        new_types_global_values.push(new_inst.clone());
                        break;
                    }
                }
            }
        }
    }

    module.types_global_values = new_types_global_values;

    specializations
}

/// Helper function to select the appropriate specialized variant based on context
fn select_array_variant_for_context(
    original_id: Word,
    context_storage_classes: &FxHashSet<StorageClass>,
    specializations: &FxHashMap<Word, (Word, Word)>,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) -> Option<Word> {
    if let Some(&(layout_required_id, layout_forbidden_id)) = specializations.get(&original_id) {
        // If context is pure workgroup, use layout_forbidden variant
        if context_storage_classes.len() == 1
            && context_storage_classes.contains(&StorageClass::Workgroup)
        {
            return Some(layout_forbidden_id);
        }

        // If context has any layout-forbidden storage classes, use layout_forbidden variant
        let has_forbidden_context = context_storage_classes
            .iter()
            .any(|&sc| !allows_layout(sc, spirv_version, has_workgroup_layout_capability));

        if has_forbidden_context {
            Some(layout_forbidden_id)
        } else {
            Some(layout_required_id)
        }
    } else {
        None
    }
}

/// Update all references to specialized array types and create specialized pointer types
fn update_references_for_specialized_arrays(
    module: &mut Module,
    specializations: &FxHashMap<Word, (Word, Word)>,
    array_contexts: &FxHashMap<Word, ArrayStorageContext>,
    spirv_version: (u8, u8),
    has_workgroup_layout_capability: bool,
) {
    if specializations.is_empty() {
        return;
    }

    let mut next_id = module.header.as_ref().map_or(1, |h| h.bound);

    // Step 1: Create new pointer types for specialized arrays
    let mut pointer_rewrite_rules = FxHashMap::default(); // old_pointer_id -> new_pointer_id
    let mut new_pointer_instructions = Vec::new();

    // Collect all pointer types that need updating
    for inst in &module.types_global_values {
        if inst.class.opcode == Op::TypePointer && inst.operands.len() >= 2 {
            let storage_class = inst.operands[0].unwrap_storage_class();
            let pointee_type = inst.operands[1].unwrap_id_ref();

            if let Some(&(layout_required_id, layout_forbidden_id)) =
                specializations.get(&pointee_type)
            {
                // Choose variant based on storage class
                let target_array_id = if allows_layout(
                    storage_class,
                    spirv_version,
                    has_workgroup_layout_capability,
                ) {
                    layout_required_id
                } else {
                    layout_forbidden_id
                };

                // Create new pointer type
                let mut new_pointer_inst = inst.clone();
                new_pointer_inst.result_id = Some(next_id);
                new_pointer_inst.operands[1] = Operand::IdRef(target_array_id);
                new_pointer_instructions.push(new_pointer_inst);

                pointer_rewrite_rules.insert(inst.result_id.unwrap(), next_id);
                next_id += 1;
            }
        }
    }

    // Step 2: Update struct field and array element references that point to
    // original (now specialized) arrays so they reference the appropriate
    // specialized variant.

    // 2a) Struct field types
    for inst in &mut module.types_global_values {
        if inst.class.opcode == Op::TypeStruct {
            for op in &mut inst.operands {
                if let Some(field_type_id) = op.id_ref_any_mut() {
                    if let Some(new_id) = select_array_variant_for_context(
                        *field_type_id,
                        // We don't have per struct storage class context, but
                        // any arrays appearing inside a Block decorated struct
                        // are expected to be in layout-required contexts.
                        &[StorageClass::StorageBuffer, StorageClass::Uniform]
                            .iter()
                            .cloned()
                            .collect::<FxHashSet<StorageClass>>(),
                        specializations,
                        spirv_version,
                        has_workgroup_layout_capability,
                    ) {
                        *field_type_id = new_id;
                    }
                }
            }
        }
    }

    // 2b) Array element references for non-specialized arrays
    for inst in &mut module.types_global_values {
        if matches!(inst.class.opcode, Op::TypeArray | Op::TypeRuntimeArray) {
            if let Some(parent_id) = inst.result_id {
                // Skip arrays that were themselves specialized (they should already be correctly set up)
                if specializations.contains_key(&parent_id) {
                    continue;
                }

                // Update element reference based on parent's context
                if let Some(element_operand) = inst.operands.get_mut(0) {
                    if let Some(elem_id) = element_operand.id_ref_any_mut() {
                        if let Some(parent_context) = array_contexts.get(&parent_id) {
                            if let Some(new_elem_id) = select_array_variant_for_context(
                                *elem_id,
                                &parent_context.storage_classes,
                                specializations,
                                spirv_version,
                                has_workgroup_layout_capability,
                            ) {
                                *elem_id = new_elem_id;
                            }
                        }
                    }
                }
            }
        }
    }

    // Step 3: Update pointer types in types section to reference specialized arrays
    let mut updated_pointer_types = Vec::new();
    for inst in &module.types_global_values {
        if inst.class.opcode == Op::TypePointer && inst.operands.len() >= 2 {
            let storage_class = inst.operands[0].unwrap_storage_class();
            let pointee_type = inst.operands[1].unwrap_id_ref();

            if let Some(&(layout_required_id, layout_forbidden_id)) =
                specializations.get(&pointee_type)
            {
                // Choose variant based on storage class
                let target_array_id = if allows_layout(
                    storage_class,
                    spirv_version,
                    has_workgroup_layout_capability,
                ) {
                    layout_required_id
                } else {
                    layout_forbidden_id
                };

                // Update the pointer to reference the appropriate specialized array
                let mut updated_inst = inst.clone();
                updated_inst.operands[1] = Operand::IdRef(target_array_id);
                updated_pointer_types.push((inst.result_id.unwrap(), updated_inst));
            }
        }
    }

    // Apply pointer type updates
    for inst in &mut module.types_global_values {
        if let Some(result_id) = inst.result_id {
            for (old_id, new_inst) in &updated_pointer_types {
                if result_id == *old_id {
                    *inst = new_inst.clone();
                    break;
                }
            }
        }
    }

    // Step 4: Keep original arrays (even after specialization) to avoid
    // potential forward-reference ordering issues. These original types are
    // now unused, but retaining them is harmless and greatly simplifies the
    // type-ordering constraints enforced by the SPIR-V -> SPIR-T lowering
    // step.
    // NOTE: If size becomes a concern, we can revisit this and implement a
    // safer removal strategy that preserves correct ordering.

    // Step 5: Add new pointer types to the module
    module.types_global_values.extend(new_pointer_instructions);

    // Update module header bound
    if let Some(ref mut header) = module.header {
        header.bound = next_id;
    }

    // Step 6: Apply pointer rewrite rules throughout the module
    for inst in module.all_inst_iter_mut() {
        if let Some(ref mut id) = inst.result_type {
            *id = pointer_rewrite_rules.get(id).copied().unwrap_or(*id);
        }
        for op in &mut inst.operands {
            if let Some(id) = op.id_ref_any_mut() {
                *id = pointer_rewrite_rules.get(id).copied().unwrap_or(*id);
            }
        }
    }

    // Step 7: Handle ArrayStride decorations for specialized arrays
    let mut decorations_to_add = Vec::new();
    for inst in &module.annotations {
        if inst.class.opcode == Op::Decorate
            && inst.operands.len() >= 2
            && inst.operands[1] == Operand::Decoration(Decoration::ArrayStride)
        {
            let target_id = inst.operands[0].unwrap_id_ref();
            if let Some(&(layout_required_id, _)) = specializations.get(&target_id) {
                let mut new_decoration = inst.clone();
                new_decoration.operands[0] = Operand::IdRef(layout_required_id);
                decorations_to_add.push(new_decoration);
            }
        }
    }
    module.annotations.extend(decorations_to_add);

    // Remove ArrayStride decorations from original and layout-forbidden arrays
    let layout_forbidden_arrays: FxHashSet<Word> = specializations
        .values()
        .map(|&(_, layout_forbidden_id)| layout_forbidden_id)
        .collect();
    let arrays_to_remove_decorations: FxHashSet<Word> = layout_forbidden_arrays
        .iter()
        .cloned()
        .chain(specializations.keys().cloned())
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

    // Step 8: Rewrite any remaining uses/results of original array IDs to the
    // chosen specialized variant (defaulting to the layout-required variant).
    if !specializations.is_empty() {
        let mut array_default_rewrite: FxHashMap<Word, Word> = FxHashMap::default();
        for (&orig, &(layout_required_id, _)) in specializations {
            array_default_rewrite.insert(orig, layout_required_id);
        }

        for inst in module.all_inst_iter_mut() {
            // Skip type declarations themselves – we only want to fix *uses* of the IDs.
            if matches!(
                inst.class.opcode,
                Op::TypeVoid
                    | Op::TypeBool
                    | Op::TypeInt
                    | Op::TypeFloat
                    | Op::TypeVector
                    | Op::TypeMatrix
                    | Op::TypeImage
                    | Op::TypeSampler
                    | Op::TypeSampledImage
                    | Op::TypeArray
                    | Op::TypeRuntimeArray
                    | Op::TypeStruct
                    | Op::TypeOpaque
                    | Op::TypePointer
                    | Op::TypeFunction
                    | Op::TypeEvent
                    | Op::TypeDeviceEvent
                    | Op::TypeReserveId
                    | Op::TypeQueue
                    | Op::TypePipe
                    | Op::TypeForwardPointer
            ) {
                continue;
            }

            // Avoid changing the declared type of function parameters and
            // composite ops, as they must stay in sync with their value
            // operands.
            if !matches!(
                inst.class.opcode,
                Op::FunctionParameter | Op::CompositeInsert | Op::CompositeExtract
            ) {
                if let Some(ref mut ty) = inst.result_type {
                    if let Some(&new) = array_default_rewrite.get(ty) {
                        *ty = new;
                    }
                }
            }
            for op in &mut inst.operands {
                if let Some(id) = op.id_ref_any_mut() {
                    if let Some(&new) = array_default_rewrite.get(id) {
                        *id = new;
                    }
                }
            }
        }
    }
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
