//! Rich error reporting for SPIR-V validation errors.
//!
//! This module provides utilities for converting SPIR-V validation errors into
//! user-friendly rustc diagnostics with source spans and helpful hints.

use std::path::Path;

use rspirv::dr::Module;
use rspirv::spirv::{BuiltIn, Capability, Decoration, Op};
use rustc_session::Session;
use spirv_tools::ValidationError;

use crate::custom_decorations::SpanRegenerator;

/// Context for generating rich validation error messages.
pub struct ValidationErrorContext<'a> {
    sess: &'a Session,
    module: Option<&'a Module>,
    span_regen: Option<SpanRegenerator<'a>>,
    filename: &'a Path,
}

impl<'a> ValidationErrorContext<'a> {
    /// Creates a new validation error context.
    pub fn new(sess: &'a Session, module: Option<&'a Module>, filename: &'a Path) -> Self {
        let span_regen = module.map(|m| SpanRegenerator::new(sess.source_map(), m));
        Self {
            sess,
            module,
            span_regen,
            filename,
        }
    }

    /// Emits a rich diagnostic for the given validation error.
    pub fn emit_error(&mut self, error: &spirv_tools::val::ValidatorError) {
        match &error.validation_error {
            ValidationError::EntryPointInterfaceLocationConflict {
                first_var,
                second_var,
                location,
                component,
                storage_class,
                ..
            } => {
                self.emit_location_conflict(
                    u32::from(*first_var),
                    u32::from(*second_var),
                    *location,
                    *component,
                    *storage_class,
                );
            }
            ValidationError::NotALogicalPointer {
                instruction,
                pointer,
                source_opcode,
            } => {
                self.emit_not_a_logical_pointer(*instruction, u32::from(*pointer), *source_opcode);
            }
            ValidationError::MissingInstructionCapability {
                opcode,
                required_capability,
            } => {
                self.emit_missing_capability(*opcode, *required_capability, None);
            }
            ValidationError::MissingOperandCapability {
                opcode,
                operand_index,
                required_capability,
            } => {
                self.emit_missing_capability(*opcode, *required_capability, Some(*operand_index));
            }
            ValidationError::MissingDescriptorSetDecoration { variable } => {
                self.emit_missing_decoration(
                    u32::from(*variable),
                    "DescriptorSet",
                    "#[spirv(descriptor_set = N)]",
                );
            }
            ValidationError::MissingBindingDecoration { variable } => {
                self.emit_missing_decoration(
                    u32::from(*variable),
                    "Binding",
                    "#[spirv(binding = N)]",
                );
            }
            ValidationError::InvalidBlockLayout {
                struct_type,
                reason,
            } => {
                self.emit_invalid_block_layout(u32::from(*struct_type), reason);
            }
            ValidationError::InvalidBuiltInType { builtin, expected } => {
                self.emit_invalid_builtin_type(*builtin, expected);
            }
            _ => {
                // Fall back to generic error message
                self.emit_generic_error(error);
            }
        }
    }

    /// Emits a generic validation error without rich formatting.
    fn emit_generic_error(&self, error: &spirv_tools::val::ValidatorError) {
        let mut err = self.sess.dcx().struct_err(error.to_string());
        err.note(format!("module `{}`", self.filename.display()));
        err.emit();
    }

    fn emit_location_conflict(
        &mut self,
        first_var_id: u32,
        second_var_id: u32,
        location: u32,
        component: u32,
        storage_class: rspirv::spirv::StorageClass,
    ) {
        let first_span = self.id_to_span(first_var_id);
        let second_span = self.id_to_span(second_var_id);

        let first_name = self.get_name(first_var_id);
        let second_name = self.get_name(second_var_id);

        let first_var_start_location = self.get_location_decoration(first_var_id);
        let first_var_type_info = self.get_variable_type_info(first_var_id);

        let first_name_fallback = format!("%{first_var_id}");
        let second_name_fallback = format!("%{second_var_id}");
        let first_name_display = first_name.as_deref().unwrap_or(&first_name_fallback);
        let second_name_display = second_name.as_deref().unwrap_or(&second_name_fallback);

        if let (Some(span1), Some(span2)) = (first_span, second_span) {
            // Rich error with source spans
            let mut err = self.sess.dcx().struct_span_err(
                span2,
                format!(
                    "{storage_class:?} variable `{second_name_display}` at location {location} conflicts with another variable"
                ),
            );

            err.span_note(
                span1,
                format!(
                    "variable `{first_name_display}` already uses location {location} component {component}"
                ),
            );

            // Add hint if the first variable spans multiple locations
            if let Some(start_loc) = first_var_start_location
                && start_loc != location
            {
                err.help(Self::format_multi_location_hint(
                    first_name_display,
                    start_loc,
                    first_var_type_info.as_ref(),
                ));
            }

            err.note(format!("module `{}`", self.filename.display()));
            err.emit();
        } else {
            // Fall back to non-span error
            let mut err = self.sess.dcx().struct_err(format!(
                "{storage_class:?} variable `{second_name_display}` at location {location} conflicts with `{first_name_display}`"
            ));
            err.note(format!("module `{}`", self.filename.display()));
            err.note(format!(
                "variables `{first_name_display}` and `{second_name_display}` both use location {location} component {component}"
            ));
            err.emit();
        }
    }

    fn emit_invalid_block_layout(&mut self, struct_type_id: u32, reason: &str) {
        let span = self.id_to_span(struct_type_id);
        let type_name = self.get_name(struct_type_id);
        let type_name_fallback = format!("%{struct_type_id}");
        let type_name_display = type_name.as_deref().unwrap_or(&type_name_fallback);

        let message = format!("struct `{type_name_display}` has invalid block layout: {reason}");

        let mut err = if let Some(span) = span {
            self.sess.dcx().struct_span_err(span, message)
        } else {
            self.sess.dcx().struct_err(message)
        };

        err.help(
            "ensure struct members are properly aligned according to std140/std430 layout rules",
        );
        err.note(format!("module `{}`", self.filename.display()));
        err.emit();
    }

    fn emit_invalid_builtin_type(&self, builtin: BuiltIn, expected: &str) {
        let mut err = self.sess.dcx().struct_err(format!(
            "BuiltIn {builtin:?} has incorrect type, expected {expected}"
        ));

        err.help(format!(
            "variables with `#[spirv(builtin = \"{builtin:?}\")]` must have the correct type"
        ));
        err.note(format!("module `{}`", self.filename.display()));
        err.emit();
    }

    fn emit_missing_decoration(&mut self, var_id: u32, decoration_name: &str, hint: &str) {
        let span = self.id_to_span(var_id);
        let var_name = self.get_name(var_id);
        let var_name_fallback = format!("%{var_id}");
        let var_name_display = var_name.as_deref().unwrap_or(&var_name_fallback);

        let message = format!(
            "resource variable `{var_name_display}` is missing {decoration_name} decoration"
        );

        let mut err = if let Some(span) = span {
            self.sess.dcx().struct_span_err(span, message)
        } else {
            self.sess.dcx().struct_err(message)
        };

        err.help(format!("add `{hint}` to the variable"));
        err.note(format!("module `{}`", self.filename.display()));
        err.emit();
    }

    fn emit_missing_capability(
        &mut self,
        opcode: Op,
        capability: Capability,
        operand_index: Option<usize>,
    ) {
        // Try to find an instruction with this opcode to get a span
        let span = self.find_instruction_span_by_opcode(opcode);

        let message = if let Some(idx) = operand_index {
            format!("operand {idx} of Op{opcode:?} requires capability {capability:?}")
        } else {
            format!("Op{opcode:?} requires capability {capability:?}")
        };

        let mut err = if let Some(span) = span {
            self.sess.dcx().struct_span_err(span, message)
        } else {
            self.sess.dcx().struct_err(message)
        };

        err.help(format!(
            "add `#[spirv(capability({capability:?}))]` to your entry point function"
        ));
        err.note(format!("module `{}`", self.filename.display()));
        err.emit();
    }

    fn emit_not_a_logical_pointer(&mut self, instruction: Op, pointer_id: u32, source_opcode: Op) {
        // Try to find a useful span - first the pointer, then the instruction using it,
        // then trace back through the def chain
        let span = self.id_to_span(pointer_id).or_else(|| {
            // Try to find the load/store instruction that uses this pointer
            self.find_instruction_using_pointer(pointer_id, instruction)
                .and_then(|inst_id| self.id_to_span(inst_id))
        });

        let pointer_name = self.get_name(pointer_id);
        let pointer_name_fallback = format!("%{pointer_id}");
        let pointer_name_display = pointer_name.as_deref().unwrap_or(&pointer_name_fallback);

        // Get SPIR-V context showing the relevant instructions
        let spirv_context = self.get_spirv_context_for_pointer(pointer_id, source_opcode);

        let message = format!(
            "Op{instruction:?} cannot use pointer `{pointer_name_display}` because it was produced by Op{source_opcode:?}"
        );

        let mut err = if let Some(span) = span {
            self.sess.dcx().struct_span_err(span, message)
        } else {
            self.sess.dcx().struct_err(message)
        };

        err.note(format!(
            "in SPIR-V's logical addressing mode, pointers for Op{instruction:?} must come from \
             specific instructions like OpVariable, OpAccessChain, or OpFunctionParameter"
        ));
        err.help(format!(
            "Op{source_opcode:?} cannot produce pointers valid for memory operations in logical addressing"
        ));

        // Show SPIR-V context if available
        if let Some(context) = spirv_context {
            err.note(format!("generated SPIR-V:\n{context}"));
        }

        err.note(format!("module `{}`", self.filename.display()));
        err.emit();
    }

    /// Finds the first instruction with the given opcode and returns its span.
    fn find_instruction_span_by_opcode(&mut self, opcode: Op) -> Option<rustc_span::Span> {
        let m = self.module?;

        // Search in functions first (most common case)
        for func in &m.functions {
            for block in &func.blocks {
                for inst in &block.instructions {
                    if inst.class.opcode == opcode
                        && let Some(id) = inst.result_id
                        && let Some(span) = self.id_to_span(id)
                    {
                        return Some(span);
                    }
                }
            }
        }

        // Search in types/globals
        for inst in &m.types_global_values {
            if inst.class.opcode == opcode
                && let Some(id) = inst.result_id
                && let Some(span) = self.id_to_span(id)
            {
                return Some(span);
            }
        }

        None
    }

    /// Finds an instruction that uses the given pointer ID with the specified opcode.
    fn find_instruction_using_pointer(&self, pointer_id: u32, opcode: Op) -> Option<u32> {
        let m = self.module?;
        for func in &m.functions {
            for block in &func.blocks {
                for inst in &block.instructions {
                    if inst.class.opcode == opcode {
                        // Check if any operand references our pointer
                        for operand in &inst.operands {
                            if let rspirv::dr::Operand::IdRef(id) = operand
                                && *id == pointer_id
                            {
                                return inst.result_id;
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Gets SPIR-V context showing the pointer-producing instruction and its use.
    fn get_spirv_context_for_pointer(&self, pointer_id: u32, source_opcode: Op) -> Option<String> {
        let m = self.module?;

        // Find the instruction that produced the pointer
        let mut context_lines = Vec::new();

        for func in &m.functions {
            for block in &func.blocks {
                for inst in &block.instructions {
                    // Found the instruction that produced the pointer
                    if inst.result_id == Some(pointer_id) && inst.class.opcode == source_opcode {
                        context_lines
                            .push(format!("       %{pointer_id} = Op{source_opcode:?} ..."));
                    }
                    // Found instructions using the pointer
                    if matches!(inst.class.opcode, Op::Load | Op::Store) {
                        for operand in &inst.operands {
                            if let rspirv::dr::Operand::IdRef(id) = operand
                                && *id == pointer_id
                            {
                                let result = inst
                                    .result_id
                                    .map(|r| format!("%{r} = "))
                                    .unwrap_or_default();
                                context_lines.push(format!(
                                    "    -> {result}Op{:?} %{pointer_id} ...",
                                    inst.class.opcode
                                ));
                            }
                        }
                    }
                }
            }
        }

        if context_lines.is_empty() {
            None
        } else {
            Some(context_lines.join("\n"))
        }
    }

    /// Looks up the rustc Span for a SPIR-V ID.
    fn id_to_span(&mut self, id: u32) -> Option<rustc_span::Span> {
        self.span_regen.as_mut().and_then(|sr| {
            sr.src_loc_for_id(id)
                .and_then(|src_loc| sr.src_loc_to_rustc(src_loc))
        })
    }

    /// Gets the name of a SPIR-V ID from `OpName`.
    fn get_name(&self, id: u32) -> Option<String> {
        self.module.and_then(|m| {
            m.debug_names.iter().find_map(|inst| {
                if inst.class.opcode == Op::Name && inst.operands.first()?.unwrap_id_ref() == id {
                    Some(inst.operands.get(1)?.unwrap_literal_string().to_string())
                } else {
                    None
                }
            })
        })
    }

    /// Gets the Location decoration value for a variable.
    fn get_location_decoration(&self, var_id: u32) -> Option<u32> {
        self.module.and_then(|m| {
            m.annotations.iter().find_map(|inst| {
                if inst.class.opcode == Op::Decorate
                    && inst.operands.first()?.unwrap_id_ref() == var_id
                    && inst.operands.get(1)?.unwrap_decoration() == Decoration::Location
                {
                    Some(inst.operands.get(2)?.unwrap_literal_bit32())
                } else {
                    None
                }
            })
        })
    }

    /// Gets type information for a variable (type name and location count).
    fn get_variable_type_info(&self, var_id: u32) -> Option<TypeInfo> {
        let m = self.module?;

        // Find the OpVariable
        let var_inst = m
            .types_global_values
            .iter()
            .find(|inst| inst.class.opcode == Op::Variable && inst.result_id == Some(var_id))?;

        // Get the pointer type, then the pointee type
        let ptr_type_id = var_inst.result_type?;
        let ptr_type = m
            .types_global_values
            .iter()
            .find(|inst| inst.result_id == Some(ptr_type_id))?;
        let pointee_type_id = ptr_type.operands.get(1)?.unwrap_id_ref();

        // Look up the type name
        let type_name = m.debug_names.iter().find_map(|inst| {
            if inst.class.opcode == Op::Name
                && inst.operands.first()?.unwrap_id_ref() == pointee_type_id
            {
                Some(inst.operands.get(1)?.unwrap_literal_string().to_string())
            } else {
                None
            }
        });

        // Compute location count
        let location_count = Self::count_type_locations(m, pointee_type_id);

        Some(TypeInfo {
            name: type_name,
            location_count,
        })
    }

    /// Counts the number of interface locations consumed by a type.
    fn count_type_locations(m: &Module, type_id: u32) -> Option<u32> {
        let type_inst = m
            .types_global_values
            .iter()
            .find(|inst| inst.result_id == Some(type_id))?;

        match type_inst.class.opcode {
            // Scalars and vectors use 1 location
            Op::TypeBool | Op::TypeInt | Op::TypeFloat | Op::TypeVector => Some(1),

            // Matrices use 1 location per column
            Op::TypeMatrix => {
                let column_count = type_inst.operands.get(1)?.unwrap_literal_bit32();
                Some(column_count)
            }

            // Arrays: element_locations * array_length
            Op::TypeArray => {
                let element_type_id = type_inst.operands.first()?.unwrap_id_ref();
                let length_id = type_inst.operands.get(1)?.unwrap_id_ref();
                let length_inst = m
                    .types_global_values
                    .iter()
                    .find(|inst| inst.result_id == Some(length_id))?;
                let length = length_inst.operands.first()?.unwrap_literal_bit32();
                let element_locs = Self::count_type_locations(m, element_type_id)?;
                Some(element_locs * length)
            }

            // Structs: sum of member locations
            Op::TypeStruct => {
                let mut total = 0u32;
                for operand in &type_inst.operands {
                    let member_type_id = operand.unwrap_id_ref();
                    total += Self::count_type_locations(m, member_type_id)?;
                }
                Some(total)
            }

            _ => None,
        }
    }

    fn format_multi_location_hint(
        var_name: &str,
        start_loc: u32,
        type_info: Option<&TypeInfo>,
    ) -> String {
        let (type_name, location_count) =
            type_info.map_or((None, None), |ti| (ti.name.as_deref(), ti.location_count));

        match (type_name, location_count) {
            (Some(name), Some(count)) => {
                format!(
                    "`{var_name}` is at location {start_loc} but type `{name}` consumes {count} locations ({}–{})",
                    start_loc,
                    start_loc + count - 1
                )
            }
            (Some(name), None) => {
                format!(
                    "`{var_name}` is at location {start_loc} but type `{name}` consumes multiple locations"
                )
            }
            (None, Some(count)) => {
                format!(
                    "`{var_name}` is at location {start_loc} but its type consumes {count} locations ({}–{})",
                    start_loc,
                    start_loc + count - 1
                )
            }
            (None, None) => {
                format!(
                    "`{var_name}` is at location {start_loc} but its type consumes multiple locations"
                )
            }
        }
    }
}

/// Information about a SPIR-V type for error reporting.
struct TypeInfo {
    /// The type name from `OpName`, if available.
    name: Option<String>,
    /// The number of interface locations consumed by this type.
    location_count: Option<u32>,
}
