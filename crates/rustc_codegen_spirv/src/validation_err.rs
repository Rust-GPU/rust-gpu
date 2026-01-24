//! Rich error reporting for SPIR-V validation errors.
//!
//! This module provides utilities for converting SPIR-V validation errors into
//! user-friendly rustc diagnostics with source spans and helpful hints.

use std::path::Path;

use rspirv::dr::Module;
use rspirv::spirv::{Decoration, Op};
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
    pub fn new(
        sess: &'a Session,
        module: Option<&'a Module>,
        filename: &'a Path,
    ) -> Self {
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
            _ => {
                // Fall back to generic error message
                self.emit_generic_error(error);
            }
        }
    }

    /// Emits a generic validation error without rich formatting.
    fn emit_generic_error(&self, error: &spirv_tools::val::ValidatorError) {
        let mut err = self.sess.dcx().struct_err(error.to_string());
        err.note("spirv-val failed");
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

        let first_name_fallback = format!("%{}", first_var_id);
        let second_name_fallback = format!("%{}", second_var_id);
        let first_name_display = first_name.as_deref().unwrap_or(&first_name_fallback);
        let second_name_display = second_name.as_deref().unwrap_or(&second_name_fallback);

        if let (Some(span1), Some(span2)) = (first_span, second_span) {
            // Rich error with source spans
            let mut err = self.sess.dcx().struct_span_err(
                span2,
                format!(
                    "{:?} variable `{}` at location {} conflicts with another variable",
                    storage_class, second_name_display, location
                ),
            );

            err.span_note(
                span1,
                format!(
                    "variable `{}` already uses location {} component {}",
                    first_name_display, location, component
                ),
            );

            // Add hint if the first variable spans multiple locations
            if let Some(start_loc) = first_var_start_location {
                if start_loc != location {
                    err.help(self.format_multi_location_hint(
                        first_name_display,
                        start_loc,
                        first_var_type_info.as_ref(),
                    ));
                }
            }

            err.note("spirv-val failed");
            err.note(format!("module `{}`", self.filename.display()));
            err.emit();
        } else {
            // Fall back to non-span error
            let mut err = self.sess.dcx().struct_err(format!(
                "{:?} variable `{}` at location {} conflicts with `{}`",
                storage_class, second_name_display, location, first_name_display
            ));
            err.note("spirv-val failed");
            err.note(format!("module `{}`", self.filename.display()));
            err.note(format!(
                "variables `{}` and `{}` both use location {} component {}",
                first_name_display, second_name_display, location, component
            ));
            err.emit();
        }
    }

    /// Looks up the rustc Span for a SPIR-V ID.
    fn id_to_span(&mut self, id: u32) -> Option<rustc_span::Span> {
        self.span_regen.as_mut().and_then(|sr| {
            sr.src_loc_for_id(id)
                .and_then(|src_loc| sr.src_loc_to_rustc(src_loc))
        })
    }

    /// Gets the name of a SPIR-V ID from OpName.
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
        let var_inst = m.types_global_values.iter().find(|inst| {
            inst.class.opcode == Op::Variable && inst.result_id == Some(var_id)
        })?;

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
        &self,
        var_name: &str,
        start_loc: u32,
        type_info: Option<&TypeInfo>,
    ) -> String {
        let (type_name, location_count) = type_info
            .map(|ti| (ti.name.as_deref(), ti.location_count))
            .unwrap_or((None, None));

        match (type_name, location_count) {
            (Some(name), Some(count)) => {
                format!(
                    "`{}` is at location {} but type `{}` consumes {} locations ({}–{})",
                    var_name,
                    start_loc,
                    name,
                    count,
                    start_loc,
                    start_loc + count - 1
                )
            }
            (Some(name), None) => {
                format!(
                    "`{}` is at location {} but type `{}` consumes multiple locations",
                    var_name, start_loc, name
                )
            }
            (None, Some(count)) => {
                format!(
                    "`{}` is at location {} but its type consumes {} locations ({}–{})",
                    var_name,
                    start_loc,
                    count,
                    start_loc,
                    start_loc + count - 1
                )
            }
            (None, None) => {
                format!(
                    "`{}` is at location {} but its type consumes multiple locations",
                    var_name, start_loc
                )
            }
        }
    }
}

/// Information about a SPIR-V type for error reporting.
struct TypeInfo {
    /// The type name from OpName, if available.
    name: Option<String>,
    /// The number of interface locations consumed by this type.
    location_count: Option<u32>,
}
