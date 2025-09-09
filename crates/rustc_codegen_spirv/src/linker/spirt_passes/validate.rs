use rustc_data_structures::fx::{FxHashMap, FxHashSet, FxIndexSet};
use spirt::func_at::FuncAtMut;
use spirt::transform::{InnerInPlaceTransform, InnerTransform, Transformed, Transformer};
use spirt::{
    Const, ConstDef, ConstKind, Context, DataInstKind, Diag, Func, GlobalVar, Module,
    ModuleDialect, Node, NodeKind, Type, TypeDef, TypeKind, cf, scalar, spv,
};
use std::collections::VecDeque;

pub fn validate(module: &mut Module) {
    let spv_spec = super::SpvSpecWithExtras::get();
    let wk = &spv_spec.well_known;

    let mut validator = Validator {
        cx: &module.cx(),
        wk,
        spv_spec_caps: match wk.Capability.def() {
            spv::spec::OperandKindDef::ValueEnum { variants } => variants,
            _ => unreachable!(),
        },

        module_spv_dialect: match &module.dialect {
            ModuleDialect::Spv(dialect) => dialect,
        },

        transformed_types: FxHashMap::default(),
        transformed_consts: FxHashMap::default(),
        seen_global_vars: FxHashSet::default(),
        global_var_queue: VecDeque::new(),
        seen_funcs: FxHashSet::default(),
        func_queue: VecDeque::new(),
    };

    // Seed the queues starting from the module exports.
    for exportee in module.exports.values_mut() {
        exportee
            .inner_transform_with(&mut validator)
            .apply_to(exportee);
    }

    // Process the queues until they're all empty.
    while !validator.global_var_queue.is_empty() || !validator.func_queue.is_empty() {
        while let Some(gv) = validator.global_var_queue.pop_front() {
            validator.in_place_transform_global_var_decl(&mut module.global_vars[gv]);
        }
        while let Some(func) = validator.func_queue.pop_front() {
            validator.in_place_transform_func_decl(&mut module.funcs[func]);
        }
    }
}

struct Validator<'a> {
    cx: &'a Context,
    wk: &'static super::SpvWellKnownWithExtras,
    spv_spec_caps: &'static spv::spec::indexed::NamedIdxMap<
        u16,
        spv::spec::Enumerant,
        spv::spec::indexed::KhrSegmented,
    >,

    module_spv_dialect: &'a spv::Dialect,

    // FIXME(eddyb) build some automation to avoid ever repeating these.
    transformed_types: FxHashMap<Type, Transformed<Type>>,
    transformed_consts: FxHashMap<Const, Transformed<Const>>,
    seen_global_vars: FxHashSet<GlobalVar>,
    global_var_queue: VecDeque<GlobalVar>,
    seen_funcs: FxHashSet<Func>,
    func_queue: VecDeque<Func>,
}

impl Transformer for Validator<'_> {
    // FIXME(eddyb) build some automation to avoid ever repeating these.
    fn transform_type_use(&mut self, ty: Type) -> Transformed<Type> {
        if let Some(&cached) = self.transformed_types.get(&ty) {
            return cached;
        }
        let transformed = self
            .transform_type_def(&self.cx[ty])
            .map(|ty_def| self.cx.intern(ty_def));
        self.transformed_types.insert(ty, transformed);
        transformed
    }
    fn transform_const_use(&mut self, ct: Const) -> Transformed<Const> {
        if let Some(&cached) = self.transformed_consts.get(&ct) {
            return cached;
        }
        let transformed = self
            .transform_const_def(&self.cx[ct])
            .map(|ct_def| self.cx.intern(ct_def));
        self.transformed_consts.insert(ct, transformed);
        transformed
    }

    fn transform_global_var_use(&mut self, gv: GlobalVar) -> Transformed<GlobalVar> {
        if self.seen_global_vars.insert(gv) {
            self.global_var_queue.push_back(gv);
        }
        Transformed::Unchanged
    }
    fn transform_func_use(&mut self, func: Func) -> Transformed<Func> {
        if self.seen_funcs.insert(func) {
            self.func_queue.push_back(func);
        }
        Transformed::Unchanged
    }

    // NOTE(eddyb) above methods are plumbing, validation methods are below.

    fn transform_type_def(&mut self, ty_def: &TypeDef) -> Transformed<TypeDef> {
        let valid = match &ty_def.kind {
            TypeKind::Scalar(ty) => {
                // HACK(eddyb) even if this seems wasteful in its allocation of
                // strings, they should only happen once each per module, and
                // also it wouldn't be hard to switch to some "small str" crate.
                let check = |type_prefix: &str, cap_prefix: &str| {
                    let width = ty.bit_width();
                    if width == 32 {
                        return Ok(());
                    }

                    let cap_name = format!("{cap_prefix}{width}");
                    let cap = self.spv_spec_caps.lookup(&cap_name).map(u32::from);
                    if cap.is_some_and(|cap| self.module_spv_dialect.capabilities.contains(&cap)) {
                        return Ok(());
                    }

                    // FIXME(eddyb) find a consistent style between all the error messages
                    // (mentioning `OpCapability` seems unfortunate, for example).
                    let type_name = format!("{type_prefix}{width}");
                    let msg = match cap {
                        None => format!("`{type_name}` type unsupported in SPIR-V"),
                        Some(_) => {
                            format!("`{type_name}` type used without `OpCapability {cap_name}`")
                        }
                    };
                    Err(Diag::err([msg.into()]))
                };
                match ty {
                    scalar::Type::Bool => Ok(()),
                    scalar::Type::SInt(_) => check("i", "Int"),
                    scalar::Type::UInt(_) => check("u", "Int"),
                    scalar::Type::Float(_) => check("f", "Float"),
                }
            }
            TypeKind::Vector(ty) => {
                // FIXME(eddyb) consider checking the element count against the
                // list of supported values (and extensions for some of them).
                let _elem_count = ty.elem_count;
                Ok(())
            }

            TypeKind::SpvInst {
                spv_inst,
                type_and_const_inputs: _,
            } => self.validate_spv_inst(spv_inst),

            TypeKind::Thunk | TypeKind::QPtr | TypeKind::SpvStringLiteralForExtInst => Ok(()),
        };
        let transformed = ty_def.inner_transform_with(self);
        match valid {
            Ok(()) => transformed,
            Err(diag) => {
                let mut ty_def = TypeDef {
                    attrs: ty_def.attrs,
                    kind: ty_def.kind.clone(),
                };
                transformed.apply_to(&mut ty_def);
                ty_def.attrs.push_diag(self.cx, diag);
                Transformed::Changed(ty_def)
            }
        }
    }

    fn transform_const_def(&mut self, ct_def: &ConstDef) -> Transformed<ConstDef> {
        let valid = match &ct_def.kind {
            ConstKind::SpvInst {
                spv_inst_and_const_inputs,
            } => {
                let (spv_inst, _const_inputs) = &**spv_inst_and_const_inputs;
                self.validate_spv_inst(spv_inst)
            }

            // HACK(eddyb) check against the equivalent SPIR-V instruction.
            ConstKind::PtrToFunc(_) => {
                self.validate_spv_inst(&self.wk.OpConstantFunctionPointerINTEL.into())
            }

            ConstKind::Undef
            | ConstKind::Scalar(_)
            | ConstKind::Vector(_)
            | ConstKind::PtrToGlobalVar(_)
            | ConstKind::SpvStringLiteralForExtInst(_) => Ok(()),
        };
        let transformed = ct_def.inner_transform_with(self);
        match valid {
            Ok(()) => transformed,
            Err(diag) => {
                let mut ct_def = ConstDef {
                    attrs: ct_def.attrs,
                    ty: ct_def.ty,
                    kind: ct_def.kind.clone(),
                };
                transformed.apply_to(&mut ct_def);
                ct_def.attrs.push_diag(self.cx, diag);
                Transformed::Changed(ct_def)
            }
        }
    }

    fn in_place_transform_node_def(&mut self, mut func_at_node: FuncAtMut<'_, Node>) {
        func_at_node.reborrow().inner_in_place_transform_with(self);

        let node_def = func_at_node.def();
        let valid = match &node_def.kind {
            NodeKind::ExitInvocation(cf::ExitInvocationKind::SpvInst(spv_inst))
            | DataInstKind::SpvInst(spv_inst) => self.validate_spv_inst(spv_inst),

            NodeKind::Select(_)
            | NodeKind::Loop { .. }
            | DataInstKind::Scalar(_)
            | DataInstKind::Vector(_)
            | DataInstKind::FuncCall(_)
            | DataInstKind::Mem(_)
            | DataInstKind::QPtr(_)
            | DataInstKind::ThunkBind(_)
            | DataInstKind::SpvExtInst { .. } => Ok(()),
        };
        if let Err(diag) = valid {
            node_def.attrs.push_diag(self.cx, diag);
        }
    }
}

impl Validator<'_> {
    fn require_spv_exts_caps<'a>(
        &self,
        describe: impl FnOnce() -> String,
        exts_providing: impl ExactSizeIterator<Item = &'a str> + Clone,
        caps_enabling: impl ExactSizeIterator<Item = u32> + Clone,
    ) -> Result<(), Diag> {
        // FIXME(eddyb) find a consistent style between all the error messages.
        let provided_by_core_spv_or_ext = exts_providing.len() == 0
            || exts_providing.clone().any(|ext| {
                self.module_spv_dialect.extensions.contains(ext)
                    || min_spv_version_implying_ext(ext).is_some_and(|min_version| {
                        let module_version = {
                            let d = &self.module_spv_dialect;
                            (d.version_major, d.version_minor)
                        };
                        module_version >= min_version
                    })
            });
        if !provided_by_core_spv_or_ext {
            let exts = exts_providing
                .map(|ext| format!("`{ext}`"))
                .collect::<Vec<_>>()
                .join(", ");
            return Err(Diag::err([
                describe().into(),
                format!(" requires one of these extensions: {exts}",).into(),
            ]));
        }

        let enabled_by_default_or_cap = caps_enabling.len() == 0
            || caps_enabling
                .clone()
                .any(|cap| self.module_spv_dialect.capabilities.contains(&cap))
            || {
                // HACK(eddyb) this is an expensive fallback, and should be
                // precomputed ahead of time, but `spirt::spv::spec` exposing
                // the necessary information is the bigger issue and should be
                // solved first, before optimizing any of this.
                let mut elabored_module_caps: FxIndexSet<_> = self
                    .module_spv_dialect
                    .capabilities
                    .iter()
                    .copied()
                    .collect();
                let mut i = 0;
                while i < elabored_module_caps.len() {
                    let cap = elabored_module_caps[i];
                    if let Some(cap) = rspirv::spirv::Capability::from_u32(cap) {
                        elabored_module_caps.extend(
                            rspirv::dr::Operand::from(cap)
                                .required_capabilities()
                                .into_iter()
                                .map(|cap| cap as u32),
                        );
                    }
                    i += 1;
                }
                caps_enabling
                    .clone()
                    .any(|cap| elabored_module_caps.contains(&cap))
            };
        if !enabled_by_default_or_cap {
            let caps = caps_enabling
                .map(|cap| {
                    let cap_name = u16::try_from(cap)
                        .ok()
                        .and_then(|cap| Some(self.spv_spec_caps.get_named(cap)?.0));
                    match cap_name {
                        Some(name) => format!("`{name}`"),
                        None => format!("<unknown Capability 0x{cap:04x}>"),
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            return Err(Diag::err([
                describe().into(),
                format!(" requires one of these capabilities: {caps}",).into(),
            ]));
        }
        Ok(())
    }

    fn validate_spv_inst(&self, spv_inst: &spv::Inst) -> Result<(), Diag> {
        // FIXME(eddyb) make this information available through `spirt::spv::spec`.
        let (exts_providing_inst, caps_enabling_insts) = {
            let inst_def =
                rspirv::grammar::CoreInstructionTable::lookup_opcode(spv_inst.opcode.as_u16())
                    .unwrap();
            (
                inst_def.extensions.iter().copied(),
                inst_def.capabilities.iter().map(|&cap| cap as u32),
            )
        };
        self.require_spv_exts_caps(
            || format!("SPIR-V `{}` instruction", spv_inst.opcode.name()),
            exts_providing_inst,
            caps_enabling_insts,
        )?;

        // FIXME(eddyb) implement this after exposing enough of the information
        // via `spirt::spv::spec` (will likely need some way to efficiently store
        // `(extensions, capabilities)`, e.g. ad-hoc interning into one integer,
        // because that will have to be added to every single e.g. `Enumerant`).
        for &imm in &spv_inst.imms {
            // HACK(eddyb) two simple cases being handled via `rspirv` as a demo.
            let check_enum_via_rspirv = |enum_kind: spv::spec::OperandKind,
                                         // HACK(eddyb) bypassing rustfmt failure mode.
                                         #[allow(unused_parens)] as_rspirv_operand: (
                                             fn(u32) -> Option<rspirv::dr::Operand>
                                         )| {
                if let spv::Imm::Short(kind, imm) = imm
                    && kind == enum_kind
                    && let Some(operand) = as_rspirv_operand(imm)
                {
                    self.require_spv_exts_caps(
                        || {
                            let (enum_name, enum_def) = kind.name_and_def();
                            let enumerant_name = match enum_def {
                                spv::spec::OperandKindDef::ValueEnum { variants } => {
                                    u16::try_from(imm)
                                        .ok()
                                        .and_then(|imm| Some(variants.get_named(imm)?.0))
                                }
                                _ => None,
                            };
                            match enumerant_name {
                                Some(name) => format!("SPIR-V `{enum_name}.{name}` operand"),
                                None => format!("<unknown {enum_name} 0x{imm:04x}>"),
                            }
                        },
                        operand.required_extensions().iter().copied(),
                        operand
                            .required_capabilities()
                            .iter()
                            .map(|&cap| cap as u32),
                    )?;
                }

                Ok(())
            };
            check_enum_via_rspirv(self.wk.ImageFormat, |imm| {
                Some(rspirv::spirv::ImageFormat::from_u32(imm)?.into())
            })?;
            check_enum_via_rspirv(self.wk.StorageClass, |imm| {
                Some(rspirv::spirv::StorageClass::from_u32(imm)?.into())
            })?;
        }

        Ok(())
    }
}

// HACK(eddyb) due to neither SPIR-T, nor `rspirv`, exposing the SPIR-V version
// which started provided an instruction/enumerator/etc. (i.e. supplanting the
// need for an extension), this "first SPIR-V version incorporating an extension"
// approximation was obtained by running (inside `spirt 0.4.0`'s source tree):
//
// jq -r '[(.instructions[], (.operand_kinds[] | (.enumerants//[])[])) \
//   | {e:.extensions|arrays|select(length>0)|unique,v:.version}] as $all \
//   | [$all[].e[]] | unique | map(\
//     . as $e | $all | map(select(.e|contains([$e])) | (.v//"None")) \
//       | max | select(. != "None") | {e:$e,v:(split(".") | map(tonumber))}\
//   ) \
//   | group_by(.v) | map({e:map(@json"\(.e)")|join("\n| "), v:.[0].v})[] \
//   | "\(.e) => (\(.v[0]), \(.v[1])),"' \
//   khronos-spec/SPIRV-Headers/include/spirv/unified1/spirv.core.grammar.json
//
fn min_spv_version_implying_ext(ext: &str) -> Option<(u8, u8)> {
    Some(match ext {
        "SPV_KHR_16bit_storage"
        | "SPV_KHR_device_group"
        | "SPV_KHR_multiview"
        | "SPV_KHR_shader_draw_parameters"
        | "SPV_KHR_storage_buffer_storage_class"
        | "SPV_KHR_variable_pointers" => (1, 3),
        "SPV_GOOGLE_decorate_string" | "SPV_KHR_no_integer_wrap_decoration" => (1, 4),
        "SPV_EXT_descriptor_indexing"
        | "SPV_EXT_physical_storage_buffer"
        | "SPV_KHR_8bit_storage"
        | "SPV_KHR_physical_storage_buffer"
        | "SPV_KHR_vulkan_memory_model" => (1, 5),
        "SPV_KHR_integer_dot_product" | "SPV_KHR_terminate_invocation" => (1, 6),
        _ => return None,
    })
}
