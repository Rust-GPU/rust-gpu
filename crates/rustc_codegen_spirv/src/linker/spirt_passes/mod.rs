//! SPIR-T pass infrastructure and supporting utilities.

pub(crate) mod controlflow;
pub(crate) mod debuginfo;
pub(crate) mod diagnostics;
pub(crate) mod explicit_layout;
mod fuse_selects;
mod reduce;
pub(crate) mod validate;

use lazy_static::lazy_static;
use rustc_data_structures::fx::{FxHashMap, FxHashSet, FxIndexSet};
use smallvec::SmallVec;
use spirt::func_at::FuncAt;
use spirt::transform::InnerInPlaceTransform;
use spirt::visit::{InnerVisit, Visitor};
use spirt::{
    AttrSet, Const, Context, DataInstDef, DataInstKind, DeclDef, EntityOrientedDenseMap, Func,
    FuncDefBody, GlobalVar, Module, Node, NodeKind, Region, Type, Value, spv,
};
use std::collections::VecDeque;
use std::str;

// HACK(eddyb) `spv::spec::Spec` with extra `WellKnown`s (that should be upstreamed).
macro_rules! def_spv_spec_with_extra_well_known {
    ($($group:ident: $ty:ty = [$($entry:ident),+ $(,)?]),+ $(,)?) => {
        struct SpvSpecWithExtras {
            __base_spec: &'static spv::spec::Spec,

            well_known: SpvWellKnownWithExtras,
        }

        #[allow(non_snake_case)]
        pub struct SpvWellKnownWithExtras {
            __base_well_known: &'static spv::spec::WellKnown,

            $($(pub $entry: $ty,)+)+
        }

        impl std::ops::Deref for SpvSpecWithExtras {
            type Target = spv::spec::Spec;
            fn deref(&self) -> &Self::Target {
                self.__base_spec
            }
        }

        impl std::ops::Deref for SpvWellKnownWithExtras {
            type Target = spv::spec::WellKnown;
            fn deref(&self) -> &Self::Target {
                self.__base_well_known
            }
        }

        impl SpvSpecWithExtras {
            #[inline(always)]
            #[must_use]
            pub fn get() -> &'static SpvSpecWithExtras {
                lazy_static! {
                    static ref SPEC: SpvSpecWithExtras = {
                        #[allow(non_camel_case_types)]
                        struct PerWellKnownGroup<$($group),+> {
                            $($group: $group),+
                        }

                        let spv_spec = spv::spec::Spec::get();
                        let wk = &spv_spec.well_known;

                        let [decorations, storage_classes] = [wk.Decoration, wk.StorageClass].map(|kind| match kind.def() {
                            spv::spec::OperandKindDef::ValueEnum { variants } => variants,
                            _ => unreachable!(),
                        });

                        let lookup_fns = PerWellKnownGroup {
                            opcode: |name| spv_spec.instructions.lookup(name).unwrap(),
                            operand_kind: |name| spv_spec.operand_kinds.lookup(name).unwrap(),
                            decoration: |name| decorations.lookup(name).unwrap().into(),
                            storage_class: |name| storage_classes.lookup(name).unwrap().into(),
                        };

                        SpvSpecWithExtras {
                            __base_spec: spv_spec,

                            well_known: SpvWellKnownWithExtras {
                                __base_well_known: &spv_spec.well_known,

                                $($($entry: (lookup_fns.$group)(stringify!($entry)),)+)+
                            },
                        }
                    };
                }
                &SPEC
            }
        }
    };
}
def_spv_spec_with_extra_well_known! {
    opcode: spv::spec::Opcode = [
        OpTypeVoid,

        OpConstantComposite,

        OpBitcast,
        OpCompositeInsert,
        OpCompositeExtract,
        OpCompositeConstruct,

        OpCopyMemory,
    ],
    operand_kind: spv::spec::OperandKind = [
        Capability,
        ExecutionModel,
        ImageFormat,
        MemoryAccess,
    ],
    decoration: u32 = [
        UserTypeGOOGLE,
        MatrixStride,
    ],
    storage_class: u32 = [
        PushConstant,
        Uniform,
        StorageBuffer,
        PhysicalStorageBuffer,
    ],
}

/// Run intra-function passes on all `Func` definitions in the `Module`.
//
// FIXME(eddyb) introduce a proper "pass manager".
// FIXME(eddyb) why does this focus on functions, it could just be module passes??
pub(super) fn run_func_passes<P>(
    module: &mut Module,
    passes: &[impl AsRef<str>],
    // FIXME(eddyb) this is a very poor approximation of a "profiler" abstraction.
    mut before_pass: impl FnMut(&'static str, &Module) -> P,
    mut after_pass: impl FnMut(Option<&Module>, P),
) {
    let cx = &module.cx();

    // FIXME(eddyb) reuse this collection work in some kind of "pass manager".
    let all_funcs = {
        let mut collector = ReachableUseCollector {
            cx,
            module,

            seen_types: FxIndexSet::default(),
            seen_consts: FxIndexSet::default(),
            seen_global_vars: FxIndexSet::default(),
            seen_funcs: FxIndexSet::default(),
        };
        for (export_key, &exportee) in &module.exports {
            export_key.inner_visit_with(&mut collector);
            exportee.inner_visit_with(&mut collector);
        }
        collector.seen_funcs
    };

    for name in passes {
        let name = name.as_ref();

        // HACK(eddyb) not really a function pass.
        if name == "qptr" {
            let layout_config = &spirt::mem::LayoutConfig {
                abstract_bool_size_align: (1, 1),
                logical_ptr_size_align: (4, 4),
                ..spirt::mem::LayoutConfig::VULKAN_SCALAR_LAYOUT
            };

            let profiler = before_pass("qptr::lower_from_spv_ptrs", module);
            spirt::passes::qptr::lower_from_spv_ptrs(module, layout_config);
            after_pass(Some(module), profiler);

            let profiler = before_pass("mem::analyze_accesses", module);
            spirt::passes::qptr::analyze_mem_accesses(module, layout_config);
            after_pass(Some(module), profiler);

            let profiler = before_pass("qptr::lift_to_spv_ptrs", module);
            spirt::passes::qptr::lift_to_spv_ptrs(module, layout_config);
            after_pass(Some(module), profiler);

            continue;
        }

        let (full_name, pass_fn): (_, fn(_, &mut _)) = match name {
            "reduce" => ("spirt_passes::reduce", reduce::reduce_in_func),
            "fuse_selects" => (
                "spirt_passes::fuse_selects",
                fuse_selects::fuse_selects_in_func,
            ),
            _ => panic!("unknown `--spirt-passes={name}`"),
        };

        let profiler = before_pass(full_name, module);
        for &func in &all_funcs {
            if let DeclDef::Present(func_def_body) = &mut module.funcs[func].def {
                pass_fn(cx, func_def_body);

                // FIXME(eddyb) avoid doing this except where changes occurred.
                remove_unused_values_in_func(func_def_body);
            }
        }
        after_pass(Some(module), profiler);
    }
}

fn decode_spv_lit_str_with<R>(imms: &[spv::Imm], f: impl FnOnce(&str) -> R) -> R {
    let wk = &SpvSpecWithExtras::get().well_known;

    // FIXME(eddyb) deduplicate with `spirt::spv::extract_literal_string`.
    let words = imms.iter().enumerate().map(|(i, &imm)| match (i, imm) {
        (0, spirt::spv::Imm::Short(k, w) | spirt::spv::Imm::LongStart(k, w))
        | (1.., spirt::spv::Imm::LongCont(k, w)) => {
            assert_eq!(k, wk.LiteralString);
            w
        }
        _ => unreachable!(),
    });
    let bytes: SmallVec<[u8; 64]> = words
        .flat_map(u32::to_le_bytes)
        .take_while(|&byte| byte != 0)
        .collect();

    f(str::from_utf8(&bytes).expect("invalid UTF-8 in string literal"))
}

// FIXME(eddyb) this is just copy-pasted from `spirt` and should be reusable.
struct ReachableUseCollector<'a> {
    cx: &'a Context,
    module: &'a Module,

    // FIXME(eddyb) build some automation to avoid ever repeating these.
    seen_types: FxIndexSet<Type>,
    seen_consts: FxIndexSet<Const>,
    seen_global_vars: FxIndexSet<GlobalVar>,
    seen_funcs: FxIndexSet<Func>,
}

impl Visitor<'_> for ReachableUseCollector<'_> {
    // FIXME(eddyb) build some automation to avoid ever repeating these.
    fn visit_attr_set_use(&mut self, _attrs: AttrSet) {}
    fn visit_type_use(&mut self, ty: Type) {
        if self.seen_types.insert(ty) {
            self.visit_type_def(&self.cx[ty]);
        }
    }
    fn visit_const_use(&mut self, ct: Const) {
        if self.seen_consts.insert(ct) {
            self.visit_const_def(&self.cx[ct]);
        }
    }

    fn visit_global_var_use(&mut self, gv: GlobalVar) {
        if self.seen_global_vars.insert(gv) {
            self.visit_global_var_decl(&self.module.global_vars[gv]);
        }
    }
    fn visit_func_use(&mut self, func: Func) {
        if self.seen_funcs.insert(func) {
            self.visit_func_decl(&self.module.funcs[func]);
        }
    }
}

// FIXME(eddyb) maybe this should be provided by `spirt::visit`.
struct VisitAllRegionsAndNodes<S, VCR, VCN> {
    state: S,
    visit_region: VCR,
    visit_node: VCN,
}
const _: () = {
    use spirt::{func_at::*, visit::*, *};

    impl<'a, S, VCR: FnMut(&mut S, FuncAt<'a, Region>), VCN: FnMut(&mut S, FuncAt<'a, Node>)>
        Visitor<'a> for VisitAllRegionsAndNodes<S, VCR, VCN>
    {
        // FIXME(eddyb) this is excessive, maybe different kinds of
        // visitors should exist for module-level and func-level?
        fn visit_attr_set_use(&mut self, _: AttrSet) {}
        fn visit_type_use(&mut self, _: Type) {}
        fn visit_const_use(&mut self, _: Const) {}
        fn visit_global_var_use(&mut self, _: GlobalVar) {}
        fn visit_func_use(&mut self, _: Func) {}

        fn visit_region_def(&mut self, func_at_region: FuncAt<'a, Region>) {
            (self.visit_region)(&mut self.state, func_at_region);
            func_at_region.inner_visit_with(self);
        }
        fn visit_node_def(&mut self, func_at_node: FuncAt<'a, Node>) {
            (self.visit_node)(&mut self.state, func_at_node);
            func_at_node.inner_visit_with(self);
        }
    }
};

// FIXME(eddyb) maybe this should be provided by `spirt::transform`.
struct ReplaceValueWith<F>(F);
const _: () = {
    use spirt::{transform::*, *};

    impl<F: FnMut(Value) -> Option<Value>> Transformer for ReplaceValueWith<F> {
        fn transform_value_use(&mut self, v: &Value) -> Transformed<Value> {
            self.0(*v).map_or(Transformed::Unchanged, Transformed::Changed)
        }
    }
};

/// Clean up after a pass by removing unused (pure) `Value` definitions from
/// a function body (both `DataInst`s and `Region` inputs/outputs).
//
// FIXME(eddyb) should this be a dedicated pass?
fn remove_unused_values_in_func(func_def_body: &mut FuncDefBody) {
    // Avoid having to support unstructured control-flow.
    if func_def_body.unstructured_cfg.is_some() {
        return;
    }

    let wk = &SpvSpecWithExtras::get().well_known;

    struct Propagator {
        func_body_region: Region,

        // FIXME(eddyb) maybe this kind of "parent map" should be provided by SPIR-T?
        loop_body_to_loop: EntityOrientedDenseMap<Region, Node>,

        // FIXME(eddyb) entity-keyed dense sets might be better for performance,
        // but would require separate sets/maps for separate `Value` cases.
        used: FxHashSet<Value>,

        queue: VecDeque<Value>,
    }
    impl Propagator {
        fn mark_used(&mut self, v: Value) {
            if let Value::Const(_) = v {
                return;
            }
            if let Value::RegionInput {
                region,
                input_idx: _,
            } = v
                && region == self.func_body_region
            {
                return;
            }
            if self.used.insert(v) {
                self.queue.push_back(v);
            }
        }
        fn propagate_used(&mut self, func: FuncAt<'_, ()>) {
            while let Some(v) = self.queue.pop_front() {
                match v {
                    Value::Const(_) => unreachable!(),
                    Value::RegionInput { region, input_idx } => {
                        let loop_node = self.loop_body_to_loop[region];
                        // NOTE(eddyb) only `Loop`s' bodies can have inputs right now.
                        let initial_inputs = &func.at(loop_node).def().inputs;
                        self.mark_used(initial_inputs[input_idx as usize]);
                        self.mark_used(func.at(region).def().outputs[input_idx as usize]);
                    }
                    Value::NodeOutput { node, output_idx } => {
                        // NOTE(eddyb) only `Select`s can have outputs right now.
                        for &case in &func.at(node).def().child_regions {
                            self.mark_used(func.at(case).def().outputs[output_idx as usize]);
                        }
                    }
                    Value::DataInstOutput { inst, .. } => {
                        for &input in &func.at(inst).def().inputs {
                            self.mark_used(input);
                        }
                    }
                }
            }
        }
    }

    // HACK(eddyb) it's simpler to first ensure `loop_body_to_loop` is computed,
    // just to allow the later unordered propagation to always work.
    let propagator = {
        let mut visitor = VisitAllRegionsAndNodes {
            state: Propagator {
                func_body_region: func_def_body.body,
                loop_body_to_loop: Default::default(),
                used: Default::default(),
                queue: Default::default(),
            },
            visit_region: |_: &mut _, _| {},
            visit_node: |propagator: &mut Propagator, func_at_node: FuncAt<'_, Node>| {
                let node_def = func_at_node.def();
                if let NodeKind::Loop { .. } = node_def.kind {
                    propagator
                        .loop_body_to_loop
                        .insert(node_def.child_regions[0], func_at_node.position);
                }
            },
        };
        func_def_body.inner_visit_with(&mut visitor);
        visitor.state
    };

    // HACK(eddyb) this kind of random-access is easier than using `spirt::transform`.
    let mut all_nodes = vec![];

    let used_values = {
        let mut visitor = VisitAllRegionsAndNodes {
            state: propagator,
            visit_region: |_: &mut _, _| {},
            visit_node: |propagator: &mut Propagator, func_at_node: FuncAt<'_, Node>| {
                all_nodes.push(func_at_node.position);

                let mut mark_used_and_propagate = |v| {
                    propagator.mark_used(v);
                    propagator.propagate_used(func_at_node.at(()));
                };
                let node_def = func_at_node.def();
                match &node_def.kind {
                    &NodeKind::Block { insts } => {
                        for func_at_inst in func_at_node.at(insts) {
                            // Ignore pure instructions (i.e. they're only used
                            // if their output value is used, from somewhere else).
                            if let DataInstKind::SpvInst(spv_inst) = &func_at_inst.def().kind {
                                // HACK(eddyb) small selection relevant for now,
                                // but should be extended using e.g. a bitset.
                                if [wk.OpNop, wk.OpCompositeInsert].contains(&spv_inst.opcode) {
                                    continue;
                                }
                            }
                            mark_used_and_propagate(Value::DataInstOutput {
                                inst: func_at_inst.position,
                                output_idx: 0,
                            });
                        }
                    }

                    &NodeKind::Loop {
                        repeat_condition, ..
                    } => mark_used_and_propagate(repeat_condition),

                    NodeKind::Select { .. }
                    | NodeKind::ExitInvocation(spirt::cf::ExitInvocationKind::SpvInst(_)) => {
                        for &v in &node_def.inputs {
                            mark_used_and_propagate(v);
                        }
                    }

                    DataInstKind::FuncCall(_)
                    | DataInstKind::Mem(_)
                    | DataInstKind::QPtr(_)
                    | DataInstKind::SpvInst(_)
                    | DataInstKind::SpvExtInst { .. } => unreachable!(),
                }
            },
        };
        func_def_body.inner_visit_with(&mut visitor);

        let mut propagator = visitor.state;
        for &v in &func_def_body.at_body().def().outputs {
            propagator.mark_used(v);
            propagator.propagate_used(func_def_body.at(()));
        }

        assert!(propagator.queue.is_empty());
        propagator.used
    };

    // FIXME(eddyb) entity-keyed dense maps might be better for performance,
    // but would require separate maps for separate `Value` cases.
    let mut value_replacements = FxHashMap::default();

    // Remove anything that didn't end up marked as used (directly or indirectly).
    for node in all_nodes {
        let node_def = func_def_body.at(node).def();
        match &node_def.kind {
            &NodeKind::Block { insts } => {
                let mut all_nops = true;
                let mut func_at_inst_iter = func_def_body.at_mut(insts).into_iter();
                while let Some(mut func_at_inst) = func_at_inst_iter.next() {
                    if let DataInstKind::SpvInst(spv_inst) = &func_at_inst.reborrow().def().kind
                        && spv_inst.opcode == wk.OpNop
                    {
                        continue;
                    }
                    if !used_values.contains(&Value::DataInstOutput {
                        inst: func_at_inst.position,
                        output_idx: 0,
                    }) {
                        // Replace the removed `DataInstDef` itself with `OpNop`,
                        // removing the ability to use its "name" as a value.
                        *func_at_inst.def() = DataInstDef {
                            attrs: Default::default(),
                            kind: DataInstKind::SpvInst(wk.OpNop.into()),
                            inputs: [].into_iter().collect(),
                            child_regions: [].into_iter().collect(),
                            outputs: [].into_iter().collect(),
                        };
                        continue;
                    }
                    all_nops = false;
                }
                // HACK(eddyb) because we can't remove list elements yet, we
                // instead replace blocks of `OpNop`s with empty ones.
                if all_nops {
                    func_def_body.at_mut(node).def().kind = NodeKind::Block {
                        insts: Default::default(),
                    };
                }
            }

            NodeKind::Select(_) => {
                // FIXME(eddyb) remove this cloning.
                let cases = node_def.child_regions.clone();

                let mut new_idx = 0;
                for original_idx in 0..node_def.outputs.len() {
                    let original_output = Value::NodeOutput {
                        node,
                        output_idx: original_idx as u32,
                    };

                    if !used_values.contains(&original_output) {
                        // Remove the output definition and corresponding value from all cases.
                        func_def_body.at_mut(node).def().outputs.remove(new_idx);
                        for &case in &cases {
                            func_def_body.at_mut(case).def().outputs.remove(new_idx);
                        }
                        continue;
                    }

                    // Record remappings for any still-used outputs that got "shifted over".
                    if original_idx != new_idx {
                        let new_output = Value::NodeOutput {
                            node,
                            output_idx: new_idx as u32,
                        };
                        value_replacements.insert(original_output, new_output);
                    }
                    new_idx += 1;
                }
            }

            NodeKind::Loop { .. } => {
                let body = node_def.child_regions[0];

                let mut new_idx = 0;
                for original_idx in 0..node_def.inputs.len() {
                    let original_input = Value::RegionInput {
                        region: body,
                        input_idx: original_idx as u32,
                    };

                    if !used_values.contains(&original_input) {
                        // Remove the input definition and corresponding values.
                        func_def_body.at_mut(node).def().inputs.remove(new_idx);
                        let body_def = func_def_body.at_mut(body).def();
                        body_def.inputs.remove(new_idx);
                        body_def.outputs.remove(new_idx);
                        continue;
                    }

                    // Record remappings for any still-used inputs that got "shifted over".
                    if original_idx != new_idx {
                        let new_input = Value::RegionInput {
                            region: body,
                            input_idx: new_idx as u32,
                        };
                        value_replacements.insert(original_input, new_input);
                    }
                    new_idx += 1;
                }
            }

            NodeKind::ExitInvocation { .. } => {}

            DataInstKind::FuncCall(_)
            | DataInstKind::Mem(_)
            | DataInstKind::QPtr(_)
            | DataInstKind::SpvInst(_)
            | DataInstKind::SpvExtInst { .. } => unreachable!(),
        }
    }

    if !value_replacements.is_empty() {
        func_def_body.inner_in_place_transform_with(&mut ReplaceValueWith(|v| match v {
            Value::Const(_) => None,
            _ => value_replacements.get(&v).copied(),
        }));
    }
}
