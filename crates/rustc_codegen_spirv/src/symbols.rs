use crate::attr::{IntrinsicType, SpirvAttribute};
use crate::builder::libm_intrinsics;
use rspirv::spirv::{BuiltIn, ExecutionMode, ExecutionModel, StorageClass};
use rustc_data_structures::fx::FxHashMap;
use rustc_span::symbol::Symbol;
use spirv_std_types::spirv_attr_version::spirv_attr_with_version;
use std::rc::Rc;

/// Various places in the codebase (mostly attribute parsing) need to compare rustc Symbols to particular keywords.
/// Symbols are interned, as in, they don't actually store the string itself inside them, but rather an index into a
/// global table of strings. Then, whenever a new Symbol is created, the global table is checked to see if the string
/// already exists, deduplicating it if so. This makes things like comparison and cloning really cheap. So, this struct
/// is to allocate all our keywords up front and intern them all, so we can do comparisons really easily and fast.
pub struct Symbols {
    pub discriminant: Symbol,
    pub rust_gpu: Symbol,
    pub spirv_attr_with_version: Symbol,
    pub vector: Symbol,
    pub v1: Symbol,
    pub libm: Symbol,
    pub entry_point_name: Symbol,
    pub spv_khr_vulkan_memory_model: Symbol,

    pub descriptor_set: Symbol,
    pub binding: Symbol,
    pub input_attachment_index: Symbol,

    pub spec_constant: Symbol,
    pub id: Symbol,
    pub default: Symbol,

    pub attributes: FxHashMap<Symbol, SpirvAttribute>,
    pub execution_modes: FxHashMap<Symbol, (ExecutionMode, ExecutionModeExtraDim)>,
    pub libm_intrinsics: FxHashMap<Symbol, libm_intrinsics::LibmIntrinsic>,
}

const BUILTINS: &[(&str, BuiltIn)] = {
    use BuiltIn::*;
    &[
        ("position", Position),
        ("point_size", PointSize),
        ("clip_distance", ClipDistance),
        ("cull_distance", CullDistance),
        ("vertex_id", VertexId),
        ("instance_id", InstanceId),
        ("primitive_id", PrimitiveId),
        ("invocation_id", InvocationId),
        ("layer", Layer),
        ("viewport_index", ViewportIndex),
        ("tess_level_outer", TessLevelOuter),
        ("tess_level_inner", TessLevelInner),
        ("tess_coord", TessCoord),
        ("patch_vertices", PatchVertices),
        ("frag_coord", FragCoord),
        ("point_coord", PointCoord),
        ("front_facing", FrontFacing),
        ("sample_id", SampleId),
        ("sample_position", SamplePosition),
        ("sample_mask", SampleMask),
        ("frag_depth", FragDepth),
        ("helper_invocation", HelperInvocation),
        ("num_workgroups", NumWorkgroups),
        // ("workgroup_size", WorkgroupSize), -- constant
        ("workgroup_id", WorkgroupId),
        ("local_invocation_id", LocalInvocationId),
        ("global_invocation_id", GlobalInvocationId),
        ("local_invocation_index", LocalInvocationIndex),
        // ("work_dim", WorkDim), -- Kernel-only
        // ("global_size", GlobalSize), -- Kernel-only
        // ("enqueued_workgroup_size", EnqueuedWorkgroupSize), -- Kernel-only
        // ("global_offset", GlobalOffset), -- Kernel-only
        // ("global_linear_id", GlobalLinearId), -- Kernel-only
        ("subgroup_size", SubgroupSize),
        // ("subgroup_max_size", SubgroupMaxSize), -- Kernel-only
        ("num_subgroups", NumSubgroups),
        // ("num_enqueued_subgroups", NumEnqueuedSubgroups), -- Kernel-only
        ("subgroup_id", SubgroupId),
        ("subgroup_local_invocation_id", SubgroupLocalInvocationId),
        ("vertex_index", VertexIndex),
        ("instance_index", InstanceIndex),
        ("subgroup_eq_mask", SubgroupEqMask),
        ("subgroup_ge_mask", SubgroupGeMask),
        ("subgroup_gt_mask", SubgroupGtMask),
        ("subgroup_le_mask", SubgroupLeMask),
        ("subgroup_lt_mask", SubgroupLtMask),
        ("base_vertex", BaseVertex),
        ("base_instance", BaseInstance),
        ("draw_index", DrawIndex),
        ("device_index", DeviceIndex),
        ("view_index", ViewIndex),
        ("bary_coord_no_persp_amd", BaryCoordNoPerspAMD),
        (
            "bary_coord_no_persp_centroid_amd",
            BaryCoordNoPerspCentroidAMD,
        ),
        ("bary_coord_no_persp_sample_amd", BaryCoordNoPerspSampleAMD),
        ("bary_coord_smooth_amd", BaryCoordSmoothAMD),
        ("bary_coord_smooth_centroid_amd", BaryCoordSmoothCentroidAMD),
        ("bary_coord_smooth_sample_amd", BaryCoordSmoothSampleAMD),
        ("bary_coord_pull_model_amd", BaryCoordPullModelAMD),
        ("frag_stencil_ref_ext", FragStencilRefEXT),
        ("viewport_mask_nv", ViewportMaskNV),
        ("secondary_position_nv", SecondaryPositionNV),
        ("secondary_viewport_mask_nv", SecondaryViewportMaskNV),
        ("position_per_view_nv", PositionPerViewNV),
        ("viewport_mask_per_view_nv", ViewportMaskPerViewNV),
        ("fully_covered_ext", FullyCoveredEXT),
        ("task_count_nv", TaskCountNV),
        ("primitive_count_nv", PrimitiveCountNV),
        ("primitive_indices_nv", PrimitiveIndicesNV),
        ("clip_distance_per_view_nv", ClipDistancePerViewNV),
        ("cull_distance_per_view_nv", CullDistancePerViewNV),
        ("layer_per_view_nv", LayerPerViewNV),
        ("mesh_view_count_nv", MeshViewCountNV),
        ("mesh_view_indices_nv", MeshViewIndicesNV),
        ("bary_coord_nv", BuiltIn::BaryCoordNV),
        ("bary_coord_no_persp_nv", BuiltIn::BaryCoordNoPerspNV),
        ("bary_coord", BaryCoordKHR),
        ("bary_coord_no_persp", BaryCoordNoPerspKHR),
        ("primitive_point_indices_ext", PrimitivePointIndicesEXT),
        ("primitive_line_indices_ext", PrimitiveLineIndicesEXT),
        (
            "primitive_triangle_indices_ext",
            PrimitiveTriangleIndicesEXT,
        ),
        ("cull_primitive_ext", CullPrimitiveEXT),
        ("frag_size_ext", FragSizeEXT),
        ("frag_invocation_count_ext", FragInvocationCountEXT),
        ("launch_id", BuiltIn::LaunchIdKHR),
        ("launch_size", BuiltIn::LaunchSizeKHR),
        ("instance_custom_index", BuiltIn::InstanceCustomIndexKHR),
        ("ray_geometry_index", BuiltIn::RayGeometryIndexKHR),
        ("world_ray_origin", BuiltIn::WorldRayOriginKHR),
        ("world_ray_direction", BuiltIn::WorldRayDirectionKHR),
        ("object_ray_origin", BuiltIn::ObjectRayOriginKHR),
        ("object_ray_direction", BuiltIn::ObjectRayDirectionKHR),
        ("ray_tmin", BuiltIn::RayTminKHR),
        ("ray_tmax", BuiltIn::RayTmaxKHR),
        ("object_to_world", BuiltIn::ObjectToWorldKHR),
        ("world_to_object", BuiltIn::WorldToObjectKHR),
        (
            "hit_triangle_vertex_positions",
            BuiltIn::HitTriangleVertexPositionsKHR,
        ),
        ("hit_kind", BuiltIn::HitKindKHR),
        ("incoming_ray_flags", BuiltIn::IncomingRayFlagsKHR),
        ("warps_per_sm_nv", WarpsPerSMNV),
        ("sm_count_nv", SMCountNV),
        ("warp_id_nv", WarpIDNV),
        ("SMIDNV", SMIDNV),
    ]
};

const STORAGE_CLASSES: &[(&str, StorageClass)] = {
    use StorageClass::*;
    &[
        ("uniform_constant", UniformConstant),
        ("input", Input),
        ("uniform", Uniform),
        ("output", Output),
        ("workgroup", Workgroup),
        ("cross_workgroup", CrossWorkgroup),
        ("private", Private),
        ("function", Function),
        ("generic", Generic),
        ("push_constant", PushConstant),
        ("atomic_counter", AtomicCounter),
        ("image", Image),
        ("storage_buffer", StorageBuffer),
        ("callable_data", StorageClass::CallableDataKHR),
        (
            "incoming_callable_data",
            StorageClass::IncomingCallableDataKHR,
        ),
        ("ray_payload", StorageClass::RayPayloadKHR),
        ("hit_attribute", StorageClass::HitAttributeKHR),
        ("incoming_ray_payload", StorageClass::IncomingRayPayloadKHR),
        ("shader_record_buffer", StorageClass::ShaderRecordBufferKHR),
        ("physical_storage_buffer", PhysicalStorageBuffer),
        ("task_payload_workgroup_ext", TaskPayloadWorkgroupEXT),
    ]
};

const EXECUTION_MODELS: &[(&str, ExecutionModel)] = {
    use ExecutionModel::*;
    &[
        ("vertex", Vertex),
        ("tessellation_control", TessellationControl),
        ("tessellation_evaluation", TessellationEvaluation),
        ("geometry", Geometry),
        ("fragment", Fragment),
        ("compute", GLCompute),
        ("task_nv", TaskNV),
        ("mesh_nv", MeshNV),
        ("task_ext", TaskEXT),
        ("mesh_ext", MeshEXT),
        ("ray_generation", ExecutionModel::RayGenerationKHR),
        ("intersection", ExecutionModel::IntersectionKHR),
        ("any_hit", ExecutionModel::AnyHitKHR),
        ("closest_hit", ExecutionModel::ClosestHitKHR),
        ("miss", ExecutionModel::MissKHR),
        ("callable", ExecutionModel::CallableKHR),
    ]
};

#[derive(Copy, Clone, Debug)]
pub enum ExecutionModeExtraDim {
    None,
    Value,
    X,
    Y,
    Z,
    Tuple,
}

const EXECUTION_MODES: &[(&str, ExecutionMode, ExecutionModeExtraDim)] = {
    use ExecutionMode::*;
    use ExecutionModeExtraDim::*;
    &[
        ("invocations", Invocations, Value),
        ("spacing_equal", SpacingEqual, None),
        ("spacing_fraction_even", SpacingFractionalEven, None),
        ("spacing_fraction_odd", SpacingFractionalOdd, None),
        ("vertex_order_cw", VertexOrderCw, None),
        ("vertex_order_ccw", VertexOrderCcw, None),
        ("pixel_center_integer", PixelCenterInteger, None),
        ("origin_upper_left", OriginUpperLeft, None),
        ("origin_lower_left", OriginLowerLeft, None),
        ("early_fragment_tests", EarlyFragmentTests, None),
        ("point_mode", PointMode, None),
        ("xfb", Xfb, None),
        ("depth_replacing", DepthReplacing, None),
        ("depth_greater", DepthGreater, None),
        ("depth_less", DepthLess, None),
        ("depth_unchanged", DepthUnchanged, None),
        ("threads", LocalSize, Tuple),
        ("local_size_hint_x", LocalSizeHint, X),
        ("local_size_hint_y", LocalSizeHint, Y),
        ("local_size_hint_z", LocalSizeHint, Z),
        ("input_points", InputPoints, None),
        ("input_lines", InputLines, None),
        ("input_lines_adjacency", InputLinesAdjacency, None),
        ("triangles", Triangles, None),
        ("input_triangles_adjacency", InputTrianglesAdjacency, None),
        ("quads", Quads, None),
        ("isolines", Isolines, None),
        ("output_vertices", OutputVertices, Value),
        ("output_points", OutputPoints, None),
        ("output_line_strip", OutputLineStrip, None),
        ("output_triangle_strip", OutputTriangleStrip, None),
        ("vec_type_hint", VecTypeHint, Value),
        ("contraction_off", ContractionOff, None),
        ("initializer", Initializer, None),
        ("finalizer", Finalizer, None),
        ("subgroup_size", SubgroupSize, Value),
        ("subgroups_per_workgroup", SubgroupsPerWorkgroup, Value),
        ("subgroups_per_workgroup_id", SubgroupsPerWorkgroupId, Value),
        ("local_size_id_x", LocalSizeId, X),
        ("local_size_id_y", LocalSizeId, Y),
        ("local_size_id_z", LocalSizeId, Z),
        ("local_size_hint_id", LocalSizeHintId, Value),
        ("post_depth_coverage", PostDepthCoverage, None),
        ("denorm_preserve", DenormPreserve, None),
        ("denorm_flush_to_zero", DenormFlushToZero, Value),
        (
            "signed_zero_inf_nan_preserve",
            SignedZeroInfNanPreserve,
            Value,
        ),
        ("rounding_mode_rte", RoundingModeRTE, Value),
        ("rounding_mode_rtz", RoundingModeRTZ, Value),
        ("stencil_ref_replacing_ext", StencilRefReplacingEXT, None),
        ("output_lines_nv", OutputLinesNV, None),
        ("output_primitives_nv", OutputPrimitivesNV, Value),
        ("derivative_group_quads_nv", DerivativeGroupQuadsNV, None),
        ("output_triangles_nv", OutputTrianglesNV, None),
        ("output_lines_ext", ExecutionMode::OutputLinesEXT, None),
        (
            "output_triangles_ext",
            ExecutionMode::OutputTrianglesEXT,
            None,
        ),
        (
            "output_primitives_ext",
            ExecutionMode::OutputPrimitivesEXT,
            Value,
        ),
        (
            "pixel_interlock_ordered_ext",
            PixelInterlockOrderedEXT,
            None,
        ),
        (
            "pixel_interlock_unordered_ext",
            PixelInterlockUnorderedEXT,
            None,
        ),
        (
            "sample_interlock_ordered_ext",
            SampleInterlockOrderedEXT,
            None,
        ),
        (
            "sample_interlock_unordered_ext",
            SampleInterlockUnorderedEXT,
            None,
        ),
        (
            "shading_rate_interlock_ordered_ext",
            ShadingRateInterlockOrderedEXT,
            None,
        ),
        (
            "shading_rate_interlock_unordered_ext",
            ShadingRateInterlockUnorderedEXT,
            None,
        ),
        // Reserved
        /*("max_workgroup_size_intel_x", MaxWorkgroupSizeINTEL, X),
        ("max_workgroup_size_intel_y", MaxWorkgroupSizeINTEL, Y),
        ("max_workgroup_size_intel_z", MaxWorkgroupSizeINTEL, Z),
        ("max_work_dim_intel", MaxWorkDimINTEL, Value),
        ("no_global_offset_intel", NoGlobalOffsetINTEL, None),
        ("num_simd_workitems_intel", NumSIMDWorkitemsINTEL, Value),*/
    ]
};

impl Symbols {
    fn new() -> Self {
        let builtins = BUILTINS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Builtin(b)));
        let storage_classes = STORAGE_CLASSES
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::StorageClass(b)));
        let execution_models = EXECUTION_MODELS
            .iter()
            .map(|&(a, b)| (a, SpirvAttribute::Entry(b.into())));
        let custom_attributes = [
            (
                "sampler",
                SpirvAttribute::IntrinsicType(IntrinsicType::Sampler),
            ),
            (
                "generic_image_type",
                SpirvAttribute::IntrinsicType(IntrinsicType::GenericImageType),
            ),
            (
                "acceleration_structure",
                SpirvAttribute::IntrinsicType(IntrinsicType::AccelerationStructureKhr),
            ),
            (
                "ray_query",
                SpirvAttribute::IntrinsicType(IntrinsicType::RayQueryKhr),
            ),
            ("block", SpirvAttribute::Block),
            ("flat", SpirvAttribute::Flat),
            ("invariant", SpirvAttribute::Invariant),
            ("per_primitive_ext", SpirvAttribute::PerPrimitiveExt),
            (
                "sampled_image",
                SpirvAttribute::IntrinsicType(IntrinsicType::SampledImage),
            ),
            (
                "runtime_array",
                SpirvAttribute::IntrinsicType(IntrinsicType::RuntimeArray),
            ),
            (
                "typed_buffer",
                SpirvAttribute::IntrinsicType(IntrinsicType::TypedBuffer),
            ),
            (
                "matrix",
                SpirvAttribute::IntrinsicType(IntrinsicType::Matrix),
            ),
            (
                "vector",
                SpirvAttribute::IntrinsicType(IntrinsicType::Vector),
            ),
            ("buffer_load_intrinsic", SpirvAttribute::BufferLoadIntrinsic),
            (
                "buffer_store_intrinsic",
                SpirvAttribute::BufferStoreIntrinsic,
            ),
        ]
        .iter()
        .cloned();
        let attributes_iter = builtins
            .chain(storage_classes)
            .chain(execution_models)
            .chain(custom_attributes)
            .map(|(a, b)| (Symbol::intern(a), b));
        let mut attributes = FxHashMap::default();
        for (a, b) in attributes_iter {
            let old = attributes.insert(a, b);
            // `.collect()` into a FxHashMap does not error on duplicates, so manually write out the
            // loop here to error on duplicates.
            assert!(old.is_none());
        }
        let mut execution_modes = FxHashMap::default();
        for &(key, mode, dim) in EXECUTION_MODES {
            let old = execution_modes.insert(Symbol::intern(key), (mode, dim));
            assert!(old.is_none());
        }

        let mut libm_intrinsics = FxHashMap::default();
        for &(a, b) in libm_intrinsics::TABLE {
            let old = libm_intrinsics.insert(Symbol::intern(a), b);
            assert!(old.is_none());
        }
        Self {
            discriminant: Symbol::intern("discriminant"),
            rust_gpu: Symbol::intern("rust_gpu"),
            spirv_attr_with_version: Symbol::intern(&spirv_attr_with_version()),
            vector: Symbol::intern("vector"),
            v1: Symbol::intern("v1"),
            libm: Symbol::intern("libm"),
            entry_point_name: Symbol::intern("entry_point_name"),
            spv_khr_vulkan_memory_model: Symbol::intern("SPV_KHR_vulkan_memory_model"),

            descriptor_set: Symbol::intern("descriptor_set"),
            binding: Symbol::intern("binding"),
            input_attachment_index: Symbol::intern("input_attachment_index"),

            spec_constant: Symbol::intern("spec_constant"),
            id: Symbol::intern("id"),
            default: Symbol::intern("default"),

            attributes,
            execution_modes,
            libm_intrinsics,
        }
    }

    /// Obtain an `Rc` handle to the current thread's `Symbols` instance, which
    /// will be shared between all `Symbols::get()` calls on the same thread.
    ///
    /// While this is relatively cheap, prefer caching it in e.g. `CodegenCx`,
    /// rather than calling `get()` every time a field of `Symbols` is needed.
    pub fn get() -> Rc<Self> {
        thread_local!(static SYMBOLS: Rc<Symbols> = Rc::new(Symbols::new()));
        SYMBOLS.with(Rc::clone)
    }
}
