// HACK(eddyb) avoids rewriting all of the imports (see `lib.rs` and `build.rs`).
use crate::maybe_pqp_cg_ssa as rustc_codegen_ssa;

use crate::builder;
use crate::codegen_cx::CodegenCx;
use crate::spirv_type::SpirvType;
use crate::symbols::Symbols;
use crate::target::SpirvTarget;
use crate::target_feature::TargetFeature;
use rspirv::dr::{Builder, Instruction, Module, Operand};
use rspirv::spirv::{
    AddressingModel, Capability, MemoryModel, Op, SourceLanguage, StorageClass, Word,
};
use rspirv::{binary::Assemble, binary::Disassemble};
use rustc_abi::Size;
use rustc_arena::DroplessArena;
use rustc_codegen_ssa::traits::{BaseTypeCodegenMethods as _, ConstCodegenMethods as _};
use rustc_data_structures::fx::{FxHashMap, FxHashSet};
use rustc_middle::mir::interpret::{AllocId, ConstAllocation};
use rustc_middle::span_bug;
use rustc_middle::ty::TyCtxt;
use rustc_span::source_map::SourceMap;
use rustc_span::symbol::Symbol;
use rustc_span::{DUMMY_SP, FileName, FileNameDisplayPreference, SourceFile, Span};
use std::assert_matches::assert_matches;
use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::hash::{Hash, Hasher};
use std::iter;
use std::ops::Range;
use std::str;
use std::sync::Arc;
use std::{fs::File, io::Write, path::Path};

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SpirvValueKind {
    Def {
        id: Word,

        /// If `id` is a pointer cast, this will be `Some`, and contain all the
        /// information necessary to regenerate the original `SpirvValue` before
        /// *any* pointer casts were applied, effectively deferring the casts
        /// (as long as all downstream uses apply `.strip_ptrcasts()` first),
        /// and bypassing errors they might cause (due to SPIR-V limitations).
        //
        // FIXME(eddyb) wouldn't it be easier to use this for *any* bitcasts?
        // (with some caveats around dedicated int<->ptr casts vs bitcasts)
        original_ptr_before_casts: Option<SpirvValue<Word>>,
    },

    // HACK(eddyb) the result of `const_data_from_alloc`, that can only ever be
    // used as the initializer value for `static_addr_of` (which, in turn, only
    // accepts the result of `const_data_from_alloc`, no real values).
    // FIXME(eddyb) remove after https://github.com/rust-lang/rust/pull/142960
    // (or a similar PR fusing those two methods) lands upstream.
    ConstDataFromAlloc {
        // HACK(eddyb) this isn't `ConstAllocation` because that would require
        // adding a `'tcx` parameter to `SpirvValue`.
        alloc_id: AllocId,
    },
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SpirvValue<K = SpirvValueKind> {
    // HACK(eddyb) used to cheaply check whether this is a SPIR-V value ID
    // with a "zombie" (deferred error) attached to it, that may need a `Span`
    // still (e.g. such as constants, which can't easily take a `Span`).
    // FIXME(eddyb) a whole `bool` field is sadly inefficient, but anything
    // which may make `SpirvValue` smaller requires far too much impl effort.
    pub zombie_waiting_for_span: bool,

    pub kind: K,
    pub ty: Word,
}

impl<K> SpirvValue<K> {
    fn map_kind<K2>(self, f: impl FnOnce(K) -> K2) -> SpirvValue<K2> {
        let SpirvValue {
            zombie_waiting_for_span,
            kind,
            ty,
        } = self;
        SpirvValue {
            zombie_waiting_for_span,
            kind: f(kind),
            ty,
        }
    }
}

impl SpirvValue {
    pub fn strip_ptrcasts(self) -> Self {
        match self.kind {
            SpirvValueKind::Def {
                id: _,
                original_ptr_before_casts: Some(original_ptr),
            } => original_ptr.map_kind(|id| SpirvValueKind::Def {
                id,
                original_ptr_before_casts: None,
            }),

            _ => self,
        }
    }

    pub fn const_fold_load(self, cx: &CodegenCx<'_>) -> Option<Self> {
        match cx.builder.lookup_const(self)? {
            SpirvConst::PtrTo { pointee, .. } => {
                // HACK(eddyb) this obtains a `SpirvValue` from the ID it contains,
                // so there's some conceptual inefficiency there, but it does
                // prevent any of the other details from being lost accidentally.
                Some(cx.builder.id_to_const_and_val.borrow().get(&pointee)?.val.1)
            }
            _ => None,
        }
    }

    // Important: we *cannot* use bx.emit() here, because this is called in
    // contexts where the emitter is already locked. Doing so may cause subtle
    // rare bugs.
    pub fn def(self, bx: &builder::Builder<'_, '_>) -> Word {
        self.def_with_span(bx, bx.span())
    }

    // def and def_cx are separated, because Builder has a span associated with
    // what it's currently emitting.
    pub fn def_cx(self, cx: &CodegenCx<'_>) -> Word {
        self.def_with_span(cx, DUMMY_SP)
    }

    pub fn def_with_span(self, cx: &CodegenCx<'_>, span: Span) -> Word {
        let id = match self.kind {
            SpirvValueKind::Def { id, .. } => id,

            SpirvValueKind::ConstDataFromAlloc { .. } => span_bug!(
                span,
                "`const_data_from_alloc` result should only be passed to `static_addr_of`"
            ),
        };
        if self.zombie_waiting_for_span {
            cx.add_span_to_zombie_if_missing(id, span);
        }
        id
    }
}

pub trait SpirvValueExt {
    fn with_type(self, ty: Word) -> SpirvValue;
}

impl SpirvValueExt for Word {
    fn with_type(self, ty: Word) -> SpirvValue {
        SpirvValue {
            zombie_waiting_for_span: false,
            kind: SpirvValueKind::Def {
                id: self,
                original_ptr_before_casts: None,
            },
            ty,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SpirvConst<'a, 'tcx> {
    /// Constants of boolean, integer or floating-point type (up to 128-bit).
    Scalar(u128),

    Null,
    Undef,

    Composite(&'a [Word]),

    /// Pointer to constant data, i.e. `&pointee`, represented as an `OpVariable`
    /// in the `Private` storage class, and with `pointee` as its initializer.
    PtrTo {
        pointee: Word,

        // HACK(eddyb) this allows deferring the actual value generation until
        // after a pointer to this value is cast to its final pointer type.
        //
        // FIXME(eddyb) replace this with `qptr` handling of constant data.
        pointee_alloc: ConstAllocation<'tcx>,
    },

    /// Pointer to the function with ID `func_id`, i.e. `func as fn(...) -> _`,
    /// represented using `OpConstantFunctionPointerINTEL` and pointer types in
    /// the `CodeSectionINTEL` storage class, from `SPV_INTEL_function_pointers`
    /// (an OpenCL extension, but these consts are meant to be legalized away).
    //
    // FIXME(eddyb) actually support `OpConstantFunctionPointerINTEL` in SPIR-T
    // and emit it (right now this uses `OpUndef` to avoid breaking SPIR-T).
    // HACK(eddyb) silencing `clippy::doc_markdown` due to "OpenCL" false positive.
    #[allow(clippy::doc_markdown)]
    PtrToFunc {
        func_id: Word,

        // HACK(eddyb) tracked only to allow a more useful zombie message.
        // FIXME(eddyb) once zombies are replaced with errors emitted from a
        // SPIR-T legality checker, the name can come from the function itself.
        mangled_func_name: &'tcx str,
    },

    /// Constant `OpBitcast` (via `OpSpecConstantOp`).
    BitCast(Word),

    /// Constant `OpPtrAccessChain` (via `OpSpecConstantOp`).
    PtrByteOffset {
        ptr: Word,
        offset: Size,
    },
}

impl<'tcx> SpirvConst<'_, 'tcx> {
    /// Replace `&[T]` fields with `&'tcx [T]` ones produced by calling
    /// `tcx.arena.dropless.alloc_slice(...)` - this is done late for two reasons:
    /// 1. it avoids allocating in the arena when the cache would be hit anyway,
    ///    which would create "garbage" (as in, unreachable allocations)
    ///    (ideally these would also be interned, but that's even more refactors)
    /// 2. an empty slice is disallowed (as it's usually handled as a special
    ///    case elsewhere, e.g. `rustc`'s `ty::List` - sadly we can't use that)
    fn tcx_arena_alloc_slices(self, cx: &CodegenCx<'tcx>) -> SpirvConst<'tcx, 'tcx> {
        fn arena_alloc_slice<'tcx, T: Copy>(cx: &CodegenCx<'tcx>, xs: &[T]) -> &'tcx [T] {
            if xs.is_empty() {
                &[]
            } else {
                cx.tcx.arena.dropless.alloc_slice(xs)
            }
        }

        match self {
            // FIXME(eddyb) these are all noop cases, could they be automated?
            SpirvConst::Scalar(v) => SpirvConst::Scalar(v),
            SpirvConst::Null => SpirvConst::Null,
            SpirvConst::Undef => SpirvConst::Undef,
            SpirvConst::PtrTo {
                pointee,
                pointee_alloc,
            } => SpirvConst::PtrTo {
                pointee,
                pointee_alloc,
            },
            SpirvConst::PtrToFunc {
                func_id,
                mangled_func_name,
            } => SpirvConst::PtrToFunc {
                func_id,
                mangled_func_name,
            },

            SpirvConst::Composite(fields) => SpirvConst::Composite(arena_alloc_slice(cx, fields)),

            SpirvConst::BitCast(v) => SpirvConst::BitCast(v),
            SpirvConst::PtrByteOffset { ptr, offset } => SpirvConst::PtrByteOffset { ptr, offset },
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct WithType<V> {
    ty: Word,
    val: V,
}

/// Primary causes for a `SpirvConst` to be deemed illegal.
#[derive(Copy, Clone, Debug)]
enum LeafIllegalConst<'tcx> {
    /// `SpirvConst::Composite` containing a `SpirvConst::PtrTo` as a field.
    /// This is illegal because `OpConstantComposite` must have other constants
    /// as its operands, and `OpVariable`s are never considered constant.
    // FIXME(eddyb) figure out if this is an accidental omission in SPIR-V.
    CompositeContainsPtrTo,

    /// `PtrToFunc` constant, which is not legal in SPIR-V (without the extension
    /// `SPV_INTEL_function_pointers`, which is not expected to be realistically
    /// present outside of Intel's OpenCL/SYCL implementation, and which will at
    /// most be used to pass such constants through to SPIR-T for legalization).
    //
    // FIXME(eddyb) legalize function pointers (and emulate recursion) in SPIR-T.
    PtrToFunc { mangled_func_name: &'tcx str },

    /// `SpirvConst::BitCast` needs to error, even when materialized to SPIR-V.
    //
    // FIXME(eddyb) replace this with `qptr` handling of constant data/exprs.
    BitCast { from_ty: Word },

    /// `SpirvConst::PtrByteOffset` needs to error, even when materialized to SPIR-V.
    //
    // FIXME(eddyb) replace this with `qptr` handling of constant data/exprs.
    PtrByteOffset,
}

impl LeafIllegalConst<'_> {
    fn message(&self) -> Cow<'static, str> {
        match *self {
            Self::CompositeContainsPtrTo => {
                "constant arrays/structs cannot contain pointers to other constants".into()
            }
            Self::PtrToFunc { mangled_func_name } => {
                let demangled_func_name =
                    format!("{:#}", rustc_demangle::demangle(mangled_func_name));
                format!("unsupported function pointer to `{demangled_func_name}`").into()
            }
            Self::BitCast { .. } => "constants cannot contain bitcasts".into(),
            Self::PtrByteOffset => {
                "constants cannot contain pointers with arbitrary byte offsets".into()
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum IllegalConst<'tcx> {
    /// This `SpirvConst` is (or contains) a "leaf" illegal constant. As there
    /// is no indirection, some of these could still be materialized at runtime,
    /// using e.g. `OpCompositeConstruct` instead of `OpConstantComposite`.
    Shallow(LeafIllegalConst<'tcx>),

    /// This `SpirvConst` is (or contains/points to) a `PtrTo` which points to
    /// a "leaf" illegal constant. As the data would have to live for `'static`,
    /// there is no way to materialize it as a pointer in SPIR-V. However, it
    /// could still be legalized during codegen by e.g. folding loads from it.
    Indirect(LeafIllegalConst<'tcx>),
}

#[derive(Copy, Clone, Debug)]
struct WithConstLegality<'tcx, V> {
    val: V,
    legal: Result<(), IllegalConst<'tcx>>,
}

/// `HashMap` key type (for `debug_file_cache` in `BuilderSpirv`), which is
/// equivalent to a whole `rustc` `SourceFile`, but has O(1) `Eq` and `Hash`
/// implementations (i.e. not involving the path or the contents of the file).
///
/// This is possible because we can compare `Arc<SourceFile>`s by equality, as
/// `rustc`'s `SourceMap` already ensures that only one `SourceFile` will be
/// allocated for some given file. For hashing, we could hash the address, or
///
struct DebugFileKey(Arc<SourceFile>);

impl PartialEq for DebugFileKey {
    fn eq(&self, other: &Self) -> bool {
        let (Self(self_sf), Self(other_sf)) = (self, other);
        Arc::ptr_eq(self_sf, other_sf)
    }
}
impl Eq for DebugFileKey {}

impl Hash for DebugFileKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self(sf) = self;
        sf.stable_id.hash(state);
        sf.src_hash.hash(state);
    }
}

#[derive(Copy, Clone)]
pub struct DebugFileSpirv<'tcx> {
    pub file_name: &'tcx str,

    /// The SPIR-V ID for the result of the `OpString` instruction containing
    /// `file_name` - this is what e.g. `OpLine` uses to identify the file.
    ///
    /// All other details about the file are also attached to this ID, using
    /// other instructions that don't produce their own IDs (e.g. `OpSource`).
    pub file_name_op_string_id: Word,
}

// HACK(eddyb) unlike a raw SPIR-V ID (or `SpirvValue`), this allows random-access.
#[derive(Copy, Clone, Debug)]
pub struct SpirvFunctionCursor {
    pub ty: Word,
    pub id: Word,
    pub index_in_builder: usize,
}

// HACK(eddyb) unlike a raw SPIR-V ID, this allows random-access.
#[derive(Copy, Clone, Debug)]
pub struct SpirvBlockCursor {
    pub parent_fn: SpirvFunctionCursor,
    pub id: Word,
    pub index_in_builder: usize,
}

/// Cursor system:
///
/// The LLVM module builder model (and therefore `codegen_ssa`) assumes that there is a central
/// module object, then, builder objects are created pointing at that central module object (e.g.
/// for adding instructions to a basic block).  Several of these builder objects can be live at the
/// same time, mutating the central module object all at once.  Unfortunately, rspirv doesn't work
/// like that. Instead, there is a single builder object, which owns a module and a "cursor". This
/// cursor indicates to the builder where to append instructions when an instruction is added -
/// e.g. if `add()` is called, then `OpAdd` is appended to the basic block pointed to by the cursor.
///
/// So! We emulate the LLVM system by treating the rspirv Builder as the "central module object",
/// then, when a "builder object" is created, we store a reference to a `RefCell<rspirv builder>`,
/// *as well as* a copy of the cursor for that particular builder. Whenever the `RefCell` is
/// borrowed, then we stomp over the rspirv cursor with our copy, causing the duration of that
/// `RefCell` borrow to use that cursor.
///
/// So, if you're writing code inside `crate::builder::Builder`, then `self.emit()` will use
/// `self.cursor` (the current basic block) as that "stomp-over" cursor and return a mutable
/// reference to the rspirv builder. If you're writing code elsewhere (`codegen_cx::CodegenCx`),
/// then `self.emit_global()` will use the generic "global cursor" and return a mutable reference
/// to the rspirv builder with no basic block nor function selected, i.e. any instructions emitted
/// will be in the global section.
//
// FIXME(eddyb) try updating documentation like the above.
// FIXME(eddyb) figure out how to replace `BuilderCursor` with something like
// `Option<SpirvBlockCursor>`, but that can't handle "in function outside BB".
#[derive(Debug, Default, Copy, Clone)]
#[must_use = "BuilderCursor should usually be assigned to the Builder.cursor field"]
struct BuilderCursor {
    fn_id_and_idx: Option<(Word, usize)>,
    block_id_and_idx: Option<(Word, usize)>,
}

pub struct BuilderSpirv<'tcx> {
    source_map: &'tcx SourceMap,
    dropless_arena: &'tcx DroplessArena,

    builder: RefCell<Builder>,

    // Bidirectional maps between `SpirvConst` and the ID of the defined global
    // (e.g. `OpConstant...`) instruction, with additional information in values
    // (i.e. each map is keyed by only some part of the other map's value type),
    // as needed to streamline operations (e.g. avoiding rederiving `SpirvValue`).
    const_to_val: RefCell<FxHashMap<WithType<SpirvConst<'tcx, 'tcx>>, SpirvValue>>,
    id_to_const_and_val:
        RefCell<FxHashMap<Word, WithConstLegality<'tcx, (SpirvConst<'tcx, 'tcx>, SpirvValue)>>>,

    debug_file_cache: RefCell<FxHashMap<DebugFileKey, DebugFileSpirv<'tcx>>>,

    enabled_capabilities: FxHashSet<Capability>,
}

impl<'tcx> BuilderSpirv<'tcx> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        sym: &Symbols,
        target: &SpirvTarget,
        features: &[TargetFeature],
    ) -> Self {
        let version = target.spirv_version();
        let memory_model = target.memory_model();

        let mut builder = Builder::new();
        builder.set_version(version.0, version.1);
        builder.module_mut().header.as_mut().unwrap().generator = 0x001B_0000;

        let mut enabled_capabilities = FxHashSet::default();

        fn add_cap(
            builder: &mut Builder,
            enabled_capabilities: &mut FxHashSet<Capability>,
            cap: Capability,
        ) {
            // This should be the only callsite of Builder::capability (aside from tests), to make
            // sure the hashset stays in sync.
            builder.capability(cap);
            enabled_capabilities.insert(cap);
        }
        fn add_ext(builder: &mut Builder, ext: Symbol) {
            // This should be the only callsite of Builder::extension (aside from tests), to make
            // sure the hashset stays in sync.
            builder.extension(ext.as_str());
        }

        for feature in features {
            match *feature {
                TargetFeature::Capability(cap) => {
                    add_cap(&mut builder, &mut enabled_capabilities, cap);
                }
                TargetFeature::Extension(ext) => {
                    add_ext(&mut builder, ext);
                }
            }
        }

        add_cap(&mut builder, &mut enabled_capabilities, Capability::Shader);
        if memory_model == MemoryModel::Vulkan {
            if version < (1, 5) {
                add_ext(&mut builder, sym.spv_khr_vulkan_memory_model);
            }
            add_cap(
                &mut builder,
                &mut enabled_capabilities,
                Capability::VulkanMemoryModel,
            );
        }

        // The linker will always be ran on this module
        add_cap(&mut builder, &mut enabled_capabilities, Capability::Linkage);

        builder.memory_model(AddressingModel::Logical, memory_model);

        Self {
            source_map: tcx.sess.source_map(),
            dropless_arena: &tcx.arena.dropless,
            builder: RefCell::new(builder),
            const_to_val: Default::default(),
            id_to_const_and_val: Default::default(),
            debug_file_cache: Default::default(),
            enabled_capabilities,
        }
    }

    pub fn finalize(self) -> Module {
        self.builder.into_inner().module()
    }

    pub fn dump_module_str(&self) -> String {
        self.builder.borrow().module_ref().disassemble()
    }

    /// Helper function useful to place right before a crash, to debug the module state.
    pub fn dump_module(&self, path: impl AsRef<Path>) {
        let module = self.builder.borrow().module_ref().assemble();
        File::create(path)
            .unwrap()
            .write_all(spirv_tools::binary::from_binary(&module))
            .unwrap();
    }

    pub fn has_capability(&self, capability: Capability) -> bool {
        self.enabled_capabilities.contains(&capability)
    }

    /// See comment on `BuilderCursor`
    fn builder(&self, cursor: BuilderCursor) -> RefMut<'_, Builder> {
        let mut builder = self.builder.borrow_mut();

        let [maybe_fn_idx, maybe_block_idx] = [cursor.fn_id_and_idx, cursor.block_id_and_idx]
            .map(|id_and_idx| id_and_idx.map(|(_, idx)| idx));

        let fn_changed = builder.selected_function() != maybe_fn_idx;
        if fn_changed {
            builder.select_function(maybe_fn_idx).unwrap();
        }

        // Only check the function/block IDs if either of their indices changed.
        if let Some((fn_id, fn_idx)) = cursor.fn_id_and_idx
            && (fn_changed || builder.selected_block() != maybe_block_idx)
        {
            builder.select_block(maybe_block_idx).unwrap();

            let function = &builder.module_ref().functions[fn_idx];
            if fn_changed {
                assert_eq!(function.def_id(), Some(fn_id));
            }
            if let Some((block_id, block_idx)) = cursor.block_id_and_idx {
                assert_eq!(function.blocks[block_idx].label_id(), Some(block_id));
            }
        }

        builder
    }

    /// See comment on `BuilderCursor`
    pub fn global_builder(&self) -> RefMut<'_, Builder> {
        self.builder(BuilderCursor::default())
    }

    /// See comment on `BuilderCursor`
    pub fn builder_for_fn(&self, func: SpirvFunctionCursor) -> RefMut<'_, Builder> {
        self.builder(BuilderCursor {
            fn_id_and_idx: Some((func.id, func.index_in_builder)),
            block_id_and_idx: None,
        })
    }

    /// See comment on `BuilderCursor`
    pub fn builder_for_block(&self, block: SpirvBlockCursor) -> RefMut<'_, Builder> {
        self.builder(BuilderCursor {
            fn_id_and_idx: Some((block.parent_fn.id, block.parent_fn.index_in_builder)),
            block_id_and_idx: Some((block.id, block.index_in_builder)),
        })
    }

    pub(crate) fn def_constant_cx(
        &self,
        ty: Word,
        val: SpirvConst<'_, 'tcx>,
        cx: &CodegenCx<'tcx>,
    ) -> SpirvValue {
        let scalar_ty = match val {
            SpirvConst::Scalar(_) => Some(cx.lookup_type(ty)),
            _ => None,
        };

        // HACK(eddyb) this is done so late (just before interning `val`) to
        // minimize any potential misuse from direct `def_constant` calls.
        let val = match (val, scalar_ty) {
            (SpirvConst::Scalar(val), Some(SpirvType::Integer(bits, signed))) => {
                let size = Size::from_bits(bits);
                SpirvConst::Scalar(if signed {
                    size.sign_extend(val) as u128
                } else {
                    size.truncate(val)
                })
            }
            _ => val,
        };

        let val_with_type = WithType { ty, val };
        if let Some(&v) = self.const_to_val.borrow().get(&val_with_type) {
            return v;
        }
        let val = val_with_type.val;

        // FIXME(eddyb) make this an extension method on `rspirv::dr::Builder`?
        let const_op = |builder: &mut Builder, op, lhs, maybe_rhs: Option<_>| {
            let id = builder.id();
            builder
                .module_mut()
                .types_global_values
                .push(Instruction::new(
                    Op::SpecConstantOp,
                    Some(ty),
                    Some(id),
                    [
                        Operand::LiteralSpecConstantOpInteger(op),
                        Operand::IdRef(lhs),
                    ]
                    .into_iter()
                    .chain(maybe_rhs.map(Operand::IdRef))
                    .collect(),
                ));
            id
        };

        let mut builder = self.global_builder();
        let id = match val {
            SpirvConst::Scalar(v) => match scalar_ty.unwrap() {
                SpirvType::Integer(..=32, _) | SpirvType::Float(..=32) => {
                    builder.constant_bit32(ty, v as u32)
                }
                SpirvType::Integer(64, _) | SpirvType::Float(64) => {
                    builder.constant_bit64(ty, v as u64)
                }
                SpirvType::Integer(128, false) => {
                    // HACK(eddyb) avoid borrow conflicts.
                    drop(builder);

                    let const_64_u32_id = cx.const_u32(64).def_cx(cx);
                    let [lo_id, hi_id] =
                        [v as u64, (v >> 64) as u64].map(|half| cx.const_u64(half).def_cx(cx));

                    builder = self.global_builder();
                    let mut const_op =
                        |op, lhs, maybe_rhs| const_op(&mut builder, op, lhs, maybe_rhs);
                    let [lo_u128_id, hi_shifted_u128_id] =
                        [(lo_id, None), (hi_id, Some(const_64_u32_id))].map(
                            |(half_u64_id, shift)| {
                                let mut half_u128_id = const_op(Op::UConvert, half_u64_id, None);
                                if let Some(shift_amount_id) = shift {
                                    half_u128_id = const_op(
                                        Op::ShiftLeftLogical,
                                        half_u128_id,
                                        Some(shift_amount_id),
                                    );
                                }
                                half_u128_id
                            },
                        );
                    const_op(Op::BitwiseOr, lo_u128_id, Some(hi_shifted_u128_id))
                }
                SpirvType::Integer(128, true) | SpirvType::Float(128) => {
                    // HACK(eddyb) avoid borrow conflicts.
                    drop(builder);

                    let v_u128_id = cx.const_u128(v).def_cx(cx);

                    builder = self.global_builder();
                    const_op(&mut builder, Op::Bitcast, v_u128_id, None)
                }
                SpirvType::Bool => match v {
                    0 => builder.constant_false(ty),
                    1 => builder.constant_true(ty),
                    _ => cx
                        .tcx
                        .dcx()
                        .fatal(format!("invalid constant value for bool: {v}")),
                },
                other => cx.tcx.dcx().fatal(format!(
                    "SpirvConst::Scalar does not support type {}",
                    other.debug(ty, cx)
                )),
            },

            SpirvConst::Null => builder.constant_null(ty),
            SpirvConst::Undef => builder.undef(ty, None),

            SpirvConst::Composite(v) => builder.constant_composite(ty, v.iter().copied()),

            SpirvConst::PtrTo {
                pointee,
                pointee_alloc: _,
            } => builder.variable(ty, None, StorageClass::Private, Some(pointee)),

            SpirvConst::PtrToFunc {
                func_id,
                mangled_func_name: _,
            } => {
                let id = builder.id();
                builder
                    .module_mut()
                    .types_global_values
                    .push(Instruction::new(
                        Op::ConstantFunctionPointerINTEL,
                        Some(ty),
                        Some(id),
                        [Operand::IdRef(func_id)].into(),
                    ));
                id
            }

            SpirvConst::BitCast(v) => const_op(&mut builder, Op::Bitcast, v, None),
            SpirvConst::PtrByteOffset { ptr, offset } => {
                // HACK(eddyb) avoid borrow conflicts.
                drop(builder);

                let byte_ptr_type = cx.type_ptr_to(cx.type_i8());
                let (op, lhs, rhs) = if ty == byte_ptr_type {
                    (
                        Op::PtrAccessChain,
                        ptr,
                        Some(cx.const_usize(offset.bytes()).def_cx(cx)),
                    )
                } else {
                    // HACK(eddyb) can't easily offset without casting first to `*i8`.
                    let ptr = self.id_to_const_and_val.borrow()[&ptr].val.1;
                    (
                        Op::Bitcast,
                        cx.const_ptr_byte_offset(cx.const_bitcast(ptr, byte_ptr_type), offset)
                            .def_cx(cx),
                        None,
                    )
                };

                builder = self.builder(BuilderCursor::default());
                const_op(&mut builder, op, lhs, rhs)
            }
        };

        let mut original_ptr_before_casts = None;

        #[allow(clippy::match_same_arms)]
        let legal = match val {
            SpirvConst::Scalar(_) => Ok(()),

            SpirvConst::Null => {
                // FIXME(eddyb) check that the type supports `OpConstantNull`.
                Ok(())
            }
            SpirvConst::Undef => {
                // FIXME(eddyb) check that the type supports `OpUndef`.
                Ok(())
            }

            SpirvConst::Composite(v) => v
                .iter()
                .map(|field| {
                    let field_entry = &self.id_to_const_and_val.borrow()[field];
                    field_entry.legal.and(
                        // `field` is itself some legal `SpirvConst`, but can we have
                        // it as part of an `OpConstantComposite`?
                        match field_entry.val.0 {
                            SpirvConst::PtrTo { .. } => Err(IllegalConst::Shallow(
                                LeafIllegalConst::CompositeContainsPtrTo,
                            )),
                            _ => Ok(()),
                        },
                    )
                })
                .reduce(|a, b| {
                    match (a, b) {
                        (Ok(()), Ok(())) => Ok(()),
                        (Err(illegal), Ok(())) | (Ok(()), Err(illegal)) => Err(illegal),

                        // Combining two causes of an illegal `SpirvConst` has to
                        // take into account which is "worse", i.e. which imposes
                        // more restrictions on how the resulting value can be used.
                        // `Indirect` is worse than `Shallow` because it cannot be
                        // materialized at runtime in the same way `Shallow` can be.
                        (Err(illegal @ IllegalConst::Indirect(_)), Err(_))
                        | (Err(_), Err(illegal @ IllegalConst::Indirect(_)))
                        | (
                            Err(illegal @ IllegalConst::Shallow(_)),
                            Err(IllegalConst::Shallow(_)),
                        ) => Err(illegal),
                    }
                })
                .unwrap_or(Ok(())),

            SpirvConst::PtrTo {
                pointee,
                pointee_alloc: _,
            } => {
                match self.id_to_const_and_val.borrow()[&pointee].legal {
                    Ok(()) => Ok(()),

                    // `Shallow` becomes `Indirect` when placed behind a pointer.
                    Err(IllegalConst::Shallow(cause) | IllegalConst::Indirect(cause)) => {
                        Err(IllegalConst::Indirect(cause))
                    }
                }
            }

            SpirvConst::PtrToFunc {
                func_id: _,
                mangled_func_name,
            } => Err(IllegalConst::Shallow(LeafIllegalConst::PtrToFunc {
                mangled_func_name,
            })),

            SpirvConst::BitCast(from_id) => {
                let from_const = self.id_to_const_and_val.borrow()[&from_id];

                if let (SpirvType::Pointer { .. }, SpirvType::Pointer { .. }) =
                    (cx.lookup_type(from_const.val.1.ty), cx.lookup_type(ty))
                {
                    let original_ptr = from_const.val.1.strip_ptrcasts();
                    let original_ptr_id = original_ptr.def_cx(cx);
                    original_ptr_before_casts = Some(original_ptr.map_kind(|_| original_ptr_id));
                }

                // HACK(eddyb) a bitcast is one of the least interesting/useful
                // kinds in which a constant can be illegal, and due to the lack
                // of an actual (`OpSpecConstantOp`-based) materialization, any
                // zombies from the value being bitcast would otherwise be lost.
                //
                // TODO(eddyb) this shouldn't be needed anymore, now that the
                // bitcast is materialized via `OpSpecConstantOp`, but for some
                // yet-unknown reason, the cast gets preferentially reported!
                Err(from_const.legal.err().unwrap_or(IllegalConst::Shallow(
                    LeafIllegalConst::BitCast {
                        from_ty: from_const.val.1.ty,
                    },
                )))
            }

            SpirvConst::PtrByteOffset { .. } => {
                Err(IllegalConst::Shallow(LeafIllegalConst::PtrByteOffset))
            }
        };

        // FIXME(eddyb) avoid dragging "const (il)legality" around, as well
        // (sadly that does require that `SpirvConst` -> SPIR-V be injective,
        // e.g. `OpUndef` can never be used for unrepresentable constants).
        if let Err(illegal) = legal {
            let msg = match illegal {
                IllegalConst::Shallow(cause) | IllegalConst::Indirect(cause) => cause.message(),
            };
            if let IllegalConst::Shallow(LeafIllegalConst::BitCast { from_ty }) = illegal {
                cx.zombie_no_span(
                    id,
                    &format!(
                        "{msg}\
                            \nfrom `{}`\
                            \n  to `{}`",
                        cx.debug_type(from_ty),
                        cx.debug_type(ty)
                    ),
                );
            } else {
                cx.zombie_no_span(id, &msg);
            }
        }

        let val = val.tcx_arena_alloc_slices(cx);

        // FIXME(eddyb) the `val`/`v` name clash is a bit unfortunate.
        let v = SpirvValue {
            zombie_waiting_for_span: legal.is_err(),
            kind: SpirvValueKind::Def {
                id,
                original_ptr_before_casts,
            },
            ty,
        };

        assert_matches!(
            self.const_to_val
                .borrow_mut()
                .insert(WithType { ty, val }, v),
            None
        );
        assert_matches!(
            self.id_to_const_and_val.borrow_mut().insert(
                id,
                WithConstLegality {
                    val: (val, v),
                    legal
                }
            ),
            None
        );

        v
    }

    pub fn lookup_const_by_id(&self, id: Word) -> Option<SpirvConst<'tcx, 'tcx>> {
        Some(self.id_to_const_and_val.borrow().get(&id)?.val.0)
    }

    pub fn lookup_const(&self, def: SpirvValue) -> Option<SpirvConst<'tcx, 'tcx>> {
        match def.kind {
            SpirvValueKind::Def { id, .. } => self.lookup_const_by_id(id),
            SpirvValueKind::ConstDataFromAlloc { .. } => None,
        }
    }

    pub fn lookup_const_scalar(&self, def: SpirvValue) -> Option<u128> {
        match self.lookup_const(def)? {
            SpirvConst::Scalar(v) => Some(v),
            _ => None,
        }
    }

    pub fn file_line_col_range_for_debuginfo(
        &self,
        span: Span,
    ) -> (DebugFileSpirv<'tcx>, Range<(u32, u32)>) {
        // HACK(eddyb) this is similar to what `#[track_caller]` does, and it
        // allows us to point to the use site of a macro, instead of inside the
        // macro (but ideally we would record the entire macro backtrace).
        let span = span.ctxt().outer_expn().expansion_cause().unwrap_or(span);

        let (lo, hi) = (span.lo(), span.hi());

        let lo_loc = self.source_map.lookup_char_pos(lo);
        let lo_line_col = (lo_loc.line as u32, lo_loc.col_display as u32);

        // Only use `hi` if the span is actually a range within a file.
        let hi_line_col = if lo <= hi {
            let hi_loc = self.source_map.lookup_char_pos(hi);
            if lo_loc.file.start_pos == hi_loc.file.start_pos {
                (hi_loc.line as u32, hi_loc.col_display as u32)
            } else {
                lo_line_col
            }
        } else {
            lo_line_col
        };

        (self.def_debug_file(lo_loc.file), lo_line_col..hi_line_col)
    }

    fn def_debug_file(&self, sf: Arc<SourceFile>) -> DebugFileSpirv<'tcx> {
        *self
            .debug_file_cache
            .borrow_mut()
            .entry(DebugFileKey(sf))
            .or_insert_with_key(|DebugFileKey(sf)| {
                let mut builder = self.global_builder();

                // FIXME(eddyb) it would be nicer if we could just rely on
                // `RealFileName::to_string_lossy` returning `Cow<'_, str>`,
                // but sadly that `'_` is the lifetime of the temporary `Arc`,
                // not `'tcx`, so we have to arena-allocate to get `&'tcx str`.
                let file_name = match &sf.name {
                    FileName::Real(name) => {
                        name.to_string_lossy(FileNameDisplayPreference::Remapped)
                    }
                    _ => sf.name.prefer_remapped_unconditionaly().to_string().into(),
                };
                let file_name = {
                    // FIXME(eddyb) it should be possible to arena-allocate a
                    // `&str` directly, but it would require upstream changes,
                    // and strings are handled by string interning in `rustc`.
                    fn arena_alloc_slice<'tcx, T: Copy>(
                        dropless_arena: &'tcx DroplessArena,
                        xs: &[T],
                    ) -> &'tcx [T] {
                        if xs.is_empty() {
                            &[]
                        } else {
                            dropless_arena.alloc_slice(xs)
                        }
                    }
                    str::from_utf8(arena_alloc_slice(self.dropless_arena, file_name.as_bytes()))
                        .unwrap()
                };
                let file_name_op_string_id = builder.string(file_name.to_owned());

                let file_contents = self
                    .source_map
                    .span_to_snippet(Span::with_root_ctxt(sf.start_pos, sf.end_position()))
                    .ok();

                // HACK(eddyb) this logic is duplicated from `spirt::spv::lift`.
                let op_source_and_continued_chunks = file_contents.as_ref().map(|contents| {
                    // The maximum word count is `2**16 - 1`, the first word is
                    // taken up by the opcode & word count, and one extra byte is
                    // taken up by the nil byte at the end of the LiteralString.
                    const MAX_OP_SOURCE_CONT_CONTENTS_LEN: usize = (0xffff - 1) * 4 - 1;

                    // `OpSource` has 3 more operands than `OpSourceContinued`,
                    // and each of them take up exactly one word.
                    const MAX_OP_SOURCE_CONTENTS_LEN: usize =
                        MAX_OP_SOURCE_CONT_CONTENTS_LEN - 3 * 4;

                    let (op_source_str, mut all_op_source_continued_str) =
                        contents.split_at(contents.len().min(MAX_OP_SOURCE_CONTENTS_LEN));

                    // FIXME(eddyb) `spirt::spv::lift` should use this.
                    let all_op_source_continued_str_chunks = iter::from_fn(move || {
                        let contents_rest = &mut all_op_source_continued_str;
                        if contents_rest.is_empty() {
                            return None;
                        }

                        // FIXME(eddyb) test with UTF-8! this `split_at` should
                        // actually take *less* than the full possible size, to
                        // avoid cutting a UTF-8 sequence.
                        let (cont_chunk, rest) = contents_rest
                            .split_at(contents_rest.len().min(MAX_OP_SOURCE_CONT_CONTENTS_LEN));
                        *contents_rest = rest;
                        Some(cont_chunk)
                    });
                    (op_source_str, all_op_source_continued_str_chunks)
                });

                if let Some((op_source_str, all_op_source_continued_str_chunks)) =
                    op_source_and_continued_chunks
                {
                    builder.source(
                        SourceLanguage::Unknown,
                        0,
                        Some(file_name_op_string_id),
                        Some(op_source_str),
                    );
                    for cont_chunk in all_op_source_continued_str_chunks {
                        builder.source_continued(cont_chunk);
                    }
                }

                DebugFileSpirv {
                    file_name,
                    file_name_op_string_id,
                }
            })
    }

    pub fn set_global_initializer(&self, global: Word, initializer: Word) {
        let mut builder = self.builder.borrow_mut();
        let module = builder.module_mut();
        let index = module
            .types_global_values
            .iter()
            .enumerate()
            .find_map(|(index, inst)| {
                if inst.result_id == Some(global) {
                    Some(index)
                } else {
                    None
                }
            })
            .expect("set_global_initializer global not found");
        // Remove and push it to the end, to keep spir-v definition order.
        let mut inst = module.types_global_values.remove(index);
        assert_eq!(inst.class.opcode, Op::Variable);
        assert_eq!(
            inst.operands.len(),
            1,
            "global already has initializer defined: {global}"
        );
        inst.operands.push(Operand::IdRef(initializer));
        module.types_global_values.push(inst);
    }
}
