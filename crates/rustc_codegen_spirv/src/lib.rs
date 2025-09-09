// HACK(eddyb) start of `rustc_codegen_ssa` crate-level attributes (see `build.rs`).
#![allow(internal_features)]
#![allow(rustc::diagnostic_outside_of_impl)]
#![allow(rustc::untranslatable_diagnostic)]
#![feature(assert_matches)]
#![feature(box_patterns)]
#![feature(file_buffered)]
#![feature(if_let_guard)]
#![feature(negative_impls)]
#![feature(rustdoc_internals)]
#![feature(string_from_utf8_lossy_owned)]
#![feature(trait_alias)]
#![feature(try_blocks)]
#![recursion_limit = "256"]
// HACK(eddyb) end of `rustc_codegen_ssa` crate-level attributes (see `build.rs`).

//! Welcome to the API documentation for the `rust-gpu` project, this API is
//! unstable and mainly intended for developing on the project itself. This is
//! the API documentation for `rustc_codegen_spirv` which is not that useful on
//! its own. You might also be interested in the following crates. There's also
//! the [Rust GPU Dev Guide][gpu-dev-guide] which contains more user-level
//! information on how to use and setup `rust-gpu`.
//!
//! - [`spirv-builder`]
//! - [`spirv-std`]
//! - [`spirv-tools`]
//! - [`spirv-tools-sys`]
//!
//! [gpu-dev-guide]: https://rust-gpu.github.io/rust-gpu/book
//! [`spirv-builder`]: https://rust-gpu.github.io/rust-gpu/api/spirv_builder
//! [`spirv-std`]: https://rust-gpu.github.io/rust-gpu/api/spirv_std
//! [`spirv-tools`]: https://rust-gpu.github.io/rust-gpu/api/spirv_tools
//! [`spirv-tools-sys`]: https://rust-gpu.github.io/rust-gpu/api/spirv_tools_sys
#![feature(rustc_private)]
// crate-specific exceptions:
#![allow(
    unsafe_code,                // rustc_codegen_ssa requires unsafe functions in traits to be impl'd
    clippy::enum_glob_use,      // pretty useful pattern with some codegen'd enums (e.g. rspirv::spirv::Op)
    clippy::todo,               // still lots to implement :)

    // FIXME(eddyb) new warnings from 1.83 rustup, apply their suggested changes.
    mismatched_lifetime_syntaxes,
    clippy::needless_lifetimes,
)]

// Unfortunately, this will not fail fast when compiling, but rather will wait for
// rustc_codegen_spirv to be compiled. Putting this in build.rs will solve that problem, however,
// that creates the much worse problem that then running `cargo check` will cause
// rustc_codegen_spirv to be *compiled* instead of merely checked, something that takes
// significantly longer. So, the trade-off between detecting a bad configuration slower for a
// faster `cargo check` is worth it.
#[cfg(all(feature = "use-compiled-tools", feature = "use-installed-tools"))]
compile_error!(
    "Either \"use-compiled-tools\" (enabled by default) or \"use-installed-tools\" may be enabled."
);

// HACK(eddyb) `build.rs` copies `rustc_codegen_ssa` (from the `rustc-dev` component)
// and patches it to produce a "pqp" ("pre-`qptr`-patched") version that maintains
// compatibility with "legacy" Rust-GPU pointer handling (mainly typed `alloca`s).
//
// FIXME(eddyb) get rid of this as soon as it's not needed anymore.
#[cfg(not(rustc_codegen_spirv_disable_pqp_cg_ssa))]
include!(concat!(env!("OUT_DIR"), "/pqp_cg_ssa.rs"));

// HACK(eddyb) guide `rustc` to finding the right deps in the sysroot, which
// (sadly) has to be outside `include!` to have any effect whatsoever.
// FIXME(eddyb) this only really handles `bitflags`, not `object`.
#[cfg(not(rustc_codegen_spirv_disable_pqp_cg_ssa))]
mod _rustc_codegen_ssa_transitive_deps_hack {
    extern crate rustc_codegen_ssa as _;
}

// NOTE(eddyb) `mod maybe_pqp_cg_ssa` is defined by the above `include`, when
// in the (default for now) `pqp_cg_ssa` mode (see `build.rs`).
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
use rustc_codegen_ssa as maybe_pqp_cg_ssa;

// FIXME(eddyb) remove all `#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]`
// as soon as they're not needed anymore (i.e. using `rustc_codegen_ssa` again).
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_abi;
extern crate rustc_apfloat;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_arena;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_ast;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_attr_data_structures;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_attr_parsing;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_codegen_ssa;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_data_structures;
extern crate rustc_driver;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_errors;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_hashes;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_hir;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_index;
extern crate rustc_interface;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_metadata;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_middle;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_session;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_span;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_target;

macro_rules! assert_ty_eq {
    ($codegen_cx:expr, $left:expr, $right:expr) => {
        assert!(
            $left == $right,
            "Expected types to be equal:\n{}\n==\n{}",
            $codegen_cx.debug_type($left),
            $codegen_cx.debug_type($right)
        )
    };
}

mod abi;
mod allocator;
mod attr;
mod builder;
mod builder_spirv;
mod codegen_cx;
mod custom_decorations;
mod custom_insts;
mod link;
mod linker;
mod spirv_type;
mod spirv_type_constraints;
mod symbols;
mod target;
mod target_feature;

use builder::Builder;
use codegen_cx::CodegenCx;
use maybe_pqp_cg_ssa::back::lto::{LtoModuleCodegen, SerializedModule, ThinModule};
use maybe_pqp_cg_ssa::back::write::{
    CodegenContext, FatLtoInput, ModuleConfig, OngoingCodegen, TargetMachineFactoryConfig,
};
use maybe_pqp_cg_ssa::base::maybe_create_entry_wrapper;
use maybe_pqp_cg_ssa::mono_item::MonoItemExt;
use maybe_pqp_cg_ssa::traits::{
    CodegenBackend, ExtraBackendMethods, ModuleBufferMethods, ThinBufferMethods,
    WriteBackendMethods,
};
use maybe_pqp_cg_ssa::{CodegenResults, CompiledModule, ModuleCodegen, ModuleKind, TargetConfig};
use rspirv::binary::Assemble;
use rustc_ast::expand::allocator::AllocatorKind;
use rustc_ast::expand::autodiff_attrs::AutoDiffItem;
use rustc_data_structures::fx::FxIndexMap;
use rustc_errors::{DiagCtxtHandle, FatalError};
use rustc_metadata::EncodedMetadata;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::mir::mono::{CodegenUnit, MonoItem, MonoItemData};
use rustc_middle::mir::pretty::write_mir_pretty;
use rustc_middle::ty::print::with_no_trimmed_paths;
use rustc_middle::ty::{InstanceKind, TyCtxt};
use rustc_session::Session;
use rustc_session::config::{self, OutputFilenames, OutputType};
use rustc_span::symbol::Symbol;
use std::any::Any;
use std::fs;
use std::io::Cursor;
use std::io::Write;
use std::path::Path;
use std::sync::Arc;
use tracing::{error, warn};

fn dump_mir(tcx: TyCtxt<'_>, mono_items: &[(MonoItem<'_>, MonoItemData)], path: &Path) {
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    let mut file = fs::File::create(path).unwrap();
    for &(mono_item, _) in mono_items {
        if let MonoItem::Fn(instance) = mono_item
            && matches!(instance.def, InstanceKind::Item(_))
        {
            let mut mir = Cursor::new(Vec::new());
            if write_mir_pretty(tcx, Some(instance.def_id()), &mut mir).is_ok() {
                writeln!(file, "{}", String::from_utf8(mir.into_inner()).unwrap()).unwrap();
            }
        }
    }
}

#[derive(Clone)]
struct SpirvCodegenBackend;

impl CodegenBackend for SpirvCodegenBackend {
    fn init(&self, sess: &Session) {
        // Set up logging/tracing. See https://github.com/Rust-GPU/rust-gpu/issues/192.
        init_logging(sess);
    }

    fn locale_resource(&self) -> &'static str {
        rustc_errors::DEFAULT_LOCALE_RESOURCE
    }

    fn target_config(&self, sess: &Session) -> TargetConfig {
        let cmdline = sess.opts.cg.target_feature.split(',');
        let cfg = sess.target.options.features.split(',');

        let target_features: Vec<_> = cfg
            .chain(cmdline)
            .filter(|l| l.starts_with('+'))
            .map(|l| &l[1..])
            .filter(|l| !l.is_empty())
            .map(Symbol::intern)
            .collect();

        // HACK(eddyb) this should be a superset of `target_features`,
        // which *additionally* also includes unstable target features,
        // but there is no reason to make a distinction for SPIR-V ones.
        let unstable_target_features = target_features.clone();

        TargetConfig {
            target_features,
            unstable_target_features,

            // FIXME(eddyb) support and/or emulate `f16` and `f128`.
            has_reliable_f16: false,
            has_reliable_f16_math: false,
            has_reliable_f128: false,
            has_reliable_f128_math: false,
        }
    }

    fn provide(&self, providers: &mut rustc_middle::util::Providers) {
        // FIXME(eddyb) this is currently only passed back to us, specifically
        // into `target_machine_factory` (which is a noop), but it might make
        // sense to move some of the target feature parsing into here.
        providers.global_backend_features = |_tcx, ()| vec![];

        crate::abi::provide(providers);
        crate::attr::provide(providers);
    }

    fn codegen_crate(&self, tcx: TyCtxt<'_>) -> Box<dyn Any> {
        Box::new(maybe_pqp_cg_ssa::base::codegen_crate(
            Self,
            tcx,
            tcx.sess
                .opts
                .cg
                .target_cpu
                .clone()
                .unwrap_or_else(|| tcx.sess.target.cpu.to_string()),
        ))
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        sess: &Session,
        _outputs: &OutputFilenames,
    ) -> (CodegenResults, FxIndexMap<WorkProductId, WorkProduct>) {
        ongoing_codegen
            .downcast::<OngoingCodegen<Self>>()
            .expect("Expected OngoingCodegen, found Box<Any>")
            .join(sess)
    }

    fn link(
        &self,
        sess: &Session,
        codegen_results: CodegenResults,
        metadata: EncodedMetadata,
        outputs: &OutputFilenames,
    ) {
        let timer = sess.timer("link_crate");
        link::link(
            sess,
            &codegen_results,
            &metadata,
            outputs,
            codegen_results.crate_info.local_crate_name.as_str(),
        );
        drop(timer);
    }
}

struct SpirvModuleBuffer(Vec<u32>);

impl SpirvModuleBuffer {
    fn as_bytes(&self) -> &[u8] {
        spirv_tools::binary::from_binary(&self.0)
    }
}
impl ModuleBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        self.as_bytes()
    }
}
impl ThinBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        self.as_bytes()
    }
    fn thin_link_data(&self) -> &[u8] {
        &[]
    }
}

impl SpirvCodegenBackend {
    fn optimize_common(
        _cgcx: &CodegenContext<Self>,
        module: &mut ModuleCodegen<<Self as WriteBackendMethods>::Module>,
    ) -> Result<(), FatalError> {
        // Apply DCE ("dead code elimination") to modules before ever serializing
        // them as `.spv` files (technically, `.rcgu.o` files inside `.rlib`s),
        // that will later get linked (potentially many times, esp. if this is
        // some big upstream library, e.g. `core` itself), and will therefore
        // benefit from not having to clean up all sorts of unreachable helpers.
        linker::dce::dce(&mut module.module_llvm);

        // FIXME(eddyb) run as many optimization passes as possible, not just DCE.

        Ok(())
    }
}

impl WriteBackendMethods for SpirvCodegenBackend {
    type Module = rspirv::dr::Module;
    type TargetMachine = ();
    type TargetMachineError = String;
    type ModuleBuffer = SpirvModuleBuffer;
    type ThinData = ();
    type ThinBuffer = SpirvModuleBuffer;

    // FIXME(eddyb) reuse the "merge" stage of `crate::linker` for this, or even
    // delegate to `run_fat_lto` (although `-Zcombine-cgu` is much more niche).
    fn run_link(
        cgcx: &CodegenContext<Self>,
        diag_handler: DiagCtxtHandle<'_>,
        _modules: Vec<ModuleCodegen<Self::Module>>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        assert!(
            cgcx.opts.unstable_opts.combine_cgu,
            "`run_link` (for `WorkItemResult::NeedsLink`) should \
             only be invoked due to `-Zcombine-cgu`"
        );
        diag_handler.fatal("Rust-GPU does not support `-Zcombine-cgu`")
    }

    // FIXME(eddyb) reuse the "merge" stage of `crate::linker` for this, or even
    // consider setting `requires_lto = true` in the target specs and moving the
    // entirety of `crate::linker` into this stage (lacking diagnostics may be
    // an issue - it's surprising `CodegenBackend::link` has `Session` at all).
    fn run_fat_lto(
        cgcx: &CodegenContext<Self>,
        _modules: Vec<FatLtoInput<Self>>,
        _cached_modules: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> Result<LtoModuleCodegen<Self>, FatalError> {
        assert!(
            cgcx.lto == rustc_session::config::Lto::Fat,
            "`run_fat_lto` (for `WorkItemResult::NeedsFatLto`) should \
             only be invoked due to `-Clto` (or equivalent)"
        );
        unreachable!("Rust-GPU does not support fat LTO")
    }

    fn run_thin_lto(
        cgcx: &CodegenContext<Self>,
        modules: Vec<(String, Self::ThinBuffer)>,
        cached_modules: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> Result<(Vec<LtoModuleCodegen<Self>>, Vec<WorkProduct>), FatalError> {
        link::run_thin(cgcx, modules, cached_modules)
    }

    fn print_pass_timings(&self) {
        warn!("TODO: Implement print_pass_timings");
    }

    fn print_statistics(&self) {
        warn!("TODO: Implement print_statistics");
    }

    fn optimize(
        cgcx: &CodegenContext<Self>,
        _dcx: DiagCtxtHandle<'_>,
        module: &mut ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
    ) -> Result<(), FatalError> {
        Self::optimize_common(cgcx, module)
    }

    fn optimize_thin(
        cgcx: &CodegenContext<Self>,
        thin_module: ThinModule<Self>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        // FIXME(eddyb) the inefficiency of Module -> [u8] -> Module roundtrips
        // comes from upstream and it applies to `rustc_codegen_llvm` as well,
        // eventually it should be properly addressed (for `ThinLocal` at least).
        let mut module = ModuleCodegen {
            module_llvm: link::with_rspirv_loader(|loader| {
                rspirv::binary::parse_bytes(thin_module.data(), loader)
            })
            .unwrap(),
            name: thin_module.name().to_string(),
            kind: ModuleKind::Regular,
            thin_lto_buffer: None,
        };
        Self::optimize_common(cgcx, &mut module)?;
        Ok(module)
    }

    fn optimize_fat(
        cgcx: &CodegenContext<Self>,
        module: &mut ModuleCodegen<Self::Module>,
    ) -> Result<(), FatalError> {
        Self::optimize_common(cgcx, module)
    }

    fn codegen(
        cgcx: &CodegenContext<Self>,
        _diag_handler: DiagCtxtHandle<'_>,
        module: ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
    ) -> Result<CompiledModule, FatalError> {
        let kind = module.kind;
        let (name, module_buffer) = Self::serialize_module(module);

        let path = cgcx.output_filenames.temp_path_for_cgu(
            OutputType::Object,
            &name,
            cgcx.invocation_temp.as_deref(),
        );
        fs::write(&path, module_buffer.as_bytes()).unwrap();

        Ok(CompiledModule {
            name,
            kind,
            object: Some(path),
            dwarf_object: None,
            bytecode: None,
            assembly: None,
            llvm_ir: None,
            links_from_incr_cache: vec![],
        })
    }

    fn prepare_thin(
        module: ModuleCodegen<Self::Module>,
        _want_summary: bool,
    ) -> (String, Self::ThinBuffer) {
        Self::serialize_module(module)
    }

    fn serialize_module(module: ModuleCodegen<Self::Module>) -> (String, Self::ModuleBuffer) {
        (
            module.name,
            SpirvModuleBuffer(module.module_llvm.assemble()),
        )
    }

    fn autodiff(
        _cgcx: &CodegenContext<Self>,
        _module: &ModuleCodegen<Self::Module>,
        _diff_fncs: Vec<AutoDiffItem>,
        _config: &ModuleConfig,
    ) -> Result<(), FatalError> {
        unreachable!("Rust-GPU does not support autodiff")
    }
}

impl ExtraBackendMethods for SpirvCodegenBackend {
    fn codegen_allocator(
        &self,
        tcx: TyCtxt<'_>,
        module_name: &str,
        kind: AllocatorKind,
        alloc_error_handler_kind: AllocatorKind,
    ) -> Self::Module {
        // HACK(eddyb) this pseudo-CGU allows using `CodegenCx` itself.
        let cgu = tcx
            .arena
            .alloc(CodegenUnit::new(Symbol::intern(module_name)));

        let cx = CodegenCx::new(tcx, cgu);
        allocator::codegen(&cx, kind, alloc_error_handler_kind);
        cx.finalize_module()
    }

    fn compile_codegen_unit<'tcx>(
        &self,
        tcx: TyCtxt<'tcx>,
        cgu_name: Symbol,
    ) -> (ModuleCodegen<Self::Module>, u64) {
        let _timer = tcx
            .prof
            .verbose_generic_activity_with_arg("codegen_module", cgu_name.to_string());

        // TODO: Do dep_graph stuff
        let cgu = tcx.codegen_unit(cgu_name);

        let mut cx = CodegenCx::new(tcx, cgu);
        let do_codegen = |cx: &mut CodegenCx<'tcx>| {
            let mono_items = cgu.items_in_deterministic_order(cx.tcx);

            if let Some(dir) = &cx.codegen_args.dump_mir {
                dump_mir(tcx, mono_items.as_slice(), &dir.join(cgu_name.to_string()));
            }

            for &(mono_item, mono_item_data) in mono_items.iter() {
                mono_item.predefine::<Builder<'_, '_>>(
                    cx,
                    cgu_name.as_str(),
                    mono_item_data.linkage,
                    mono_item_data.visibility,
                );
            }

            // ... and now that we have everything pre-defined, fill out those definitions.
            for &(mono_item, mono_item_data) in &mono_items {
                mono_item.define::<Builder<'_, '_>>(cx, cgu_name.as_str(), mono_item_data);
            }

            if let Some(_entry) = maybe_create_entry_wrapper::<Builder<'_, '_>>(cx, cgu) {
                // attributes::sanitize(&cx, SanitizerSet::empty(), entry);
            }
        };
        // HACK(eddyb) mutable access needed for `mono_item.define::<...>(cx, ...)`
        // but that alone leads to needless cloning and smuggling a mutable borrow
        // through `DumpModuleOnPanic` (for both its `Drop` impl and `do_codegen`).
        if let Some(path) = cx.codegen_args.dump_module_on_panic.clone() {
            let module_dumper = DumpModuleOnPanic {
                cx: &mut cx,
                path: &path,
            };
            with_no_trimmed_paths!(do_codegen(module_dumper.cx));
            drop(module_dumper);
        } else {
            with_no_trimmed_paths!(do_codegen(&mut cx));
        }

        (
            ModuleCodegen {
                name: cgu_name.to_string(),
                module_llvm: cx.finalize_module(),
                kind: ModuleKind::Regular,
                thin_lto_buffer: None,
            },
            0,
        )
    }

    fn target_machine_factory(
        &self,
        _sess: &Session,
        _opt_level: config::OptLevel,
        _target_features: &[String],
    ) -> Arc<(dyn Fn(TargetMachineFactoryConfig) -> Result<(), String> + Send + Sync + 'static)>
    {
        Arc::new(|_| Ok(()))
    }
}

struct DumpModuleOnPanic<'a, 'cx, 'tcx> {
    cx: &'cx mut CodegenCx<'tcx>,
    path: &'a Path,
}

impl Drop for DumpModuleOnPanic<'_, '_, '_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            if self.path.has_root() {
                self.cx.builder.dump_module(self.path);
            } else {
                error!("{}", self.cx.builder.dump_module_str());
            }
        }
    }
}

/// This is the entrypoint for a hot plugged `rustc_codegen_spirv`
#[unsafe(no_mangle)]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    // Tweak rustc's default ICE panic hook, to direct people to `rust-gpu`.
    rustc_driver::install_ice_hook("https://github.com/rust-gpu/rust-gpu/issues/new", |dcx| {
        dcx.handle().note(concat!(
            "`rust-gpu` version `",
            env!("CARGO_PKG_VERSION"),
            "`"
        ));
    });

    Box::new(SpirvCodegenBackend)
}

// Set up logging/tracing. See https://github.com/Rust-GPU/rust-gpu/issues/192.
fn init_logging(sess: &Session) {
    use std::env::{self, VarError};
    use std::io::{self, IsTerminal};
    use tracing_subscriber::layer::SubscriberExt;

    // Set up the default subscriber with optional filtering.
    let filter = tracing_subscriber::EnvFilter::from_env("RUSTGPU_LOG");
    #[cfg(not(rustc_codegen_spirv_disable_pqp_cg_ssa))]
    let filter = filter.add_directive("rustc_codegen_spirv::maybe_pqp_cg_ssa=off".parse().unwrap());
    let subscriber = tracing_subscriber::Registry::default().with(filter);

    #[derive(Debug, Default)]
    enum OutputFormat {
        #[default]
        Tree,
        Flat,
        Json,
    }

    let output_format = match env::var("RUSTGPU_LOG_FORMAT").as_deref() {
        Ok("tree") | Err(VarError::NotPresent) => OutputFormat::Tree,
        Ok("flat") => OutputFormat::Flat,
        Ok("json") => OutputFormat::Json,
        Ok(value) => sess.dcx().fatal(format!(
            "invalid output format value '{value}': expected one of tree, flat, or json",
        )),
        Err(VarError::NotUnicode(value)) => sess.dcx().fatal(format!(
            "invalid output format value '{}': expected one of tree, flat, or json",
            value.to_string_lossy()
        )),
    };

    let subscriber: Box<dyn tracing::Subscriber + Send + Sync> = match output_format {
        OutputFormat::Tree => {
            // TODO(@LegNeato): Query dcx color support when rustc exposes it.
            let color_logs = match env::var("RUSTGPU_LOG_COLOR").as_deref() {
                Ok("always") => true,
                Ok("never") => false,
                Ok("auto") | Err(VarError::NotPresent) => io::stderr().is_terminal(),
                Ok(value) => sess.dcx().fatal(format!(
                    "invalid log color value '{value}': expected one of always, never, or auto",
                )),
                Err(VarError::NotUnicode(value)) => sess.dcx().fatal(format!(
                    "invalid log color value '{}': expected one of always, never, or auto",
                    value.to_string_lossy()
                )),
            };

            let tree_layer = tracing_tree::HierarchicalLayer::default()
                .with_writer(io::stderr)
                .with_ansi(color_logs)
                .with_targets(true)
                .with_wraparound(10)
                .with_verbose_exit(true)
                .with_verbose_entry(true)
                .with_indent_amount(2);

            #[cfg(debug_assertions)]
            let tree_layer = tree_layer.with_thread_ids(true).with_thread_names(true);

            Box::new(subscriber.with(tree_layer))
        }
        OutputFormat::Flat => Box::new(subscriber),
        OutputFormat::Json => Box::new(subscriber.with(tracing_subscriber::fmt::layer().json())),
    };
    tracing::subscriber::set_global_default(subscriber).unwrap();
}
