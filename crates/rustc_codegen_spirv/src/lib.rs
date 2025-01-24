// HACK(eddyb) start of `rustc_codegen_ssa` crate-level attributes (see `build.rs`).
#![allow(internal_features)]
#![allow(rustc::diagnostic_outside_of_impl)]
#![allow(rustc::untranslatable_diagnostic)]
#![feature(assert_matches)]
#![feature(box_patterns)]
#![feature(debug_closure_helpers)]
#![feature(file_buffered)]
#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(negative_impls)]
#![feature(rustdoc_internals)]
#![feature(trait_alias)]
#![feature(try_blocks)]
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
#![feature(result_flattening)]
// crate-specific exceptions:
#![allow(
    unsafe_code,                // rustc_codegen_ssa requires unsafe functions in traits to be impl'd
    clippy::match_on_vec_items, // rustc_codegen_spirv has less strict panic requirements than other embark projects
    clippy::enum_glob_use,      // pretty useful pattern with some codegen'd enums (e.g. rspirv::spirv::Op)
    clippy::todo,               // still lots to implement :)

    // FIXME(eddyb) new warnings from 1.83 rustup, apply their suggested changes.
    elided_named_lifetimes,
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
extern crate rustc_apfloat;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_arena;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_ast;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_attr;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_codegen_ssa;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_data_structures;
extern crate rustc_driver;
#[cfg(rustc_codegen_spirv_disable_pqp_cg_ssa)]
extern crate rustc_errors;
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
use maybe_pqp_cg_ssa::{CodegenResults, CompiledModule, ModuleCodegen, ModuleKind};
use rspirv::binary::Assemble;
use rustc_ast::expand::allocator::AllocatorKind;
use rustc_data_structures::fx::FxIndexMap;
use rustc_errors::{DiagCtxtHandle, ErrorGuaranteed, FatalError};
use rustc_metadata::EncodedMetadata;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::mir::mono::{MonoItem, MonoItemData};
use rustc_middle::mir::pretty::write_mir_pretty;
use rustc_middle::ty::print::with_no_trimmed_paths;
use rustc_middle::ty::{self, Instance, InstanceKind, TyCtxt};
use rustc_session::Session;
use rustc_session::config::{self, OutputFilenames, OutputType};
use rustc_span::symbol::{Symbol, sym};
use std::any::Any;
use std::fs::{File, create_dir_all};
use std::io::Cursor;
use std::io::Write;
use std::path::Path;
use std::sync::Arc;
use tracing::{error, warn};

fn dump_mir(tcx: TyCtxt<'_>, mono_items: &[(MonoItem<'_>, MonoItemData)], path: &Path) {
    create_dir_all(path.parent().unwrap()).unwrap();
    let mut file = File::create(path).unwrap();
    for &(mono_item, _) in mono_items {
        if let MonoItem::Fn(instance) = mono_item {
            if matches!(instance.def, InstanceKind::Item(_)) {
                let mut mir = Cursor::new(Vec::new());
                if write_mir_pretty(tcx, Some(instance.def_id()), &mut mir).is_ok() {
                    writeln!(file, "{}", String::from_utf8(mir.into_inner()).unwrap()).unwrap();
                }
            }
        }
    }
}

fn is_blocklisted_fn<'tcx>(
    tcx: TyCtxt<'tcx>,
    sym: &symbols::Symbols,
    instance: Instance<'tcx>,
) -> bool {
    // TODO: These sometimes have a constant value of an enum variant with a hole
    if let InstanceKind::Item(def_id) = instance.def {
        if let Some(debug_trait_def_id) = tcx.get_diagnostic_item(sym::Debug) {
            // Helper for detecting `<_ as core::fmt::Debug>::fmt` (in impls).
            let is_debug_fmt_method = |def_id| match tcx.opt_associated_item(def_id) {
                Some(assoc) if assoc.ident(tcx).name == sym::fmt => match assoc.container {
                    ty::AssocItemContainer::Impl => {
                        let impl_def_id = assoc.container_id(tcx);
                        tcx.impl_trait_ref(impl_def_id)
                            .map(|tr| tr.skip_binder().def_id)
                            == Some(debug_trait_def_id)
                    }
                    ty::AssocItemContainer::Trait => false,
                },
                _ => false,
            };

            if is_debug_fmt_method(def_id) {
                return true;
            }

            if tcx.opt_item_ident(def_id).map(|i| i.name) == Some(sym.fmt_decimal) {
                if let Some(parent_def_id) = tcx.opt_parent(def_id) {
                    if is_debug_fmt_method(parent_def_id) {
                        return true;
                    }
                }
            }
        }
    }

    false
}

// TODO: Should this store Vec or Module?
struct SpirvModuleBuffer(Vec<u32>);

impl ModuleBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        spirv_tools::binary::from_binary(&self.0)
    }
}

// TODO: Should this store Vec or Module?
struct SpirvThinBuffer(Vec<u32>);

impl ThinBufferMethods for SpirvThinBuffer {
    fn data(&self) -> &[u8] {
        spirv_tools::binary::from_binary(&self.0)
    }
    fn thin_link_data(&self) -> &[u8] {
        &[]
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

    fn target_features(&self, sess: &Session, _allow_unstable: bool) -> Vec<Symbol> {
        let cmdline = sess.opts.cg.target_feature.split(',');
        let cfg = sess.target.options.features.split(',');
        cfg.chain(cmdline)
            .filter(|l| l.starts_with('+'))
            .map(|l| &l[1..])
            .filter(|l| !l.is_empty())
            .map(Symbol::intern)
            .collect()
    }

    fn provide(&self, providers: &mut rustc_middle::util::Providers) {
        // FIXME(eddyb) this is currently only passed back to us, specifically
        // into `target_machine_factory` (which is a noop), but it might make
        // sense to move some of the target feature parsing into here.
        providers.global_backend_features = |_tcx, ()| vec![];

        crate::abi::provide(providers);
        crate::attr::provide(providers);
    }

    fn codegen_crate(
        &self,
        tcx: TyCtxt<'_>,
        metadata: EncodedMetadata,
        need_metadata_module: bool,
    ) -> Box<dyn Any> {
        Box::new(maybe_pqp_cg_ssa::base::codegen_crate(
            Self,
            tcx,
            tcx.sess
                .opts
                .cg
                .target_cpu
                .clone()
                .unwrap_or_else(|| tcx.sess.target.cpu.to_string()),
            metadata,
            need_metadata_module,
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
        outputs: &OutputFilenames,
    ) -> Result<(), ErrorGuaranteed> {
        let timer = sess.timer("link_crate");
        link::link(
            sess,
            &codegen_results,
            outputs,
            codegen_results.crate_info.local_crate_name.as_str(),
        );
        drop(timer);

        sess.dcx().has_errors().map_or(Ok(()), Err)
    }
}

impl WriteBackendMethods for SpirvCodegenBackend {
    type Module = Vec<u32>;
    type TargetMachine = ();
    type TargetMachineError = String;
    type ModuleBuffer = SpirvModuleBuffer;
    type ThinData = ();
    type ThinBuffer = SpirvThinBuffer;

    fn run_link(
        _cgcx: &CodegenContext<Self>,
        _diag_handler: DiagCtxtHandle<'_>,
        _modules: Vec<ModuleCodegen<Self::Module>>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        todo!()
    }

    fn run_fat_lto(
        _: &CodegenContext<Self>,
        _: Vec<FatLtoInput<Self>>,
        _: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> Result<LtoModuleCodegen<Self>, FatalError> {
        todo!()
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

    unsafe fn optimize(
        _: &CodegenContext<Self>,
        _: DiagCtxtHandle<'_>,
        _: &ModuleCodegen<Self::Module>,
        _: &ModuleConfig,
    ) -> Result<(), FatalError> {
        // TODO: Implement
        Ok(())
    }

    unsafe fn optimize_thin(
        _cgcx: &CodegenContext<Self>,
        thin_module: ThinModule<Self>,
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        let module = ModuleCodegen {
            module_llvm: spirv_tools::binary::to_binary(thin_module.data())
                .unwrap()
                .to_vec(),
            name: thin_module.name().to_string(),
            kind: ModuleKind::Regular,
        };
        Ok(module)
    }

    fn optimize_fat(
        _: &CodegenContext<Self>,
        _: &mut ModuleCodegen<Self::Module>,
    ) -> Result<(), FatalError> {
        todo!()
    }

    unsafe fn codegen(
        cgcx: &CodegenContext<Self>,
        _diag_handler: DiagCtxtHandle<'_>,
        module: ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
    ) -> Result<CompiledModule, FatalError> {
        let path = cgcx
            .output_filenames
            .temp_path(OutputType::Object, Some(&module.name));
        // Note: endianness doesn't matter, readers deduce endianness from magic header.
        let spirv_module = spirv_tools::binary::from_binary(&module.module_llvm);
        File::create(&path)
            .unwrap()
            .write_all(spirv_module)
            .unwrap();
        Ok(CompiledModule {
            name: module.name,
            kind: module.kind,
            object: Some(path),
            dwarf_object: None,
            bytecode: None,
            assembly: None,
            llvm_ir: None,
        })
    }

    fn prepare_thin(
        module: ModuleCodegen<Self::Module>,
        _want_summary: bool,
    ) -> (String, Self::ThinBuffer) {
        (module.name, SpirvThinBuffer(module.module_llvm))
    }

    fn serialize_module(module: ModuleCodegen<Self::Module>) -> (String, Self::ModuleBuffer) {
        (module.name, SpirvModuleBuffer(module.module_llvm))
    }
}

impl ExtraBackendMethods for SpirvCodegenBackend {
    fn codegen_allocator(
        &self,
        _: TyCtxt<'_>,
        _: &str,
        _: AllocatorKind,
        _: AllocatorKind,
    ) -> Self::Module {
        todo!()
    }

    fn compile_codegen_unit(
        &self,
        tcx: TyCtxt<'_>,
        cgu_name: Symbol,
    ) -> (ModuleCodegen<Self::Module>, u64) {
        let _timer = tcx
            .prof
            .verbose_generic_activity_with_arg("codegen_module", cgu_name.to_string());

        // TODO: Do dep_graph stuff
        let cgu = tcx.codegen_unit(cgu_name);

        let cx = CodegenCx::new(tcx, cgu);
        let do_codegen = || {
            let mono_items = cx.codegen_unit.items_in_deterministic_order(cx.tcx);

            if let Some(dir) = &cx.codegen_args.dump_mir {
                dump_mir(tcx, mono_items.as_slice(), &dir.join(cgu_name.to_string()));
            }

            for &(mono_item, mono_item_data) in mono_items.iter() {
                if let MonoItem::Fn(instance) = mono_item {
                    if is_blocklisted_fn(cx.tcx, &cx.sym, instance) {
                        continue;
                    }
                }
                mono_item.predefine::<Builder<'_, '_>>(
                    &cx,
                    mono_item_data.linkage,
                    mono_item_data.visibility,
                );
            }

            // ... and now that we have everything pre-defined, fill out those definitions.
            for &(mono_item, _) in mono_items.iter() {
                if let MonoItem::Fn(instance) = mono_item {
                    if is_blocklisted_fn(cx.tcx, &cx.sym, instance) {
                        continue;
                    }
                }
                mono_item.define::<Builder<'_, '_>>(&cx);
            }

            if let Some(_entry) = maybe_create_entry_wrapper::<Builder<'_, '_>>(&cx) {
                // attributes::sanitize(&cx, SanitizerSet::empty(), entry);
            }
        };
        if let Some(path) = &cx.codegen_args.dump_module_on_panic {
            let module_dumper = DumpModuleOnPanic { cx: &cx, path };
            with_no_trimmed_paths!(do_codegen());
            drop(module_dumper);
        } else {
            with_no_trimmed_paths!(do_codegen());
        }
        let spirv_module = cx.finalize_module().assemble();

        (
            ModuleCodegen {
                name: cgu_name.to_string(),
                module_llvm: spirv_module,
                kind: ModuleKind::Regular,
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
    cx: &'cx CodegenCx<'tcx>,
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
#[no_mangle]
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
