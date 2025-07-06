#[cfg(test)]
mod test;

mod dce;
mod destructure_composites;
mod duplicates;
mod entry_interface;
mod import_export_link;
mod inline;
mod ipo;
mod mem2reg;
mod param_weakening;
mod peephole_opts;
mod simple_passes;
mod specializer;
mod spirt_passes;
mod zombies;

use std::borrow::Cow;

use crate::codegen_cx::{ModuleOutputType, SpirvMetadata};
use crate::custom_decorations::{CustomDecoration, SrcLocDecoration, ZombieDecoration};
use crate::custom_insts;
use either::Either;
use rspirv::binary::{Assemble, Consumer};
use rspirv::dr::{Block, Loader, Module, ModuleHeader, Operand};
use rspirv::spirv::{Op, StorageClass, Word};
use rustc_data_structures::fx::FxHashMap;
use rustc_errors::ErrorGuaranteed;
use rustc_session::Session;
use rustc_session::config::OutputFilenames;
use std::collections::BTreeMap;
use std::ffi::{OsStr, OsString};
use std::path::PathBuf;

pub type Result<T> = std::result::Result<T, ErrorGuaranteed>;

#[derive(Default)]
pub struct Options {
    pub compact_ids: bool,
    pub dce: bool,
    pub early_report_zombies: bool,
    pub infer_storage_classes: bool,
    pub structurize: bool,
    pub spirt_passes: Vec<String>,

    pub abort_strategy: Option<String>,
    pub module_output_type: ModuleOutputType,

    pub spirv_metadata: SpirvMetadata,

    /// Whether to preserve `LinkageAttributes "..." Export` decorations,
    /// even after resolving imports to exports.
    ///
    /// **Note**: currently only used for unit testing, and not exposed elsewhere.
    pub keep_link_exports: bool,

    // NOTE(eddyb) these are debugging options that used to be env vars
    // (for more information see `docs/src/codegen-args.md`).
    pub dump_post_merge: Option<PathBuf>,
    pub dump_pre_inline: Option<PathBuf>,
    pub dump_post_inline: Option<PathBuf>,
    pub dump_post_split: Option<PathBuf>,
    pub dump_spirt_passes: Option<PathBuf>,
    pub spirt_strip_custom_debuginfo_from_dumps: bool,
    pub spirt_keep_debug_sources_in_dumps: bool,
    pub spirt_keep_unstructured_cfg_in_dumps: bool,
    pub specializer_dump_instances: Option<PathBuf>,
}

pub enum LinkResult {
    SingleModule(Box<Module>),
    MultipleModules {
        /// The "file stem" key is computed from the "entry name" in the value
        /// (through `sanitize_filename`, replacing invalid chars with `-`),
        /// but it's used as the map key because it *has to* be unique, even if
        /// lossy sanitization could have erased distinctions between entry names.
        file_stem_to_entry_name_and_module: BTreeMap<OsString, (String, Module)>,
    },
}

fn id(header: &mut ModuleHeader) -> Word {
    let result = header.bound;
    header.bound += 1;
    result
}

fn apply_rewrite_rules<'a>(
    rewrite_rules: &FxHashMap<Word, Word>,
    blocks: impl IntoIterator<Item = &'a mut Block>,
) {
    let all_ids_mut = blocks
        .into_iter()
        .flat_map(|b| b.label.iter_mut().chain(b.instructions.iter_mut()))
        .flat_map(|inst| {
            inst.result_id
                .iter_mut()
                .chain(inst.result_type.iter_mut())
                .chain(
                    inst.operands
                        .iter_mut()
                        .filter_map(|op| op.id_ref_any_mut()),
                )
        });
    for id in all_ids_mut {
        if let Some(&rewrite) = rewrite_rules.get(id) {
            *id = rewrite;
        }
    }
}

fn get_names(module: &Module) -> FxHashMap<Word, &str> {
    let entry_names = module
        .entry_points
        .iter()
        .filter(|i| i.class.opcode == Op::EntryPoint)
        .map(|i| {
            (
                i.operands[1].unwrap_id_ref(),
                i.operands[2].unwrap_literal_string(),
            )
        });
    let debug_names = module
        .debug_names
        .iter()
        .filter(|i| i.class.opcode == Op::Name)
        .map(|i| {
            (
                i.operands[0].unwrap_id_ref(),
                i.operands[1].unwrap_literal_string(),
            )
        });
    // items later on take priority
    entry_names.chain(debug_names).collect()
}

fn get_name<'a>(names: &FxHashMap<Word, &'a str>, id: Word) -> Cow<'a, str> {
    names.get(&id).map_or_else(
        || Cow::Owned(format!("Unnamed function ID %{id}")),
        |&s| Cow::Borrowed(s),
    )
}

impl Options {
    // FIXME(eddyb) using a method on this type seems a bit sketchy.
    fn spirt_cleanup_for_dumping(&self, module: &mut spirt::Module) {
        if self.spirt_strip_custom_debuginfo_from_dumps {
            spirt_passes::debuginfo::convert_custom_debuginfo_to_spv(module);
        }
        if !self.spirt_keep_debug_sources_in_dumps {
            const DOTS: &str = "⋯";
            let dots_interned_str = module.cx().intern(DOTS);
            let spirt::ModuleDebugInfo::Spv(debuginfo) = &mut module.debug_info;
            for sources in debuginfo.source_languages.values_mut() {
                for file in sources.file_contents.values_mut() {
                    *file = DOTS.into();
                }
                sources.file_contents.insert(
                    dots_interned_str,
                    "sources hidden, to show them use \
                     `RUSTGPU_CODEGEN_ARGS=--spirt-keep-debug-sources-in-dumps`"
                        .into(),
                );
            }
        }
    }
}

pub fn link(
    sess: &Session,
    mut inputs: Vec<Module>,
    opts: &Options,
    outputs: &OutputFilenames,
    disambiguated_crate_name_for_dumps: &OsStr,
) -> Result<LinkResult> {
    // HACK(eddyb) this is defined here to allow SPIR-T pretty-printing to apply
    // to SPIR-V being dumped, outside of e.g. `--dump-spirt-passes`.
    // FIXME(eddyb) this isn't used everywhere, sadly - to find those, search
    // elsewhere for `.assemble()` and/or `spirv_tools::binary::from_binary`.
    let spv_module_to_spv_words_and_spirt_module = |spv_module: &Module| {
        let spv_words;
        let spv_bytes = {
            let _timer = sess.timer("assemble-to-spv_bytes-for-spirt");
            spv_words = spv_module.assemble();
            // FIXME(eddyb) this is wastefully cloning all the bytes, but also
            // `spirt::Module` should have a method that takes `Vec<u32>`.
            spirv_tools::binary::from_binary(&spv_words).to_vec()
        };

        // FIXME(eddyb) should've really been "spirt::Module::lower_from_spv_bytes".
        let lower_from_spv_timer = sess.timer("spirt::Module::lower_from_spv_file");
        let cx = std::rc::Rc::new(spirt::Context::new());
        crate::custom_insts::register_to_spirt_context(&cx);
        (
            spv_words,
            spirt::Module::lower_from_spv_bytes(cx, spv_bytes),
            // HACK(eddyb) this is only returned for `SpirtDumpGuard`.
            lower_from_spv_timer,
        )
    };

    // FIXME(eddyb) deduplicate with `SpirtDumpGuard`.
    let dump_spv_and_spirt = |spv_module: &Module, dump_file_path_stem: PathBuf| {
        let (spv_words, spirt_module_or_err, _) =
            spv_module_to_spv_words_and_spirt_module(spv_module);
        std::fs::write(
            dump_file_path_stem.with_extension("spv"),
            spirv_tools::binary::from_binary(&spv_words),
        )
        .unwrap();

        // FIXME(eddyb) reify SPIR-V -> SPIR-T errors so they're easier to debug.
        if let Ok(mut module) = spirt_module_or_err {
            // HACK(eddyb) avoid pretty-printing massive amounts of unused SPIR-T.
            spirt::passes::link::minimize_exports(&mut module, |export_key| {
                matches!(export_key, spirt::ExportKey::SpvEntryPoint { .. })
            });

            opts.spirt_cleanup_for_dumping(&mut module);

            let pretty = spirt::print::Plan::for_module(&module).pretty_print();

            // FIXME(eddyb) don't allocate whole `String`s here.
            std::fs::write(
                dump_file_path_stem.with_extension("spirt"),
                pretty.to_string(),
            )
            .unwrap();
            std::fs::write(
                dump_file_path_stem.with_extension("spirt.html"),
                pretty
                    .render_to_html()
                    .with_dark_mode_support()
                    .to_html_doc(),
            )
            .unwrap();
        }
    };

    let mut output = {
        let _timer = sess.timer("link_merge");
        // shift all the ids
        let mut bound = inputs[0].header.as_ref().unwrap().bound - 1;
        let version = inputs[0].header.as_ref().unwrap().version();

        for module in inputs.iter_mut().skip(1) {
            simple_passes::shift_ids(module, bound);
            bound += module.header.as_ref().unwrap().bound - 1;
            let this_version = module.header.as_ref().unwrap().version();
            if version != this_version {
                return Err(sess.dcx().err(format!(
                    "cannot link two modules with different SPIR-V versions: v{}.{} and v{}.{}",
                    version.0, version.1, this_version.0, this_version.1
                )));
            }
        }

        // merge the binaries
        let mut loader = Loader::new();

        for module in inputs {
            module.all_inst_iter().for_each(|inst| {
                loader.consume_instruction(inst.clone());
            });
        }

        let mut output = loader.module();
        let mut header = ModuleHeader::new(bound + 1);
        header.set_version(version.0, version.1);
        header.generator = 0x001B_0000;
        output.header = Some(header);
        output
    };

    if let Some(dir) = &opts.dump_post_merge {
        dump_spv_and_spirt(&output, dir.join(disambiguated_crate_name_for_dumps));
    }

    // remove duplicates (https://github.com/KhronosGroup/SPIRV-Tools/blob/e7866de4b1dc2a7e8672867caeb0bdca49f458d3/source/opt/remove_duplicates_pass.cpp)
    {
        let _timer = sess.timer("link_remove_duplicates");
        duplicates::remove_duplicate_extensions(&mut output);
        duplicates::remove_duplicate_capabilities(&mut output);
        duplicates::remove_duplicate_ext_inst_imports(&mut output);
        duplicates::remove_duplicate_types(&mut output);
        // jb-todo: strip identical OpDecoration / OpDecorationGroups
    }

    // find import / export pairs
    {
        let _timer = sess.timer("link_find_pairs");
        import_export_link::run(opts, sess, &mut output)?;
    }

    {
        let _timer = sess.timer("link_fragment_inst_check");
        simple_passes::check_fragment_insts(sess, &output)?;
    }

    // HACK(eddyb) this has to run before the `report_and_remove_zombies` pass,
    // so that any zombies that are passed as call arguments, but eventually unused,
    // won't be (incorrectly) considered used.
    {
        let _timer = sess.timer("link_remove_unused_params");
        output = param_weakening::remove_unused_params(output);
    }

    if opts.early_report_zombies {
        // HACK(eddyb) `report_and_remove_zombies` is bad at determining whether
        // some things are dead (such as whole blocks), and there's no reason to
        // *not* run DCE, given SPIR-T exists and makes DCE mandatory, but we're
        // still only going to do the minimum necessary ("block ordering").
        {
            let _timer = sess.timer("link_block_ordering_pass-before-report_and_remove_zombies");
            for func in &mut output.functions {
                simple_passes::block_ordering_pass(func);
            }
        }

        let _timer = sess.timer("link_report_and_remove_zombies");
        zombies::report_and_remove_zombies(sess, &mut output)?;
    }

    if opts.infer_storage_classes {
        // HACK(eddyb) this is not the best approach, but storage class inference
        // can still fail in entirely legitimate ways (i.e. mismatches in zombies).
        if !opts.early_report_zombies {
            let _timer = sess.timer("link_dce-before-specialize_generic_storage_class");
            dce::dce(&mut output);
        }

        let _timer = sess.timer("specialize_generic_storage_class");
        // HACK(eddyb) `specializer` requires functions' blocks to be in RPO order
        // (i.e. `block_ordering_pass`) - this could be relaxed by using RPO visit
        // inside `specializer`, but this is easier.
        for func in &mut output.functions {
            simple_passes::block_ordering_pass(func);
        }
        output = specializer::specialize(opts, output, specializer::SimpleSpecialization {
            specialize_operand: |operand| {
                matches!(operand, Operand::StorageClass(StorageClass::Generic))
            },

            // NOTE(eddyb) this can be anything that is guaranteed to pass
            // validation - there are no constraints so this is either some
            // unused pointer, or perhaps one created using `OpConstantNull`
            // and simply never mixed with pointers that have a storage class.
            // It would be nice to use `Generic` itself here so that we leave
            // some kind of indication of it being unconstrained, but `Generic`
            // requires additional capabilities, so we use `Function` instead.
            // TODO(eddyb) investigate whether this can end up in a pointer
            // type that's the value of a module-scoped variable, and whether
            // `Function` is actually invalid! (may need `Private`)
            concrete_fallback: Operand::StorageClass(StorageClass::Function),
        });
    }

    // NOTE(eddyb) with SPIR-T, we can do `mem2reg` before inlining, too!
    {
        if opts.dce {
            let _timer = sess.timer("link_dce-before-inlining");
            dce::dce(&mut output);
        }

        let _timer = sess.timer("link_block_ordering_pass_and_mem2reg-before-inlining");
        let mut pointer_to_pointee = FxHashMap::default();
        let mut constants = FxHashMap::default();
        let mut u32 = None;
        for inst in &output.types_global_values {
            match inst.class.opcode {
                Op::TypePointer => {
                    pointer_to_pointee
                        .insert(inst.result_id.unwrap(), inst.operands[1].unwrap_id_ref());
                }
                Op::TypeInt
                    if inst.operands[0].unwrap_literal_bit32() == 32
                        && inst.operands[1].unwrap_literal_bit32() == 0 =>
                {
                    assert!(u32.is_none());
                    u32 = Some(inst.result_id.unwrap());
                }
                Op::Constant if u32.is_some() && inst.result_type == u32 => {
                    let value = inst.operands[0].unwrap_literal_bit32();
                    constants.insert(inst.result_id.unwrap(), value);
                }
                _ => {}
            }
        }
        for func in &mut output.functions {
            simple_passes::block_ordering_pass(func);
            // Note: mem2reg requires functions to be in RPO order (i.e. block_ordering_pass)
            mem2reg::mem2reg(
                output.header.as_mut().unwrap(),
                &mut output.types_global_values,
                &pointer_to_pointee,
                &constants,
                func,
            );
            destructure_composites::destructure_composites(func);
        }
    }

    if opts.dce {
        let _timer =
            sess.timer("link_dce-and-remove_duplicate_debuginfo-after-mem2reg-before-inlining");
        dce::dce(&mut output);
        duplicates::remove_duplicate_debuginfo(&mut output);
    }

    if let Some(dir) = &opts.dump_pre_inline {
        dump_spv_and_spirt(&output, dir.join(disambiguated_crate_name_for_dumps));
    }

    {
        let _timer = sess.timer("link_inline");
        inline::inline(sess, &mut output)?;
    }

    if opts.dce {
        let _timer = sess.timer("link_dce-after-inlining");
        dce::dce(&mut output);
    }

    // HACK(eddyb) this has to be after DCE, to not break SPIR-T w/ dead decorations.
    if let Some(dir) = &opts.dump_post_inline {
        dump_spv_and_spirt(&output, dir.join(disambiguated_crate_name_for_dumps));
    }

    {
        let _timer = sess.timer("link_block_ordering_pass_and_mem2reg-after-inlining");
        let mut pointer_to_pointee = FxHashMap::default();
        let mut constants = FxHashMap::default();
        let mut u32 = None;
        for inst in &output.types_global_values {
            match inst.class.opcode {
                Op::TypePointer => {
                    pointer_to_pointee
                        .insert(inst.result_id.unwrap(), inst.operands[1].unwrap_id_ref());
                }
                Op::TypeInt
                    if inst.operands[0].unwrap_literal_bit32() == 32
                        && inst.operands[1].unwrap_literal_bit32() == 0 =>
                {
                    assert!(u32.is_none());
                    u32 = Some(inst.result_id.unwrap());
                }
                Op::Constant if u32.is_some() && inst.result_type == u32 => {
                    let value = inst.operands[0].unwrap_literal_bit32();
                    constants.insert(inst.result_id.unwrap(), value);
                }
                _ => {}
            }
        }
        for func in &mut output.functions {
            simple_passes::block_ordering_pass(func);
            // Note: mem2reg requires functions to be in RPO order (i.e. block_ordering_pass)
            mem2reg::mem2reg(
                output.header.as_mut().unwrap(),
                &mut output.types_global_values,
                &pointer_to_pointee,
                &constants,
                func,
            );
            destructure_composites::destructure_composites(func);
        }
    }

    if opts.dce {
        let _timer =
            sess.timer("link_dce-and-remove_duplicate_debuginfo-after-mem2reg-after-inlining");
        dce::dce(&mut output);
        duplicates::remove_duplicate_debuginfo(&mut output);
    }

    // NOTE(eddyb) SPIR-T pipeline is entirely limited to this block.
    {
        let (spv_words, module_or_err, lower_from_spv_timer) =
            spv_module_to_spv_words_and_spirt_module(&output);
        let module = &mut module_or_err.map_err(|e| {
            let spv_path = outputs.temp_path_ext("spirt-lower-from-spv-input.spv", None);

            let was_saved_msg =
                match std::fs::write(&spv_path, spirv_tools::binary::from_binary(&spv_words)) {
                    Ok(()) => format!("was saved to {}", spv_path.display()),
                    Err(e) => format!("could not be saved: {e}"),
                };

            sess.dcx()
                .struct_err(format!("{e}"))
                .with_note("while lowering SPIR-V module to SPIR-T (spirt::spv::lower)")
                .with_note(format!("input SPIR-V module {was_saved_msg}"))
                .emit()
        })?;

        let mut dump_guard = SpirtDumpGuard {
            sess,
            linker_options: opts,
            outputs,
            disambiguated_crate_name_for_dumps,

            module,
            per_pass_module_for_dumping: vec![],
            any_spirt_bugs: false,
        };
        let module = &mut *dump_guard.module;
        // FIXME(eddyb) set the name into `dump_guard` to be able to access it on panic.
        let before_pass = |pass| sess.timer(pass);
        let mut after_pass = |pass, module: &spirt::Module, timer| {
            drop(timer);
            if opts.dump_spirt_passes.is_some() {
                dump_guard
                    .per_pass_module_for_dumping
                    .push((pass, module.clone()));
            }
        };
        // HACK(eddyb) don't dump the unstructured state if not requested, as
        // after SPIR-T 0.4.0 it's extremely verbose (due to def-use hermeticity).
        if opts.spirt_keep_unstructured_cfg_in_dumps || !opts.structurize {
            after_pass("lower_from_spv", module, lower_from_spv_timer);
        } else {
            drop(lower_from_spv_timer);
        }

        // NOTE(eddyb) this *must* run on unstructured CFGs, to do its job.
        // FIXME(eddyb) no longer relying on structurization, try porting this
        // to replace custom aborts in `Block`s and inject `ExitInvocation`s
        // after them (truncating the `Block` and/or parent region if necessary).
        {
            let _timer = before_pass(
                "spirt_passes::controlflow::convert_custom_aborts_to_unstructured_returns_in_entry_points",
            );
            spirt_passes::controlflow::convert_custom_aborts_to_unstructured_returns_in_entry_points(opts, module);
        }

        if opts.structurize {
            let timer = before_pass("spirt::legalize::structurize_func_cfgs");
            spirt::passes::legalize::structurize_func_cfgs(module);
            after_pass("structurize_func_cfgs", module, timer);
        }

        if !opts.spirt_passes.is_empty() {
            // FIXME(eddyb) why does this focus on functions, it could just be module passes??
            spirt_passes::run_func_passes(
                module,
                &opts.spirt_passes,
                |name, _module| before_pass(name),
                after_pass,
            );
        }

        {
            let _timer = before_pass("spirt_passes::diagnostics::report_diagnostics");
            spirt_passes::diagnostics::report_diagnostics(sess, opts, module).map_err(
                |spirt_passes::diagnostics::ReportedDiagnostics {
                     rustc_errors_guarantee,
                     any_errors_were_spirt_bugs,
                 }| {
                    dump_guard.any_spirt_bugs |= any_errors_were_spirt_bugs;
                    rustc_errors_guarantee
                },
            )?;
        }

        // Replace our custom debuginfo instructions just before lifting to SPIR-V.
        {
            let _timer = before_pass("spirt_passes::debuginfo::convert_custom_debuginfo_to_spv");
            spirt_passes::debuginfo::convert_custom_debuginfo_to_spv(module);
        }

        let spv_words = {
            let _timer = before_pass("spirt::Module::lift_to_spv_module_emitter");
            module.lift_to_spv_module_emitter().unwrap().words
        };
        // FIXME(eddyb) dump both SPIR-T and `spv_words` if there's an error here.
        output = {
            let _timer = sess.timer("parse-spv_words-from-spirt");
            let mut loader = Loader::new();
            rspirv::binary::parse_words(&spv_words, &mut loader).unwrap();
            loader.module()
        };
    }

    // Ensure that no references remain, to our custom "extended instruction set".
    for inst in &output.ext_inst_imports {
        assert_eq!(inst.class.opcode, Op::ExtInstImport);
        let ext_inst_set = inst.operands[0].unwrap_literal_string();
        if ext_inst_set.starts_with(custom_insts::CUSTOM_EXT_INST_SET_PREFIX) {
            let expected = &custom_insts::CUSTOM_EXT_INST_SET[..];
            if ext_inst_set == expected {
                return Err(sess.dcx().err(format!(
                    "`OpExtInstImport {ext_inst_set:?}` should not have been \
                         left around after SPIR-T passes"
                )));
            } else {
                return Err(sess.dcx().err(format!(
                    "unsupported `OpExtInstImport {ext_inst_set:?}`
                     (expected {expected:?} name - version mismatch?)"
                )));
            }
        }
    }

    // FIXME(eddyb) rewrite these passes to SPIR-T ones, so we don't have to
    // parse the output of `spirt::spv::lift` back into `rspirv` - also, for
    // multi-module, it's much simpler with SPIR-T, just replace `module.exports`
    // with a single-entry map, run `spirt::spv::lift` (or even `spirt::print`)
    // on `module`, then put back the full original `module.exports` map.
    {
        let _timer = sess.timer("peephole_opts");
        let types = peephole_opts::collect_types(&output);
        for func in &mut output.functions {
            peephole_opts::composite_construct(&types, func);
            peephole_opts::vector_ops(output.header.as_mut().unwrap(), &types, func);
            peephole_opts::bool_fusion(output.header.as_mut().unwrap(), &types, func);
        }
    }

    {
        let _timer = sess.timer("link_gather_all_interface_vars_from_uses");
        entry_interface::gather_all_interface_vars_from_uses(&mut output);
    }

    if opts.spirv_metadata == SpirvMetadata::NameVariables {
        let _timer = sess.timer("link_name_variables");
        simple_passes::name_variables_pass(&mut output);
    }

    {
        let _timer = sess.timer("link_sort_globals");
        simple_passes::sort_globals(&mut output);
    }

    let mut output = if opts.module_output_type == ModuleOutputType::Multiple {
        let mut file_stem_to_entry_name_and_module = BTreeMap::new();
        for (i, entry) in output.entry_points.iter().enumerate() {
            let mut module = output.clone();
            module.entry_points.clear();
            module.entry_points.push(entry.clone());
            let entry_name = entry.operands[2].unwrap_literal_string().to_string();
            let mut file_stem = OsString::from(
                sanitize_filename::sanitize_with_options(&entry_name, sanitize_filename::Options {
                    replacement: "-",
                    ..Default::default()
                })
                .replace("--", "-"),
            );
            // It's always possible to find an unambiguous `file_stem`, but it
            // may take two tries (or more, in bizzare/adversarial cases).
            let mut disambiguator = Some(i);
            loop {
                use std::collections::btree_map::Entry;
                match file_stem_to_entry_name_and_module.entry(file_stem) {
                    Entry::Vacant(entry) => {
                        entry.insert((entry_name, module));
                        break;
                    }
                    // FIXME(eddyb) false positive: `file_stem` was moved out of,
                    // so assigning it is necessary, but clippy doesn't know that.
                    #[allow(clippy::assigning_clones)]
                    Entry::Occupied(entry) => {
                        // FIXME(eddyb) there's no way to access the owned key
                        // passed to `BTreeMap::entry` from `OccupiedEntry`.
                        file_stem = entry.key().clone();
                        file_stem.push(".");
                        match disambiguator.take() {
                            Some(d) => file_stem.push(d.to_string()),
                            None => file_stem.push("next"),
                        }
                    }
                }
            }
        }
        LinkResult::MultipleModules {
            file_stem_to_entry_name_and_module,
        }
    } else {
        LinkResult::SingleModule(Box::new(output))
    };

    let output_module_iter = match &mut output {
        LinkResult::SingleModule(m) => Either::Left(std::iter::once((None, &mut **m))),
        LinkResult::MultipleModules {
            file_stem_to_entry_name_and_module,
        } => Either::Right(
            file_stem_to_entry_name_and_module
                .iter_mut()
                .map(|(file_stem, (_, m))| (Some(file_stem), m)),
        ),
    };
    for (file_stem, output) in output_module_iter {
        if let Some(dir) = &opts.dump_post_split {
            let mut file_name = disambiguated_crate_name_for_dumps.to_os_string();
            if let Some(file_stem) = file_stem {
                file_name.push(".");
                file_name.push(file_stem);
            }

            dump_spv_and_spirt(output, dir.join(file_name));
        }
        // Run DCE again, even if module_output_type == ModuleOutputType::Multiple - the first DCE ran before
        // structurization and mem2reg (for perf reasons), and mem2reg may remove references to
        // invalid types, so we need to DCE again.
        if opts.dce {
            let _timer = sess.timer("link_dce_2");
            dce::dce(output);
        }

        {
            let _timer = sess.timer("link_remove_duplicate_debuginfo");
            duplicates::remove_duplicate_debuginfo(output);
        }

        if opts.compact_ids {
            let _timer = sess.timer("link_compact_ids");
            // compact the ids https://github.com/KhronosGroup/SPIRV-Tools/blob/e02f178a716b0c3c803ce31b9df4088596537872/source/opt/compact_ids_pass.cpp#L43
            output.header.as_mut().unwrap().bound = simple_passes::compact_ids(output);
        };

        // FIXME(eddyb) convert these into actual `OpLine`s with a SPIR-T pass,
        // but that'd require keeping the modules in SPIR-T form (once lowered),
        // and never loading them back into `rspirv` once lifted back to SPIR-V.
        SrcLocDecoration::remove_all(output);

        // FIXME(eddyb) might make more sense to rewrite these away on SPIR-T.
        ZombieDecoration::remove_all(output);
    }

    Ok(output)
}

/// Helper for dumping SPIR-T on drop, which allows panics to also dump,
/// not just successful compilation (i.e. via `--dump-spirt-passes`).
struct SpirtDumpGuard<'a> {
    sess: &'a Session,
    linker_options: &'a Options,
    outputs: &'a OutputFilenames,
    disambiguated_crate_name_for_dumps: &'a OsStr,

    module: &'a mut spirt::Module,
    per_pass_module_for_dumping: Vec<(&'static str, spirt::Module)>,
    any_spirt_bugs: bool,
}

impl Drop for SpirtDumpGuard<'_> {
    fn drop(&mut self) {
        self.any_spirt_bugs |= std::thread::panicking();

        let mut dump_spirt_file_path =
            self.linker_options
                .dump_spirt_passes
                .as_ref()
                .map(|dump_dir| {
                    dump_dir
                        .join(self.disambiguated_crate_name_for_dumps)
                        .with_extension("spirt")
                });

        // FIXME(eddyb) this won't allow seeing the individual passes, but it's
        // better than nothing (theoretically the whole "SPIR-T pipeline" could
        // be put in a loop so that everything is redone with per-pass tracking,
        // but that requires keeping around e.g. the initial SPIR-V for longer,
        // and probably invoking the "SPIR-T pipeline" here, as looping is hard).
        if self.any_spirt_bugs && dump_spirt_file_path.is_none() {
            if self.per_pass_module_for_dumping.is_empty() {
                self.per_pass_module_for_dumping
                    .push(("", self.module.clone()));
            }
            dump_spirt_file_path = Some(self.outputs.temp_path_ext("spirt", None));
        }

        if let Some(dump_spirt_file_path) = &dump_spirt_file_path {
            for (_, module) in &mut self.per_pass_module_for_dumping {
                self.linker_options.spirt_cleanup_for_dumping(module);
            }

            // FIXME(eddyb) catch panics during pretty-printing itself, and
            // tell the user to use `--dump-spirt-passes` (and resolve the
            // second FIXME below so it does anything) - also, that may need
            // quieting the panic handler, likely controlled by a `thread_local!`
            // (while the panic handler is global), but that would be useful
            // for collecting a panic message (assuming any of this is worth it).
            // FIXME(eddyb) when per-pass versions are available, try building
            // plans for individual versions, or maybe repeat `Plan::for_versions`
            // without the last version if it initially panicked?
            let plan = spirt::print::Plan::for_versions(
                self.module.cx_ref(),
                self.per_pass_module_for_dumping
                    .iter()
                    .map(|(pass, module)| (format!("after {pass}"), module)),
            );
            let pretty = plan.pretty_print();

            // FIXME(eddyb) don't allocate whole `String`s here.
            std::fs::write(dump_spirt_file_path, pretty.to_string()).unwrap();
            std::fs::write(
                dump_spirt_file_path.with_extension("spirt.html"),
                pretty
                    .render_to_html()
                    .with_dark_mode_support()
                    .to_html_doc(),
            )
            .unwrap();
            if self.any_spirt_bugs {
                let mut note = self.sess.dcx().struct_note("SPIR-T bugs were encountered");
                note.help(format!(
                    "pretty-printed SPIR-T was saved to {}.html",
                    dump_spirt_file_path.display()
                ));
                if self.linker_options.dump_spirt_passes.is_none() {
                    note.help("re-run with `RUSTGPU_CODEGEN_ARGS=\"--dump-spirt-passes=$PWD\"` for more details");
                }
                note.note("pretty-printed SPIR-T is preferred when reporting Rust-GPU issues");
                note.emit();
            }
        }
    }
}
