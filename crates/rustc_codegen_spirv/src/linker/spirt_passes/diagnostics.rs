use crate::custom_decorations::{SpanRegenerator, SrcLocDecoration};
use rustc_data_structures::fx::FxIndexSet;
use rustc_errors::EmissionGuarantee;
use rustc_session::Session;
use rustc_span::Span;
use smallvec::SmallVec;
use spirt::visit::{InnerVisit, Visitor};
use spirt::{
    Attr, AttrSet, Const, ConstKind, Context, DataInstDef, DataInstForm, DataInstKind, DbgSrcLoc,
    Diag, DiagLevel, ExportKey, Exportee, Func, GlobalVar, InternedStr, Module, Type, spv,
};
use std::{mem, str};

pub(crate) struct ReportedDiagnostics {
    pub rustc_errors_guarantee: rustc_errors::ErrorGuaranteed,
    pub any_errors_were_spirt_bugs: bool,
}

pub(crate) fn report_diagnostics(
    sess: &Session,
    module: &Module,
) -> Result<(), ReportedDiagnostics> {
    let cx = &module.cx();

    let mut reporter = DiagnosticReporter {
        sess,

        cx,

        module,

        seen_attrs: FxIndexSet::default(),
        seen_types: FxIndexSet::default(),
        seen_consts: FxIndexSet::default(),
        seen_global_vars: FxIndexSet::default(),
        seen_funcs: FxIndexSet::default(),

        use_stack: SmallVec::new(),
        span_regen: SpanRegenerator::new_spirt(sess.source_map(), module),
        overall_result: Ok(()),
        any_spirt_bugs: false,
    };
    for (export_key, exportee) in &module.exports {
        assert_eq!(reporter.use_stack.len(), 0);

        if let &Exportee::Func(func) = exportee {
            let func_decl = &module.funcs[func];
            reporter.use_stack.push(UseOrigin::IntraFunc {
                func_attrs: func_decl.attrs,
                special_func: Some(SpecialFunc::Exported(export_key)),
                inst_attrs: AttrSet::default(),
                origin: IntraFuncUseOrigin::Other,
            });
            if reporter.seen_funcs.insert(func) {
                reporter.visit_func_decl(func_decl);
            }
            // NOTE(eddyb) this is visited last, so that uses of the interface
            // variables don't lack relevant context from the function body.
            export_key.inner_visit_with(&mut reporter);
            reporter.use_stack.pop();
        }
        export_key.inner_visit_with(&mut reporter);
        exportee.inner_visit_with(&mut reporter);
    }

    reporter
        .overall_result
        .map_err(|rustc_errors_guarantee| ReportedDiagnostics {
            rustc_errors_guarantee,
            any_errors_were_spirt_bugs: reporter.any_spirt_bugs,
        })
}

// FIXME(eddyb) this looks a lot like `ReachableUseCollector`, maybe some
// automation should be built around "deep visitors" in general?
struct DiagnosticReporter<'a> {
    sess: &'a Session,

    cx: &'a Context,

    module: &'a Module,

    seen_attrs: FxIndexSet<AttrSet>,
    seen_types: FxIndexSet<Type>,
    seen_consts: FxIndexSet<Const>,
    seen_global_vars: FxIndexSet<GlobalVar>,
    seen_funcs: FxIndexSet<Func>,

    use_stack: SmallVec<[UseOrigin<'a>; 8]>,
    span_regen: SpanRegenerator<'a>,
    overall_result: crate::linker::Result<()>,
    any_spirt_bugs: bool,
}

enum UseOrigin<'a> {
    Global {
        kind: &'static &'static str,
        attrs: AttrSet,
    },
    IntraFunc {
        func_attrs: AttrSet,
        special_func: Option<SpecialFunc<'a>>,

        inst_attrs: AttrSet,
        origin: IntraFuncUseOrigin,
    },
}

#[derive(Copy, Clone)]
enum SpecialFunc<'a> {
    /// This function is exported from the `Module` (likely an entry-point).
    Exported(&'a ExportKey),

    /// This function doesn't have its own `FuncDecl`, but rather is an inlined
    /// callee (i.e. through `DbgSrcLoc`'s `inlined_callee_name_and_call_site`).
    Inlined { callee_name: InternedStr },
}

#[derive(Copy, Clone)]
enum IntraFuncUseOrigin {
    CallCallee,
    Other,
}

impl SpanRegenerator<'_> {
    fn dbg_src_loc_to_rustc_span_and_outer_scope<'a>(
        &mut self,
        cx: &Context,
        dbg_src_loc: Option<DbgSrcLoc>,
    ) -> (Span, Option<UseOrigin<'a>>) {
        let (span, outer_scope) = match dbg_src_loc {
            Some(DbgSrcLoc {
                file_path,
                start_line_col: (line_start, col_start),
                end_line_col: (line_end, col_end),
                inlined_callee_name_and_call_site,
            }) => (
                self.src_loc_to_rustc(SrcLocDecoration {
                    file_name: &cx[file_path],
                    line_start,
                    col_start,
                    line_end,
                    col_end,
                }),
                inlined_callee_name_and_call_site.map(|(callee_name, call_site_attrs)| {
                    UseOrigin::IntraFunc {
                        func_attrs: AttrSet::default(),
                        special_func: Some(SpecialFunc::Inlined { callee_name }),
                        inst_attrs: call_site_attrs,
                        origin: IntraFuncUseOrigin::Other,
                    }
                }),
            ),
            None => (None, None),
        };
        (span.unwrap_or_default(), outer_scope)
    }
}

impl UseOrigin<'_> {
    fn dbg_src_loc(&self, cx: &Context) -> Option<DbgSrcLoc> {
        match *self {
            Self::Global { attrs, .. } => attrs.dbg_src_loc(cx),
            Self::IntraFunc {
                func_attrs,
                inst_attrs,
                ..
            } => (inst_attrs.dbg_src_loc(cx)).or_else(|| func_attrs.dbg_src_loc(cx)),
        }
    }

    fn note<G: EmissionGuarantee>(
        &self,
        cx: &Context,
        span_regen: &mut SpanRegenerator<'_>,
        err: &mut rustc_errors::Diag<'_, G>,
    ) {
        let wk = &super::SpvSpecWithExtras::get().well_known;

        let name_from_attrs = |attrs: AttrSet, kind| {
            cx[attrs]
                .attrs
                .iter()
                .find_map(|attr| match attr {
                    Attr::SpvAnnotation(spv_inst) if spv_inst.opcode == wk.OpName => {
                        Some(super::decode_spv_lit_str_with(&spv_inst.imms, |name| {
                            format!("`{name}`")
                        }))
                    }
                    _ => None,
                })
                .unwrap_or_else(|| format!("unnamed {kind}"))
        };
        let note = match self {
            &Self::Global { kind, attrs } => {
                format!("used by {}", name_from_attrs(attrs, *kind))
            }
            Self::IntraFunc {
                func_attrs,
                special_func,
                inst_attrs: _,
                origin,
            } => {
                let func_desc = special_func
                    .map(|special_func| match special_func {
                        SpecialFunc::Exported(&ExportKey::LinkName(name)) => {
                            format!("function export `{}`", &cx[name])
                        }
                        SpecialFunc::Exported(ExportKey::SpvEntryPoint { imms, .. }) => {
                            match imms[..] {
                                [em @ spv::Imm::Short(em_kind, _), ref name_imms @ ..] => {
                                    assert_eq!(em_kind, wk.ExecutionModel);
                                    let em =
                                        spv::print::operand_from_imms([em]).concat_to_plain_text();
                                    super::decode_spv_lit_str_with(name_imms, |name| {
                                        format!(
                                            "{} entry-point `{name}`",
                                            em.strip_prefix("ExecutionModel.").unwrap()
                                        )
                                    })
                                }
                                _ => unreachable!(),
                            }
                        }
                        SpecialFunc::Inlined { callee_name } => match &cx[callee_name] {
                            "" => "unnamed function".into(),
                            callee_name => format!("`{callee_name}`"),
                        },
                    })
                    .unwrap_or_else(|| name_from_attrs(*func_attrs, "function"));
                match origin {
                    IntraFuncUseOrigin::CallCallee => format!("called by {func_desc}"),
                    IntraFuncUseOrigin::Other => format!("used from within {func_desc}"),
                }
            }
        };

        let (span, outer_scope) =
            span_regen.dbg_src_loc_to_rustc_span_and_outer_scope(cx, self.dbg_src_loc(cx));
        err.span_note(span, note);
        if let Some(outer_scope) = outer_scope {
            outer_scope.note(cx, span_regen, err);
        }
    }
}

impl DiagnosticReporter<'_> {
    fn diag_build_note_all_and_emit<G: EmissionGuarantee>(
        &mut self,
        attrs: AttrSet,
        f: impl FnOnce(rustc_errors::DiagCtxtHandle<'_>, Span) -> rustc_errors::Diag<'_, G>,
        g: impl FnOnce(G::EmitResult) -> crate::linker::Result<()>,
    ) {
        // Split off the last entry in `self.use_stack` if it's for the definition
        // that `attrs` come from - this should almost always be the case, except
        // for instructions inside a function body, or visitor bugs.
        let (current_def, use_stack_for_def) = self
            .use_stack
            .split_last()
            .filter(
                |(
                    UseOrigin::Global {
                        attrs: use_attrs, ..
                    }
                    | UseOrigin::IntraFunc {
                        func_attrs: use_attrs,
                        ..
                    },
                    _,
                )| *use_attrs == attrs,
            )
            .map_or((None, &self.use_stack[..]), |(current, stack)| {
                (Some(current), stack)
            });

        let (def_span, def_outer_scope) =
            self.span_regen.dbg_src_loc_to_rustc_span_and_outer_scope(
                self.cx,
                current_def
                    .and_then(|def| def.dbg_src_loc(self.cx))
                    .or_else(|| {
                        // If there's no clear source for the span, try to get
                        // it from the same attrs as the diagnostic, which could
                        // be missing from `use_stack` in some edge cases
                        // (e.g. function parameters).
                        attrs.dbg_src_loc(self.cx)
                    }),
            );

        let mut diag = f(self.sess.dcx(), def_span);
        if let Some(def_outer_scope) = def_outer_scope {
            def_outer_scope.note(self.cx, &mut self.span_regen, &mut diag);
        }
        for use_origin in use_stack_for_def.iter().rev() {
            use_origin.note(self.cx, &mut self.span_regen, &mut diag);
        }
        self.overall_result = self.overall_result.and(g(diag.emit()));
    }

    fn report_from_attrs(&mut self, attrs: AttrSet) {
        for diag in attrs.diags(self.cx) {
            let Diag { level, message } = diag;

            let prefix = match level {
                DiagLevel::Bug(location) => {
                    let location = location.to_string();
                    let location = match location.rsplit_once("/src/") {
                        Some((_path_prefix, intra_src)) => intra_src,
                        None => &location,
                    };
                    format!("SPIR-T BUG [{location}] ")
                }
                DiagLevel::Error | DiagLevel::Warning => "".to_string(),
            };
            let (deps, msg) = spirt::print::Plan::for_root(self.cx, message)
                .pretty_print_deps_and_root_separately();

            let deps = deps.to_string();
            let suffix = if !deps.is_empty() {
                format!("\n  where\n    {}", deps.replace('\n', "\n    "))
            } else {
                "".to_string()
            };

            let msg = [prefix, msg.to_string(), suffix].concat();
            match level {
                DiagLevel::Bug(_) | DiagLevel::Error => self.diag_build_note_all_and_emit(
                    attrs,
                    |dcx, def_span| dcx.struct_span_err(def_span, msg),
                    Err,
                ),
                DiagLevel::Warning => self.diag_build_note_all_and_emit(
                    attrs,
                    |dcx, def_span| dcx.struct_span_warn(def_span, msg),
                    Ok,
                ),
            }
            self.any_spirt_bugs = matches!(level, DiagLevel::Bug(_));
        }
    }
}

impl<'a> Visitor<'a> for DiagnosticReporter<'a> {
    fn visit_attr_set_use(&mut self, attrs: AttrSet) {
        // HACK(eddyb) this avoids reporting the same diagnostics more than once.
        if self.seen_attrs.insert(attrs) {
            self.report_from_attrs(attrs);
        }
    }
    fn visit_type_use(&mut self, ty: Type) {
        if self.seen_types.insert(ty) {
            let ty_def = &self.cx[ty];
            self.use_stack.push(UseOrigin::Global {
                kind: &"type",
                attrs: ty_def.attrs,
            });
            self.visit_type_def(ty_def);
            self.use_stack.pop();
        }
    }
    fn visit_const_use(&mut self, ct: Const) {
        if self.seen_consts.insert(ct) {
            let ct_def = &self.cx[ct];
            match ct_def.kind {
                // HACK(eddyb) don't push an `UseOrigin` for `GlobalVar` pointers.
                ConstKind::PtrToGlobalVar(_) if ct_def.attrs == AttrSet::default() => {
                    self.visit_const_def(ct_def);
                }
                _ => {
                    self.use_stack.push(UseOrigin::Global {
                        kind: &"constant",
                        attrs: ct_def.attrs,
                    });
                    self.visit_const_def(ct_def);
                    self.use_stack.pop();
                }
            }
        }
    }
    fn visit_data_inst_form_use(&mut self, data_inst_form: DataInstForm) {
        // NOTE(eddyb) this contains no deduplication because each `DataInstDef`
        // will have any diagnostics reported separately.
        self.visit_data_inst_form_def(&self.cx[data_inst_form]);
    }

    fn visit_global_var_use(&mut self, gv: GlobalVar) {
        if self.seen_global_vars.insert(gv) {
            let gv_decl = &self.module.global_vars[gv];
            self.use_stack.push(UseOrigin::Global {
                // FIXME(eddyb) may be a `&CONST`, or an interface variable,
                // not necessarily an user variable, so this could be confusing.
                kind: &"global variable",
                attrs: gv_decl.attrs,
            });
            self.visit_global_var_decl(gv_decl);
            self.use_stack.pop();
        }
    }
    fn visit_func_use(&mut self, func: Func) {
        if self.seen_funcs.insert(func) {
            let func_decl = &self.module.funcs[func];
            self.use_stack.push(UseOrigin::IntraFunc {
                func_attrs: func_decl.attrs,
                special_func: None,
                inst_attrs: AttrSet::default(),
                origin: IntraFuncUseOrigin::Other,
            });
            self.visit_func_decl(func_decl);
            self.use_stack.pop();
        }
    }

    fn visit_data_inst_def(&mut self, data_inst_def: &'a DataInstDef) {
        let replace_origin = |this: &mut Self, new_origin| match this.use_stack.last_mut() {
            Some(UseOrigin::IntraFunc { origin, .. }) => mem::replace(origin, new_origin),
            _ => unreachable!(),
        };

        match self.use_stack.last_mut() {
            Some(UseOrigin::IntraFunc { inst_attrs, .. }) => {
                *inst_attrs = data_inst_def.attrs;
            }
            _ => unreachable!(),
        }

        if let DataInstKind::FuncCall(func) = self.cx[data_inst_def.form].kind {
            // HACK(eddyb) visit `func` early, to control its `use_stack`, with
            // the later visit from `inner_visit_with` ignored as a duplicate.
            let old_origin = replace_origin(self, IntraFuncUseOrigin::CallCallee);
            self.visit_func_use(func);
            replace_origin(self, old_origin);
        }

        data_inst_def.inner_visit_with(self);
    }
}
