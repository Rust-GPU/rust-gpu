use crate::abi::ConvSpirvType;
use crate::builder::Builder;
use crate::builder_spirv::{SpirvConst, SpirvValue, SpirvValueExt, SpirvValueKind};
use crate::codegen_cx::FmtArgsCtor;
use crate::custom_insts::CustomOp;
use crate::spirv_type::SpirvType;
use either::Either;
use itertools::Itertools;
use rspirv::dr::Operand;
use rspirv::spirv::{Op, Word};
use rustc_abi::BackendRepr;
use rustc_data_structures::fx::FxHashSet;
use rustc_middle::ty::Ty;
use rustc_middle::ty::layout::LayoutOf;
use rustc_span::def_id::DefId;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::cell::Cell;

// HACK(eddyb) Rust 2021 `panic!` always uses `format_args!`, even
// in the simple case that used to pass a `&str` constant, which
// would not remain reachable in the SPIR-V - but `format_args!` is
// more complex and neither immediate (`fmt::Arguments` is too big)
// nor simplified in MIR (e.g. promoted to a constant) in any way,
// so we have to try and remove the `fmt::Arguments::new` call here.
#[derive(Default)]
pub struct DecodedFormatArgs<'tcx> {
    /// If fully constant, the `pieces: &'a [&'static str]` input
    /// of `fmt::Arguments<'a>` (i.e. the strings between args).
    const_pieces: Option<SmallVec<[String; 2]>>,

    /// Original references for `fmt::Arguments<'a>` dynamic arguments,
    /// i.e. the `&'a T` passed to `fmt::rt::Argument::<'a>::new_*`,
    /// tracking the type `T` and `char` formatting specifier.
    ///
    /// E.g. for `format_args!("{a} {b:x}")` they'll be:
    /// * `&a` with `typeof a` and ' ',
    /// * `&b` with `typeof b` and 'x'
    ref_arg_ids_with_ty_and_spec: SmallVec<[(Word, Ty<'tcx>, char); 2]>,

    /// If `fmt::Arguments::new_v1_formatted` was used, this holds
    /// the length of the `&[fmt::rt::Placeholder]` slice, which
    /// currently cannot be directly supported, and therefore even
    /// if all of `ref_arg_ids_with_ty_and_spec` are printable,
    /// a much jankier fallback still has to be used, as it were:
    ///
    /// `format!("a{{0}}b{{1}}c\n  with {{…}} from: {}, {}", x, y)`
    /// (w/ `const_pieces = ["a", "b", "c"]` & `ref_args = [&x, &y]`).
    has_unknown_fmt_placeholder_to_args_mapping: Option<usize>,
}

pub struct FormatArgsNotRecognized(pub String);

pub type FormatArgsResult<'tcx> = Result<DecodedFormatArgs<'tcx>, FormatArgsNotRecognized>;

impl<'tcx> DecodedFormatArgs<'tcx> {
    pub fn try_decode_and_remove_format_args<'a>(
        builder: &mut Builder<'a, 'tcx>,
        args: &[SpirvValue],
        panic_def_id: Option<DefId>,
    ) -> FormatArgsResult<'tcx> {
        let mut decoded_format_args = DecodedFormatArgs::default();

        // HACK(eddyb) work around mutable borrowing conflicts.
        let cx = builder.cx;

        let const_u32_as_usize = |ct_id| match cx.builder.lookup_const_by_id(ct_id)? {
            SpirvConst::Scalar(x) => Some(u32::try_from(x).ok()? as usize),
            _ => None,
        };
        let const_slice_as_elem_ids = |ptr_id: Word, len: usize| {
            if let SpirvConst::PtrTo { pointee } = cx.builder.lookup_const_by_id(ptr_id)?
                && let SpirvConst::Composite(elems) = cx.builder.lookup_const_by_id(pointee)?
                && elems.len() == len
            {
                return Some(elems);
            }
            None
        };
        let const_ptr_to_composite_len = |ptr_id: Word| {
            if let SpirvConst::PtrTo { pointee } = cx.builder.lookup_const_by_id(ptr_id)?
                && let SpirvConst::Composite(elems) = cx.builder.lookup_const_by_id(pointee)?
            {
                return Some(elems.len());
            }
            None
        };
        let array_len_from_ptr_type = |ptr_ty_id: Word| {
            let pointee_ty_id = match cx.lookup_type(ptr_ty_id) {
                SpirvType::Pointer { pointee } => pointee,
                _ => return None,
            };
            let count = match cx.lookup_type(pointee_ty_id) {
                SpirvType::Array { count, .. } => count,
                _ => return None,
            };
            match count.kind {
                SpirvValueKind::Def(count_id) => const_u32_as_usize(count_id),
                _ => None,
            }
        };
        let const_slice_as_u8s = |ptr_id: Word, len: usize| {
            const_slice_as_elem_ids(ptr_id, len)?
                .iter()
                .map(|&id| u8::try_from(const_u32_as_usize(id)?).ok())
                .collect::<Option<Vec<u8>>>()
        };
        let const_str_as_utf8 = |&[str_ptr_id, str_len_id]: &[Word; 2]| {
            let len_bits = const_u32_as_usize(str_len_id)?;
            [Some(len_bits), (len_bits & 1 == 1).then_some(len_bits >> 1)]
                .into_iter()
                .flatten()
                .find_map(|str_len| {
                    let piece_str_bytes = const_slice_as_u8s(str_ptr_id, str_len)?;
                    String::from_utf8(piece_str_bytes).ok()
                })
        };
        let parse_encoded_template = |template_ptr_id: Word, template_len: usize| {
            let bytes = const_slice_as_u8s(template_ptr_id, template_len)?;

            let mut i = 0;
            let mut pieces = SmallVec::<[String; 2]>::new();
            let mut current_piece = String::new();
            let mut placeholder_count = 0usize;
            let mut next_implicit_arg_index = 0u16;
            let mut placeholders_map_1_to_1 = true;

            while let Some(&first) = bytes.get(i) {
                i += 1;
                match first {
                    0 => break,
                    1..=127 => {
                        let lit_len = first as usize;
                        let lit = bytes.get(i..i + lit_len)?;
                        i += lit_len;
                        current_piece.push_str(std::str::from_utf8(lit).ok()?);
                    }
                    128 => {
                        let len_bytes = bytes.get(i..i + 2)?;
                        i += 2;
                        let lit_len = u16::from_le_bytes([len_bytes[0], len_bytes[1]]) as usize;
                        let lit = bytes.get(i..i + lit_len)?;
                        i += lit_len;
                        current_piece.push_str(std::str::from_utf8(lit).ok()?);
                    }
                    0xC0..=0xFF => {
                        let flags_len = if first & 1 != 0 { 4 } else { 0 };
                        let width_len = if first & 2 != 0 { 2 } else { 0 };
                        let precision_len = if first & 4 != 0 { 2 } else { 0 };
                        let arg_index_len = if first & 8 != 0 { 2 } else { 0 };
                        let placeholder_len = flags_len + width_len + precision_len + arg_index_len;
                        let placeholder = bytes.get(i..i + placeholder_len)?;
                        i += placeholder_len;

                        let mut off = flags_len + width_len + precision_len;
                        let arg_index = if arg_index_len != 0 {
                            let lo = *placeholder.get(off)?;
                            let hi = *placeholder.get(off + 1)?;
                            off += 2;
                            let arg_index = u16::from_le_bytes([lo, hi]);
                            next_implicit_arg_index = arg_index.saturating_add(1);
                            arg_index
                        } else {
                            let arg_index = next_implicit_arg_index;
                            next_implicit_arg_index = next_implicit_arg_index.saturating_add(1);
                            arg_index
                        };
                        debug_assert_eq!(off, placeholder_len);

                        // Width/precision can indirectly reference count arguments.
                        if first & (1 << 4) != 0 || first & (1 << 5) != 0 {
                            placeholders_map_1_to_1 = false;
                        }
                        if arg_index as usize != placeholder_count {
                            placeholders_map_1_to_1 = false;
                        }

                        pieces.push(std::mem::take(&mut current_piece));
                        placeholder_count += 1;
                    }
                    _ => return None,
                }
            }
            if i != bytes.len() {
                return None;
            }

            pieces.push(current_piece);
            Some((pieces, placeholder_count, placeholders_map_1_to_1))
        };

        let panic_const_message = || {
            let def_id = panic_def_id?;
            let def_path = cx.tcx.def_path_str(def_id);
            let name = def_path.strip_prefix("core::panicking::panic_const::")?;
            match name {
                "panic_const_add_overflow" => Some("attempt to add with overflow"),
                "panic_const_sub_overflow" => Some("attempt to subtract with overflow"),
                "panic_const_mul_overflow" => Some("attempt to multiply with overflow"),
                "panic_const_div_overflow" => Some("attempt to divide with overflow"),
                "panic_const_rem_overflow" => {
                    Some("attempt to calculate the remainder with overflow")
                }
                "panic_const_neg_overflow" => Some("attempt to negate with overflow"),
                "panic_const_shr_overflow" => Some("attempt to shift right with overflow"),
                "panic_const_shl_overflow" => Some("attempt to shift left with overflow"),
                "panic_const_div_by_zero" => Some("attempt to divide by zero"),
                "panic_const_rem_by_zero" => {
                    Some("attempt to calculate the remainder with a divisor of zero")
                }
                "panic_const_coroutine_resumed" => Some("coroutine resumed after completion"),
                "panic_const_async_fn_resumed" => Some("`async fn` resumed after completion"),
                "panic_const_async_gen_fn_resumed" => {
                    Some("`async gen fn` resumed after completion")
                }
                "panic_const_gen_fn_none" => {
                    Some("`gen fn` should just keep returning `None` after completion")
                }
                "panic_const_coroutine_resumed_panic" => Some("coroutine resumed after panicking"),
                "panic_const_async_fn_resumed_panic" => Some("`async fn` resumed after panicking"),
                "panic_const_async_gen_fn_resumed_panic" => {
                    Some("`async gen fn` resumed after panicking")
                }
                "panic_const_gen_fn_none_panic" => {
                    Some("`gen fn` should just keep returning `None` after panicking")
                }
                "panic_const_coroutine_resumed_drop" => Some("coroutine resumed after async drop"),
                "panic_const_async_fn_resumed_drop" => Some("`async fn` resumed after async drop"),
                "panic_const_async_gen_fn_resumed_drop" => {
                    Some("`async gen fn` resumed after async drop")
                }
                "panic_const_gen_fn_none_drop" => Some("`gen fn` resumed after async drop"),
                _ => None,
            }
        };

        // HACK(eddyb) `panic_explicit` doesn't take any regular arguments,
        // only an (implicit) `&'static panic::Location<'static>`.
        if args.len() == 1 {
            decoded_format_args.const_pieces = Some(
                [panic_const_message().unwrap_or("explicit panic").into()]
                    .into_iter()
                    .collect(),
            );
            return Ok(decoded_format_args);
        }

        // HACK(eddyb) some entry-points only take a `&str`, not `fmt::Arguments`.
        if let [
            SpirvValue {
                kind: SpirvValueKind::Def(a_id),
                ty: a_ty,
            },
            SpirvValue {
                kind: SpirvValueKind::Def(b_id),
                ty: b_ty,
            },
            ref other_args @ ..,
        ] = args[..]
        {
            // Optional `force_no_backtrace` and/or `&'static panic::Location<'static>`.
            if other_args.len() <= 2
                && let Some(const_msg) = const_str_as_utf8(&[a_id, b_id])
            {
                decoded_format_args.const_pieces = Some([const_msg].into_iter().collect());
                return Ok(decoded_format_args);
            }

            // Dynamic `&str` panic messages (e.g. `panic_display(&msg)` where
            // `msg` isn't a directly recoverable constant).
            if other_args.len() <= 2
                && matches!(cx.lookup_type(a_ty), SpirvType::Pointer { .. })
                && matches!(cx.lookup_type(b_ty), SpirvType::Integer(..))
            {
                decoded_format_args.const_pieces =
                    Some(["<dynamic panic message>".into()].into_iter().collect());
                return Ok(decoded_format_args);
            }
        }

        // Newer `core::fmt::Arguments` can be scalarized at the panic call-site
        // (`template: *const u8`, `args: *const fmt::rt::Argument`), instead of
        // being passed as one aggregate value.
        let split_fmt_args = match *args {
            [
                SpirvValue {
                    kind: SpirvValueKind::Def(template_id),
                    ty: template_ty_id,
                },
                SpirvValue {
                    kind: SpirvValueKind::Def(rt_args_or_tagged_len_id),
                    ty: rt_args_or_tagged_len_ty_id,
                },
                ref trailing @ ..,
            ] if trailing.len() <= 2
                && matches!(cx.lookup_type(template_ty_id), SpirvType::Pointer { .. })
                && matches!(
                    cx.lookup_type(rt_args_or_tagged_len_ty_id),
                    SpirvType::Pointer { .. } | SpirvType::Integer(..)
                )
                && const_str_as_utf8(&[template_id, rt_args_or_tagged_len_id]).is_none() =>
            {
                let looks_like_bool = matches!(
                    cx.builder.lookup_const_by_id(rt_args_or_tagged_len_id),
                    Some(SpirvConst::Scalar(0 | 1))
                );
                if trailing.len() == 1 && looks_like_bool {
                    None
                } else {
                    Some((
                        template_id,
                        template_ty_id,
                        rt_args_or_tagged_len_id,
                        rt_args_or_tagged_len_ty_id,
                    ))
                }
            }
            _ => None,
        };

        let format_args_id = if split_fmt_args.is_none() {
            match *args {
                // HACK(eddyb) `panic_nounwind_fmt` takes an extra argument.
                [
                    SpirvValue {
                        kind: SpirvValueKind::Def(format_args_id),
                        ..
                    },
                    _, // `&'static panic::Location<'static>`
                ]
                | [
                    SpirvValue {
                        kind: SpirvValueKind::Def(format_args_id),
                        ..
                    },
                    _, // `force_no_backtrace: bool`
                    _, // `&'static panic::Location<'static>`
                ] => format_args_id,

                _ => {
                    return Err(FormatArgsNotRecognized(
                        "panic entry-point call args".into(),
                    ));
                }
            }
        } else {
            0
        };

        let custom_ext_inst_set_import = builder.ext_inst.borrow_mut().import_custom(builder);

        // HACK(eddyb) we can remove SSA instructions even when they have
        // side-effects, *as long as* they are "local" enough and cannot
        // be observed from outside this current invocation - because the
        // the abort, any SSA definitions or local variable writes can't
        // be actually used anywhere else (other than *before* the abort).
        let mut builder = builder.emit();
        let func_idx = builder.selected_function().unwrap();
        let block_idx = builder.selected_block().unwrap();
        let func = &mut builder.module_mut().functions[func_idx];

        // HACK(eddyb) this is used to check that all `Op{Store,Load}`s
        // that may get removed, operate on local `OpVariable`s,
        // i.e. are not externally observable.
        let local_var_ids: FxHashSet<_> = func.blocks[0]
            .instructions
            .iter()
            .take_while(|inst| inst.class.opcode == Op::Variable)
            .map(|inst| inst.result_id.unwrap())
            .collect();
        let require_local_var = |ptr_id, var| {
            Some(())
                .filter(|()| local_var_ids.contains(&ptr_id))
                .ok_or_else(|| FormatArgsNotRecognized(format!("{var} storage not local")))
        };

        let mut non_debug_insts = func.blocks[block_idx]
            .instructions
            .iter()
            .enumerate()
            .filter(|(_, inst)| {
                let is_standard_debug = [Op::Line, Op::NoLine].contains(&inst.class.opcode);
                let is_custom_debug = inst.class.opcode == Op::ExtInst
                    && inst.operands[0].unwrap_id_ref() == custom_ext_inst_set_import
                    && CustomOp::decode_from_ext_inst(inst).is_debuginfo();
                !(is_standard_debug || is_custom_debug)
            });

        // HACK(eddyb) to aid in pattern-matching, relevant instructions
        // are decoded to values of this `enum`. For instructions that
        // produce results, the result ID is the first `ID` value.
        #[derive(Debug)]
        enum Inst<ID> {
            Bitcast(ID, ID),
            CompositeExtract(ID, ID, u32),
            CompositeInsert(ID, ID, ID, u32),
            InBoundsAccessChain(ID, ID, u32),
            InBoundsAccessChain2(ID, ID, u32, u32),
            Store(ID, ID),
            Load(ID, ID),
            CopyMemory(ID, ID),
            Call(ID, ID, SmallVec<[ID; 4]>),

            // HACK(eddyb) this only exists for better error reporting,
            // as `Result<Inst<...>, Op>` would only report one `Op`.
            Unsupported(
                // HACK(eddyb) only exists for `fmt::Debug` in case of error.
                #[allow(dead_code)] Op,
            ),
        }

        let taken_inst_idx_range = Cell::new(func.blocks[block_idx].instructions.len())..;

        // Take `count` instructions, advancing backwards, but returning
        // instructions in their original order (and decoded to `Inst`s).
        let mut try_rev_take = |count: isize| {
            // HACK(eddyb) this is extremely silly but it's easier to do
            // this than to rely on `Iterator::peekable` or anything else,
            // lower down this file, without messing up the state here.
            let is_peek = count < 0;
            let count = count.unsigned_abs();

            let mut non_debug_insts_for_peek = is_peek.then(|| non_debug_insts.clone());
            let non_debug_insts = non_debug_insts_for_peek
                .as_mut()
                .unwrap_or(&mut non_debug_insts);

            // FIXME(eddyb) there might be an easier way to do this,
            // e.g. maybe `map_while` + post-`collect` length check?
            let maybe_rev_insts = (0..count).map(|_| {
                let (i, inst) = non_debug_insts.next_back()?;
                if !is_peek {
                    taken_inst_idx_range.start.set(i);
                }

                // HACK(eddyb) avoid the logic below that assumes only ID operands
                if inst.class.opcode == Op::CompositeExtract
                    && let (Some(r), &[Operand::IdRef(x), Operand::LiteralBit32(i)]) =
                        (inst.result_id, &inst.operands[..])
                {
                    return Some(Inst::CompositeExtract(r, x, i));
                }
                if inst.class.opcode == Op::CompositeInsert
                    && let (
                        Some(r),
                        &[
                            Operand::IdRef(inserted),
                            Operand::IdRef(base),
                            Operand::LiteralBit32(i),
                        ],
                    ) = (inst.result_id, &inst.operands[..])
                {
                    return Some(Inst::CompositeInsert(r, inserted, base, i));
                }

                // HACK(eddyb) all instructions accepted below
                // are expected to take no more than 4 operands,
                // and this is easier to use than an iterator.
                let id_operands = inst
                    .operands
                    .iter()
                    .map(|operand| operand.id_ref_any())
                    .collect::<Option<SmallVec<[_; 4]>>>()?;

                let const_as_u32 = |id| match cx.builder.lookup_const_by_id(id)? {
                    SpirvConst::Scalar(x) => u32::try_from(x).ok(),
                    _ => None,
                };

                // Decode the instruction into one of our `Inst`s.
                Some(
                    match (inst.class.opcode, inst.result_id, &id_operands[..]) {
                        (Op::Bitcast, Some(r), &[x]) => Inst::Bitcast(r, x),
                        (Op::InBoundsAccessChain, Some(r), &[p, i]) => {
                            if let Some(i) = const_as_u32(i) {
                                Inst::InBoundsAccessChain(r, p, i)
                            } else {
                                Inst::Unsupported(inst.class.opcode)
                            }
                        }
                        (Op::InBoundsAccessChain, Some(r), &[p, i, j]) => {
                            if let [Some(i), Some(j)] = [i, j].map(const_as_u32) {
                                Inst::InBoundsAccessChain2(r, p, i, j)
                            } else {
                                Inst::Unsupported(inst.class.opcode)
                            }
                        }
                        (Op::Store, None, &[p, v]) => Inst::Store(p, v),
                        (Op::Load, Some(r), &[p]) => Inst::Load(r, p),
                        (Op::CopyMemory, None, &[a, b]) => Inst::CopyMemory(a, b),
                        (Op::FunctionCall, Some(r), [f, args @ ..]) => {
                            Inst::Call(r, *f, args.iter().copied().collect())
                        }
                        _ => Inst::Unsupported(inst.class.opcode),
                    },
                )
            });
            let mut insts = maybe_rev_insts.collect::<Option<SmallVec<[_; 4]>>>()?;
            insts.reverse();
            Some(insts)
        };
        let lookup_fmt_args_ctor = |callee_id| {
            cx.fmt_args_new_fn_ids
                .borrow()
                .get(&callee_id)
                .copied()
                .ok_or_else(|| {
                    FormatArgsNotRecognized("fmt::Arguments::new callee not registered".into())
                })
        };
        let (ctor, call_args_storage) =
            if let Some((template_id, template_ty_id, rt_args_ptr_id, rt_args_ptr_ty_id)) =
                split_fmt_args
            {
                let ctor = if let (Some(template_len), Some(rt_args_count)) = (
                    const_ptr_to_composite_len(template_id)
                        .or_else(|| array_len_from_ptr_type(template_ty_id)),
                    const_ptr_to_composite_len(rt_args_ptr_id)
                        .or_else(|| array_len_from_ptr_type(rt_args_ptr_ty_id)),
                ) {
                    FmtArgsCtor::NewTemplate {
                        template_len,
                        rt_args_count,
                    }
                } else if let Some(&[Inst::Call(_, callee_id, ref call_args)]) =
                    try_rev_take(-1).as_deref()
                    && call_args.len() == 2
                    && [call_args[0], call_args[1]] == [template_id, rt_args_ptr_id]
                {
                    // Consume the matched call instruction.
                    try_rev_take(1).unwrap();
                    lookup_fmt_args_ctor(callee_id)?
                } else {
                    // We failed to recover constructor metadata for an already-split
                    // `fmt::Arguments` value. Keep panic lowering sound by falling
                    // back to an unknown panic message, without requiring decompilation.
                    return Ok(decoded_format_args);
                };

                (
                    ctor,
                    SmallVec::<[Word; 8]>::from_slice(&[template_id, rt_args_ptr_id]),
                )
            } else {
                // Newer rustc can pass the `fmt::Arguments::new_*` result directly to
                // panic entry points (single trailing call), while older versions go
                // through a local temporary (`Call -> Store -> Load`).
                let fmt_args_new_call_inst_count = if matches!(
                    try_rev_take(-3).as_deref(),
                    Some(&[Inst::Call(_, _, _), Inst::Store(_, _), Inst::Load(_, _)])
                ) {
                    3
                } else if let Some(&[Inst::Call(call_ret_id, callee_id, _)]) =
                    try_rev_take(-1).as_deref()
                {
                    if call_ret_id == format_args_id
                        || cx.fmt_args_new_fn_ids.borrow().contains_key(&callee_id)
                    {
                        1
                    } else {
                        0
                    }
                } else if matches!(
                    try_rev_take(-5).as_deref(),
                    Some(&[
                        Inst::Call(call_ret_id, _, _),
                        Inst::CompositeExtract(extracted0, from0, 0),
                        Inst::CompositeExtract(extracted1, from1, 1),
                        Inst::CompositeInsert(inserted0, value0, _, 0),
                        Inst::CompositeInsert(inserted1, value1, inserted0_prev, 1),
                    ]) if [from0, from1] == [call_ret_id; 2]
                        && [value0, value1] == [extracted0, extracted1]
                        && inserted0 == inserted0_prev
                        && inserted1 == format_args_id
                ) {
                    5
                } else if try_rev_take(-1).is_some() {
                    0
                } else {
                    // HACK(eddyb) gather context for new call patterns before bailing.
                    let mut insts = SmallVec::<[Inst<Word>; 32]>::new();
                    while let Some(extra_inst) = try_rev_take(1) {
                        insts.extend(extra_inst);
                        if insts.len() >= 32 {
                            break;
                        }
                    }
                    insts.reverse();

                    if insts.is_empty() {
                        return Ok(decoded_format_args);
                    }
                    if !insts.iter().any(|inst| matches!(inst, Inst::Call(_, _, _))) {
                        return Ok(decoded_format_args);
                    }
                    return Err(FormatArgsNotRecognized(format!(
                        "fmt::Arguments::new call sequence ({insts:?})",
                    )));
                };
                if fmt_args_new_call_inst_count == 0 {
                    // HACK(eddyb) gather context for new call patterns before bailing.
                    let mut insts = SmallVec::<[Inst<Word>; 32]>::new();
                    while let Some(extra_inst) = try_rev_take(1) {
                        insts.extend(extra_inst);
                        if insts.len() >= 32 {
                            break;
                        }
                    }
                    insts.reverse();

                    if insts.is_empty() {
                        return Ok(decoded_format_args);
                    }
                    if !insts.iter().any(|inst| matches!(inst, Inst::Call(_, _, _))) {
                        return Ok(decoded_format_args);
                    }
                    return Err(FormatArgsNotRecognized(format!(
                        "fmt::Arguments::new call sequence ({insts:?})",
                    )));
                }
                let fmt_args_new_call_insts = try_rev_take(fmt_args_new_call_inst_count).unwrap();
                match fmt_args_new_call_insts[..] {
                    [
                        Inst::Call(call_ret_id, callee_id, ref call_args),
                        Inst::Store(st_dst_id, st_val_id),
                        Inst::Load(ld_val_id, ld_src_id),
                    ] if call_ret_id == st_val_id
                        && st_dst_id == ld_src_id
                        && ld_val_id == format_args_id =>
                    {
                        require_local_var(st_dst_id, "fmt::Arguments::new destination")?;

                        (
                            lookup_fmt_args_ctor(callee_id)?,
                            call_args.iter().copied().collect(),
                        )
                    }
                    [Inst::Call(call_ret_id, callee_id, ref call_args)]
                        if call_ret_id == format_args_id
                            || cx.fmt_args_new_fn_ids.borrow().contains_key(&callee_id) =>
                    {
                        (
                            lookup_fmt_args_ctor(callee_id)?,
                            call_args.iter().copied().collect(),
                        )
                    }
                    [
                        Inst::Call(call_ret_id, callee_id, ref call_args),
                        Inst::CompositeExtract(extracted0, from0, 0),
                        Inst::CompositeExtract(extracted1, from1, 1),
                        Inst::CompositeInsert(inserted0, value0, _, 0),
                        Inst::CompositeInsert(inserted1, value1, inserted0_prev, 1),
                    ] if [from0, from1] == [call_ret_id; 2]
                        && [value0, value1] == [extracted0, extracted1]
                        && inserted0 == inserted0_prev
                        && inserted1 == format_args_id =>
                    {
                        (
                            lookup_fmt_args_ctor(callee_id)?,
                            call_args.iter().copied().collect(),
                        )
                    }
                    _ => {
                        // HACK(eddyb) this gathers more context before reporting.
                        let mut insts = fmt_args_new_call_insts;
                        insts.reverse();
                        while let Some(extra_inst) = try_rev_take(1) {
                            insts.extend(extra_inst);
                            if insts.len() >= 32 {
                                break;
                            }
                        }
                        insts.reverse();

                        return Err(FormatArgsNotRecognized(format!(
                            "fmt::Arguments::new call sequence ({insts:?})",
                        )));
                    }
                }
            };
        let call_args = call_args_storage.as_slice();
        enum PiecesSource {
            Slice { ptr_id: Word, len: usize },
            EncodedTemplate { ptr_id: Word, len: usize },
            DirectConstStr([Word; 2]),
        }
        let mut template_placeholder_info = None;
        let (pieces_source, (rt_args_slice_ptr_id, rt_args_count)) = match (ctor, call_args) {
            // `<core::fmt::Arguments>::new`
            (
                FmtArgsCtor::NewTemplate {
                    template_len,
                    rt_args_count,
                },
                &[template_ptr_id, rt_args_slice_ptr_id],
            ) => (
                PiecesSource::EncodedTemplate {
                    ptr_id: template_ptr_id,
                    len: template_len,
                },
                (Some(rt_args_slice_ptr_id), rt_args_count),
            ),

            // `<core::fmt::Arguments>::new_v1_formatted`
            //
            // HACK(eddyb) this isn't fully supported,
            // as that would require digging into unstable
            // internals of `core::fmt::rt::Placeholder`s,
            // but the whole call still needs to be removed,
            // and both const str pieces and runtime args
            // can still be printed (even if in jankier way).
            (
                FmtArgsCtor::NewV1FormattedDynamic,
                &[
                    pieces_slice_ptr_id,
                    pieces_len_id,
                    rt_args_slice_ptr_id,
                    rt_args_len_id,
                    fmt_placeholders_slice_ptr_id,
                    fmt_placeholders_len_id,
                ],
            ) => {
                let [pieces_len, rt_args_len, fmt_placeholders_len] =
                    match [pieces_len_id, rt_args_len_id, fmt_placeholders_len_id]
                        .map(const_u32_as_usize)
                    {
                        [Some(a), Some(b), Some(c)] => [a, b, c],
                        _ => {
                            return Err(FormatArgsNotRecognized(
                                "fmt::Arguments::new_v1_formatted \
                                             with dynamic lengths"
                                    .into(),
                            ));
                        }
                    };

                let prepare_args_insts = try_rev_take(2).ok_or_else(|| {
                    FormatArgsNotRecognized(
                        "fmt::Arguments::new_v1_formatted call: ran out of instructions".into(),
                    )
                })?;
                let (rt_args_slice_ptr_id, _fmt_placeholders_slice_ptr_id) =
                    match prepare_args_insts[..] {
                        [
                            Inst::Bitcast(rt_args_cast_out_id, rt_args_cast_in_id),
                            Inst::Bitcast(placeholders_cast_out_id, placeholders_cast_in_id),
                        ] if rt_args_cast_out_id == rt_args_slice_ptr_id
                            && placeholders_cast_out_id == fmt_placeholders_slice_ptr_id =>
                        {
                            (rt_args_cast_in_id, placeholders_cast_in_id)
                        }
                        _ => {
                            return Err(FormatArgsNotRecognized(format!(
                                "fmt::Arguments::new_v1_formatted call sequence ({prepare_args_insts:?})",
                            )));
                        }
                    };

                decoded_format_args.has_unknown_fmt_placeholder_to_args_mapping =
                    Some(fmt_placeholders_len);

                (
                    PiecesSource::Slice {
                        ptr_id: pieces_slice_ptr_id,
                        len: pieces_len,
                    },
                    (Some(rt_args_slice_ptr_id), rt_args_len),
                )
            }

            // `<core::fmt::Arguments>::from_str`
            (FmtArgsCtor::FromStr, &[str_ptr_id, str_len_id]) => (
                PiecesSource::DirectConstStr([str_ptr_id, str_len_id]),
                (None, 0),
            ),

            // `<core::fmt::Arguments>::new_v1`
            (
                FmtArgsCtor::NewV1 {
                    pieces_len,
                    rt_args_count,
                },
                &[pieces_slice_ptr_id, rt_args_slice_ptr_id],
            ) => (
                PiecesSource::Slice {
                    ptr_id: pieces_slice_ptr_id,
                    len: pieces_len,
                },
                (Some(rt_args_slice_ptr_id), rt_args_count),
            ),

            // `<core::fmt::Arguments>::new_const`
            (FmtArgsCtor::NewConst { pieces_len }, &[pieces_slice_ptr_id]) => (
                PiecesSource::Slice {
                    ptr_id: pieces_slice_ptr_id,
                    len: pieces_len,
                },
                (None, 0),
            ),

            _ => {
                return Err(FormatArgsNotRecognized(
                    "fmt::Arguments::new ctor/call-args mismatch".into(),
                ));
            }
        };

        // HACK(eddyb) this is the worst part: if we do have runtime
        // arguments (from e.g. new `assert!`s being added to `core`),
        // we have to confirm their many instructions for removal.
        if rt_args_count > 0 {
            let rt_args_array_ptr_id = rt_args_slice_ptr_id.unwrap();

            // Each runtime argument has A instructions to call one of
            // the `fmt::rt::Argument::new_*` functions (and temporarily
            // store its result), and B instructions to copy it into
            // the appropriate slot in the array. The groups of A and B
            // instructions, for all runtime args, are each separate,
            // so the B×N later instructions are all processed first,
            // before moving (backwards) to the A×N earlier instructions.

            let rev_copies_to_rt_args_array_src_ptrs: SmallVec<[_; 4]> = (0..rt_args_count)
                .rev()
                .map(|rt_arg_idx| {
                    let mut copy_to_rt_args_array_insts = try_rev_take(3).ok_or_else(|| {
                        FormatArgsNotRecognized(
                            "[fmt::rt::Argument; N] copy: ran out of instructions".into(),
                        )
                    })?;

                    // HACK(eddyb) account for both the split and combined
                    // access chain cases that `inbounds_gep` can now cause.
                    if let Inst::InBoundsAccessChain(dst_field_ptr, dst_base_ptr, 0) =
                        copy_to_rt_args_array_insts[0]
                        && let Some(mut prev_insts) = try_rev_take(1)
                    {
                        assert_eq!(prev_insts.len(), 1);
                        let prev_inst = prev_insts.pop().unwrap();

                        match prev_inst {
                            Inst::InBoundsAccessChain(array_elem_ptr, array_ptr, idx)
                                if dst_base_ptr == array_elem_ptr =>
                            {
                                copy_to_rt_args_array_insts[0] =
                                    Inst::InBoundsAccessChain2(dst_field_ptr, array_ptr, idx, 0);
                            }
                            _ => {
                                // HACK(eddyb) don't lose the taken `prev_inst`.
                                copy_to_rt_args_array_insts.insert(0, prev_inst);
                            }
                        }
                    }

                    match copy_to_rt_args_array_insts[..] {
                        [
                            Inst::InBoundsAccessChain2(
                                dst_field_ptr,
                                dst_array_base_ptr,
                                array_idx,
                                0,
                            ),
                            Inst::InBoundsAccessChain(src_field_ptr, src_base_ptr, 0),
                            Inst::CopyMemory(copy_dst, copy_src),
                        ] if dst_array_base_ptr == rt_args_array_ptr_id
                            && array_idx as usize == rt_arg_idx
                            && (copy_dst, copy_src) == (dst_field_ptr, src_field_ptr) =>
                        {
                            Ok(src_base_ptr)
                        }
                        _ => Err(FormatArgsNotRecognized(format!(
                            "[fmt::rt::Argument; N] copy sequence ({copy_to_rt_args_array_insts:?})"
                        ))),
                    }
                })
                .collect::<Result<_, _>>()?;

            // HACK(eddyb) sometimes there is an extra tuple of refs,
            // nowadays, but MIR opts mean it's not always guaranteed,
            // hopefully it's always uniform across all the arguments.
            let mut maybe_ref_args_tmp_slot_ptr = None;

            let rev_maybe_ref_arg_ids_with_ty_and_spec = ((0..rt_args_count)
                .rev()
                .zip_eq(rev_copies_to_rt_args_array_src_ptrs))
            .map(|(rt_arg_idx, copy_to_rt_args_array_src_ptr)| {
                let rt_arg_new_call_insts = try_rev_take(4).ok_or_else(|| {
                    FormatArgsNotRecognized(
                        "fmt::rt::Argument::new call: ran out of instructions".into(),
                    )
                })?;
                let (ref_arg_id, ty, spec) = match rt_arg_new_call_insts[..] {
                    [
                        Inst::Call(call_ret_id, callee_id, ref call_args),
                        Inst::InBoundsAccessChain(tmp_slot_field_ptr, tmp_slot_ptr, 0),
                        Inst::CompositeExtract(field, wrapper_newtype, 0),
                        Inst::Store(st_dst_ptr, st_val),
                    ] if wrapper_newtype == call_ret_id
                        && tmp_slot_ptr == copy_to_rt_args_array_src_ptr
                        && (st_dst_ptr, st_val) == (tmp_slot_field_ptr, field) =>
                    {
                        cx.fmt_rt_arg_new_fn_ids_to_ty_and_spec
                            .borrow()
                            .get(&callee_id)
                            .and_then(|&(ty, spec)| match call_args[..] {
                                [x] => Some((x, ty, spec)),
                                _ => None,
                            })
                    }
                    _ => None,
                }
                .ok_or_else(|| {
                    FormatArgsNotRecognized(format!(
                        "fmt::rt::Argument::new call sequence ({rt_arg_new_call_insts:?})"
                    ))
                })?;

                // HACK(eddyb) `0` (an invalid ID) is later used as a
                // placeholder (see also `maybe_ref_args_tmp_slot_ptr`).
                assert_ne!(ref_arg_id, 0);

                // HACK(eddyb) `try_rev_take(-2)` is "peeking", not taking.
                let maybe_ref_args_tuple_load_insts = try_rev_take(-2);
                let maybe_ref_arg_id = match maybe_ref_args_tuple_load_insts.as_deref() {
                    Some(
                        &[
                            Inst::InBoundsAccessChain(field_ptr, base_ptr, field_idx),
                            Inst::Load(ld_val, ld_src_ptr),
                        ],
                    ) if maybe_ref_args_tmp_slot_ptr
                        .is_none_or(|expected| base_ptr == expected)
                        && field_idx as usize == rt_arg_idx
                        && (ld_val, ld_src_ptr) == (ref_arg_id, field_ptr) =>
                    {
                        // HACK(eddyb) consume the peeked instructions.
                        try_rev_take(2).unwrap();

                        maybe_ref_args_tmp_slot_ptr = Some(base_ptr);

                        // HACK(eddyb) using `0` (an invalid ID) as a
                        // placeholder to require further processing.
                        0
                    }
                    _ => ref_arg_id,
                };

                Ok((maybe_ref_arg_id, ty, spec))
            })
            .collect::<Result<_, _>>()?;

            decoded_format_args.ref_arg_ids_with_ty_and_spec =
                rev_maybe_ref_arg_ids_with_ty_and_spec;
            decoded_format_args.ref_arg_ids_with_ty_and_spec.reverse();

            // HACK(eddyb) see above for context regarding the use of
            // `0` as placeholders and `maybe_ref_args_tmp_slot_ptr`.
            if let Some(ref_args_tmp_slot_ptr) = maybe_ref_args_tmp_slot_ptr {
                for (rt_arg_idx, (maybe_ref_arg_id, ..)) in decoded_format_args
                    .ref_arg_ids_with_ty_and_spec
                    .iter_mut()
                    .enumerate()
                    .rev()
                {
                    if *maybe_ref_arg_id == 0 {
                        let ref_arg_store_insts = try_rev_take(2).ok_or_else(|| {
                            FormatArgsNotRecognized(
                                "fmt::rt::Argument::new argument store: ran out of instructions"
                                    .into(),
                            )
                        })?;

                        *maybe_ref_arg_id = match ref_arg_store_insts[..] {
                            [
                            Inst::InBoundsAccessChain(field_ptr, base_ptr, field_idx),
                            Inst::Store(st_dst_ptr, st_val),
                            ] if base_ptr == ref_args_tmp_slot_ptr
                                && field_idx as usize == rt_arg_idx
                                && st_dst_ptr == field_ptr =>
                                {
                                    Some(st_val)
                                }
                            _ => None,
                        }
                            .ok_or_else(|| {
                                FormatArgsNotRecognized(format!(
                                    "fmt::rt::Argument::new argument store sequence ({ref_arg_store_insts:?})"
                                ))
                            })?;
                    }
                }
            }
        }

        decoded_format_args.const_pieces = match pieces_source {
            PiecesSource::DirectConstStr(str_ref) => {
                const_str_as_utf8(&str_ref).map(|s| [s].into_iter().collect())
            }
            PiecesSource::EncodedTemplate {
                ptr_id: template_ptr_id,
                len: template_len,
            } => parse_encoded_template(template_ptr_id, template_len).map(
                |(pieces, placeholder_count, placeholders_map_1_to_1)| {
                    template_placeholder_info = Some((placeholder_count, placeholders_map_1_to_1));
                    pieces
                },
            ),
            PiecesSource::Slice {
                ptr_id: pieces_slice_ptr_id,
                len: pieces_len,
            } => {
                // If the `pieces: &[&str]` slice needs a bitcast, it'll be here.
                // HACK(eddyb) `try_rev_take(-1)` is "peeking", not taking.
                let pieces_slice_ptr_id = match try_rev_take(-1).as_deref() {
                    Some(&[Inst::Bitcast(out_id, in_id)]) if out_id == pieces_slice_ptr_id => {
                        // HACK(eddyb) consume the peeked instructions.
                        try_rev_take(1).unwrap();

                        in_id
                    }
                    _ => pieces_slice_ptr_id,
                };

                match const_slice_as_elem_ids(pieces_slice_ptr_id, pieces_len) {
                    Some(piece_ids) => piece_ids
                        .iter()
                        .map(|&piece_id| match cx.builder.lookup_const_by_id(piece_id)? {
                            SpirvConst::Composite(piece) => {
                                const_str_as_utf8(piece.try_into().ok()?)
                            }
                            _ => None,
                        })
                        .collect::<Option<_>>(),
                    // HACK(eddyb) minor upstream blunder results in at
                    // least one instance of a runtime `[&str; 1]` array,
                    // see also this comment left on the responsible PR:
                    // https://github.com/rust-lang/rust/pull/129658#discussion_r2181834781
                    // HACK(eddyb) `try_rev_take(-4)` is "peeking", not taking.
                    None if pieces_len == 1 => match try_rev_take(-4).as_deref() {
                        Some(
                            &[
                                Inst::InBoundsAccessChain2(field0_ptr, array_ptr_0, 0, 0),
                                Inst::Store(st0_dst_ptr, st0_val),
                                Inst::InBoundsAccessChain2(field1_ptr, array_ptr_1, 0, 1),
                                Inst::Store(st1_dst_ptr, st1_val),
                            ],
                        ) if [array_ptr_0, array_ptr_1] == [pieces_slice_ptr_id; 2]
                            && st0_dst_ptr == field0_ptr
                            && st1_dst_ptr == field1_ptr =>
                        {
                            // HACK(eddyb) consume the peeked instructions.
                            try_rev_take(4).unwrap();

                            const_str_as_utf8(&[st0_val, st1_val])
                                .map(|s| [s].into_iter().collect())
                        }
                        _ => None,
                    },
                    None => None,
                }
            }
        };

        if let Some((placeholder_count, placeholders_map_1_to_1)) = template_placeholder_info {
            let decoded_arg_count = decoded_format_args.ref_arg_ids_with_ty_and_spec.len();
            if !placeholders_map_1_to_1 || placeholder_count != decoded_arg_count {
                decoded_format_args.has_unknown_fmt_placeholder_to_args_mapping =
                    Some(placeholder_count);
            }
        }

        // Keep all instructions up to (but not including) the last one
        // confirmed above to be the first instruction of `format_args!`.
        func.blocks[block_idx]
            .instructions
            .truncate(taken_inst_idx_range.start.get());

        Ok(decoded_format_args)
    }
}

pub trait CodegenPanic<'a, 'tcx> {
    fn codegen_panic(&self, builder: &mut Builder<'a, 'tcx>, result_type: Word) -> SpirvValue;
}

impl<'a, 'tcx> CodegenPanic<'a, 'tcx> for DecodedFormatArgs<'tcx> {
    fn codegen_panic(&self, builder: &mut Builder<'a, 'tcx>, result_type: Word) -> SpirvValue {
        match self {
            DecodedFormatArgs {
                const_pieces: None, ..
            } => {
                builder.abort_with_kind_and_message_debug_printf(
                    "panic",
                    "<unknown message>",
                    std::iter::empty(),
                );
                builder.undef(result_type)
            }

            DecodedFormatArgs {
                const_pieces: Some(const_pieces),
                ref_arg_ids_with_ty_and_spec,
                has_unknown_fmt_placeholder_to_args_mapping,
            } => {
                let mut debug_printf_args = SmallVec::<[_; 2]>::new();
                let args = ref_arg_ids_with_ty_and_spec
                    .iter()
                    .map(|&(ref_id, ty, spec)| {
                        use rustc_abi::{Float::*, Integer::*, Primitive::*};

                        let layout = builder.layout_of(ty);

                        let scalar = match layout.backend_repr {
                            BackendRepr::Scalar(scalar) => Some(scalar.primitive()),
                            _ => None,
                        };
                        let debug_printf_fmt = match (spec, scalar) {
                            // FIXME(eddyb) support more of these,
                            // potentially recursing to print ADTs.
                            (' ' | '?', Some(Int(I32, false))) => "%u",
                            ('x', Some(Int(I32, false))) => "%x",
                            (' ' | '?', Some(Int(I32, true))) => "%i",
                            (' ' | '?', Some(Float(F32))) => "%f",

                            _ => "",
                        };

                        if debug_printf_fmt.is_empty() {
                            return Cow::Owned(
                                format!("{{/* unprintable {ty} */:{spec}}}").replace('%', "%%"),
                            );
                        }

                        let spirv_type = layout.spirv_type(builder.span(), builder);
                        debug_printf_args.push(
                            builder
                                .emit()
                                .load(spirv_type, None, ref_id, None, [])
                                .unwrap()
                                .with_type(spirv_type),
                        );
                        Cow::Borrowed(debug_printf_fmt)
                    });

                // HACK(eddyb) due to `fmt::Arguments::new_v1_formatted`,
                // we can't always assume that all the formatting arguments
                // are used 1:1 as placeholders (i.e. between `const_pieces`).
                let (placeholder_count, placeholders_are_args) =
                    match has_unknown_fmt_placeholder_to_args_mapping {
                        Some(count) => (*count, false),
                        None => (args.len(), true),
                    };

                // HACK(eddyb) extra sanity check to avoid visual mishaps.
                let valid_placeholder_count = placeholder_count
                    .clamp(const_pieces.len().saturating_sub(1), const_pieces.len());
                let placeholders_are_args =
                    placeholders_are_args && placeholder_count == valid_placeholder_count;

                // FIXME(eddyb) stop using `itertools`'s `intersperse`,
                // when it gets stabilized on `Iterator` instead.
                #[allow(unstable_name_collisions)]
                let (placeholders, suffix) = if placeholders_are_args {
                    (Either::Left(args), None)
                } else {
                    // See also `has_unknown_fmt_placeholder_to_args_mapping`
                    // comment (which has an example for 3 pieces and 2 args).
                    //
                    // FIXME(eddyb) this could definitely be improved, but
                    // so far this only really gets hit in esoteric `core`
                    // internals (UB checks and `char::encode_utf{8,16}`).
                    (
                        Either::Right(
                            (0..valid_placeholder_count).map(|i| format!("{{{i}}}").into()),
                        ),
                        Some(
                            ["\n  with {…} from: ".into()]
                                .into_iter()
                                .chain(args.intersperse(", ".into())),
                        ),
                    )
                };

                let message = const_pieces
                    .into_iter()
                    .map(|s| Cow::Owned(s.replace('%', "%%")))
                    .interleave(placeholders)
                    .chain(suffix.into_iter().flatten())
                    .collect::<String>();

                // HACK(eddyb) redirect any possible panic call to an abort, to avoid
                // needing to materialize `&core::panic::Location` or `format_args!`.
                builder.abort_with_kind_and_message_debug_printf(
                    "panic",
                    message,
                    debug_printf_args,
                );
                builder.undef(result_type)
            }
        }
    }
}

impl<'a, 'tcx> CodegenPanic<'a, 'tcx> for FormatArgsResult<'tcx> {
    fn codegen_panic(&self, builder: &mut Builder<'a, 'tcx>, result_type: Word) -> SpirvValue {
        match self {
            Ok(e) => e.codegen_panic(builder, result_type),
            Err(FormatArgsNotRecognized(step)) => {
                if let Some(current_span) = builder.current_span
                    && !builder.tcx.sess.opts.unstable_opts.ui_testing
                {
                    // HACK(eddyb) Cargo silences warnings in dependencies.
                    let force_warn = |span, msg| -> rustc_errors::Diag<'_, ()> {
                        rustc_errors::Diag::new(
                            builder.tcx.dcx(),
                            rustc_errors::Level::ForceWarning,
                            msg,
                        )
                        .with_span(span)
                    };
                    let mut warn = force_warn(
                        current_span,
                        "failed to find and remove `format_args!` construction for this `panic!`",
                    );

                    warn.note(
                        "compilation may later fail due to leftover `format_args!` internals",
                    );

                    if builder.tcx.sess.opts.unstable_opts.inline_mir != Some(false) {
                        warn.note("missing `-Zinline-mir=off` flag (should've been set by `spirv-builder`)")
                            .help("check `.cargo` and environment variables for potential overrides")
                            .help("(or, if not using `spirv-builder` at all, add the flag manually)");
                    } else {
                        warn.note(format!("[RUST-GPU BUG] bailed from {step}"));
                    }

                    warn.emit();
                }

                let msg = "<unknown message> (failed to find/decode `format_args!` expansion)";
                builder.abort_with_kind_and_message_debug_printf("panic", msg, std::iter::empty());
                builder.undef(result_type)
            }
        }
    }
}
