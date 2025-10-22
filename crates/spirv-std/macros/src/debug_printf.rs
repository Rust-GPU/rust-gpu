use proc_macro::TokenStream;
use proc_macro2::Span;
use std::fmt::Write;

pub struct DebugPrintfInput {
    pub span: Span,
    pub format_string: String,
    pub variables: Vec<syn::Expr>,
}

impl syn::parse::Parse for DebugPrintfInput {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::parse::Result<Self> {
        let span = input.span();

        if input.is_empty() {
            return Ok(Self {
                span,
                format_string: Default::default(),
                variables: Default::default(),
            });
        }

        let format_string = input.parse::<syn::LitStr>()?;
        if !input.is_empty() {
            input.parse::<syn::token::Comma>()?;
        }
        let variables =
            syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::parse_terminated(input)?;

        Ok(Self {
            span,
            format_string: format_string.value(),
            variables: variables.into_iter().collect(),
        })
    }
}

fn parse_error(message: &str, span: Span) -> TokenStream {
    syn::Error::new(span, message).to_compile_error().into()
}

enum FormatType {
    Scalar {
        ty: proc_macro2::TokenStream,
    },
    Vector {
        ty: proc_macro2::TokenStream,
        width: usize,
    },
}

pub fn debug_printf_inner(input: DebugPrintfInput) -> TokenStream {
    let DebugPrintfInput {
        format_string,
        variables,
        span,
    } = input;

    fn map_specifier_to_type(
        specifier: char,
        chars: &mut std::str::Chars<'_>,
    ) -> Option<proc_macro2::TokenStream> {
        let mut peekable = chars.peekable();

        Some(match specifier {
            'd' | 'i' => quote::quote! { i32 },
            'o' | 'x' | 'X' => quote::quote! { u32 },
            'a' | 'A' | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' => quote::quote! { f32 },
            'u' => {
                if matches!(peekable.peek(), Some('l')) {
                    chars.next();
                    quote::quote! { u64 }
                } else {
                    quote::quote! { u32 }
                }
            }
            'l' => {
                if matches!(peekable.peek(), Some('u' | 'x')) {
                    chars.next();
                    quote::quote! { u64 }
                } else {
                    return None;
                }
            }
            _ => return None,
        })
    }

    let mut chars = format_string.chars();
    let mut format_arguments = Vec::new();

    while let Some(mut ch) = chars.next() {
        if ch == '%' {
            ch = match chars.next() {
                Some('%') => continue,
                None => return parse_error("Unterminated format specifier", span),
                Some(ch) => ch,
            };

            let mut has_precision = false;

            while ch.is_ascii_digit() {
                ch = match chars.next() {
                    Some(ch) => ch,
                    None => {
                        return parse_error(
                            "Unterminated format specifier: missing type after precision",
                            span,
                        );
                    }
                };

                has_precision = true;
            }

            if has_precision && ch == '.' {
                ch = match chars.next() {
                    Some(ch) => ch,
                    None => {
                        return parse_error(
                            "Unterminated format specifier: missing type after decimal point",
                            span,
                        );
                    }
                };

                while ch.is_ascii_digit() {
                    ch = match chars.next() {
                        Some(ch) => ch,
                        None => {
                            return parse_error(
                                "Unterminated format specifier: missing type after fraction precision",
                                span,
                            );
                        }
                    };
                }
            }

            if ch == 'v' {
                let width = match chars.next() {
                    Some('2') => 2,
                    Some('3') => 3,
                    Some('4') => 4,
                    Some(ch) => {
                        return parse_error(&format!("Invalid width for vector: {ch}"), span);
                    }
                    None => return parse_error("Missing vector dimensions specifier", span),
                };

                ch = match chars.next() {
                    Some(ch) => ch,
                    None => return parse_error("Missing vector type specifier", span),
                };

                let ty = match map_specifier_to_type(ch, &mut chars) {
                    Some(ty) => ty,
                    _ => {
                        return parse_error(
                            &format!("Unrecognised vector type specifier: '{ch}'"),
                            span,
                        );
                    }
                };

                format_arguments.push(FormatType::Vector { ty, width });
            } else {
                let ty = match map_specifier_to_type(ch, &mut chars) {
                    Some(ty) => ty,
                    _ => {
                        return parse_error(
                            &format!("Unrecognised format specifier: '{ch}'"),
                            span,
                        );
                    }
                };

                format_arguments.push(FormatType::Scalar { ty });
            }
        }
    }

    if format_arguments.len() != variables.len() {
        return syn::Error::new(
            span,
            format!(
                "{} % arguments were found, but {} variables were given",
                format_arguments.len(),
                variables.len()
            ),
        )
        .to_compile_error()
        .into();
    }

    let mut variable_idents = String::new();
    let mut input_registers = Vec::new();
    let mut op_loads = Vec::new();

    for (i, (variable, format_argument)) in variables.into_iter().zip(format_arguments).enumerate()
    {
        let ident = quote::format_ident!("_{}", i);

        let _ = write!(variable_idents, "%{ident} ");

        let assert_fn = match format_argument {
            FormatType::Scalar { ty } => {
                quote::quote! { spirv_std::debug_printf::assert_is_type::<#ty> }
            }
            FormatType::Vector { ty, width } => {
                quote::quote! { spirv_std::debug_printf::assert_is_vector::<#ty, _, #width> }
            }
        };

        input_registers.push(quote::quote! {
            #ident = in(reg) &#assert_fn(#variable),
        });

        let op_load = format!("%{ident} = OpLoad _ {{{ident}}}");

        op_loads.push(quote::quote! {
            #op_load,
        });
    }

    let input_registers = input_registers
        .into_iter()
        .collect::<proc_macro2::TokenStream>();
    let op_loads = op_loads.into_iter().collect::<proc_macro2::TokenStream>();
    // Escapes the '{' and '}' characters in the format string.
    // Since the `asm!` macro expects '{' '}' to surround its arguments, we have to use '{{' and '}}' instead.
    // The `asm!` macro will then later turn them back into '{' and '}'.
    let format_string = format_string.replace('{', "{{").replace('}', "}}");

    let op_string = format!("%string = OpString {format_string:?}");

    let output = quote::quote! {
        ::core::arch::asm!(
            "%void = OpTypeVoid",
            #op_string,
            "%debug_printf = OpExtInstImport \"NonSemantic.DebugPrintf\"",
            #op_loads
            concat!("%result = OpExtInst %void %debug_printf 1 %string ", #variable_idents),
            #input_registers
        )
    };

    output.into()
}
