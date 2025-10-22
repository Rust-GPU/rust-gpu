use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::{ToTokens, quote};
use syn::ImplItemFn;
use syn::visit_mut::VisitMut;

const SAMPLE_PARAM_COUNT: usize = 4;
const SAMPLE_PARAM_GENERICS: [&str; SAMPLE_PARAM_COUNT] = ["B", "L", "G", "S"];
const SAMPLE_PARAM_TYPES: [&str; SAMPLE_PARAM_COUNT] = ["B", "L", "(G,G)", "S"];
const SAMPLE_PARAM_OPERANDS: [&str; SAMPLE_PARAM_COUNT] = ["Bias", "Lod", "Grad", "Sample"];
const SAMPLE_PARAM_NAMES: [&str; SAMPLE_PARAM_COUNT] = ["bias", "lod", "grad", "sample_index"];
const SAMPLE_PARAM_GRAD_INDEX: usize = 2; // Grad requires some special handling because it uses 2 arguments
const SAMPLE_PARAM_EXPLICIT_LOD_MASK: usize = 0b0110; // which params require the use of ExplicitLod rather than ImplicitLod

fn is_grad(i: usize) -> bool {
    i == SAMPLE_PARAM_GRAD_INDEX
}

struct SampleImplRewriter(usize, syn::Type);

impl SampleImplRewriter {
    pub fn rewrite(mask: usize, f: &syn::ItemImpl) -> syn::ItemImpl {
        let mut new_impl = f.clone();
        let mut ty_str = String::from("SampleParams<");

        // based on the mask, form a `SampleParams` type string and add the generic parameters to the `impl<>` generics
        // example type string: `"SampleParams<SomeTy<B>, NoneTy, NoneTy>"`
        for i in 0..SAMPLE_PARAM_COUNT {
            if mask & (1 << i) != 0 {
                new_impl.generics.params.push(syn::GenericParam::Type(
                    syn::Ident::new(SAMPLE_PARAM_GENERICS[i], Span::call_site()).into(),
                ));
                ty_str.push_str("SomeTy<");
                ty_str.push_str(SAMPLE_PARAM_TYPES[i]);
                ty_str.push('>');
            } else {
                ty_str.push_str("NoneTy");
            }
            ty_str.push(',');
        }
        ty_str.push('>');
        let ty: syn::Type = syn::parse(ty_str.parse().unwrap()).unwrap();

        // use the type to insert it into the generic argument of the trait we're implementing
        // e.g., `ImageWithMethods<Dummy>` becomes `ImageWithMethods<SampleParams<SomeTy<B>, NoneTy, NoneTy>>`
        if let Some(t) = &mut new_impl.trait_
            && let syn::PathArguments::AngleBracketed(a) =
                &mut t.1.segments.last_mut().unwrap().arguments
            && let Some(syn::GenericArgument::Type(t)) = a.args.last_mut()
        {
            *t = ty.clone();
        }

        // rewrite the implemented functions
        SampleImplRewriter(mask, ty).visit_item_impl_mut(&mut new_impl);
        new_impl
    }

    // generates an operands string for use in the assembly, e.g. "Bias %bias Lod %lod", based on the mask
    #[allow(clippy::needless_range_loop)]
    fn get_operands(&self) -> String {
        let mut op = String::new();
        for i in 0..SAMPLE_PARAM_COUNT {
            if self.0 & (1 << i) != 0 {
                if is_grad(i) {
                    op.push_str("Grad %grad_x %grad_y ");
                } else {
                    op.push_str(SAMPLE_PARAM_OPERANDS[i]);
                    op.push_str(" %");
                    op.push_str(SAMPLE_PARAM_NAMES[i]);
                    op.push(' ');
                }
            }
        }
        op
    }

    // generates list of assembly loads for the data, e.g. "%bias = OpLoad _ {bias}", etc.
    #[allow(clippy::needless_range_loop)]
    fn add_loads(&self, t: &mut Vec<TokenTree>) {
        for i in 0..SAMPLE_PARAM_COUNT {
            if self.0 & (1 << i) != 0 {
                if is_grad(i) {
                    t.push(TokenTree::Literal(proc_macro2::Literal::string(
                        "%grad_x = OpLoad _ {grad_x}",
                    )));
                    t.push(TokenTree::Punct(proc_macro2::Punct::new(
                        ',',
                        proc_macro2::Spacing::Alone,
                    )));
                    t.push(TokenTree::Literal(proc_macro2::Literal::string(
                        "%grad_y = OpLoad _ {grad_y}",
                    )));
                    t.push(TokenTree::Punct(proc_macro2::Punct::new(
                        ',',
                        proc_macro2::Spacing::Alone,
                    )));
                } else {
                    let s = format!("%{0} = OpLoad _ {{{0}}}", SAMPLE_PARAM_NAMES[i]);
                    t.push(TokenTree::Literal(proc_macro2::Literal::string(s.as_str())));
                    t.push(TokenTree::Punct(proc_macro2::Punct::new(
                        ',',
                        proc_macro2::Spacing::Alone,
                    )));
                }
            }
        }
    }

    // generates list of register specifications, e.g. `bias = in(reg) &params.bias.0, ...` as separate tokens
    #[allow(clippy::needless_range_loop)]
    fn add_regs(&self, t: &mut Vec<TokenTree>) {
        for i in 0..SAMPLE_PARAM_COUNT {
            if self.0 & (1 << i) != 0 {
                // HACK(eddyb) the extra `{...}` force the pointers to be to
                // fresh variables holding value copies, instead of the originals,
                // allowing `OpLoad _` inference to pick the appropriate type.
                let s = if is_grad(i) {
                    "grad_x=in(reg) &{params.grad.0.0},grad_y=in(reg) &{params.grad.0.1},"
                        .to_string()
                } else {
                    format!("{0} = in(reg) &{{params.{0}.0}},", SAMPLE_PARAM_NAMES[i])
                };
                let ts: proc_macro2::TokenStream = s.parse().unwrap();
                t.extend(ts);
            }
        }
    }
}

impl VisitMut for SampleImplRewriter {
    fn visit_impl_item_fn_mut(&mut self, item: &mut ImplItemFn) {
        // rewrite the last parameter of this method to be of type `SampleParams<...>` we generated earlier
        if let Some(syn::FnArg::Typed(p)) = item.sig.inputs.last_mut() {
            *p.ty.as_mut() = self.1.clone();
        }
        syn::visit_mut::visit_impl_item_fn_mut(self, item);
    }

    fn visit_macro_mut(&mut self, m: &mut syn::Macro) {
        if m.path.is_ident("asm") {
            // this is where the asm! block is manipulated
            let t = m.tokens.clone();
            let mut new_t = Vec::new();
            let mut altered = false;

            for tt in t {
                match tt {
                    TokenTree::Literal(l) => {
                        if let Ok(l) = syn::parse::<syn::LitStr>(l.to_token_stream().into()) {
                            // found a string literal
                            let s = l.value();
                            if s.contains("$PARAMS") {
                                altered = true;
                                // add load instructions before the sampling instruction
                                self.add_loads(&mut new_t);
                                // and insert image operands
                                let s = s.replace("$PARAMS", &self.get_operands());
                                let lod_type = if self.0 & SAMPLE_PARAM_EXPLICIT_LOD_MASK != 0 {
                                    "ExplicitLod"
                                } else {
                                    "ImplicitLod"
                                };
                                let s = s.replace("$LOD", lod_type);

                                new_t.push(TokenTree::Literal(proc_macro2::Literal::string(
                                    s.as_str(),
                                )));
                            } else {
                                new_t.push(TokenTree::Literal(l.token()));
                            }
                        } else {
                            new_t.push(TokenTree::Literal(l));
                        }
                    }
                    _ => {
                        new_t.push(tt);
                    }
                }
            }

            if altered {
                // finally, add register specs
                self.add_regs(&mut new_t);
            }

            // replace all tokens within the asm! block with our new list
            m.tokens = new_t.into_iter().collect();
        }
    }
}

pub fn gen_sample_param_permutations(item: TokenStream) -> TokenStream {
    let item_impl = syn::parse_macro_input!(item as syn::ItemImpl);
    let mut fns = Vec::new();

    for m in 1..(1 << SAMPLE_PARAM_COUNT) {
        fns.push(SampleImplRewriter::rewrite(m, &item_impl));
    }

    // uncomment to output generated tokenstream to stdout
    //println!("{}", quote! { #(#fns)* }.to_string());
    quote! { #(#fns)* }.into()
}
