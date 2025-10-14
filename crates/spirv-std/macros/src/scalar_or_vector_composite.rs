use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::punctuated::Punctuated;
use syn::{Fields, FieldsNamed, FieldsUnnamed, GenericParam, Token};

pub fn derive(item: TokenStream) -> syn::Result<TokenStream> {
    // Whenever we'll properly resolve the crate symbol, replace this.
    let spirv_std = quote!(spirv_std);

    // Defer all validation to our codegen backend. Rather than erroring here, emit garbage.
    let item = syn::parse2::<syn::ItemStruct>(item)?;
    let struct_ident = &item.ident;
    let gens = &item.generics.params;
    let gen_refs = &item
        .generics
        .params
        .iter()
        .map(|p| match p {
            GenericParam::Lifetime(p) => p.lifetime.to_token_stream(),
            GenericParam::Type(p) => p.ident.to_token_stream(),
            GenericParam::Const(p) => p.ident.to_token_stream(),
        })
        .collect::<Punctuated<_, Token![,]>>();
    let where_clause = &item.generics.where_clause;

    let content =
        match item.fields {
            Fields::Named(FieldsNamed { named, .. }) => {
                let content = named.iter().map(|f| {
                let ident = &f.ident;
                quote!(#ident: #spirv_std::ScalarOrVectorComposite::transform(self.#ident, f))
            }).collect::<Punctuated<_, Token![,]>>();
                quote!(Self { #content })
            }
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                let content = (0..unnamed.len())
                    .map(|i| {
                        let i = syn::Index::from(i);
                        quote!(#spirv_std::ScalarOrVectorComposite::transform(self.#i, f))
                    })
                    .collect::<Punctuated<_, Token![,]>>();
                quote!(Self(#content))
            }
            Fields::Unit => quote!(Self),
        };

    Ok(quote! {
        impl<#gens> #spirv_std::ScalarOrVectorComposite for #struct_ident<#gen_refs> #where_clause {
            #[inline]
            fn transform<F: #spirv_std::ScalarOrVectorTransform>(self, f: &mut F) -> Self {
                #content
            }
        }
    })
}
