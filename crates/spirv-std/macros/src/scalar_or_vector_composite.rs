use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::punctuated::Punctuated;
use syn::{
    Data, DataStruct, DataUnion, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, GenericParam,
    Token,
};

pub fn derive(item: TokenStream) -> syn::Result<TokenStream> {
    // Whenever we'll properly resolve the crate symbol, replace this.
    let spirv_std = quote!(spirv_std);

    // Defer all validation to our codegen backend. Rather than erroring here, emit garbage.
    let item = syn::parse2::<DeriveInput>(item)?;
    let content = match &item.data {
        Data::Enum(_) => derive_enum(&spirv_std, &item),
        Data::Struct(data) => derive_struct(&spirv_std, data),
        Data::Union(DataUnion { union_token, .. }) => {
            Err(syn::Error::new_spanned(union_token, "Union not supported"))
        }
    }?;

    let ident = &item.ident;
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

    Ok(quote! {
        impl<#gens> #spirv_std::ScalarOrVectorComposite for #ident<#gen_refs> #where_clause {
            #[inline]
            fn transform<F: #spirv_std::ScalarOrVectorTransform>(self, f: &mut F) -> Self {
                #content
            }
        }
    })
}

pub fn derive_struct(spirv_std: &TokenStream, data: &DataStruct) -> syn::Result<TokenStream> {
    Ok(match &data.fields {
        Fields::Named(FieldsNamed { named, .. }) => {
            let content = named
                .iter()
                .map(|f| {
                    let ident = &f.ident;
                    quote!(#ident: #spirv_std::ScalarOrVectorComposite::transform(self.#ident, f))
                })
                .collect::<Punctuated<_, Token![,]>>();
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
    })
}

pub fn derive_enum(spirv_std: &TokenStream, item: &DeriveInput) -> syn::Result<TokenStream> {
    let mut attributes = item.attrs.iter().filter(|a| a.path().is_ident("repr"));
    let repr = match (attributes.next(), attributes.next()) {
        (None, _) => Err(syn::Error::new_spanned(
            item,
            "Missing #[repr(...)] attribute",
        )),
        (Some(repr), None) => Ok(repr),
        (Some(_), Some(_)) => Err(syn::Error::new_spanned(
            item,
            "Multiple #[repr(...)] attributes found",
        )),
    }?;
    let prim = &repr.meta.require_list()?.tokens;
    Ok(quote! {
        #spirv_std::assert_is_integer::<#prim>();
        <Self as From<#prim>>::from(#spirv_std::ScalarOrVectorComposite::transform(<Self as Into<#prim>>::into(self), f))
    })
}
