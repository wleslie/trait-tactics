//! Macros that provide common patterns for implementing traits in terms of other traits.

use itertools::Itertools;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    GenericArgument, Generics, Ident, Path, PathArguments, Token, Type,
};

#[proc_macro]
pub fn assign_via_binop_ref(input: TokenStream) -> TokenStream {
    struct Args {
        impl_tok: Token![impl],
        generics: Generics,
        tr: Path,
        rhs: Type,
        for_tok: Token![for],
        self_ty: Type,
        fn_tok: Token![fn],
        method: Ident,
        delegate: Path,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let impl_tok = input.parse()?;
            let mut generics =
                if input.peek(Token![<]) { input.parse()? } else { Generics::default() };
            let tr = input.parse::<Path>()?;
            let rhs = if let PathArguments::AngleBracketed(list) =
                &tr.segments.last().expect("path cannot be empty").arguments
            {
                list.args.iter().exactly_one().ok().map(|arg| {
                    if let GenericArgument::Type(ty) = arg {
                        Ok(ty)
                    } else {
                        Err(syn::Error::new(arg.span(), "generic argument must be a type"))
                    }
                })
            } else {
                None
            }
            .unwrap_or_else(|| {
                Err(syn::Error::new(tr.span(), "expected exactly one generic argument"))
            })?
            .clone();
            let for_tok = input.parse()?;
            let self_ty = input.parse()?;
            generics.where_clause = input.parse()?;
            let content;
            braced!(content in input);
            let fn_tok = content.parse()?;
            let method = content.parse()?;
            content.parse::<Token![=>]>()?;
            let delegate = content.parse()?;
            Ok(Self { impl_tok, generics, tr, rhs, for_tok, self_ty, fn_tok, method, delegate })
        }
    }
    let Args { impl_tok, generics, tr, rhs, for_tok, self_ty, fn_tok, method, delegate, .. } =
        parse_macro_input!(input);
    let where_clause = &generics.where_clause;

    quote! {
        #impl_tok #generics #tr #for_tok #self_ty #where_clause {
            #fn_tok #method(&mut self, rhs: #rhs) {
                *self = #delegate(&*self, rhs);
            }
        }
    }
    .into()
}
