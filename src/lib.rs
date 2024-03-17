//! Macros that provide common patterns for implementing traits in terms of other traits.

use itertools::Itertools;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
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
        head: ImplTraitHead,
        rhs: Type,
        fn_tok: Token![fn],
        method: Ident,
        delegate: Path,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let head = input.parse::<ImplTraitHead>()?;
            let rhs = trait_operand_type(&head.tr)?;
            let content;
            braced!(content in input);
            let fn_tok = content.parse()?;
            let method = content.parse()?;
            content.parse::<Token![=>]>()?;
            let delegate = content.parse()?;
            Ok(Self { head, rhs, fn_tok, method, delegate })
        }
    }
    let Args { head, rhs, fn_tok, method, delegate, .. } = parse_macro_input!(input);

    quote! {
        #head {
            #fn_tok #method(&mut self, rhs: #rhs) {
                *self = #delegate(&*self, rhs);
            }
        }
    }
    .into()
}

fn trait_operand_type(tr: &Path) -> syn::Result<Type> {
    Ok(match &tr.segments.last().expect("path cannot be empty").arguments {
        PathArguments::None => None,
        PathArguments::AngleBracketed(list) => {
            let mut it = list.args.iter();
            if let Some(arg) = it.next() {
                if it.next().is_some() {
                    return Err(syn::Error::new(
                        tr.span(),
                        "expected exactly one generic argument",
                    ));
                }
                if let GenericArgument::Type(ty) = arg {
                    Some(ty.clone())
                } else {
                    return Err(syn::Error::new(arg.span(), "generic argument must be a type"));
                }
            } else {
                None
            }
        }
        _ => return Err(syn::Error::new(tr.span(), "generic arguments must be angle-bracketed")),
    }
    .unwrap_or(syn::parse_quote!(Self)))
}

struct ImplTraitHead {
    impl_tok: Token![impl],
    generics: Generics,
    tr: Path,
    for_tok: Token![for],
    self_ty: Type,
}
impl Parse for ImplTraitHead {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let impl_tok = input.parse()?;
        let mut generics = if input.peek(Token![<]) { input.parse()? } else { Generics::default() };
        let tr = input.parse::<Path>()?;
        let for_tok = input.parse()?;
        let self_ty = input.parse()?;
        generics.where_clause = input.parse()?;
        Ok(Self { impl_tok, generics, tr, for_tok, self_ty })
    }
}
impl ToTokens for ImplTraitHead {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Self { impl_tok, generics, tr, for_tok, self_ty } = self;
        let where_clause = &generics.where_clause;
        tokens.extend(quote!(#impl_tok #generics #tr #for_tok #self_ty #where_clause));
    }
}
