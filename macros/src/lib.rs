use std::mem;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    GenericArgument, Generics, Ident, Path, PathArguments, ReturnType, Token, Type,
};

// --------------------------------------------------------------------------

#[proc_macro]
pub fn assign_via_binop_ref_lhs(input: TokenStream) -> TokenStream {
    let CustomImplTraitFn { head, rhs, fn_decl, tail: Delegation { delegate }, .. } =
        parse_macro_input!(input);
    quote! {
        #head {
            #fn_decl(&mut self, rhs: #rhs) {
                *self = #delegate(&*self, rhs);
            }
        }
    }
    .into()
}

#[proc_macro]
pub fn assign_via_assign_ref(input: TokenStream) -> TokenStream {
    let CustomImplTraitFn { head, tr_without_rhs, rhs, fn_decl, tail: Empty } =
        parse_macro_input!(input);
    let method = &fn_decl.ident;
    quote! {
        #head {
            #fn_decl(&mut self, rhs: #rhs) {
                #tr_without_rhs::#method(self, &rhs)
            }
        }
    }
    .into()
}

#[proc_macro]
pub fn binop_via_assign(input: TokenStream) -> TokenStream {
    let CustomImplTraitFn { head, rhs, fn_decl, tail: Delegation { delegate }, .. } =
        parse_macro_input!(input);
    quote! {
        #head {
            type Output = Self;
            #fn_decl(mut self, rhs: #rhs) -> Self::Output {
                #delegate(&mut self, rhs);
                self
            }
        }
    }
    .into()
}

#[proc_macro]
pub fn binop_via_binop_ref_rhs(input: TokenStream) -> TokenStream {
    let CustomImplTraitFn::<ReturnType> { head, tr_without_rhs, rhs, fn_decl, tail: ret } =
        parse_macro_input!(input);
    let output_ty = ret.into_type();
    let method = &fn_decl.ident;
    quote! {
        #head {
            type Output = #output_ty;
            #fn_decl(self, rhs: #rhs) -> Self::Output {
                #tr_without_rhs::#method(self, &rhs)
            }
        }
    }
    .into()
}

#[proc_macro]
pub fn binop_via_binop_ref_lhs(input: TokenStream) -> TokenStream {
    let CustomImplTraitFn::<ReturnType> { head, tr_without_rhs, rhs, fn_decl, tail: ret } =
        parse_macro_input!(input);
    let output_ty = ret.into_type();
    let method = &fn_decl.ident;
    quote! {
        #head {
            type Output = #output_ty;
            #fn_decl(self, rhs: #rhs) -> Self::Output {
                #tr_without_rhs::#method(&self, rhs)
            }
        }
    }
    .into()
}

// --------------------------------------------------------------------------

/// Given a [Path] of the form `P<T>`, removes and returns the `T`.
///
/// If the path has no generic argument, returns the `Self` type, because most built-in
/// binary-operator traits like [std::ops::Add] declare a default operand `<Rhs = Self>`.
fn strip_trait_operand_type(tr: &mut Path) -> syn::Result<Type> {
    let path_args = mem::replace(
        &mut tr.segments.last_mut().expect("path cannot be empty").arguments,
        PathArguments::None,
    );
    Ok(match path_args {
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

trait ReturnTypeExt {
    fn into_type(self) -> Type;
}
impl ReturnTypeExt for ReturnType {
    fn into_type(self) -> Type {
        match self {
            ReturnType::Default => syn::parse_quote! { () },
            ReturnType::Type(_, ty) => *ty,
        }
    }
}

/// `impl ... for ... { fn foo ... }`
struct CustomImplTraitFn<Tail> {
    head: ImplTraitHead,
    tr_without_rhs: Path,
    rhs: Type,
    fn_decl: FnDecl,
    tail: Tail,
}
impl<Tail: Parse> Parse for CustomImplTraitFn<Tail> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let head = input.parse::<ImplTraitHead>()?;
        let mut tr_without_rhs = head.tr.clone();
        let rhs = strip_trait_operand_type(&mut tr_without_rhs)?;
        let content;
        braced!(content in input);
        let fn_decl = content.parse()?;
        let tail = content.parse()?;
        Ok(Self { head, tr_without_rhs, rhs, fn_decl, tail })
    }
}

/// `impl ... for ...`
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
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self { impl_tok, generics, tr, for_tok, self_ty } = self;
        let where_clause = &generics.where_clause;
        tokens.extend(quote!(#impl_tok #generics #tr #for_tok #self_ty #where_clause));
    }
}

/// `fn foo`
struct FnDecl {
    fn_tok: Token![fn],
    ident: Ident,
}
impl Parse for FnDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fn_tok = input.parse()?;
        let ident = input.parse()?;
        Ok(Self { fn_tok, ident })
    }
}
impl ToTokens for FnDecl {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self { fn_tok, ident } = self;
        tokens.extend(quote!(#fn_tok #ident));
    }
}

/// `=> Foo::bar`
struct Delegation {
    delegate: Path,
}
impl Parse for Delegation {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![=>]>()?;
        let delegate = input.parse()?;
        Ok(Self { delegate })
    }
}

struct Empty;
impl Parse for Empty {
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        Ok(Self)
    }
}
