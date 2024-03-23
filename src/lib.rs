//! Macros that provide common patterns for implementing traits in terms of other traits.

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

/// Implements `&mut A ⋄= B` in terms of `&A ⋄ &B → A` via `*self = &*self ⋄ y`.
#[proc_macro]
pub fn assign_via_binop_ref_lhs(input: TokenStream) -> TokenStream {
    struct Args {
        head: ImplTraitHead,
        rhs: Type,
        decl: FnDecl,
        delegation: Delegation,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let head = input.parse::<ImplTraitHead>()?;
            let rhs = trait_operand_type(&head.tr)?;
            let content;
            braced!(content in input);
            let decl = content.parse()?;
            let delegation = content.parse()?;
            Ok(Self { head, rhs, decl, delegation })
        }
    }
    let Args { head, rhs, decl, delegation } = parse_macro_input!(input);
    let Delegation { delegate } = delegation;
    quote! {
        #head {
            #decl(&mut self, rhs: #rhs) {
                *self = #delegate(&*self, rhs);
            }
        }
    }
    .into()
}

/// Implements `&mut A ⋄= B` in terms of `&mut A ⋄= &B` via `self ⋄= &y`.
#[proc_macro]
pub fn assign_via_assign_ref(input: TokenStream) -> TokenStream {
    struct Args {
        head: ImplTraitHead,
        rhs: Type,
        decl: FnDecl,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let head = input.parse::<ImplTraitHead>()?;
            let rhs = trait_operand_type(&head.tr)?;
            let content;
            braced!(content in input);
            let decl = content.parse()?;
            Ok(Self { head, rhs, decl })
        }
    }
    let Args { head, rhs, decl } = parse_macro_input!(input);
    let stripped_tr = {
        let mut tr = head.tr.clone();
        strip_path_arguments(&mut tr);
        tr
    };
    let method = &decl.ident;
    quote! {
        #head {
            #decl(&mut self, rhs: #rhs) {
                #stripped_tr::#method(self, &rhs)
            }
        }
    }
    .into()
}

/// Implements `A ⋄ B → A` in terms of `&mut A ⋄= B` via `x ⋄= y; x`.
#[proc_macro]
pub fn binop_via_assign(input: TokenStream) -> TokenStream {
    struct Args {
        head: ImplTraitHead,
        rhs: Type,
        decl: FnDecl,
        delegation: Delegation,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let head = input.parse::<ImplTraitHead>()?;
            let rhs = trait_operand_type(&head.tr)?;
            let content;
            braced!(content in input);
            let decl = content.parse()?;
            let delegation = content.parse()?;
            Ok(Self { head, rhs, decl, delegation })
        }
    }
    let Args { head, rhs, decl, delegation } = parse_macro_input!(input);
    let Delegation { delegate } = delegation;
    quote! {
        #head {
            type Output = Self;
            #decl(mut self, rhs: #rhs) -> Self::Output {
                #delegate(&mut self, rhs);
                self
            }
        }
    }
    .into()
}

/// Implements `A ⋄ B → A` in terms of `A ⋄ &B → A` via `x ⋄ &y`.
#[proc_macro]
pub fn binop_via_binop_ref_rhs(input: TokenStream) -> TokenStream {
    struct Args {
        head: ImplTraitHead,
        rhs: Type,
        decl: FnDecl,
        ret: ReturnType,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let head = input.parse::<ImplTraitHead>()?;
            let rhs = trait_operand_type(&head.tr)?;
            let content;
            braced!(content in input);
            let decl = content.parse()?;
            let ret = content.parse()?;
            Ok(Self { head, rhs, decl, ret })
        }
    }
    let Args { head, rhs, decl, ret } = parse_macro_input!(input);
    let output_ty = match ret {
        ReturnType::Default => syn::parse_quote! { () },
        ReturnType::Type(_, ty) => *ty,
    };
    let stripped_tr = {
        let mut tr = head.tr.clone();
        strip_path_arguments(&mut tr);
        tr
    };
    let method = &decl.ident;
    quote! {
        #head {
            type Output = #output_ty;
            #decl(self, rhs: #rhs) -> Self::Output {
                #stripped_tr::#method(self, &rhs)
            }
        }
    }
    .into()
}

/// Implements `A ⋄ B → A` in terms of `&A ⋄ B → A` via `&x ⋄ y`.
#[proc_macro]
pub fn binop_via_binop_ref_lhs(input: TokenStream) -> TokenStream {
    struct Args {
        head: ImplTraitHead,
        rhs: Type,
        decl: FnDecl,
        ret: ReturnType,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let head = input.parse::<ImplTraitHead>()?;
            let rhs = trait_operand_type(&head.tr)?;
            let content;
            braced!(content in input);
            let decl = content.parse()?;
            let ret = content.parse()?;
            Ok(Self { head, rhs, decl, ret })
        }
    }
    let Args { head, rhs, decl, ret } = parse_macro_input!(input);
    let output_ty = match ret {
        ReturnType::Default => syn::parse_quote! { () },
        ReturnType::Type(_, ty) => *ty,
    };
    let stripped_tr = {
        let mut tr = head.tr.clone();
        strip_path_arguments(&mut tr);
        tr
    };
    let method = &decl.ident;
    quote! {
        #head {
            type Output = #output_ty;
            #decl(self, rhs: #rhs) -> Self::Output {
                #stripped_tr::#method(&self, rhs)
            }
        }
    }
    .into()
}

// --------------------------------------------------------------------------

fn path_arguments(path: &Path) -> &PathArguments {
    &path.segments.last().expect("path cannot be empty").arguments
}
fn path_arguments_mut(path: &mut Path) -> &mut PathArguments {
    &mut path.segments.last_mut().expect("path cannot be empty").arguments
}
fn strip_path_arguments(path: &mut Path) -> PathArguments {
    mem::replace(path_arguments_mut(path), PathArguments::None)
}

fn trait_operand_type(tr: &Path) -> syn::Result<Type> {
    Ok(match path_arguments(tr) {
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
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self { impl_tok, generics, tr, for_tok, self_ty } = self;
        let where_clause = &generics.where_clause;
        tokens.extend(quote!(#impl_tok #generics #tr #for_tok #self_ty #where_clause));
    }
}

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
