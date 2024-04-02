use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ReturnType};

use crate::{CustomImplTraitFn, Delegation, Empty, ReturnTypeExt};

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
