use std::mem;

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{
    meta::ParseNestedMeta,
    parse::{Nothing, Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    GenericArgument, ItemImpl, Path, PathArguments, ReturnType, Type,
};

// --------------------------------------------------------------------------

mod binop;

#[proc_macro_attribute]
pub fn assign_via_binop_ref_lhs(args: TokenStream, item: TokenStream) -> TokenStream {
    binop::assign_via_binop_ref_lhs_attr(args, item)
}
#[proc_macro_attribute]
pub fn assign_via_assign_ref(args: TokenStream, item: TokenStream) -> TokenStream {
    binop::assign_via_assign_ref_attr(args, item)
}
#[proc_macro_attribute]
pub fn binop_via_assign(args: TokenStream, item: TokenStream) -> TokenStream {
    binop::binop_via_assign_attr(args, item)
}
#[proc_macro_attribute]
pub fn binop_via_binop_ref_rhs(args: TokenStream, item: TokenStream) -> TokenStream {
    binop::binop_via_binop_ref_rhs_attr(args, item)
}
#[proc_macro_attribute]
pub fn binop_via_binop_ref_lhs(args: TokenStream, item: TokenStream) -> TokenStream {
    binop::binop_via_binop_ref_lhs_attr(args, item)
}

// --------------------------------------------------------------------------

#[proc_macro_attribute]
pub fn partial_ord_via_ord(args: TokenStream, item: TokenStream) -> TokenStream {
    parse_macro_input!(args as Nothing);
    let ItemImplEmpty(mut item_impl) = parse_macro_input!(item);
    item_impl.items.push(parse_quote! {
        fn partial_cmp(&self, other: &Self) -> ::std::option::Option<::std::cmp::Ordering> {
            ::std::option::Option::Some(::std::cmp::Ord::cmp(self, other))
        }
    });
    item_impl.into_token_stream().into()
}

#[cfg(feature = "num-traits")]
#[proc_macro_attribute]
pub fn sum_via_fold_zero_add(args: TokenStream, item: TokenStream) -> TokenStream {
    parse_macro_input!(args as Nothing);
    let ItemImplEmpty(mut item_impl) = parse_macro_input!(item);
    item_impl.items.push(parse_quote! {
        fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
            // FIXME: do these paths need to be imported?
            iter.fold(Zero::zero(), Add::add)
        }
    });
    item_impl.into_token_stream().into()
}

// --------------------------------------------------------------------------

fn parse_meta_value_into_arg<T: Parse>(
    meta: &ParseNestedMeta<'_>,
    arg: &mut Option<T>,
) -> syn::Result<()> {
    if arg.is_some() {
        return Err(meta.error("conflicting argument"));
    }
    *arg = Some(meta.value()?.parse()?);
    Ok(())
}

fn extract_trait_from_impl(item_impl: &ItemImpl) -> syn::Result<&Path> {
    match &item_impl.trait_ {
        Some((None, tr, _)) => Ok(tr),
        Some((Some(not), _, _)) => {
            Err(syn::Error::new(not.span, "negative implementations are not allowed"))
        }
        None => Err(syn::Error::new(item_impl.span(), "must be a trait implementation")),
    }
}

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

/// An [ItemImpl] whose body must be empty.
struct ItemImplEmpty(ItemImpl);
impl Parse for ItemImplEmpty {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let item_impl = input.parse::<ItemImpl>()?;
        if !item_impl.items.is_empty() {
            return Err(syn::Error::new(
                item_impl.brace_token.span.join(),
                "impl body must be empty",
            ));
        }
        Ok(Self(item_impl))
    }
}
