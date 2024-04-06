use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::{
    meta,
    parse::{Parse, ParseStream, Parser},
    parse_macro_input, parse_quote, Expr, ItemImpl, Path, Type,
};

use crate::{
    extract_trait_from_impl, parse_meta_value_into_arg, strip_trait_operand_type, ItemImplEmpty,
};

pub fn assign_via_binop_ref_lhs_attr(args: TokenStream, item: TokenStream) -> TokenStream {
    struct Args {
        func: Ident,
        binop: Expr,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut func = None;
            let mut binop = None;
            meta::parser(|meta| {
                if meta.path.is_ident("fn") {
                    parse_meta_value_into_arg(&meta, &mut func)
                } else if meta.path.is_ident("binop") {
                    parse_meta_value_into_arg(&meta, &mut binop)
                } else {
                    Err(meta.error("unrecognized argument"))
                }
            })
            .parse2(input.parse()?)?;
            Ok(Self {
                func: func.ok_or_else(|| input.error("required argument 'fn' missing"))?,
                binop: binop.ok_or_else(|| input.error("required argument 'binop' missing"))?,
            })
        }
    }
    let Args { func, binop } = parse_macro_input!(args);
    let InputItem { mut item_impl, rhs, .. } = parse_macro_input!(item);
    item_impl.items.push(parse_quote! {
        fn #func(&mut self, rhs: #rhs) {
            *self = #binop(&*self, rhs);
        }
    });
    item_impl.into_token_stream().into()
}

pub fn assign_via_assign_ref_attr(args: TokenStream, item: TokenStream) -> TokenStream {
    struct Args {
        func: Ident,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut func = None;
            meta::parser(|meta| {
                if meta.path.is_ident("fn") {
                    parse_meta_value_into_arg(&meta, &mut func)
                } else {
                    Err(meta.error("unrecognized argument"))
                }
            })
            .parse2(input.parse()?)?;
            Ok(Self { func: func.ok_or_else(|| input.error("required argument 'fn' missing"))? })
        }
    }
    let Args { func } = parse_macro_input!(args);
    let InputItem { mut item_impl, tr_stem, rhs } = parse_macro_input!(item);
    item_impl.items.push(parse_quote! {
        fn #func(&mut self, rhs: #rhs) {
            #tr_stem::#func(self, &rhs)
        }
    });
    item_impl.into_token_stream().into()
}

pub fn binop_via_assign_attr(args: TokenStream, item: TokenStream) -> TokenStream {
    struct Args {
        func: Ident,
        assign: Expr,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut func = None;
            let mut assign = None;
            meta::parser(|meta| {
                if meta.path.is_ident("fn") {
                    parse_meta_value_into_arg(&meta, &mut func)
                } else if meta.path.is_ident("assign") {
                    parse_meta_value_into_arg(&meta, &mut assign)
                } else {
                    Err(meta.error("unrecognized argument"))
                }
            })
            .parse2(input.parse()?)?;
            Ok(Self {
                func: func.ok_or_else(|| input.error("required argument 'fn' missing"))?,
                assign: assign.ok_or_else(|| input.error("required argument 'assign' missing"))?,
            })
        }
    }
    let Args { func, assign } = parse_macro_input!(args);
    let InputItem { mut item_impl, rhs, .. } = parse_macro_input!(item);
    item_impl.items.push(parse_quote! { type Output = Self; });
    item_impl.items.push(parse_quote! {
        fn #func(mut self, rhs: #rhs) -> Self::Output {
            #assign(&mut self, rhs);
            self
        }
    });
    item_impl.into_token_stream().into()
}

pub fn binop_via_binop_ref_rhs_attr(args: TokenStream, item: TokenStream) -> TokenStream {
    struct Args {
        func: Ident,
        output: Type,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut func = None;
            let mut output = None;
            meta::parser(|meta| {
                if meta.path.is_ident("fn") {
                    parse_meta_value_into_arg(&meta, &mut func)
                } else if meta.path.is_ident("output") {
                    parse_meta_value_into_arg(&meta, &mut output)
                } else {
                    Err(meta.error("unrecognized argument"))
                }
            })
            .parse2(input.parse()?)?;
            Ok(Self {
                func: func.ok_or_else(|| input.error("required argument 'fn' missing"))?,
                output: output.ok_or_else(|| input.error("required argument 'output' missing"))?,
            })
        }
    }
    let Args { func, output } = parse_macro_input!(args);
    let InputItem { mut item_impl, tr_stem, rhs } = parse_macro_input!(item);
    item_impl.items.push(parse_quote! { type Output = #output; });
    item_impl.items.push(parse_quote! {
        fn #func(self, rhs: #rhs) -> Self::Output {
            #tr_stem::#func(self, &rhs)
        }
    });
    item_impl.into_token_stream().into()
}

pub fn binop_via_binop_ref_lhs_attr(args: TokenStream, item: TokenStream) -> TokenStream {
    struct Args {
        func: Ident,
        output: Type,
    }
    impl Parse for Args {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut func = None;
            let mut output = None;
            meta::parser(|meta| {
                if meta.path.is_ident("fn") {
                    parse_meta_value_into_arg(&meta, &mut func)
                } else if meta.path.is_ident("output") {
                    parse_meta_value_into_arg(&meta, &mut output)
                } else {
                    Err(meta.error("unrecognized argument"))
                }
            })
            .parse2(input.parse()?)?;
            Ok(Self {
                func: func.ok_or_else(|| input.error("required argument 'fn' missing"))?,
                output: output.ok_or_else(|| input.error("required argument 'output' missing"))?,
            })
        }
    }
    let Args { func, output } = parse_macro_input!(args);
    let InputItem { mut item_impl, tr_stem, rhs } = parse_macro_input!(item);
    item_impl.items.push(parse_quote! { type Output = #output; });
    item_impl.items.push(parse_quote! {
        fn #func(self, rhs: #rhs) -> Self::Output {
            #tr_stem::#func(&self, rhs)
        }
    });
    item_impl.into_token_stream().into()
}

struct InputItem {
    item_impl: ItemImpl,
    tr_stem: Path,
    rhs: Type,
}
impl Parse for InputItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ItemImplEmpty(item_impl) = input.parse()?;
        let mut tr_stem = extract_trait_from_impl(&item_impl)?.clone();
        let rhs = strip_trait_operand_type(&mut tr_stem)?;
        Ok(Self { item_impl, tr_stem, rhs })
    }
}
