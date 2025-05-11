use proc_macro::TokenStream;
use syn::{
    parse_macro_input,
    parse_quote,
    Expr,
    ExprBinary,
    Token,
};
use syn::punctuated::Punctuated;
use quote::quote;

#[proc_macro]
pub fn rats(tokens: TokenStream) -> TokenStream {
    let exprs = parse_macro_input!(tokens with Punctuated::<Expr, Token![,]>::parse_terminated);
    let ratted = exprs.into_iter().map(floatify);
    quote!( [#(#ratted),*].into() ).into()
}

fn floatify(expr: Expr) -> Expr {
    match expr {
        Expr::Binary(expr) => {
            let left = Box::new(floatify(*expr.left));
            let right = Box::new(floatify(*expr.right));
            Expr::Binary(ExprBinary{
                attrs: expr.attrs,
                left,
                op: expr.op,
                right,
            })
        }
        _ => {
            parse_quote!((#expr) as f32)
        }
    }
}
