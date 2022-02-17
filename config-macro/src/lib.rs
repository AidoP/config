use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, DeriveInput, spanned::Spanned};

#[proc_macro_derive(Config)]
pub fn derive_config(input: TokenStream) -> TokenStream {
    let mut errors = Vec::new();
    let mut error = |span: Span, msg: &str| errors.push(quote_spanned!{span=> ::std::compile_error!(#msg); });
    let input: DeriveInput = parse_macro_input!(input);
    let DeriveInput { ident, data, ..} = &input;
    let config_name = ident.to_string();
    
    let s = match data {
        syn::Data::Struct(s) => s,
        syn::Data::Enum(_) => return quote!{ ::std::compile_error!("this trait cannot be derived for enums"); }.into(),
        syn::Data::Union(_) => return quote!{ ::std::compile_error!("this trait cannot be derived for unions"); }.into()
    };

    let field_setters = s.fields.iter()
        .filter_map(|field|
            if let Some(ident) = &field.ident {
                let field_name = ident.to_string();
                let from_value_fn = quote_spanned! {field.ty.span()=> ::config::FromValue::from_value};
                Some(quote!{
                    #field_name => ::std::result::Result::Ok(match value.kind() {
                        ::config::ValueKind::Default => self.#ident = Self::default().#ident,
                        ::config::ValueKind::Value(value) => #from_value_fn(&mut self.#ident, &value)?
                    })
                })
            } else {
                error(field.span(), "Config fields must be named");
                None
            }
        );

    quote!{
        impl ::config::Config for #ident {
            const NAME: &'static str = #config_name;
            fn set<'a>(&mut self, field: &::config::Field<'a>) -> ::config::Result<'a, ()> {
                let (name, value) = field.clone().destructure();
                match name {
                    #(#field_setters,)*
                    _ => ::std::result::Result::Err(::config::Error::NoField(field.clone(), Self::NAME))
                }
            }
        }
    }.into()
}