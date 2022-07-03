use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, DeriveInput, spanned::Spanned};

#[proc_macro_derive(Config)]
pub fn derive_config(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let DeriveInput { ident, data, ..} = &input;
    
    match data {
        syn::Data::Struct(s) => derive_struct(ident, s),
        syn::Data::Enum(e) => derive_enum(ident, e),
        syn::Data::Union(_) => return quote!{ ::std::compile_error!("this trait cannot be derived for unions"); }.into()
    }
}

fn derive_struct(ident: &syn::Ident, s: &syn::DataStruct) -> TokenStream {
    let mut errors = Vec::new();
    let mut error = |span: Span, msg: &str| errors.push(quote_spanned!{span=> ::std::compile_error!(#msg); });
    let struct_ty = format!("struct {}", ident);
    /* May use for user help printing
    let mut first = true;
    for field in s.fields.iter() {
        if let Some(ident) = &field.ident {
            if !first {
                struct_ty.push_str(", ")
            } else {
                first = false;
            }
            // TODO: proper Display printing of types
            let ty = &field.ty;
            let ty = quote!{ #ty }.to_string();
            let ty = ty.replace(" < ", "<");
            let ty = ty.replace(" >", ">");
            let ty = ty.replace(" :: ", "::");
            struct_ty.push_str(&format!("{}", ident))
        } else {
            error(field.span(), "Config fields must be named")
        }
    }
    struct_ty.push_str(" }");*/

    let field_setters: Vec<_> = s.fields.iter()
        .filter_map(|field|
            if let syn::Visibility::Inherited = field.vis { 
                None
            } else if let Some(ident) = &field.ident {
                let field_name = ident.to_string();
                let from_value_fn = quote_spanned! {field.ty.span()=> ::config::FromValue::from_value_partial};
                Some(quote!{
                    #field_name => ::std::result::Result::Ok(#from_value_fn(&mut self.#ident, field.value())?)
                })
            } else {
                error(field.span(), "Config fields must be named");
                None
            }
        )
        .collect();
    
    let field_constructors: Vec<_> = s.fields.iter()
        .filter_map(|field|
            if let (Some(ident), syn::Visibility::Inherited) = (&field.ident, &field.vis) { 
                Some(quote_spanned!{field.ty.span()=> #ident: ::std::default::Default::default() })
            } else if let Some(ident) = &field.ident {
                let field_name = ident.to_string();
                let from_value_fn = quote_spanned! {field.ty.span()=> ::config::FromValue::from_value_new};
                Some(quote!{
                    #ident: #from_value_fn(
                        fields
                            .remove(#field_name)
                            .ok_or(::config::Error::MissingField(value.clone(), #field_name, Self::data_type()))?
                            .value()
                    )?
                })
            } else {
                error(field.span(), "Config fields must be named");
                None
            }
        )
        .collect();
    
    quote!{
        #(#errors)*
        impl ::config::Struct for #ident {
            fn set<'a>(&mut self, field: &::config::Field<'a>) -> ::config::Result<'a, ()> {
                let field_name = field.field().clean(::config::DataType::Named("field name"))?;
                match field_name.as_ref() {
                    #(#field_setters,)*
                    field_name => ::std::result::Result::Err(::config::Error::NoField(field.clone(), <Self as ::config::FromValue>::data_type(), field_name.into()))
                }
            }
        }
        impl ::config::FromValue for #ident {
            fn data_type() -> ::config::DataType { ::config::DataType::Named(#struct_ty) }
            fn from_value_new<'a>(value: &::config::Value<'a>) -> ::config::Result<'a, Self> {
                match value {
                    none @ ::config::identifier!("none") => ::std::result::Result::Err(::config::Error::NoDefault(none.clone(), Self::data_type())),
                    ::config::Value::Struct { fields: field_list, ..} => {
                        let mut fields = ::std::collections::HashMap::new();
                        for field in field_list.iter() {
                            let name = field.field().clean(::config::DataType::Named("field name"))?.to_string();
                            fields.insert(name, field);
                        }
                        let new = Self {
                            #(
                                #field_constructors
                            ),*
                        };
                        if let ::std::option::Option::Some((name, field)) = fields.into_iter().next() {
                            ::std::result::Result::Err(::config::Error::NoField(field.clone(), Self::data_type(), name))
                        } else {
                            ::std::result::Result::Ok(new)
                        }
                    }
                    value => ::std::result::Result::Err(::config::Error::InvalidType(value.clone(), Self::data_type()))
                }
            }
            fn from_value_partial<'a>(&mut self, value: &::config::Value<'a>) -> ::config::Result<'a, ()> {
                match value {
                    none @ ::config::identifier!("none") => ::std::result::Result::Err(::config::Error::NoDefault(none.clone(), Self::data_type())),
                    ::config::Value::Struct { fields, ..} => {
                        for field in fields.iter() {
                            ::config::Struct::set(self, field)?
                        }
                        ::std::result::Result::Ok(())
                    }
                    value => ::std::result::Result::Err(::config::Error::InvalidType(value.clone(), Self::data_type()))
                }
            }
        }
    }.into()
}
fn derive_enum(ident: &syn::Ident, e: &syn::DataEnum) -> TokenStream {
    use syn::{Fields, FieldsUnnamed, FieldsNamed};
    let enum_ty = format!("enum {}", ident);

    let variant_constructors = e.variants.iter().map(|variant| {
        let variant_data_type = format!("{}::{}", enum_ty, variant.ident);
        let variant_ident = &variant.ident;
        let variant_name = variant.ident.to_string().to_lowercase();
        match &variant.fields {
            Fields::Unit => quote!{ ::config::identifier!(#variant_name) => ::std::result::Result::Ok(Self::#variant_ident) },
            Fields::Unnamed(FieldsUnnamed {unnamed, ..}) if unnamed.len() == 0 => quote!{ ::config::identifier!(#variant_name) => ::std::result::Result::Ok(Self::#variant_ident()) },
            Fields::Named(FieldsNamed {named, ..}) if named.len() == 0 => quote!{ ::config::identifier!(#variant_name) => ::std::result::Result::Ok(Self::#variant_ident{}) },
            Fields::Unnamed(FieldsUnnamed {unnamed, ..}) => {
                let field_constructors = unnamed.iter().enumerate().map(|(i, f)| {
                    let field_ty = &f.ty;
                    let field_index = i;
                    quote_spanned!{f.span()=> <#field_ty as ::config::FromValue>::from_value_new(&values[#field_index])? }
                });
                let field_count = unnamed.len();
                quote!{
                    ::config::Value::NamedArray { name: ::config::Spanned { str: #variant_name, ..}, values, ..} => if values.len() != #field_count {
                        ::std::result::Result::Err(::config::Error::InvalidType(value.clone(), Self::data_type()))
                    } else {
                        ::std::result::Result::Ok(Self::#variant_ident(#(#field_constructors),*))
                    }
                }
            },
            Fields::Named(FieldsNamed {named, ..}) => {
                let field_constructors = named.iter().map(|f| {
                    let field_ident = f.ident.as_ref().unwrap();
                    let field_name = field_ident.to_string();
                    let field_ty = &f.ty;
                    quote_spanned!{f.span()=>
                        #field_ident: <#field_ty as ::config::FromValue>::from_value_new(
                            fields.remove(#field_name)
                                .ok_or(::config::Error::MissingField(value.clone(), #field_name, data_type()))?.value()
                        )?
                    }
                });
                quote!{
                    ::config::Value::NamedStruct { name: ::config::Spanned { str: #variant_name, ..}, fields: field_list, ..} => {
                        fn data_type() -> ::config::DataType { ::config::DataType::Named(#variant_data_type) }
                        let mut fields = ::std::collections::HashMap::new();
                        for field in field_list.iter() {
                            fields.insert(field.field().clean(::config::DataType::Named("field name"))?, field);
                        }
                        let this = Self::#variant_ident { #(#field_constructors),* };
                        if let Some((field_name, &field)) = fields.iter().next() {
                            return ::std::result::Result::Err(::config::Error::NoField(field.clone(), data_type(), field_name.to_string()))
                        }
                        ::std::result::Result::Ok(this)
                    }
                }
            }
        }
    });
    let variant_setters = e.variants.iter().map(|variant| {
        let variant_data_type = format!("{}::{}", enum_ty, variant.ident);
        let variant_ident = &variant.ident;
        let variant_name = variant.ident.to_string().to_lowercase();
        match &variant.fields {
            Fields::Unit => quote!{ ::config::identifier!(#variant_name) => *self = Self::#variant_ident },
            Fields::Unnamed(FieldsUnnamed {unnamed, ..}) if unnamed.len() == 0 => quote!{ ::config::identifier!(#variant_name) => *self = Self::#variant_ident() },
            Fields::Named(FieldsNamed {named, ..}) if named.len() == 0 => quote!{ ::config::identifier!(#variant_name) => *self = Self::#variant_ident{} },
            Fields::Unnamed(FieldsUnnamed {unnamed, ..}) => {
                let field_constructors = unnamed.iter().enumerate().map(|(i, f)| {
                    let field_ty = &f.ty;
                    let field_index = i;
                    quote_spanned!{f.span()=> <#field_ty as ::config::FromValue>::from_value_new(&values[#field_index])? }
                });
                let field_count = unnamed.len();
                quote!{
                    ::config::Value::NamedArray { name: ::config::Spanned { str: #variant_name, ..}, values, ..} => if values.len() != #field_count {
                        return ::std::result::Result::Err(::config::Error::InvalidType(value.clone(), Self::data_type()))
                    } else {
                        *self = Self::#variant_ident(#(#field_constructors),*)
                    }
                }
            },
            Fields::Named(FieldsNamed {named, ..}) => {
                let variant_fields = named.iter().map(|f| {
                    let field_ident = f.ident.as_ref().unwrap();
                    quote_spanned!{f.span()=> #field_ident }
                });
                let field_constructors = named.iter().map(|f| {
                    let field_ident = f.ident.as_ref().unwrap();
                    let field_name = field_ident.to_string();
                    let field_ty = &f.ty;
                    quote_spanned!{f.span()=> #field_name => <#field_ty as ::config::FromValue>::from_value_partial(#field_ident, __field__.value())? }
                });
                quote!{
                    ::config::Value::NamedStruct { name: ::config::Spanned { str: #variant_name, ..}, fields: __fields__, ..} => match self {
                        Self::#variant_ident { #(#variant_fields),* } => {
                            fn data_type() -> ::config::DataType { ::config::DataType::Named(#variant_data_type) }
                            for __field__ in __fields__.into_iter() {
                                match __field__.field().clean(::config::DataType::Named("field name"))?.as_ref() {
                                    #(#field_constructors,)*
                                    field_name => return ::std::result::Result::Err(::config::Error::NoField(__field__.clone(), data_type(), field_name.to_string()))
                                }
                            }
                        },
                        _ => *self = Self::from_value_new(value)?
                    }
                }
            }
        }
    });

    quote!{
        impl ::config::FromValue for #ident {
            fn data_type() -> ::config::DataType { ::config::DataType::Named(#enum_ty) }
            fn from_value_new<'a>(value: &::config::Value<'a>) -> ::config::Result<'a, Self> {
                match value {
                    #(#variant_constructors,)*
                    _ => ::std::result::Result::Err(::config::Error::InvalidType(value.clone(), Self::data_type()))
                }
            }
            fn from_value_partial<'a>(&mut self, value: &::config::Value<'a>) -> ::config::Result<'a, ()> {
                match value {
                    #(#variant_setters,)*
                    _ => return ::std::result::Result::Err(::config::Error::InvalidType(value.clone(), Self::data_type()))
                }
                #[allow(unreachable_code)]
                Ok(())
            }
        }
    }.into()
}