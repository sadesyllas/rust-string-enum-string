#![feature(default_free_fn)]

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, Attribute, Data, DeriveInput, Expr, ExprAssign, ExprLit, Lit, Variant,
};

#[proc_macro_attribute]
pub fn string_enum_string(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(item as DeriveInput);
    let enum_ident = ast.ident.clone();
    let mut variants: Vec<(Variant, Option<String>, Option<String>, isize)> = Vec::new();
    let mut repr = quote! { isize };

    if let Some(attr) = ast.attrs.iter().find(|attr| {
        if let Some(path) = attr.path.get_ident() {
            path == "repr"
        } else {
            false
        }
    }) {
        if let Ok(_type) = attr.parse_args::<Expr>() {
            repr = _type.into_token_stream();
        }
    }

    if let Data::Enum(ref mut data) = ast.data {
        let mut value = 0isize;

        for variant in &mut data.variants {
            value = get_next_value(variant, value);

            let mut other_variant_names: Vec<(Option<String>, Option<String>)> = Vec::new();

            variant.attrs = variant
                .attrs
                .drain(0..variant.attrs.len())
                .filter(|attr| {
                    if let Some(path) = attr.path.get_ident() {
                        if path == "variant" {
                            other_variant_names.append(&mut get_other_variant_names(attr));

                            return false;
                        }
                    }

                    true
                })
                .collect();

            if !other_variant_names
                .iter()
                .any(|(display, _)| display.is_some())
            {
                variants.push((
                    variant.clone(),
                    Some(variant.ident.to_string()),
                    None,
                    value,
                ));
            }

            if !other_variant_names.iter().any(|(_, parse)| parse.is_some()) {
                variants.push((
                    variant.clone(),
                    None,
                    Some(variant.ident.to_string().trim().to_lowercase().to_string()),
                    value,
                ));
            }

            for (display, parse) in other_variant_names {
                variants.push((variant.clone(), display, parse, value));
            }
        }
    } else {
        panic!(
            "Could not parse enum variants for {}",
            enum_ident.to_string()
        )
    }

    let mut from_numeric_arms: Vec<quote::__private::TokenStream> = Vec::new();
    let mut from_numeric_arms_seen_names: HashSet<String> = HashSet::new();

    for (Variant { ident, .. }, _, _, value) in &variants {
        if from_numeric_arms_seen_names.contains(&ident.to_string()) {
            continue;
        }

        from_numeric_arms_seen_names.insert(ident.to_string());

        from_numeric_arms.push(quote! { #value => #enum_ident::#ident });
    }

    let mut display_arms: Vec<quote::__private::TokenStream> = Vec::new();
    let mut from_str_arms: Vec<quote::__private::TokenStream> = Vec::new();
    let mut as_parsed_arms: Vec<quote::__private::TokenStream> = Vec::new();

    for (Variant { ident, .. }, display, parse, _) in &variants {
        if let Some(display) = display {
            display_arms.push(quote! { #enum_ident::#ident => f.write_str(#display).unwrap() });
        }

        if let Some(parse) = parse {
            from_str_arms.push(quote! { #parse => #enum_ident::#ident });

            as_parsed_arms.push(quote! { #enum_ident::#ident => #parse })
        }
    }

    quote! {
        #ast

        impl #enum_ident {
            pub fn as_parsed(&self) -> &'static str {
                match self {
                    #( #as_parsed_arms, )*
                }
            }
        }

        impl std::fmt::Display for #enum_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #( #display_arms, )*
                }

                Ok(())
            }
        }

        impl From<#repr> for #enum_ident {
            fn from(value: #repr) -> Self {
                match value as isize {
                    #( #from_numeric_arms, )*
                    other => panic!(
                        "{} is not a valid numeric value for enumeration {}",
                        other,
                        std::any::type_name::<#enum_ident>()
                    ),
                }
            }
        }

        impl From<&str> for #enum_ident {
            fn from(value: &str) -> Self {
                match value.trim().to_lowercase().as_str() {
                    #( #from_str_arms, )*
                    _ => unreachable!(),
                }
            }
        }
    }
    .into()
}

fn get_next_value(variant: &Variant, value: isize) -> isize {
    if let Some((
        _,
        Expr::Lit(ExprLit {
            lit: Lit::Int(value),
            ..
        }),
    )) = &variant.discriminant
    {
        value.base10_parse().unwrap()
    } else {
        value + 1
    }
}

fn get_other_variant_names(attr: &Attribute) -> Vec<(Option<String>, Option<String>)> {
    let mut other_variant_names: Vec<(Option<String>, Option<String>)> = Vec::new();

    if let Ok(Expr::Tuple(tuple)) = attr.parse_args::<Expr>() {
        for expr in tuple.elems {
            if let Some((key, value)) = parse_variant_key_value(&expr) {
                if key == "display" {
                    other_variant_names.push((Some(value), None));
                } else {
                    other_variant_names.push((None, Some(value)));
                };
            }
        }
    }

    if let Ok(expr) = attr.parse_args::<Expr>() {
        if let Some((key, value)) = parse_variant_key_value(&expr) {
            if key == "display" {
                other_variant_names.push((Some(value), None));
            } else {
                other_variant_names.push((None, Some(value)));
            };
        }
    }

    other_variant_names
}

fn parse_variant_key_value(expr: &Expr) -> Option<(String, String)> {
    if let Expr::Assign(ExprAssign { left, right, .. }) = expr {
        let key = quote! { #left }.to_string();

        if key != "display" && key != "parse" {
            return None;
        }

        if let Expr::Lit(ExprLit {
            lit: Lit::Str(value),
            ..
        }) = &**right
        {
            let value = value.value().trim().to_owned();

            if value.is_empty() {
                panic!("An alternative display or parse name for an enum variant must have a non empty value");
            }

            return Some((key, value));
        }
    }

    None
}
