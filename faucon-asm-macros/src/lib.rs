//! Internal implementation details of `faucon-asm`.
//!
//! Do not use this crate directly!

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse::Error, parse_macro_input, DeriveInput, Result};

#[proc_macro_derive(Instruction, attributes(opcode, subopcode, operands))]
pub fn instruction(input: TokenStream) -> TokenStream {
    // Parse input into a syntax tree.
    let ast = parse_macro_input!(input as DeriveInput);

    // Build the impl.
    impl_instruction(&ast).unwrap().into()
}

fn impl_instruction(ast: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    if let syn::Data::Enum(data) = &ast.data {
        let mut fields: Vec<proc_macro2::TokenStream> = Vec::new();
        for variant in data.variants.iter() {
            let opcode = parse_int_arg(&extract_attribute(variant, "opcode")?)?;
            let subopcode = parse_int_arg(&extract_attribute(variant, "subopcode")?)?;
            let operands = parse_str_arg(&extract_attribute(variant, "operands")?)?;

            fields.push(quote! {
                (#opcode, #subopcode) => Instruction::#variant.ident(#opcode, #subopcode, #operands)
            });
        }

        Ok(quote! {
            impl From<(u8, u8)> for Instruction {
                fn from(op: (u8, u8)) -> Option<Self> {
                    #(#fields),*
                }
            }
        })
    } else {
        panic!("#[derive(Instruction)] can only be applied to enums")
    }
}

struct Attr {
    pub ident: syn::Ident,
    pub meta: syn::NestedMeta,
}

impl Attr {
    fn new(ident: syn::Ident, meta: syn::NestedMeta) -> Self {
        Attr { ident, meta }
    }
}

fn extract_attribute(variant: &syn::Variant, name: &str) -> Result<Attr> {
    if let Some(attribute) = variant
        .attrs
        .iter()
        .find(|a| a.path.segments.len() == 1 && a.path.segments[0].ident == name)
    {
        if let syn::Meta::List(ref nested_meta) = attribute.parse_meta()? {
            if nested_meta.nested.len() == 1 {
                Ok(Attr::new(
                    attribute.path.segments[0].ident.clone(),
                    nested_meta.nested[0].clone(),
                ))
            } else {
                Err(Error::new(
                    attribute.path.segments[0].ident.span(),
                    format!("\"{}\" is expected to be a single value", name),
                ))
            }
        } else {
            Err(Error::new(
                attribute.path.segments[0].ident.span(),
                format!("\"{}\" is expected to be a single value", name),
            ))
        }
    } else {
        Err(Error::new(
            Span::call_site(),
            format!("attr \"{}\" missing", name),
        ))
    }
}

fn parse_int_arg(attr: &Attr) -> Result<u8> {
    if let syn::NestedMeta::Lit(syn::Lit::Int(ref int)) = attr.meta {
        Ok(int.base10_parse().unwrap())
    } else {
        Err(Error::new(
            Span::call_site(),
            format!("\"{}\" is expected to be an int", attr.ident),
        ))
    }
}

fn parse_str_arg(attr: &Attr) -> Result<String> {
    if let syn::NestedMeta::Lit(syn::Lit::Str(ref str)) = attr.meta {
        Ok(str.value())
    } else {
        Err(Error::new(
            Span::call_site(),
            format!("\"{}\" is expected to be an int", attr.ident),
        ))
    }
}
