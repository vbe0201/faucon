//! Internal implementation details of `faucon-asm`.
//!
//! Do not use this crate directly!

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse::Error, parse_macro_input, DeriveInput, Result};

#[proc_macro_derive(Instruction, attributes(insn))]
pub fn instruction(input: TokenStream) -> TokenStream {
    // Parse input into a syntax tree.
    let ast = parse_macro_input!(input as DeriveInput);

    // Build the impl.
    impl_instruction(&ast).unwrap().into()
}

fn impl_instruction(ast: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    if let syn::Data::Enum(data) = &ast.data {
        let mut fields: Vec<proc_macro2::TokenStream> = Vec::new();
        for variant in data
            .variants
            .iter()
            .filter(|v| v.ident != syn::Ident::new("XXX", Span::call_site()))
            .collect::<Vec<&syn::Variant>>()
        {
            let (opcode, subopcode, operands) = extract_insn_attributes(variant)?;

            panic!(format!("{} {} {}", opcode, subopcode, operands));
        }

        unimplemented!()
    } else {
        Err(Error::new(
            Span::call_site(),
            "#[derive(Instruction)] can only be applied to enums",
        ))
    }
}

fn extract_insn_attributes(variant: &syn::Variant) -> Result<(u8, u8, String)> {
    if let Some(attr) = variant
        .attrs
        .iter()
        .find(|a| a.path.segments.len() == 1 && a.path.segments[0].ident == "insn")
    {
        if let syn::Meta::List(ref nested_list) = attr.parse_meta()? {
            if nested_list.nested.len() == 3 {
                let mut arguments = Vec::new();

                for nested_meta in nested_list.nested.iter() {
                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(ref value)) = nested_meta {
                        arguments.push(value);
                    } else {
                        return Err(Error::new(
                            attr.path.segments[0].ident.span(),
                            "#[insn] is expecting its arguments in name=value format",
                        ));
                    }
                }

                let opcode = parse_int_arg(arguments[0], "opcode")?;
                let subopcode = parse_int_arg(arguments[1], "subopcode")?;
                let operands = parse_str_arg(&arguments[2], "operands")?;
                Ok((opcode, subopcode, operands))
            } else {
                Err(Error::new(
                    attr.path.segments[0].ident.span(),
                    "#[insn] is expecting 3 arguments",
                ))
            }
        } else {
            Err(Error::new(
                attr.path.segments[0].ident.span(),
                "#[insn] is expecting arguments in list-style",
            ))
        }
    } else {
        Err(Error::new(
            Span::call_site(),
            "#[insn] attribute is missing",
        ))
    }
}

fn parse_int_arg(meta: &syn::MetaNameValue, name: &str) -> Result<u8> {
    verify_ident_name(&meta.path, name)?;

    if let syn::Lit::Int(ref int) = meta.lit {
        Ok(int.base10_parse().unwrap())
    } else {
        Err(Error::new(
            Span::call_site(),
            format!("Failed to parse the \"{}\" integer literal", name),
        ))
    }
}

fn parse_str_arg(meta: &syn::MetaNameValue, name: &str) -> Result<String> {
    verify_ident_name(&meta.path, name)?;

    if let syn::Lit::Str(ref str) = meta.lit {
        Ok(str.value())
    } else {
        Err(Error::new(
            Span::call_site(),
            format!("Failed to parse the \"{}\" string literal", name),
        ))
    }
}

fn verify_ident_name(path: &syn::Path, name: &str) -> Result<()> {
    if !path.is_ident(&syn::Ident::new(name, Span::call_site())) {
        Err(Error::new(
            Span::call_site(),
            format!("#[insn] must have a \"{}\" argument", name),
        ))
    } else {
        Ok(())
    }
}
