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
    //let x = ;
    //panic!("{}", x);
    impl_instruction(&ast).unwrap().into()
}

fn impl_instruction(ast: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    if let syn::Data::Enum(data) = &ast.data {
        let name = &ast.ident;

        // Prepare the opcode tables.
        let mut srri8 = vec![quote! { None }; 0xF];
        let mut srwi8 = vec![quote! { None }; 0xF];
        let mut srwi16 = vec![quote! { None }; 0xF];
        let mut sri8 = vec![quote! { None }; 0xF];
        let mut sri16 = vec![quote! { None }; 0xF];
        let mut swi8 = vec![quote! { None }; 0xF];
        let mut smi8 = vec![quote! { None }; 0xF];
        let mut smi16 = vec![quote! { None }; 0xF];
        let mut srr = vec![quote! { None }; 0xF];
        let mut srw = vec![quote! { None }; 0xF];
        let mut swr = vec![quote! { None }; 0xF];
        let mut smr = vec![quote! { None }; 0xF];
        let mut srrw = vec![quote! { None }; 0xF];
        let mut sm = vec![quote! { None }; 0xF];
        let mut i24 = vec![quote! { None }; 0xF];
        let mut rwi8 = vec![quote! { None }; 0xF];
        let mut ri32 = vec![quote! { None }; 0x1];
        let mut rwi16 = vec![quote! { None }; 0xF];
        let mut mi8 = vec![quote! { None }; 0xF];
        let mut mi16 = vec![quote! { None }; 0xF];
        let mut ri8 = vec![quote! { None }; 0xF];
        let mut i8 = vec![quote! { None }; 0x3F];
        let mut i16 = vec![quote! { None }; 0x3F];
        let mut n = vec![quote! { None }; 0xF];
        let mut r = vec![quote! { None }; 0xF];
        let mut rr = vec![quote! { None }; 0xF];
        let mut w = vec![quote! { None }; 0xF];
        let mut mr = vec![quote! { None }; 0xF];
        let mut rw = vec![quote! { None }; 0xF];
        let mut rrw = vec![quote! { None }; 0xF];

        let mut register_instruction =
            |vname: &syn::Ident, opcode: u8, subopcode: u8, operands: Vec<syn::Meta>| {
                let (size, a, b) = parse_opcode(opcode);
                let b = b as usize;
                let subopcode = subopcode as usize;

                let value = quote! {
                    Some(instruction_meta!(#vname, #opcode, #subopcode, vec![#(#operands),*]))
                };

                match size {
                    0x0..=0x2 => match a {
                        0x0 => {
                            srri8[b] = value;
                        }
                        0x1 => {
                            srwi8[b] = value;
                        }
                        0x2 => {
                            srwi16[b] = value;
                        }
                        0x3 => match b {
                            0x0 => {
                                sri8[subopcode] = value;
                            }
                            0x1 => {
                                sri16[subopcode] = value;
                            }
                            0x4 => {
                                swi8[subopcode] = value;
                            }
                            0x6 => {
                                smi8[subopcode] = value;
                            }
                            0x7 => {
                                smi16[subopcode] = value;
                            }
                            0x8 => {
                                srr[subopcode] = value;
                            }
                            0x9 => {
                                srw[subopcode] = value;
                            }
                            0xA => {
                                swr[subopcode] = value;
                            }
                            0xB => {
                                smr[subopcode] = value;
                            }
                            0xC => {
                                srrw[subopcode] = value;
                            }
                            0xD => {
                                sm[subopcode] = value;
                            }
                            0xE => {
                                i24[subopcode] = value;
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    },
                    0x3 => match a {
                        0x0 => {
                            rwi8[b] = value;
                        }
                        0x1 => {
                            ri32[0] = value;
                        }
                        0x2 => {
                            rwi16[b] = value;
                        }
                        0x3 => match b {
                            0x0 => {
                                mi8[subopcode] = value;
                            }
                            0x1 => {
                                mi16[subopcode] = value;
                            }
                            0x2 => {
                                ri8[subopcode] = value;
                            }
                            0x4 => {
                                i8[subopcode] = value;
                            }
                            0x5 => {
                                i16[subopcode] = value;
                            }
                            0x8 => {
                                n[subopcode] = value;
                            }
                            0x9 => {
                                r[subopcode] = value;
                            }
                            0xA => {
                                rr[subopcode] = value;
                            }
                            0xC => {
                                w[subopcode] = value;
                            }
                            0xD => {
                                mr[subopcode] = value;
                            }
                            0xE => {
                                rw[subopcode] = value;
                            }
                            0xF => {
                                rrw[subopcode] = value;
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
            };

        for variant in data
            .variants
            .iter()
            .filter(|v| v.ident != syn::Ident::new("XXX", Span::call_site()))
            .collect::<Vec<&syn::Variant>>()
        {
            let vname = &variant.ident;

            for result in extract_insn_attributes(variant)? {
                let (opcode, subopcode, operands) = result;

                register_instruction(vname, opcode, subopcode, operands);
            }
        }

        Ok(quote! {
            const FORM_SRRI8: [Option<InstructionMeta>; 0xF] = [
                #(#srri8),*
            ];

            const FORM_SRWI8: [Option<InstructionMeta>; 0xF] = [
                #(#srwi8),*
            ];

            const FORM_SRWI16: [Option<InstructionMeta>; 0xF] = [
                #(#srwi16),*
            ];

            const FORM_SRI8: [Option<InstructionMeta>; 0xF] = [
                #(#sri8),*
            ];

            const FORM_SRI16: [Option<InstructionMeta>; 0xF] = [
                #(#sri16),*
            ];

            const FORM_SWI8: [Option<InstructionMeta>; 0xF] = [
                #(#swi8),*
            ];

            const FORM_SMI8: [Option<InstructionMeta>; 0xF] = [
                #(#smi8),*
            ];

            const FORM_SMI16: [Option<InstructionMeta>; 0xF] = [
                #(#smi16),*
            ];

            const FORM_SRR: [Option<InstructionMeta>; 0xF] = [
                #(#srr),*
            ];

            const FORM_SRW: [Option<InstructionMeta>; 0xF] = [
                #(#srw),*
            ];

            const FORM_SWR: [Option<InstructionMeta>; 0xF] = [
                #(#swr),*
            ];

            const FORM_SMR: [Option<InstructionMeta>; 0xF] = [
                #(#smr),*
            ];

            const FORM_SRRW: [Option<InstructionMeta>; 0xF] = [
                #(#srrw),*
            ];

            const FORM_SM: [Option<InstructionMeta>; 0xF] = [
                #(#sm),*
            ];

            const FORM_I24: [Option<InstructionMeta>; 0xF] = [
                #(#i24),*
            ];

            const FORM_RWI8: [Option<InstructionMeta>; 0xF] = [
                #(#rwi8),*
            ];

            const FORM_RI32: [Option<InstructionMeta>; 0x1] = [
                #(#ri32),*
            ];

            const FORM_RWI16: [Option<InstructionMeta>; 0xF] = [
                #(#rwi16),*
            ];

            const FORM_MI8: [Option<InstructionMeta>; 0xF] = [
                #(#mi8),*
            ];

            const FORM_MI16: [Option<InstructionMeta>; 0xF] = [
                #(#mi16),*
            ];

            const FORM_RI8: [Option<InstructionMeta>; 0xF] = [
                #(#ri8),*
            ];

            const FORM_I8: [Option<InstructionMeta>; 0x3F] = [
                #(#i8),*
            ];

            const FORM_I16: [Option<InstructionMeta>; 0x3F] = [
                #(#i16),*
            ];

            const FORM_N: [Option<InstructionMeta>; 0xF] = [
                #(#n),*
            ];

            const FORM_R: [Option<InstructionMeta>; 0xF] = [
                #(#r),*
            ];

            const FORM_RR: [Option<InstructionMeta>; 0xF] = [
                #(#rr),*
            ];

            const FORM_W: [Option<InstructionMeta>; 0xF] = [
                #(#w),*
            ];

            const FORM_MR: [Option<InstructionMeta>; 0xF] = [
                #(#mr),*
            ];

            const FORM_RW: [Option<InstructionMeta>; 0xF] = [
                #(#rw),*
            ];

            const FORM_RRW: [Option<InstructionMeta>; 0xF] = [
                #(#rrw),*
            ];

            impl #name {
                /// Checks if the instruction is invalid or unknown.
                pub fn invalid(&self) -> bool {
                    match self {
                        #name::XXX => true,
                        _ => false,
                    }
                }

                /// Parses a sized instruction in form 1.
                ///
                /// This covers the opcode range from 0x00 to 0xBF. Form 1 essentially
                /// means that `a` decides the form and `b` selects the subopcode.
                pub fn parse_sized_form_1(a: u8, b: u8) -> Option<InstructionMeta> {
                    let b = b as usize;

                    match a {
                        0x0 => FORM_SRRI8[b],
                        0x1 => FORM_SRWI8[b],
                        0x2 => FORM_SRWI16[b],
                        _ => None,
                    }
                }

                /// Parses a sized instruction in form 2.
                ///
                /// This covers the opcode range from 0x00 to 0xBF. Form 2 essentially means that
                /// `b` decides the form and `subopcode` is used to match the actual instruction.
                pub fn parse_sized_form_2(b: u8, subopcode: u8) -> Option<InstructionMeta> {
                    let subopcode = subopcode as usize;

                    match b {
                        0x0 => FORM_SRI8[subopcode],
                        0x1 => FORM_SRI16[subopcode],
                        0x4 => FORM_SWI8[subopcode],
                        0x6 => FORM_SMI8[subopcode],
                        0x7 => FORM_SMI16[subopcode],
                        0x8 => FORM_SRR[subopcode],
                        0x9 => FORM_SRW[subopcode],
                        0xA => FORM_SWR[subopcode],
                        0xB => FORM_SMR[subopcode],
                        0xC => FORM_SRRW[subopcode],
                        0xD => FORM_SM[subopcode],
                        0xE => FORM_I24[subopcode],
                        _ => None,
                    }
                }

                /// Parses an unsized instruction in form 1.
                ///
                /// This covers the opcode range from 0xC0 to 0xFF. Form 1 essentially
                /// means that `a` decides the form and `b` selects the subopcode.
                pub fn parse_unsized_form_1(a: u8, b: u8) -> Option<InstructionMeta> {
                    let b = b as usize;

                    match a {
                        0x0 => FORM_RWI8[b],
                        0x1 => FORM_RI32[0],
                        0x2 => FORM_RWI16[b],
                        _ => None,
                    }
                }

                /// Parses an unsized instruction in form 2.
                ///
                /// This covers the opcode range from 0xC0 to 0xFF. Form 2 essentially means that
                /// `b` decides the form and `subopcode` is used to match the actual instruction.
                pub fn parse_unsized_form_2(b: u8, subopcode: u8) -> Option<InstructionMeta> {
                    let subopcode = subopcode as usize;

                    match b {
                        0x0 => FORM_MI8[subopcode],
                        0x1 => FORM_MI16[subopcode],
                        0x2 => FORM_RI8[subopcode],
                        0x4 => FORM_I8[subopcode],
                        0x5 => FORM_I16[subopcode],
                        0x8 => FORM_N[subopcode],
                        0x9 => FORM_R[subopcode],
                        0xA => FORM_RR[subopcode],
                        0xC => FORM_W[subopcode],
                        0xD => FORM_MR[subopcode],
                        0xE => FORM_RW[subopcode],
                        0xF => FORM_RRW[subopcode],
                        _ => None,
                    }
                }
            }
        })
    } else {
        Err(Error::new(
            Span::call_site(),
            "#[derive(Instruction)] can only be applied to enums",
        ))
    }
}

fn parse_opcode(opcode: u8) -> (u8, u8, u8) {
    (opcode >> 6, opcode >> 4 & 0x3, opcode & 0xF)
}

fn extract_insn_attributes(variant: &syn::Variant) -> Result<Vec<(u8, u8, Vec<syn::Meta>)>> {
    let mut results = Vec::new();

    for attr in variant
        .attrs
        .iter()
        .filter(|a| a.path.segments.len() == 1 && a.path.segments[0].ident == "insn")
    {
        if let syn::Meta::List(ref nested_list) = attr.parse_meta()? {
            if nested_list.nested.len() == 3 {
                let mut arguments = Vec::new();
                let mut operands = None;

                for nested_meta in nested_list.nested.iter() {
                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(ref value)) = nested_meta {
                        arguments.push(value);
                    } else if let syn::NestedMeta::Meta(syn::Meta::List(ref list)) = nested_meta {
                        operands = Some(list);
                    } else {
                        return Err(Error::new(
                            attr.path.segments[0].ident.span(),
                            "#[insn] is expecting its arguments in name=value format",
                        ));
                    }
                }

                let opcode = parse_int_arg(arguments[0], "opcode")?;
                let subopcode = parse_int_arg(arguments[1], "subopcode")?;
                let operands = parse_operands_vec(operands.unwrap(), "operands")?;
                results.push((opcode, subopcode, operands));
            } else {
                return Err(Error::new(
                    attr.path.segments[0].ident.span(),
                    "#[insn] is expecting 3 arguments",
                ));
            }
        } else {
            return Err(Error::new(
                attr.path.segments[0].ident.span(),
                "#[insn] is expecting arguments in list-style",
            ));
        }
    }

    if results.len() == 0 {
        Err(Error::new(
            Span::call_site(),
            "#[insn] attribute is missing",
        ))
    } else {
        Ok(results)
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

fn parse_operands_vec(meta: &syn::MetaList, name: &str) -> Result<Vec<syn::Meta>> {
    verify_ident_name(&meta.path, name)?;

    let mut result = Vec::new();
    for element in meta.nested.iter() {
        if let syn::NestedMeta::Meta(ref meta) = element {
            result.push(meta.clone());
        }
    }

    Ok(result)
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
