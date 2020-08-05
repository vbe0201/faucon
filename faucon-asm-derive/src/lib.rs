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
        let name = &ast.ident;

        // Prepare the opcode tables.
        let mut mrr = vec![quote! { None }; 0x3];
        let mut srri8 = vec![quote! { None }; 0x10];
        let mut srwi8 = vec![quote! { None }; 0x10];
        let mut srwi16 = vec![quote! { None }; 0x10];
        let mut sri8 = vec![quote! { None }; 0x10];
        let mut sri16 = vec![quote! { None }; 0x10];
        let mut swi8 = vec![quote! { None }; 0x10];
        let mut smi8 = vec![quote! { None }; 0x10];
        let mut smi16 = vec![quote! { None }; 0x10];
        let mut srri16 = vec![quote! { None }; 0x10];
        let mut srr = vec![quote! { None }; 0x10];
        let mut srw = vec![quote! { None }; 0x10];
        let mut swr = vec![quote! { None }; 0x10];
        let mut smr = vec![quote! { None }; 0x10];
        let mut srrw = vec![quote! { None }; 0x10];
        let mut sm = vec![quote! { None }; 0x10];
        let mut i24 = vec![quote! { None }; 0x10];
        let mut rwi8 = vec![quote! { None }; 0x10];
        let mut ri32 = vec![quote! { None }; 0x1];
        let mut rwi16 = vec![quote! { None }; 0x10];
        let mut mi8 = vec![quote! { None }; 0x10];
        let mut mi16 = vec![quote! { None }; 0x10];
        let mut ri8 = vec![quote! { None }; 0x10];
        let mut i8 = vec![quote! { None }; 0x40];
        let mut i16 = vec![quote! { None }; 0x40];
        let mut rir = vec![quote! { None }; 0x10];
        let mut n = vec![quote! { None }; 0x10];
        let mut r = vec![quote! { None }; 0x10];
        let mut rr = vec![quote! { None }; 0x10];
        let mut w = vec![quote! { None }; 0x10];
        let mut mr = vec![quote! { None }; 0x10];
        let mut rw = vec![quote! { None }; 0x10];
        let mut rrw = vec![quote! { None }; 0x10];

        let mut register_instruction =
            |vname: &syn::Ident, opcode: u8, subopcode: u8, operands: Vec<syn::Meta>| {
                let (size, a, b) = parse_opcode(opcode);
                let b = b as usize;
                let subopcode = subopcode as usize;

                let mut real_operands = Vec::new();
                real_operands.extend(operands.iter().map(|o| quote! { #o }));
                while real_operands.len() < 3 {
                    real_operands.push(quote! { NOP })
                }

                let value = quote! {
                    Some(instruction_meta!(#vname, #opcode, #subopcode, [#(#real_operands),*]))
                };

                match size {
                    0x0..=0x2 => match a {
                        0x0 => {
                            mrr[subopcode] = value;
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
                            0x2 => {
                                srr[subopcode] = value;
                            }
                            0x4 => {
                                swi8[subopcode] = value;
                            }
                            0x5 => {
                                srri8[subopcode] = value;
                            }
                            0x6 => {
                                smi8[subopcode] = value;
                            }
                            0x7 => {
                                smi16[subopcode] = value;
                            }
                            0x8 => {
                                srri16[subopcode] = value;
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
                            0xF => {
                                srr[subopcode] = value;
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
                            0x3 => {
                                i16[subopcode] = value;
                            }
                            0x4 => {
                                i8[subopcode] = value;
                            }
                            0x5 => {
                                i16[subopcode] = value;
                            }
                            0x6 => {
                                rir[subopcode] = value;
                            }
                            0x7 => {
                                rir[subopcode] = value;
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
            const FORM_MRR: [Option<InstructionMeta>; 0x3] = [
                #(#mrr),*
            ];

            const FORM_SRRI8: [Option<InstructionMeta>; 0x10] = [
                #(#srri8),*
            ];

            const FORM_SRWI8: [Option<InstructionMeta>; 0x10] = [
                #(#srwi8),*
            ];

            const FORM_SRWI16: [Option<InstructionMeta>; 0x10] = [
                #(#srwi16),*
            ];

            const FORM_SRI8: [Option<InstructionMeta>; 0x10] = [
                #(#sri8),*
            ];

            const FORM_SRI16: [Option<InstructionMeta>; 0x10] = [
                #(#sri16),*
            ];

            const FORM_SWI8: [Option<InstructionMeta>; 0x10] = [
                #(#swi8),*
            ];

            const FORM_SMI8: [Option<InstructionMeta>; 0x10] = [
                #(#smi8),*
            ];

            const FORM_SMI16: [Option<InstructionMeta>; 0x10] = [
                #(#smi16),*
            ];

            const FORM_SRRI16: [Option<InstructionMeta>; 0x10] = [
                #(#srri16),*
            ];

            const FORM_SRR: [Option<InstructionMeta>; 0x10] = [
                #(#srr),*
            ];

            const FORM_SRW: [Option<InstructionMeta>; 0x10] = [
                #(#srw),*
            ];

            const FORM_SWR: [Option<InstructionMeta>; 0x10] = [
                #(#swr),*
            ];

            const FORM_SMR: [Option<InstructionMeta>; 0x10] = [
                #(#smr),*
            ];

            const FORM_SRRW: [Option<InstructionMeta>; 0x10] = [
                #(#srrw),*
            ];

            const FORM_SM: [Option<InstructionMeta>; 0x10] = [
                #(#sm),*
            ];

            const FORM_I24: [Option<InstructionMeta>; 0x10] = [
                #(#i24),*
            ];

            const FORM_RWI8: [Option<InstructionMeta>; 0x10] = [
                #(#rwi8),*
            ];

            const FORM_RI32: [Option<InstructionMeta>; 0x1] = [
                #(#ri32),*
            ];

            const FORM_RWI16: [Option<InstructionMeta>; 0x10] = [
                #(#rwi16),*
            ];

            const FORM_MI8: [Option<InstructionMeta>; 0x10] = [
                #(#mi8),*
            ];

            const FORM_MI16: [Option<InstructionMeta>; 0x10] = [
                #(#mi16),*
            ];

            const FORM_RI8: [Option<InstructionMeta>; 0x10] = [
                #(#ri8),*
            ];

            const FORM_I8: [Option<InstructionMeta>; 0x40] = [
                #(#i8),*
            ];

            const FORM_I16: [Option<InstructionMeta>; 0x40] = [
                #(#i16),*
            ];

            const FORM_RIR: [Option<InstructionMeta>; 0x10] = [
                #(#rir),*
            ];

            const FORM_N: [Option<InstructionMeta>; 0x10] = [
                #(#n),*
            ];

            const FORM_R: [Option<InstructionMeta>; 0x10] = [
                #(#r),*
            ];

            const FORM_RR: [Option<InstructionMeta>; 0x10] = [
                #(#rr),*
            ];

            const FORM_W: [Option<InstructionMeta>; 0x10] = [
                #(#w),*
            ];

            const FORM_MR: [Option<InstructionMeta>; 0x10] = [
                #(#mr),*
            ];

            const FORM_RW: [Option<InstructionMeta>; 0x10] = [
                #(#rw),*
            ];

            const FORM_RRW: [Option<InstructionMeta>; 0x10] = [
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
                pub fn parse_sized_form_1(a: u8, b: u8, subopcode: u8) -> Option<InstructionMeta> {
                    let b = b as usize;
                    let subopcode = subopcode as usize;

                    match a {
                        0x0 => FORM_MRR[subopcode].clone(),
                        0x1 => FORM_SRWI8[b].clone(),
                        0x2 => FORM_SRWI16[b].clone(),
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
                        0x0 => FORM_SRI8[subopcode].clone(),
                        0x1 => FORM_SRI16[subopcode].clone(),
                        0x2 => FORM_SRR[0x2].clone(),
                        0x4 => FORM_SWI8[subopcode].clone(),
                        0x5 => FORM_SRRI8[subopcode].clone(),
                        0x6 => FORM_SMI8[subopcode].clone(),
                        0x7 => FORM_SMI16[subopcode].clone(),
                        0x8 => FORM_SRRI16[subopcode].clone(),
                        0x9 => FORM_SRW[subopcode].clone(),
                        0xA => FORM_SWR[subopcode].clone(),
                        0xB => FORM_SMR[subopcode].clone(),
                        0xC => FORM_SRRW[subopcode].clone(),
                        0xD => FORM_SM[subopcode].clone(),
                        0xE => FORM_I24[subopcode].clone(),
                        0xF => FORM_SRR[subopcode].clone(),
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
                        0x0 => FORM_RWI8[b].clone(),
                        0x1 => FORM_RI32[0].clone(),
                        0x2 => FORM_RWI16[b].clone(),
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
                        0x0 => FORM_MI8[subopcode].clone(),
                        0x1 => FORM_MI16[subopcode].clone(),
                        0x2 => FORM_RI8[subopcode].clone(),
                        0x3 => FORM_I16[subopcode].clone(),
                        0x4 => FORM_I8[subopcode].clone(),
                        0x5 => FORM_I16[subopcode].clone(),
                        0x6 => FORM_RIR[subopcode].clone(),
                        0x7 => FORM_RIR[subopcode].clone(),
                        0x8 => FORM_N[subopcode].clone(),
                        0x9 => FORM_R[subopcode].clone(),
                        0xA => FORM_RR[subopcode].clone(),
                        0xC => FORM_W[subopcode].clone(),
                        0xD => FORM_MR[subopcode].clone(),
                        0xE => FORM_RW[subopcode].clone(),
                        0xF => FORM_RRW[subopcode].clone(),
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
