use std::convert::TryFrom;

use proc_macro2::{Span, TokenStream};
use syn::{parse::Error, Ident, Result};

use crate::parser;

/// The name of the attribute that is supported by the proc-macro.
pub const ATTR: &str = "insn";

/// Generates the AST that the `Instruction` derive macro will expand to.
///
/// The macro is used for conveniently adding support for new instructions
/// to the disassembler with lowest possible effort.
///
/// At compile-time, it expands to a bunch of const arrays which will be baked
/// into the final binary. These are arranged and ordered by the forms of
/// instruction groups and allow for fast opcode lookup through subopcode
/// indexing on the arrays.
pub fn impl_instruction(ast: &syn::DeriveInput) -> Result<TokenStream> {
    if let syn::Data::Enum(data) = &ast.data {
        generate_lookup_tables(&ast.ident, data)
    } else {
        Err(Error::new(
            Span::call_site(),
            "#[derive(Instruction)] can only be applied to enums",
        ))
    }
}

fn generate_lookup_tables(name: &Ident, data: &syn::DataEnum) -> Result<TokenStream> {
    let mut wi = vec![quote! { None }; 0x3];
    let mut srwi8 = vec![quote! { None }; 0x10];
    let mut srwi16 = vec![quote! { None }; 0x10];
    let mut sri8 = vec![quote! { None }; 0x10];
    let mut sri16 = vec![quote! { None }; 0x10];
    let mut srr = vec![quote! { None }; 0x10];
    let mut sunk = vec![quote! { None }; 0x10];
    let mut swi8 = vec![quote! { None }; 0x10];
    let mut srri8 = vec![quote! { None }; 0x10];
    let mut smi8 = vec![quote! { None }; 0x10];
    let mut smi16 = vec![quote! { None }; 0x10];
    let mut srri16 = vec![quote! { None }; 0x10];
    let mut srw = vec![quote! { None }; 0x10];
    let mut swrr = vec![quote! { None }; 0x10];
    let mut smr = vec![quote! { None }; 0x10];
    let mut srrw = vec![quote! { None }; 0x10];
    let mut sm = vec![quote! { None }; 0x10];
    let mut i24 = vec![quote! { None }; 0x3];
    let mut swr = vec![quote! { None }; 0x10];
    let mut rwi8 = vec![quote! { None }; 0x10];
    let mut wi32 = vec![quote! { None }; 0x1];
    let mut rwi16 = vec![quote! { None }; 0x10];
    let mut mi8 = vec![quote! { None }; 0x10];
    let mut mi16 = vec![quote! { None }; 0x10];
    let mut r1 = vec![quote! { None }; 0x10];
    let mut i16z = vec![quote! { None }; 0x10];
    let mut i8z = vec![quote! { None }; 0x40];
    let mut i16s = vec![quote! { None }; 0x40];
    let mut rri1 = vec![quote! { None }; 0x10];
    let mut rri2 = vec![quote! { None }; 0x10];
    let mut n = vec![quote! { None }; 0x10];
    let mut r2 = vec![quote! { None }; 0x10];
    let mut rr = vec![quote! { None }; 0x10];
    let mut ri = vec![quote! { None }; 0x10];
    let mut w = vec![quote! { None }; 0x10];
    let mut rm = vec![quote! { None }; 0x10];
    let mut rw = vec![quote! { None }; 0x10];
    let mut rrw = vec![quote! { None }; 0x10];

    // Given an opcode and a subopcode, this closure determines the appropriate opcode
    // table from the above vectors and inserts an InstructionMeta table at the index
    // of the subopcode to enhance instruction lookup speed through array indexing.
    let mut fill_table =
        |vname: &Ident, opcode: u8, subopcode: u8, operands: &mut Vec<TokenStream>| {
            let (size, a, b) = (
                (opcode >> 6) as usize,
                (opcode >> 4 & 0x3) as usize,
                (opcode & 0xF) as usize,
            );
            let subopcode = subopcode as usize;

            // faucon-asm stores the operands of each instruction in `[Argument; 3]` arrays.
            // For instructions that have less than 3 real operands, the remaining space in
            // the array is being filled out with `NOP` as a placeholder/padding.
            while operands.len() < 3 {
                operands.push(quote! { None });
            }

            let instruction_meta = quote! {
                Some(InstructionMeta::new(
                    InstructionKind::#vname,
                    #opcode as u8,
                    #subopcode as u8,
                    [#(#operands),*],
                ))
            };

            match (size, a, b) {
                (0x0..=0x2, 0x0, _) => wi[subopcode] = instruction_meta,
                (0x0..=0x2, 0x1, _) => srwi8[b] = instruction_meta,
                (0x0..=0x2, 0x2, _) => srwi16[b] = instruction_meta,
                (0x0..=0x2, 0x3, 0x0) => sri8[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x1) => sri16[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x2) => srr[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x3) => sunk[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x4) => swi8[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x5) => srri8[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x6) => smi8[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x7) => smi16[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x8) => srri16[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0x9) => srw[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0xA) => swrr[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0xB) => smr[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0xC) => srrw[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0xD) => sm[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0xE) => i24[subopcode] = instruction_meta,
                (0x0..=0x2, 0x3, 0xF) => swr[subopcode] = instruction_meta,
                (0x3, 0x0, _) => rwi8[b] = instruction_meta,
                (0x3, 0x1, _) => wi32[0] = instruction_meta,
                (0x3, 0x2, _) => rwi16[b] = instruction_meta,
                (0x3, 0x3, 0x0) => mi8[subopcode] = instruction_meta,
                (0x3, 0x3, 0x1) => mi16[subopcode] = instruction_meta,
                (0x3, 0x3, 0x2) => r1[subopcode] = instruction_meta,
                (0x3, 0x3, 0x3) => i16z[subopcode] = instruction_meta,
                (0x3, 0x3, 0x4) => i8z[subopcode] = instruction_meta,
                (0x3, 0x3, 0x5) => i16s[subopcode] = instruction_meta,
                (0x3, 0x3, 0x6) => rri1[subopcode] = instruction_meta,
                (0x3, 0x3, 0x7) => rri2[subopcode] = instruction_meta,
                (0x3, 0x3, 0x8) => n[subopcode] = instruction_meta,
                (0x3, 0x3, 0x9) => r2[subopcode] = instruction_meta,
                (0x3, 0x3, 0xA) => rr[subopcode] = instruction_meta,
                (0x3, 0x3, 0xB) => ri[subopcode] = instruction_meta,
                (0x3, 0x3, 0xC) => w[subopcode] = instruction_meta,
                (0x3, 0x3, 0xD) => rm[subopcode] = instruction_meta,
                (0x3, 0x3, 0xE) => rw[subopcode] = instruction_meta,
                (0x3, 0x3, 0xF) => rrw[subopcode] = instruction_meta,
                _ => unreachable!(),
            }
        };

    // Iterate through all the variants of the enum that derives from the `Instruction` macro
    // and parse the attribute decorators to insert the instructions into the lookup tables.
    for variant in data.variants.iter() {
        let vname = &variant.ident;

        // Parse and collect all the #[insn] decorators that are attached to this variant.
        let mut insn_attributes = variant
            .attrs
            .iter()
            .filter(|a| a.path.segments.len() == 1 && a.path.segments[0].ident == ATTR)
            .map(|a| Attributes::try_from(a).unwrap())
            .collect::<Vec<Attributes>>();

        // Use the previously collected data to add the instructions to the lookup tables.
        for Attributes {
            opcode,
            subopcode,
            operands,
        } in insn_attributes.iter_mut()
        {
            fill_table(vname, *opcode, *subopcode, operands);
        }
    }

    Ok(quote! {
        const FORM_WI: [Option<InstructionMeta>; 0x3] = [
            #(#wi),*
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

        const FORM_SRR: [Option<InstructionMeta>; 0x10] = [
            #(#srr),*
        ];

        const FORM_SUNK: [Option<InstructionMeta>; 0x10] = [
            #(#sunk),*
        ];

        const FORM_SWI8: [Option<InstructionMeta>; 0x10] = [
            #(#swi8),*
        ];

        const FORM_SRRI8: [Option<InstructionMeta>; 0x10] = [
            #(#srri8),*
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

        const FORM_SRW: [Option<InstructionMeta>; 0x10] = [
            #(#srw),*
        ];

        const FORM_SWRR: [Option<InstructionMeta>; 0x10] = [
            #(#swrr),*
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

        const FORM_I24: [Option<InstructionMeta>; 0x3] = [
            #(#i24),*
        ];

        const FORM_SWR: [Option<InstructionMeta>; 0x10] = [
            #(#swr),*
        ];

        const FORM_RWI8: [Option<InstructionMeta>; 0x10] = [
            #(#rwi8),*
        ];

        const FORM_WI32: [Option<InstructionMeta>; 0x1] = [
            #(#wi32),*
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

        const FORM_R1: [Option<InstructionMeta>; 0x10] = [
            #(#r1),*
        ];

        const FORM_I16Z: [Option<InstructionMeta>; 0x10] = [
            #(#i16z),*
        ];

        const FORM_I8Z: [Option<InstructionMeta>; 0x40] = [
            #(#i8z),*
        ];

        const FORM_I16S: [Option<InstructionMeta>; 0x40] = [
            #(#i16s),*
        ];

        const FORM_RRI1: [Option<InstructionMeta>; 0x10] = [
            #(#rri1),*
        ];

        const FORM_RRI2: [Option<InstructionMeta>; 0x10] = [
            #(#rri2),*
        ];

        const FORM_N: [Option<InstructionMeta>; 0x10] = [
            #(#n),*
        ];

        const FORM_R2: [Option<InstructionMeta>; 0x10] = [
            #(#r2),*
        ];

        const FORM_RR: [Option<InstructionMeta>; 0x10] = [
            #(#rr),*
        ];

        const FORM_RI: [Option<InstructionMeta>; 0x10] = [
            #(#ri),*
        ];

        const FORM_W: [Option<InstructionMeta>; 0x10] = [
            #(#w),*
        ];

        const FORM_RM: [Option<InstructionMeta>; 0x10] = [
            #(#rm),*
        ];

        const FORM_RW: [Option<InstructionMeta>; 0x10] = [
            #(#rw),*
        ];

        const FORM_RRW: [Option<InstructionMeta>; 0x10] = [
            #(#rrw),*
        ];

        impl #name {
            pub fn lookup_meta(
                sized: bool,
                a: u8,
                b: u8,
                subopcode: u8
            ) -> Option<InstructionMeta> {
                let b = b as usize;
                let subopcode = subopcode as usize;

                match (sized, a, b) {
                    (true, 0x0, _) => FORM_WI[subopcode].clone(),
                    (true, 0x1, _) => FORM_SRWI8[b].clone(),
                    (true, 0x2, _) => FORM_SRWI16[b].clone(),
                    (true, 0x3, 0x0) => FORM_SRI8[subopcode].clone(),
                    (true, 0x3, 0x1) => FORM_SRI16[subopcode].clone(),
                    (true, 0x3, 0x2) => FORM_SRR[subopcode].clone(),
                    (true, 0x3, 0x3) => FORM_SUNK[subopcode].clone(),
                    (true, 0x3, 0x4) => FORM_SWI8[subopcode].clone(),
                    (true, 0x3, 0x5) => FORM_SRRI8[subopcode].clone(),
                    (true, 0x3, 0x6) => FORM_SMI8[subopcode].clone(),
                    (true, 0x3, 0x7) => FORM_SMI16[subopcode].clone(),
                    (true, 0x3, 0x8) => FORM_SRRI16[subopcode].clone(),
                    (true, 0x3, 0x9) => FORM_SRW[subopcode].clone(),
                    (true, 0x3, 0xA) => FORM_SWRR[subopcode].clone(),
                    (true, 0x3, 0xB) => FORM_SMR[subopcode].clone(),
                    (true, 0x3, 0xC) => FORM_SRRW[subopcode].clone(),
                    (true, 0x3, 0xD) => FORM_SM[subopcode].clone(),
                    (true, 0x3, 0xE) => FORM_I24[subopcode].clone(),
                    (true, 0x3, 0xF) => FORM_SWR[subopcode].clone(),
                    (false, 0x0, _) => FORM_RWI8[b].clone(),
                    (false, 0x1, _) => FORM_WI32[0].clone(),
                    (false, 0x2, _) => FORM_RWI16[b].clone(),
                    (false, 0x3, 0x0) => FORM_MI8[subopcode].clone(),
                    (false, 0x3, 0x1) => FORM_MI16[subopcode].clone(),
                    (false, 0x3, 0x2) => FORM_R1[subopcode].clone(),
                    (false, 0x3, 0x3) => FORM_I16Z[subopcode].clone(),
                    (false, 0x3, 0x4) => FORM_I8Z[subopcode].clone(),
                    (false, 0x3, 0x5) => FORM_I16S[subopcode].clone(),
                    (false, 0x3, 0x6) => FORM_RRI1[subopcode].clone(),
                    (false, 0x3, 0x7) => FORM_RRI2[subopcode].clone(),
                    (false, 0x3, 0x8) => FORM_N[subopcode].clone(),
                    (false, 0x3, 0x9) => FORM_R2[subopcode].clone(),
                    (false, 0x3, 0xA) => FORM_RR[subopcode].clone(),
                    (false, 0x3, 0xB) => FORM_RI[subopcode].clone(),
                    (false, 0x3, 0xC) => FORM_W[subopcode].clone(),
                    (false, 0x3, 0xD) => FORM_RM[subopcode].clone(),
                    (false, 0x3, 0xE) => FORM_RW[subopcode].clone(),
                    (false, 0x3, 0xF) => FORM_RRW[subopcode].clone(),
                    _ => unreachable!(),
                }
            }
        }
    })
}

#[derive(Debug)]
struct Attributes {
    opcode: u8,
    subopcode: u8,
    operands: Vec<TokenStream>,
}

impl TryFrom<&syn::Attribute> for Attributes {
    type Error = Error;

    fn try_from(attr: &syn::Attribute) -> Result<Self> {
        let properties = parser::flatten_attribute_meta(attr)?;
        if properties.len() < 3 {
            return Err(Error::new(
                attr.path.segments[0].ident.span(),
                format!(
                    "\"{}\" is expecting at least 3 properties: opcode, subopcode, operands",
                    ATTR
                ),
            ));
        }

        Ok(Attributes {
            opcode: parser::parse_int_meta("opcode", &properties[0])?,
            subopcode: parser::parse_int_meta("subopcode", &properties[1])?,
            operands: parser::parse_list_meta("operands", &properties[2])?,
        })
    }
}
