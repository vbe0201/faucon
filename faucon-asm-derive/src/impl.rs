use std::convert::TryFrom;

use proc_macro2::{Span, TokenStream};
use syn::{parse::Error, Ident, Result};

use crate::parser;

// The name of the sole attribute supported by the `Instruction` macro.
pub const ATTR: &str = "insn";

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

    let mut get_forms_match_arms = Vec::new();

    // Given an opcode and a subopcode, this closure determines the appropriate opcode
    // table from the above vectors and inserts an InstructionMeta table at the index
    // of the subopcode to enhance instruction lookup speed through array indexing.
    let mut fill_table = |vname: &Ident,
                          opcode: u8,
                          subopcode: u8,
                          operands: &mut Vec<TokenStream>,
                          forms: &mut Vec<TokenStream>| {
        let (size, a, b) = (
            (opcode >> 6) as usize,
            (opcode >> 4 & 0x3) as usize,
            (opcode & 0xF) as usize,
        );
        let subopcode = subopcode as usize;

        // faucon-asm stores the operands of each instruction in `[Argument; 3]` arrays.
        // Since instructions have varying numbers of operands from 0 to 3, we store every
        // operand in `Option::Some` and ignore those that are `Option::None`.
        while operands.len() < 3 {
            operands.push(quote! { ::std::option::Option::None });
        }

        let instruction_meta = {
            let meta = quote! {
                crate::isa::InstructionMeta::new(
                    #name::#vname,
                    #opcode,
                    #subopcode as ::std::primitive::u8,
                    [#(#operands),*],
                )
            };
            forms.push(meta.clone());

            quote! { Some(#meta) }
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
        let mut get_forms_vec = Vec::new();

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
            fill_table(vname, *opcode, *subopcode, operands, &mut get_forms_vec);
        }

        get_forms_match_arms.push(quote! { #name::#vname => ::std::vec![#(#get_forms_vec),*] });
    }

    Ok(quote! {
        static FORM_WI: [::std::option::Option<crate::isa::InstructionMeta>; 0x3] = [
            #(#wi),*
        ];

        static FORM_SRWI8: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#srwi8),*
        ];

        static FORM_SRWI16: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#srwi16),*
        ];

        static FORM_SRI8: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#sri8),*
        ];

        static FORM_SRI16: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#sri16),*
        ];

        static FORM_SRR: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#srr),*
        ];

        static FORM_SUNK: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#sunk),*
        ];

        static FORM_SWI8: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#swi8),*
        ];

        static FORM_SRRI8: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#srri8),*
        ];

        static FORM_SMI8: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#smi8),*
        ];

        static FORM_SMI16: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#smi16),*
        ];

        static FORM_SRRI16: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#srri16),*
        ];

        static FORM_SRW: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#srw),*
        ];

        static FORM_SWRR: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#swrr),*
        ];

        static FORM_SMR: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#smr),*
        ];

        static FORM_SRRW: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#srrw),*
        ];

        static FORM_SM: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#sm),*
        ];

        static FORM_I24: [::std::option::Option<crate::isa::InstructionMeta>; 0x3] = [
            #(#i24),*
        ];

        static FORM_SWR: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#swr),*
        ];

        static FORM_RWI8: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rwi8),*
        ];

        static FORM_WI32: [::std::option::Option<crate::isa::InstructionMeta>; 0x1] = [
            #(#wi32),*
        ];

        static FORM_RWI16: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rwi16),*
        ];

        static FORM_MI8: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#mi8),*
        ];

        static FORM_MI16: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#mi16),*
        ];

        static FORM_R1: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#r1),*
        ];

        static FORM_I16Z: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#i16z),*
        ];

        static FORM_I8Z: [::std::option::Option<crate::isa::InstructionMeta>; 0x40] = [
            #(#i8z),*
        ];

        static FORM_I16S: [::std::option::Option<crate::isa::InstructionMeta>; 0x40] = [
            #(#i16s),*
        ];

        static FORM_RRI1: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rri1),*
        ];

        static FORM_RRI2: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rri2),*
        ];

        static FORM_N: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#n),*
        ];

        static FORM_R2: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#r2),*
        ];

        static FORM_RR: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rr),*
        ];

        static FORM_RI: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#ri),*
        ];

        static FORM_W: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#w),*
        ];

        static FORM_RM: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rm),*
        ];

        static FORM_RW: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rw),*
        ];

        static FORM_RRW: [::std::option::Option<crate::isa::InstructionMeta>; 0x10] = [
            #(#rrw),*
        ];

        impl #name {
            pub(crate) fn lookup_meta(
                sized: std::primitive::bool,
                a: ::std::primitive::u8,
                b: ::std::primitive::u8,
                subopcode: ::std::primitive::u8
            ) -> ::std::option::Option<crate::isa::InstructionMeta> {
                let b = b as std::primitive::usize;
                let subopcode = subopcode as ::std::primitive::usize;

                (match (sized, a, b) {
                    (true, 0x0, _) => &FORM_WI[subopcode],
                    (true, 0x1, _) => &FORM_SRWI8[b],
                    (true, 0x2, _) => &FORM_SRWI16[b],
                    (true, 0x3, 0x0) => &FORM_SRI8[subopcode],
                    (true, 0x3, 0x1) => &FORM_SRI16[subopcode],
                    (true, 0x3, 0x2) => &FORM_SRR[subopcode],
                    (true, 0x3, 0x3) => &FORM_SUNK[subopcode],
                    (true, 0x3, 0x4) => &FORM_SWI8[subopcode],
                    (true, 0x3, 0x5) => &FORM_SRRI8[subopcode],
                    (true, 0x3, 0x6) => &FORM_SMI8[subopcode],
                    (true, 0x3, 0x7) => &FORM_SMI16[subopcode],
                    (true, 0x3, 0x8) => &FORM_SRRI16[subopcode],
                    (true, 0x3, 0x9) => &FORM_SRW[subopcode],
                    (true, 0x3, 0xA) => &FORM_SWRR[subopcode],
                    (true, 0x3, 0xB) => &FORM_SMR[subopcode],
                    (true, 0x3, 0xC) => &FORM_SRRW[subopcode],
                    (true, 0x3, 0xD) => &FORM_SM[subopcode],
                    (true, 0x3, 0xE) => &FORM_I24[subopcode],
                    (true, 0x3, 0xF) => &FORM_SWR[subopcode],
                    (false, 0x0, _) => &FORM_RWI8[b],
                    (false, 0x1, _) => &FORM_WI32[0],
                    (false, 0x2, _) => &FORM_RWI16[b],
                    (false, 0x3, 0x0) => &FORM_MI8[subopcode],
                    (false, 0x3, 0x1) => &FORM_MI16[subopcode],
                    (false, 0x3, 0x2) => &FORM_R1[subopcode],
                    (false, 0x3, 0x3) => &FORM_I16Z[subopcode],
                    (false, 0x3, 0x4) => &FORM_I8Z[subopcode],
                    (false, 0x3, 0x5) => &FORM_I16S[subopcode],
                    (false, 0x3, 0x6) => &FORM_RRI1[subopcode],
                    (false, 0x3, 0x7) => &FORM_RRI2[subopcode],
                    (false, 0x3, 0x8) => &FORM_N[subopcode],
                    (false, 0x3, 0x9) => &FORM_R2[subopcode],
                    (false, 0x3, 0xA) => &FORM_RR[subopcode],
                    (false, 0x3, 0xB) => &FORM_RI[subopcode],
                    (false, 0x3, 0xC) => &FORM_W[subopcode],
                    (false, 0x3, 0xD) => &FORM_RM[subopcode],
                    (false, 0x3, 0xE) => &FORM_RW[subopcode],
                    (false, 0x3, 0xF) => &FORM_RRW[subopcode],
                    _ => unreachable!(),
                }).clone()
            }

            pub(crate) fn get_forms(&self) -> ::std::vec::Vec<crate::isa::InstructionMeta> {
                match self {
                    #(#get_forms_match_arms),*
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
