use std::mem::size_of;

use crate::assembler::context::{Context, Directive, Section};
use crate::assembler::error::ParseError;
use crate::assembler::lexer::Token;

fn write_byte(output: &mut Vec<u8>, b: u8) {
    output.push(b);
}

fn write_halfword(output: &mut Vec<u8>, hw: u16) {
    output.extend(hw.to_le_bytes().iter());
}

fn write_word(output: &mut Vec<u8>, w: u32) {
    output.extend(w.to_le_bytes().iter())
}

fn write_str(output: &mut Vec<u8>, s: &str) {
    output.extend_from_slice(s.as_bytes());
}

fn align_up(address: u32, align: u32) -> u32 {
    assert!(align.is_power_of_two(), "Alignment must be a power of two");
    (address + align - 1) & !(align - 1)
}

fn skip(output: &mut Vec<u8>, size: u32, value: u8) {
    output.extend(vec![value; size as usize]);
}

fn lower_directive<'a>(output: &mut Vec<u8>, pc: &mut u32, directive: Directive<'a>) {
    match directive {
        Directive::Align(align) => {
            let new_pc = align_up(*pc, align);
            skip(output, new_pc - *pc, 0);
            *pc = new_pc;
        }
        Directive::Byte(b) => {
            write_byte(output, b);
            *pc += size_of::<u8>() as u32;
        }
        Directive::Halfword(hw) => {
            write_halfword(output, hw);
            *pc += size_of::<u16>() as u32;
        }
        Directive::Word(w) => {
            write_word(output, w);
            *pc += size_of::<u32>() as u32;
        }
        Directive::Skip(size, value) => {
            skip(output, size, value.unwrap_or(0));
            *pc += size;
        }
        Directive::Str(s) => {
            write_str(output, s);
            *pc += s.len() as u32;
        }
        // All other directives actively manipulate the assembler context and
        // have been processed and filtered out before this code executes.
        _ => unreachable!(),
    }
}

fn first_pass_assemble_section<'a>(
    context: &mut Context<'a>,
    mut section: Section,
) -> Result<Vec<u8>, ParseError> {
    let mut pc = section.base;
    let mut output = Vec::new();

    while let Some(span) = section.get_code_token() {
        match span.token() {
            Token::Directive(_) => {
                let dir = context.get_directive();
                lower_directive(&mut output, &mut pc, dir);
            }
            Token::Expression(_expr) => todo!(),
            Token::Label(_label) => todo!(),
            Token::Mnemonic((_kind, _size)) => todo!(),
            _ => unreachable!(),
        }
    }

    Ok(output)
}

pub fn build_context<'a>(mut context: Context<'a>) -> Result<Vec<u8>, ParseError> {
    let mut output = Vec::new();

    // Assemble all the sections to machine code in a first run.
    // This will be needed to produce valid offsets for inserting branch targets
    // in a second run that can make use of the real memory offsets.
    while let Some(section) = context.get_section() {
        output.extend(first_pass_assemble_section(&mut context, section)?);
    }

    Ok(output)
}
