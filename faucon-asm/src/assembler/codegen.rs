use std::mem::size_of;

use crate::arguments::{Argument, MachineEncoding};
use crate::assembler::context::{Context, Directive, Section};
use crate::assembler::error::ParseError;
use crate::assembler::lexer::Token;
use crate::isa::InstructionMeta;
use crate::opcode::{build_opcode_form, OperandSize};

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

fn matches_size(form: &InstructionMeta, size: &OperandSize) -> bool {
    match (form.sized, size) {
        (false, OperandSize::Unsized) => true,
        (false, _) => false,
        (true, OperandSize::Unsized) => false,
        (true, _) => true,
    }
}

fn matches_operand(section: &mut Section, size: &OperandSize, arg: &Argument) -> bool {
    // TODO: Handle branches correctly.
    let token = section.peek_code_token().unwrap().token();
    match arg {
        Argument::SizeConverter(c) => {
            let real_arg = c(size.value());
            matches_operand(section, size, &real_arg)
        }

        Argument::PcRel8(imm) => imm.matches(token),
        Argument::PcRel16(imm) => imm.matches(token),

        Argument::U8(imm) => imm.matches(token),
        Argument::I8(imm) => imm.matches(token),
        Argument::U16(imm) => imm.matches(token),
        Argument::I16(imm) => imm.matches(token),
        Argument::U24(imm) => imm.matches(token),
        Argument::I24(imm) => imm.matches(token),
        Argument::U32(imm) => imm.matches(token),
        Argument::I32(imm) => imm.matches(token),

        Argument::Register(reg) => reg.matches(token),
        Argument::Flag(imm) => imm.matches(token),

        Argument::Memory(mem) => mem.matches(token),
    }
}

fn select_instruction_form<'a>(
    forms: &'a Vec<InstructionMeta>,
    size: &OperandSize,
    section: &mut Section,
) -> Option<&'a InstructionMeta> {
    forms.iter().find(|m| {
        // We need to match the size of the form against the size written in assembly first.
        // For `Argument::SizeConverter` variants that evaluate to an argument to match an
        // operand against, this is important to not trigger the unreachable!() branch by
        // passing an unexpected value. Although this wouldn't cause any damage, it directly
        // causes the program to abort and bypasses the detailed error reporting mechanisms.
        if matches_size(m, size) {
            section.reset_peek_index();
            m.operands
                .iter()
                .filter_map(|o| o.as_ref())
                .fold(true, |acc, a| acc && matches_operand(section, size, a))
        } else {
            false
        }
    })
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

fn lower_instruction(
    output: &mut Vec<u8>,
    pc: &mut u32,
    form: &InstructionMeta,
    size: &OperandSize,
    _section: &mut Section,
) {
    // Construct and write the instruction opcode.
    output.push(size.value() << 6 | build_opcode_form(form.a, form.b));

    // Write the instruction subopcode into the location.
    let subopcode_position = *pc as usize + form.subopcode_location.get() as usize;
    output.resize(subopcode_position + 1, 0);
    output[subopcode_position] |= form.subopcode_location.build_value(form.subopcode);

    // TODO: Write operands.

    // Increment the program counter to point to the next instruction.
    *pc += output[*pc as usize..].len() as u32;
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
            Token::Mnemonic((kind, size)) => {
                let instruction_forms = kind.get_forms();
                let form = select_instruction_form(&instruction_forms, size, &mut section).unwrap();
                lower_instruction(&mut output, &mut pc, form, size, &mut section);
            }
            _ => unreachable!(),
        }
    }

    Ok(output)
}

pub fn build_context<'a>(mut context: Context<'a>) -> Result<Vec<u8>, ParseError> {
    // Pre-allocate one microcode page in the output buffer to reduce allocations.
    let mut output = Vec::with_capacity(0x100);

    // Assemble all the sections to machine code in a first run.
    // This will be needed to produce valid offsets for inserting branch targets
    // in a second run that can make use of the real memory offsets.
    while let Some(section) = context.get_section() {
        output.extend(first_pass_assemble_section(&mut context, section)?);
    }

    Ok(output)
}
