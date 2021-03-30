use std::mem::size_of;

use num_traits::{cast, FromPrimitive, PrimInt, WrappingSub};

use crate::arguments::{Argument, MachineEncoding, Positional};
use crate::assembler::context::{Context, Directive, Section};
use crate::assembler::error::ParseError;
use crate::assembler::lexer::Token;
use crate::bytes_ext::SaturatingCast;
use crate::isa::InstructionMeta;
use crate::opcode::{build_opcode_form, OperandSize};
use crate::operands::{MemoryAccess, Register};

#[derive(Debug)]
struct Relocation<'a> {
    pub address: u32,
    pub position: u32,
    patched: bool,
    pub symbol: &'a str,
    pub argument: Argument,
}

impl<'a> Relocation<'a> {
    pub fn new(address: u32, symbol: &'a str, arg: &Argument) -> Self {
        Relocation {
            address,
            position: address,
            patched: false,
            symbol,
            argument: arg.clone(),
        }
    }

    pub fn patch(&mut self, address_base: u32, position_base: u32) {
        if !self.patched {
            self.address += address_base;
            self.position += position_base;
            self.patched = true;
        }
    }
}

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

#[inline]
fn resize_extend(vec: &mut Vec<u8>, new_len: usize) {
    if vec.len() < new_len {
        vec.resize(new_len, 0);
    }
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
    if let Some(span) = section.peek_code_token() {
        // If the token is a symbol, it will be resolved in the second pass.
        // Just make sure that a `u32` would fit as the operand in that case.
        let mut token = span.token();
        if let Token::Symbol(_) = token {
            token = &Token::UnsignedInt(0);
        }

        match arg {
            Argument::SizeConverter(c) => {
                let real_arg = c(size.value());
                section.peek_index -= 1;
                matches_operand(section, size, &real_arg)
            }

            Argument::PcRel8(_) => todo!(),
            Argument::PcRel16(_) => todo!(),

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
    } else {
        false
    }
}

#[inline]
fn unwrap_immediate<T: FromPrimitive + PrimInt + SaturatingCast<u8> + WrappingSub>(
    token: Token,
) -> T {
    match token {
        Token::Flag(imm) => cast(imm).unwrap(),
        Token::SignedInt(imm) => cast(imm).unwrap(),
        Token::UnsignedInt(imm) => cast(imm).unwrap(),
        _ => unreachable!(),
    }
}

#[inline]
fn unwrap_register(token: Token) -> Register {
    match token {
        Token::Register(reg) => reg,
        _ => unreachable!(),
    }
}

#[inline]
fn unwrap_memory_access(token: Token) -> MemoryAccess {
    match token {
        Token::Memory(mem) => mem,
        _ => unreachable!(),
    }
}

fn select_instruction_form<'a>(
    section: &mut Section,
    forms: &'a Vec<InstructionMeta>,
    size: &OperandSize,
) -> Option<&'a InstructionMeta> {
    forms.iter().find(|m| {
        // We need to match the size of the form against the size written in assembly first.
        // For `Argument::SizeConverter` variants that evaluate to an argument to match an
        // operand against, this is important to not trigger the unreachable!() branch by
        // passing an unexpected value. Although this wouldn't cause any damage, it directly
        // causes the program to abort and bypasses the detailed error reporting mechanisms.
        if matches_size(m, size) {
            section.peek_index = 0;
            m.operands
                .iter()
                .filter_map(|o| o.as_ref())
                .fold(true, |acc, a| acc && matches_operand(section, size, a))
        } else {
            false
        }
    })
}

fn lower_operand_impl(buffer: &mut [u8], _pc: u32, token: Token, arg: &Argument) {
    match arg {
        Argument::PcRel8(_) => todo!(),
        Argument::PcRel16(_) => todo!(),

        Argument::U8(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::I8(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::U16(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::I16(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::U24(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::I24(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::U32(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::I32(imm) => imm.write(buffer, unwrap_immediate(token)),

        Argument::Register(reg) => reg.write(buffer, unwrap_register(token)),
        Argument::Flag(imm) => imm.write(buffer, unwrap_immediate(token)),

        Argument::Memory(mem) => mem.write(buffer, unwrap_memory_access(token)),

        _ => unreachable!(),
    }
}

fn lower_operand<'a>(
    output: &mut Vec<u8>,
    pc: u32,
    symbols: &mut Vec<Relocation<'a>>,
    section: &mut Section<'a>,
    size: &OperandSize,
    arg: &Argument,
) {
    // If necessary, evaluate the real argument value and call the function again.
    if let Argument::SizeConverter(c) = arg {
        let real_arg = c(size.value());
        return lower_operand(output, pc, symbols, section, size, &real_arg);
    }

    // Resize the output buffer to fit the next operand.
    resize_extend(output, pc as usize + arg.position() + arg.width());

    // Lower the value of the operand into machine code and write it to the buffer.
    let buffer = &mut output[pc as usize..];
    let token = {
        let t = section.get_code_token().unwrap().unwrap();

        // If the token is a symbol, add it to the symbol cache for evaluation in
        // the second pass and proceed with an unsigned immediate value of zero.
        if let Token::Symbol(s) = t {
            symbols.push(Relocation::new(pc, s, arg));
            Token::UnsignedInt(0)
        } else {
            t
        }
    };
    lower_operand_impl(buffer, pc, token, arg)
}

fn lower_directive<'a>(output: &mut Vec<u8>, section: &mut Section<'a>, directive: Directive<'a>) {
    match directive {
        Directive::Align(align) => {
            let new_counter = align_up(section.counter, align);
            skip(output, new_counter - section.counter, 0);
            section.counter = new_counter;
        }
        Directive::Byte(b) => {
            write_byte(output, b);
            section.counter += size_of::<u8>() as u32;
        }
        Directive::Halfword(hw) => {
            write_halfword(output, hw);
            section.counter += size_of::<u16>() as u32;
        }
        Directive::Word(w) => {
            write_word(output, w);
            section.counter += size_of::<u32>() as u32;
        }
        Directive::Skip(size, value) => {
            skip(output, size, value.unwrap_or(0));
            section.counter += size;
        }
        Directive::Str(s) => {
            write_str(output, s);
            section.counter += s.len() as u32;
        }
        // All other directives actively manipulate the assembler context and
        // have been processed and filtered out before this code executes.
        _ => unreachable!(),
    }
}

fn lower_instruction<'a>(
    output: &mut Vec<u8>,
    section: &mut Section<'a>,
    form: &InstructionMeta,
    size: &OperandSize,
    symbols: &mut Vec<Relocation<'a>>,
) {
    let pc = section.counter;

    // Construct and write the instruction opcode.
    output.push(size.value() << 6 | build_opcode_form(form.a, form.b));

    // Write the instruction subopcode at its position.
    let subopcode_position = pc as usize + form.subopcode_location.position() as usize;
    resize_extend(output, subopcode_position + 1);
    output[subopcode_position] = (output[subopcode_position] & !form.subopcode_location.mask())
        | form.subopcode_location.build_value(form.subopcode);

    // Lower the value of the operand into machine code.
    for operand in form.operands.iter().filter_map(|o| o.as_ref()) {
        lower_operand(output, section.base + pc, symbols, section, size, operand);
    }

    // Increment the program counter to point to the next instruction.
    section.counter += output[pc as usize..].len() as u32;
}

fn first_pass_assemble_section<'a>(
    context: &mut Context<'a>,
    mut section: Section<'a>,
    symbols: &mut Vec<Relocation<'a>>,
) -> Result<Vec<u8>, ParseError> {
    let mut output = Vec::new();

    while let Some(span) = section.get_code_token() {
        match span.token() {
            Token::Directive(_) => {
                let dir = context.get_directive();
                lower_directive(&mut output, &mut section, dir);
            }
            Token::Label(label) => {
                context
                    .set_label_address(label, section.base + section.counter)
                    .unwrap();
            }
            Token::Mnemonic((kind, size)) => {
                let instruction_forms = kind.get_forms();
                let form = select_instruction_form(&mut section, &instruction_forms, size).unwrap();
                lower_instruction(&mut output, &mut section, form, size, symbols);
            }
            _ => unreachable!(),
        }
    }

    Ok(output)
}

pub fn build_context<'a>(mut context: Context<'a>) -> Result<Vec<u8>, ParseError> {
    // Pre-allocate one microcode page in the output buffer to reduce allocations.
    let mut output = Vec::with_capacity(0x100);
    let mut symbol_table = Vec::new();

    // First pass: Assemble all sections into machine code and store information
    // to evaluate symbols into immediate values in a symbol table.
    while let Some(section) = context.get_section() {
        let address_base = section.base;
        let position_base = output.len() as u32;

        output.extend(first_pass_assemble_section(
            &mut context,
            section,
            &mut symbol_table,
        )?);

        for rel in symbol_table.iter_mut() {
            rel.patch(address_base, position_base);
        }
    }

    // Second pass: Now that most of the code is assembled and the offsets and
    // values of the remaining symbols are known, do the symbol resolution and
    // apply all relocations to the assembled code.
    for rel in symbol_table {
        let buffer = &mut output[rel.position as usize..];
        let symbol = context.find_symbol(rel.symbol).unwrap();

        lower_operand_impl(
            buffer,
            rel.address,
            Token::UnsignedInt(*symbol),
            &rel.argument,
        );
    }

    Ok(output)
}
