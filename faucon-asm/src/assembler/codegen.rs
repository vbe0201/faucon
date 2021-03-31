use std::mem::size_of;

use num_traits::{cast, FromPrimitive, PrimInt, WrappingSub};

use crate::arguments::{Argument, MachineEncoding, Positional};
use crate::assembler::context::{Context, Directive, Relocation, Section};
use crate::assembler::error::ParseError;
use crate::assembler::lexer::Token;
use crate::bytes_ext::SaturatingCast;
use crate::isa::InstructionMeta;
use crate::opcode::{build_opcode_form, OperandSize};
use crate::operands::{MemoryAccess, Register};

#[inline]
fn write_byte(output: &mut Vec<u8>, b: u8) {
    output.push(b);
}

#[inline]
fn write_halfword(output: &mut Vec<u8>, hw: u16) {
    output.extend(hw.to_le_bytes().iter());
}

#[inline]
fn write_word(output: &mut Vec<u8>, w: u32) {
    output.extend(w.to_le_bytes().iter());
}

#[inline]
fn write_str(output: &mut Vec<u8>, s: &str) {
    output.extend_from_slice(s.as_bytes());
}

#[inline]
fn skip(output: &mut Vec<u8>, size: u32, value: u8) {
    output.extend(vec![value; size as usize]);
}

fn align_up(address: u32, align: u32) -> u32 {
    assert!(align.is_power_of_two(), "Alignment must be a power of two");
    (address + align - 1) & !(align - 1)
}

fn resize_extend(vec: &mut Vec<u8>, new_len: usize) {
    if vec.len() < new_len {
        vec.resize(new_len, 0);
    }
}

fn matches_size(form: &InstructionMeta, size: &OperandSize) -> bool {
    match (form.sized, size) {
        (_, OperandSize::Unsized) => !form.sized,
        _ => form.sized,
    }
}

fn resolve_symbol<'a>(context: &Context<'a>, symbol: &'a str) -> Option<Token<'a>> {
    context
        .find_symbol(symbol)
        .and_then(|v| Some(Token::UnsignedInt(v)))
}

fn matches_operand_impl(arg: &Argument, t: &Token) -> bool {
    match arg {
        Argument::PcRel8(_) => todo!(),
        Argument::PcRel16(_) => todo!(),

        Argument::U8(imm) => imm.matches(t),
        Argument::I8(imm) => imm.matches(t),
        Argument::U16(imm) => imm.matches(t),
        Argument::I16(imm) => imm.matches(t),
        Argument::U32(imm) => imm.matches(t),
        Argument::I32(imm) => imm.matches(t),

        Argument::Register(reg) => reg.matches(t),
        Argument::Flag(imm) => imm.matches(t),

        Argument::Memory(mem) => mem.matches(t),

        Argument::SizeConverter(_) => unreachable!(),
    }
}

fn matches_operand<'a>(
    context: &Context<'a>,
    section: &mut Section,
    size: &OperandSize,
    arg: &Argument,
) -> Option<bool> {
    // If necessary, evaluate the converter to a real argument and re-call the function.
    if let Argument::SizeConverter(c) = arg {
        let real_arg = c(size.value());
        return matches_operand(context, section, size, &real_arg);
    }

    // Peek at the next code token and see if it matches the currently selected form.
    match section.peek_code_token().and_then(|s| Some(s.token())) {
        Some(Token::Symbol(s)) => {
            // When matching a symbol, we try to resolve its real value first. If this
            // fails e.g. because the instruction is a forward branch to a yet unresolved
            // symbol, we return `None` to signal that this operand cannot be matched yet.
            if let Some(symbol) = resolve_symbol(context, s) {
                Some(matches_operand_impl(&arg, &symbol))
            } else if matches_operand_impl(&arg, &Token::UnsignedInt(0)) {
                // Make sure that the argument describes an immediate operand at all by
                // checking against a value of zero. If the operand resolves to anything
                // else, symbols are not allowed in this context and the form should be
                // considered invalid for the set of tokens we check against.
                None
            } else {
                Some(false)
            }
        }
        Some(token) => Some(matches_operand_impl(&arg, token)),
        None => Some(false),
    }
}

fn select_instruction_form<'a>(
    context: &Context<'a>,
    section: &mut Section,
    forms: &'a Vec<InstructionMeta>,
    size: &OperandSize,
) -> Option<&'a InstructionMeta> {
    let mut candidates = Vec::with_capacity(6);
    let full_match = forms.iter().filter(|m| matches_size(m, size)).find(|&m| {
        section.peek_index = 0;
        m.operands
            .iter()
            .flatten()
            .all(|a| match matches_operand(context, section, size, a) {
                Some(true) => true,
                Some(false) => false,
                None => {
                    candidates.push(m);
                    false
                }
            })
    });

    full_match.or_else(|| {
        candidates
            .into_iter()
            .max_by_key(|m| m.operands.iter().flatten().last().unwrap().width())
    })
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

fn lower_operand_impl(buffer: &mut [u8], _pc: u32, token: Token, arg: &Argument) {
    match arg {
        Argument::PcRel8(_) => todo!(),
        Argument::PcRel16(_) => todo!(),

        Argument::U8(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::I8(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::U16(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::I16(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::U32(imm) => imm.write(buffer, unwrap_immediate(token)),
        Argument::I32(imm) => imm.write(buffer, unwrap_immediate(token)),

        Argument::Register(reg) => reg.write(buffer, unwrap_register(token)),
        Argument::Flag(imm) => imm.write(buffer, unwrap_immediate(token)),

        Argument::Memory(mem) => mem.write(buffer, unwrap_memory_access(token)),

        Argument::SizeConverter(_) => unreachable!(),
    }
}

fn lower_operand<'a>(
    context: &Context<'a>,
    output: &mut Vec<u8>,
    pc: u32,
    relocations: &mut Vec<Relocation<'a>>,
    section: &mut Section<'a>,
    size: &OperandSize,
    arg: &Argument,
) {
    // If necessary, evaluate the converter to a real argument and re-call the function.
    if let Argument::SizeConverter(c) = arg {
        let real_arg = c(size.value());
        return lower_operand(context, output, pc, relocations, section, size, &real_arg);
    }

    // Resize the output buffer to fit the next operand.
    resize_extend(output, pc as usize + arg.position() + arg.width());

    // Lower the value of the operand into machine code and write it to the buffer.
    let buffer = &mut output[pc as usize..];
    let token = {
        // SAFETY: From the previous instruction form selection step, we already peeked
        // at the required tokens and know for a fact that we can safely unwrap here.
        let token = section.get_code_token().unwrap().unwrap();

        // If the token is a symbol, try to evaluate it or create a new relocation to be
        // evaluated and patched at a later time in the assembler pipeline.
        if let Token::Symbol(s) = token {
            if let Some(real_token) = resolve_symbol(context, s) {
                real_token
            } else {
                relocations.push(Relocation::new(pc, s, arg));
                Token::UnsignedInt(0)
            }
        } else {
            token
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
        // All other directives actively manipulate the assembler context and have
        // been processed and filtered out at an earlier step of the pipeline.
        _ => unreachable!(),
    }
}

fn lower_instruction<'a>(
    context: &Context<'a>,
    output: &mut Vec<u8>,
    section: &mut Section<'a>,
    form: &InstructionMeta,
    size: &OperandSize,
    relocations: &mut Vec<Relocation<'a>>,
) {
    let pc = section.counter;

    // Construct and write the instruction opcode.
    write_byte(
        output,
        size.value() << 6 | build_opcode_form(form.a, form.b),
    );

    // Write the instruction subopcode at its expected position.
    let subopcode_position = pc as usize + form.subopcode_location.position() as usize;
    resize_extend(output, subopcode_position + 1);
    output[subopcode_position] = (output[subopcode_position] & !form.subopcode_location.mask())
        | form.subopcode_location.build_value(form.subopcode);

    // Lower the value of the operand into machine code.
    for arg in form.operands.iter().flatten() {
        lower_operand(
            context,
            output,
            section.base + pc,
            relocations,
            section,
            size,
            arg,
        );
    }

    // Increment the internal counter to point to the next instruction.
    section.counter += output[pc as usize..].len() as u32;
}

fn first_pass_assemble_section<'a>(
    context: &mut Context<'a>,
    mut section: Section<'a>,
    relocations: &mut Vec<Relocation<'a>>,
) -> Result<Vec<u8>, ParseError> {
    // Generously allocate one code page for the section to reduce allocations.
    let mut output = Vec::with_capacity(0x100);

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
                let form = select_instruction_form(context, &mut section, &instruction_forms, size)
                    .unwrap();
                lower_instruction(context, &mut output, &mut section, form, size, relocations);
            }
            _ => unreachable!(),
        }
    }

    Ok(output)
}

pub fn build_context<'a>(mut context: Context<'a>) -> Result<Vec<u8>, ParseError> {
    // Pre-allocate three microcode pages in the output buffer to reduce allocations.
    // Also generously allocate space for the amount of relocations.
    let mut output = Vec::with_capacity(0x300);
    let mut relocations = Vec::with_capacity(25);

    // First pass: Assemble all sections into machine code and store relocation
    // information to evaluate label symbols into immediate values later on.
    while let Some(section) = context.get_section() {
        let address_base = section.base;
        let position_base = output.len() as u32;

        output.extend(first_pass_assemble_section(
            &mut context,
            section,
            &mut relocations,
        )?);

        for rel in relocations.iter_mut() {
            rel.patch(address_base, position_base);
        }
    }

    // Second pass: Now that all of the code is assembled and the memory offsets of
    // each label is known, we can do the remaining symbol resolution and patch all
    // code offsets where a value of `0` has been inserted as a placeholder.
    for rel in relocations {
        let buffer = &mut output[rel.position as usize..];
        let symbol = resolve_symbol(&context, rel.symbol).unwrap();

        lower_operand_impl(buffer, rel.address, symbol, &rel.argument);
    }

    Ok(output)
}
