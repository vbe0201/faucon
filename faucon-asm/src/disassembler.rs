//! Disassembler for the Falcon ISA.

use std::io::Read;

use crate::arguments::Argument;
use crate::isa::*;
use crate::opcode;
use crate::{Error, Instruction, Result};

mod pretty;
pub use pretty::Disassembler;

/// Reads an instruction from a given [`Read`]er and attempts to parse it into an
/// [`Instruction`] object.
///
/// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
/// [`Instruction`]: ../struct.Instruction.html
pub fn read_instruction<R: Read>(reader: &mut R) -> Result<Instruction> {
    let mut insn = Vec::new();

    // First, read the opcode of the next instruction and parse it.
    read_bytes(&mut insn, reader, 1)?;
    let operand_size = opcode::OperandSize::from(insn[0]);
    let (a, b) = opcode::get_opcode_form(insn[0]);

    // Read the subopcode that is necessary to look up the instruction.
    let subopcode_location = opcode::get_subopcode_location(operand_size.value(), a, b)
        .ok_or(Error::UnknownInstruction(insn[0]))?;
    read_bytes(&mut insn, reader, subopcode_location.get())?;
    let subopcode = subopcode_location.parse(&insn);

    // Now do the actual instruction lookup and read the remaining bytes.
    let mut instruction_meta = lookup_instruction(operand_size.sized(), a, b, subopcode)
        .ok_or(Error::UnknownInstruction(insn[0]))?;
    read_operands(
        &mut insn,
        reader,
        operand_size.value(),
        &mut instruction_meta.operands,
    )?;

    Ok(Instruction::new(insn, operand_size, instruction_meta))
}

fn lookup_instruction(sized: bool, a: u8, b: u8, subopcode: u8) -> Option<InstructionMeta> {
    if sized {
        if a == 3 {
            InstructionKind::parse_sized_form_2(b, subopcode)
        } else {
            InstructionKind::parse_sized_form_1(a, b, subopcode)
        }
    } else {
        if a == 3 {
            InstructionKind::parse_unsized_form_2(b, subopcode)
        } else {
            InstructionKind::parse_unsized_form_1(a, b)
        }
    }
}

fn read_operands<R: Read>(
    buffer: &mut Vec<u8>,
    reader: &mut R,
    operand_size: u8,
    operands: &mut [Argument],
) -> Result<()> {
    for operand in operands.iter_mut() {
        // If the argument is actually a dummy placeholder, we can skip it.
        if operand == &Argument::Nop {
            continue;
        }

        // If the argument is a SizeConverter helper, evaluate it and replace
        // it with a real operand to save us some hassle later on.
        if let Argument::SizeConverter(c) = operand {
            *operand = c(operand_size);
        }

        // Calculate the amount of bytes to read until the operand completely fits
        // into the buffer.
        let mut bytes_to_read = 0;
        let operand_width = operand.position() + operand.width();
        if buffer.len() < operand_width {
            bytes_to_read += (operand_width - buffer.len()) as u64;
        }

        // Read the operand bytes.
        read_bytes(buffer, reader, bytes_to_read)?;
    }

    Ok(())
}

fn read_bytes<R: Read>(buffer: &mut Vec<u8>, reader: &mut R, amount: u64) -> Result<usize> {
    if let Ok(amount_read) = reader.take(amount).read_to_end(buffer) {
        // If no bytes were read at all purposefully, it shouldn't count as an EOF.
        if amount != 0 && amount_read == 0 {
            Err(Error::Eof)
        } else {
            Ok(amount_read)
        }
    } else {
        Err(Error::IoError)
    }
}
