//! Disassembler for the Falcon ISA.

use std::io::Read;

use crate::arguments::{Argument, Positional};
use crate::isa::*;
use crate::opcode;
use crate::{Error, Instruction, Result};

mod pretty;
pub use pretty::Disassembler;

/// Reads an instruction from a given [`Read`]er holding its binary representation and
/// attempts to parse it into an [`Instruction`] object.
///
/// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
/// [`Instruction`]: ../struct.Instruction.html
pub fn read_instruction<R: Read>(reader: &mut R, offset: &mut usize) -> Result<Instruction> {
    let mut insn = Vec::new();

    // Read the opcode of the next instruction and parse it.
    let opcode = {
        read_bytes(&mut insn, reader, 1)?;
        insn[0]
    };

    // Parse the opcode to obtain the instruction size and the encoding form.
    let (a, b) = opcode::get_opcode_form(opcode);
    let operand_size = opcode::OperandSize::from(opcode);

    // Parse the subopcode value required for instruction lookup.
    let subopcode = {
        let location = opcode::get_subopcode_location(operand_size.value(), a, b)
            .ok_or(Error::UnknownInstruction(opcode))?;
        read_bytes(&mut insn, reader, location.get())?;

        location.parse_value(&insn)
    };

    Ok({
        // Look up a matching instruction variant and read out the operands it takes.
        let mut meta = InstructionKind::lookup_meta(operand_size.sized(), a, b, subopcode)
            .ok_or(Error::UnknownInstruction(opcode))?;
        read_operands(&mut insn, reader, operand_size.value(), &mut meta.operands)?;

        // Increment the offset to point to the next instruction.
        let current_offset = *offset;
        *offset += insn.len();

        Instruction::new(insn, current_offset, operand_size, meta)
    })
}

fn read_operands<R: Read>(
    buffer: &mut Vec<u8>,
    reader: &mut R,
    operand_size: u8,
    operands: &mut [Option<Argument>],
) -> Result<()> {
    for operand in operands.iter_mut().filter_map(|o| o.as_mut()) {
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
