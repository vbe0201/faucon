//! Disassembler implementation for the Falcon ISA.

use std::io::Read;

use crate::instruction::InstructionKind;
use crate::opcode::*;
use crate::operand::is_unsized;
use crate::{Error, Instruction, Result};

/// Reads an instruction from a given [`Read`]er and attempts to parse it into
/// an [`Instruction`] object.
///
/// The reader is supposed to provide enough bytes to construct an instruction
/// and it is advised to call this function with a [`Cursor`] that tracks the
/// position in the bytes provider in a loop to read all the instructions, e.g.
/// from a input binary.
///
/// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
/// [`Instruction`]: ../struct.Instruction.html
/// [`Cursor`]: https://doc.rust-lang.org/std/io/struct.Cursor.html
pub fn read_instruction<R: Read>(reader: &mut R) -> Result<Instruction> {
    let mut insn = Vec::new();

    // First, read the opcode of an instruction and mask it appropriately.
    read_bytes(&mut insn, reader, 1)?;
    let mut opcode = insn[0];
    if is_unsized(opcode) {
        opcode &= 0xFF;
    } else {
        opcode &= !SIZE_MASK;
    }

    // Read and extract the subopcode.
    let subopcode_location =
        get_subopcode_location(opcode).ok_or(Error::InvalidInstruction(opcode))?;
    read_bytes(&mut insn, reader, subopcode_location.value() as u64)?;
    let subopcode = parse_subopcode(&insn, subopcode_location);

    // Given the extracted information, construct the instruction in question.
    let insn_kind = InstructionKind::from((opcode, subopcode));
    let mut instruction = Instruction::new(insn_kind).ok_or(Error::InvalidInstruction(insn[0]))?;
    instruction.feed(&insn);

    // Read and extract all the operands that belong to the instruction.
    read_operands(&mut instruction, reader)?;

    Ok(instruction)
}

fn read_operands<R: Read>(insn: &mut Instruction, reader: &mut R) -> Result<()> {
    // Check if the instruction even has operands. If not, we can safely opt out.
    let operands = if let Some(o) = insn.kind.operands() {
        o
    } else {
        return Ok(());
    };

    // If there are operands, read them into the instruction buffer.
    for operand in operands.iter() {
        let operand_start = operand.location() as u64;
        let operand_length = operand.size() as u64;

        // First, check if the operand is already fully available.
        // In such a case, we can opt out immediately.
        let operand_bytes = insn.bytes[operand_start as usize..].len() as u64;
        if operand_bytes >= operand_length {
            continue;
        }

        // If that's not the case, continue reading the missing chunk.
        let mut remainder = Vec::new();
        read_bytes(&mut remainder, reader, operand_length - operand_bytes)?;

        // Under certain circumstances, the buffer may have not been filled to the
        // actual start of the operand, so this needs to be taken care of as well.
        let actual_length = insn.bytes.len() as u64 + remainder.len() as u64;
        let required_length = operand_start + operand_length;
        if actual_length < required_length {
            read_bytes(&mut remainder, reader, required_length - actual_length)?;
        }

        // Lastly, feed the bytes to the instruction.
        insn.feed(&remainder);
    }

    Ok(())
}

fn read_bytes<R: Read>(buffer: &mut Vec<u8>, reader: &mut R, amount: u64) -> Result<usize> {
    if let Ok(amount_read) = reader.take(amount).read_to_end(buffer) {
        if amount != 0 && amount_read == 0 {
            Err(Error::Eof)
        } else {
            Ok(amount_read)
        }
    } else {
        Err(Error::IoError)
    }
}
