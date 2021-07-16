//! Disassembler for the Falcon ISA.

use std::io::Read;

use crate::bitfields::Position;
use crate::isa::*;
use crate::opcode::{self, OperandSize};
use crate::operands::Operand;
use crate::{FalconError, Instruction};

mod pretty;
pub use pretty::Disassembler;

/// Disassembles an [`Instruction`] from a given [`std::io::Read`]er.
pub fn read_instruction<R: Read>(reader: &mut R, pc: &mut u32) -> Result<Instruction, FalconError> {
    let mut insn = Vec::new();

    // Read the opcode of the next instruction and parse it.
    let opcode = {
        read_bytes(&mut insn, reader, 1)?;
        insn[0]
    };
    let (a, b) = opcode::get_opcode_form(opcode);
    let operand_size = OperandSize::from(opcode);

    // Parse the subopcode value for instruction lookup.
    let subopcode = {
        let location = opcode::get_subopcode_location(operand_size.value(), a, b)
            .ok_or(FalconError::InvalidOpcode(opcode))?;
        let bf = location.field();

        read_bytes(&mut insn, reader, bf.position().0 as u64)?;
        bf.read(&insn)
    };

    Ok({
        // Look up the opcode in our generated lookup tables.
        let mut meta = InstructionKind::lookup_meta(operand_size.sized(), a, b, subopcode)
            .ok_or(FalconError::InvalidOpcode(opcode))?;

        // Read and decode all instruction operands.
        let operands = read_operands(&mut insn, reader, &mut meta, operand_size.value(), *pc)?;

        // Construct a new instruction object from the metadata and the raw bytes.
        let instruction = Instruction::new(meta, operand_size, operands, *pc, insn);

        // Increment the program counter to point to the next instruction.
        *pc += instruction.raw_bytes().len() as u32;

        instruction
    })
}

fn read_operands<R: Read>(
    buf: &mut Vec<u8>,
    reader: &mut R,
    meta: &mut InstructionMeta,
    operand_size: u8,
    pc: u32,
) -> Result<Vec<Operand>, FalconError> {
    let mut result = Vec::new();

    for dispatch in meta.operands.iter_mut().flatten() {
        // If the argument is dynamic, evaluate it based on the operand size.
        let encoding = dispatch.evaluate(operand_size);

        // Calculate the amount of bytes to read until we have the full operand
        // inside `buf`.
        let mut bytes_to_read = 0;
        let operand_width = {
            let (start, nbytes) = encoding.position();
            start + nbytes
        };
        if buf.len() < operand_width {
            bytes_to_read += (operand_width - buf.len()) as u64;
        }

        // Read the operand bytes and decode them into an operand.
        read_bytes(buf, reader, bytes_to_read)?;
        result.push(encoding.read(pc, buf));
    }

    Ok(result)
}

fn read_bytes<R: Read>(
    buf: &mut Vec<u8>,
    reader: &mut R,
    amount: u64,
) -> Result<usize, FalconError> {
    match reader.take(amount).read_to_end(buf) {
        Ok(0) if amount != 0 => Err(FalconError::Eof),
        Ok(nbytes) => Ok(nbytes),
        Err(e) => Err(FalconError::IoError(e)),
    }
}
