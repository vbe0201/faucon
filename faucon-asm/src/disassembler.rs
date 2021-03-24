//! Disassembler for the Falcon ISA.

use std::io::Read;

use crate::arguments::{Argument, Positional};
use crate::isa::*;
use crate::opcode;
use crate::operands::Operand;
use crate::{FalconError, Instruction, Result};

mod pretty;
pub use pretty::Disassembler;

/// Reads an [`Instruction`] from a given [`std::io::Read`]er holding its binary
/// representation.
pub fn read_instruction<'a, R: Read>(reader: &mut R, pc: &mut u32) -> Result<Instruction> {
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
            .ok_or(FalconError::InvalidOpcode(opcode))?;
        read_bytes(&mut insn, reader, location.get())?;

        location.parse_value(&insn)
    };

    Ok({
        // Look up a matching instruction variant and read out the operands it takes.
        let mut meta = InstructionKind::lookup_meta(operand_size.sized(), a, b, subopcode)
            .ok_or(FalconError::InvalidOpcode(opcode))?;
        let operands = read_operands(
            &mut insn,
            reader,
            operand_size.value(),
            *pc as i32,
            &mut meta.operands,
        )?;

        // Construct the instruction object from metadata and raw bytes.
        let instruction = Instruction::new(meta, operand_size, operands, *pc).with_raw_bytes(insn);

        // Increment the program counter to point to the next instruction.
        *pc += instruction.raw_bytes().unwrap().len() as u32;

        instruction
    })
}

fn read_operands<R: Read>(
    buffer: &mut Vec<u8>,
    reader: &mut R,
    operand_size: u8,
    pc: i32,
    args: &mut [Option<Argument>],
) -> Result<Vec<Operand>> {
    let mut operands = Vec::new();
    for arg in args.iter_mut().filter_map(|o| o.as_mut()) {
        // If the argument is a SizeConverter helper, evaluate it and replace
        // it with a real operand to save us some hassle later on.
        if let Argument::SizeConverter(c) = arg {
            *arg = c(operand_size);
        }

        // Calculate the amount of bytes to read until the operand completely fits
        // into the buffer.
        let mut bytes_to_read = 0;
        let operand_width = arg.position() + arg.width();
        if buffer.len() < operand_width {
            bytes_to_read += (operand_width - buffer.len()) as u64;
        }

        // Read the operand from the underlying input source and parse it.
        read_bytes(buffer, reader, bytes_to_read)?;
        operands.push(Operand::parse(arg, pc, buffer));
    }

    Ok(operands)
}

fn read_bytes<R: Read>(buffer: &mut Vec<u8>, reader: &mut R, amount: u64) -> Result<usize> {
    match reader.take(amount).read_to_end(buffer) {
        Ok(0) if amount != 0 => Err(FalconError::Eof),
        Ok(amount_read) => Ok(amount_read),
        Err(e) => Err(FalconError::IoError(e)),
    }
}
