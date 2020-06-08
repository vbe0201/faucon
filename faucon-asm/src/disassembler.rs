use std::fs::File;
use std::io::{Cursor, Read, Result};
use std::path::Path;

use crate::instruction::InstructionKind;
use crate::opcode::*;
use crate::operand::*;
use crate::Instruction;

pub fn read_instruction<R: Read>(reader: &mut R) -> Result<Instruction> {
    let mut insn = Vec::new();

    // First, read the opcode of an instruction and construct the subopcode's location.
    reader.take(1).read_to_end(&mut insn)?;
    let subopcode_location =
        get_subopcode_location(insn[0]).expect("Unrecognized opcode encountered");

    // Read enough bytes to extract the subopcode into the buffer.
    reader
        .take(subopcode_location.value() as u64)
        .read_to_end(&mut insn)?;

    // Extract the subopcode.
    let subopcode = parse_subopcode(&insn, subopcode_location);

    // Construct the instruction in question through (opcode, subopcode) identifier.
    let instruction_kind = InstructionKind::from((insn[0], subopcode));

    // Read enough bytes to cover all operands supported by the instruction.
    let mut instruction = Instruction::new(instruction_kind).expect("Invalid instruction hit.");

    let operand = *instruction
        .operands()
        .iter()
        .max_by_key(|o| o.size())
        .unwrap();
    if operand.location() > insn.len() {
        reader
            .take((operand.location() - insn.len() + operand.size()) as u64)
            .read_to_end(&mut insn)?;
    }

    instruction.feed(&insn);

    Ok(instruction)
}
