use faucon_asm::instruction::InstructionKind;
use faucon_asm::{Instruction, Operand};

use crate::cpu::Cpu;

/// Macro to expand an operand to a given expression when it is guaranteed that
/// it takes a specific variant.
macro_rules! operand {
    ($operand:expr, $($variant:pat)|* => $result:expr) => {
        if false {
            unreachable!()
        } $(else if let $variant = $operand {
            Some($result)
        })* else {
            None
        }
    };
}

/// Emulates a given CPU instruction.
///
/// Returns the amount of CPU cycles that the instruction took.
pub fn process_instruction(cpu: &mut Cpu, insn: &Instruction) -> usize {
    match insn.kind {
        InstructionKind::XOR(_, _, _) => xor(cpu, insn),
        _ => todo!("Emulate remaining instructions"),
    }
}

/// Executes an XOR instruction.
fn xor(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands().unwrap();

    let destination = operand!(operands[0], Operand::Register(reg) => reg).unwrap();
    let source1 = match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operand!(operands[1], Operand::Register(reg) => reg).unwrap(),
        0xF0 | 0xF1 | 0xFD => destination,
        _ => unreachable!(),
    };
    let source2 = operand!(match insn.opcode() {
        0xC0 | 0xE0 | 0xFF => operands[2],
        0xF0 | 0xF1 | 0xFD => operands[1],
        _ => unreachable!(),
    }, Operand::I8(int) | Operand::I16(int) => int as u32)
    .unwrap();

    let result = cpu.registers.get_gpr(source1.value) ^ source2;
    cpu.registers.set_gpr(destination.value, result);

    1
}
