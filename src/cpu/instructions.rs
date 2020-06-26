use faucon_asm::instruction::InstructionKind;
use faucon_asm::operand::OperandSize;
use faucon_asm::{Instruction, Operand};

use crate::cpu::{Cpu, CpuFlag};

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

/// Checks if the sign bit of an instruction result is set.
///
/// This is necessary to determine whether the sign flag
/// should be set.
fn set_negative(x: u32, insn: &Instruction) -> bool {
    let sz: u32 = insn.operand_size().into();

    (x >> (sz - 1) & 1) != 0
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

    // Extract the operands required to perform the operation.
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

    // Compute the result of the operation and store it.
    let result = cpu.registers.get_gpr(source1.value) ^ source2;
    cpu.registers.set_gpr(destination.value, result);

    // Set the CPU flags accordingly.
    let destination_value = cpu.registers.get_gpr(destination.value);

    cpu.registers.set_flag(CpuFlag::CARRY, false);
    cpu.registers.set_flag(CpuFlag::OVERFLOW, false);
    cpu.registers
        .set_flag(CpuFlag::NEGATIVE, set_negative(destination_value, insn));
    cpu.registers
        .set_flag(CpuFlag::ZERO, destination_value == 0);

    1
}
