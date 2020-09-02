//! Instructions related to controlling the state of the processor.

use faucon_asm::Instruction;

use super::{utils, Cpu, ExecutionState};

/// Halts the microcode execution and triggers the EXIT interrupt.
pub fn exit(cpu: &mut Cpu, _: &Instruction) -> usize {
    // Modify the execution state of the processor.
    cpu.state = ExecutionState::Stopped;

    // TODO: Trigger EXIT interrupt.

    1
}

/// Halts the microcode execution until an interrupt is received.
pub fn sleep(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (a flag bit).
    let flag = insn.operands()[0];

    // If the flag bit is set, put the processor into sleeping state.
    let flag = utils::parse_flag(flag).unwrap();
    if cpu.registers.get_flag(flag) {
        cpu.state = ExecutionState::Sleeping;
    }

    // Signal irregular PC increment to the CPU.
    cpu.increment_pc = false;

    1
}

/// Copies a value into another register.
pub fn mov(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (register and register or immediate).
    let destination = operands[0];
    let source = operands[1];

    // Copy the source value to the destination.
    utils::write_reg(cpu, insn.operand_size(), destination, source);

    // Signal regular PC increment to the CPU.
    cpu.increment_pc = true;

    1
}
