//! Instructions related to Falcon virtual memory in the code segment.

use faucon_asm::Instruction;

use super::Cpu;

/// Reads the TLB corresponding to a given physical address.
pub fn ptlb(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = operands[0];
    let source = operands[1];

    // Look up the TLB.
    let physical_index = ((cpu.registers[source] & 0xFF) << 8) as u16;
    let tlb = cpu.memory.tlb.get_physical_entry(physical_index);

    // Build and write the result value.
    cpu.registers[destination] = (tlb.flags as u32) << 24 | (tlb.virtual_page_number as u32) << 8;

    1
}

/// Reads the TLB corresponding to a given virtual address.
pub fn vtlb(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = operands[0];
    let source = operands[1];

    // Look up the TLB to get the result value and write it to the destination.
    let result = cpu.memory.tlb.lookup_raw(cpu.registers[source] & 0xFFFFFF);
    cpu.registers[destination] = result;

    1
}

/// Invalidates a TLB entry corresponding to a physical address.
pub fn itlb(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operand (one register).
    let source = operands[0];

    // Look up the TLB.
    let physical_index = ((cpu.registers[source] & 0xFF) << 8) as u16;
    let tlb = cpu.memory.tlb.get_physical_entry(physical_index);

    // Clear the entry.
    tlb.clear();

    1
}
