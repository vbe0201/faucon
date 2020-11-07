//! Instructions that invoke crypto commands on the Secure Co-Processor.

use faucon_asm::Instruction;

use super::{utils, Cpu};

// TODO: Verify the cycle values.

/// Does nothing.
///
/// NOTE: This is a lie. Multiple NOPs still fill up the SCP command queue, so odds are
/// pretty high that it does something that I am not aware of.
pub fn cnop(_: &mut Cpu, _: &Instruction) -> usize {
    // TODO: What's going on here?
    1
}

/// Moves the contents of a crypto register into another crypto register.
pub fn cmov(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.mov(destination.1, source.1);

    1
}

/// Generates a block of random data into the destination register.
pub fn crnd(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (single register).
    let destination = utils::get_register_value(insn.operands()[0]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.rnd(destination.1);

    1
}

/// XORs the contents of two registers and stores the result in the destination register.
pub fn cxor(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.xor(destination.1, source.1);

    1
}

/// Adds an immediate to the contents of a given destination register.
pub fn cadd(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (a register and an immediate).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_value(cpu, insn.operand_size(), operands[1]);

    // Execute the crypto command on the SCP.
    cpu.scp.add(destination.1, source as u8);

    1
}

/// ANDs the contents of two registers and stores the result in the destination register.
pub fn cand(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.and(destination.1, source.1);

    1
}

/// Reverses the block in a given register and stores the result in a destination register.
pub fn crev(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.rev(destination.1, source.1);

    1
}

/// Performs a Galois field multiplication on the contents of a register and stores the result
/// in a destination register.
pub fn cgfmul(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.gfmul(destination.1, source.1);

    1
}

/// Sets the key register for encryption and decryption operations.
pub fn ckeyreg(cpu: &mut Cpu, insn: &Instruction) -> usize {
    // Extract the instruction operands (single register).
    let source = utils::get_register_value(insn.operands()[0]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.keyreg(source.1);

    1
}

/// Goes through the AES Key Schedule to generate the last Round Key into the destination
/// register using the Cipher Key in the source register.
pub fn ckexp(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.kexp(destination.1, source.1);

    1
}

/// Reverses the AES Key Schedule to generate the Cipher Key into the destination register
/// using the last Round Key in the source register.
pub fn ckrexp(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.krexp(destination.1, source.1);

    1
}

/// Encrypts the contents of a register using AES-128-ECB and stores the ciphertext in the
/// destination register.
pub fn cenc(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.enc(destination.1, source.1);

    1
}

/// Decrypts the contents of a register using AES-128-ECB and stores the plaintext in the
/// destination register.
pub fn cdec(cpu: &mut Cpu, insn: &Instruction) -> usize {
    let operands = insn.operands();

    // Extract the instruction operands (two registers).
    let destination = utils::get_register_value(operands[0]).unwrap();
    let source = utils::get_register_value(operands[1]).unwrap();

    // Execute the crypto command on the SCP.
    cpu.scp.dec(destination.1, source.1);

    1
}
