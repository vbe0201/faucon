//! Disassembler for the Falcon v5 (fuc5) ISA.

use std::fs::File;
use std::io::Read;
use std::path::Path;

/// Denotes the operand size of an instruction.
///
/// The value is determined by the highest two bits of
/// the first byte of each instruction.
#[derive(Debug)]
pub enum OperandSize {
    /// An operand size of 8-bit.
    EightBit,
    /// An operand size of 16-bit.
    SixteenBit,
    /// An operand size of 32-bit.
    ThirtyTwoBit,
    /// An unknown/variable operand size.
    Unsized,
}

impl From<u8> for OperandSize {
    fn from(first_byte: u8) -> Self {
        // Check the highest two bits.
        match first_byte >> 6 {
            0b00 => OperandSize::EightBit,
            0b01 => OperandSize::SixteenBit,
            0b10 => OperandSize::ThirtyTwoBit,
            0b11 => OperandSize::Unsized,
            _ => panic!(),
        }
    }
}

/// Possible locations in where subopcodes are stored.
///
/// These vary from instruction to instruction.
pub enum SubopcodeLocation {
    /// Subopcode is stored in the low 4 bits of byte 0.
    O1,
    /// Subopcode is stored in the low 4 bits of byte 1.
    O2,
    /// Subopcode is stored in the low 6 bits of byte 1.
    OL,
    /// Subopcode is stored in the low 4 bits of byte 2.
    O3,
}

/// Possible locations in where a register is stored.
pub enum RegisterLocation {
    /// Register is encoded in low 4 bits of byte 1.
    Low1,
    /// Register is encoded in high 4 bits of byte 1.
    High1,
    /// Register is encoded in high 4 bits of byte 2.
    High2,
}

/// Possible directions in which a register is used.
pub enum RegisterDirection {
    /// The register is encoded as a source register.
    Source,
    /// The register is encoded as a destination register.
    Destination,
    /// The register is encoded as both, source and destination.
    SourceDestination,
}

/// Represents a CPU register as it is encoded within an instruction.
///
/// Given are the information on where a register is exactly stored and
/// how it is exactly utilised.
pub struct Register(RegisterLocation, RegisterDirection);

/// Supported types of operands that can be accessed.
pub enum Operand {
    /// An encoded register.
    R(Register),
    /// 8-bit immediate encoded in byte 2.
    I8,
    /// 16-bit immediate encoded in bytes 2 (low part) and 3 (high part).
    I16,
}

/// Assembly Instruction mnemonics described by the ISA.
#[derive(Debug)]
pub enum Mnemonic {
    // ===== Arithmetic instructions =====
    CMP,
    CMPU,
    CMPS,
    ADD,
    ADC,
    SUB,
    SBB,
    SHL,
    SHR,
    SAR,
    SHLC,
    SHRC,
    NOT,
    NEG,
    MOV,
    MOVF,
    MOVW,
    HSWAP,
    SETHI,
    CLEAR,
    CE,
    SETF,
    MULU,
    MULS,
    SEXT,
    EXTR,
    EXTRS,
    INS,
    AND,
    OR,
    XOR,
    XBIT,
    BSET,
    BCLR,
    BTGL,
    DIV,
    MOD,
    SETP,
    // ======= Memory instructions =======
    LD,
    ST,
    PUSH,
    MPUSH,
    POP,
    MPOP,
    MPOPRET,
    MPOPADD,
    MPOPADDRET,
    // ======= Branch instructions =======
    BRA,
    LBRA,
    JMP,
    CALL,
    LCALL,
    RET,
    // ======== Processor control ========
    EXIT,
    SLEEP,
    // ======= Code virtual memory =======
    PTLB,
    VTLB,
    ITLB,
    // ============ Interrupts ===========
    IRET,
    TRAP,
    // == Xfers to/from external memory ==
    XCLD,
    XDLD,
    XDST,
    XCWAIT,
    XDWAIT,
    // ==== Input/Output instructions ====
    IOWR,
    IOWRS,
    IORD,
    // ======= Crypto instructions =======
    NOP,
    CMOV,
    CXSIN,
    CXSOUT,
    CRND,
    CS0BEGIN,
    CS0EXEC,
    CS1BEGIN,
    CS1EXEC,
    CCHMOD,
    CXOR,
    CADD,
    CAND,
    CREV,
    CPRECMAC,
    CSECRET,
    CKEYREG,
    CKEXP,
    CKREXP,
    CENC,
    CDEC,
    CSIGAUTH,
    CSIGCLR,
    CSIGENC,
    // ===================================
}

/// A Falcon CPU instruction.
#[derive(Debug)]
pub struct Instruction {
    /// The general opcode this instruction belongs to.
    pub opcode: u8,
    /// The subopcode location for the instruction.
    ///
    /// This is an internally used property that is relevant for
    /// extracting further information from a given set of bytes.
    subopcode_location: SubopcodeLocation,
    /// The particular subopcode for this instruction.
    pub subopcode: u8,
    /// The instruction mnemonic.
    pub mnemonic: Mnemonic,
    /// The operand size this instruction has set.
    pub operand_size: OperandSize,
    /// An vector containing the operands for this instruction.
    operands: Vec<Operand>,
}

/// Extracts the subopcode from the opcode, given its location.
pub fn extract_subopcode(opcode: &[u8], location: SubopcodeLocation) -> u8 {
    match location {
        SubopcodeLocation::O1 => opcode[0] & 0xF,
        SubopcodeLocation::O2 => opcode[1] & 0xF,
        SubopcodeLocation::OL => opcode[1] & 0x3F,
        SubopcodeLocation::O3 => opcode[2] & 0xF,
    }
}

/// Reads the contents of a given binary file into a byte array.
pub fn read_binary<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut binary_buffer = Vec::new();

    let mut file = File::open(path)
        .expect("Failed to read the given binary! Please verify that its path is valid!");
    file.read_to_end(&mut binary_buffer).unwrap();

    binary_buffer.into_boxed_slice()
}
