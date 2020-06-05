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
    fn from(opcode: u8) -> Self {
        // Check the highest two bits.
        match opcode >> 6 {
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
    S,
    /// The register is encoded as a destination register.
    D,
    /// The register is encoded as both, source and destination.
    SD,
}

/// Represents a CPU register as it is encoded within an instruction.
///
/// Given are the information on where a register is exactly stored and
/// how it is exactly utilised.
pub struct Register(pub RegisterLocation, pub RegisterDirection);

/// Supported types of operands that can be accessed.
pub enum Operand {
    /// An encoded register.
    R(Register),
    /// 8-bit immediate encoded in byte 2.
    I8,
    /// 16-bit immediate encoded in bytes 2 (low part) and 3 (high part).
    I16,
}

/// Parses an operand notation into the corresponding [`Operand`] representation.
///
/// [`Operand`]: struct.Operand.html
macro_rules! operand {
    (R1S) => {
        Operand::R(Register(RegisterLocation::Low1, RegisterDirection::S))
    };
    (R2S) => {
        Operand::R(Register(RegisterLocation::High1, RegisterDirection::S))
    };
    (R3S) => {
        Operand::R(Register(RegisterLocation::High2, RegisterDirection::S))
    };
    (R1D) => {
        Operand::R(Register(RegisterLocation::Low1, RegisterDirection::D))
    };
    (R2D) => {
        Operand::R(Register(RegisterLocation::High1, RegisterDirection::D))
    };
    (R3D) => {
        Operand::R(Register(RegisterLocation::High2, RegisterDirection::D))
    };
    (R1SD) => {
        Operand::R(Register(RegisterLocation::Low1, RegisterDirection::SD))
    };
    (R2SD) => {
        Operand::R(Register(RegisterLocation::High1, RegisterDirection::SD))
    };
    (R3SD) => {
        Operand::R(Register(RegisterLocation::High2, RegisterDirection::SD))
    };
    (I8) => {
        Operand::I8
    };
    (I16) => {
        Operand::I16
    };
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

/// Given the first instruction byte, this function extracts the [`SubopcodeLocation`]
/// which can then be used to extract the actual subopcode through [`extract_subopcode`].
///
/// If the provided opcode is invalid, this function returns `None`.
///
/// [`SubopcodeLocation`]: enum.SubopcodeLocation.html
/// [`extract_subopcode`]: fn.extract_subopcode.html
pub fn get_subopcode_location(opcode: u8) -> Option<SubopcodeLocation> {
    match opcode {
        // Sized opcodes
        0x00..=0x2F => Some(SubopcodeLocation::O1),
        0x30..=0x37 => Some(SubopcodeLocation::O2),
        0x38..=0x3C => Some(SubopcodeLocation::O3),
        0x3d => Some(SubopcodeLocation::O2),

        // Unsized opcodes
        0xC0..=0xEF => Some(SubopcodeLocation::O1),
        0xF0..=0xF2 => Some(SubopcodeLocation::O2),
        0xF4..=0xF5 => Some(SubopcodeLocation::OL),
        0xF8..=0xF9 => Some(SubopcodeLocation::O2),
        0xFA => Some(SubopcodeLocation::O3),
        0xFC => Some(SubopcodeLocation::O2),
        0xFD..=0xFF => Some(SubopcodeLocation::O3),

        _ => None,
    }
}

/// Extracts the subopcode from the opcode, given its location.
pub fn extract_subopcode(instruction: &[u8], location: SubopcodeLocation) -> u8 {
    match location {
        SubopcodeLocation::O1 => instruction[0] & 0xF,
        SubopcodeLocation::O2 => instruction[1] & 0xF,
        SubopcodeLocation::OL => instruction[1] & 0x3F,
        SubopcodeLocation::O3 => instruction[2] & 0xF,
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
