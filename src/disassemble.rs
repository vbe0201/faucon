use std::fmt;

use crate::isa;

fn mnemonic(opcode: isa::Opcode) -> &'static str {
    match opcode {
        isa::Opcode::CMPU => "cmpu",
        isa::Opcode::CMPS => "cmps",
        isa::Opcode::CMP => "cmp",
        isa::Opcode::ADD => "add",
        isa::Opcode::CCR => "ccr",
        isa::Opcode::ADC => "adc",
        isa::Opcode::SUB => "sub",
        isa::Opcode::SBB => "sbb",
        isa::Opcode::SHL => "shl",
        isa::Opcode::SHR => "shr",
        isa::Opcode::SAR => "sar",
        isa::Opcode::SHLC => "shlc",
        isa::Opcode::SHRC => "shrc",
        isa::Opcode::NOT => "not",
        isa::Opcode::NEG => "neg",
        isa::Opcode::HSWAP => "hswap",
        isa::Opcode::SETHI => "sethi",
        isa::Opcode::CLEAR => "clear",
        isa::Opcode::TEST => "test",
        isa::Opcode::MULU => "mulu",
        isa::Opcode::MULS => "muls",
        isa::Opcode::SEXT => "sext",
        isa::Opcode::AND => "and",
        isa::Opcode::OR => "or",
        isa::Opcode::XOR => "xor",
        isa::Opcode::XBIT => "xbit",
        isa::Opcode::BSET => "bset",
        isa::Opcode::BCLR => "bclr",
        isa::Opcode::BTGL => "btgl",
        isa::Opcode::DIV => "div",
        isa::Opcode::MOD => "mod",
        isa::Opcode::SETP => "setp",
        isa::Opcode::EXTR => "extr",
        isa::Opcode::EXTRS => "extrs",
        isa::Opcode::INS => "ins",
        isa::Opcode::MOV => "mov",
        isa::Opcode::LD => "ld",
        isa::Opcode::ST => "st",
        isa::Opcode::PUSH => "push",
        isa::Opcode::POP => "pop",
        isa::Opcode::MPUSH => "mpush",
        isa::Opcode::MPOP => "mpop",
        isa::Opcode::MPOPADD => "mpopadd",
        isa::Opcode::MPOPRET => "mpopret",
        isa::Opcode::MPOPADDRET => "mpopaddret",
        isa::Opcode::CALL => "call",
        isa::Opcode::LCALL => "lcall",
        isa::Opcode::JMP => "jmp",
        isa::Opcode::LJMP => "ljmp",
        isa::Opcode::BP => "bp",
        isa::Opcode::BC => "bc",
        isa::Opcode::BO => "bo",
        isa::Opcode::BS => "bs",
        isa::Opcode::BZ => "bz",
        isa::Opcode::BA => "ba",
        isa::Opcode::BNA => "bna",
        isa::Opcode::BRA => "bra",
        isa::Opcode::BNP => "bnp",
        isa::Opcode::BNC => "bnc",
        isa::Opcode::BNO => "bno",
        isa::Opcode::BNS => "bns",
        isa::Opcode::BNZ => "bnz",
        isa::Opcode::BG => "bg",
        isa::Opcode::BLE => "ble",
        isa::Opcode::BL => "bl",
        isa::Opcode::BGE => "bge",
        isa::Opcode::BCMPE => "bcmpe",
        isa::Opcode::BCMPNE => "bcmpne",
        isa::Opcode::RET => "ret",
        isa::Opcode::HALT => "halt",
        isa::Opcode::SLEEP => "sleep",
        isa::Opcode::IMBLK => "imblk",
        isa::Opcode::IMTAG => "imtag",
        isa::Opcode::IMINV => "iminv",
        isa::Opcode::IRET => "iret",
        isa::Opcode::TRAP => "trap",
        isa::Opcode::IMLD => "imld",
        isa::Opcode::DMLD => "dmld",
        isa::Opcode::DMST => "dmst",
        isa::Opcode::IMWAIT => "imwait",
        isa::Opcode::DMWAIT => "dmwait",
        isa::Opcode::DMFENCE => "dmfence",
        isa::Opcode::IOWR => "iowr",
        isa::Opcode::IOWRS => "iowrs",
        isa::Opcode::IORD => "iord",
        isa::Opcode::IORDS => "iords",
    }
}

fn size_suffix(size: isa::OperandSize) -> &'static str {
    match size {
        isa::OperandSize::EightBit => ".b",
        isa::OperandSize::SixteenBit => ".h",
        isa::OperandSize::ThirtyTwoBit => ".w",
        isa::OperandSize::Unsized => "",
    }
}

fn special_reg_name(reg: isa::Register) -> &'static str {
    static NAMES: [&'static str; 16] = [
        "iv0", "iv1", "iv2", "ev", "sp", "pc", "imb", "dmb", "csw", "ccr", "sec", "ctx", "exci",
        "sec1", "imb1", "dmb1",
    ];

    NAMES[reg.index as usize & 0xF]
}

fn flag_name(flag: isa::Flag) -> &'static str {
    static NAMES: [&'static str; 32] = [
        "p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "c", "o", "s", "z", "??", "??", "??", "??",
        "ie0", "ie1", "ie2", "??", "is0", "is1", "is2", "??", "ea", "??", "??", "??", "??", "??",
        "??", "??",
    ];

    NAMES[flag.0 as usize & 0x1F]
}

fn memory_space(space: isa::MemorySpace) -> char {
    match space {
        isa::MemorySpace::Io => 'I',
        isa::MemorySpace::Data => 'D',
    }
}

fn format_imm(imm: isa::Immediate, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let (sign, value) = if imm.signed {
        let tmp = imm.value.cast_signed();
        (if tmp.is_negative() { "-" } else { "" }, tmp.unsigned_abs())
    } else {
        ("", imm.value)
    };

    write!(f, "{sign}{value:#x}")
}

fn format_reg(reg: isa::Register, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match reg.kind {
        isa::RegisterKind::Gpr => write!(f, "$r{}", reg.index),
        isa::RegisterKind::Spr => write!(f, "${}", special_reg_name(reg)),
    }
}

fn format_mem(mem: isa::MemoryAccess, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}[", memory_space(mem.space))?;
    format_reg(mem.base, f)?;

    match mem.offset {
        isa::MemoryOffset::Reg { index, scale } if scale != 0 => {
            write!(f, " + ")?;
            format_reg(index, f)?;
            if scale != 1 {
                write!(f, " * {scale}")?;
            }
        }
        isa::MemoryOffset::Imm(disp) if disp != 0 => write!(f, " + {disp:#x}")?,

        _ => (),
    }

    write!(f, "]")
}

fn format_flag(flag: isa::Flag, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", flag_name(flag))
}

fn format_bits(bits: isa::BitRange, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}", bits.start, bits.end)
}

fn format_instruction(insn: &isa::Instruction, pc: u32, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}{}", mnemonic(insn.opcode), size_suffix(insn.size))?;
    for operand in &insn.operands {
        if let isa::Operand::None = operand {
            break;
        }

        write!(f, " ")?;
        match operand {
            isa::Operand::None => (),
            &isa::Operand::Imm(imm) => format_imm(imm, f)?,
            &isa::Operand::Reg(reg) => format_reg(reg, f)?,
            &isa::Operand::Flag(flag) => format_flag(flag, f)?,
            &isa::Operand::Mem(mem) => format_mem(mem, f)?,
            &isa::Operand::Bits(bits) => format_bits(bits, f)?,
            &isa::Operand::PcRel(mut imm) => {
                imm.value = imm.value.wrapping_add(pc);
                format_imm(imm, f)?
            }
        }
    }

    Ok(())
}

/// An [`Instruction`] disassembled from a stream of code.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DisassembledInstruction {
    /// The instruction representation.
    pub insn: isa::Instruction,
    /// The program counter from which the instruction was read.
    pub pc: u32,
}

impl fmt::Display for DisassembledInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_instruction(&self.insn, self.pc, f)
    }
}

/// A disassembler which consumes a given byte slice of machine
/// code and returns [`DisassembledInstruction`]s.
///
/// The disassembler keeps yielding instructions until it runs
/// out of code or an error occurs during decoding. The
/// [`remainder()`] method can be used to disambiguate these
/// scenarios.
#[derive(Clone, Debug)]
pub struct StreamDisassembler<'a> {
    code: &'a [u8],
    pc: u32,
}

impl<'a> StreamDisassembler<'a> {
    /// Creates a stream disassembler over a slice of code.
    pub fn new(code: &'a [u8]) -> Self {
        Self { code, pc: 0 }
    }

    /// Creates a stream disassembler from code and an initial
    /// program counter value.
    pub fn new_with_pc(code: &'a [u8], pc: u32) -> Self {
        Self { code, pc }
    }

    /// Gets a view of the remaining code in the disassembler.
    #[inline]
    pub fn remainder(&self) -> &'a [u8] {
        self.code
    }
}

impl Iterator for StreamDisassembler<'_> {
    type Item = DisassembledInstruction;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_pc = self.pc;
        match isa::decode(&mut self.pc, self.code) {
            Some(insn) => {
                let num_bytes = (self.pc - cur_pc) as usize;
                self.code = &self.code[num_bytes..];
                Some(DisassembledInstruction { insn, pc: cur_pc })
            }
            None => None,
        }
    }
}
