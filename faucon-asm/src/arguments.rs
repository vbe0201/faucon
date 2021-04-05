use std::cmp::min;
use std::mem::size_of;
use std::ops::Range;

use num_traits::{cast, FromPrimitive, PrimInt, WrappingSub};

use crate::assembler::Token;
use crate::bytes_ext::ByteEncoding;
use crate::operands::{self, Operand};

// A trait that defines a quantity of a specific size that is stored at a
// specific position in Falcon machine code.
//
// This aids as a helper for reading and writing operands in binary data when
// their position is known, e.g. through fixed encoding strategies.
pub trait Positional {
    // The index of the starting byte in a byte slice where the quantity is
    // encoded.
    fn position(&self) -> usize;

    // The width of the encoded quantity in machine code, measured in bytes.
    fn width(&self) -> usize;

    // Gets the range of the encoded quantity within a slice of instruction bytes.
    fn as_range(&self) -> Range<usize> {
        self.position()..self.position() + self.width()
    }
}

// A trait outlining how specific quantities are encoded in Falcon machine code.
//
// This is used to extract information out of Falcon machine instructions and
// writing them, respectively.
pub trait MachineEncoding {
    type Output;

    // Reads the underlying output type out of the given byte slice containing
    // one Falcon machine instruction.
    fn read(&self, instruction: &[u8]) -> Self::Output;

    // Checks if an assembly token matches the criteria to be encoded into its
    // machine representation for a specific operand.
    fn matches(&self, token: &Token) -> bool;

    // Writes the underlying output type from a value to a buffer containing
    // Falcon machine code bytes.
    fn write(&self, code: &mut [u8], element: Self::Output);

    // Writes the value of an instruction operand to a buffer containing Falcon
    // machine code bytes.
    fn write_operand(&self, code: &mut [u8], element: Operand);
}

// A helper macro that is supposed to unwrap an `Argument` of a known kind into
// its contained type, may it be a `Register`, `Immediate` or `MemoryAccess`.
//
// This is useful when operands encode each other, e.g. a memory access encodes
// a specific register argument, to remove the boilerplate code of validating a
// variant of `Argument` at runtime by feeding that directly into the constant
// evaluation engine.
macro_rules! unwrap {
    ($argument:expr, $variant:pat => $result:expr) => {
        if let $variant = $argument {
            $result
        } else {
            // SAFETY: As an internal macro that is only applied on constant
            // `Argument` instances with known variants, it always produces the
            // expected results and code flow never gets to land here.
            unsafe { ::std::hint::unreachable_unchecked() }
        }
    };
}

// An unsigned 8-bit immediate.
//
// These are used for bit positions, shifts and 8-bit instructions.
pub const I8: Argument = Argument::U8(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 8-bit immediate.
//
// These are used for comparisons and PC-relative offsets.
pub const I8S: Argument = Argument::I8(Immediate {
    position: 2,
    width: 1,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// An unsigned 8-bit immediate zero-extended to 16 bits.
//
// These are used for sethi and 16-bit instructions.
pub const I8ZX16: Argument = Argument::U16(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 8-bit immediate sign-extended to 16 bits.
//
// These are used for sethi and 16-bit instructions.
pub const I8SX16: Argument = Argument::I16(Immediate {
    position: 2,
    width: 1,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// An unsigned 8-bit immediate zero-extended to 32 bits.
//
// These are used for memory addressing and most 32-bit instructions.
pub const I8ZX32: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 32-bit immediate sign-extended to 32 bits.
//
// These are used for memory addressing and most 32-bit instructions.
pub const I8SX32: Argument = Argument::I32(Immediate {
    position: 2,
    width: 1,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 8-bit immediate sign-extended to 32 bits.
//
// These are used for Falcon v5 MOV instructions.
pub const I8SX32P1: Argument = Argument::I32(Immediate {
    position: 1,
    width: 1,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
// by one.
//
// These are mainly used for memory addressing.
pub const I8ZX32S1: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: Some(1),
    mask: None,
    raw_value: None,
});

// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
// by two.
//
// These are mainly used for memory addressing.
pub const I8ZX32S2: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: Some(2),
    mask: None,
    raw_value: None,
});

// An unsigned 8-bit immediate zero-extended to 32 bits and shifted left
// by 16.
//
// These are used by the SETHI instruction.
pub const I8ZX32S16: Argument = Argument::U32(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: Some(16),
    mask: None,
    raw_value: None,
});

// A helper that leverages the selection of a correct parser for immediates
// in sized instructions to the disassembler.
//
// This handles zero-extension for the different operand sizes.
pub const I8ZXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I8,
    1 => I8ZX16,
    2 => I8ZX32,
    _ => unreachable!(),
});

// A helper that leverages the selection of an appropriate parser for immediates
// in sized instructions to the disassembler.
//
// This handles sign-extension for the different operand sizes.
pub const I8SXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I8S,
    1 => I8SX16,
    2 => I8SX32,
    _ => unreachable!(),
});

// An unsigned 16-bit immediate truncated to the low 8 bits.
//
// Used by 8-bit instructions which have a 16-bit immediate form for
// whatever reason.
pub const I16T8: Argument = Argument::U8(Immediate {
    position: 2,
    width: 2,
    sign: false,
    shift: None,
    mask: Some(0xFF),
    raw_value: None,
});

// An unsigned 16-bit immediate.
//
// These are used by sethi and 16-bit instructions.
pub const I16: Argument = Argument::U16(Immediate {
    position: 2,
    width: 2,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 16-bit immediate.
//
// These are used for signed comparisons.
pub const I16S: Argument = Argument::I16(Immediate {
    position: 2,
    width: 2,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// An unsigned 16-bit immediate zero-extended to 32 bits.
//
// These are used for most 32-bit instructions.
pub const I16ZX32: Argument = Argument::U32(Immediate {
    position: 2,
    width: 2,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// An unsigned 16-bit immediate zero-extended to 32 bits.
//
// These are used for Falcon v5 call instructions.
pub const I16ZX32P1: Argument = Argument::U32(Immediate {
    position: 1,
    width: 2,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 16-bit immediate sign-extended to 32 bits.
//
// These are used for most 32-bit instructions.
pub const I16SX32: Argument = Argument::I32(Immediate {
    position: 2,
    width: 2,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 16-bit immediate sign-extended to 32 bits.
//
// These are used for Falcon v5 MOV instructions.
pub const I16SX32P1: Argument = Argument::I32(Immediate {
    position: 1,
    width: 2,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// A helper that leverages the selection of a correct parser for immediates
// in sized instructions to the disassembler.
//
// This handles zero-extension for the different operand sizes.
pub const I16ZXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I16T8,
    1 => I16,
    2 => I16ZX32,
    _ => unreachable!(),
});

// A helper that leverages the selection of an appropriate parser for immediates
// in sized instructions to the disassembler.
//
// This handles sign-extension for the different operand sizes.
pub const I16SXS: Argument = Argument::SizeConverter(|size| match size {
    0 => I16T8,
    1 => I16S,
    2 => I16SX32,
    _ => unreachable!(),
});

// An unsigned 24-bit immediate zero-extended to 32 bits.
//
// These are used for absolute call/jump addresses.
pub const I24ZX32: Argument = Argument::U32(Immediate {
    position: 1,
    width: 3,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// A signed 24-bit immediate sign-extended to 32 bits.
//
// These are used for Falcon v5 CALL instructions.
pub const I24SX32: Argument = Argument::I32(Immediate {
    position: 1,
    width: 3,
    sign: true,
    shift: None,
    mask: None,
    raw_value: None,
});

// An unsigned 32-bit immediate.
//
// These are used for MOV instructions.
pub const I32: Argument = Argument::U32(Immediate {
    position: 1,
    width: 4,
    sign: false,
    shift: None,
    mask: None,
    raw_value: None,
});

// An 8-bit PC-relative offset.
//
// These are used for branches.
pub const PC8: Argument = Argument::PcRel8(unwrap!(I8S, Argument::I8(i) => i));

// A 16-bit PC-relative offset.
//
// These are used for branches.
pub const PC16: Argument = Argument::PcRel16(unwrap!(I16S, Argument::I16(i) => i));

// A bitfield within an 8-bit-range.
//
// Bitfields are used for specific bitfield manipulation instructions and denote
// the lower bit index and its size subtracted by 1.
pub const BITF8: Argument = Argument::Bitfield(unwrap!(I8ZX32, Argument::U32(i) => i));

// A bitfield within a 16-bit-range.
//
// Bitfields are used for specific bitfield manipulation instructions and denote
// the lower bit index and its size subtracted by 1.
pub const BITF16: Argument = Argument::Bitfield(unwrap!(I16ZX32, Argument::U32(i) => i));

// A Falcon general-purpose register, encoded in the low 4 bits of the first
// instruction byte.
pub const R0: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Gpr,
    position: 0,
    high: false,
    raw_value: None,
});

// A Falcon general-purpose register, encoded in the low 4 bits of the second
// instruction byte.
pub const R1: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Gpr,
    position: 1,
    high: false,
    raw_value: None,
});

// A Falcon general-purpose register, encoded in the high 4 bits of the second
// instruction byte.
pub const R2: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Gpr,
    position: 1,
    high: true,
    raw_value: None,
});

// A Falcon general-purpose register, encoded in the high 4 bits of the third
// instruction byte.
pub const R3: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Gpr,
    position: 2,
    high: true,
    raw_value: None,
});

// The stack pointer register.
//
// It is used for instructions that operate on $sp by default, without
// encoding its value in the instruction bytes.
pub const SP: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Spr,
    position: 0,
    high: false,
    raw_value: Some(4),
});

// The CPU flags register.
//
// It is used for instructions that operate on $csw by default, without
// encoding its value in the instruction bytes.
pub const CSW: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Spr,
    position: 0,
    high: false,
    raw_value: Some(8),
});

// A selected bit in the Falcon $csw register.
//
// Treated as an 8-bit immediate by the hardware, used for miscellaneous
// instructions that operate on the flag bits.
pub const FLAG: Argument = Argument::Flag(Immediate {
    position: 2,
    width: 1,
    sign: false,
    shift: None,
    mask: Some(0x1F),
    raw_value: None,
});

// A selected predicate bit in the Falcon $csw register.
//
// Treated actually as a part of the subopcodes for `BP`/`BNP` instructions,
// this decides which bit of the CSW register should be checked.
pub const PRED: Argument = Argument::Flag(Immediate {
    position: 1,
    width: 1,
    sign: false,
    shift: None,
    mask: Some(0b111),
    raw_value: None,
});

// A software trap value.
//
// It is used by the TRAP instruction and is encoded in the low two bits of
// instruction byte 1, the subopcode in this case.
pub const TRAP: Argument = Argument::U8(Immediate {
    position: 1,
    width: 1,
    sign: false,
    shift: None,
    mask: Some(0b11),
    raw_value: None,
});

// A Falcon special-purpose register, encoded in the high 4 bits of the second
// instruction byte.
pub const SR1: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Spr,
    position: 1,
    high: true,
    raw_value: None,
});

// A Falcon special-purpose register, encoded in the low 4 bits of the second
// instruction byte.
pub const SR2: Argument = Argument::Register(Register {
    kind: operands::RegisterKind::Spr,
    position: 1,
    high: false,
    raw_value: None,
});

// A memory access to an 8-bit value in Falcon DMem. The address is stored in a single
// register.
pub const MEMR8: Argument = Argument::Memory(MemoryAccess::Reg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
));

// A memory access to a 16-bit value in Falcon DMem. The address is stored in a single
// register.
pub const MEMR16: Argument = Argument::Memory(MemoryAccess::Reg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
));

// A memory access to a 32-bit value in Falcon DMem. The address is stored in a single
// register.
pub const MEMR32: Argument = Argument::Memory(MemoryAccess::Reg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
));

// A helper that leverages the selection of an appropriate parser for memory access
// encodings in sized instructions to the disassembler.
pub const MEMR: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMR8,
    1 => MEMR16,
    2 => MEMR32,
    _ => unreachable!(),
});

// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
// base address in a register and an immediate offset.
pub const MEMRI8: Argument = Argument::Memory(MemoryAccess::RegImm(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32, Argument::U32(imm) => imm),
));

// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
// base address in a register and an immediate offset.
pub const MEMRI16: Argument = Argument::Memory(MemoryAccess::RegImm(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32S1, Argument::U32(imm) => imm),
));

// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
// base address in a register and an immediate offset.
pub const MEMRI32: Argument = Argument::Memory(MemoryAccess::RegImm(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32S2, Argument::U32(imm) => imm),
));

// A helper that leverages the selection of an appropriate parser for memory access
// encodings in sized instructions to the disassembler.
pub const MEMRI: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMRI8,
    1 => MEMRI16,
    2 => MEMRI32,
    _ => unreachable!(),
});

// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
// base address in the `$sp` register and an immediate offset.
pub const MEMSPI8: Argument = Argument::Memory(MemoryAccess::RegImm(
    operands::MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(I8ZX32, Argument::U32(imm) => imm),
));

// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
// base address in the `$sp` register and an immediate offset.
pub const MEMSPI16: Argument = Argument::Memory(MemoryAccess::RegImm(
    operands::MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(I8ZX32S1, Argument::U32(imm) => imm),
));

// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
// base address in the `$sp` register and an immediate offset.
pub const MEMSPI32: Argument = Argument::Memory(MemoryAccess::RegImm(
    operands::MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(I8ZX32S2, Argument::U32(imm) => imm),
));

// A helper that leverages the selection of an appropriate parser for memory access
// encodings in sized instructions to the disassembler.
pub const MEMSPI: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMSPI8,
    1 => MEMSPI16,
    2 => MEMSPI32,
    _ => unreachable!(),
});

// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
// base address in the `$sp` register and an offset in another register.
pub const MEMSPR8: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    1,
));

// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
// base address in the `$sp` register and an offset * 2 in another register.
pub const MEMSPR16: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    2,
));

// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
// base address in the `$sp` register and an offset * 4 in another register.
pub const MEMSPR32: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(SP, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    4,
));

// A helper that leverages the selection of an appropriate parser for memory access
// encodings in sized instructions to the disassembler.
pub const MEMSPR: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMSPR8,
    1 => MEMSPR16,
    2 => MEMSPR32,
    _ => unreachable!(),
});

// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
// base address in a register and an offset in another register.
pub const MEMRR8: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    1,
));

// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
// base address in a register and an offset * 2 in another register.
pub const MEMRR16: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    2,
));

// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
// base address in a register and an offset * 4 in another register.
pub const MEMRR32: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    4,
));

// A helper that leverages the selection of an appropriate parser for memory access
// encodings in sized instructions to the disassembler.
pub const MEMRR: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMRR8,
    1 => MEMRR16,
    2 => MEMRR32,
    _ => unreachable!(),
});

// A memory access to an 8-bit value in Falcon DMem. The address is composed from a
// base address in a register and an offset in another register.
pub const MEMRRALT8: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R3, Argument::Register(r) => r),
    1,
));

// A memory access to a 16-bit value in Falcon DMem. The address is composed from a
// base address in a register and an offset * 2 in another register.
pub const MEMRRALT16: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R3, Argument::Register(r) => r),
    2,
));

// A memory access to a 32-bit value in Falcon DMem. The address is composed from a
// base address in a register and an offset * 4 in another register.
pub const MEMRRALT32: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::DMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R3, Argument::Register(r) => r),
    4,
));

// A helper that leverages the selection of an appropriate parser for memory access
// encodings in sized instructions to the disassembler.
pub const MEMRRALT: Argument = Argument::SizeConverter(|size| match size {
    0 => MEMRRALT8,
    1 => MEMRRALT16,
    2 => MEMRRALT32,
    _ => unreachable!(),
});

// A memory access to a 32-bit value in Falcon IMem. The address is specified by a
// single register.
pub const IOR: Argument = Argument::Memory(MemoryAccess::Reg(
    operands::MemorySpace::IMem,
    unwrap!(R2, Argument::Register(r) => r),
));

// A memory access to a 32-bit value in Falcon IMem. The address is composed from a
// base address in a register and an offset * 4 in another register.
pub const IORR: Argument = Argument::Memory(MemoryAccess::RegReg(
    operands::MemorySpace::IMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(R1, Argument::Register(r) => r),
    4,
));

// A memory access to a 32-bit value in Falcon IMem. The address is composed from a
// base address in a register and an immediate offset.
pub const IORI: Argument = Argument::Memory(MemoryAccess::RegImm(
    operands::MemorySpace::IMem,
    unwrap!(R2, Argument::Register(r) => r),
    unwrap!(I8ZX32S2, Argument::U32(imm) => imm),
));

// Dissects a bitfield given its encoding as an immediate.
//
// An encoded bitfield is either a 10-bit or 8-bit immediate which encodes
// the lower start index of the field in its low 5 bits. The high 3 or 5 bits,
// respectively, encode the amount of bits to extract subtracted by one.
#[inline]
pub const fn dissect_bitfield(imm: u32) -> (u32, u32) {
    (imm & 0x1F, imm >> 5 & 0x1F)
}

#[inline]
const fn build_raw_bitfield(n: u32, i: u32) -> u32 {
    n << 5 | i
}

// Builds a bitfield immediate encoding based on the supplied information.
//
// `i` is the lower start index of the bitfield, whereas `n` is the higher
// boundary of bits to cover.
#[inline]
pub const fn build_bitfield(start: u32, end: u32) -> u32 {
    build_raw_bitfield(end - start, start)
}

#[inline]
fn sign_extend<T>(value: T, numbits: usize) -> T
where
    T: FromPrimitive + PrimInt,
{
    if size_of::<T>() == (numbits >> 3) {
        return value;
    }

    if ((value >> (numbits - 1)) & T::one()) != T::zero() {
        value | (!T::zero()) << numbits
    } else {
        value
    }
}

// Wrapper around Falcon instruction operands.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Argument {
    // A helper that selects an argument at runtime depending on instruction size.
    //
    // This converter holds a closure which consumes the high two bits of an opcode
    // encoding the instruction size in order to select a fitting argument for
    // translating the operands. This is especially helpful to select which width
    // to extend an immediate to when dealing with variable sizing.
    SizeConverter(fn(size: u8) -> Argument),

    // A signed 8-bit PC-relative offset.
    PcRel8(Immediate<i8>),
    // A signed 16-bit PC-relative offset.
    PcRel16(Immediate<i16>),

    // An unsigned 8-bit immediate.
    U8(Immediate<u8>),
    // A signed 8-bit immediate.
    I8(Immediate<i8>),
    // An unsigned 16-bit immediate.
    U16(Immediate<u16>),
    // A signed 16-bit immediate.
    I16(Immediate<i16>),
    // An unsigned 32-bit immediate.
    U32(Immediate<u32>),
    // A signed 32-bit immediate.
    I32(Immediate<i32>),

    // A bitfield range encoded as an immediate.
    Bitfield(Immediate<u32>),

    // A CPU register.
    Register(Register),
    // A flag bit in the `$csw` register.
    Flag(Immediate<u8>),

    // A direct memory access to an address in a specific memory space.
    Memory(MemoryAccess),
}

impl Positional for Argument {
    fn position(&self) -> usize {
        match self {
            Argument::U8(imm) => imm.position(),
            Argument::I8(imm) => imm.position(),
            Argument::U16(imm) => imm.position(),
            Argument::I16(imm) => imm.position(),
            Argument::U32(imm) => imm.position(),
            Argument::I32(imm) => imm.position(),
            Argument::Bitfield(imm) => imm.position(),
            Argument::Register(reg) => reg.position(),
            Argument::Flag(imm) => imm.position(),
            Argument::Memory(mem) => mem.position(),
            Argument::PcRel8(imm) => imm.position(),
            Argument::PcRel16(imm) => imm.position(),
            Argument::SizeConverter(_) => unreachable!(),
        }
    }

    fn width(&self) -> usize {
        match self {
            Argument::U8(imm) => imm.width(),
            Argument::I8(imm) => imm.width(),
            Argument::U16(imm) => imm.width(),
            Argument::I16(imm) => imm.width(),
            Argument::U32(imm) => imm.width(),
            Argument::I32(imm) => imm.width(),
            Argument::Bitfield(imm) => imm.width(),
            Argument::Register(reg) => reg.width(),
            Argument::Flag(imm) => imm.width(),
            Argument::Memory(mem) => mem.width(),
            Argument::PcRel8(imm) => imm.width(),
            Argument::PcRel16(imm) => imm.width(),
            Argument::SizeConverter(_) => unreachable!(),
        }
    }
}

// An immediate operand in Falcon assembly.
//
// Immediates can either carry metadata to parse them from instruction
// bytes or a fixed value that is returned unconditionally.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Immediate<T> {
    position: usize,
    width: usize,
    sign: bool,
    shift: Option<usize>,
    mask: Option<T>,

    raw_value: Option<T>,
}

impl<T: FromPrimitive + PrimInt + WrappingSub> Immediate<T> {
    #[inline]
    fn get_shift(&self) -> usize {
        self.shift.unwrap_or(0)
    }

    #[inline]
    fn get_mask(&self) -> T {
        self.mask.unwrap_or(T::zero().wrapping_sub(&T::one()))
    }
}

impl<T> Positional for Immediate<T> {
    fn position(&self) -> usize {
        self.position
    }

    fn width(&self) -> usize {
        self.width
    }
}

impl<T: FromPrimitive + PrimInt + ByteEncoding + WrappingSub> MachineEncoding
    for Immediate<T>
{
    type Output = T;

    fn read(&self, instruction: &[u8]) -> Self::Output {
        // If this immediate is defined by a fixed value, return it.
        if let Some(value) = self.raw_value {
            return value;
        }

        let mut result = T::read_from_bytes(&instruction[self.as_range()], self.width());
        if let Some(mask) = self.mask {
            result = result & mask;
        }
        result << self.get_shift()
    }

    fn matches(&self, token: &Token) -> bool {
        // Try to extract the value and convert it into the appropriate type. If that
        // fails, we can assume that the value would not produce a match anyway.
        let value = if let Some(value) = match token {
            Token::Flag(_) => return true,
            Token::SignedInt(imm) => cast(*imm),
            Token::UnsignedInt(imm) => cast(*imm),
            Token::Bitfield((start, end)) => cast(build_bitfield(*start, *end)),
            _ => return false,
        } {
            value
        } else {
            return false;
        };

        // If this operand defaults to a fixed value, compare it against the token.
        if let Some(raw_value) = self.raw_value {
            return value == raw_value;
        }

        // Do an appropriate boundary check to determine whether the value is valid.
        if self.sign {
            // Calculate the boundaries of the supported value range. Signed
            // numbers generally don't have shifts or bitmasks applied to them.
            let nbits = (self.width << 3) - 1;
            let min_value = sign_extend(T::one() << nbits, self.width << 3);
            let max_value = !min_value;

            min_value <= value && value <= max_value
        } else {
            // Calculate the upper boundary of the supported value range.
            // The lower boundary of `0` is enforced by Rust's design.
            let nbits = (size_of::<T>() - self.width as usize) << 3;
            let max_value = (T::zero().wrapping_sub(&T::one()) << nbits) >> nbits;

            value <= max_value
        }
    }

    fn write(&self, code: &mut [u8], element: Self::Output) {
        if self.raw_value.is_none() {
            (element >> self.get_shift()).write_to_bytes(
                &mut code[self.as_range()],
                self.get_mask(),
                self.width(),
            );
        }
    }

    fn write_operand(&self, code: &mut [u8], element: Operand) {
        match element {
            Operand::Flag(imm) => self.write(code, cast(imm).unwrap()),
            Operand::Imm(imm) => self.write(code, cast(imm).unwrap()),
            Operand::UImm(imm) => self.write(code, cast(imm).unwrap()),
            Operand::Bitfield(i, n) => self.write(code, cast(build_raw_bitfield(n, i)).unwrap()),
            _ => panic!("Cannot encode operand kind as immediate"),
        }
    }
}

// A CPU register in Falcon assembly.
//
// There are 16 general-purpose register and 16 additional special-purpose registers.
// This structure holds the data necessary to parse them form instruction bytes and
// to determine how they are used by an instruction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Register {
    kind: operands::RegisterKind,
    position: usize,
    high: bool,

    raw_value: Option<u8>,
}

impl Register {
    fn parse(&self, byte: u8) -> u8 {
        if self.high {
            byte >> 4
        } else {
            byte & 0xF
        }
    }
}

impl Positional for Register {
    fn position(&self) -> usize {
        self.position
    }

    fn width(&self) -> usize {
        1
    }
}

impl MachineEncoding for Register {
    type Output = operands::Register;

    fn read(&self, instruction: &[u8]) -> Self::Output {
        let register = if let Some(value) = self.raw_value {
            value
        } else {
            self.parse(instruction[self.position])
        };

        operands::Register(self.kind, register as usize)
    }

    fn matches(&self, token: &Token) -> bool {
        match token {
            Token::Register(reg) => match self.raw_value {
                Some(_) => true, // Default values do always match.
                None => reg.0 == self.kind && reg.1 < 16,
            },
            _ => false,
        }
    }

    fn write(&self, code: &mut [u8], element: Self::Output) {
        // If this register has a fixed value, there's no need to serialize it.
        if self.raw_value.is_none() {
            if self.high {
                code[self.position] = code[self.position] & 0xF | (element.1 as u8) << 4;
            } else {
                code[self.position] = code[self.position] & !0xF | element.1 as u8;
            }
        }
    }

    fn write_operand(&self, code: &mut [u8], element: Operand) {
        match element {
            Operand::Register(reg) => self.write(code, reg),
            _ => panic!("Cannot encode operand kind as register"),
        }
    }
}

// A direct Falcon memory access composed of registers, immediates and magic values.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemoryAccess {
    // A memory access to either DMEM or IMEM that is composed of a single register.
    //
    // The address calculation takes the following form: `[$reg]`
    Reg(operands::MemorySpace, Register),
    // A memory access to either DMEM or IMEM that is composed of two registers and a
    // constant value that is used for scaling the offset.
    //
    // The address calculation takes the following form: `[$reg1 + $reg2 * scale]`
    RegReg(operands::MemorySpace, Register, Register, u8),
    // A memory access to either DMEM or IMEM that is composed of a register and an
    // immediate value.
    //
    // The address calculation takes the following form: `[$reg + imm]`
    RegImm(operands::MemorySpace, Register, Immediate<u32>),
}

impl Positional for MemoryAccess {
    fn position(&self) -> usize {
        match self {
            MemoryAccess::Reg(_, reg) => reg.position(),
            MemoryAccess::RegReg(_, reg1, reg2, _) => min(reg1.position(), reg2.position()),
            MemoryAccess::RegImm(_, reg, imm) => min(reg.position(), imm.position()),
        }
    }

    fn width(&self) -> usize {
        match self {
            MemoryAccess::Reg(_, _) => 1,
            MemoryAccess::RegReg(_, _, _, _) => 1,
            MemoryAccess::RegImm(_, reg, imm) => reg.width() + imm.width(),
        }
    }
}

impl MachineEncoding for MemoryAccess {
    type Output = operands::MemoryAccess;

    fn read(&self, instruction: &[u8]) -> Self::Output {
        match self {
            MemoryAccess::Reg(space, reg) => operands::MemoryAccess::Reg {
                space: *space,
                base: reg.read(instruction),
            },
            MemoryAccess::RegReg(space, reg1, reg2, scale) => operands::MemoryAccess::RegReg {
                space: *space,
                base: reg1.read(instruction),
                offset: reg2.read(instruction),
                scale: *scale,
            },
            MemoryAccess::RegImm(space, reg, imm) => operands::MemoryAccess::RegImm {
                space: *space,
                base: reg.read(instruction),
                offset: imm.read(instruction),
            },
        }
    }

    fn matches(&self, token: &Token) -> bool {
        match (self, token) {
            (
                MemoryAccess::Reg(_, _base),
                Token::Memory(operands::MemoryAccess::Reg { space: _, base }),
            ) => _base.matches(&Token::Register(*base)),
            (
                MemoryAccess::RegReg(_, _base, _offset, _scale),
                Token::Memory(operands::MemoryAccess::RegReg {
                    space: _,
                    base,
                    offset,
                    scale,
                }),
            ) => {
                _base.matches(&Token::Register(*base))
                    && _offset.matches(&Token::Register(*offset))
                    && _scale == scale
            }
            (
                MemoryAccess::RegImm(_, _base, _),
                Token::Memory(operands::MemoryAccess::Reg { space: _, base }),
            ) => _base.matches(&Token::Register(*base)),
            (
                MemoryAccess::RegImm(_, _base, _offset),
                Token::Memory(operands::MemoryAccess::RegImm {
                    space: _,
                    base,
                    offset,
                }),
            ) => {
                _base.matches(&Token::Register(*base))
                    && _offset.matches(&Token::UnsignedInt(*offset))
            }
            _ => false,
        }
    }

    fn write(&self, code: &mut [u8], element: Self::Output) {
        match (self, element) {
            (MemoryAccess::Reg(_, _base), operands::MemoryAccess::Reg { space: _, base }) => {
                _base.write(code, base);
            }
            (
                MemoryAccess::RegReg(_, _base, _offset, _),
                operands::MemoryAccess::RegReg {
                    space: _,
                    base,
                    offset,
                    scale: _,
                },
            ) => {
                _base.write(code, base);
                _offset.write(code, offset);
            }
            (
                MemoryAccess::RegImm(_, _base, _offset),
                operands::MemoryAccess::Reg { space: _, base },
            ) => {
                _base.write(code, base);
                _offset.write(code, 0);
            }
            (
                MemoryAccess::RegImm(_, _base, _offset),
                operands::MemoryAccess::RegImm {
                    space: _,
                    base,
                    offset,
                },
            ) => {
                _base.write(code, base);
                _offset.write(code, offset);
            }
            _ => panic!("Attempted to write invalid memory access form"),
        }
    }

    fn write_operand(&self, code: &mut [u8], element: Operand) {
        match element {
            Operand::Memory(mem) => self.write(code, mem),
            _ => panic!("Cannot encode operand kind as memory access"),
        }
    }
}
