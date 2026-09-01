use crate::{
    isa::{Instruction, Opcode, OperandSize, opcode, operands::*},
    utils,
};

const fn real_opcode(size: OperandSize, opcode: u8) -> u8 {
    let sub_field = opcode::classify_subopcode(opcode);
    let size_field = opcode::INSTRUCTION_SIZE;

    if sub_field.start() == size_field.start() && sub_field.end() == size_field.end() {
        // If the subopcode is located in the instruction size bits,
        // avoid encoding a meaningless operand size.
        opcode
    } else {
        opcode::set_operand_size(size, opcode)
    }
}

const fn imask(opcode: u8, subopcode: u8, operands: &[OperandSpec; 3]) -> (u64, u64) {
    // Match the given first byte of an instruction.
    let mut mask = 0xFF;
    let mut predicate = opcode as u64;

    // Then, match the given subopcode at the expected position.
    let subopcode_field = opcode::classify_subopcode(opcode);
    mask = subopcode_field.encode(mask, 0xFF);
    predicate = subopcode_field.encode(predicate, subopcode as u64);

    // Clear all bits in the mask that belong to operands.
    mask = operands[0].unmask(mask);
    mask = operands[1].unmask(mask);
    mask = operands[2].unmask(mask);

    // Clear all bits in the predicate that belong to operands.
    predicate = operands[0].unmask(predicate);
    predicate = operands[1].unmask(predicate);
    predicate = operands[2].unmask(predicate);

    (mask, predicate)
}

const fn wildcard((mask, pred): (u64, u64)) -> (u64, u64) {
    // For predicates with unsized operand mode bits, keep the mask
    // as-is. For sized instructions, clear the size bits and do not
    // match them as part of the mask.
    match opcode::operand_size(pred as u8) {
        OperandSize::Unsized => (mask, pred),
        _ => {
            let size_field = opcode::INSTRUCTION_SIZE;
            (size_field.encode(mask, 0), size_field.encode(pred, 0))
        }
    }
}

#[derive(Debug)]
struct InstructionSpec {
    opcode: Opcode,
    sized: bool,
    mask: u64,
    predicate: u64,
    operands: [OperandSpec; 3],
}

macro_rules! isa {
    // Public entrypoint: Creates a list of `InstructionSpec` entries.
    (
        $(
            $op:ident $([$sz:ident])? : $opc:literal/$sub:literal $([$($opr:expr),*])?
        ),* $(,)?
    ) => {
        [
            $(isa!(@entry $op $([$sz])? : $opc/$sub $([$($opr),*])?)),*
        ]
    };

    // Wildcard instruction specification.
    (@entry $op:ident: $opc:literal/$sub:literal $([$($opr:expr),*])?) => {{
        let operands = isa!(@operand $($($opr),*)?);
        let (mask, pred) = wildcard(imask($opc, $sub, &operands));
        InstructionSpec {
            opcode: Opcode::$op,
            sized: !matches!(opcode::operand_size($opc), OperandSize::Unsized),
            mask,
            predicate: pred,
            operands,
        }
    }};

    // Sized instruction specification.
    (@entry $op:ident[$sz:ident]: $opc:literal/$sub:literal $([$($opr:expr),*])?) => {{
        let operands = isa!(@operand $($($opr),*)?);
        let (mask, pred) = imask(
            real_opcode(OperandSize::$sz, $opc),
            $sub,
            &operands,
        );
        InstructionSpec {
            opcode: Opcode::$op,
            sized: !matches!(OperandSize::$sz, OperandSize::Unsized),
            mask,
            predicate: pred,
            operands,
        }
    }};

    // Helper: Create the 3-element `OperandSpec` list.
    (@operand) => { [const { NONE }; 3] };
    (@operand $op1:expr) => { [$op1, NONE, NONE] };
    (@operand $op1:expr, $op2:expr) => { [$op1, $op2, NONE] };
    (@operand $op1:expr, $op2:expr, $op3:expr) => { [$op1, $op2, $op3] };
}

impl InstructionSpec {
    #[inline]
    fn required_read_size(&self) -> usize {
        // Calculate the required bytes for opcode + subopcode matching.
        let mask_bits = u64::BITS - self.mask.leading_zeros();
        let mask_width = utils::align_up(mask_bits as u64, 8) as usize / 8;

        // Calculate the required bytes to decode all instruction operands.
        let operands_width = self
            .operands
            .iter()
            .map(OperandSpec::required_read_size)
            .max()
            .unwrap();

        mask_width.max(operands_width)
    }

    #[inline]
    fn size(&self, instruction: u64) -> OperandSize {
        if self.sized {
            opcode::operand_size(instruction as u8)
        } else {
            OperandSize::Unsized
        }
    }

    #[inline]
    fn match_bytes(&self, instruction: u64) -> bool {
        if (instruction & self.mask) == self.predicate {
            // Sized masks can potentially match unsized instructions,
            // so check size bits explicitly.
            match (
                opcode::operand_size(instruction as u8),
                opcode::operand_size(self.predicate as u8),
            ) {
                (OperandSize::Unsized, OperandSize::Unsized) => true,
                (OperandSize::Unsized, _) => false,
                _ => true,
            }
        } else {
            false
        }
    }
}

static INSTRUCTION_TABLE: [InstructionSpec; 284] = isa![
    // Compares two unsigned values and sets ALU flags based on the result.
    CMPU:           0x30/0x04 [R2, U8],
    CMPU[EightBit]: 0x31/0x04 [R2, U8],
    CMPU:           0x31/0x04 [R2, U16],
    CMPU:           0x24/0x04 [R2, R1],

    // Compares two signed values and sets ALU flags based on the result.
    CMPS:           0x30/0x05 [R2, I8],
    CMPS[EightBit]: 0x31/0x05 [R2, I8],
    CMPS:           0x31/0x05 [R2, I16],
    CMPS:           0x25/0x05 [R2, R1],

    // Compares two values and sets ALU flags based on the result.
    CMP:           0x30/0x06 [R2, I8],
    CMP[EightBit]: 0x31/0x06 [R2, I8],
    CMP:           0x31/0x06 [R2, I16],
    CMP:           0x26/0x06 [R2, R1],

    // Sums two operands and stores the result.
    ADD:           0x10/0x00 [R1, R2, U8],
    ADD[EightBit]: 0x38/0x00 [R1, R2, U8],
    ADD:           0x38/0x00 [R1, R2, U16],
    ADD:           0x3C/0x00 [R3, R2, R1],
    ADD:           0x36/0x00 [R2, U8],
    ADD[EightBit]: 0x37/0x00 [R2, U8],
    ADD:           0x37/0x00 [R2, U16],
    ADD:           0x3B/0x00 [R2, R1],
    ADD:           0xF4/0x30 [SP, I8],
    ADD:           0xF5/0x30 [SP, I16],
    ADD:           0xF9/0x01 [SP, R2],

    // Configures DMA override for the SCP.
    CCR: 0xF4/0x3C [U8],
    CCR: 0xF5/0x3C [U16],

    // Sums two operands with carry and stores the result.
    ADC:           0x11/0x01 [R1, R2, U8],
    ADC[EightBit]: 0x38/0x01 [R1, R2, U8],
    ADC:           0x38/0x01 [R1, R2, U16],
    ADC:           0x3C/0x01 [R3, R2, R1],
    ADC:           0x36/0x01 [R2, U8],
    ADC[EightBit]: 0x37/0x01 [R2, U8],
    ADC:           0x37/0x01 [R2, U16],
    ADC:           0x3B/0x01 [R2, R1],

    // Subtracts two operands and stores the result.
    SUB:           0x12/0x02 [R1, R2, U8],
    SUB[EightBit]: 0x38/0x02 [R1, R2, U8],
    SUB:           0x38/0x02 [R1, R2, U16],
    SUB:           0x3C/0x02 [R3, R2, R1],
    SUB:           0x36/0x02 [R2, U8],
    SUB[EightBit]: 0x37/0x02 [R2, U8],
    SUB:           0x37/0x02 [R2, U16],
    SUB:           0x3B/0x02 [R2, R1],

    // Subtracts two operands with borrow and stores the result.
    SBB:           0x13/0x03 [R1, R2, U8],
    SBB[EightBit]: 0x38/0x03 [R1, R2, U8],
    SBB:           0x38/0x03 [R1, R2, U16],
    SBB:           0x3C/0x03 [R3, R2, R1],
    SBB:           0x36/0x03 [R2, U8],
    SBB[EightBit]: 0x37/0x03 [R2, U8],
    SBB:           0x37/0x03 [R2, U16],
    SBB:           0x3B/0x03 [R2, R1],

    // Shifts a value left and stores the result.
    SHL: 0x14/0x04 [R1, R2, U8],
    SHL: 0x3C/0x04 [R3, R2, R1],
    SHL: 0x36/0x04 [R2, U8],
    SHL: 0x3B/0x04 [R2, R1],

    // Shifts a value right and stores the result.
    SHR: 0x15/0x05 [R1, R2, U8],
    SHR: 0x3C/0x05 [R3, R2, R1],
    SHR: 0x36/0x05 [R2, U8],
    SHR: 0x3B/0x05 [R2, R1],

    // Shifts a signed value right and stores the result.
    SAR: 0x17/0x07 [R1, R2, U8],
    SAR: 0x3C/0x07 [R3, R2, R1],
    SAR: 0x36/0x07 [R2, U8],
    SAR: 0x3B/0x07 [R2, R1],

    // Shifts a value left with carry and stores the result.
    SHLC: 0x1C/0x0C [R1, R2, U8],
    SHLC: 0x3C/0x0C [R3, R2, R1],
    SHLC: 0x36/0x0C [R2, U8],
    SHLC: 0x3B/0x0C [R2, R1],

    // Shifts a value right with carry and stores the result.
    SHRC: 0x1D/0x0D [R1, R2, U8],
    SHRC: 0x3C/0x0D [R3, R2, R1],
    SHRC: 0x36/0x0D [R2, U8],
    SHRC: 0x3B/0x0D [R2, R1],

    // Flips all bits in the value.
    NOT: 0x39/0x00 [R1, R2],
    NOT: 0x3D/0x00 [R2],

    // Negates a value.
    NEG: 0x39/0x01 [R1, R2],
    NEG: 0x3D/0x01 [R2],

    // Rotates a value by half its size.
    HSWAP: 0x39/0x03 [R1, R2],
    HSWAP: 0x3D/0x03 [R2],

    // Sets the high 16 bits of a register to a value.
    SETHI: 0xF0/0x03 [R2, U8S16],

    // Clears the contents of a register.
    CLEAR: 0x3D/0x04 [R2],

    // Sets flags in $csw based on the operand.
    TEST: 0x3D/0x05 [R2],

    // Performs unsigned multiplication and stores the result.
    MULU: 0xC0/0x00 [R1, R2, U8],
    MULU: 0xE0/0x00 [R1, R2, U16],
    MULU: 0xFF/0x00 [R3, R2, R1],
    MULU: 0xF0/0x00 [R2, U8],
    MULU: 0xFD/0x00 [R2, R1],

    // Performs signed multiplication and stores the result.
    MULS: 0xC1/0x01 [R1, R2, I8],
    MULS: 0xE1/0x01 [R1, R2, I16],
    MULS: 0xFF/0x01 [R3, R2, R1],
    MULS: 0xF0/0x01 [R2, I8],
    MULS: 0xFD/0x01 [R2, R1],

    // Sign-extends a value and stores the result.
    SEXT: 0xC2/0x02 [R1, R2, U8],
    SEXT: 0xFF/0x02 [R3, R2, R1],
    SEXT: 0xF0/0x02 [R2, U8],
    SEXT: 0xFD/0x02 [R2, R1],

    // Binary AND operation on two operands.
    AND: 0xC4/0x04 [R1, R2, U8],
    AND: 0xE4/0x04 [R1, R2, U16],
    AND: 0xFF/0x04 [R3, R2, R1],
    AND: 0xF0/0x04 [R2, U8],
    AND: 0xF1/0x04 [R2, U16],
    AND: 0xFD/0x04 [R2, R1],

    // Binary OR operation on two operands.
    OR: 0xC5/0x05 [R1, R2, U8],
    OR: 0xE5/0x05 [R1, R2, U16],
    OR: 0xFF/0x05 [R3, R2, R1],
    OR: 0xF0/0x05 [R2, U8],
    OR: 0xF1/0x05 [R2, U16],
    OR: 0xFD/0x05 [R2, R1],

    // Binary XOR operation on two operands.
    XOR: 0xC6/0x06 [R1, R2, U8],
    XOR: 0xE6/0x06 [R1, R2, U16],
    XOR: 0xFF/0x06 [R3, R2, R1],
    XOR: 0xF0/0x06 [R2, U8],
    XOR: 0xF1/0x06 [R2, U16],
    XOR: 0xFD/0x06 [R2, R1],

    // Extracts a bit and stores it in the lowest bit of destination.
    XBIT: 0xC8/0x08 [R1, R2, U8],
    XBIT: 0xFF/0x08 [R3, R2, R1],
    XBIT: 0xF0/0x0C [R2, CSW, FLAG],
    XBIT: 0xFE/0x0C [R1, CSW, R2],

    // Sets a specific bit in a given register.
    BSET: 0xF0/0x09 [R2, U8],
    BSET: 0xFD/0x09 [R2, R1],
    BSET: 0xF4/0x31 [CSW, FLAG],
    BSET: 0xF9/0x09 [CSW, R2],

    // Clears a specific bit in a given register.
    BCLR: 0xF0/0x0A [R2, U8],
    BCLR: 0xFD/0x0A [R2, R1],
    BCLR: 0xF4/0x32 [CSW, FLAG],
    BCLR: 0xF9/0x0A [CSW, R2],

    // Flips a specific bit in a given register.
    BTGL: 0xF0/0x0B [R2, U8],
    BTGL: 0xFD/0x0B [R2, R1],
    BTGL: 0xF4/0x33 [CSW, FLAG],
    BTGL: 0xF9/0x0B [CSW, R2],

    // Performs unsigned 32-bit division on the operands.
    DIV: 0xCC/0x0C [R1, R2, U8],
    DIV: 0xEC/0x0C [R1, R2, U16],
    DIV: 0xFF/0x0C [R3, R2, R1],

    // Computes the modulus of two unsigned 32-bit operands.
    MOD: 0xCD/0x0D [R1, R2, U8],
    MOD: 0xED/0x0D [R1, R2, U16],
    MOD: 0xFF/0x0D [R3, R2, R1],

    // Sets a bit in $flags to the lowest bit of operand.
    SETP: 0xF2/0x08 [FLAG, R2],
    SETP: 0xFA/0x08 [R1, R2],

    // Extracts an unsigned bitfield from the operand.
    EXTR: 0xC7/0x07 [R1, R2, BITR8],
    EXTR: 0xE7/0x07 [R1, R2, BITR16],
    EXTR: 0xFF/0x07 [R3, R2, R1],

    // Extracts a signed bitfield from the operand.
    EXTRS: 0xC3/0x03 [R1, R2, BITR8],
    EXTRS: 0xE3/0x03 [R1, R2, BITR16],
    EXTRS: 0xFF/0x03 [R3, R2, R1],

    // Masks a bitfield from source into destination.
    INS: 0xCB/0x0B [R1, R2, BITR8],
    INS: 0xEB/0x0B [R1, R2, BITR16],

    // Moves operand to destination register.
    MOV[EightBit]:     0x00/0x00 [R0, I8P1],
    MOV[SixteenBit]:   0x00/0x01 [R0, I16P1],
    MOV[ThirtyTwoBit]: 0x00/0x02 [R0, I24],
    MOV:               0xD0/0x00 [R0, U32],
    MOV:               0x32/0x02 [R1, R2],
    MOV:               0xFE/0x00 [SR2, R2],
    MOV:               0xFE/0x01 [R1, SR1],

    // Loads a value from DMEM to register.
    LD[EightBit]:     0x18/0x08 [R1, MEMRI8],
    LD[SixteenBit]:   0x18/0x08 [R1, MEMRI16],
    LD[ThirtyTwoBit]: 0x18/0x08 [R1, MEMRI32],
    LD[EightBit]:     0x34/0x00 [R2, MEMSPI8],
    LD[SixteenBit]:   0x34/0x00 [R2, MEMSPI16],
    LD[ThirtyTwoBit]: 0x34/0x00 [R2, MEMSPI32],
    LD[EightBit]:     0x3A/0x00 [R2, MEMSPR8],
    LD[SixteenBit]:   0x3A/0x00 [R2, MEMSPR16],
    LD[ThirtyTwoBit]: 0x3A/0x00 [R2, MEMSPR32],
    LD[EightBit]:     0x3C/0x08 [R3, MEMRR8],
    LD[SixteenBit]:   0x3C/0x08 [R3, MEMRR16],
    LD[ThirtyTwoBit]: 0x3C/0x08 [R3, MEMRR32],
    LD:               0x3F/0x0F [R1, MEMR],

    // Spills a value from register to DMEM.
    ST:               0x20/0x00 [MEMR, R1],
    ST[EightBit]:     0x21/0x01 [MEMSPR8, R2],
    ST[SixteenBit]:   0x21/0x01 [MEMSPR16, R2],
    ST[ThirtyTwoBit]: 0x21/0x01 [MEMSPR32, R2],
    ST[EightBit]:     0x30/0x01 [MEMSPI8, R2],
    ST[SixteenBit]:   0x30/0x01 [MEMSPI16, R2],
    ST[ThirtyTwoBit]: 0x30/0x01 [MEMSPI32, R2],
    ST[EightBit]:     0x35/0x05 [MEMRI8, R1],
    ST[SixteenBit]:   0x35/0x05 [MEMRI16, R1],
    ST[ThirtyTwoBit]: 0x35/0x05 [MEMRI32, R1],
    ST[EightBit]:     0x3C/0x09 [MEMRRALT8, R1],
    ST[SixteenBit]:   0x3C/0x09 [MEMRRALT16, R1],
    ST[ThirtyTwoBit]: 0x3C/0x09 [MEMRRALT32, R1],

    // Pushes an operand onto the stack.
    PUSH: 0xF9/0x00 [R2],

    // Pops an operand off the stack.
    POP: 0xFC/0x00 [R2],

    // Pushes all registers in range from $r0 to $rX onto the stack.
    MPUSH: 0xF9/0x02 [R2],

    // Pops all registers in range from $r0 to $rX off the stack.
    MPOP: 0xFB/0x00 [R2],

    // Performs MPOP and increments $sp by a given value.
    MPOPADD: 0xFB/0x04 [R2, I8],
    MPOPADD: 0xFB/0x02 [R2, I16],

    // Performs MPOP followed by RET.
    MPOPRET: 0xFB/0x01 [R2],

    // Performs MPOPADD followed by RET.
    MPOPADDRET: 0xFB/0x05 [R2, I8],
    MPOPADDRET: 0xFB/0x03 [R2, I16],

    // Jumps and pushes the return address onto the stack.
    CALL:           0xF4/0x21 [U8],
    CALL:           0xF3/0x03 [U16P1],
    CALL:           0xF9/0x05 [R2],
    LCALL[Unsized]: 0x3E/0x01 [U24],

    // Jumps to an absolute address.
    JMP: 0xF4/0x20 [U8],
    JMP: 0xF5/0x20 [U16],
    JMP: 0xF9/0x04 [R2],
    LJMP[Unsized]: 0x3E/0x00 [U24],

    // Jumps to PC-relative address when the predicate is fulfilled.
    BP: 0xF4/0x00 [PRED, PC8],
    BP: 0xF4/0x01 [PRED, PC8],
    BP: 0xF4/0x02 [PRED, PC8],
    BP: 0xF4/0x03 [PRED, PC8],
    BP: 0xF4/0x04 [PRED, PC8],
    BP: 0xF4/0x05 [PRED, PC8],
    BP: 0xF4/0x06 [PRED, PC8],
    BP: 0xF4/0x07 [PRED, PC8],
    BP: 0xF5/0x00 [PRED, PC16],
    BP: 0xF5/0x01 [PRED, PC16],
    BP: 0xF5/0x02 [PRED, PC16],
    BP: 0xF5/0x03 [PRED, PC16],
    BP: 0xF5/0x04 [PRED, PC16],
    BP: 0xF5/0x05 [PRED, PC16],
    BP: 0xF5/0x06 [PRED, PC16],
    BP: 0xF5/0x07 [PRED, PC16],

    // Jumps to PC-relative address when the carry bit is set.
    BC: 0xF4/0x08 [PC8],
    BC: 0xF5/0x08 [PC16],

    // Jumps to PC-relative address when the overflow bit is set.
    BO: 0xF4/0x09 [PC8],
    BO: 0xF5/0x09 [PC16],

    // Jumps to PC-relative address when the sign bit is set.
    BS: 0xF4/0x0A [PC8],
    BS: 0xF5/0x0A [PC16],

    // Jumps to PC-relative address when the zero bit is set.
    BZ: 0xF4/0x0B [PC8],
    BZ: 0xF5/0x0B [PC16],

    // Jumps to PC-relative address when the unsigned greater bit is set.
    BA: 0xF4/0x0C [PC8],
    BA: 0xF5/0x0C [PC16],

    // Jumps to PC-relative address when the unsigned smaller bit is set.
    BNA: 0xF4/0x0D [PC8],
    BNA: 0xF5/0x0D [PC16],

    // Jumps to PC-relative target unconditionally.
    BRA: 0xF4/0x0E [PC8],
    BRA: 0xF5/0x0E [PC16],

    // Jumps to PC-relative target when the given predicate is false.
    BNP: 0xF4/0x10 [PRED, PC8],
    BNP: 0xF4/0x11 [PRED, PC8],
    BNP: 0xF4/0x12 [PRED, PC8],
    BNP: 0xF4/0x13 [PRED, PC8],
    BNP: 0xF4/0x14 [PRED, PC8],
    BNP: 0xF4/0x15 [PRED, PC8],
    BNP: 0xF4/0x16 [PRED, PC8],
    BNP: 0xF4/0x17 [PRED, PC8],
    BNP: 0xF5/0x10 [PRED, PC16],
    BNP: 0xF5/0x11 [PRED, PC16],
    BNP: 0xF5/0x12 [PRED, PC16],
    BNP: 0xF5/0x13 [PRED, PC16],
    BNP: 0xF5/0x14 [PRED, PC16],
    BNP: 0xF5/0x15 [PRED, PC16],
    BNP: 0xF5/0x16 [PRED, PC16],
    BNP: 0xF5/0x17 [PRED, PC16],

    // Jumps to PC-relative address when the carry bit is not set.
    BNC: 0xF4/0x18 [PC8],
    BNC: 0xF5/0x18 [PC16],

    // Jumps to PC-relative address when the overflow bit is not set.
    BNO: 0xF4/0x19 [PC8],
    BNO: 0xF5/0x19 [PC16],

    // Jumps to PC-relative address when the sign bit is not set.
    BNS: 0xF4/0x1A [PC8],
    BNS: 0xF5/0x1A [PC16],

    // Jumps to PC-relative address when the zero bit is not set.
    BNZ: 0xF4/0x1B [PC8],
    BNZ: 0xF5/0x1B [PC16],

    // Jumps to PC-relative address when the signed greater bit is set.
    BG: 0xF4/0x1C [PC8],
    BG: 0xF5/0x1C [PC16],

    // Jumps to PC-relative address when the signed less or equal bit is set.
    BLE: 0xF4/0x1D [PC8],
    BLE: 0xF5/0x1D [PC16],

    // Jumps to PC-relative address when the signed less bit is set.
    BL: 0xF4/0x1E [PC8],
    BL: 0xF5/0x1E [PC16],

    // Jumps to PC-relative address when the signed greater or equal bit is set.
    BGE: 0xF4/0x1F [PC8],
    BGE: 0xF5/0x1F [PC16],

    // Jumps to a PC-relative address if two operands compare equal.
    BCMPE: 0x33/0x00 [R2, U8, PC8P3],
    BCMPE: 0x33/0x09 [R2, U8, PC16P3],
    BCMPE: 0x33/0x0A [R2, U16, PC8P4],
    BCMPE: 0x33/0x0B [R2, U16, PC16P4],

    // Jumps to a PC-relative address if two operands do not compare equal.
    BCMPNE: 0x33/0x04 [R2, U8, PC8P3],
    BCMPNE: 0x33/0x0D [R2, U8, PC16P3],
    BCMPNE: 0x33/0x0E [R2, U16, PC8P4],
    BCMPNE: 0x33/0x0F [R2, U16, PC16P4],

    // Returns from a previous call.
    RET: 0xF8/0x00,

    // Halts microcode execution and triggers exit interrupt.
    HALT: 0xF8/0x02,

    // Puts the processor to sleep until an unmasked interrupt occurs.
    SLEEP: 0xF4/0x28 [FLAG],

    // Loads the TLB entry for a given physical page.
    IMBLK: 0xFE/0x02 [R1, R2],

    // Loads the TLB entry for a given virtual address.
    IMTAG: 0xFE/0x03 [R1, R2],

    // Invalidates a non-secret TLB entry for a physical page.
    IMINV: 0xF9/0x08 [R2],

    // Returns from an interrupt handler.
    IRET: 0xF8/0x01,

    // Triggers a software interrupt.
    TRAP: 0xF8/0x08 [TRAP],

    // Submits a DMA request to load code from external memory.
    IMLD: 0xFA/0x04 [R2, R1],

    // Submits a DMA request to load data from external memory.
    DMLD: 0xFA/0x05 [R2, R1],

    // Submits a DMA request to store local data in external memory.
    DMST: 0xFA/0x06 [R2, R1],

    // Waits for all DMA code loads to complete.
    IMWAIT: 0xF8/0x07,

    // Waits for all DMA data loads/stores to complete.
    DMWAIT: 0xF8/0x03,

    // Constructs a memory barrier for DMA transfers.
    DMFENCE: 0xF8/0x06,

    // Asynchronously writes to the I/O space.
    IOWR: 0xF6/0x06 [IORI, R1],
    IOWR: 0xFA/0x00 [IOR, R1],

    // Synchronously writes to the I/O space.
    IOWRS: 0xF7/0x07 [IORI, R1],
    IOWRS: 0xFA/0x01 [IOR, R2],

    // Asynchronously reads from the I/O space.
    IORD: 0xCF/0x0F [R1, IORI],
    IORD: 0xFF/0x0F [R3, IORR],

    // Synchronously reads from the I/O space.
    IORDS: 0xCE/0x0E [R1, IORI],
    IORDS: 0xFF/0x0E [R3, IORR],
];

/// Tries to decode an [`Instruction`] from the `code` bytes.
///
/// On success, `pc` will be incremented by the number of bytes
/// consumed from the start of the code slice.
pub fn decode(pc: &mut u32, code: &[u8]) -> Option<Instruction> {
    let insn = utils::load_le(code);

    for entry in &INSTRUCTION_TABLE {
        let read_size = entry.required_read_size();

        if code.len() < read_size {
            continue;
        }

        if !entry.match_bytes(insn) {
            continue;
        }

        *pc += read_size as u32;

        let operands = entry.operands.each_ref().map(|op| op.decode(insn));
        return Some(Instruction {
            size: entry.size(insn),
            opcode: entry.opcode,
            operands,
        });
    }

    None
}
