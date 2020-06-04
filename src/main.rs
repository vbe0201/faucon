mod disassembler;

fn main() {
    // fd: O3 R2SD R1S
    // and $r0 $r9
    // 0xfd, 0x9, 0x4
    let instruction = [0xfd, 0x9, 0x4];

    println!(
        "Operand size: {:?}",
        disassembler::OperandSize::from(instruction[0])
    );
    println!(
        "Subopcode: {:?}",
        disassembler::extract_subopcode(&instruction, disassembler::SubopcodeLocation::O3)
    );
    //println!("Operand 1 (R2SD): {:?}", );
    println!("Operand 2 (R1S): {:?}", instruction[1] & 0xF);
}
