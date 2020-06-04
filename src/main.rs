mod disassembler;

fn main() {
    println!("{:?}", disassembler::OperandSize::from(0x3a));
}
