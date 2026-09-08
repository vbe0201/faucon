use crate::{
    assemble::{SourceSpan, Spanned},
    isa,
};

/// Representation of a fully parsed input program.
///
/// This is the Abstract Syntax Tree of the assembler. It is
/// produced by the parsing stage and all subsequent passes
/// will operate on the AST directly. For that purpose, the
/// nodes include optional fields which are filled on demand.
///
/// Note that the AST intentionally does not accomodate any
/// form of error nodes; the parsing stage only returns a
/// valid AST or a stream of errors instead.
#[derive(Clone, Debug)]
pub struct Program {
    /// The statements in the program.
    pub statements: Vec<Statement>,
}

/// Represents statements that form an assembler program.
#[derive(Clone, Debug)]
pub enum Statement {
    /// A label definition.
    Label(SourceSpan),
    /// A processor instruction.
    Instruction(Instruction),
    /// An assembler directive.
    Directive(Directive),
}

/// A parsed assembler directive in the source program.
#[derive(Clone, Debug)]
pub struct Directive {
    /// The name of the directives.
    pub name: SourceSpan,
    /// The operands to the directive.
    pub operands: [Option<Spanned<Value>>; 3],
}

/// A parsed instruction in the source program.
#[derive(Clone, Debug)]
pub struct Instruction {
    /// The instruction mnemonic.
    pub mnemonic: isa::Opcode,
    /// The operand size mode.
    pub size: isa::OperandSize,
    /// The instruction operands, if any.
    pub operands: [Option<Spanned<Value>>; 3],
}

/// A value in the source program, serving as an operand for
/// instructions and directives.
#[derive(Clone, Debug)]
pub enum Value {
    /// Represents an empty value.
    Empty,
    /// A symbolic reference.
    Label(SourceSpan),
    /// An integer, signed or unsigned.
    Int(isa::Immediate),
    /// A string literal.
    Str(SourceSpan),
    /// A reigster literal.
    Reg(isa::Register),
    /// A flag literal.
    Flag(isa::Flag),
    /// A memory access description.
    Mem(isa::MemoryAccess),
    /// A bit range specification.
    Bits(isa::BitRange),
}
