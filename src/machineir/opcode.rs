use std::fmt;

use context::handle::{FunctionHandle, RegisterHandle};
use machineir::operand::Operand;
use machineir::typ::Type;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOpKind {
    Const,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, PartialEq, Eq)]
pub enum JumpCondKind {
    Unconditional,
    Eq0(RegisterHandle),
    Neq0(RegisterHandle),
    Neq(RegisterHandle, RegisterHandle),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Debug(String),
    Label(String),
    Copy { typ: Type, dst: Operand, src: Operand },
    UnaryOp { typ: Type, kind: UnaryOpKind, dst: Operand, src: Operand },
    BinaryOp { typ: Type, kind: BinaryOpKind, dst: Operand, src1: Operand, src2: Operand },
    Load { typ: Type, dst: Operand, src: Operand },
    Store { typ: Type, dst: Operand, src: Operand },
    Jump { kind: JumpCondKind, target: Operand },
    Call { func: FunctionHandle, typ: Type, result: Option<Operand>, args: Vec<Operand> },
    Return { typ: Type, result: Option<Operand> },
}

impl Opcode {
    pub fn print(&self) {
        match self {
            _ => unimplemented!(),
        }
    }
}

macro_rules! format {
    (0)=>("{:<8}");
    (1)=>("{:<8}{}");
    (2)=>("{:<8}{}, {}");
    (3)=>("{:<8}{}, {}, {}");
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Opcode::*;
        match self {
            &Debug(ref msg) => write!(f, format!(1), "debug", msg),
            &Label(ref label) => write!(f, format!(1), "label", label),
            &Copy { .. } => unimplemented!(),
            &UnaryOp { .. } => unimplemented!(),
            &BinaryOp { .. } => unimplemented!(),
            &Load { .. } => unimplemented!(),
            &Store { .. } => unimplemented!(),
            &Jump { .. } => unimplemented!(),
            &Call { .. } => unimplemented!(),
            &Return { .. } => unimplemented!(),
        }
    }
}
