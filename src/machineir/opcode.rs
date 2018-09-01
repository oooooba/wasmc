use std::fmt;

use context::handle::{FunctionHandle, RegisterHandle};
use machineir::operand::Operand;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOpKind {
    Const,
    Wrap,
    ZeroExtension,
    SignExtension,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Sar,
}

#[derive(Debug, PartialEq, Eq)]
pub enum JumpCondKind {
    Unconditional,
    Eq0(RegisterHandle),
    Neq0(RegisterHandle),
    Eq(RegisterHandle, RegisterHandle),
    Neq(RegisterHandle, RegisterHandle),
    LtU(RegisterHandle, RegisterHandle),
    LeU(RegisterHandle, RegisterHandle),
    GtS(RegisterHandle, RegisterHandle),
    GeS(RegisterHandle, RegisterHandle),
    GeU(RegisterHandle, RegisterHandle),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OffsetKind {
    None,
    Register(RegisterHandle),
}

impl OffsetKind {
    pub fn print(&self) {
        use self::OffsetKind::*;
        match self {
            &None => print!("none"),
            &Register(reg) => reg.print(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OpOperandKind {
    Register(RegisterHandle),
    ConstI32(u32),
    ConstI64(u64),
}

impl OpOperandKind {
    fn print(&self) {
        use self::OpOperandKind::*;
        match self {
            &Register(reg) => reg.print(),
            &ConstI32(n) => print!("{}", n),
            &ConstI64(n) => print!("{}", n),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Debug(String),
    Label(String),
    Copy {
        dst: RegisterHandle,
        src: RegisterHandle,
    },
    UnaryOp {
        kind: UnaryOpKind,
        dst: Operand,
        src: Operand,
    },
    BinaryOp {
        kind: BinaryOpKind,
        dst: RegisterHandle,
        src1: RegisterHandle,
        src2: OpOperandKind,
    },
    Load {
        dst: Operand,
        src_base: Operand,
        src_offset: OffsetKind,
    },
    Store {
        dst_base: Operand,
        dst_offset: OffsetKind,
        src: Operand,
    },
    Jump {
        kind: JumpCondKind,
        target: Operand,
    },
    Call {
        func: FunctionHandle,
        result: Option<Operand>,
        args: Vec<Operand>,
    },
    Return {
        result: Option<Operand>,
    },
}

impl Opcode {
    pub fn print(&self) {
        use self::Opcode::*;
        match self {
            &Debug(ref msg) => {
                print!("debug");
                print!("\t");
                print!("{}", msg);
            }
            &Label(ref label) => {
                print!("label");
                print!("\t");
                print!("{}", label);
            }
            &Copy {
                ref dst, ref src, ..
            } => {
                dst.print();
                print!(" = ");
                print!("copy");
                print!(" ");
                src.print();
            }
            &UnaryOp {
                ref kind,
                ref dst,
                ref src,
                ..
            } => {
                dst.print();
                print!(" = ");
                print!("unary<{:?}>", kind);
                print!(" ");
                src.print();
            }
            &BinaryOp {
                ref kind,
                ref dst,
                ref src1,
                ref src2,
                ..
            } => {
                dst.print();
                print!(" = ");
                print!("binary<{:?}>", kind);
                print!(" ");
                src1.print();
                print!(", ");
                src2.print();
            }
            &Load {
                ref dst,
                ref src_base,
                ref src_offset,
            } => {
                dst.print();
                print!(" = ");
                print!("load");
                print!(" ");
                src_base.print();
                print!(", ");
                src_offset.print();
            }
            &Store {
                ref dst_base,
                ref dst_offset,
                ref src,
            } => {
                dst_base.print();
                print!(", ");
                dst_offset.print();
                print!(" = ");
                print!("store");
                print!(" ");
                src.print();
            }
            &Jump {
                ref kind,
                ref target,
            } => {
                print!("jump<{:?}>", kind);
                print!(" ");
                target.print();
            }
            &Call {
                ref func,
                ref result,
                ref args,
                ..
            } => {
                if let &Some(ref res) = result {
                    res.print();
                    print!(" = ");
                }
                print!("call");
                print!(" ");
                print!("{}", func.get_func_name());
                args.iter().for_each(|arg| {
                    print!(", ");
                    arg.print();
                });
            }
            &Return { ref result, .. } => {
                print!("return");
                if let &Some(ref res) = result {
                    print!(" ");
                    res.print();
                }
            }
        }
    }
}

macro_rules! format {
    (0) => {
        "{:<8}"
    };
    (1) => {
        "{:<8}{}"
    };
    (2) => {
        "{:<8}{}, {}"
    };
    (3) => {
        "{:<8}{}, {}, {}"
    };
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
