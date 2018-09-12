use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle, RegisterHandle};
use machineir::typ::Type;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CastKind {
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
    LtS(RegisterHandle, RegisterHandle),
    LtU(RegisterHandle, RegisterHandle),
    LeS(RegisterHandle, RegisterHandle),
    LeU(RegisterHandle, RegisterHandle),
    GtS(RegisterHandle, RegisterHandle),
    GtU(RegisterHandle, RegisterHandle),
    GeS(RegisterHandle, RegisterHandle),
    GeU(RegisterHandle, RegisterHandle),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum JumpTargetKind {
    BasicBlock(BasicBlockHandle),
}

impl JumpTargetKind {
    fn print(&self) {
        use self::JumpTargetKind::*;
        match self {
            &BasicBlock(bb) => bb.print(),
        }
    }
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
pub enum Address {
    Var(RegisterHandle),
    RegBaseRegOffset {
        base: RegisterHandle,
        offset: RegisterHandle,
    },
    RegBaseRegIndex {
        base: RegisterHandle,
        index: RegisterHandle,
        scale: Type,
    },
}

impl Address {
    pub fn print(&self) {
        use self::Address::*;
        print!("[");
        match self {
            &Var(reg) => {
                reg.print();
            }
            &RegBaseRegOffset { base, offset } => {
                base.print();
                print!(" + ");
                offset.print();
            }
            &RegBaseRegIndex {
                base,
                index,
                ref scale,
            } => {
                base.print();
                print!(" + ");
                index.print();
                print!(" * ");
                print!("{}", scale.get_ptr_notation());
            }
        }
        print!("]");
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ConstKind {
    ConstI8(u8),
    ConstI32(u32),
    ConstI64(u64),
}

impl ConstKind {
    fn print(&self) {
        use self::ConstKind::*;
        match self {
            &ConstI8(i) => print!("{}", i),
            &ConstI32(i) => print!("{}", i),
            &ConstI64(i) => print!("{}", i),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OperandKind {
    Register(RegisterHandle),
    ImmI8(u8),
    ImmI32(u32),
    ImmI64(u64),
}

impl OperandKind {
    fn print(&self) {
        use self::OperandKind::*;
        match self {
            &Register(reg) => reg.print(),
            &ImmI8(n) => print!("{}", n),
            &ImmI32(n) => print!("{}", n),
            &ImmI64(n) => print!("{}", n),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CallTargetKind {
    Function(FunctionHandle),
    Indirect(Address),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Debug(String),
    Label(String),
    Copy {
        dst: RegisterHandle,
        src: RegisterHandle,
    },
    Const {
        dst: RegisterHandle,
        src: ConstKind,
    },
    Cast {
        kind: CastKind,
        dst: RegisterHandle,
        src: RegisterHandle,
    },
    BinaryOp {
        kind: BinaryOpKind,
        dst: RegisterHandle,
        src1: RegisterHandle,
        src2: OperandKind,
    },
    Load {
        dst: RegisterHandle,
        src: Address,
    },
    Store {
        dst: Address,
        src: RegisterHandle,
    },
    Jump {
        kind: JumpCondKind,
        target: JumpTargetKind,
    },
    Call {
        func: CallTargetKind,
        result: Option<RegisterHandle>,
        args: Vec<RegisterHandle>,
    },
    Return {
        result: Option<RegisterHandle>,
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
            &Const { dst, ref src, .. } => {
                dst.print();
                print!(" = ");
                print!("const");
                print!(" ");
                src.print();
            }
            &Cast {
                ref kind, dst, src, ..
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
            &Load { dst, ref src } => {
                dst.print();
                print!(" = ");
                print!("load");
                print!(" ");
                src.print();
            }
            &Store { ref dst, src } => {
                dst.print();
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
                match func {
                    &CallTargetKind::Function(f) => print!("{}", f.get_func_name()),
                    &CallTargetKind::Indirect(ref addr) => addr.print(),
                }
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
            &Const { .. } => unimplemented!(),
            &Cast { .. } => unimplemented!(),
            &BinaryOp { .. } => unimplemented!(),
            &Load { .. } => unimplemented!(),
            &Store { .. } => unimplemented!(),
            &Jump { .. } => unimplemented!(),
            &Call { .. } => unimplemented!(),
            &Return { .. } => unimplemented!(),
        }
    }
}
