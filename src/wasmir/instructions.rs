use std::fmt;

use wasmir::types::{Resulttype, Valtype};
use wasmir::{Funcidx, Globalidx, Labelidx, Localidx, Typeidx};

#[derive(Debug)]
pub enum Const {
    I32(u32),
    I64(u64),
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Const::*;
        match self {
            &I32(i) => write!(f, "Const::I32({})", i),
            &I64(i) => write!(f, "Const::I64({})", i),
        }
    }
}

#[derive(Debug)]
pub enum Iunop {
    Clz32,
    Ctz32,
}

impl fmt::Display for Iunop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Iunop::*;
        match self {
            &Clz32 => write!(f, "Iunop::Clz32"),
            &Ctz32 => write!(f, "Iunop::Ctz32"),
        }
    }
}

#[derive(Debug)]
pub enum Ibinop {
    Add32,
    Sub32,
    Mul32,
    Mul64,
    DivU32,
    And32,
    Or32,
    Xor32,
    Shl32,
    ShrS32,
    ShrU32,
    ShrU64,
}

impl fmt::Display for Ibinop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Ibinop::*;
        match self {
            &Add32 => write!(f, "Ibinop::Add32"),
            &Sub32 => write!(f, "Ibinop::Sub32"),
            &Mul32 => write!(f, "Ibinop::Mul32"),
            &Mul64 => write!(f, "Ibinop::Mul64"),
            &DivU32 => write!(f, "Ibinop::DivU32"),
            &And32 => write!(f, "Ibinop::And32"),
            &Or32 => write!(f, "Ibinop::Or32"),
            &Xor32 => write!(f, "Ibinop::Xor32"),
            &Shl32 => write!(f, "Ibinop::Shl32"),
            &ShrS32 => write!(f, "Ibinop::ShrS32"),
            &ShrU32 => write!(f, "Ibinop::ShrU32"),
            &ShrU64 => write!(f, "Ibinop::ShrU64"),
        }
    }
}

#[derive(Debug)]
pub enum Itestop {
    Eqz32,
}

impl fmt::Display for Itestop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Itestop::*;
        match self {
            &Eqz32 => write!(f, "Itestop::Eqz32"),
        }
    }
}

#[derive(Debug)]
pub enum Irelop {
    Eq32,
    Ne32,
    LtS32,
    LtU32,
    GtS32,
    GtU32,
    LeS32,
    LeU32,
    GeU32,
}

impl fmt::Display for Irelop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Irelop::*;
        match self {
            &Eq32 => write!(f, "Irelop::Eq32"),
            &Ne32 => write!(f, "Irelop::Ne32"),
            &LtS32 => write!(f, "Irelop::LtS32"),
            &LtU32 => write!(f, "Irelop::LtU32"),
            &GtS32 => write!(f, "Irelop::GtS32"),
            &GtU32 => write!(f, "Irelop::GtU32"),
            &LeS32 => write!(f, "Irelop::LeS32"),
            &LeU32 => write!(f, "Irelop::LeU32"),
            &GeU32 => write!(f, "Irelop::GeU32"),
        }
    }
}

#[derive(Debug)]
pub enum Cvtop {
    Wrap,
    ExtendU,
    ExtendS,
}

impl fmt::Display for Cvtop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Cvtop::*;
        match self {
            &Wrap => write!(f, "Cvtop::Wrap"),
            &ExtendU => write!(f, "Cvtop::ExtendU"),
            &ExtendS => write!(f, "Cvtop::ExtendS"),
        }
    }
}

#[derive(Debug)]
pub struct Memarg {
    offset: u32,
    align: u32,
}

impl fmt::Display for Memarg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Memarg {{ offset: {}, align: {} }}",
            self.offset, self.align
        )
    }
}

impl Memarg {
    pub fn new(offset: u32, align: u32) -> Memarg {
        Memarg { offset, align }
    }

    pub fn get_offset(&self) -> u32 {
        self.offset
    }
}

#[derive(Debug)]
pub enum Loadattr {
    I32,
    I64,
    I32x8S,
    I32x8U,
}

impl fmt::Display for Loadattr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Loadattr::*;
        match self {
            &I32 => write!(f, "Loadattr::I32"),
            &I64 => write!(f, "Loadattr::I64"),
            &I32x8S => write!(f, "Loadattr::I32x8S"),
            &I32x8U => write!(f, "Loadattr::I32x8U"),
        }
    }
}

#[derive(Debug)]
pub enum Storeattr {
    I32,
    I64,
    I32x8,
}

impl fmt::Display for Storeattr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Storeattr::*;
        match self {
            &I32 => write!(f, "Storeattr::I32"),
            &I64 => write!(f, "Storeattr::I64"),
            &I32x8 => write!(f, "Storeattr::I32x8"),
        }
    }
}

#[derive(Debug)]
pub enum WasmInstr {
    Const(Const),
    Iunop(Iunop),
    Ibinop(Ibinop),
    Itestop(Itestop),
    Irelop(Irelop),
    Cvtop {
        op: Cvtop,
        dst_type: Valtype,
        src_type: Valtype,
    },
    Unreachable,
    Block(Resulttype, Vec<WasmInstr>),
    If(Resulttype, Vec<WasmInstr>, Vec<WasmInstr>),
    Loop(Resulttype, Vec<WasmInstr>),
    Br(Labelidx),
    BrIf(Labelidx),
    BrTable {
        table: Vec<Labelidx>,
        default: Labelidx,
    },
    Return,
    GetLocal(Localidx),
    SetLocal(Localidx),
    TeeLocal(Localidx),
    GetGlobal(Globalidx),
    SetGlobal(Globalidx),
    Load {
        attr: Loadattr,
        arg: Memarg,
    },
    Store {
        attr: Storeattr,
        arg: Memarg,
    },
    Call(Funcidx),
    CallIndirect(Typeidx),
    Drop,
    Select,
}

#[derive(Debug)]
pub struct Expr(Vec<WasmInstr>);

impl Expr {
    pub fn new(expr: Vec<WasmInstr>) -> Expr {
        Expr(expr)
    }

    pub fn get_instr_sequences(&self) -> &Vec<WasmInstr> {
        &self.0
    }
}
