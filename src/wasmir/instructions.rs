use wasmir::{Funcidx, Labelidx, Localidx};
use wasmir::types::{Resulttype, Valtype};

#[derive(Debug)]
pub enum Const {
    I32(u32),
    I64(u64),
}

#[derive(Debug)]
pub enum Ibinop {
    Add32,
    Sub32,
    Mul32,
    Mul64,
    Shl32,
    ShrU32,
    ShrU64,
}

#[derive(Debug)]
pub enum Itestop {
    Eqz32,
}

#[derive(Debug)]
pub enum Irelop {
    Eq32,
    LtS32,
}

#[derive(Debug)]
pub enum Cvtop {
    Wrap,
    ExtendU,
    ExtendS,
}

#[derive(Debug)]
pub struct Memarg {
    offset: u32,
    align: u32,
}

impl Memarg {
    pub fn new(offset: u32, align: u32) -> Memarg {
        Memarg { offset, align }
    }
}

#[derive(Debug)]
pub enum Loadattr {
    I32,
}

#[derive(Debug)]
pub enum WasmInstr {
    Const(Const),
    Ibinop(Ibinop),
    Itestop(Itestop),
    Irelop(Irelop),
    Cvtop { op: Cvtop, dst_type: Valtype, src_type: Valtype },
    Block(Resulttype, Vec<WasmInstr>),
    If(Resulttype, Vec<WasmInstr>, Vec<WasmInstr>),
    Loop(Resulttype, Vec<WasmInstr>),
    Br(Labelidx),
    BrIf(Labelidx),
    Return,
    GetLocal(Localidx),
    SetLocal(Localidx),
    TeeLocal(Localidx),
    Load { attr: Loadattr, arg: Memarg },
    Call(Funcidx),
    Drop,
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

