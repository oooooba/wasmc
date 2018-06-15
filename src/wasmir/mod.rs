pub enum Valtype {
    U32,
}

pub struct Resulttype(Option<Vec<Valtype>>);

impl Resulttype {
    pub fn new(t: Option<Vec<Valtype>>) -> Resulttype {
        Resulttype(t)
    }
    pub fn peek(&self) -> &Option<Vec<Valtype>> {
        &self.0
    }
}

pub enum Const {
    I32(u32),
}

pub enum Ibinop {
    Add32,
    Sub32,
}

pub enum Binop {
    Ibinop(Ibinop),
}

pub enum WasmInstr {
    Const(Const),
    Binop(Binop),
    Block(Resulttype, Vec<WasmInstr>),
    If(Resulttype, Vec<WasmInstr>, Vec<WasmInstr>),
    Loop(Resulttype, Vec<WasmInstr>),
    Br(usize),
    BrIf(usize),
}
