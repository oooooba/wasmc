#[derive(Clone, Copy)]
pub struct Typeidx(u32);

impl Typeidx {
    pub fn new(idx: u32) -> Typeidx {
        Typeidx(idx)
    }

    pub fn as_index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy)]
pub struct Localidx(u32);

impl Localidx {
    pub fn new(idx: u32) -> Localidx {
        Localidx(idx)
    }

    pub fn as_index(&self) -> usize {
        self.0 as usize
    }
}

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

pub struct Functype(Vec<Valtype>, Vec<Valtype>);

impl Functype {
    pub fn new(t_in: Vec<Valtype>, t_out: Vec<Valtype>) -> Functype {
        Functype(t_in, t_out)
    }

    pub fn peek_in_typ(&self) -> &Vec<Valtype> {
        &self.0
    }

    pub fn peek_out_typ(&self) -> &Vec<Valtype> {
        &self.1
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
    Return,
    GetLocal(Localidx),
}

pub struct Expr(Vec<WasmInstr>);

impl Expr {
    pub fn new(expr: Vec<WasmInstr>) -> Expr {
        Expr(expr)
    }

    pub fn get_instr_sequences(&self) -> &Vec<WasmInstr> {
        &self.0
    }
}

pub struct Func {
    typ: Typeidx,
    _locals: Vec<()>,
    body: Expr,
}

impl Func {
    pub fn new(typ: Typeidx, _locals: Vec<()>, body: Expr) -> Func {
        Func { typ, _locals, body }
    }

    pub fn get_type(&self) -> &Typeidx {
        &self.typ
    }

    pub fn get_body(&self) -> &Expr {
        &self.body
    }
}

pub struct Module {
    types: Vec<Functype>,
    funcs: Vec<Func>,
}

impl Module {
    pub fn new(types: Vec<Functype>, funcs: Vec<Func>) -> Module {
        Module {
            types: types,
            funcs,
        }
    }

    pub fn get_types(&self) -> &Vec<Functype> {
        &self.types
    }

    pub fn get_funcs(&self) -> &Vec<Func> {
        &self.funcs
    }
}
