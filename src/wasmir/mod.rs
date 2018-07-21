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
pub struct Funcidx(u32);

impl Funcidx {
    pub fn new(idx: u32) -> Funcidx {
        Funcidx(idx)
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
    I32,
    I64,
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

pub enum Itestop {
    Eqz32,
}

pub enum Irelop {
    Eq32,
}

pub enum WasmInstr {
    Const(Const),
    Ibinop(Ibinop),
    Itestop(Itestop),
    Irelop(Irelop),
    Block(Resulttype, Vec<WasmInstr>),
    If(Resulttype, Vec<WasmInstr>, Vec<WasmInstr>),
    Loop(Resulttype, Vec<WasmInstr>),
    Br(usize),
    BrIf(usize),
    Return,
    GetLocal(Localidx),
    SetLocal(Localidx),
    TeeLocal(Localidx),
    Call(Funcidx),
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
    locals: Vec<Valtype>,
    body: Expr,
}

impl Func {
    pub fn new(typ: Typeidx, locals: Vec<Valtype>, body: Expr) -> Func {
        Func { typ, locals, body }
    }

    pub fn get_type(&self) -> &Typeidx {
        &self.typ
    }

    pub fn get_locals(&self) -> &Vec<Valtype> {
        &self.locals
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
