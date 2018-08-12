pub mod instructions;
pub mod types;

use wasmir::instructions::Expr;
use wasmir::types::{Functype, Globaltype, Valtype};

#[derive(Debug, Clone, Copy)]
pub struct Typeidx(u32);

impl Typeidx {
    pub fn new(idx: u32) -> Typeidx {
        Typeidx(idx)
    }

    pub fn as_index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Funcidx(u32);

impl Funcidx {
    pub fn new(idx: u32) -> Funcidx {
        Funcidx(idx)
    }

    pub fn as_index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Localidx(u32);

impl Localidx {
    pub fn new(idx: u32) -> Localidx {
        Localidx(idx)
    }

    pub fn as_index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct Global {
    typ: Globaltype,
    init: Expr,
}

impl Global {
    pub fn new(typ: Globaltype, init: Expr) -> Global {
        Global { typ, init }
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
