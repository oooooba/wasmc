pub mod instructions;
pub mod types;

use wasmir::instructions::Expr;
use wasmir::types::{Functype, Globaltype, Tabletype, Valtype};

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
pub struct Tableidx(u32);

impl Tableidx {
    pub fn new(idx: u32) -> Tableidx {
        Tableidx(idx)
    }

    pub fn as_index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Memidx(u32);

impl Memidx {
    pub fn new(idx: u32) -> Memidx {
        Memidx(idx)
    }

    pub fn as_index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Globalidx(u32);

impl Globalidx {
    pub fn new(idx: u32) -> Globalidx {
        Globalidx(idx)
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

pub struct Table {
    typ: Tabletype,
}

impl Table {
    pub fn new(typ: Tabletype) -> Table {
        Table { typ }
    }
}

#[derive(Debug)]
pub enum Exportdesc {
    Func(Funcidx),
    Table(Tableidx),
    Mem(Memidx),
    Global(Globalidx),
}

#[derive(Debug)]
pub struct Export {
    name: String,
    desc: Exportdesc,
}

impl Export {
    pub fn new(name: String, desc: Exportdesc) -> Export {
        Export { name, desc }
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
