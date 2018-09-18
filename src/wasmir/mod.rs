pub mod instructions;
pub mod types;

use wasmir::instructions::Expr;
use wasmir::types::{Functype, Globaltype, Memtype, Tabletype, Valtype};

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

#[derive(Debug, Clone, Copy)]
pub struct Labelidx(u32);

impl Labelidx {
    pub fn new(idx: u32) -> Labelidx {
        Labelidx(idx)
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

    pub fn get_type(&self) -> &Globaltype {
        &self.typ
    }

    pub fn get_init(&self) -> &Expr {
        &self.init
    }
}

#[derive(Debug)]
pub struct Elem {
    table: Tableidx,
    offset: Expr,
    init: Vec<Funcidx>,
}

impl Elem {
    pub fn new(table: Tableidx, offset: Expr, init: Vec<Funcidx>) -> Elem {
        Elem {
            table,
            offset,
            init,
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Table {
    typ: Tabletype,
}

impl Table {
    pub fn new(typ: Tabletype) -> Table {
        Table { typ }
    }

    pub fn get_type(&self) -> &Tabletype {
        &self.typ
    }
}

#[derive(Debug)]
pub struct Mem {
    typ: Memtype,
}

impl Mem {
    pub fn new(typ: Memtype) -> Mem {
        Mem { typ }
    }

    pub fn get_type(&self) -> &Memtype {
        &self.typ
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

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn get_desc(&self) -> &Exportdesc {
        &self.desc
    }
}

#[derive(Debug)]
pub enum Importdesc {
    Func(Typeidx),
    Table(Tabletype),
    Mem(Memtype),
    Global(Globaltype),
}

#[derive(Debug)]
pub struct Import {
    module: String,
    name: String,
    desc: Importdesc,
}

impl Import {
    pub fn new(module: String, name: String, desc: Importdesc) -> Import {
        Import { module, name, desc }
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn get_desc(&self) -> &Importdesc {
        &self.desc
    }
}

#[derive(Debug)]
pub struct Data {
    data: Memidx,
    offset: Expr,
    init: Vec<u8>,
}

impl Data {
    pub fn new(data: Memidx, offset: Expr, init: Vec<u8>) -> Data {
        Data { data, offset, init }
    }

    pub fn get_data(&self) -> Memidx {
        self.data
    }

    pub fn get_offset(&self) -> &Expr {
        &self.offset
    }

    pub fn get_init(&self) -> &Vec<u8> {
        &self.init
    }
}

#[derive(Debug)]
pub struct Module {
    types: Vec<Functype>,
    funcs: Vec<Func>,
    tables: Vec<Table>,
    mems: Vec<Mem>,
    globals: Vec<Global>,
    exports: Vec<Export>,
    imports: Vec<Import>,
    data: Vec<Data>,
}

impl Module {
    pub fn new(
        types: Vec<Functype>,
        funcs: Vec<Func>,
        tables: Vec<Table>,
        mems: Vec<Mem>,
        globals: Vec<Global>,
        exports: Vec<Export>,
        imports: Vec<Import>,
        data: Vec<Data>,
    ) -> Module {
        Module {
            types,
            funcs,
            tables,
            mems,
            globals,
            exports,
            imports,
            data,
        }
    }

    pub fn get_types(&self) -> &Vec<Functype> {
        &self.types
    }

    pub fn get_funcs(&self) -> &Vec<Func> {
        &self.funcs
    }

    pub fn get_tables(&self) -> &Vec<Table> {
        &self.tables
    }

    pub fn get_mems(&self) -> &Vec<Mem> {
        &self.mems
    }

    pub fn get_globals(&self) -> &Vec<Global> {
        &self.globals
    }

    pub fn get_exports(&self) -> &Vec<Export> {
        &self.exports
    }

    pub fn get_imports(&self) -> &Vec<Import> {
        &self.imports
    }

    pub fn get_data(&self) -> &Vec<Data> {
        &self.data
    }
}
