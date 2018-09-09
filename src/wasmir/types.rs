#[derive(Debug, Clone)]
pub enum Valtype {
    I32,
    I64,
}

#[derive(Debug)]
pub struct Resulttype(Option<Vec<Valtype>>);

impl Resulttype {
    pub fn new(t: Option<Vec<Valtype>>) -> Resulttype {
        Resulttype(t)
    }
    pub fn peek(&self) -> &Option<Vec<Valtype>> {
        &self.0
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Limits {
    min: u32,
    max: Option<u32>,
}

impl Limits {
    pub fn new(min: u32, max: Option<u32>) -> Limits {
        Limits { min, max }
    }

    pub fn get_min(&self) -> u32 {
        self.min
    }

    pub fn get_max(&self) -> &Option<u32> {
        &self.max
    }
}

#[derive(Debug)]
pub struct Memtype {
    lim: Limits,
}

impl Memtype {
    pub fn new(lim: Limits) -> Memtype {
        Memtype { lim }
    }

    pub fn get_lim(&self) -> &Limits {
        &self.lim
    }
}

#[derive(Debug)]
pub enum Elemtype {
    Anyfunc,
}

#[derive(Debug)]
pub struct Tabletype {
    limits: Limits,
    elemtype: Elemtype,
}

impl Tabletype {
    pub fn new(limits: Limits, elemtype: Elemtype) -> Tabletype {
        Tabletype { limits, elemtype }
    }

    pub fn get_limits(&self) -> &Limits {
        &self.limits
    }
}

#[derive(Debug)]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug)]
pub struct Globaltype {
    mutability: Mut,
    valtype: Valtype,
}

impl Globaltype {
    pub fn new(mutability: Mut, valtype: Valtype) -> Globaltype {
        Globaltype {
            mutability,
            valtype,
        }
    }

    pub fn mutability(&self) -> &Mut {
        &self.mutability
    }

    pub fn valtype(&self) -> &Valtype {
        &self.valtype
    }
}
