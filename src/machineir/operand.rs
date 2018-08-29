use std::fmt;

use context::handle::{BasicBlockHandle, RegisterHandle};
use machineir::typ::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MemoryKind {
    Local,
    StaticGlobal,
    DynamicGlobal { min: usize, max: Option<usize> },
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum OperandKind {
    Register(RegisterHandle),
    PhysicalRegister(RegisterHandle),
    ConstI32(u32),
    ConstI64(u64),
    Label(BasicBlockHandle),
    Memory {
        index: usize,
        typ: Type,
        kind: MemoryKind,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Operand {
    kind: OperandKind,
}

impl Operand {
    pub fn new_register(handle: RegisterHandle) -> Operand {
        Operand {
            kind: OperandKind::Register(handle),
        }
    }

    pub fn new_physical_register(handle: RegisterHandle) -> Operand {
        Operand {
            kind: OperandKind::PhysicalRegister(handle),
        }
    }

    pub fn new_const_i32(n: u32) -> Operand {
        Operand {
            kind: OperandKind::ConstI32(n),
        }
    }

    pub fn new_const_i64(n: u64) -> Operand {
        Operand {
            kind: OperandKind::ConstI64(n),
        }
    }

    pub fn new_label(handle: BasicBlockHandle) -> Operand {
        Operand {
            kind: OperandKind::Label(handle),
        }
    }

    pub fn new_memory(index: usize, typ: Type, kind: MemoryKind) -> Operand {
        Operand {
            kind: OperandKind::Memory { index, typ, kind },
        }
    }

    pub fn get_kind(&self) -> &OperandKind {
        &self.kind
    }

    pub fn get_kind_mut(&mut self) -> &mut OperandKind {
        &mut self.kind
    }

    pub fn set_kind(&mut self, kind: OperandKind) {
        self.kind = kind;
    }

    pub fn is_register(&self) -> bool {
        match self.kind {
            OperandKind::Register(_) => true,
            _ => false,
        }
    }

    pub fn get_as_register(&self) -> Option<RegisterHandle> {
        match self.kind {
            OperandKind::Register(handle) => Some(handle),
            _ => None,
        }
    }

    pub fn get_as_physical_register(&self) -> Option<RegisterHandle> {
        match self.kind {
            OperandKind::PhysicalRegister(handle) => Some(handle),
            _ => None,
        }
    }

    pub fn get_as_const_i32(&self) -> Option<u32> {
        match self.kind {
            OperandKind::ConstI32(i) => Some(i),
            _ => None,
        }
    }

    pub fn get_as_label(&self) -> Option<BasicBlockHandle> {
        match self.kind {
            OperandKind::Label(label) => Some(label),
            _ => None,
        }
    }

    pub fn print(&self) {
        use self::OperandKind::*;
        match self.kind {
            Register(reg) => reg.get().print(),
            PhysicalRegister(reg) => reg.get().print(),
            ConstI32(n) => print!("{}", n),
            ConstI64(n) => print!("{}", n),
            Label(bb) => bb.print(),
            Memory { index, .. } => print!("[{}]", index),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::OperandKind::*;
        match self.kind {
            Register(reg) => write!(f, "{}", reg.get()),
            PhysicalRegister(reg) => write!(f, "{}", reg.get()),
            ConstI32(n) => write!(f, "{}", n),
            ConstI64(n) => write!(f, "{}", n),
            Label(bb) => write!(f, "label_{}", bb),
            Memory { index, .. } => write!(f, "[{}]", index),
        }
    }
}

impl fmt::Debug for OperandKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::OperandKind::*;
        match self {
            &Register(ref register) => write!(f, "Register({:?})", register),
            &PhysicalRegister(ref register) => write!(f, "PhysicalRegister({:?})", register),
            &ConstI32(ref i) => write!(f, "ConstantI32({:?})", i),
            &ConstI64(ref i) => write!(f, "ConstantI64({:?})", i),
            &Label(ref basic_block) => write!(f, "Label({:?})", basic_block),
            &Memory {
                index,
                ref typ,
                ref kind,
            } => write!(f, "Memory({} / {:?} / {:?})", index, typ, kind),
        }
    }
}
