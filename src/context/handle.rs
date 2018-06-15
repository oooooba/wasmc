use std::fmt;
use std::ops::{Deref, DerefMut};

use machineir::basicblock::BasicBlock;
use machineir::function::Function;
use machineir::instruction::Instr;
use machineir::register::Register;
use pass::PassKind;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct RegisterHandle(usize);

impl RegisterHandle {
    pub(super) fn new(id: usize) -> Self {
        RegisterHandle(id)
    }

    pub fn get(&self) -> &Register {
        unsafe {
            super::CONTEXT.registers.as_ref().unwrap().get(self).unwrap()
        }
    }

    pub fn get_mut(&mut self) -> &mut Register {
        unsafe {
            super::CONTEXT.registers.as_mut().unwrap().get_mut(self).unwrap()
        }
    }

    pub fn print(&self) {
        print!("{}r{}", if self.get().is_physical() { "p" } else { "v" }, self.0);
    }
}

impl Deref for RegisterHandle {
    type Target = Register;
    fn deref(&self) -> &Register {
        self.get()
    }
}

impl DerefMut for RegisterHandle {
    fn deref_mut(&mut self) -> &mut Register {
        self.get_mut()
    }
}

impl fmt::Display for RegisterHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct InstrHandle(usize);

impl InstrHandle {
    pub(super) fn new(id: usize) -> Self {
        InstrHandle(id)
    }

    pub fn get(&self) -> &super::Instr {
        unsafe {
            super::CONTEXT.instrs.as_ref().unwrap().get(self).unwrap()
        }
    }

    pub fn get_mut(&mut self) -> &mut super::Instr {
        unsafe {
            super::CONTEXT.instrs.as_mut().unwrap().get_mut(self).unwrap()
        }
    }

    pub fn print(&self) {
        print!("i{}", self.0);
    }
}

impl Deref for InstrHandle {
    type Target = Instr;
    fn deref(&self) -> &Instr {
        self.get()
    }
}

impl DerefMut for InstrHandle {
    fn deref_mut(&mut self) -> &mut Instr {
        self.get_mut()
    }
}

impl fmt::Display for InstrHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct BasicBlockHandle(usize);

impl BasicBlockHandle {
    pub(super) fn new(id: usize) -> Self {
        BasicBlockHandle(id)
    }

    pub fn get(&self) -> &BasicBlock {
        unsafe {
            super::CONTEXT.basic_blocks.as_ref().unwrap().get(self).unwrap()
        }
    }

    pub fn get_mut(&mut self) -> &mut BasicBlock {
        unsafe {
            super::CONTEXT.basic_blocks.as_mut().unwrap().get_mut(self).unwrap()
        }
    }

    pub fn print(&self) {
        print!("bb{}", self.0);
    }
}

impl Deref for BasicBlockHandle {
    type Target = BasicBlock;
    fn deref(&self) -> &BasicBlock {
        self.get()
    }
}

impl DerefMut for BasicBlockHandle {
    fn deref_mut(&mut self) -> &mut BasicBlock {
        self.get_mut()
    }
}

impl fmt::Display for BasicBlockHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct FunctionHandle(usize);

impl FunctionHandle {
    pub(super) fn new(id: usize) -> Self {
        FunctionHandle(id)
    }

    pub fn get(&self) -> &Function {
        unsafe {
            super::CONTEXT.functions.as_ref().unwrap().get(self).unwrap()
        }
    }

    pub fn get_mut(&mut self) -> &mut Function {
        unsafe {
            super::CONTEXT.functions.as_mut().unwrap().get_mut(self).unwrap()
        }
    }

    pub fn print(&self) {
        print!("f{}", self.0);
    }
}

impl Deref for FunctionHandle {
    type Target = Function;
    fn deref(&self) -> &Function {
        self.get()
    }
}

impl DerefMut for FunctionHandle {
    fn deref_mut(&mut self) -> &mut Function {
        self.get_mut()
    }
}

impl fmt::Display for FunctionHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct PassHandle(usize);

impl PassHandle {
    pub(super) fn new(id: usize) -> Self {
        PassHandle(id)
    }

    pub fn get(&self) -> &PassKind {
        unsafe {
            super::CONTEXT.passes.as_ref().unwrap().get(self).unwrap()
        }
    }

    pub fn get_mut(&mut self) -> &mut PassKind {
        unsafe {
            super::CONTEXT.passes.as_mut().unwrap().get_mut(self).unwrap()
        }
    }

    pub fn print(&self) {
        print!("pass{}", self.0);
    }
}

impl Deref for PassHandle {
    type Target = PassKind;
    fn deref(&self) -> &PassKind {
        self.get()
    }
}

impl DerefMut for PassHandle {
    fn deref_mut(&mut self) -> &mut PassKind {
        self.get_mut()
    }
}

impl fmt::Display for PassHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
