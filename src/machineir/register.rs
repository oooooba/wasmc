use std::fmt;

use context::handle::RegisterHandle;
use machineir::typ::Type;

#[derive(PartialEq, Eq, Debug)]
pub struct Register {
    handle: RegisterHandle,
    typ: Type,
    is_physical: bool,
    offset: usize, // only used when is_physical is false, ToDo: fix
}

impl Register {
    pub fn new(handle: RegisterHandle, typ: Type) -> Register {
        Register {
            handle,
            typ,
            is_physical: false,
            offset: 0,
        }
    }

    pub fn get_handle(&self) -> &RegisterHandle {
        &self.handle
    }

    pub fn get_typ(&self) -> &Type {
        &self.typ
    }

    pub fn set_physical(&mut self) {
        self.is_physical = true;
    }

    pub fn is_physical(&self) -> bool {
        self.is_physical
    }

    pub fn get_offset(&self) -> usize {
        self.offset
    }

    pub fn set_offset(&mut self, offset: usize) {
        self.offset = offset;
    }

    pub fn print(&self) {
        print!(
            "{}reg{}",
            if self.is_physical() { "p" } else { "v" },
            self.handle
        )
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}reg{}",
            if self.is_physical() { "p" } else { "v" },
            self.handle
        )
    }
}
