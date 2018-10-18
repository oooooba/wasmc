use std::fmt;

use context::handle::RegisterHandle;
use machineir::typ::Type;

#[derive(PartialEq, Eq, Debug)]
pub struct Register {
    handle: RegisterHandle,
    typ: Type,
    is_physical: bool,
}

impl Register {
    pub fn new(handle: RegisterHandle, typ: Type) -> Register {
        Register {
            handle,
            typ,
            is_physical: false,
        }
    }

    pub fn get_handle(&self) -> &RegisterHandle {
        &self.handle
    }

    pub fn get_type(&self) -> &Type {
        &self.typ
    }

    pub fn set_physical(&mut self) -> RegisterHandle {
        self.is_physical = true;
        self.handle
    }

    pub fn is_physical(&self) -> bool {
        self.is_physical
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
