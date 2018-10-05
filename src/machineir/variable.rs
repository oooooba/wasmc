use context::handle::{RegionHandle, VariableHandle};
use machineir::typ::Type;

#[derive(PartialEq, Eq, Debug)]
pub struct Variable {
    handle: VariableHandle,
    region: RegionHandle,
    typ: Type,
    name: String,
}

impl Variable {
    pub fn new(handle: VariableHandle, typ: Type, region: RegionHandle) -> Variable {
        Variable {
            handle,
            typ,
            region,
            name: format!("variable_{}", handle),
        }
    }

    pub fn get_region(&self) -> RegionHandle {
        self.region
    }

    pub fn get_type(&self) -> &Type {
        &self.typ
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn set_name(&mut self, name: String) -> VariableHandle {
        self.name = name;
        self.handle
    }
}
