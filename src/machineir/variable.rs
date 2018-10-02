use context::handle::{RegionHandle, VariableHandle};
use machineir::typ::Type;

#[derive(PartialEq, Eq, Debug)]
pub struct Variable {
    handle: VariableHandle,
    region: RegionHandle,
    typ: Type,
}

impl Variable {
    pub fn new(handle: VariableHandle, typ: Type, region: RegionHandle) -> Variable {
        Variable {
            handle,
            typ,
            region,
        }
    }
}
