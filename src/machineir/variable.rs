use context::handle::{RegionHandle, VariableHandle};

#[derive(PartialEq, Eq, Debug)]
pub struct Variable {
    handle: VariableHandle,
    region: RegionHandle,
}

impl Variable {
    pub fn new(handle: VariableHandle, region: RegionHandle) -> Variable {
        Variable { handle, region }
    }
}
