use context::handle::{FunctionHandle, ModuleHandle, RegionHandle};

#[derive(PartialEq, Eq, Debug)]
pub struct Module {
    handle: ModuleHandle,
    functions: Vec<FunctionHandle>,
    dynamic_regions: Vec<RegionHandle>,
    indirect_function_tables: Vec<RegionHandle>,
}

impl Module {
    pub fn new(handle: ModuleHandle) -> Module {
        Module {
            handle: handle,
            functions: vec![],
            dynamic_regions: vec![],
            indirect_function_tables: vec![],
        }
    }

    pub fn get_handle(&self) -> &ModuleHandle {
        &self.handle
    }

    pub fn get_functions(&self) -> &Vec<FunctionHandle> {
        &self.functions
    }

    pub fn get_mut_functions(&mut self) -> &mut Vec<FunctionHandle> {
        &mut self.functions
    }

    pub fn get_dynamic_regions(&self) -> &Vec<RegionHandle> {
        &self.dynamic_regions
    }

    pub fn get_mut_dynamic_regions(&mut self) -> &mut Vec<RegionHandle> {
        &mut self.dynamic_regions
    }

    pub fn get_indirect_function_tables(&self) -> &Vec<RegionHandle> {
        &self.indirect_function_tables
    }

    pub fn get_mut_indirect_function_tables(&mut self) -> &mut Vec<RegionHandle> {
        &mut self.indirect_function_tables
    }
}
