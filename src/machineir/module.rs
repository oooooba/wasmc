use context::handle::{FunctionHandle, ModuleHandle, RegionHandle};
use context::Context;
use machineir::region::RegionKind;
use pass::{BasicBlockPass, FunctionPass, ModulePass};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Linkage {
    Export,
    Import,
    Private,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Module {
    handle: ModuleHandle,
    functions: Vec<FunctionHandle>,
    global_regions: Vec<RegionHandle>,
    indirect_function_tables: Vec<RegionHandle>,
}

impl Module {
    pub fn new(handle: ModuleHandle) -> Module {
        Module {
            handle: handle,
            functions: vec![],
            global_regions: vec![],
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

    pub fn get_global_regions(&self) -> &Vec<RegionHandle> {
        &self.global_regions
    }

    pub fn get_mut_global_regions(&mut self) -> &mut Vec<RegionHandle> {
        &mut self.global_regions
    }

    pub fn create_global_region(&mut self, kind: RegionKind) -> RegionHandle {
        let region = Context::create_region(kind);
        self.global_regions.push(region);
        region
    }

    pub fn get_indirect_function_tables(&self) -> &Vec<RegionHandle> {
        &self.indirect_function_tables
    }

    pub fn get_mut_indirect_function_tables(&mut self) -> &mut Vec<RegionHandle> {
        &mut self.indirect_function_tables
    }

    pub fn apply_module_pass(&self, module_pass: &mut dyn ModulePass) {
        module_pass.do_action(self.handle)
    }

    pub fn apply_function_pass(&self, function_pass: &mut dyn FunctionPass) {
        for function in &self.functions {
            function.apply_function_pass(function_pass);
        }
    }

    pub fn apply_basic_block_pass(&self, basic_block_pass: &mut dyn BasicBlockPass) {
        for function in &self.functions {
            function.apply_basic_block_pass(basic_block_pass);
        }
    }
}
