use context::handle::{FunctionHandle, ModuleHandle, RegionHandle};
use context::Context;
use machineir::region::RegionKind;
use pass::{BasicBlockPass, FunctionPass, ModulePass};

#[derive(Debug, PartialEq, Eq)]
pub enum Linkage {
    Export,
    Import,
    Private,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Module {
    handle: ModuleHandle,
    functions: Vec<FunctionHandle>,
    mutable_global_variable_region: RegionHandle,
    const_global_variable_region: RegionHandle,
    dynamic_regions: Vec<RegionHandle>,
    global_regions: Vec<RegionHandle>,
    indirect_function_tables: Vec<RegionHandle>,
}

impl Module {
    pub fn new(handle: ModuleHandle) -> Module {
        Module {
            handle: handle,
            functions: vec![],
            mutable_global_variable_region: Context::create_region(RegionKind::MutableGlobal),
            const_global_variable_region: Context::create_region(RegionKind::ReadOnlyGlobal),
            dynamic_regions: vec![],
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

    pub fn get_mutable_global_variable_region(&self) -> RegionHandle {
        self.mutable_global_variable_region
    }

    pub fn get_const_global_variable_region(&self) -> RegionHandle {
        self.const_global_variable_region
    }

    pub fn get_dynamic_regions(&self) -> &Vec<RegionHandle> {
        &self.dynamic_regions
    }

    pub fn get_mut_dynamic_regions(&mut self) -> &mut Vec<RegionHandle> {
        &mut self.dynamic_regions
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
