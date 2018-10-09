use std::collections::VecDeque;
use std::fmt;

use context::handle::{
    BasicBlockHandle, FunctionHandle, ModuleHandle, RegionHandle, VariableHandle,
};
use context::Context;
use machineir::module::Linkage;
use machineir::region::RegionKind;
use machineir::typ::Type;
use pass::{BasicBlockPass, FunctionPass, InstrPass};

#[derive(PartialEq, Eq, Debug)]
pub struct Function {
    handle: FunctionHandle,
    func_name: String,
    basic_blocks: VecDeque<BasicBlockHandle>,
    parameter_types: Vec<Type>,
    parameter_variables: Vec<VariableHandle>,
    result_types: Vec<Type>,
    local_region: RegionHandle,
    linkage: Linkage,
    module: ModuleHandle,
    is_program_initializer: bool,
}

impl Function {
    pub fn new(
        handle: FunctionHandle,
        func_name: String,
        parameter_types: Vec<Type>,
        result_types: Vec<Type>,
        module: ModuleHandle,
    ) -> Function {
        let mut region = Context::create_region(RegionKind::Local);

        let mut parameter_variables = vec![];
        for typ in parameter_types.iter() {
            let var = region.create_variable(typ.clone(), None);
            parameter_variables.push(var);
        }

        Function {
            handle,
            func_name,
            basic_blocks: VecDeque::new(),
            parameter_types,
            parameter_variables,
            result_types,
            local_region: region,
            linkage: Linkage::Private,
            module,
            is_program_initializer: false,
        }
    }

    pub fn get_handle(&self) -> &FunctionHandle {
        &self.handle
    }

    pub fn get_func_name(&self) -> &String {
        &self.func_name
    }

    pub fn set_func_name(&mut self, func_name: String) -> FunctionHandle {
        self.func_name = func_name;
        self.handle
    }

    pub fn get_basic_blocks(&self) -> &VecDeque<BasicBlockHandle> {
        &self.basic_blocks
    }

    pub fn get_mut_basic_blocks(&mut self) -> &mut VecDeque<BasicBlockHandle> {
        &mut self.basic_blocks
    }

    pub fn get_parameter_types(&self) -> &Vec<Type> {
        &self.parameter_types
    }

    pub fn get_parameter_variables(&self) -> &Vec<VariableHandle> {
        &self.parameter_variables
    }

    pub fn get_result_types(&self) -> &Vec<Type> {
        &self.result_types
    }

    pub fn get_local_region(&self) -> RegionHandle {
        self.local_region
    }

    pub fn get_linkage(&self) -> &Linkage {
        &self.linkage
    }

    pub fn set_linkage(&mut self, linkage: Linkage) -> FunctionHandle {
        self.linkage = linkage;
        self.handle
    }

    pub fn get_module(&self) -> ModuleHandle {
        self.module
    }

    pub fn is_program_initializer(&self) -> bool {
        self.is_program_initializer
    }

    pub fn set_program_initializer(&mut self) -> FunctionHandle {
        self.is_program_initializer = true;
        self.handle
    }

    pub fn print(&self) {
        println!("Function ({:?}) / <{}>", self.handle, self.get_func_name());
        for basic_block in self.basic_blocks.iter() {
            basic_block.get().print();
        }
    }

    pub fn apply_function_pass(&self, function_pass: &mut dyn FunctionPass) {
        function_pass.do_action(self.handle)
    }

    pub fn apply_basic_block_pass(&self, basic_block_pass: &mut dyn BasicBlockPass) {
        for basic_block in &self.basic_blocks {
            basic_block.apply_basic_block_pass(basic_block_pass)
        }
    }

    pub fn apply_instr_pass(&self, instr_pass: &mut dyn InstrPass) {
        for basic_block in &self.basic_blocks {
            basic_block.apply_instr_pass(instr_pass)
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
