use std::collections::{HashMap, VecDeque};
use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle, RegionHandle};
use context::Context;
use machineir::region::RegionKind;
use machineir::typ::Type;

#[derive(PartialEq, Eq, Debug)]
pub struct Function {
    handle: FunctionHandle,
    func_name: String,
    basic_blocks: VecDeque<BasicBlockHandle>,
    parameter_types: Vec<Type>,
    result_types: Vec<Type>,
    local_variables: HashMap<usize, Type>,
    local_region: RegionHandle,
}

impl Function {
    pub fn new(
        handle: FunctionHandle,
        func_name: String,
        parameter_types: Vec<Type>,
        result_types: Vec<Type>,
    ) -> Function {
        let local_variables = parameter_types
            .iter()
            .enumerate()
            .map(|p| (p.0, p.1.clone()))
            .collect();
        Function {
            handle,
            func_name,
            basic_blocks: VecDeque::new(),
            parameter_types,
            result_types,
            local_variables,
            local_region: Context::create_region(RegionKind::Local),
        }
    }

    pub fn get_handle(&self) -> &FunctionHandle {
        &self.handle
    }

    pub fn get_func_name(&self) -> &String {
        &self.func_name
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

    pub fn get_result_types(&self) -> &Vec<Type> {
        &self.result_types
    }

    pub fn get_local_variables(&self) -> &HashMap<usize, Type> {
        &self.local_variables
    }

    pub fn get_mut_local_variables(&mut self) -> &mut HashMap<usize, Type> {
        &mut self.local_variables
    }

    pub fn get_local_region(&self) -> RegionHandle {
        self.local_region
    }

    pub fn print(&self) {
        println!("Function ({:?}) / <{}>", self.handle, self.get_func_name());
        for basic_block in self.basic_blocks.iter() {
            basic_block.get().print();
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
