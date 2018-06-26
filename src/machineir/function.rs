use std::collections::{HashMap, VecDeque};
use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle};
use machineir::typ::Type;

#[derive(PartialEq, Eq, Debug)]
pub struct Function {
    handle: FunctionHandle,
    basic_blocks: VecDeque<BasicBlockHandle>,
    parameter_types: Vec<Type>,
    result_types: Vec<Type>,
    local_variables: HashMap<usize, Type>,
}

impl Function {
    pub fn new(handle: FunctionHandle, parameter_types: Vec<Type>, result_types: Vec<Type>) -> Function {
        let local_variables = parameter_types.iter().enumerate()
            .map(|p| (p.0, p.1.clone())).collect();
        Function {
            handle: handle,
            basic_blocks: VecDeque::new(),
            parameter_types: parameter_types,
            result_types: result_types,
            local_variables: local_variables,
        }
    }

    pub fn get_handle(&self) -> &FunctionHandle {
        &self.handle
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

    pub fn get_local_variables(&self) -> &HashMap<usize, Type> {
        &self.local_variables
    }

    pub fn get_mut_local_variables(&mut self) -> &mut HashMap<usize, Type> {
        &mut self.local_variables
    }

    pub fn print(&self) {
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
