use std::collections::VecDeque;
use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle, RegionHandle, RegisterHandle};
use context::Context;
use machineir::region::RegionKind;
use machineir::typ::Type;

#[derive(Debug, PartialEq, Eq)]
pub enum Linkage {
    Export,
    Import,
    Private,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Function {
    handle: FunctionHandle,
    func_name: String,
    basic_blocks: VecDeque<BasicBlockHandle>,
    parameter_types: Vec<Type>,
    parameter_variables: Vec<RegisterHandle>,
    result_types: Vec<Type>,
    local_region: RegionHandle,
    linkage: Linkage,
}

impl Function {
    pub fn new(
        handle: FunctionHandle,
        func_name: String,
        parameter_types: Vec<Type>,
        result_types: Vec<Type>,
    ) -> Function {
        let mut region = Context::create_region(RegionKind::Local);
        let mut parameter_variables = vec![];
        for typ in parameter_types.iter() {
            let reg = Context::create_register(typ.clone());
            region.get_mut_offset_map().insert(reg, 0);
            parameter_variables.push(reg);
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

    pub fn get_parameter_variables(&self) -> &Vec<RegisterHandle> {
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
