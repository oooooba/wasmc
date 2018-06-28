use std::collections::HashMap;

use context::handle::{FunctionHandle, ModuleHandle};

#[derive(PartialEq, Eq, Debug)]
pub struct Module {
    handle: ModuleHandle,
    functions: HashMap<String, FunctionHandle>,
}

impl Module {
    pub fn new(handle: ModuleHandle) -> Module {
        Module {
            handle: handle,
            functions: HashMap::new(),
        }
    }

    pub fn get_handle(&self) -> &ModuleHandle {
        &self.handle
    }

    pub fn get_functions(&self) -> &HashMap<String, FunctionHandle> {
        &self.functions
    }

    pub fn get_mut_functions(&mut self) -> &mut HashMap<String, FunctionHandle> {
        &mut self.functions
    }
}
