use std::collections::VecDeque;
use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle};

#[derive(PartialEq, Eq, Debug)]
pub struct Function {
    handle: FunctionHandle,
    basic_blocks: VecDeque<BasicBlockHandle>,
}

impl Function {
    pub fn new(handle: FunctionHandle) -> Function {
        Function {
            handle: handle,
            basic_blocks: VecDeque::new(),
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
