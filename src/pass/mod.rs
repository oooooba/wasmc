use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle};

pub trait InstrPass {
    fn do_action(&mut self, instr: InstrHandle);
}

impl fmt::Debug for InstrPass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

pub trait BasicBlockPass {
    fn do_action(&mut self, basic_block: BasicBlockHandle);
}

impl fmt::Debug for BasicBlockPass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

pub trait FunctionPass {
    fn do_action(&mut self, function: FunctionHandle);
}

impl fmt::Debug for FunctionPass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

pub trait ModulePass {
    fn do_action(&mut self, module: ModuleHandle);
}

impl fmt::Debug for ModulePass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
