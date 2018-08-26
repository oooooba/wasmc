use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, PassHandle};
use context::Context;

pub trait InstrPass {
    fn do_action(&mut self, instr: InstrHandle);
    fn initialize(&mut self, _pass_manager: &mut PassManager) {}
    fn finalize(&mut self, _pass_manager: &mut PassManager) {}
}

impl fmt::Debug for InstrPass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

pub trait BasicBlockPass {
    fn do_action(&mut self, basic_block: BasicBlockHandle);
    fn initialize(&mut self, _pass_manager: &mut PassManager) {}
    fn finalize(&mut self, _pass_manager: &mut PassManager) {}
}

impl fmt::Debug for BasicBlockPass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

pub trait FunctionPass {
    fn do_action(&mut self, function: FunctionHandle);
    fn initialize(&mut self, _pass_manager: &mut PassManager) {}
    fn finalize(&mut self, _pass_manager: &mut PassManager) {}
}

impl fmt::Debug for FunctionPass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

pub trait ModulePass {
    fn do_action(&mut self, module: ModuleHandle);
    fn initialize(&mut self, _pass_manager: &mut PassManager) {}
    fn finalize(&mut self, _pass_manager: &mut PassManager) {}
}

impl fmt::Debug for ModulePass {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum PassKind {
    InstrPass(Box<InstrPass>),
    BasicBlockPass(Box<BasicBlockPass>),
    FunctionPass(Box<FunctionPass>),
    ModulePass(Box<ModulePass>),
}

pub struct PassManager {
    passes: Vec<PassHandle>,
}

impl PassManager {
    pub fn new() -> PassManager {
        PassManager { passes: vec![] }
    }

    pub fn add_instr_pass(&mut self, pass: Box<InstrPass>) -> PassHandle {
        let handle = Context::create_pass(PassKind::InstrPass(pass));
        self.passes.push(handle);
        handle
    }

    pub fn add_basic_block_pass(&mut self, pass: Box<BasicBlockPass>) -> PassHandle {
        let handle = Context::create_pass(PassKind::BasicBlockPass(pass));
        self.passes.push(handle);
        handle
    }

    pub fn add_function_pass(&mut self, pass: Box<FunctionPass>) -> PassHandle {
        let handle = Context::create_pass(PassKind::FunctionPass(pass));
        self.passes.push(handle);
        handle
    }

    pub fn add_module_pass(&mut self, pass: Box<ModulePass>) -> PassHandle {
        let handle = Context::create_pass(PassKind::ModulePass(pass));
        self.passes.push(handle);
        handle
    }

    fn run_on_instr(&mut self, instr: InstrHandle, pass: &mut Box<InstrPass>) {
        let mut before_pass_manager = PassManager::new();
        pass.initialize(&mut before_pass_manager);
        before_pass_manager.run_with_instr(instr);

        pass.do_action(instr);

        let mut after_pass_manager = PassManager::new();
        pass.finalize(&mut after_pass_manager);
        after_pass_manager.run_with_instr(instr);
    }

    fn run_on_basic_block(
        &mut self,
        basic_block: BasicBlockHandle,
        pass: &mut Box<BasicBlockPass>,
    ) {
        let mut before_pass_manager = PassManager::new();
        pass.initialize(&mut before_pass_manager);
        before_pass_manager.run_with_basic_block(basic_block);

        pass.do_action(basic_block);

        let mut after_pass_manager = PassManager::new();
        pass.finalize(&mut after_pass_manager);
        after_pass_manager.run_with_basic_block(basic_block);
    }

    fn run_on_function(&mut self, function: FunctionHandle, pass: &mut Box<FunctionPass>) {
        let mut before_pass_manager = PassManager::new();
        pass.initialize(&mut before_pass_manager);
        before_pass_manager.run_with_function(function);

        pass.do_action(function);

        let mut after_pass_manager = PassManager::new();
        pass.finalize(&mut after_pass_manager);
        after_pass_manager.run_with_function(function);
    }

    fn run_on_module(&mut self, module: ModuleHandle, pass: &mut Box<ModulePass>) {
        let mut before_pass_manager = PassManager::new();
        pass.initialize(&mut before_pass_manager);
        before_pass_manager.run_with_module(module);

        pass.do_action(module);

        let mut after_pass_manager = PassManager::new();
        pass.finalize(&mut after_pass_manager);
        after_pass_manager.run_with_module(module);
    }

    fn run_with_instr(&mut self, instr: InstrHandle) {
        for i in 0..self.passes.len() {
            use self::PassKind::*;
            let mut pass = self.passes[i];
            match pass.get_mut() {
                &mut InstrPass(ref mut pass) => self.run_on_instr(instr, pass),
                &mut BasicBlockPass(_) => panic!(),
                &mut FunctionPass(_) => panic!(),
                &mut ModulePass(_) => panic!(),
            }
        }
    }

    fn run_with_basic_block(&mut self, basic_block: BasicBlockHandle) {
        for i in 0..self.passes.len() {
            let mut pass = self.passes[i];
            use self::PassKind::*;
            match pass.get_mut() {
                &mut InstrPass(ref mut pass) => {
                    for instr in basic_block.get_instrs().iter() {
                        self.run_on_instr(*instr, pass);
                    }
                }
                &mut BasicBlockPass(ref mut pass) => self.run_on_basic_block(basic_block, pass),
                &mut FunctionPass(_) => panic!(),
                &mut ModulePass(_) => panic!(),
            }
        }
    }

    fn run_with_function(&mut self, function: FunctionHandle) {
        for i in 0..self.passes.len() {
            let mut pass = self.passes[i];
            use self::PassKind::*;
            match pass.get_mut() {
                &mut InstrPass(ref mut pass) => {
                    for basic_block in function.get_basic_blocks().iter() {
                        for instr in basic_block.get_instrs().iter() {
                            self.run_on_instr(*instr, pass);
                        }
                    }
                }
                &mut BasicBlockPass(ref mut pass) => {
                    for basic_block in function.get_basic_blocks().iter() {
                        self.run_on_basic_block(*basic_block, pass);
                    }
                }
                &mut FunctionPass(ref mut pass) => self.run_on_function(function, pass),
                &mut ModulePass(_) => panic!(),
            }
        }
    }

    fn run_with_module(&mut self, module: ModuleHandle) {
        for i in 0..self.passes.len() {
            let mut pass = self.passes[i];
            use self::PassKind::*;
            match pass.get_mut() {
                &mut InstrPass(ref mut pass) => {
                    for function in module.get_functions().iter() {
                        for basic_block in function.get_basic_blocks().iter() {
                            for instr in basic_block.get_instrs().iter() {
                                self.run_on_instr(*instr, pass);
                            }
                        }
                    }
                }
                &mut BasicBlockPass(ref mut pass) => {
                    for function in module.get_functions().iter() {
                        for basic_block in function.get_basic_blocks().iter() {
                            self.run_on_basic_block(*basic_block, pass);
                        }
                    }
                }
                &mut FunctionPass(ref mut pass) => {
                    for function in module.get_functions().iter() {
                        self.run_on_function(*function, pass);
                    }
                }
                &mut ModulePass(ref mut pass) => self.run_on_module(module, pass),
            }
        }
    }

    pub fn run(&mut self, module: ModuleHandle) {
        self.run_with_module(module);
    }
}
