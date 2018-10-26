use std::collections::VecDeque;
use std::fmt;

use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle};
use context::Context;
use machineir::opcode::Opcode;
use pass::{BasicBlockPass, InstrPass};

pub struct Iterator {
    basic_block: BasicBlockHandle,
    index: usize,
}

impl Iterator {
    pub fn get(&self) -> Option<InstrHandle> {
        if self.basic_block.check_instrs_index(self.index) {
            Some(self.basic_block.get_instrs()[self.index])
        } else {
            None
        }
    }

    pub fn insert_before(&mut self, instr: InstrHandle) {
        if !self.basic_block.check_instrs_index(self.index) {
            panic!();
        }
        self.basic_block.get_mut_instrs().insert(self.index, instr);
        self.advance();
    }

    pub fn insert_after(&mut self, instr: InstrHandle) {
        if !self.basic_block.check_instrs_index(self.index) {
            panic!();
        }
        let n = self.basic_block.instrs.len();
        if self.index == n - 1 {
            self.basic_block.get_mut_instrs().push_back(instr);
        } else {
            self.basic_block
                .get_mut_instrs()
                .insert(self.index + 1, instr);
        }
    }

    pub fn advance(&mut self) {
        if self.basic_block.check_instrs_index(self.index) {
            self.index += 1;
        } else {
            panic!()
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct BasicBlock {
    handle: BasicBlockHandle,
    instrs: VecDeque<InstrHandle>,
    function: FunctionHandle,
    name: String,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "BasicBlock[{}]", self.handle)?;
        writeln!(f, "  {}:", self.get_name())?;
        for instr in self.instrs.iter() {
            writeln!(f, "    {}: {}", instr, instr.get())?;
        }
        Ok(())
    }
}

impl BasicBlock {
    pub fn new(handle: BasicBlockHandle, function: FunctionHandle) -> BasicBlock {
        BasicBlock {
            handle,
            instrs: VecDeque::new(),
            function,
            name: format!("label_{}", handle),
        }
    }

    pub fn iterator(&self) -> Iterator {
        Iterator {
            basic_block: self.handle,
            index: 0,
        }
    }

    pub fn get_handle(&self) -> &BasicBlockHandle {
        &self.handle
    }

    fn check_instrs_index(&self, index: usize) -> bool {
        index < self.get_instrs().len()
    }

    pub fn get_instrs(&self) -> &VecDeque<InstrHandle> {
        &self.instrs
    }

    pub fn get_mut_instrs(&mut self) -> &mut VecDeque<InstrHandle> {
        &mut self.instrs
    }

    pub fn set_instrs(&mut self, instrs: VecDeque<InstrHandle>) -> BasicBlockHandle {
        self.instrs = instrs;
        self.handle
    }

    pub fn add_instr(&mut self, instr: InstrHandle) {
        self.instrs.push_back(instr)
    }

    pub fn emit_instr(&mut self, opcode: Opcode) -> InstrHandle {
        let instr = Context::create_instr(opcode, self.handle);
        self.instrs.push_back(instr);
        instr
    }

    pub fn get_function(&self) -> FunctionHandle {
        self.function
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn print(&self) {
        println!("BasicBlock ({:?} / {})", self.handle, self.get_name());
        for instr in self.instrs.iter() {
            print!("    ");
            instr.print();
            print!(" @ ");
            instr.get().print();
            println!();
        }
    }

    pub fn apply_basic_block_pass(&self, basic_block_pass: &mut dyn BasicBlockPass) {
        basic_block_pass.do_action(self.handle)
    }

    pub fn apply_instr_pass(&self, instr_pass: &mut dyn InstrPass) {
        for instr in &self.instrs {
            instr.apply_instr_pass(instr_pass)
        }
    }
}
