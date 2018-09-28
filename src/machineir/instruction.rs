use std::fmt;

use context::handle::{BasicBlockHandle, InstrHandle};
use machineir::opcode::Opcode;
use pass::InstrPass;

#[derive(PartialEq, Eq, Debug)]
pub struct Instr {
    handle: InstrHandle,
    opcode: Opcode,
    basic_block: BasicBlockHandle,
}

impl Instr {
    pub fn new(handle: InstrHandle, opcode: Opcode, basic_block: BasicBlockHandle) -> Instr {
        Instr {
            handle: handle,
            opcode: opcode,
            basic_block: basic_block,
        }
    }

    pub fn get_handle(&self) -> &InstrHandle {
        &self.handle
    }

    pub fn get_opcode(&self) -> &Opcode {
        &self.opcode
    }

    pub fn get_mut_opcode(&mut self) -> &mut Opcode {
        &mut self.opcode
    }

    pub fn set_opcode(&mut self, opcode: Opcode) {
        self.opcode = opcode;
    }

    pub fn get_basic_block(&self) -> BasicBlockHandle {
        self.basic_block
    }

    pub fn print(&self) {
        self.opcode.print();
    }

    pub fn apply_instr_pass(&self, instr_pass: &mut dyn InstrPass) {
        instr_pass.do_action(self.handle)
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.opcode)
    }
}
