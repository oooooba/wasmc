use std::fmt;

use context::handle::{BasicBlockHandle, InstrHandle, RegisterHandle};
use machineir::opcode::Opcode;
use machineir::operand::Operand;

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

    pub fn get_destination_register_operand(&self) -> Option<&Operand> {
        self.opcode.get_destination_register_operand()
    }

    pub fn get_mut_destination_register_operand(&mut self) -> Option<&mut Operand> {
        self.opcode.get_mut_destination_register_operand()
    }

    pub fn get_destination_register(&self) -> Option<RegisterHandle> {
        self.opcode.get_destination_register()
    }

    pub fn get_source_register_operands(&self) -> Vec<&Operand> {
        self.opcode.get_source_register_operands()
    }

    pub fn get_mut_source_register_operands(&mut self) -> Vec<&mut Operand> {
        self.opcode.get_mut_source_register_operands()
    }

    pub fn get_source_registers(&self) -> Vec<RegisterHandle> {
        self.opcode.get_source_registers()
    }

    pub fn get_registers(&self) -> Vec<RegisterHandle> {
        self.opcode.get_registers()
    }

    pub fn print(&self) {
        self.opcode.print();
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.opcode)
    }
}
