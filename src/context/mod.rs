pub mod handle;

use std::collections::HashMap;

use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, PassHandle, RegisterHandle};
use machineir::basicblock::{BasicBlock, BasicBlockKind};
use machineir::function::Function;
use machineir::instruction::Instr;
use machineir::register::Register;
use machineir::opcode::Opcode;
use machineir::typ::Type;
use pass::PassKind;

#[derive(Debug)]
pub struct Context {
    registers: Option<HashMap<RegisterHandle, Register>>,
    num_created_registers: usize,
    instrs: Option<HashMap<InstrHandle, Instr>>,
    num_created_instrs: usize,
    basic_blocks: Option<HashMap<BasicBlockHandle, BasicBlock>>,
    num_created_basic_blocks: usize,
    functions: Option<HashMap<FunctionHandle, Function>>,
    num_created_functions: usize,
    passes: Option<HashMap<PassHandle, PassKind>>,
    num_created_passes3: usize,
}

static mut CONTEXT: Context = Context {
    registers: None,
    num_created_registers: 0,
    instrs: None,
    num_created_instrs: 0,
    basic_blocks: None,
    num_created_basic_blocks: 0,
    functions: None,
    num_created_functions: 0,
    passes: None,
    num_created_passes3: 0,
};

impl Context {
    pub fn init() {
        unsafe {
            CONTEXT.registers = Some(HashMap::new());
            CONTEXT.instrs = Some(HashMap::new());
            CONTEXT.basic_blocks = Some(HashMap::new());
            CONTEXT.functions = Some(HashMap::new());
            CONTEXT.passes = Some(HashMap::new());
        }
    }

    pub fn create_register(typ: Type) -> RegisterHandle {
        unsafe {
            let id = CONTEXT.num_created_registers;
            CONTEXT.num_created_registers += 1;
            let handle = RegisterHandle::new(id);
            let register = Register::new(handle, typ);
            CONTEXT.registers.as_mut().unwrap().insert(handle, register);
            handle
        }
    }

    pub fn create_instr(opcode: Opcode, basic_block: BasicBlockHandle) -> InstrHandle {
        unsafe {
            let id = CONTEXT.num_created_instrs;
            CONTEXT.num_created_instrs += 1;
            let handle = InstrHandle::new(id);
            let instr = Instr::new(handle, opcode, basic_block);
            CONTEXT.instrs.as_mut().unwrap().insert(handle, instr);
            handle
        }
    }

    pub fn create_basic_block(kind: BasicBlockKind) -> BasicBlockHandle {
        unsafe {
            let id = CONTEXT.num_created_basic_blocks;
            CONTEXT.num_created_basic_blocks += 1;
            let handle = BasicBlockHandle::new(id);
            let basic_block = BasicBlock::new(handle, kind);
            CONTEXT.basic_blocks.as_mut().unwrap().insert(handle, basic_block);
            handle
        }
    }

    pub fn create_function() -> FunctionHandle {
        unsafe {
            let id = CONTEXT.num_created_functions;
            CONTEXT.num_created_functions += 1;
            let handle = FunctionHandle::new(id);
            let function = Function::new(handle);
            CONTEXT.functions.as_mut().unwrap().insert(handle, function);
            handle
        }
    }

    pub fn create_pass(pass: PassKind) -> PassHandle {
        unsafe {
            let id = CONTEXT.num_created_passes3;
            CONTEXT.num_created_passes3 += 1;
            let handle = PassHandle::new(id);
            CONTEXT.passes.as_mut().unwrap().insert(handle, pass);
            handle
        }
    }
}
