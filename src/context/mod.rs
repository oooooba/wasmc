pub mod handle;

use std::collections::BTreeMap;

use context::handle::{
    BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, PassHandle, RegionHandle,
    RegisterHandle,
};
use machineir::basicblock::BasicBlock;
use machineir::function::Function;
use machineir::instruction::Instr;
use machineir::module::Module;
use machineir::opcode::Opcode;
use machineir::region::{Region, RegionKind};
use machineir::register::Register;
use machineir::typ::Type;
use pass::PassKind;

#[derive(Debug)]
pub struct Context {
    registers: Option<BTreeMap<RegisterHandle, Register>>,
    num_created_registers: usize,
    instrs: Option<BTreeMap<InstrHandle, Instr>>,
    num_created_instrs: usize,
    basic_blocks: Option<BTreeMap<BasicBlockHandle, BasicBlock>>,
    num_created_basic_blocks: usize,
    functions: Option<BTreeMap<FunctionHandle, Function>>,
    num_created_functions: usize,
    regions: Option<BTreeMap<RegionHandle, Region>>,
    num_created_regions: usize,
    modules: Option<BTreeMap<ModuleHandle, Module>>,
    num_created_modules: usize,
    passes: Option<BTreeMap<PassHandle, PassKind>>,
    num_created_passes: usize,
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
    regions: None,
    num_created_regions: 0,
    modules: None,
    num_created_modules: 0,
    passes: None,
    num_created_passes: 0,
};

impl Context {
    pub fn init() {
        unsafe {
            CONTEXT.registers = Some(BTreeMap::new());
            CONTEXT.instrs = Some(BTreeMap::new());
            CONTEXT.basic_blocks = Some(BTreeMap::new());
            CONTEXT.functions = Some(BTreeMap::new());
            CONTEXT.regions = Some(BTreeMap::new());
            CONTEXT.modules = Some(BTreeMap::new());
            CONTEXT.passes = Some(BTreeMap::new());
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

    pub fn create_basic_block() -> BasicBlockHandle {
        unsafe {
            let id = CONTEXT.num_created_basic_blocks;
            CONTEXT.num_created_basic_blocks += 1;
            let handle = BasicBlockHandle::new(id);
            let basic_block = BasicBlock::new(handle);
            CONTEXT
                .basic_blocks
                .as_mut()
                .unwrap()
                .insert(handle, basic_block);
            handle
        }
    }

    pub fn create_function(
        func_name: String,
        parameter_types: Vec<Type>,
        result_types: Vec<Type>,
        module: ModuleHandle,
    ) -> FunctionHandle {
        unsafe {
            let id = CONTEXT.num_created_functions;
            CONTEXT.num_created_functions += 1;
            let handle = FunctionHandle::new(id);
            let function = Function::new(handle, func_name, parameter_types, result_types, module);
            CONTEXT.functions.as_mut().unwrap().insert(handle, function);
            handle
        }
    }

    pub fn create_region(kind: RegionKind) -> RegionHandle {
        unsafe {
            let id = CONTEXT.num_created_regions;
            CONTEXT.num_created_regions += 1;
            let handle = RegionHandle::new(id);
            let region = Region::new(handle, kind);
            CONTEXT.regions.as_mut().unwrap().insert(handle, region);
            handle
        }
    }

    pub fn create_module() -> ModuleHandle {
        unsafe {
            let id = CONTEXT.num_created_modules;
            CONTEXT.num_created_modules += 1;
            let handle = ModuleHandle::new(id);
            let module = Module::new(handle);
            CONTEXT.modules.as_mut().unwrap().insert(handle, module);
            handle
        }
    }

    pub fn create_pass(pass: PassKind) -> PassHandle {
        unsafe {
            let id = CONTEXT.num_created_passes;
            CONTEXT.num_created_passes += 1;
            let handle = PassHandle::new(id);
            CONTEXT.passes.as_mut().unwrap().insert(handle, pass);
            handle
        }
    }
}
