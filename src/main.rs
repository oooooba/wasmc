extern crate wasmc;

use std::collections::HashMap;

use wasmc::allocation::{EmitAssemblyPass, InsertBasicBlockLabelPass, ModuleInitPass,
                        PreEmitAssemblyPass, SimpleRegisterAllocationPass};
use wasmc::context::Context;
use wasmc::context::handle::{FunctionHandle, RegisterHandle};
use wasmc::machineir::typ::Type;
use wasmc::pass::{FunctionPass, PassManager};
use wasmc::wasmir;
use wasmc::wasmir::{Const, Ibinop, Irelop, Itestop, Resulttype, Valtype, WasmInstr};
use wasmc::wasm2machine::WasmToMachine;

pub struct MainPass {
    registers: Vec<RegisterHandle>,
    argument_registers: Vec<RegisterHandle>,
    result_register: RegisterHandle,
    register_name_map: HashMap<RegisterHandle, &'static str>,
}

impl FunctionPass for MainPass {
    fn initialize(&mut self, pass_manager: &mut PassManager) {
        pass_manager.add_function_pass(SimpleRegisterAllocationPass::create(
            self.registers.clone(), self.argument_registers.clone(), self.result_register));
        pass_manager.add_basic_block_pass(InsertBasicBlockLabelPass::create());
    }

    fn do_action(&mut self, _function: FunctionHandle) {}

    fn finalize(&mut self, pass_manager: &mut PassManager) {
        pass_manager.add_function_pass(PreEmitAssemblyPass::create("rbp"));
        pass_manager.add_instr_pass(EmitAssemblyPass::create(self.register_name_map.clone(), "rbp"));
    }
}

impl MainPass {
    pub fn create() -> Box<MainPass> {
        let mut r1 = Context::create_register(Type::I32);
        r1.set_physical();
        let mut r2 = Context::create_register(Type::I32);
        r2.set_physical();
        let mut r3 = Context::create_register(Type::I32);
        r3.set_physical();
        let registers = vec![r1, r2, r3];

        let mut ar1 = Context::create_register(Type::I32);
        ar1.set_physical();
        let mut ar2 = Context::create_register(Type::I32);
        ar2.set_physical();
        let mut ar3 = Context::create_register(Type::I32);
        ar3.set_physical();
        let argument_registers = vec![ar1, ar2, ar3];

        let result_register = r1;

        let mut register_name_map = HashMap::new();
        register_name_map.insert(r1, "eax");
        register_name_map.insert(r2, "ebx");
        register_name_map.insert(r3, "ecx");
        register_name_map.insert(ar1, "edi");
        register_name_map.insert(ar2, "esi");
        register_name_map.insert(ar3, "edx");

        Box::new(MainPass {
            registers,
            argument_registers,
            result_register,
            register_name_map,
        })
    }
}

fn main() {
    Context::init();

    let module = {
        let code1 = vec![
            WasmInstr::Block(Resulttype::new(Some(vec![Valtype::I32])), vec![
                WasmInstr::Block(Resulttype::new(Some(vec![Valtype::I32])), vec![
                    WasmInstr::Block(Resulttype::new(Some(vec![Valtype::I32])), vec![
                        WasmInstr::Const(Const::I32(1)),
                    ]),
                    WasmInstr::Const(Const::I32(0)),
                    WasmInstr::Const(Const::I32(10)),
                    WasmInstr::Irelop(Irelop::Eq32),
                    WasmInstr::If(
                        Resulttype::new(Some(vec![Valtype::I32])),
                        vec![
                            WasmInstr::Const(Const::I32(1)),
                        ], vec![
                            WasmInstr::Const(Const::I32(2)),
                        ]),
                    WasmInstr::Ibinop(Ibinop::Add32),
                ]),
            ]),
        ];
        let code2 = vec![
            WasmInstr::Block(Resulttype::new(Some(vec![Valtype::I32])), vec![
                WasmInstr::Block(Resulttype::new(Some(vec![Valtype::I32])), vec![
                    WasmInstr::Const(Const::I32(0)),
                    WasmInstr::Itestop(Itestop::Eqz32),
                    WasmInstr::If(
                        Resulttype::new(Some(vec![Valtype::I32])),
                        vec![
                            WasmInstr::Const(Const::I32(1)),
                        ], vec![
                            WasmInstr::Const(Const::I32(2)),
                        ]),
                    WasmInstr::Block(Resulttype::new(Some(vec![Valtype::I32])), vec![
                        WasmInstr::Const(Const::I32(1)),
                    ]),
                    WasmInstr::Ibinop(Ibinop::Add32),
                ]),
            ]),
        ];
        let code3 = vec![
            WasmInstr::GetLocal(wasmir::Localidx::new(0)),
            WasmInstr::Const(Const::I32(0)),
            WasmInstr::Irelop(Irelop::Eq32),
            WasmInstr::If(
                Resulttype::new(Some(vec![Valtype::I32])),
                vec![
                    WasmInstr::Const(Const::I32(0)),
                ], vec![
                    WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                    WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                    WasmInstr::Const(Const::I32(1)),
                    WasmInstr::Ibinop(Ibinop::Sub32),
                    WasmInstr::Call(wasmir::Funcidx::new(0)),
                    WasmInstr::Ibinop(Ibinop::Add32),
                    WasmInstr::Return,
                ]),
        ];
        let code4 = vec![
            WasmInstr::Block(Resulttype::new(Some(vec![])), vec![
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::Itestop(Itestop::Eqz32),
                WasmInstr::BrIf(0),
                WasmInstr::Const(Const::I32(1)),
                WasmInstr::Return,
            ]),
            WasmInstr::Const(Const::I32(0)),
        ];

        let mut functions = vec![];
        let functype = wasmir::Functype::new(vec![Valtype::I32], vec![Valtype::I32]);
        let functype_index = wasmir::Typeidx::new(0);
        for code in vec![code1, code2, code3, code4] {
            let function = wasmir::Func::new(functype_index, vec![], wasmir::Expr::new(code));
            functions.push(function);
        }

        let module = wasmir::Module::new(vec![functype], functions);
        let mut wasm_to_ir = WasmToMachine::new();
        wasm_to_ir.emit(&module);
        wasm_to_ir.finalize()
    };

    let mut pass_manager = PassManager::new();
    pass_manager.add_module_pass(ModuleInitPass::create());
    pass_manager.add_function_pass(MainPass::create());
    pass_manager.run(module);
}
