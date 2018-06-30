extern crate wasmc;

use std::collections::HashMap;

use wasmc::allocation::{EmitAssemblyPass, InsertBasicBlockLabelPass,
                        PreEmitAssemblyPass, SimpleRegisterAllocationPass};
use wasmc::context::Context;
use wasmc::machineir::typ::Type;
use wasmc::pass::{GroupPass, PassManager};
use wasmc::wasmir;
use wasmc::wasmir::{Binop, Const, Ibinop, Irelop, Resulttype, Valtype, WasmInstr};
use wasmc::wasm2machine::WasmToMachine;

pub struct MainPass {}

impl GroupPass for MainPass {
    fn do_action(&mut self, pass_manager: &mut PassManager) {
        let mut r1 = Context::create_register(Type::I32);
        r1.set_physical();
        let mut r2 = Context::create_register(Type::I32);
        r2.set_physical();
        let mut r3 = Context::create_register(Type::I32);
        r3.set_physical();
        let mut ar1 = Context::create_register(Type::I32);
        ar1.set_physical();
        let mut ar2 = Context::create_register(Type::I32);
        ar2.set_physical();
        let mut ar3 = Context::create_register(Type::I32);
        ar3.set_physical();

        pass_manager.add_function_pass(SimpleRegisterAllocationPass::create(vec![r1, r2, r3], vec![ar1, ar2, ar3], r1));
        pass_manager.add_basic_block_pass(InsertBasicBlockLabelPass::create());

        let mut reg_name_map = HashMap::new();
        reg_name_map.insert(r1, "eax");
        reg_name_map.insert(r2, "ebx");
        reg_name_map.insert(r3, "ecx");
        reg_name_map.insert(ar1, "edi");
        reg_name_map.insert(ar2, "esi");
        reg_name_map.insert(ar3, "edx");

        pass_manager.add_function_pass(PreEmitAssemblyPass::create("rbp"));
        pass_manager.add_instr_pass(EmitAssemblyPass::create(reg_name_map, "rbp"));
    }
}

impl MainPass {
    pub fn create() -> Box<MainPass> {
        Box::new(MainPass {})
    }
}

fn main() {
    Context::init();

    let mut module = {
        let code = vec![
            WasmInstr::GetLocal(wasmir::Localidx::new(0)),
            WasmInstr::Const(Const::I32(0)),
            WasmInstr::Binop(Binop::Irelop(Irelop::Eq32)),
            WasmInstr::If(
                Resulttype::new(Some(vec![Valtype::U32])),
                vec![
                    WasmInstr::Const(Const::I32(0)),
                ], vec![
                    WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                    WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                    WasmInstr::Const(Const::I32(1)),
                    WasmInstr::Binop(Binop::Ibinop(Ibinop::Sub32)),
                    WasmInstr::Call(wasmir::Funcidx::new(0)),
                    WasmInstr::Binop(Binop::Ibinop(Ibinop::Add32)),
                ]),
            WasmInstr::Return,
        ];
        let functype = wasmir::Functype::new(vec![Valtype::U32], vec![Valtype::U32]);
        let function = wasmir::Func::new(wasmir::Typeidx::new(0), vec![], wasmir::Expr::new(code));
        let module = wasmir::Module::new(vec![functype], vec![function]);
        let mut wasm_to_ir = WasmToMachine::new();
        wasm_to_ir.emit(&module);
        wasm_to_ir.finalize()
    };
    let mut pass_manager = PassManager::new();
    pass_manager.add_group_pass(MainPass::create());
    pass_manager.run(*module.get_mut_functions().get(&format!("f_{}", 0)).unwrap());
}
