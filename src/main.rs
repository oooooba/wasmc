extern crate wasmc;

use std::collections::HashMap;

use wasmc::allocation::{AnalyzeMemoryOffsetPass, EmitAssemblyPass, InsertBasicBlockLabelPass,
                        PostEmitAssemblyPass, PreEmitAssemblyPass, SimpleRegisterAllocationPass};
use wasmc::context::Context;
use wasmc::machineir::typ::Type;
use wasmc::pass::PassManager;
use wasmc::wasmir::{Binop, Const, Ibinop, Resulttype, Valtype, WasmInstr};
use wasmc::wasm2machine::WasmToMachine;

fn main() {
    Context::init();

    let function = {
        let code = WasmInstr::Block(Resulttype::new(Some(vec![Valtype::U32])), vec![
            WasmInstr::Const(Const::I32(5)),
            WasmInstr::Const(Const::I32(6)),
            WasmInstr::Binop(Binop::Ibinop(Ibinop::Add32)),
            WasmInstr::If(
                Resulttype::new(Some(vec![Valtype::U32])),
                vec![
                    WasmInstr::Const(Const::I32(1)),
                    WasmInstr::Const(Const::I32(2)),
                    WasmInstr::Binop(Binop::Ibinop(Ibinop::Add32)),
                ], vec![
                    WasmInstr::Const(Const::I32(3)),
                    WasmInstr::Const(Const::I32(4)),
                    WasmInstr::Binop(Binop::Ibinop(Ibinop::Add32)),
                ]),
            WasmInstr::Loop(
                Resulttype::new(None),
                vec![
                    WasmInstr::Const(Const::I32(7)),
                    WasmInstr::Const(Const::I32(8)),
                    WasmInstr::Binop(Binop::Ibinop(Ibinop::Add32)),
                    WasmInstr::BrIf(0),
                    WasmInstr::Const(Const::I32(9)),
                    WasmInstr::Const(Const::I32(10)),
                    WasmInstr::Binop(Binop::Ibinop(Ibinop::Add32)),
                    WasmInstr::BrIf(0),
                ]),
        ]);
        let mut wasm_to_ir = WasmToMachine::new(Resulttype::new(Some(vec![Valtype::U32])));
        wasm_to_ir.emit(&code);
        wasm_to_ir.finalize()
    };
    {
        let mut r1 = Context::create_register(Type::I32);
        r1.set_physical();
        let mut r2 = Context::create_register(Type::I32);
        r2.set_physical();
        let mut r3 = Context::create_register(Type::I32);
        r3.set_physical();

        let mut pass_manager = PassManager::new();

        pass_manager.add_instr_pass(AnalyzeMemoryOffsetPass::create());
        pass_manager.add_basic_block_pass(SimpleRegisterAllocationPass::create(vec![r1, r2, r3]));
        pass_manager.add_basic_block_pass(InsertBasicBlockLabelPass::create());

        let mut reg_name_map = HashMap::new();
        reg_name_map.insert(r1, "eax");
        reg_name_map.insert(r2, "ebx");
        reg_name_map.insert(r3, "ecx");

        pass_manager.add_function_pass(PreEmitAssemblyPass::create());
        pass_manager.add_instr_pass(EmitAssemblyPass::create(reg_name_map, "rdi"));
        pass_manager.add_function_pass(PostEmitAssemblyPass::create());

        pass_manager.run(function);
    }
}
