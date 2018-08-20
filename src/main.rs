extern crate wasmc;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufReader;
use std::iter::FromIterator;

use wasmc::allocation::SimpleRegisterAllocationPass;
use wasmc::asmprinter::{EmitAssemblyPass, InsertBasicBlockLabelPass, ModuleInitPass};
use wasmc::context::Context;
use wasmc::context::handle::{FunctionHandle, ModuleHandle, RegisterHandle};
use wasmc::machineir::typ::Type;
use wasmc::parser;
use wasmc::pass::{FunctionPass, PassManager};
use wasmc::wasmir;
use wasmc::wasmir::{Labelidx, Module};
use wasmc::wasmir::types::{Functype, Resulttype, Valtype};
use wasmc::wasmir::instructions::{Const, Cvtop, Expr, Ibinop, Irelop, Itestop, WasmInstr};
use wasmc::wasm2machine::WasmToMachine;

pub struct MainPass {
    registers: Vec<HashMap<Type, RegisterHandle>>,
    argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    result_register: HashMap<Type, RegisterHandle>,
    base_pointer_register: RegisterHandle,
    stack_pointer_register: RegisterHandle,
    register_name_map: HashMap<RegisterHandle, &'static str>,
}

impl FunctionPass for MainPass {
    fn initialize(&mut self, pass_manager: &mut PassManager) {
        pass_manager.add_function_pass(SimpleRegisterAllocationPass::create(
            self.registers.clone(), self.argument_registers.clone(), self.result_register.clone()));
    }

    fn do_action(&mut self, _function: FunctionHandle) {}

    fn finalize(&mut self, pass_manager: &mut PassManager) {
        pass_manager.add_basic_block_pass(InsertBasicBlockLabelPass::create());
        pass_manager.add_function_pass(EmitAssemblyPass::create(
            self.register_name_map.clone(), self.base_pointer_register,
            self.stack_pointer_register, self.argument_registers.clone()));
    }
}

impl MainPass {
    pub fn create() -> Box<MainPass> {
        let mut reg_al = Context::create_register(Type::I8);
        reg_al.set_physical();
        let mut reg_eax = Context::create_register(Type::I32);
        reg_eax.set_physical();
        let mut reg_rax = Context::create_register(Type::I64);
        reg_rax.set_physical();
        let mut reg_ebx = Context::create_register(Type::I32);
        reg_ebx.set_physical();
        let mut reg_rbx = Context::create_register(Type::I64);
        reg_rbx.set_physical();
        let mut reg_cl = Context::create_register(Type::I8);
        reg_cl.set_physical();
        let mut reg_edx = Context::create_register(Type::I32);
        reg_edx.set_physical();
        let mut reg_rdx = Context::create_register(Type::I64);
        reg_rdx.set_physical();
        let mut reg_edi = Context::create_register(Type::I32);
        reg_edi.set_physical();
        let mut reg_rdi = Context::create_register(Type::I64);
        reg_rdi.set_physical();
        let mut reg_esi = Context::create_register(Type::I32);
        reg_esi.set_physical();
        let mut reg_rsi = Context::create_register(Type::I64);
        reg_rsi.set_physical();
        let mut reg_rbp = Context::create_register(Type::I64);
        reg_rbp.set_physical();
        let mut reg_rsp = Context::create_register(Type::I64);
        reg_rsp.set_physical();

        let registers = vec![
            HashMap::from_iter(vec![(Type::I32, reg_eax), (Type::I64, reg_rax)]),
            HashMap::from_iter(vec![(Type::I32, reg_ebx), (Type::I64, reg_rbx)]),
            HashMap::from_iter(vec![(Type::I8, reg_cl)]),
        ];

        let argument_registers = vec![
            HashMap::from_iter(vec![(Type::I32, reg_edi), (Type::I64, reg_rdi)]),
            HashMap::from_iter(vec![(Type::I32, reg_esi), (Type::I64, reg_rsi)]),
            HashMap::from_iter(vec![(Type::I32, reg_edx), (Type::I64, reg_rdx)]),
        ];

        let result_register = HashMap::from_iter(vec![
            (Type::I8, reg_al),
            (Type::I32, reg_eax),
            (Type::I64, reg_rax),
        ]);

        let register_name_map = HashMap::from_iter(vec![
            (reg_al, "al"),
            (reg_eax, "eax"),
            (reg_rax, "rax"),
            (reg_ebx, "ebx"),
            (reg_rbx, "rbx"),
            (reg_cl, "cl"),
            (reg_edx, "edx"),
            (reg_rdx, "rdx"),
            (reg_edi, "edi"),
            (reg_rdi, "rdi"),
            (reg_esi, "esi"),
            (reg_rsi, "rsi"),
            (reg_rbp, "rbp"),
            (reg_rsp, "rsp"),
        ]);

        Box::new(MainPass {
            registers,
            argument_registers,
            result_register,
            base_pointer_register: reg_rbp,
            stack_pointer_register: reg_rsp,
            register_name_map,
        })
    }
}

#[allow(dead_code)]
fn create_test_wasm_ir() -> Module {
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
                WasmInstr::Call(wasmir::Funcidx::new(2)),
                WasmInstr::Ibinop(Ibinop::Add32),
            ]),
    ];
    let code4 = vec![
        WasmInstr::Block(Resulttype::new(Some(vec![])), vec![
            WasmInstr::GetLocal(wasmir::Localidx::new(0)),
            WasmInstr::Itestop(Itestop::Eqz32),
            WasmInstr::BrIf(Labelidx::new(0)),
            WasmInstr::Const(Const::I32(1)),
            WasmInstr::Return,
        ]),
        WasmInstr::Const(Const::I32(0)),
    ];
    let code5 = vec![
        WasmInstr::Block(Resulttype::new(Some(vec![])), vec![
            WasmInstr::GetLocal(wasmir::Localidx::new(0)),
            WasmInstr::Itestop(Itestop::Eqz32),
            WasmInstr::BrIf(Labelidx::new(0)),
            WasmInstr::GetLocal(wasmir::Localidx::new(0)),
            WasmInstr::Const(Const::I32(1)),
            WasmInstr::Ibinop(Ibinop::Shl32),
            WasmInstr::GetLocal(wasmir::Localidx::new(0)),
            WasmInstr::Const(Const::I32(-1i32 as u32)),
            WasmInstr::Ibinop(Ibinop::Add32),
            WasmInstr::Cvtop { op: Cvtop::ExtendU, dst_type: Valtype::I64, src_type: Valtype::I32 },
            WasmInstr::GetLocal(wasmir::Localidx::new(0)),
            WasmInstr::Const(Const::I32(-2i32 as u32)),
            WasmInstr::Ibinop(Ibinop::Add32),
            WasmInstr::Cvtop { op: Cvtop::ExtendU, dst_type: Valtype::I64, src_type: Valtype::I32 },
            WasmInstr::Ibinop(Ibinop::Mul64),
            WasmInstr::Const(Const::I64(1)),
            WasmInstr::Ibinop(Ibinop::ShrU64),
            WasmInstr::Cvtop { op: Cvtop::Wrap, dst_type: Valtype::I32, src_type: Valtype::I64 },
            WasmInstr::Ibinop(Ibinop::Add32),
            WasmInstr::Const(Const::I32(-1i32 as u32)),
            WasmInstr::Ibinop(Ibinop::Add32),
            WasmInstr::Return,
        ]),
        WasmInstr::Const(Const::I32(0)),
    ];

    let mut functions = vec![];
    let functype = Functype::new(vec![Valtype::I32], vec![Valtype::I32]);
    let functype_index = wasmir::Typeidx::new(0);
    for code in vec![code1, code2, code3, code4, code5] {
        let function = wasmir::Func::new(functype_index, vec![], Expr::new(code));
        functions.push(function);
    }
    Module::new(vec![functype], functions, vec![], vec![], vec![], vec![], vec![])
}

fn lower_wasm_ir_to_machine_ir(module: &Module) -> ModuleHandle {
    let mut wasm_to_machine = WasmToMachine::new();
    wasm_to_machine.emit(module);
    wasm_to_machine.finalize()
}

fn emit_x86_assembly(module: ModuleHandle) {
    let mut pass_manager = PassManager::new();
    pass_manager.add_module_pass(ModuleInitPass::create());
    pass_manager.add_function_pass(MainPass::create());
    pass_manager.run(module);
}

fn main() {
    Context::init();
    let binary_module_filename = env::args().skip(1).next().unwrap();
    let mut reader = BufReader::new(File::open(&binary_module_filename).unwrap());
    let wasm_module = match parser::parse(&mut reader) {
        Ok(m) => m,
        Err(e) => panic!("{:?}", e),
    };
    let machine_module = lower_wasm_ir_to_machine_ir(&wasm_module);
    emit_x86_assembly(machine_module);
}
