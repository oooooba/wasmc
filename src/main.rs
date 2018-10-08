extern crate wasmc;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufReader;
use std::iter::FromIterator;

use wasmc::allocation::SimpleRegisterAllocationPass;
use wasmc::asmprinter::{EmitAssemblyFunctionPass, InsertBasicBlockLabelPass, ModuleInitPass};
use wasmc::context::handle::{ModuleHandle, RegisterHandle};
use wasmc::context::Context;
use wasmc::machineir::typ::Type;
use wasmc::parser;
use wasmc::pass::ModulePass;
use wasmc::wasm2machine::WasmToMachine;
use wasmc::wasmir;
use wasmc::wasmir::instructions::{Const, Cvtop, Expr, Ibinop, Irelop, Itestop, WasmInstr};
use wasmc::wasmir::types::{Functype, Resulttype, Valtype};
use wasmc::wasmir::{Export, Exportdesc, Funcidx, Labelidx, Module};

pub struct MainPass {
    registers: Vec<HashMap<Type, RegisterHandle>>,
    argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    result_register: HashMap<Type, RegisterHandle>,
    base_pointer_register: RegisterHandle,
    stack_pointer_register: RegisterHandle,
    instruction_pointer_register: RegisterHandle,
    register_name_map: HashMap<RegisterHandle, &'static str>,
}

impl ModulePass for MainPass {
    fn do_action(&mut self, module: ModuleHandle) {
        module.apply_function_pass(&mut SimpleRegisterAllocationPass::new(
            self.registers.clone(),
            self.argument_registers.clone(),
            self.result_register.clone(),
            self.base_pointer_register,
            self.stack_pointer_register,
        ));
        module.apply_basic_block_pass(&mut InsertBasicBlockLabelPass::new());
        module.apply_function_pass(&mut EmitAssemblyFunctionPass::new(
            self.register_name_map.clone(),
            self.base_pointer_register,
            self.stack_pointer_register,
            self.instruction_pointer_register,
            self.argument_registers.clone(),
        ));
    }
}

impl MainPass {
    pub fn new() -> MainPass {
        let reg_al = Context::create_register(Type::I8).set_physical();
        let reg_eax = Context::create_register(Type::I32).set_physical();
        let reg_rax = Context::create_register(Type::I64).set_physical();
        let reg_ebx = Context::create_register(Type::I32).set_physical();
        let reg_rbx = Context::create_register(Type::I64).set_physical();
        let reg_ecx = Context::create_register(Type::I32).set_physical();
        let reg_rcx = Context::create_register(Type::I64).set_physical();
        let reg_cl = Context::create_register(Type::I8).set_physical();
        let reg_edx = Context::create_register(Type::I32).set_physical();
        let reg_rdx = Context::create_register(Type::I64).set_physical();
        let reg_edi = Context::create_register(Type::I32).set_physical();
        let reg_rdi = Context::create_register(Type::I64).set_physical();
        let reg_esi = Context::create_register(Type::I32).set_physical();
        let reg_rsi = Context::create_register(Type::I64).set_physical();
        let reg_r8d = Context::create_register(Type::I32).set_physical();
        let reg_r8 = Context::create_register(Type::I64).set_physical();
        let reg_r9d = Context::create_register(Type::I32).set_physical();
        let reg_r9 = Context::create_register(Type::I64).set_physical();
        let reg_rbp = Context::create_register(Type::I64).set_physical();
        let reg_rsp = Context::create_register(Type::I64).set_physical();
        let reg_rip = Context::create_register(Type::Pointer).set_physical();

        let registers = vec![
            HashMap::from_iter(vec![
                (Type::I8, reg_al),
                (Type::I32, reg_eax),
                (Type::I64, reg_rax),
                (Type::Pointer, reg_rax),
            ]),
            HashMap::from_iter(vec![
                (Type::I32, reg_ebx),
                (Type::I64, reg_rbx),
                (Type::Pointer, reg_rbx),
            ]),
            HashMap::from_iter(vec![
                (Type::I8, reg_cl),
                (Type::I32, reg_ecx),
                (Type::I64, reg_rcx),
                (Type::Pointer, reg_rcx),
            ]),
        ];

        let argument_registers = vec![
            HashMap::from_iter(vec![(Type::I32, reg_edi), (Type::I64, reg_rdi)]),
            HashMap::from_iter(vec![(Type::I32, reg_esi), (Type::I64, reg_rsi)]),
            HashMap::from_iter(vec![(Type::I32, reg_edx), (Type::I64, reg_rdx)]),
            HashMap::from_iter(vec![(Type::I32, reg_ecx), (Type::I64, reg_rcx)]),
            HashMap::from_iter(vec![(Type::I32, reg_r8d), (Type::I64, reg_r8)]),
            HashMap::from_iter(vec![(Type::I32, reg_r9d), (Type::I64, reg_r9)]),
        ];

        let result_register = HashMap::from_iter(vec![
            (Type::I8, reg_al),
            (Type::I32, reg_eax),
            (Type::I64, reg_rax),
            (Type::Pointer, reg_rax),
        ]);

        let register_name_map = HashMap::from_iter(vec![
            (reg_al, "al"),
            (reg_eax, "eax"),
            (reg_rax, "rax"),
            (reg_ebx, "ebx"),
            (reg_rbx, "rbx"),
            (reg_ecx, "ecx"),
            (reg_rcx, "rcx"),
            (reg_cl, "cl"),
            (reg_edx, "edx"),
            (reg_rdx, "rdx"),
            (reg_edi, "edi"),
            (reg_rdi, "rdi"),
            (reg_esi, "esi"),
            (reg_rsi, "rsi"),
            (reg_r8d, "r8d"),
            (reg_r8, "r8"),
            (reg_r9d, "r9d"),
            (reg_r9, "r9"),
            (reg_rbp, "rbp"),
            (reg_rsp, "rsp"),
            (reg_rip, "rip"),
        ]);

        MainPass {
            registers,
            argument_registers,
            result_register,
            base_pointer_register: reg_rbp,
            stack_pointer_register: reg_rsp,
            instruction_pointer_register: reg_rip,
            register_name_map,
        }
    }
}

#[allow(dead_code)]
fn create_test_wasm_ir() -> Module {
    let code1 = vec![WasmInstr::Block(
        Resulttype::new(Some(vec![Valtype::I32])),
        vec![WasmInstr::Block(
            Resulttype::new(Some(vec![Valtype::I32])),
            vec![
                WasmInstr::Block(
                    Resulttype::new(Some(vec![Valtype::I32])),
                    vec![WasmInstr::Const(Const::I32(1))],
                ),
                WasmInstr::Const(Const::I32(0)),
                WasmInstr::Const(Const::I32(10)),
                WasmInstr::Irelop(Irelop::Eq32),
                WasmInstr::If(
                    Resulttype::new(Some(vec![Valtype::I32])),
                    vec![WasmInstr::Const(Const::I32(1))],
                    vec![WasmInstr::Const(Const::I32(2))],
                ),
                WasmInstr::Ibinop(Ibinop::Add32),
            ],
        )],
    )];
    let code2 = vec![WasmInstr::Block(
        Resulttype::new(Some(vec![Valtype::I32])),
        vec![WasmInstr::Block(
            Resulttype::new(Some(vec![Valtype::I32])),
            vec![
                WasmInstr::Const(Const::I32(0)),
                WasmInstr::Itestop(Itestop::Eqz32),
                WasmInstr::If(
                    Resulttype::new(Some(vec![Valtype::I32])),
                    vec![WasmInstr::Const(Const::I32(1))],
                    vec![WasmInstr::Const(Const::I32(2))],
                ),
                WasmInstr::Block(
                    Resulttype::new(Some(vec![Valtype::I32])),
                    vec![WasmInstr::Const(Const::I32(1))],
                ),
                WasmInstr::Ibinop(Ibinop::Add32),
            ],
        )],
    )];
    let code3 = vec![
        WasmInstr::GetLocal(wasmir::Localidx::new(0)),
        WasmInstr::Const(Const::I32(0)),
        WasmInstr::Irelop(Irelop::Eq32),
        WasmInstr::If(
            Resulttype::new(Some(vec![Valtype::I32])),
            vec![WasmInstr::Const(Const::I32(0))],
            vec![
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::Const(Const::I32(1)),
                WasmInstr::Ibinop(Ibinop::Sub32),
                WasmInstr::Call(wasmir::Funcidx::new(2)),
                WasmInstr::Ibinop(Ibinop::Add32),
            ],
        ),
    ];
    let code4 = vec![
        WasmInstr::Block(
            Resulttype::new(Some(vec![])),
            vec![
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::Itestop(Itestop::Eqz32),
                WasmInstr::BrIf(Labelidx::new(0)),
                WasmInstr::Const(Const::I32(1)),
                WasmInstr::Return,
            ],
        ),
        WasmInstr::Const(Const::I32(0)),
    ];
    let code5 = vec![
        WasmInstr::Block(
            Resulttype::new(Some(vec![])),
            vec![
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::Itestop(Itestop::Eqz32),
                WasmInstr::BrIf(Labelidx::new(0)),
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::Const(Const::I32(1)),
                WasmInstr::Ibinop(Ibinop::Shl32),
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::Const(Const::I32(-1i32 as u32)),
                WasmInstr::Ibinop(Ibinop::Add32),
                WasmInstr::Cvtop {
                    op: Cvtop::ExtendU,
                    dst_type: Valtype::I64,
                    src_type: Valtype::I32,
                },
                WasmInstr::GetLocal(wasmir::Localidx::new(0)),
                WasmInstr::Const(Const::I32(-2i32 as u32)),
                WasmInstr::Ibinop(Ibinop::Add32),
                WasmInstr::Cvtop {
                    op: Cvtop::ExtendU,
                    dst_type: Valtype::I64,
                    src_type: Valtype::I32,
                },
                WasmInstr::Ibinop(Ibinop::Mul64),
                WasmInstr::Const(Const::I64(1)),
                WasmInstr::Ibinop(Ibinop::ShrU64),
                WasmInstr::Cvtop {
                    op: Cvtop::Wrap,
                    dst_type: Valtype::I32,
                    src_type: Valtype::I64,
                },
                WasmInstr::Ibinop(Ibinop::Add32),
                WasmInstr::Const(Const::I32(-1i32 as u32)),
                WasmInstr::Ibinop(Ibinop::Add32),
                WasmInstr::Return,
            ],
        ),
        WasmInstr::Const(Const::I32(0)),
    ];

    let mut functions = vec![];
    let functype = Functype::new(vec![Valtype::I32], vec![Valtype::I32]);
    let functype_index = wasmir::Typeidx::new(0);
    for code in vec![code1, code2, code3, code4, code5] {
        let function = wasmir::Func::new(functype_index, vec![], Expr::new(code));
        functions.push(function);
    }
    Module::new(
        vec![functype],
        functions,
        vec![],
        vec![],
        vec![],
        vec![
            Export::new("f_0".to_string(), Exportdesc::Func(Funcidx::new(0))),
            Export::new("f_1".to_string(), Exportdesc::Func(Funcidx::new(1))),
            Export::new("f_2".to_string(), Exportdesc::Func(Funcidx::new(2))),
            Export::new("f_3".to_string(), Exportdesc::Func(Funcidx::new(3))),
            Export::new("f_4".to_string(), Exportdesc::Func(Funcidx::new(4))),
        ],
        vec![],
        vec![],
    )
}

fn lower_wasm_ir_to_machine_ir(module: &Module) -> ModuleHandle {
    let mut wasm_to_machine = WasmToMachine::new(module);
    wasm_to_machine.emit(module);
    wasm_to_machine.finalize()
}

fn emit_x86_assembly(module: ModuleHandle) {
    module.apply_module_pass(&mut ModuleInitPass::new());
    module.apply_module_pass(&mut MainPass::new());
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
