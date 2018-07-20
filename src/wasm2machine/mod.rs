use std::collections::HashMap;

use context::Context;
use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, RegisterHandle};
use machineir::opcode;
use machineir::opcode::{JumpCondKind, Opcode, UnaryOpKind};
use machineir::typ;
use machineir::typ::Type;
use machineir::operand::{Operand, OperandKind};
use wasmir;
use wasmir::{Const, Functype, Ibinop, Irelop, Itestop, Resulttype, Valtype, WasmInstr};

#[derive(Debug)]
struct OperandStack {
    stack: Vec<Operand>,
}

impl OperandStack {
    pub fn new() -> OperandStack {
        OperandStack { stack: vec![] }
    }

    fn push(&mut self, operand: Operand) {
        self.stack.push(operand)
    }

    fn pop(&mut self) -> Option<Operand> {
        self.stack.pop()
    }

    fn len(&self) -> usize {
        self.stack.len()
    }
}

#[derive(Debug)]
pub struct WasmToMachine {
    operand_stack: OperandStack,
    current_basic_block: BasicBlockHandle,
    entry_block: BasicBlockHandle,
    basic_block_to_continuation: HashMap<BasicBlockHandle, (BasicBlockHandle, Vec<RegisterHandle>)>,
    current_function: FunctionHandle,
    module: ModuleHandle,
    local_variable_types: Vec<Type>,
}

impl WasmToMachine {
    pub fn new() -> WasmToMachine {
        let dummy_block = Context::create_basic_block();
        let (parameter_types, result_types) = WasmToMachine::map_functype(&wasmir::Functype::new(vec![], vec![]));
        let dummy_function = Context::create_function("".to_string(), parameter_types, result_types);
        WasmToMachine {
            operand_stack: OperandStack::new(),
            current_basic_block: dummy_block,
            entry_block: dummy_block,
            basic_block_to_continuation: HashMap::new(),
            current_function: dummy_function,
            module: Context::create_module(),
            local_variable_types: vec![],
        }
    }

    pub fn finalize(self) -> ModuleHandle {
        self.module
    }

    fn emit_binop(&mut self, op: &wasmir::Ibinop) {
        let register = Operand::new_register(Context::create_register(Type::I32));
        let rhs = self.operand_stack.pop().unwrap();
        let lhs = self.operand_stack.pop().unwrap();
        self.operand_stack.push(register.clone());
        let opcode = match op {
            &Ibinop::Add32 => opcode::Opcode::BinaryOp {
                typ: typ::Type::I32,
                kind: opcode::BinaryOpKind::Add,
                dst: register,
                src1: lhs,
                src2: rhs,
            },
            &Ibinop::Sub32 => opcode::Opcode::BinaryOp {
                typ: typ::Type::I32,
                kind: opcode::BinaryOpKind::Sub,
                dst: register,
                src1: lhs,
                src2: rhs,
            },
        };
        self.emit_on_current_basic_block(opcode);
    }

    fn emit_if(&mut self, resulttype: &Resulttype, cond_kind: opcode::JumpCondKind, then_instrs: &Vec<WasmInstr>, else_instrs: &Vec<WasmInstr>) {
        let result_registers = WasmToMachine::setup_result_registers(resulttype);
        let then_block = Context::create_basic_block();
        let else_block = Context::create_basic_block();
        let merge_block = Context::create_basic_block();

        self.emit_on_current_basic_block(Opcode::Jump { kind: cond_kind, target: Operand::new_label(else_block) });

        self.emit_entering_block(then_block, merge_block, result_registers.clone(), then_instrs);
        self.emit_exiting_block(then_block, JumpCondKind::Unconditional,
                                true, false, false);

        self.emit_entering_block(else_block, merge_block, result_registers, else_instrs);
        self.emit_exiting_block(else_block, JumpCondKind::Unconditional,
                                true, true, true);

        self.switch_current_basic_block_to(merge_block);
    }

    fn emit_return(&mut self, result_registers: &Vec<RegisterHandle>) {
        assert!(result_registers.len() == 0 || result_registers.len() == 1);
        if result_registers.len() == 0 {
            self.emit_on_current_basic_block(Opcode::Return { result: None });
        } else if result_registers.len() == 1 {
            let result_register = result_registers[0];
            let result = Operand::new_register(result_register);
            self.emit_on_current_basic_block(Opcode::Return { result: Some(result) });
        }
    }

    fn emit_entering_block(&mut self, expr_block: BasicBlockHandle, cont_block: BasicBlockHandle,
                           result_registers: Vec<RegisterHandle>, instrs: &Vec<WasmInstr>) {
        self.basic_block_to_continuation.insert(expr_block, (cont_block, result_registers));
        self.switch_current_basic_block_to(expr_block);
        self.operand_stack.push(Operand::new_label(self.current_basic_block));
        self.emit_instrs(instrs);
    }

    fn emit_exiting_block(&mut self, entering_block: BasicBlockHandle, jump_cond_kind: JumpCondKind,
                          removes_label: bool, restores_operands: bool, replaces_registers: bool) {
        if removes_label {
            self.remove_label(entering_block);
        }
        let (cont_block, result_registers) = self.basic_block_to_continuation.get(&entering_block).unwrap().clone();
        let opcode = Opcode::Jump {
            kind: jump_cond_kind,
            target: Operand::new_label(cont_block),
        };
        self.emit_transition_procedure(opcode, result_registers, restores_operands, replaces_registers);
    }

    fn emit_body1(&mut self, wasm_instr: &WasmInstr) -> bool {
        match wasm_instr {
            &WasmInstr::Const(Const::I32(i)) => {
                let register = Operand::new_register(Context::create_register(Type::I32));
                self.operand_stack.push(register.clone());
                self.emit_on_current_basic_block(Opcode::UnaryOp {
                    typ: Type::I32,
                    kind: UnaryOpKind::Const,
                    dst: register,
                    src: Operand::new_const_i32(i),
                });
            }
            &WasmInstr::Ibinop(ref op) => self.emit_binop(op),
            &WasmInstr::Itestop(_) => unimplemented!(),
            &WasmInstr::Irelop(_) => unimplemented!(),
            &WasmInstr::Block(ref resulttype, ref instrs) => {
                let result_registers = WasmToMachine::setup_result_registers(resulttype);
                let expr_block = Context::create_basic_block();
                let cont_block = Context::create_basic_block();

                self.emit_on_current_basic_block(opcode::Opcode::Jump {
                    kind: opcode::JumpCondKind::Unconditional,
                    target: Operand::new_label(expr_block),
                });

                self.emit_entering_block(expr_block, cont_block, result_registers, instrs);
                self.emit_exiting_block(expr_block, JumpCondKind::Unconditional,
                                        true, true, true);

                self.switch_current_basic_block_to(cont_block);
            }
            &WasmInstr::If(_, _, _) => unimplemented!(),
            &WasmInstr::Loop(ref resulttype, ref instrs) => {
                let result_registers = WasmToMachine::setup_result_registers(resulttype);
                assert_eq!(result_registers.len(), 0);
                let body_block = Context::create_basic_block();
                let exit_block = Context::create_basic_block();

                self.emit_on_current_basic_block(opcode::Opcode::Jump {
                    kind: opcode::JumpCondKind::Unconditional,
                    target: Operand::new_label(body_block),
                });

                self.emit_entering_block(body_block, exit_block, result_registers, instrs);
                self.emit_on_current_basic_block(opcode::Opcode::Jump {
                    kind: opcode::JumpCondKind::Unconditional,
                    target: Operand::new_label(body_block),
                });
                self.remove_label(body_block);

                self.switch_current_basic_block_to(exit_block);
            }
            &WasmInstr::Br(index) => {
                let entering_block = self.get_label_at(index);
                self.emit_exiting_block(entering_block, JumpCondKind::Unconditional,
                                        false, true, false);

                let new_block = Context::create_basic_block();
                self.switch_current_basic_block_to(new_block);
                self.emit_on_current_basic_block(Opcode::Debug("unreachable block".to_string()));
            }
            &WasmInstr::BrIf(_) => unimplemented!(),
            &WasmInstr::Return => {
                let entering_block = self.entry_block;
                self.emit_exiting_block(entering_block, JumpCondKind::Unconditional,
                                        false, true, false);

                let new_block = Context::create_basic_block();
                self.switch_current_basic_block_to(new_block);
                self.emit_on_current_basic_block(Opcode::Debug("unreachable block".to_string()));
            }
            &WasmInstr::GetLocal(ref localidx) => {
                let index = localidx.as_index();
                let typ = Type::I32;
                let dst_reg = Operand::new_register(Context::create_register(typ.clone()));
                let src_mem = Operand::new_memory(index, typ.clone());
                self.operand_stack.push(dst_reg.clone());
                self.emit_on_current_basic_block(Opcode::Load { typ, dst: dst_reg, src: src_mem });
            }
            &WasmInstr::SetLocal(ref localidx) => {
                let index = localidx.as_index();
                let src_reg = self.operand_stack.pop().unwrap();
                let typ = src_reg.get_as_register().unwrap().get_typ().clone();
                let dst_mem = Operand::new_memory(index, typ.clone());
                self.emit_on_current_basic_block(Opcode::Store { typ, dst: dst_mem, src: src_reg });
            }
            &WasmInstr::Call(ref funcidx) => {
                let index = funcidx.as_index();
                let function = self.module.get_functions()[index];

                let mut args = vec![];
                assert!(function.get_parameter_types().len() <= self.operand_stack.len());
                for _ in 0..function.get_parameter_types().len() {
                    args.push(self.operand_stack.pop().unwrap());
                }
                args.reverse();

                assert!(function.get_result_types().len() == 0 || function.get_result_types().len() == 1);
                let result = if function.get_result_types().len() == 0 {
                    None
                } else if function.get_result_types().len() == 1 {
                    let typ = &function.get_result_types()[0];
                    let result_reg = Operand::new_register(Context::create_register(typ.clone()));
                    self.operand_stack.push(result_reg.clone());
                    Some(result_reg)
                } else {
                    unreachable!()
                };
                self.emit_on_current_basic_block(Opcode::Call {
                    func: function,
                    typ: Type::I32,
                    result,
                    args,
                });
            }
        };
        true
    }

    fn emit_body2(&mut self, wasm_instr0: &WasmInstr, wasm_instr1: &WasmInstr) -> bool {
        match (wasm_instr0, wasm_instr1) {
            (&WasmInstr::Itestop(ref op), &WasmInstr::If(ref resulttype, ref then_instrs, ref else_instrs)) => {
                let operand = self.operand_stack.pop().unwrap();
                let reg = operand.get_as_register().unwrap();
                let cond_kind = match op {
                    &Itestop::Eqz32 => JumpCondKind::Neq0(reg),
                };
                self.emit_if(resulttype, cond_kind, then_instrs, else_instrs);
            }
            (&WasmInstr::Itestop(ref op), &WasmInstr::BrIf(index)) => {
                let cond_reg = self.pop_conditional_register();
                let jump_cond_kind = match op {
                    &Itestop::Eqz32 => JumpCondKind::Eq0(cond_reg),
                };
                let entering_block = self.get_label_at(index);
                self.emit_exiting_block(entering_block, jump_cond_kind,
                                        false, true, false);

                let new_block = Context::create_basic_block();
                self.switch_current_basic_block_to(new_block);
            }
            (&WasmInstr::Irelop(ref op), &WasmInstr::If(ref resulttype, ref then_instrs, ref else_instrs)) => {
                let rhs = self.operand_stack.pop().unwrap();
                let rhs_reg = rhs.get_as_register().unwrap();
                let lhs = self.operand_stack.pop().unwrap();
                let lhs_reg = lhs.get_as_register().unwrap();
                let cond_kind = match op {
                    &Irelop::Eq32 => JumpCondKind::Neq(lhs_reg, rhs_reg),
                };
                self.emit_if(resulttype, cond_kind, then_instrs, else_instrs);
            }
            _ => return false,
        }
        true
    }

    fn switch_current_basic_block_to(&mut self, basic_block: BasicBlockHandle) {
        self.current_function.get_mut_basic_blocks().push_back(basic_block);
        self.current_basic_block = basic_block;
    }

    fn emit_on_current_basic_block(&mut self, opcode: Opcode) -> InstrHandle {
        let instr = Context::create_instr(opcode, self.current_basic_block);
        self.current_basic_block.add_instr(instr);
        instr
    }

    fn emit_instrs(&mut self, instrs: &Vec<WasmInstr>) {
        let mut i = 0;
        while i < instrs.len() {
            if i + 1 < instrs.len() {
                let wasm_instr0 = &instrs[i];
                let wasm_instr1 = &instrs[i + 1];
                if self.emit_body2(wasm_instr0, wasm_instr1) {
                    i += 2;
                    continue
                }
            }
            let wasm_instr0 = &instrs[i];
            if !self.emit_body1(wasm_instr0) {
                panic!();
            }
            i += 1;
        }
    }

    fn remove_label(&mut self, expr_block: BasicBlockHandle) {
        let mut tmp_stack = OperandStack::new();
        let mut is_valid = false;
        while let Some(operand) = self.operand_stack.pop() {
            if operand.get_kind() == &OperandKind::Label(expr_block) {
                is_valid = true;
                break;
            }
            tmp_stack.push(operand);
        }
        assert!(is_valid);

        while let Some(operand) = tmp_stack.pop() {
            self.operand_stack.push(operand);
        }
    }

    fn get_label_at(&mut self, index: usize) -> BasicBlockHandle {
        let mut num_labels = 0;
        for operand in self.operand_stack.stack.iter().rev() {
            if let &OperandKind::Label(ref bb) = operand.get_kind() {
                num_labels += 1;
                if index + 1 == num_labels {
                    return *bb;
                }
            }
        }
        panic!()
    }

    fn emit_transition_procedure(&mut self, opcode: Opcode, mut registers: Vec<RegisterHandle>,
                                 restores_operands: bool, replaces_registers: bool) {
        assert!(!(!restores_operands && replaces_registers));
        let mut tmp_operand_stack = OperandStack::new();
        while let Some(register) = registers.pop() {
            let mut operand = self.operand_stack.pop().unwrap();
            let typ = register.get_typ().clone();
            if let &OperandKind::Register(src_register) = operand.get_kind() {
                self.emit_on_current_basic_block(Opcode::Copy {
                    typ,
                    dst: Operand::new_register(register),
                    src: Operand::new_register(src_register),
                });
            } else {
                panic!()
            }
            if replaces_registers {
                operand.set_kind(OperandKind::Register(register));
            }
            tmp_operand_stack.push(operand);
        }

        self.emit_on_current_basic_block(opcode);

        if restores_operands {
            while let Some(operand) = tmp_operand_stack.pop() {
                self.operand_stack.push(operand);
            }
        }
    }

    fn create_registers_for_types(typs: Vec<Type>) -> Vec<RegisterHandle> {
        typs.into_iter().map(|t| {
            Context::create_register(t)
        }).collect()
    }

    fn setup_result_registers(resulttype: &Resulttype) -> Vec<RegisterHandle> {
        WasmToMachine::create_registers_for_types(WasmToMachine::map_resulttype(resulttype))
    }

    fn pop_conditional_register(&mut self) -> RegisterHandle {
        match self.operand_stack.pop() {
            Some(operand) => {
                match operand.get_kind() {
                    &OperandKind::Register(register) => {
                        if register.get_typ() == &Type::I32 {
                            register
                        } else {
                            panic!()
                        }
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn map_resulttype(resulttype: &Resulttype) -> Vec<Type> {
        match resulttype.peek() {
            &Some(ref vt) => vt.iter().map(|t| { WasmToMachine::map_valtype(t) }).collect(),
            &None => vec![],
        }
    }

    fn map_functype(functype: &Functype) -> (Vec<Type>, Vec<Type>) {
        let typ_in = functype.peek_in_typ().iter().map(|t| { WasmToMachine::map_valtype(t) }).collect();
        let typ_out = functype.peek_out_typ().iter().map(|t| { WasmToMachine::map_valtype(t) }).collect();
        (typ_in, typ_out)
    }

    fn map_valtype(valtype: &Valtype) -> Type {
        use wasmir::Valtype::*;
        match valtype {
            &I32 => Type::I32,
            &I64 => Type::I64,
        }
    }

    pub fn emit(&mut self, module: &wasmir::Module) {
        for (funcidx, func) in module.get_funcs().iter().enumerate() {
            let typeidx = func.get_type();
            let functype = &module.get_types()[typeidx.as_index()];
            let (parameter_types, result_types) = WasmToMachine::map_functype(&functype);
            let mut local_variable_types = parameter_types.clone();
            let func_name = format!("f_{}", funcidx);
            let function = Context::create_function(func_name, parameter_types, result_types.clone());
            self.module.get_mut_functions().push(function);
            assert_eq!(self.module.get_functions().len() - 1, funcidx);
            local_variable_types.append(&mut func.get_locals().iter()
                .map(|typ| WasmToMachine::map_valtype(typ)).collect());

            let entry_block = Context::create_basic_block();
            let exit_block = Context::create_basic_block();

            let dummy_func = self.current_function;
            let dummy_block = self.current_basic_block;

            self.entry_block = entry_block;
            self.basic_block_to_continuation = HashMap::new();
            self.current_function = function;
            self.local_variable_types = local_variable_types;

            let result_registers = WasmToMachine::create_registers_for_types(result_types);
            self.emit_entering_block(entry_block, exit_block,
                                     result_registers.clone(), func.get_body().get_instr_sequences());
            self.emit_exiting_block(entry_block, JumpCondKind::Unconditional,
                                    true, true, true);

            self.switch_current_basic_block_to(exit_block);
            self.emit_return(&result_registers);

            self.current_function = dummy_func;
            self.current_basic_block = dummy_block;
        }
    }
}
