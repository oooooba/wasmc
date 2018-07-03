use context::Context;
use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, RegisterHandle};
use machineir::basicblock::BasicBlockKind;
use machineir::opcode;
use machineir::opcode::Opcode;
use machineir::typ;
use machineir::typ::Type;
use machineir::operand::{Operand, OperandKind};
use wasmir;
use wasmir::{Binop, Const, Functype, Resulttype, Valtype, WasmInstr};

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
    exit_block: BasicBlockHandle,
    result_registers: Vec<RegisterHandle>,
    current_function: FunctionHandle,
    module: ModuleHandle,
}

impl WasmToMachine {
    pub fn new() -> WasmToMachine {
        let dummy_block = Context::create_basic_block(BasicBlockKind::ContinuationBlock(vec![]));
        let (parameter_types, result_types) = WasmToMachine::map_functype(&wasmir::Functype::new(vec![], vec![]));
        let dummy_function = Context::create_function("".to_string(), parameter_types, result_types);
        WasmToMachine {
            operand_stack: OperandStack::new(),
            current_basic_block: dummy_block,
            exit_block: dummy_block,
            result_registers: vec![],
            current_function: dummy_function,
            module: Context::create_module(),
        }
    }

    pub fn finalize(self) -> ModuleHandle {
        self.module
    }

    fn emit_binop(&mut self, op: &Binop) {
        let register = Operand::new_register(Context::create_register(Type::I32));
        let rhs = self.operand_stack.pop().unwrap();
        let lhs = self.operand_stack.pop().unwrap();
        self.operand_stack.push(register.clone());
        let opcode = match op {
            &wasmir::Binop::Ibinop(wasmir::Ibinop::Add32) => opcode::Opcode::BinaryOp {
                typ: typ::Type::I32,
                kind: opcode::BinaryOpKind::Add,
                dst: register,
                src1: lhs,
                src2: rhs,
            },
            &wasmir::Binop::Ibinop(wasmir::Ibinop::Sub32) => opcode::Opcode::BinaryOp {
                typ: typ::Type::I32,
                kind: opcode::BinaryOpKind::Sub,
                dst: register,
                src1: lhs,
                src2: rhs,
            },
            &wasmir::Binop::Irelop(wasmir::Irelop::Eq32) => Opcode::Eq(Type::I32, register, lhs, rhs),
        };
        self.emit_on_current_basic_block(opcode);
    }

    fn emit_body(&mut self, wasm_instr: &WasmInstr) {
        match wasm_instr {
            &WasmInstr::Const(Const::I32(i)) => {
                let register = Operand::new_register(Context::create_register(Type::I32));
                self.operand_stack.push(register.clone());
                self.emit_on_current_basic_block(Opcode::Const(Type::I32, register, Operand::new_const_i32(i)));
            }
            &WasmInstr::Binop(ref op) => self.emit_binop(op),
            &WasmInstr::Block(ref resulttype, ref instrs) => {
                let result_registers = WasmToMachine::setup_result_registers(resulttype);
                let cont_block = Context::create_basic_block(BasicBlockKind::ContinuationBlock(result_registers));
                let expr_block = Context::create_basic_block(BasicBlockKind::ExprBlock(cont_block));

                self.emit_on_expr_basic_block(expr_block, cont_block, instrs);
                self.reset_for_continuation(cont_block);
            }
            &WasmInstr::If(ref resulttype, ref then_instrs, ref else_instrs) => {
                let cond_reg = self.pop_conditional_register();
                let result_registers = WasmToMachine::setup_result_registers(resulttype);
                let merge_block = Context::create_basic_block(BasicBlockKind::ContinuationBlock(result_registers));
                let then_block = Context::create_basic_block(BasicBlockKind::ExprBlock(merge_block));
                let else_block = Context::create_basic_block(BasicBlockKind::ExprBlock(merge_block));

                self.emit_on_current_basic_block(Opcode::BrIfZero(Operand::new_register(cond_reg), Operand::new_label(else_block)));

                self.emit_on_expr_basic_block(then_block, merge_block, then_instrs);
                self.emit_on_expr_basic_block(else_block, merge_block, else_instrs);
                self.reset_for_continuation(merge_block);
            }
            &WasmInstr::Loop(ref resulttype, ref instrs) => {
                let result_registers = WasmToMachine::setup_result_registers(resulttype);
                assert_eq!(result_registers.len(), 0);
                let exit_block = Context::create_basic_block(BasicBlockKind::ContinuationBlock(result_registers));
                let body_block = Context::create_basic_block(BasicBlockKind::ExprBlock(exit_block));

                self.emit_on_current_basic_block(Opcode::Br(Operand::new_label(body_block)));

                self.emit_on_expr_basic_block(body_block, body_block, instrs);
                self.reset_for_continuation(exit_block);
            }
            &WasmInstr::Br(index) => {
                let target_block = self.get_label_at(index);
                self.emit_copy_for_transition(target_block);
                let target_cont_block = *target_block.get_continuation_block().unwrap();
                self.emit_on_current_basic_block(Opcode::Br(Operand::new_label(target_cont_block)));

                let current_cont_block = *self.current_basic_block.get_continuation_block().unwrap();
                let new_basic_block = Context::create_basic_block(BasicBlockKind::ExprBlock(current_cont_block));

                self.current_function.get_mut_basic_blocks().push_back(new_basic_block);
                self.current_basic_block = new_basic_block;
                self.emit_on_current_basic_block(Opcode::Debug("unreachable block".to_string()));
            }
            &WasmInstr::BrIf(index) => {
                let cond_reg = self.pop_conditional_register();
                let target_block = self.get_label_at(index);
                self.emit_copy_for_transition(target_block);
                let target_cont_block = *target_block.get_continuation_block().unwrap();
                self.emit_on_current_basic_block(Opcode::BrIfNonZero(Operand::new_register(cond_reg), Operand::new_label(target_cont_block)));

                let current_cont_block = *self.current_basic_block.get_continuation_block().unwrap();
                let new_basic_block = Context::create_basic_block(BasicBlockKind::ExprBlock(current_cont_block));

                self.current_function.get_mut_basic_blocks().push_back(new_basic_block);
                self.current_basic_block = new_basic_block;
            }
            &WasmInstr::Return => {
                assert_eq!(self.result_registers.len(), self.operand_stack.len());
                assert!(self.result_registers.len() == 0 || self.result_registers.len() == 1);
                if self.result_registers.len() == 0 {
                    self.emit_on_current_basic_block(Opcode::Return(Type::I32, None));
                } else if self.result_registers.len() == 1 {
                    let result_registers = self.result_registers.clone();
                    let result_register = result_registers[0];
                    self.emit_copy_to_store_result(result_registers, false);
                    let result = Operand::new_register(result_register);
                    self.emit_on_current_basic_block(Opcode::Return(Type::I32, Some(result)));
                }
            }
            &WasmInstr::GetLocal(ref localidx) => {
                let index = localidx.as_index();
                let typ = Type::I32;
                let dst_reg = Operand::new_register(Context::create_register(typ.clone()));
                let src_mem = Operand::new_memory(index, typ.clone());
                self.operand_stack.push(dst_reg.clone());
                self.emit_on_current_basic_block(Opcode::Load(typ, dst_reg, src_mem));
            }
            &WasmInstr::Call(ref funcidx) => {
                let index = funcidx.as_index();
                let funcname = format!("f_{}", index);
                let function = *self.module.get_functions().get(&funcname).unwrap();

                let mut args = vec![];
                assert!(function.get_parameter_types().len() <= self.operand_stack.len());
                for _ in 0..function.get_parameter_types().len() {
                    args.push(self.operand_stack.pop().unwrap());
                }
                args.reverse();

                assert!(function.get_result_types().len() == 0 || function.get_result_types().len() == 1);
                if function.get_result_types().len() == 0 {
                    self.emit_on_current_basic_block(Opcode::Call(funcname, Type::I32, None, args));
                } else if function.get_result_types().len() == 1 {
                    let typ = &function.get_result_types()[0];
                    let result_reg = Operand::new_register(Context::create_register(typ.clone()));
                    self.operand_stack.push(result_reg.clone());
                    self.emit_on_current_basic_block(Opcode::Call(funcname, Type::I32, Some(result_reg), args));
                }
            }
        }
    }

    fn emit_on_current_basic_block(&mut self, opcode: Opcode) -> InstrHandle {
        let instr = Context::create_instr(opcode, self.current_basic_block);
        self.current_basic_block.add_instr(instr);
        instr
    }

    fn emit_on_expr_basic_block(&mut self, expr_block: BasicBlockHandle, cont_block: BasicBlockHandle, instrs: &Vec<WasmInstr>) {
        self.current_function.get_mut_basic_blocks().push_back(expr_block);
        self.current_basic_block = expr_block;
        self.operand_stack.push(Operand::new_label(expr_block));
        for instr in instrs.iter() {
            self.emit_body(instr);
        }
        self.finalize_basic_block(expr_block);

        let num_results = cont_block.get_result_registers()
            .map_or(0, |r| r.len()); // 0 for loop
        self.emit_on_current_basic_block(Opcode::Br(Operand::new_label(cont_block)));
        for _ in 0..num_results {
            self.operand_stack.pop();
        }
    }

    fn reset_for_continuation(&mut self, basic_block: BasicBlockHandle) {
        let result_registers = basic_block.get_result_registers().unwrap();
        for register in result_registers.iter() {
            self.operand_stack.push(Operand::new_register(*register));
        }
        self.current_function.get_mut_basic_blocks().push_back(basic_block);
        self.current_basic_block = basic_block;
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

    fn emit_copy_to_store_result(&mut self, mut registers: Vec<RegisterHandle>, restores_operands: bool) {
        let mut tmp_operand_stack = OperandStack::new();
        while let Some(register) = registers.pop() {
            let operand = self.operand_stack.pop().unwrap();
            tmp_operand_stack.push(operand.clone());
            let typ = register.get_typ().clone();
            if let &OperandKind::Register(src_register) = operand.get_kind() {
                self.emit_on_current_basic_block(Opcode::Copy(typ, Operand::new_register(register), Operand::new_register(src_register)));
            } else {
                unimplemented!()
            }
        }
        if restores_operands {
            while let Some(operand) = tmp_operand_stack.pop() {
                self.operand_stack.push(operand);
            }
        }
    }

    fn emit_copy_for_transition(&mut self, basic_block: BasicBlockHandle) {
        let merge_block = *basic_block.get_continuation_block().unwrap();
        let registers = merge_block.get_result_registers().unwrap().clone();
        self.emit_copy_to_store_result(registers, true);
    }

    fn finalize_basic_block(&mut self, basic_block: BasicBlockHandle) {
        let mut tmp_stack = OperandStack::new();
        let mut is_valid = false;
        while let Some(operand) = self.operand_stack.pop() {
            if operand.get_kind() == &OperandKind::Label(basic_block) {
                is_valid = true;
                break;
            }
            tmp_stack.push(operand);
        }

        assert!(is_valid);

        while let Some(operand) = tmp_stack.pop() {
            self.operand_stack.push(operand);
        }

        self.emit_copy_for_transition(basic_block);
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
            &U32 => Type::I32,
        }
    }

    pub fn emit(&mut self, module: &wasmir::Module) {
        for (funcidx, func) in module.get_funcs().iter().enumerate() {
            let typeidx = func.get_type();
            let functype = &module.get_types()[typeidx.as_index()];
            let (_in_typs, out_typs) = WasmToMachine::map_functype(&functype);
            let result_registers = WasmToMachine::create_registers_for_types(out_typs);
            let exit_block = Context::create_basic_block(BasicBlockKind::ContinuationBlock(vec![]));
            let entry_block = Context::create_basic_block(BasicBlockKind::ExprBlock(exit_block));
            let dummy_func = self.current_function;
            let (parameter_types, result_types) = WasmToMachine::map_functype(&functype);
            let func_name = format!("f_{}", funcidx);
            let function = Context::create_function(func_name.clone(), parameter_types, result_types);
            self.module.get_mut_functions().insert(func_name, function);

            self.current_basic_block = entry_block;
            self.exit_block = exit_block;
            self.result_registers = result_registers;
            self.current_function = function;

            self.current_function.get_mut_basic_blocks().push_back(entry_block);
            for instr in func.get_body().get_instr_sequences() {
                self.emit_body(instr)
            }
            self.current_function.get_mut_basic_blocks().push_back(self.exit_block);

            self.current_function = dummy_func;
        }
    }
}
