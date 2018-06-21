use context::Context;
use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, RegisterHandle};
use machineir::basicblock::BasicBlockKind;
use machineir::opcode::Opcode;
use machineir::typ::Type;
use machineir::operand::{Operand, OperandKind};
use wasmir;
use wasmir::{Binop, Const, Functype, Ibinop, Resulttype, Valtype, WasmInstr};

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
    function: FunctionHandle,
}

impl WasmToMachine {
    pub fn new(functype: Functype) -> WasmToMachine {
        let (_in_typs, out_typs) = WasmToMachine::map_functype(&functype);
        let result_registers = WasmToMachine::create_registers_for_types(out_typs);
        let exit_block = Context::create_basic_block(BasicBlockKind::ContinuationBlock(vec![]));
        let entry_block = Context::create_basic_block(BasicBlockKind::ExprBlock(exit_block));
        let mut function = Context::create_function(WasmToMachine::map_functype(&functype).1);
        function.get_mut_basic_blocks().push_back(entry_block);
        WasmToMachine {
            operand_stack: OperandStack::new(),
            current_basic_block: entry_block,
            exit_block: exit_block,
            result_registers: result_registers,
            function: function,
        }
    }

    pub fn finalize(mut self) -> FunctionHandle {
        self.function.get_mut_basic_blocks().push_back(self.exit_block);
        assert_eq!(self.result_registers.len(), self.operand_stack.len());
        let result_registers = self.result_registers.clone();
        self.emit_copy_to_store_result(result_registers, false);
        self.function
    }

    pub fn emit_ir(&mut self, wasm_instr: &WasmInstr) {
        match wasm_instr {
            &WasmInstr::Const(Const::I32(i)) => {
                let register = Operand::new_register(Context::create_register(Type::I32));
                self.operand_stack.push(register.clone());
                self.emit_on_current_basic_block(Opcode::Const(Type::I32, register, Operand::new_const_i32(i)));
            }
            &WasmInstr::Binop(Binop::Ibinop(Ibinop::Add32)) => {
                let register = Operand::new_register(Context::create_register(Type::I32));
                let rhs = self.operand_stack.pop().unwrap();
                let lhs = self.operand_stack.pop().unwrap();
                self.operand_stack.push(register.clone());
                self.emit_on_current_basic_block(Opcode::Add(Type::I32, register, lhs, rhs));
            }
            &WasmInstr::Binop(Binop::Ibinop(Ibinop::Sub32)) => {
                let register = Operand::new_register(Context::create_register(Type::I32));
                let rhs = self.operand_stack.pop().unwrap();
                let lhs = self.operand_stack.pop().unwrap();
                self.operand_stack.push(register.clone());
                self.emit_on_current_basic_block(Opcode::Sub(Type::I32, register, lhs, rhs));
            }
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

                self.function.get_mut_basic_blocks().push_back(new_basic_block);
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

                self.function.get_mut_basic_blocks().push_back(new_basic_block);
                self.current_basic_block = new_basic_block;
            }
        }
    }

    fn emit_on_current_basic_block(&mut self, opcode: Opcode) -> InstrHandle {
        let instr = Context::create_instr(opcode, self.current_basic_block);
        self.current_basic_block.add_instr(instr);
        instr
    }

    fn emit_on_expr_basic_block(&mut self, expr_block: BasicBlockHandle, cont_block: BasicBlockHandle, instrs: &Vec<WasmInstr>) {
        self.function.get_mut_basic_blocks().push_back(expr_block);
        self.current_basic_block = expr_block;
        self.operand_stack.push(Operand::new_label(expr_block));
        for instr in instrs.iter() {
            self.emit_ir(instr);
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
        self.function.get_mut_basic_blocks().push_back(basic_block);
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
        self.emit_ir(module.get_funcs()[0].get_body())
    }
}
