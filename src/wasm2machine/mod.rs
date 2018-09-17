use std::collections::HashMap;

use context::handle::{
    BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, RegionHandle, RegisterHandle,
};
use context::Context;
use machineir::function::Linkage;
use machineir::opcode;
use machineir::opcode::{
    Address, BinaryOpKind, CallTargetKind, CastKind, ConstKind, JumpCondKind, JumpTargetKind,
    Opcode, OperandKind,
};
use machineir::region::RegionKind;
use machineir::typ::Type;
use wasmir;
use wasmir::instructions::{
    Const, Cvtop, Ibinop, Irelop, Itestop, Iunop, Loadattr, Storeattr, WasmInstr,
};
use wasmir::types::{Functype, Mut, Resulttype, Valtype};
use wasmir::{Exportdesc, Importdesc, Labelidx, Typeidx};

#[derive(Debug)]
enum StackElem {
    Value(RegisterHandle),
    Label(BasicBlockHandle),
}

#[derive(Debug)]
struct OperandStack {
    stack: Vec<StackElem>,
}

impl OperandStack {
    fn new() -> OperandStack {
        OperandStack { stack: vec![] }
    }

    fn push(&mut self, operand: StackElem) {
        self.stack.push(operand)
    }

    fn push_value(&mut self, value: RegisterHandle) {
        self.push(StackElem::Value(value))
    }

    fn push_label(&mut self, label: BasicBlockHandle) {
        self.push(StackElem::Label(label))
    }

    fn pop(&mut self) -> Option<StackElem> {
        self.stack.pop()
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    fn remove_label(&mut self, basic_block: BasicBlockHandle) -> bool {
        let mut tmp_stack = OperandStack::new();
        let mut is_valid = false;
        while let Some(operand) = self.stack.pop() {
            match operand {
                StackElem::Label(bb) if bb == basic_block => {
                    is_valid = true;
                    break;
                }
                operand => tmp_stack.push(operand),
            }
        }
        while let Some(operand) = tmp_stack.pop() {
            self.push(operand);
        }
        is_valid
    }

    fn get_label_at(&mut self, index: usize) -> Option<BasicBlockHandle> {
        let mut num_labels = 0;
        for operand in self.stack.iter().rev() {
            if let &StackElem::Label(bb) = operand {
                num_labels += 1;
                if index + 1 == num_labels {
                    return Some(bb);
                }
            }
        }
        None
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
    local_variables: Vec<RegisterHandle>,
    global_variables: Vec<(RegisterHandle, RegionHandle)>,
    function_types: Vec<(Vec<Type>, Vec<Type>)>,
    num_import_functions: usize,
}

impl WasmToMachine {
    fn declare_function(
        func_name: String,
        typeidx: Typeidx,
        linkage: Linkage,
        function_types: &Vec<(Vec<Type>, Vec<Type>)>,
        mut machine_module: ModuleHandle,
    ) {
        let (parameter_types, result_types) = function_types[typeidx.as_index()].clone();
        let function =
            Context::create_function(func_name, parameter_types, result_types, machine_module)
                .set_linkage(linkage);
        machine_module.get_mut_functions().push(function);
    }

    fn declare_functions(
        wasm_module: &wasmir::Module,
        function_types: &Vec<(Vec<Type>, Vec<Type>)>,
        machine_module: ModuleHandle,
    ) -> (usize, usize) {
        for import in wasm_module.get_imports().iter() {
            use self::Importdesc::*;
            let typeidx = match import.get_desc() {
                &Func(typeidx) => typeidx,
                _ => continue,
            };
            WasmToMachine::declare_function(
                import.get_name().to_string(),
                typeidx,
                Linkage::Import,
                function_types,
                machine_module,
            );
        }
        let num_import_functions = machine_module.get_functions().len();

        for (i, func) in wasm_module.get_funcs().iter().enumerate() {
            let typeidx = *func.get_type();
            WasmToMachine::declare_function(
                format!("f_{}", num_import_functions + i),
                typeidx,
                Linkage::Private,
                function_types,
                machine_module,
            );
        }
        let num_define_functions = machine_module.get_functions().len() - num_import_functions;

        (num_import_functions, num_define_functions)
    }

    pub fn new(wasmir_module: &wasmir::Module) -> WasmToMachine {
        let mut module = Context::create_module();

        let dummy_block = Context::create_basic_block();
        let (parameter_types, result_types) =
            WasmToMachine::map_functype(&Functype::new(vec![], vec![]));
        let dummy_function =
            Context::create_function("".to_string(), parameter_types, result_types, module);

        let mut global_variables = vec![];
        for global in wasmir_module.get_globals().iter() {
            let typ = WasmToMachine::map_valtype(global.get_type().valtype());
            let var = Context::create_register(typ);
            let mut region = match global.get_type().mutability() {
                &Mut::Var => module.get_mutable_global_variable_region(),
                &Mut::Const => module.get_const_global_variable_region(),
            };
            region.get_mut_offset_map().insert(var, 0);
            global_variables.push((var, region));
        }

        let memory = if wasmir_module.get_mems().len() == 0 {
            Context::create_region(RegionKind::DynamicGlobal { min: 0, max: None })
        } else {
            assert_eq!(wasmir_module.get_mems().len(), 1);
            let mem = &wasmir_module.get_mems()[0];
            Context::create_region(RegionKind::DynamicGlobal {
                min: mem.get_type().get_lim().get_min() as usize,
                max: mem.get_type().get_lim().get_max().map(|i| i as usize),
            })
        };
        module.get_mut_dynamic_regions().push(memory);

        let table = if wasmir_module.get_tables().len() == 0 {
            Context::create_region(RegionKind::DynamicGlobal { min: 0, max: None })
        } else {
            assert_eq!(wasmir_module.get_tables().len(), 1);
            let tab = &wasmir_module.get_tables()[0];
            Context::create_region(RegionKind::DynamicGlobal {
                min: tab.get_type().get_limits().get_min() as usize,
                max: tab.get_type().get_limits().get_max().map(|i| i as usize),
            })
        };
        module.get_mut_indirect_function_tables().push(table);

        let function_types = wasmir_module
            .get_types()
            .iter()
            .map(|functype| WasmToMachine::map_functype(functype))
            .collect();

        let (num_import_functions, _) =
            WasmToMachine::declare_functions(wasmir_module, &function_types, module);

        for export in wasmir_module.get_exports().iter() {
            match export.get_desc() {
                &Exportdesc::Func(funcidx) => {
                    let index = funcidx.as_index();
                    module.get_mut_functions()[index]
                        .set_func_name(export.get_name().clone())
                        .set_linkage(Linkage::Export);
                }
                _ => continue,
            }
        }

        WasmToMachine {
            operand_stack: OperandStack::new(),
            current_basic_block: dummy_block,
            entry_block: dummy_block,
            basic_block_to_continuation: HashMap::new(),
            current_function: dummy_function,
            module,
            local_variables: vec![],
            global_variables,
            function_types,
            num_import_functions,
        }
    }

    pub fn finalize(self) -> ModuleHandle {
        self.module
    }

    fn emit_unop(&mut self, _op: &Iunop) {
        unimplemented!()
    }

    fn emit_binop(&mut self, op: &Ibinop) {
        use self::Ibinop::*;

        let typ = match op {
            &Add32 | &Sub32 | &Mul32 | &DivU32 | &And32 | &Or32 | &Xor32 | &Shl32 | &ShrS32
            | &ShrU32 => Type::I32,
            &Mul64 | &Or64 | &Shl64 | &ShrU64 => Type::I64,
        };
        let dst = Context::create_register(typ);

        let rhs = self.operand_stack.pop().unwrap();
        let src2 = match rhs {
            StackElem::Value(reg) => OperandKind::Register(reg),
            StackElem::Label(_) => unreachable!(),
        };

        let lhs = self.operand_stack.pop().unwrap();
        let src1 = match lhs {
            StackElem::Value(reg) => reg,
            StackElem::Label(_) => unreachable!(),
        };

        self.operand_stack.push_value(dst);
        let opcode = match op {
            &Add32 => Opcode::BinaryOp {
                kind: opcode::BinaryOpKind::Add,
                dst,
                src1,
                src2,
            },
            &Sub32 => Opcode::BinaryOp {
                kind: opcode::BinaryOpKind::Sub,
                dst,
                src1,
                src2,
            },
            &Mul32 | &Mul64 => Opcode::BinaryOp {
                kind: opcode::BinaryOpKind::Mul,
                dst,
                src1,
                src2,
            },
            &DivU32 => Opcode::BinaryOp {
                kind: opcode::BinaryOpKind::Div,
                dst,
                src1,
                src2,
            },
            &And32 => Opcode::BinaryOp {
                kind: opcode::BinaryOpKind::And,
                dst,
                src1,
                src2,
            },
            &Or32 | &Or64 => Opcode::BinaryOp {
                kind: opcode::BinaryOpKind::Or,
                dst,
                src1,
                src2,
            },
            &Xor32 => Opcode::BinaryOp {
                kind: opcode::BinaryOpKind::Xor,
                dst,
                src1,
                src2,
            },
            &Shl32 => self.emit_shift_opcode_helper(BinaryOpKind::Sll, dst, src1, src2, 32),
            &ShrS32 => self.emit_shift_opcode_helper(BinaryOpKind::Sra, dst, src1, src2, 32),
            &ShrU32 => self.emit_shift_opcode_helper(BinaryOpKind::Srl, dst, src1, src2, 32),
            &Shl64 => self.emit_shift_opcode_helper(BinaryOpKind::Sll, dst, src1, src2, 64),
            &ShrU64 => self.emit_shift_opcode_helper(BinaryOpKind::Srl, dst, src1, src2, 64),
        };
        self.emit_on_current_basic_block(opcode);
    }

    fn emit_cvtop(&mut self, op: &Cvtop, dst_type: &Valtype, _src_type: &Valtype) {
        use self::Cvtop::*;

        let src = match self.operand_stack.pop().unwrap() {
            StackElem::Value(reg) => reg,
            StackElem::Label(_) => unreachable!(),
        };

        let dst_typ = match dst_type {
            &Valtype::I32 => Type::I32,
            &Valtype::I64 => Type::I64,
        };
        let dst = Context::create_register(dst_typ);

        self.operand_stack.push_value(dst);
        let opcode = match op {
            &Wrap => Opcode::Cast {
                kind: CastKind::Wrap,
                dst,
                src,
            },
            &ExtendU => Opcode::Cast {
                kind: CastKind::ZeroExtension,
                dst,
                src,
            },
            &ExtendS => Opcode::Cast {
                kind: CastKind::SignExtension,
                dst,
                src,
            },
        };
        self.emit_on_current_basic_block(opcode);
    }

    fn emit_if(
        &mut self,
        resulttype: &Resulttype,
        cond_kind: opcode::JumpCondKind,
        then_instrs: &Vec<WasmInstr>,
        else_instrs: &Vec<WasmInstr>,
    ) {
        let result_registers = WasmToMachine::setup_result_registers(resulttype);
        let then_block = Context::create_basic_block();
        let else_block = Context::create_basic_block();
        let merge_block = Context::create_basic_block();

        self.emit_on_current_basic_block(Opcode::Jump {
            kind: cond_kind,
            target: JumpTargetKind::BasicBlock(else_block),
        });

        self.emit_entering_block(
            then_block,
            merge_block,
            result_registers.clone(),
            then_instrs,
        );
        self.emit_exiting_block(then_block, JumpCondKind::Unconditional, true, false, false);

        self.emit_entering_block(else_block, merge_block, result_registers, else_instrs);
        self.emit_exiting_block(else_block, JumpCondKind::Unconditional, true, true, true);

        self.switch_current_basic_block_to(merge_block);
    }

    fn emit_br_if(&mut self, cond_kind: opcode::JumpCondKind, index: Labelidx) {
        let entering_block = self.operand_stack.get_label_at(index.as_index()).unwrap();
        self.emit_exiting_block(entering_block, cond_kind, false, true, false);
        let new_block = Context::create_basic_block();
        self.switch_current_basic_block_to(new_block);
    }

    fn emit_return(&mut self, result_registers: &Vec<RegisterHandle>) {
        assert!(result_registers.len() == 0 || result_registers.len() == 1);
        if result_registers.len() == 0 {
            self.emit_on_current_basic_block(Opcode::Return { result: None });
        } else if result_registers.len() == 1 {
            let result = result_registers[0];
            self.emit_on_current_basic_block(Opcode::Return {
                result: Some(result),
            });
        }
    }

    fn emit_entering_block(
        &mut self,
        expr_block: BasicBlockHandle,
        cont_block: BasicBlockHandle,
        result_registers: Vec<RegisterHandle>,
        instrs: &Vec<WasmInstr>,
    ) {
        self.basic_block_to_continuation
            .insert(expr_block, (cont_block, result_registers));
        self.switch_current_basic_block_to(expr_block);
        self.operand_stack.push_label(self.current_basic_block);
        self.emit_instrs(instrs);
    }

    fn emit_exiting_block(
        &mut self,
        entering_block: BasicBlockHandle,
        jump_cond_kind: JumpCondKind,
        removes_label: bool,
        restores_operands: bool,
        replaces_registers: bool,
    ) {
        if removes_label {
            self.operand_stack.remove_label(entering_block);
        }
        let (cont_block, result_registers) = self
            .basic_block_to_continuation
            .get(&entering_block)
            .unwrap()
            .clone();
        let opcode = Opcode::Jump {
            kind: jump_cond_kind,
            target: JumpTargetKind::BasicBlock(cont_block),
        };
        self.emit_transition_procedure(
            opcode,
            result_registers,
            restores_operands,
            replaces_registers,
        );
    }

    fn emit_shift_opcode_helper(
        &mut self,
        op: BinaryOpKind,
        dst: RegisterHandle,
        src_target: RegisterHandle,
        src_num_shift: OperandKind,
        num_shift_limit: usize,
    ) -> Opcode {
        assert!(op == BinaryOpKind::Sll || op == BinaryOpKind::Srl || op == BinaryOpKind::Sra);
        assert!(num_shift_limit == 32 || num_shift_limit == 64);

        let src_num_shift = match src_num_shift {
            OperandKind::Register(reg) => {
                let num_shift_reg = Context::create_register(Type::I8);
                self.emit_on_current_basic_block(Opcode::Cast {
                    kind: CastKind::Wrap,
                    dst: num_shift_reg,
                    src: reg,
                });

                let canonical_num_shift_reg = Context::create_register(Type::I8);
                self.emit_on_current_basic_block(Opcode::BinaryOp {
                    kind: BinaryOpKind::And,
                    dst: canonical_num_shift_reg,
                    src1: num_shift_reg,
                    src2: OperandKind::ImmI8(num_shift_limit as u8 - 1),
                });
                OperandKind::Register(canonical_num_shift_reg)
            }
            OperandKind::ImmI8(n) => OperandKind::ImmI8(n as u8 % num_shift_limit as u8),
            OperandKind::ImmI32(n) => OperandKind::ImmI8(n as u8 % num_shift_limit as u8),
            OperandKind::ImmI64(n) => OperandKind::ImmI8(n as u8 % num_shift_limit as u8),
        };

        Opcode::BinaryOp {
            kind: op,
            dst,
            src1: src_target,
            src2: src_num_shift,
        }
    }

    fn emit_body1(&mut self, wasm_instr: &WasmInstr) -> bool {
        match wasm_instr {
            &WasmInstr::Const(ref cst) => {
                let (typ, src) = match cst {
                    &Const::I32(i) => (Type::I32, ConstKind::ConstI32(i)),
                    &Const::I64(i) => (Type::I64, ConstKind::ConstI64(i)),
                };
                let dst = Context::create_register(typ);
                self.operand_stack.push_value(dst);
                self.emit_on_current_basic_block(Opcode::Const { dst, src });
            }
            &WasmInstr::Iunop(ref op) => self.emit_unop(op),
            &WasmInstr::Ibinop(ref op) => self.emit_binop(op),
            &WasmInstr::Itestop(_) => unimplemented!(),
            &WasmInstr::Irelop(ref op) => {
                let rhs = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let lhs = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let cond_kind = match op {
                    &Irelop::Eq32 => JumpCondKind::Neq(lhs, rhs),
                    &Irelop::Ne32 => JumpCondKind::Eq(lhs, rhs),
                    &Irelop::LtS32 => JumpCondKind::GeS(lhs, rhs),
                    &Irelop::LtU32 => JumpCondKind::GeU(lhs, rhs),
                    &Irelop::GtS32 => JumpCondKind::LeS(lhs, rhs),
                    &Irelop::GtU32 => JumpCondKind::LeU(lhs, rhs),
                    &Irelop::LeS32 => JumpCondKind::GtS(lhs, rhs),
                    &Irelop::LeU32 => JumpCondKind::GtU(lhs, rhs),
                    &Irelop::GeS32 => JumpCondKind::LtS(lhs, rhs),
                    &Irelop::GeU32 => JumpCondKind::LtU(lhs, rhs),
                };
                self.emit_if(
                    &Resulttype::new(Some(vec![Valtype::I32])),
                    cond_kind,
                    &vec![WasmInstr::Const(Const::I32(1))],
                    &vec![WasmInstr::Const(Const::I32(0))],
                );
            }
            &WasmInstr::Cvtop {
                ref op,
                ref dst_type,
                ref src_type,
            } => self.emit_cvtop(op, dst_type, src_type),
            &WasmInstr::Unreachable => unimplemented!(),
            &WasmInstr::Block(ref resulttype, ref instrs) => {
                let result_registers = WasmToMachine::setup_result_registers(resulttype);
                let expr_block = Context::create_basic_block();
                let cont_block = Context::create_basic_block();

                self.emit_on_current_basic_block(opcode::Opcode::Jump {
                    kind: opcode::JumpCondKind::Unconditional,
                    target: JumpTargetKind::BasicBlock(expr_block),
                });

                self.emit_entering_block(expr_block, cont_block, result_registers, instrs);
                self.emit_exiting_block(expr_block, JumpCondKind::Unconditional, true, true, true);

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
                    target: JumpTargetKind::BasicBlock(body_block),
                });

                self.emit_entering_block(body_block, body_block, result_registers, instrs);
                self.operand_stack.remove_label(body_block);

                self.switch_current_basic_block_to(exit_block);
            }
            &WasmInstr::Br(index) => {
                let entering_block = self.operand_stack.get_label_at(index.as_index()).unwrap();
                self.emit_exiting_block(
                    entering_block,
                    JumpCondKind::Unconditional,
                    false,
                    true,
                    false,
                );

                let new_block = Context::create_basic_block();
                self.switch_current_basic_block_to(new_block);
                self.emit_on_current_basic_block(Opcode::Debug("unreachable block".to_string()));
            }
            &WasmInstr::BrIf(index) => {
                let cond_reg = self.pop_conditional_register();
                let cond_kind = JumpCondKind::Neq0(cond_reg);
                self.emit_br_if(cond_kind, index);
            }
            &WasmInstr::BrTable { ref table, default } => {
                let index = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let blocks = table
                    .iter()
                    .map(|labelidx| {
                        self.operand_stack
                            .get_label_at(labelidx.as_index())
                            .unwrap()
                    }).collect();
                let default_block = self.operand_stack.get_label_at(default.as_index()).unwrap();
                self.emit_on_current_basic_block(Opcode::Jump {
                    kind: JumpCondKind::Table(blocks, index),
                    target: JumpTargetKind::BasicBlock(default_block),
                });
            }
            &WasmInstr::Return => {
                let entering_block = self.entry_block;
                self.emit_exiting_block(
                    entering_block,
                    JumpCondKind::Unconditional,
                    false,
                    true,
                    false,
                );

                let new_block = Context::create_basic_block();
                self.switch_current_basic_block_to(new_block);
                self.emit_on_current_basic_block(Opcode::Debug("unreachable block".to_string()));
            }
            &WasmInstr::GetLocal(ref localidx) => {
                let index = localidx.as_index();
                let var = self.local_variables[index];
                let dst = Context::create_register(var.get_typ().clone());
                self.operand_stack.push_value(dst);
                self.emit_on_current_basic_block(Opcode::Load {
                    dst,
                    src: Address::Var(var),
                });
            }
            &WasmInstr::SetLocal(ref localidx) => {
                let index = localidx.as_index();
                let var = self.local_variables[index];
                let src = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                assert_eq!(src.get_typ(), var.get_typ());
                self.emit_on_current_basic_block(Opcode::Store {
                    dst: Address::Var(var),
                    src,
                });
            }
            &WasmInstr::TeeLocal(ref localidx) => {
                let index = localidx.as_index();
                let var = self.local_variables[index];
                let src = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                assert_eq!(src.get_typ(), var.get_typ());
                self.operand_stack.push_value(src);
                self.emit_on_current_basic_block(Opcode::Store {
                    dst: Address::Var(var),
                    src,
                });
            }
            &WasmInstr::GetGlobal(globalidx) => {
                let index = globalidx.as_index();
                let var = self.global_variables[index].0;
                let dst = Context::create_register(var.get_typ().clone());
                self.operand_stack.push_value(dst);
                self.emit_on_current_basic_block(Opcode::Load {
                    dst,
                    src: Address::Var(var),
                });
            }
            &WasmInstr::SetGlobal(globalidx) => {
                let index = globalidx.as_index();
                let var = self.global_variables[index].0;
                let src = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                assert_eq!(src.get_typ(), var.get_typ());
                self.emit_on_current_basic_block(Opcode::Store {
                    dst: Address::Var(var),
                    src,
                });
            }
            &WasmInstr::Load { ref attr, ref arg } => {
                let offset = {
                    let base = match self.operand_stack.pop().unwrap() {
                        StackElem::Value(reg) => reg,
                        StackElem::Label(_) => unreachable!(),
                    };
                    let tmp = Context::create_register(Type::I64);
                    self.emit_on_current_basic_block(Opcode::Cast {
                        kind: CastKind::SignExtension,
                        dst: tmp,
                        src: base,
                    });
                    let offset = Context::create_register(Type::I64);
                    self.emit_on_current_basic_block(Opcode::BinaryOp {
                        kind: BinaryOpKind::Add,
                        dst: offset,
                        src1: tmp,
                        src2: OperandKind::ImmI64(arg.get_offset() as i64 as u64),
                    });
                    offset
                };

                let dst = match attr {
                    &Loadattr::I32 => Context::create_register(Type::I32),
                    &Loadattr::I64 => Context::create_register(Type::I64),
                    &Loadattr::I32x8S | &Loadattr::I32x8U => Context::create_register(Type::I8),
                };
                let memory_variable = self.module.get_dynamic_regions()[0].get_variable();
                self.emit_on_current_basic_block(Opcode::Load {
                    dst,
                    src: Address::RegBaseRegOffset {
                        base: memory_variable,
                        offset,
                    },
                });
                let result = match attr {
                    &Loadattr::I32 => dst,
                    &Loadattr::I64 => dst,
                    &Loadattr::I32x8S => {
                        let result = Context::create_register(Type::I32);
                        self.emit_on_current_basic_block(Opcode::Cast {
                            kind: CastKind::SignExtension,
                            dst: result,
                            src: dst,
                        });
                        result
                    }
                    &Loadattr::I32x8U => {
                        let result = Context::create_register(Type::I32);
                        self.emit_on_current_basic_block(Opcode::Cast {
                            kind: CastKind::ZeroExtension,
                            dst: result,
                            src: dst,
                        });
                        result
                    }
                };
                self.operand_stack.push_value(result);
            }
            &WasmInstr::Store { ref attr, ref arg } => {
                let value = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };

                let src = match attr {
                    &Storeattr::I32 => {
                        assert_eq!(value.get_typ(), &Type::I32);
                        value
                    }
                    &Storeattr::I64 => {
                        assert_eq!(value.get_typ(), &Type::I64);
                        value
                    }
                    &Storeattr::I32x8 => {
                        let src = Context::create_register(Type::I8);
                        self.emit_on_current_basic_block(Opcode::Cast {
                            kind: CastKind::Wrap,
                            dst: src,
                            src: value,
                        });
                        src
                    }
                };

                let offset = {
                    let base = match self.operand_stack.pop().unwrap() {
                        StackElem::Value(reg) => reg,
                        StackElem::Label(_) => unreachable!(),
                    };
                    let tmp = Context::create_register(Type::I64);
                    self.emit_on_current_basic_block(Opcode::Cast {
                        kind: CastKind::SignExtension,
                        dst: tmp,
                        src: base,
                    });
                    let offset = Context::create_register(Type::I64);
                    self.emit_on_current_basic_block(Opcode::BinaryOp {
                        kind: BinaryOpKind::Add,
                        dst: offset,
                        src1: tmp,
                        src2: OperandKind::ImmI64(arg.get_offset() as i64 as u64),
                    });
                    offset
                };

                let memory_variable = self.module.get_dynamic_regions()[0].get_variable();

                self.emit_on_current_basic_block(Opcode::Store {
                    dst: Address::RegBaseRegOffset {
                        base: memory_variable,
                        offset,
                    },
                    src,
                });
            }
            &WasmInstr::Call(ref funcidx) => {
                let index = funcidx.as_index();
                let function = self.module.get_functions()[index];

                let mut args = vec![];
                assert!(function.get_parameter_types().len() <= self.operand_stack.len());
                for _ in 0..function.get_parameter_types().len() {
                    match self.operand_stack.pop().unwrap() {
                        StackElem::Value(reg) => args.push(reg),
                        StackElem::Label(_) => unreachable!(),
                    }
                }
                args.reverse();

                assert!(
                    function.get_result_types().len() == 0
                        || function.get_result_types().len() == 1
                );
                let result = if function.get_result_types().len() == 0 {
                    None
                } else if function.get_result_types().len() == 1 {
                    let typ = function.get_result_types()[0].clone();
                    let result = Context::create_register(typ);
                    self.operand_stack.push_value(result);
                    Some(result)
                } else {
                    unreachable!()
                };
                self.emit_on_current_basic_block(Opcode::Call {
                    func: CallTargetKind::Function(function),
                    result,
                    args,
                });
            }
            &WasmInstr::CallIndirect(typeidx) => {
                let tmp_index = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let index = Context::create_register(Type::I64);
                self.emit_on_current_basic_block(Opcode::Cast {
                    kind: CastKind::SignExtension,
                    dst: index,
                    src: tmp_index,
                });

                /* ToDo: insert some checking code
                 * 1. check whether the index is valid or not
                 * 2. check whether entry of table is non-NULL or not
                 * if the result of checking is false, then invoke trap function
                 */

                let (parameter_types, result_types) =
                    &self.function_types[typeidx.as_index()].clone();

                let mut args = vec![];
                assert!(parameter_types.len() <= self.operand_stack.len());
                for _ in 0..parameter_types.len() {
                    match self.operand_stack.pop().unwrap() {
                        StackElem::Value(reg) => args.push(reg),
                        StackElem::Label(_) => unreachable!(),
                    }
                }
                args.reverse();

                assert!(result_types.len() == 0 || result_types.len() == 1);
                let result = if result_types.len() == 0 {
                    None
                } else if result_types.len() == 1 {
                    let typ = result_types[0].clone();
                    let result = Context::create_register(typ);
                    self.operand_stack.push_value(result);
                    Some(result)
                } else {
                    unreachable!()
                };

                let table_variable = self.module.get_indirect_function_tables()[0].get_variable();
                self.emit_on_current_basic_block(Opcode::Call {
                    func: CallTargetKind::Indirect(Address::RegBaseRegIndex {
                        base: table_variable,
                        index,
                        scale: Type::Pointer,
                    }),
                    result,
                    args,
                });
            }
            &WasmInstr::Drop => {
                assert!(!self.operand_stack.is_empty());
                self.operand_stack.pop();
            }
            &WasmInstr::Select => {
                let cond = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                assert_eq!(cond.get_typ(), &Type::I32);
                let val_false = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let val_true = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                assert_eq!(val_true.get_typ(), val_false.get_typ());

                let result = Context::create_register(val_true.get_typ().clone());
                let bb_true = Context::create_basic_block();
                let bb_false = Context::create_basic_block();
                let bb_merge = Context::create_basic_block();

                self.emit_on_current_basic_block(Opcode::Jump {
                    kind: JumpCondKind::Eq0(cond),
                    target: JumpTargetKind::BasicBlock(bb_true),
                });

                self.switch_current_basic_block_to(bb_true);
                self.emit_on_current_basic_block(Opcode::Copy {
                    dst: result,
                    src: val_true,
                });
                self.emit_on_current_basic_block(Opcode::Jump {
                    kind: JumpCondKind::Unconditional,
                    target: JumpTargetKind::BasicBlock(bb_merge),
                });

                self.switch_current_basic_block_to(bb_false);
                self.emit_on_current_basic_block(Opcode::Copy {
                    dst: result,
                    src: val_false,
                });
                self.emit_on_current_basic_block(Opcode::Jump {
                    kind: JumpCondKind::Unconditional,
                    target: JumpTargetKind::BasicBlock(bb_merge),
                });

                self.switch_current_basic_block_to(bb_merge);
                self.operand_stack.push_value(result);
            }
        };
        true
    }

    fn emit_body2(&mut self, wasm_instr0: &WasmInstr, wasm_instr1: &WasmInstr) -> bool {
        match (wasm_instr0, wasm_instr1) {
            (
                &WasmInstr::Itestop(ref op),
                &WasmInstr::If(ref resulttype, ref then_instrs, ref else_instrs),
            ) => {
                let reg = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
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
                self.emit_br_if(jump_cond_kind, index);
            }
            (
                &WasmInstr::Irelop(ref op),
                &WasmInstr::If(ref resulttype, ref then_instrs, ref else_instrs),
            ) => {
                let rhs = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let lhs = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let cond_kind = match op {
                    &Irelop::Eq32 => JumpCondKind::Neq(lhs, rhs),
                    &Irelop::Ne32 => JumpCondKind::Eq(lhs, rhs),
                    &Irelop::LtS32 => JumpCondKind::GeS(lhs, rhs),
                    &Irelop::LtU32 => JumpCondKind::GeU(lhs, rhs),
                    &Irelop::GtS32 => JumpCondKind::LeS(lhs, rhs),
                    &Irelop::GtU32 => JumpCondKind::LeU(lhs, rhs),
                    &Irelop::LeS32 => JumpCondKind::GtS(lhs, rhs),
                    &Irelop::LeU32 => JumpCondKind::GtU(lhs, rhs),
                    &Irelop::GeS32 => JumpCondKind::LtS(lhs, rhs),
                    &Irelop::GeU32 => JumpCondKind::LtU(lhs, rhs),
                };
                self.emit_if(resulttype, cond_kind, then_instrs, else_instrs);
            }
            (&WasmInstr::Irelop(ref op), &WasmInstr::BrIf(index)) => {
                let rhs = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let lhs = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let cond_kind = match op {
                    &Irelop::Eq32 => JumpCondKind::Eq(lhs, rhs),
                    &Irelop::Ne32 => JumpCondKind::Neq(lhs, rhs),
                    &Irelop::LtS32 => JumpCondKind::LtS(lhs, rhs),
                    &Irelop::LtU32 => JumpCondKind::LtU(lhs, rhs),
                    &Irelop::GtS32 => JumpCondKind::GtS(lhs, rhs),
                    &Irelop::GtU32 => JumpCondKind::GtU(lhs, rhs),
                    &Irelop::LeS32 => JumpCondKind::LeS(lhs, rhs),
                    &Irelop::LeU32 => JumpCondKind::LeU(lhs, rhs),
                    &Irelop::GeS32 => JumpCondKind::GeS(lhs, rhs),
                    &Irelop::GeU32 => JumpCondKind::GeU(lhs, rhs),
                };
                self.emit_br_if(cond_kind, index);
            }
            _ => return false,
        }
        true
    }

    fn switch_current_basic_block_to(&mut self, basic_block: BasicBlockHandle) {
        self.current_function
            .get_mut_basic_blocks()
            .push_back(basic_block);
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
                    continue;
                }
            }
            let wasm_instr0 = &instrs[i];
            if !self.emit_body1(wasm_instr0) {
                panic!();
            }
            i += 1;
        }
    }

    fn emit_transition_procedure(
        &mut self,
        opcode: Opcode,
        mut registers: Vec<RegisterHandle>,
        restores_operands: bool,
        replaces_registers: bool,
    ) {
        assert!(!(!restores_operands && replaces_registers));
        let mut tmp_operand_stack = OperandStack::new();
        while let Some(dst_reg) = registers.pop() {
            let src_reg = match self.operand_stack.pop().unwrap() {
                StackElem::Value(reg) => reg,
                StackElem::Label(_) => unreachable!(),
            };
            self.emit_on_current_basic_block(Opcode::Copy {
                dst: dst_reg,
                src: src_reg,
            });
            if replaces_registers {
                tmp_operand_stack.push_value(dst_reg);
            } else {
                tmp_operand_stack.push_value(src_reg);
            }
        }

        self.emit_on_current_basic_block(opcode);

        if restores_operands {
            while let Some(operand) = tmp_operand_stack.pop() {
                self.operand_stack.push(operand);
            }
        }
    }

    fn create_registers_for_types(typs: Vec<Type>) -> Vec<RegisterHandle> {
        typs.into_iter()
            .map(|t| Context::create_register(t))
            .collect()
    }

    fn setup_result_registers(resulttype: &Resulttype) -> Vec<RegisterHandle> {
        WasmToMachine::create_registers_for_types(WasmToMachine::map_resulttype(resulttype))
    }

    fn pop_conditional_register(&mut self) -> RegisterHandle {
        match self.operand_stack.pop().unwrap() {
            StackElem::Value(reg) if reg.get_typ() == &Type::I32 => reg,
            StackElem::Value(_) => unreachable!(),
            StackElem::Label(_) => unreachable!(),
        }
    }

    fn map_resulttype(resulttype: &Resulttype) -> Vec<Type> {
        match resulttype.peek() {
            &Some(ref vt) => vt.iter().map(|t| WasmToMachine::map_valtype(t)).collect(),
            &None => vec![],
        }
    }

    fn map_functype(functype: &Functype) -> (Vec<Type>, Vec<Type>) {
        let typ_in = functype
            .peek_in_typ()
            .iter()
            .map(|t| WasmToMachine::map_valtype(t))
            .collect();
        let typ_out = functype
            .peek_out_typ()
            .iter()
            .map(|t| WasmToMachine::map_valtype(t))
            .collect();
        (typ_in, typ_out)
    }

    fn map_valtype(valtype: &Valtype) -> Type {
        use self::Valtype::*;
        match valtype {
            &I32 => Type::I32,
            &I64 => Type::I64,
        }
    }

    fn initialize_global_variables(&mut self, module: &wasmir::Module) {
        for (i, global) in module.get_globals().iter().enumerate() {
            let expr = global.get_init();
            assert_eq!(expr.get_instr_sequences().len(), 1);
            match &expr.get_instr_sequences()[0] {
                &WasmInstr::Const(ref cst) => {
                    let (typ, src) = match cst {
                        &Const::I32(i) => (Type::I32, ConstKind::ConstI32(i)),
                        &Const::I64(i) => (Type::I64, ConstKind::ConstI64(i)),
                    };
                    let dst = self.global_variables[i].0;
                    assert_eq!(&typ, dst.get_typ());
                    let val = Opcode::Const { dst, src };
                    self.global_variables[i]
                        .1
                        .get_mut_initial_value_map()
                        .insert(dst, val);
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn emit(&mut self, module: &wasmir::Module) {
        self.initialize_global_variables(module);
        for (i, func) in module.get_funcs().iter().enumerate() {
            let function = self.module.get_functions()[self.num_import_functions + i];
            let mut local_variables = function.get_parameter_variables().clone();
            for valtype in func.get_locals().iter() {
                let typ = WasmToMachine::map_valtype(valtype);
                let var = Context::create_register(typ);
                function
                    .get_local_region()
                    .get_mut_offset_map()
                    .insert(var, 0);
                local_variables.push(var);
            }

            let entry_block = Context::create_basic_block();
            let exit_block = Context::create_basic_block();

            let dummy_func = self.current_function;
            let dummy_block = self.current_basic_block;

            self.entry_block = entry_block;
            self.basic_block_to_continuation = HashMap::new();
            self.current_function = function;
            self.local_variables = local_variables;

            let result_registers =
                WasmToMachine::create_registers_for_types(function.get_result_types().clone());
            self.emit_entering_block(
                entry_block,
                exit_block,
                result_registers.clone(),
                func.get_body().get_instr_sequences(),
            );
            self.emit_exiting_block(entry_block, JumpCondKind::Unconditional, true, true, true);

            self.switch_current_basic_block_to(exit_block);
            self.emit_return(&result_registers);

            self.current_function = dummy_func;
            self.current_basic_block = dummy_block;
        }
    }
}
