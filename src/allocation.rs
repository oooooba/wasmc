use std::collections::HashMap;

use context::Context;
use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, RegisterHandle};
use machineir::opcode::{BinaryOpKind, JumpCondKind, Opcode, UnaryOpKind};
use machineir::operand::{Operand, OperandKind};
use machineir::typ::Type;
use pass::{BasicBlockPass, FunctionPass, InstrPass, ModulePass};

#[derive(Debug)]
pub struct SimpleRegisterAllocationPass {
    physical_registers: Vec<HashMap<Type, RegisterHandle>>,
    physical_argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    physical_result_register: RegisterHandle,
    virtual_register_indexes: HashMap<RegisterHandle, usize>,
}

impl FunctionPass for SimpleRegisterAllocationPass {
    fn do_action(&mut self, mut function: FunctionHandle) {
        for basic_block_i in 0..function.get_mut_basic_blocks().len() {
            let mut basic_block = function.get_mut_basic_blocks()[basic_block_i];

            let mut iter = basic_block.iterator();
            while let Some(mut instr) = iter.get() {
                let (new_opcode, num_advance) = match instr.get_opcode() {
                    &Opcode::Copy { ref dst, ref src } => {
                        let new_src = match src.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.physical_result_register;
                                let store_instr = self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (Some(Opcode::Copy { dst: new_dst, src: new_src }), 1)
                    }
                    &Opcode::UnaryOp { ref kind, ref dst, ref src } => {
                        let new_src = match src.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            &OperandKind::ConstI32(_) => src.clone(),
                            _ => unimplemented!(),
                        };

                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.physical_result_register;
                                let store_instr = self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (Some(Opcode::UnaryOp { kind: kind.clone(), dst: new_dst, src: new_src }), 1)
                    }
                    &Opcode::BinaryOp { ref kind, ref dst, ref src1, ref src2 } => {
                        let new_src1 = match src1.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        let new_src2 = match src2.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 1);
                                let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.physical_result_register;
                                let store_instr = self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (Some(Opcode::BinaryOp { kind: kind.clone(), dst: new_dst, src1: new_src1, src2: new_src2 }), 1)
                    }
                    &Opcode::Load { ref dst, ref src } => {
                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.physical_result_register;
                                let store_instr = self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (Some(Opcode::Load { dst: new_dst, src: src.clone() }), 1)
                    }
                    &Opcode::Store { ref dst, ref src } => {
                        let new_src = match src.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            &OperandKind::ConstI32(_) => src.clone(),
                            _ => unimplemented!(),
                        };

                        (Some(Opcode::Store { dst: dst.clone(), src: new_src }), 0)
                    }
                    &Opcode::Jump { ref kind, ref target } => {
                        use self::JumpCondKind::*;
                        let new_cond_kind = match kind {
                            &Unconditional => Unconditional,
                            &Eq0(reg) | &Neq0(reg) => {
                                assert!(!reg.is_physical());
                                let preg = self.allocate_physical_register(reg, 0);
                                let load_instr = self.create_load_instr(basic_block, preg, reg, function);
                                iter.insert_before(load_instr);
                                match kind {
                                    &Eq0(_) => Eq0(preg),
                                    &Neq0(_) => Neq0(preg),
                                    _ => unreachable!(),
                                }
                            }
                            &Neq(reg1, reg2) => {
                                assert!(!reg1.is_physical());
                                let preg1 = self.allocate_physical_register(reg1, 0);
                                let load_instr1 = self.create_load_instr(basic_block, preg1, reg1, function);
                                iter.insert_before(load_instr1);

                                assert!(!reg2.is_physical());
                                let preg2 = self.allocate_physical_register(reg2, 1);
                                let load_instr2 = self.create_load_instr(basic_block, preg2, reg2, function);
                                iter.insert_before(load_instr2);

                                Neq(preg1, preg2)
                            }
                        };

                        (Some(Opcode::Jump { kind: new_cond_kind, target: target.clone() }), 0)
                    }
                    &Opcode::Call { ref func, ref result, ref args } => {
                        let mut new_args = vec![];
                        for (i, arg) in args.iter().enumerate() {
                            let vreg = arg.get_as_register().unwrap();
                            let preg = self.allocate_physical_argument_register(vreg, i);
                            let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                            iter.insert_before(load_instr);
                            new_args.push(Operand::new_physical_register(preg));
                        }

                        let new_result = if let &Some(ref result) = result {
                            let vreg = result.get_as_register().unwrap();
                            let preg = self.physical_result_register;
                            let store_instr = self.create_store_instr(basic_block, vreg, preg, function);
                            iter.insert_after(store_instr);
                            Some(Operand::new_physical_register(preg))
                        } else {
                            None
                        };

                        (Some(Opcode::Call { func: func.clone(), result: new_result, args: new_args }), 1)
                    }
                    &Opcode::Return { result: Some(ref result) } => {
                        let vreg = result.get_as_register().unwrap();
                        let preg = self.physical_result_register;
                        let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                        iter.insert_before(load_instr);
                        let new_result = Operand::new_physical_register(preg);

                        (Some(Opcode::Return { result: Some(new_result) }), 0)
                    }
                    _ => (None, 0),
                };

                if let Some(new_opcode) = new_opcode {
                    instr.set_opcode(new_opcode);
                }

                iter.advance(); // current instr to next instr
                for _ in 0..num_advance { // skip inserted instrs
                    iter.advance();
                }
            }
        }
    }
}

impl SimpleRegisterAllocationPass {
    pub fn create(
        physical_registers: Vec<HashMap<Type, RegisterHandle>>,
        physical_argument_registers: Vec<HashMap<Type, RegisterHandle>>,
        physical_result_register: RegisterHandle) -> Box<SimpleRegisterAllocationPass> {
        Box::new(SimpleRegisterAllocationPass {
            physical_registers: physical_registers,
            physical_argument_registers: physical_argument_registers,
            physical_result_register: physical_result_register,
            virtual_register_indexes: HashMap::new(),
        })
    }

    fn get_or_create_virtual_register_index(&mut self, vreg: RegisterHandle, mut function: FunctionHandle) -> usize {
        assert!(!vreg.is_physical());
        let mut vreg_index = self.virtual_register_indexes.get(&vreg).map(|i| *i);
        if vreg_index.is_none() {
            let new_index = function.get_local_variables().len();
            self.virtual_register_indexes.insert(vreg, new_index);
            function.get_mut_local_variables().insert(new_index, vreg.get_typ().clone());
            vreg_index = Some(new_index);
        }
        vreg_index.unwrap()
    }

    fn create_load_instr(&mut self, basic_block: BasicBlockHandle, preg: RegisterHandle,
                         vreg: RegisterHandle, function: FunctionHandle) -> InstrHandle {
        assert!(preg.is_physical());
        assert!(!vreg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let vreg_index = self.get_or_create_virtual_register_index(vreg, function);
        Context::create_instr(Opcode::Load {
            dst: Operand::new_physical_register(preg),
            src: Operand::new_memory(vreg_index, typ),
        }, basic_block)
    }

    fn create_store_instr(&mut self, basic_block: BasicBlockHandle, vreg: RegisterHandle,
                          preg: RegisterHandle, function: FunctionHandle) -> InstrHandle {
        assert!(!vreg.is_physical());
        assert!(preg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let vreg_index = self.get_or_create_virtual_register_index(vreg, function);
        Context::create_instr(Opcode::Store {
            dst: Operand::new_memory(vreg_index, typ),
            src: Operand::new_physical_register(preg),
        }, basic_block)
    }

    fn allocate_physical_register(&self, vreg: RegisterHandle, index: usize) -> RegisterHandle {
        assert!(!vreg.is_physical());
        let preg = *self.physical_registers[index].get(vreg.get_typ()).unwrap();
        assert!(preg.is_physical());
        preg
    }

    fn allocate_physical_argument_register(&self, vreg: RegisterHandle, index: usize) -> RegisterHandle {
        assert!(!vreg.is_physical());
        let preg = *self.physical_argument_registers[index].get(vreg.get_typ()).unwrap();
        assert!(preg.is_physical());
        preg
    }
}

#[derive(Debug)]
pub struct InsertBasicBlockLabelPass {}

impl BasicBlockPass for InsertBasicBlockLabelPass {
    fn do_action(&mut self, mut basic_block: BasicBlockHandle) {
        let opc = Opcode::Label(format!("label_{}", basic_block));
        let instr = Context::create_instr(opc, basic_block);
        let instrs = basic_block.get_mut_instrs();
        instrs.push_front(instr);
    }
}

impl InsertBasicBlockLabelPass {
    pub fn create() -> Box<InsertBasicBlockLabelPass> {
        Box::new(InsertBasicBlockLabelPass {})
    }
}

#[derive(Debug)]
pub struct ModuleInitPass {}

impl ModulePass for ModuleInitPass {
    fn do_action(&mut self, _module: ModuleHandle) {
        println!(".intel_syntax noprefix");
        println!();
    }
}

impl ModuleInitPass {
    pub fn create() -> Box<ModuleInitPass> {
        Box::new(ModuleInitPass {})
    }
}

#[derive(Debug)]
pub struct PreEmitAssemblyPass {
    base_pointer_register: &'static str,
}

impl FunctionPass for PreEmitAssemblyPass {
    fn do_action(&mut self, function: FunctionHandle) {
        println!(".global {}", function.get_func_name());
        println!("{}:", function.get_func_name());
        println!("push rbp");
        println!("mov rbp, rsp");
        let len_buffer = function.get_local_variables().iter().map(|p| p.1.get_size()).sum::<usize>();
        let word_size = 8;
        println!("sub rsp, {}", ((len_buffer + word_size - 1) / word_size) * word_size);

        // store parameter registers to memory
        let param_regs = vec!["edi", "esi", "edx", "ecx"]; // treat only 32 bit integers currently
        //let param_regs=vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        assert!(function.get_parameter_types().len() < param_regs.len());
        for (i, typ) in function.get_parameter_types().iter().enumerate() {
            let dst_offset = (i + 1) * typ.get_size();
            let src_name = param_regs[i];
            println!("mov dword ptr [{} - {}], {}", self.base_pointer_register, dst_offset, src_name);
        }
    }
}

impl PreEmitAssemblyPass {
    pub fn create(base_pointer_register: &'static str) -> Box<PreEmitAssemblyPass> {
        Box::new(PreEmitAssemblyPass { base_pointer_register })
    }
}

#[derive(Debug)]
pub struct EmitAssemblyPass {
    physical_register_name_map: HashMap<RegisterHandle, &'static str>,
    base_pointer_register: &'static str,
}

impl InstrPass for EmitAssemblyPass {
    fn do_action(&mut self, instr: InstrHandle) {
        use self::Opcode::*;
        match instr.get_opcode() {
            &Debug(ref msg) => {
                println!("# {}", msg);
            }
            &Label(ref label) => {
                println!("{}:", label);
            }
            &Copy { ref dst, ref src, .. } => {
                let dst = dst.get_as_physical_register().unwrap();
                let dst_name = self.physical_register_name_map.get(&dst).unwrap();
                let src = src.get_as_physical_register().unwrap();
                let src_name = self.physical_register_name_map.get(&src).unwrap();
                println!("mov {}, {}", dst_name, src_name);
            }
            &UnaryOp { ref kind, ref dst, ref src, .. } => {
                let dst = dst.get_as_physical_register().unwrap();
                assert!(dst.is_physical());
                let dst_name = self.physical_register_name_map.get(&dst).unwrap();
                match kind {
                    &UnaryOpKind::Const => {
                        match src.get_kind() {
                            &OperandKind::ConstI32(cst) => println!("mov {}, {}", dst_name, cst),
                            _ => unimplemented!(),
                        }
                    }
                };
            }
            &BinaryOp { ref kind, ref dst, ref src1, ref src2, .. } => {
                assert_eq!(dst, src1);
                let dst = dst.get_as_physical_register().unwrap();
                assert!(dst.is_physical());
                let op = match kind {
                    &BinaryOpKind::Add => "add",
                    &BinaryOpKind::Sub => "sub",
                    &BinaryOpKind::Mul => "imul",
                    &BinaryOpKind::Shr => "shr",
                    &BinaryOpKind::And => "and",
                };
                match src2.get_kind() {
                    &OperandKind::PhysicalRegister(preg) => self.emit_binop_reg_reg(op, dst, preg),
                    &OperandKind::ConstI32(imm) => self.emit_binop_reg_imm32(op, dst, imm),
                    _ => unimplemented!(),
                }
            }
            &Load { ref dst, ref src, .. } => {
                let dst = dst.get_as_physical_register().unwrap();
                assert!(dst.is_physical());
                let dst_name = self.physical_register_name_map.get(&dst).unwrap();

                let src_offset = match src.get_kind() {
                    &OperandKind::Memory { index, ref typ } => (index + 1) * typ.get_size(),
                    _ => unimplemented!(),
                };

                println!("mov {}, dword ptr [{} - {}]", dst_name, self.base_pointer_register, src_offset);
            }
            &Store { ref dst, ref src, .. } => {
                let dst_offset = match dst.get_kind() {
                    &OperandKind::Memory { index, ref typ } => (index + 1) * typ.get_size(),
                    _ => unimplemented!(),
                };

                let src = src.get_as_physical_register().unwrap();
                assert!(src.is_physical());
                let src_name = self.physical_register_name_map.get(&src).unwrap();

                println!("mov dword ptr [{} - {}], {}", self.base_pointer_register, dst_offset, src_name);
            }
            &Jump { ref kind, ref target } => {
                let target = target.get_as_label().unwrap();
                use self::JumpCondKind::*;
                match kind {
                    &Unconditional => {
                        println!("jmp label_{}", target);
                    }
                    &Eq0(preg) => {
                        self.emit_binop_reg_reg("test", preg, preg);
                        println!("jz label_{}", target);
                    }
                    &Neq0(preg) => {
                        self.emit_binop_reg_reg("test", preg, preg);
                        println!("jnz label_{}", target);
                    }
                    &Neq(preg1, preg2) => {
                        self.emit_binop_reg_reg("cmp", preg1, preg2);
                        println!("jnz label_{}", target);
                    }
                }
            }
            &Call { ref func, .. } => {
                println!("call {}", func.get_func_name());
            }
            &Return { .. } => {
                println!("mov rsp, rbp");
                println!("pop rbp");
                println!("ret");
            }
        }
    }
}

impl EmitAssemblyPass {
    pub fn create(physical_register_name_map: HashMap<RegisterHandle, &'static str>, base_pointer_register: &'static str) -> Box<EmitAssemblyPass> {
        Box::new(EmitAssemblyPass {
            physical_register_name_map: physical_register_name_map,
            base_pointer_register: base_pointer_register,
        })
    }

    fn emit_binop_reg_reg(&mut self, op: &'static str, dst: RegisterHandle, src: RegisterHandle) {
        println!("{} {}, {}", op, self.physical_register_name_map.get(&dst).unwrap(), self.physical_register_name_map.get(&src).unwrap());
    }

    fn emit_binop_reg_imm32(&mut self, op: &'static str, target: RegisterHandle, imm: u32) {
        println!("{} {}, {}", op, self.physical_register_name_map.get(&target).unwrap(), imm);
    }
}
