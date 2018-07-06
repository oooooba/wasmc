use std::collections::HashMap;

use context::Context;
use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, RegisterHandle};
use machineir::opcode::{BinaryOpKind, JumpCondKind, Opcode};
use machineir::operand::{Operand, OperandKind};
use machineir::typ::Type;
use pass::{BasicBlockPass, FunctionPass, InstrPass};

#[derive(Debug)]
pub struct SimpleRegisterAllocationPass {
    physical_registers: Vec<RegisterHandle>,
    physical_argument_registers: Vec<RegisterHandle>,
    physical_result_register: RegisterHandle,
    virtual_register_indexes: HashMap<RegisterHandle, usize>,
}

impl FunctionPass for SimpleRegisterAllocationPass {
    fn do_action(&mut self, mut function: FunctionHandle) {
        for basic_block_i in 0..function.get_mut_basic_blocks().len() {
            let mut basic_block = function.get_mut_basic_blocks()[basic_block_i];
            let num_instrs = basic_block.get_instrs().len();
            let mut num_insertion = 0;

            for instr_i in 0..num_instrs {
                let mut instr = basic_block.get_mut_instrs()[instr_i + num_insertion];

                if instr.get_opcode().is_jump_instr() {
                    use self::JumpCondKind::*;
                    let new_opcode = match instr.get_opcode() {
                        &Opcode::Jump { ref kind, ref target } => {
                            let new_cond_kind = match kind {
                                &Unconditional => Unconditional,
                                &Eq0(reg) => {
                                    assert!(!reg.is_physical());
                                    let preg = self.physical_registers[0];
                                    self.emit_load_instr(basic_block, instr_i + num_insertion, preg, reg, function);
                                    num_insertion += 1;
                                    Eq0(preg)
                                }
                                &Neq(reg1, reg2) => {
                                    assert!(!reg1.is_physical());
                                    let preg1 = self.physical_registers[0];
                                    self.emit_load_instr(basic_block, instr_i + num_insertion, preg1, reg1, function);
                                    num_insertion += 1;

                                    assert!(!reg2.is_physical());
                                    let preg2 = self.physical_registers[1];
                                    self.emit_load_instr(basic_block, instr_i + num_insertion, preg2, reg2, function);
                                    num_insertion += 1;

                                    Neq(preg1, preg2)
                                }
                            };
                            Opcode::Jump { kind: new_cond_kind, target: target.clone() }
                        }
                        _ => unimplemented!(),
                    };
                    instr.set_opcode(new_opcode);
                    continue;
                }

                if instr.get_opcode().is_return_instr() {
                    continue;
                }

                // load source registers
                let num_srcs = instr.get_opcode().get_source_operands().len();
                for src_i in 1..num_srcs + 1 {
                    let insertion_point = instr_i + num_insertion;
                    let vreg = if let Some(vreg) = instr.get_opcode().get_source_operand(src_i).and_then(|o| o.get_as_register()) {
                        vreg
                    } else {
                        continue;
                    };
                    let preg = if instr.get_opcode().is_call_instr() {
                        self.physical_argument_registers[src_i - 1]
                    } else {
                        self.physical_registers[src_i - 1]
                    };
                    self.emit_load_instr(basic_block, insertion_point, preg, vreg, function);
                    num_insertion += 1;
                    instr.get_mut_opcode().set_source_operand(src_i, Operand::new_physical_register(preg));
                }

                if instr.get_opcode().is_call_instr() {
                    if instr.get_opcode().get_destination_register_operand().is_none() {
                        continue;
                    }
                }

                // replace destination register
                let preg = self.physical_result_register;
                let vreg = instr.get_opcode().get_destination_register_operand().and_then(|o| o.get_as_register()).unwrap();
                instr.get_mut_opcode().set_destination_operand(Operand::new_physical_register(preg));

                // store destination register
                let insertion_point = instr_i + num_insertion + 1;
                self.emit_store_instr(basic_block, insertion_point, vreg, preg, function);
                num_insertion += 1;
            }
        }
    }
}

impl SimpleRegisterAllocationPass {
    pub fn create(
        physical_registers: Vec<RegisterHandle>,
        physical_argument_registers: Vec<RegisterHandle>,
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

    fn emit_load_instr(&mut self, mut basic_block: BasicBlockHandle, insertion_point: usize,
                       preg: RegisterHandle, vreg: RegisterHandle, function: FunctionHandle) {
        assert!(preg.is_physical());
        assert!(!vreg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let vreg_index = self.get_or_create_virtual_register_index(vreg, function);
        let load_instr = Context::create_instr(Opcode::Load(typ.clone(),
                                                            Operand::new_physical_register(preg),
                                                            Operand::new_memory(vreg_index, typ)), basic_block);
        basic_block.get_mut_instrs().insert(insertion_point, load_instr);
    }

    fn emit_store_instr(&mut self, mut basic_block: BasicBlockHandle, insertion_point: usize,
                        vreg: RegisterHandle, preg: RegisterHandle, function: FunctionHandle) {
        assert!(!vreg.is_physical());
        assert!(preg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let vreg_index = self.get_or_create_virtual_register_index(vreg, function);
        let store_instr = Context::create_instr(Opcode::Store(typ.clone(),
                                                              Operand::new_memory(vreg_index, typ),
                                                              Operand::new_physical_register(preg)), basic_block);
        basic_block.get_mut_instrs().insert(insertion_point, store_instr);
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
pub struct PreEmitAssemblyPass {
    base_pointer_register: &'static str,
}

impl FunctionPass for PreEmitAssemblyPass {
    fn do_action(&mut self, function: FunctionHandle) {
        println!(".intel_syntax noprefix");
        println!(".global {}", "entry_point");
        println!();
        println!("entry_point:");
        println!("{}:", function.get_func_name());
        println!("push rbp");
        println!("mov rbp, rsp");
        let len_buffer = (function.get_local_variables().len() + 1) * Type::I32.get_size();
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
pub struct PostEmitAssemblyPass {}

impl FunctionPass for PostEmitAssemblyPass {
    fn do_action(&mut self, _function: FunctionHandle) {
        println!("mov rsp, rbp");
        println!("pop rbp");
        println!("ret");
    }
}

impl PostEmitAssemblyPass {
    pub fn create() -> Box<PostEmitAssemblyPass> {
        Box::new(PostEmitAssemblyPass {})
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
                println!("; {}", msg);
            }
            &Label(ref label) => {
                println!("{}:", label);
            }
            &Const(_, ref dst, ref cst) => {
                let dst = dst.get_as_physical_register().unwrap();
                let dst_name = self.physical_register_name_map.get(&dst).unwrap();
                let cst = cst.get_as_const_i32().unwrap();
                println!("mov {}, {}", dst_name, cst);
            }
            &Br(ref target) => {
                let target = target.get_as_label().unwrap();
                println!("jmp label_{}", target);
            }
            &BrIfNonZero(ref cond, ref target) => {
                let cond = cond.get_as_physical_register().unwrap();
                let target = target.get_as_label().unwrap();
                self.emit_binop_reg_reg("test", cond, cond);
                println!("jnz label_{}", target);
            }
            &Copy(_, ref dst, ref src) => {
                let dst = dst.get_as_physical_register().unwrap();
                let dst_name = self.physical_register_name_map.get(&dst).unwrap();
                let src = src.get_as_physical_register().unwrap();
                let src_name = self.physical_register_name_map.get(&src).unwrap();
                println!("mov {}, {}", dst_name, src_name);
            }
            &Load(_, ref dst, ref src) => {
                let dst = dst.get_as_physical_register().unwrap();
                assert!(dst.is_physical());
                let dst_name = self.physical_register_name_map.get(&dst).unwrap();

                let src_offset = match src.get_kind() {
                    &OperandKind::Memory { index, ref typ } => (index + 1) * typ.get_size(),
                    _ => unimplemented!(),
                };

                println!("mov {}, dword ptr [{} - {}]", dst_name, self.base_pointer_register, src_offset);
            }
            &Store(_, ref dst, ref src) => {
                let dst_offset = match dst.get_kind() {
                    &OperandKind::Memory { index, ref typ } => (index + 1) * typ.get_size(),
                    _ => unimplemented!(),
                };

                let src = src.get_as_physical_register().unwrap();
                assert!(src.is_physical());
                let src_name = self.physical_register_name_map.get(&src).unwrap();

                println!("mov dword ptr [{} - {}], {}", self.base_pointer_register, dst_offset, src_name);
            }
            &Return(_, _) => {
                println!("mov rsp, rbp");
                println!("pop rbp");
                println!("ret");
            }
            &Call(ref funcname, _, _, _) => {
                println!("call {}", funcname);
            }
            &Eq(_, ref dst, ref src1, ref src2) => {
                assert_eq!(dst, src1);
                let dst = dst.get_as_physical_register().unwrap();
                let dst_name = *self.physical_register_name_map.get(&dst).unwrap();
                assert!(dst.is_physical());
                match src2.get_kind() {
                    &OperandKind::PhysicalRegister(preg) => {
                        self.emit_binop_reg_reg("cmp", dst, preg);
                        println!("jnz label_i{}x", instr);
                        println!("mov {}, {}", dst_name, 1);
                        println!("jmp label_i{}y", instr);
                        println!("label_i{}x:", instr);
                        println!("mov {}, {}", dst_name, 0);
                        println!("label_i{}y:", instr);
                    }
                    _ => unimplemented!(),
                }
            }
            &BinaryOp { ref kind, ref dst, ref src1, ref src2, .. } => {
                assert_eq!(dst, src1);
                let dst = dst.get_as_physical_register().unwrap();
                assert!(dst.is_physical());
                let op = match kind {
                    &BinaryOpKind::Add => "add",
                    &BinaryOpKind::Sub => "sub",
                    &BinaryOpKind::Mul => "imul",
                };
                match src2.get_kind() {
                    &OperandKind::PhysicalRegister(preg) => self.emit_binop_reg_reg(op, dst, preg),
                    _ => unimplemented!(),
                }
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
                    &Neq(preg1, preg2) => {
                        self.emit_binop_reg_reg("cmp", preg1, preg2);
                        println!("jnz label_{}", target);
                    }
                }
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
}
