use std::collections::HashMap;

use context::Context;
use context::handle::{BasicBlockHandle, FunctionHandle, ModuleHandle, RegisterHandle};
use machineir::opcode::{BinaryOpKind, JumpCondKind, Opcode, UnaryOpKind};
use machineir::operand::OperandKind;
use machineir::typ::Type;
use pass::{BasicBlockPass, FunctionPass, ModulePass};

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
pub struct EmitAssemblyPass {
    register_name_map: HashMap<RegisterHandle, &'static str>,
    base_pointer_register: RegisterHandle,
    stack_pointer_register: RegisterHandle,
    argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    local_variable_index_to_offset_map: HashMap<usize, usize>,
}

impl FunctionPass for EmitAssemblyPass {
    fn do_action(&mut self, mut function: FunctionHandle) {
        {
            // calculate offset of local variables
            let word_size = 8;
            let mut len_buffer = word_size;
            for (index, typ) in function.get_local_variables().iter() {
                self.local_variable_index_to_offset_map.insert(*index, len_buffer);
                len_buffer += ((typ.get_size() + word_size - 1) / word_size) * word_size;
            }

            let base_pointer_register = self.register_name_map.get(&self.base_pointer_register).unwrap();
            let stack_pointer_register = self.register_name_map.get(&self.stack_pointer_register).unwrap();

            println!(".global {}", function.get_func_name());
            println!("{}:", function.get_func_name());
            println!("push {}", base_pointer_register);
            println!("mov {}, {}", base_pointer_register, stack_pointer_register);
            println!("sub {}, {}", stack_pointer_register, len_buffer);

            // store parameter registers to memory
            assert!(function.get_parameter_types().len() < self.argument_registers.len());
            for (i, typ) in function.get_parameter_types().iter().enumerate() {
                let dst_offset = self.get_local_variable_offset_of(i);
                let src_reg = self.argument_registers[i].get(typ).unwrap();
                let src_name = self.register_name_map.get(src_reg).unwrap();
                let ptr_notation = typ.get_ptr_notation();
                println!("mov {} ptr [{} - {}], {}", ptr_notation, base_pointer_register, dst_offset, src_name);
            }
        }

        for basic_block_i in 0..function.get_mut_basic_blocks().len() {
            let mut basic_block = function.get_mut_basic_blocks()[basic_block_i];

            let mut iter = basic_block.iterator();
            while let Some(mut instr) = iter.get() {
                use self::Opcode::*;
                print!("# ");
                instr.get().print();
                println!();
                match instr.get_opcode() {
                    &Debug(ref msg) => {
                        println!("# {}", msg);
                    }
                    &Label(ref label) => {
                        println!("{}:", label);
                    }
                    &Copy { ref dst, ref src, .. } => {
                        let dst = dst.get_as_physical_register().unwrap();
                        let dst_name = self.register_name_map.get(&dst).unwrap();
                        let src = src.get_as_physical_register().unwrap();
                        let src_name = self.register_name_map.get(&src).unwrap();
                        println!("mov {}, {}", dst_name, src_name);
                    }
                    &UnaryOp { ref kind, ref dst, ref src, .. } => {
                        let dst = dst.get_as_physical_register().unwrap();
                        assert!(dst.is_physical());
                        let dst_name = self.register_name_map.get(&dst).unwrap();
                        match kind {
                            &UnaryOpKind::Const => {
                                match src.get_kind() {
                                    &OperandKind::ConstI32(cst) => println!("mov {}, {}", dst_name, cst),
                                    &OperandKind::ConstI64(cst) => println!("mov {}, {}", dst_name, cst),
                                    _ => unimplemented!(),
                                }
                            }
                            &UnaryOpKind::Wrap => println!("# UnaryOpKind::Wrap"),
                            &UnaryOpKind::ZeroExtension | &UnaryOpKind::SignExtension => {
                                assert_eq!(dst.get_typ(), &Type::I64);
                                let src = src.get_as_physical_register().unwrap();
                                assert_eq!(src.get_typ(), &Type::I32);
                                match kind {
                                    &UnaryOpKind::ZeroExtension => println!("# UnaryOpKind::ZeroExtension"),
                                    &UnaryOpKind::SignExtension => println!("cdqe"),
                                    _ => panic!(),
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
                            &OperandKind::ConstI64(imm) => self.emit_binop_reg_imm64(op, dst, imm),
                            _ => unimplemented!(),
                        }
                    }
                    &Load { ref dst, ref src, .. } => {
                        let dst = dst.get_as_physical_register().unwrap();
                        assert!(dst.is_physical());
                        let dst_name = self.register_name_map.get(&dst).unwrap();

                        let (src_offset, typ) = match src.get_kind() {
                            &OperandKind::Memory { index, ref typ } => (self.get_local_variable_offset_of(index), typ),
                            _ => unimplemented!(),
                        };

                        let ptr_notation = typ.get_ptr_notation();
                        let bpr_name = self.register_name_map.get(&self.base_pointer_register).unwrap();

                        println!("mov {}, {} ptr [{} - {}]", dst_name, ptr_notation, bpr_name, src_offset);
                    }
                    &Store { ref dst, ref src, .. } => {
                        let (dst_offset, typ) = match dst.get_kind() {
                            &OperandKind::Memory { index, ref typ } => (self.get_local_variable_offset_of(index), typ),
                            _ => unimplemented!(),
                        };

                        let src = src.get_as_physical_register().unwrap();
                        assert!(src.is_physical());
                        let src_name = self.register_name_map.get(&src).unwrap();

                        let ptr_notation = typ.get_ptr_notation();
                        let bpr_name = self.register_name_map.get(&self.base_pointer_register).unwrap();

                        println!("mov {} ptr [{} - {}], {}", ptr_notation, bpr_name, dst_offset, src_name);
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
                iter.advance();
            }
        }
        self.local_variable_index_to_offset_map.clear();
    }
}

impl EmitAssemblyPass {
    pub fn create(
        physical_register_name_map: HashMap<RegisterHandle, &'static str>,
        base_pointer_register: RegisterHandle,
        stack_pointer_register: RegisterHandle,
        argument_registers: Vec<HashMap<Type, RegisterHandle>>) -> Box<EmitAssemblyPass> {
        Box::new(EmitAssemblyPass {
            register_name_map: physical_register_name_map,
            base_pointer_register,
            stack_pointer_register,
            argument_registers,
            local_variable_index_to_offset_map: HashMap::new(),
        })
    }

    fn emit_binop_reg_reg(&mut self, op: &'static str, dst: RegisterHandle, src: RegisterHandle) {
        println!("{} {}, {}", op, self.register_name_map.get(&dst).unwrap(), self.register_name_map.get(&src).unwrap());
    }

    fn emit_binop_reg_imm32(&mut self, op: &'static str, target: RegisterHandle, imm: u32) {
        println!("{} {}, {}", op, self.register_name_map.get(&target).unwrap(), imm);
    }

    fn emit_binop_reg_imm64(&mut self, op: &'static str, target: RegisterHandle, imm: u64) {
        println!("{} {}, {}", op, self.register_name_map.get(&target).unwrap(), imm);
    }

    fn get_local_variable_offset_of(&self, index: usize) -> usize {
        *self.local_variable_index_to_offset_map.get(&index).unwrap()
    }
}
