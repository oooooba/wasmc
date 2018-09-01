use std::collections::HashMap;

use context::handle::{BasicBlockHandle, FunctionHandle, ModuleHandle, RegisterHandle};
use context::Context;
use machineir::opcode::{BinaryOpKind, JumpCondKind, OffsetKind, Opcode, UnaryOpKind};
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
}

impl FunctionPass for EmitAssemblyPass {
    fn do_action(&mut self, mut function: FunctionHandle) {
        {
            let len_buffer = function.get_local_region().calculate_variable_offset();

            let base_pointer_register = self
                .register_name_map
                .get(&self.base_pointer_register)
                .unwrap();
            let stack_pointer_register = self
                .register_name_map
                .get(&self.stack_pointer_register)
                .unwrap();

            println!(".global {}", function.get_func_name());
            println!("{}:", function.get_func_name());
            println!("push {}", base_pointer_register);
            println!("mov {}, {}", base_pointer_register, stack_pointer_register);
            println!("sub {}, {}", stack_pointer_register, len_buffer);
            println!("push rbx");

            // store parameter registers to memory
            assert!(function.get_parameter_types().len() < self.argument_registers.len());
            assert_eq!(
                function.get_parameter_types().len(),
                function.get_parameter_variables().len()
            );
            for (i, var) in function.get_parameter_variables().iter().enumerate() {
                let dst_offset = *function
                    .get_local_region()
                    .get_offset_map()
                    .get(var)
                    .unwrap();
                let typ = var.get_typ();
                let src_reg = self.argument_registers[i].get(typ).unwrap();
                let src_name = self.register_name_map.get(src_reg).unwrap();
                let ptr_notation = typ.get_ptr_notation();
                println!(
                    "mov {} ptr [{} - {}], {}",
                    ptr_notation, base_pointer_register, dst_offset, src_name
                );
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
                    &Copy {
                        ref dst, ref src, ..
                    } => {
                        let dst = dst.get_as_physical_register().unwrap();
                        let dst_name = self.register_name_map.get(&dst).unwrap();
                        let src = src.get_as_physical_register().unwrap();
                        let src_name = self.register_name_map.get(&src).unwrap();
                        println!("mov {}, {}", dst_name, src_name);
                    }
                    &UnaryOp {
                        ref kind,
                        ref dst,
                        ref src,
                        ..
                    } => {
                        let dst = dst.get_as_physical_register().unwrap();
                        assert!(dst.is_physical());
                        let dst_name = self.register_name_map.get(&dst).unwrap();
                        match kind {
                            &UnaryOpKind::Const => match src.get_kind() {
                                &OperandKind::ConstI32(cst) => {
                                    println!("mov {}, {}", dst_name, cst)
                                }
                                &OperandKind::ConstI64(cst) => {
                                    println!("mov {}, {}", dst_name, cst)
                                }
                                _ => unimplemented!(),
                            },
                            &UnaryOpKind::Wrap => println!("# UnaryOpKind::Wrap"),
                            &UnaryOpKind::ZeroExtension | &UnaryOpKind::SignExtension => {
                                assert_eq!(dst.get_typ(), &Type::I64);
                                let src = src.get_as_physical_register().unwrap();
                                assert_eq!(src.get_typ(), &Type::I32);
                                match kind {
                                    &UnaryOpKind::ZeroExtension => {
                                        println!("# UnaryOpKind::ZeroExtension")
                                    }
                                    &UnaryOpKind::SignExtension => println!("cdqe"),
                                    _ => panic!(),
                                }
                            }
                        };
                    }
                    &BinaryOp {
                        ref kind,
                        ref dst,
                        ref src1,
                        ref src2,
                        ..
                    } => {
                        assert_eq!(dst, src1);
                        let dst = dst.get_as_physical_register().unwrap();
                        assert!(dst.is_physical());
                        let op = match kind {
                            &BinaryOpKind::Add => "add",
                            &BinaryOpKind::Sub => "sub",
                            &BinaryOpKind::Mul => "imul",
                            &BinaryOpKind::Div => "idiv",
                            &BinaryOpKind::Shl => "shl",
                            &BinaryOpKind::Shr => "shr",
                            &BinaryOpKind::Sar => "sar",
                            &BinaryOpKind::And => "and",
                            &BinaryOpKind::Or => "or",
                            &BinaryOpKind::Xor => "xor",
                        };
                        match src2.get_kind() {
                            &OperandKind::PhysicalRegister(preg) => {
                                self.emit_binop_reg_reg(op, dst, preg)
                            }
                            &OperandKind::ConstI32(imm) => self.emit_binop_reg_imm32(op, dst, imm),
                            &OperandKind::ConstI64(imm) => self.emit_binop_reg_imm64(op, dst, imm),
                            _ => unimplemented!(),
                        }
                    }
                    &Load {
                        ref dst,
                        ref src_base,
                        ref src_offset,
                    } => {
                        let dst = dst.get_as_physical_register().unwrap();
                        assert!(dst.is_physical());
                        let dst_name = self.register_name_map.get(&dst).unwrap();

                        match src_base.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let region = function.get_local_region();
                                let src_offset = match src_offset {
                                    &OffsetKind::None => {
                                        region.get_offset_map().get(&vreg).unwrap()
                                    }
                                    &OffsetKind::Register(_) => unimplemented!(),
                                };
                                let ptr_notation = vreg.get_typ().get_ptr_notation();
                                let bpr_name = self
                                    .register_name_map
                                    .get(&self.base_pointer_register)
                                    .unwrap();
                                println!(
                                    "mov {}, {} ptr [{} - {}]",
                                    dst_name, ptr_notation, bpr_name, src_offset
                                );
                            }
                            _ => unimplemented!(),
                        }
                    }
                    &Store {
                        ref dst_base,
                        ref dst_offset,
                        ref src,
                    } => {
                        let src = src.get_as_physical_register().unwrap();
                        assert!(src.is_physical());
                        let src_name = self.register_name_map.get(&src).unwrap();

                        match dst_base.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let region = function.get_local_region();
                                let dst_offset = match dst_offset {
                                    &OffsetKind::None => {
                                        region.get_offset_map().get(&vreg).unwrap()
                                    }
                                    &OffsetKind::Register(_) => unimplemented!(),
                                };
                                let ptr_notation = vreg.get_typ().get_ptr_notation();
                                let bpr_name = self
                                    .register_name_map
                                    .get(&self.base_pointer_register)
                                    .unwrap();
                                println!(
                                    "mov {} ptr [{} - {}], {}",
                                    ptr_notation, bpr_name, dst_offset, src_name
                                );
                            }
                            _ => unimplemented!(),
                        }
                    }
                    &Jump {
                        ref kind,
                        ref target,
                    } => {
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
                            &Eq(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jz label_{}", target);
                            }
                            &Neq(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jnz label_{}", target);
                            }
                            &LtU(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jb label_{}", target);
                            }
                            &LeU(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jbe label_{}", target);
                            }
                            &GtS(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jg label_{}", target);
                            }
                            &GeS(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jge label_{}", target);
                            }
                            &GeU(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jae label_{}", target);
                            }
                        }
                    }
                    &Call { ref func, .. } => {
                        println!("call {}", func.get_func_name());
                    }
                    &Return { .. } => {
                        println!("pop rbx");
                        println!("mov rsp, rbp");
                        println!("pop rbp");
                        println!("ret");
                    }
                }
                iter.advance();
            }
        }
    }
}

impl EmitAssemblyPass {
    pub fn create(
        physical_register_name_map: HashMap<RegisterHandle, &'static str>,
        base_pointer_register: RegisterHandle,
        stack_pointer_register: RegisterHandle,
        argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    ) -> Box<EmitAssemblyPass> {
        Box::new(EmitAssemblyPass {
            register_name_map: physical_register_name_map,
            base_pointer_register,
            stack_pointer_register,
            argument_registers,
        })
    }

    fn emit_binop_reg_reg(&mut self, op: &'static str, dst: RegisterHandle, src: RegisterHandle) {
        println!(
            "{} {}, {}",
            op,
            self.register_name_map.get(&dst).unwrap(),
            self.register_name_map.get(&src).unwrap()
        );
    }

    fn emit_binop_reg_imm32(&mut self, op: &'static str, target: RegisterHandle, imm: u32) {
        println!(
            "{} {}, {}",
            op,
            self.register_name_map.get(&target).unwrap(),
            imm
        );
    }

    fn emit_binop_reg_imm64(&mut self, op: &'static str, target: RegisterHandle, imm: u64) {
        println!(
            "{} {}, {}",
            op,
            self.register_name_map.get(&target).unwrap(),
            imm
        );
    }
}
