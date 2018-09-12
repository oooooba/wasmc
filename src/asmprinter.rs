use std::collections::HashMap;

use context::handle::{BasicBlockHandle, FunctionHandle, ModuleHandle, RegisterHandle};
use context::Context;
use machineir::function::Linkage;
use machineir::opcode::{
    Address, BinaryOpKind, CallTargetKind, CastKind, ConstKind, JumpCondKind, JumpTargetKind,
    Opcode, OperandKind,
};
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
        println!(".text");
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
            match function.get_linkage() {
                &Linkage::Export => println!(".global {}", function.get_func_name()),
                &Linkage::Import => return,
                &Linkage::Private => (),
            }
            println!("{}:", function.get_func_name());

            let len_buffer = function.get_local_region().calculate_variable_offset();

            let base_pointer_register = self
                .register_name_map
                .get(&self.base_pointer_register)
                .unwrap();
            let stack_pointer_register = self
                .register_name_map
                .get(&self.stack_pointer_register)
                .unwrap();

            println!("push {}", base_pointer_register);
            println!("mov {}, {}", base_pointer_register, stack_pointer_register);
            println!("sub {}, {}", stack_pointer_register, len_buffer);
            println!("push rbx");

            // store parameter registers to memory
            assert!(function.get_parameter_types().len() <= self.argument_registers.len());
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
                    &Copy { dst, src } => {
                        let dst_name = self.register_name_map.get(&dst).unwrap();
                        let src_name = self.register_name_map.get(&src).unwrap();
                        println!("mov {}, {}", dst_name, src_name);
                    }
                    &Const { dst, ref src } => {
                        let dst_name = self.register_name_map.get(&dst).unwrap();
                        match src {
                            &ConstKind::ConstI8(i) => println!("mov {}, {}", dst_name, i),
                            &ConstKind::ConstI32(i) => println!("mov {}, {}", dst_name, i),
                            &ConstKind::ConstI64(i) => println!("mov {}, {}", dst_name, i),
                        };
                    }
                    &Cast {
                        ref kind, dst, src, ..
                    } => {
                        assert!(dst.is_physical());
                        assert!(src.is_physical());
                        match kind {
                            &CastKind::Wrap => println!("# UnaryOpKind::Wrap"),
                            &CastKind::ZeroExtension | &CastKind::SignExtension => {
                                assert!(dst.get_typ().get_size() > src.get_typ().get_size());
                                match kind {
                                    &CastKind::ZeroExtension => {
                                        println!("# UnaryOpKind::ZeroExtension")
                                    }
                                    &CastKind::SignExtension => {
                                        match (dst.get_typ(), src.get_typ()) {
                                            (&Type::I64, &Type::I32) => println!("cdqe"),
                                            (&Type::I32, &Type::I8) => {
                                                println!("cbw");
                                                println!("cwde");
                                            }
                                            _ => unimplemented!(),
                                        }
                                    }
                                    _ => panic!(),
                                }
                            }
                        };
                    }
                    &BinaryOp {
                        ref kind,
                        dst,
                        src1,
                        ref src2,
                    } => {
                        assert_eq!(dst, src1);
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
                        match src2 {
                            &OperandKind::Register(src2) => self.emit_binop_reg_reg(op, dst, src2),
                            &OperandKind::ImmI8(imm) => self.emit_binop_reg_imm8(op, dst, imm),
                            &OperandKind::ImmI32(imm) => self.emit_binop_reg_imm32(op, dst, imm),
                            &OperandKind::ImmI64(imm) => self.emit_binop_reg_imm64(op, dst, imm),
                        }
                    }
                    &Load { dst, ref src } => {
                        assert!(dst.is_physical());
                        let dst_name = self.register_name_map.get(&dst).unwrap();
                        let ptr_notation = dst.get_typ().get_ptr_notation();
                        match src {
                            &Address::Var(var) => {
                                assert!(!var.is_physical());
                                if function
                                    .get_local_region()
                                    .get_offset_map()
                                    .contains_key(&var)
                                {
                                    let region = function.get_local_region();
                                    let offset = *region.get_offset_map().get(&var).unwrap();
                                    let bpr_name = self
                                        .register_name_map
                                        .get(&self.base_pointer_register)
                                        .unwrap();
                                    println!(
                                        "mov {}, {} ptr [{} - {}]",
                                        dst_name, ptr_notation, bpr_name, offset
                                    );
                                } else if function
                                    .get_module()
                                    .get_mutable_global_variable_region()
                                    .get_offset_map()
                                    .contains_key(&var)
                                {
                                    let region =
                                        function.get_module().get_mutable_global_variable_region();
                                    let offset = *region.get_offset_map().get(&var).unwrap();
                                    println!(
                                        "mov {}, {} ptr [{} + {}]",
                                        dst_name,
                                        ptr_notation,
                                        region.get_name(),
                                        offset
                                    );
                                } else if function
                                    .get_module()
                                    .get_const_global_variable_region()
                                    .get_offset_map()
                                    .contains_key(&var)
                                {
                                    let region =
                                        function.get_module().get_const_global_variable_region();
                                    let offset = *region.get_offset_map().get(&var).unwrap();
                                    println!(
                                        "mov {}, {} ptr [{} + {}]",
                                        dst_name,
                                        ptr_notation,
                                        region.get_name(),
                                        offset
                                    );
                                } else {
                                    unreachable!()
                                }
                            }
                            &Address::RegBaseRegOffset { base, offset } => {
                                assert!(base.is_physical());
                                assert!(offset.is_physical());
                                let base_name = self.register_name_map.get(&base).unwrap();
                                let offset_name = self.register_name_map.get(&offset).unwrap();
                                println!(
                                    "mov {}, {} ptr [{} + {}]",
                                    dst_name, ptr_notation, base_name, offset_name
                                );
                            }
                            &Address::RegBaseRegIndex { .. } => unimplemented!(),
                        }
                    }
                    &Store { ref dst, src } => {
                        assert!(src.is_physical());
                        let src_name = self.register_name_map.get(&src).unwrap();
                        let ptr_notation = src.get_typ().get_ptr_notation();
                        match dst {
                            &Address::Var(var) => {
                                assert!(!var.is_physical());
                                if function
                                    .get_local_region()
                                    .get_offset_map()
                                    .contains_key(&var)
                                {
                                    let region = function.get_local_region();
                                    let offset = *region.get_offset_map().get(&var).unwrap();
                                    let bpr_name = self
                                        .register_name_map
                                        .get(&self.base_pointer_register)
                                        .unwrap();
                                    println!(
                                        "mov {} ptr [{} - {}], {}",
                                        ptr_notation, bpr_name, offset, src_name
                                    );
                                } else if function
                                    .get_module()
                                    .get_mutable_global_variable_region()
                                    .get_offset_map()
                                    .contains_key(&var)
                                {
                                    let region =
                                        function.get_module().get_mutable_global_variable_region();
                                    let offset = *region.get_offset_map().get(&var).unwrap();
                                    println!(
                                        "mov {} ptr [{} + {}], {}",
                                        ptr_notation,
                                        region.get_name(),
                                        offset,
                                        src_name
                                    );
                                } else if function
                                    .get_module()
                                    .get_const_global_variable_region()
                                    .get_offset_map()
                                    .contains_key(&var)
                                {
                                    let region =
                                        function.get_module().get_const_global_variable_region();
                                    let offset = *region.get_offset_map().get(&var).unwrap();
                                    println!(
                                        "mov {} ptr [{} + {}], {}",
                                        ptr_notation,
                                        region.get_name(),
                                        offset,
                                        src_name
                                    );
                                } else {
                                    unreachable!()
                                }
                            }
                            &Address::RegBaseRegOffset { base, offset } => {
                                assert!(base.is_physical());
                                assert!(offset.is_physical());
                                let base_name = self.register_name_map.get(&base).unwrap();
                                let offset_name = self.register_name_map.get(&offset).unwrap();
                                println!(
                                    "mov {} ptr [{} + {}], {}",
                                    ptr_notation, base_name, offset_name, src_name
                                );
                            }
                            &Address::RegBaseRegIndex { .. } => unimplemented!(),
                        }
                    }
                    &Jump {
                        ref kind,
                        ref target,
                    } => {
                        let target = match target {
                            JumpTargetKind::BasicBlock(bb) => bb,
                        };
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
                            &LtS(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jl label_{}", target);
                            }
                            &LtU(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jb label_{}", target);
                            }
                            &LeS(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jle label_{}", target);
                            }
                            &LeU(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jbe label_{}", target);
                            }
                            &GtS(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("jg label_{}", target);
                            }
                            &GtU(preg1, preg2) => {
                                self.emit_binop_reg_reg("cmp", preg1, preg2);
                                println!("ja label_{}", target);
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
                    &Call { ref func, .. } => match func {
                        &CallTargetKind::Function(f) => println!("call {}", f.get_func_name()),
                        &CallTargetKind::Indirect(ref addr) => {
                            let ptr_notation = Type::Pointer.get_ptr_notation();
                            match addr {
                                &Address::Var(var) => {
                                    assert!(!var.is_physical());
                                    let region = function.get_local_region();
                                    let offset = *region.get_offset_map().get(&var).unwrap();
                                    let bpr_name = self
                                        .register_name_map
                                        .get(&self.base_pointer_register)
                                        .unwrap();
                                    println!(
                                        "call {} ptr [{} - {}]",
                                        ptr_notation, bpr_name, offset
                                    );
                                }
                                &Address::RegBaseRegOffset { base, offset } => {
                                    assert!(base.is_physical());
                                    assert!(offset.is_physical());
                                    println!("call {} ptr [{} + {}]", ptr_notation, base, offset);
                                }
                                &Address::RegBaseRegIndex {
                                    base,
                                    index,
                                    ref scale,
                                } => {
                                    assert!(base.is_physical());
                                    assert!(index.is_physical());
                                    let base_name = self.register_name_map.get(&base).unwrap();
                                    let index_name = self.register_name_map.get(&index).unwrap();
                                    println!(
                                        "lea {}, [{} + {} * {}]",
                                        base_name,
                                        base_name,
                                        index_name,
                                        scale.get_size()
                                    );
                                    println!("call {}", base_name);
                                }
                            }
                        }
                    },
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

    fn emit_binop_reg_imm8(&mut self, op: &'static str, target: RegisterHandle, imm: u8) {
        println!(
            "{} {}, {}",
            op,
            self.register_name_map.get(&target).unwrap(),
            imm
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
