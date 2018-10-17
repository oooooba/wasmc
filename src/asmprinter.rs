use std::collections::HashMap;

use context::handle::{
    BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, RegionHandle, RegisterHandle,
    VariableHandle,
};
use machineir::module::Linkage;
use machineir::opcode::{
    Address, BinaryOpKind, CallTargetKind, CastKind, ConstKind, JumpCondKind, JumpTargetKind,
    Opcode, OperandKind,
};
use machineir::region::RegionKind;
use machineir::typ::Type;
use pass::{BasicBlockPass, FunctionPass, InstrPass, ModulePass};

#[derive(Debug)]
struct InsertCallingProgramInitializersInstrPass {}

impl ModulePass for InsertCallingProgramInitializersInstrPass {
    fn do_action(&mut self, module: ModuleHandle) {
        println!(".section .init");
        for function in module.get_functions() {
            if !function.is_program_initializer() {
                continue;
            }
            println!("call {}", function.get_func_name());
        }
    }
}

impl InsertCallingProgramInitializersInstrPass {
    fn new() -> InsertCallingProgramInitializersInstrPass {
        InsertCallingProgramInitializersInstrPass {}
    }
}

#[derive(Debug)]
struct ModuleInitPass {}

impl ModulePass for ModuleInitPass {
    fn do_action(&mut self, module: ModuleHandle) {
        println!(".intel_syntax noprefix");
        println!();

        for global_region in module.get_global_regions() {
            self.emit_global_region(*global_region);
        }

        module.apply_module_pass(&mut InsertCallingProgramInitializersInstrPass::new());

        println!(".text");
    }
}

impl ModuleInitPass {
    fn new() -> ModuleInitPass {
        ModuleInitPass {}
    }

    fn emit_global_region(&self, region: RegionHandle) {
        match region.get_kind() {
            &RegionKind::Local => panic!("pre-condition"),
            _ => {}
        }

        let export = match region.get_linkage() {
            &Linkage::Export => true,
            &Linkage::Import => return,
            &Linkage::Private => false,
        };

        match region.get_kind() {
            &RegionKind::Local => unreachable!(),
            &RegionKind::MutableGlobal => println!(".data"),
            &RegionKind::ReadOnlyGlobal => println!(".section .rodata"),
            &RegionKind::VariableSizedGlobal { .. } => unimplemented!(),
        };
        println!(".align {}", Type::Pointer.get_size());
        if export {
            println!(".global {}", region.get_variable().get_name());
            println!(".global {}", region.get_name());
        }
        println!("{}:", region.get_variable().get_name());
        println!("{}:", region.get_name());

        let mut offset_map_vec: Vec<(VariableHandle, usize)> =
            region.get_offset_map().clone().into_iter().collect();
        offset_map_vec.sort_unstable_by(|a, b| a.1.cmp(&b.1));

        let mut start_of_zeros_offset = 0;
        for (var, offset) in offset_map_vec.into_iter() {
            println!("# var.type={:?}, offset={}", var.get_type(), offset);
            for _ in 0..(offset - start_of_zeros_offset) {
                println!(".byte {}", 0);
            }
            let directive = match var.get_type() {
                &Type::I8 => "byte",
                &Type::I32 => "long",
                &Type::I64 => "quad",
                &Type::Pointer => "quad",
            };
            let cst = region.get_initial_value_map().get(&var).unwrap();
            let value = match cst {
                Some(ConstKind::ConstI8(i)) => *i as usize,
                Some(ConstKind::ConstI32(i)) => *i as usize,
                Some(ConstKind::ConstI64(i)) => *i as usize,
                None => 0,
            };
            println!(".{} {}", directive, value);
            start_of_zeros_offset = offset + var.get_type().get_size();
        }
        println!();
    }
}

#[derive(Debug)]
struct EmitX86AssemblyPass<'a> {
    register_name_map: &'a HashMap<RegisterHandle, &'static str>,
    instruction_pointer_register: RegisterHandle,
}

impl<'a> InstrPass for EmitX86AssemblyPass<'a> {
    fn do_action(&mut self, instr: InstrHandle) {
        use self::Opcode::*;
        print!("# ");
        instr.get().print();
        println!();
        match instr.get_opcode() {
            &Debug(ref msg) => {
                println!("# {}", msg);
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
                            &CastKind::ZeroExtension => println!("# UnaryOpKind::ZeroExtension"),
                            &CastKind::SignExtension => match (dst.get_typ(), src.get_typ()) {
                                (&Type::I64, &Type::I32) => println!("cdqe"),
                                (&Type::I32, &Type::I8) => {
                                    println!("cbw");
                                    println!("cwde");
                                }
                                _ => unimplemented!(),
                            },
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
                    &BinaryOpKind::Sll => "shl",
                    &BinaryOpKind::Srl => "shr",
                    &BinaryOpKind::Sra => "sar",
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
                    &Address::Var(_) => unreachable!(),
                    &Address::LabelBaseImmOffset { base, offset } => {
                        match base.get_kind() {
                            &RegionKind::Local => unreachable!(),
                            _ => {}
                        }
                        let ipr_name = self
                            .register_name_map
                            .get(&self.instruction_pointer_register)
                            .unwrap();
                        let offset_op = if offset >= 0 { "+" } else { "-" };
                        let offset = offset.abs();
                        println!(
                            "mov {}, {} ptr [{} + {} {} {}]",
                            dst_name,
                            ptr_notation,
                            ipr_name,
                            base.get_name(),
                            offset_op,
                            offset,
                        );
                    }
                    &Address::LabelBaseRegOffset { .. } => unreachable!(),
                    &Address::RegBaseImmOffset { base, offset } => {
                        let base_name = self.register_name_map.get(&base).unwrap();
                        let op = if offset >= 0 { "+" } else { "-" };
                        let offset = offset.abs();
                        println!(
                            "mov {}, {} ptr [{} {} {}]",
                            dst_name, ptr_notation, base_name, op, offset,
                        );
                    }
                    &Address::RegBaseRegOffset { .. } => unimplemented!(),
                    &Address::RegBaseRegIndex { .. } => unimplemented!(),
                }
            }
            &Store { ref dst, src } => {
                assert!(src.is_physical());
                let src_name = self.register_name_map.get(&src).unwrap();
                let ptr_notation = src.get_typ().get_ptr_notation();
                match dst {
                    &Address::Var(_) => unreachable!(),
                    &Address::LabelBaseImmOffset { base, offset } => {
                        match base.get_kind() {
                            &RegionKind::Local => unreachable!(),
                            _ => {}
                        }
                        let ipr_name = self
                            .register_name_map
                            .get(&self.instruction_pointer_register)
                            .unwrap();
                        let offset_op = if offset >= 0 { "+" } else { "-" };
                        let offset = offset.abs();
                        println!(
                            "mov {} ptr [{} + {} {} {}], {}",
                            ptr_notation,
                            ipr_name,
                            base.get_name(),
                            offset_op,
                            offset,
                            src_name
                        );
                    }
                    &Address::LabelBaseRegOffset { .. } => unreachable!(),
                    &Address::RegBaseImmOffset { base, offset } => {
                        let base_name = self.register_name_map.get(&base).unwrap();
                        let op = if offset >= 0 { "+" } else { "-" };
                        let offset = offset.abs();
                        println!(
                            "mov {} ptr [{} {} {}], {}",
                            ptr_notation, base_name, op, offset, src_name
                        );
                    }
                    &Address::RegBaseRegOffset { base, offset } => {
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
                    JumpTargetKind::BasicBlock(bb) => *bb,
                };
                use self::JumpCondKind::*;
                match kind {
                    &Unconditional => {
                        println!("jmp {}", target.get_name());
                    }
                    &Eq0(preg) => self.emit_jump("test", "jz", target, preg, preg),
                    &Neq0(preg) => self.emit_jump("test", "jnz", target, preg, preg),
                    &Eq(preg1, preg2) => self.emit_jump("cmp", "jz", target, preg1, preg2),
                    &Neq(preg1, preg2) => self.emit_jump("cmp", "jnz", target, preg1, preg2),
                    &LtS(preg1, preg2) => self.emit_jump("cmp", "jl", target, preg1, preg2),
                    &LtU(preg1, preg2) => self.emit_jump("cmp", "jb", target, preg1, preg2),
                    &LeS(preg1, preg2) => self.emit_jump("cmp", "jle", target, preg1, preg2),
                    &LeU(preg1, preg2) => self.emit_jump("cmp", "jbe", target, preg1, preg2),
                    &GtS(preg1, preg2) => self.emit_jump("cmp", "jg", target, preg1, preg2),
                    &GtU(preg1, preg2) => self.emit_jump("cmp", "ja", target, preg1, preg2),
                    &GeS(preg1, preg2) => self.emit_jump("cmp", "jge", target, preg1, preg2),
                    &GeU(preg1, preg2) => self.emit_jump("cmp", "jae", target, preg1, preg2),
                    &Table(ref table, preg) => {
                        // ToDo: fix
                        for (i, basic_block) in table.iter().enumerate() {
                            self.emit_binop_reg_imm64("cmp", preg, i as u64);
                            println!("jz {}", basic_block.get_name());
                        }
                        println!("jmp {}", target.get_name());
                    }
                }
            }
            &Call { ref func, .. } => match func {
                &CallTargetKind::Function(f) => println!("call {}", f.get_func_name()),
                &CallTargetKind::Indirect(ref addr) => match addr {
                    &Address::Var(_) => unreachable!(),
                    &Address::LabelBaseImmOffset { .. } => unreachable!(),
                    &Address::LabelBaseRegOffset { .. } => unreachable!(),
                    &Address::RegBaseImmOffset { .. } => unimplemented!(),
                    &Address::RegBaseRegOffset { .. } => unimplemented!(),
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
                },
            },
            &Return { .. } => {
                println!("pop rbx");
                println!("mov rsp, rbp");
                println!("pop rbp");
                println!("ret");
            }
            &AddressOf { dst, ref location } => {
                let dst_name = self.register_name_map.get(&dst).unwrap();
                match location {
                    &Address::Var(var) => {
                        match var.get_region().get_kind() {
                            &RegionKind::Local => unreachable!(),
                            _ => {}
                        }
                        let ipr_name = self
                            .register_name_map
                            .get(&self.instruction_pointer_register)
                            .unwrap();
                        println!("lea {}, [{} + {}]", dst_name, ipr_name, var.get_name());
                    }
                    &Address::LabelBaseImmOffset { base, offset } => {
                        match base.get_kind() {
                            &RegionKind::Local => unreachable!(),
                            _ => {}
                        }
                        let ipr_name = self
                            .register_name_map
                            .get(&self.instruction_pointer_register)
                            .unwrap();
                        let offset_op = if offset >= 0 { "+" } else { "-" };
                        let offset = offset.abs();
                        println!(
                            "lea {}, [{} + {} {} {}]",
                            dst_name,
                            ipr_name,
                            base.get_name(),
                            offset_op,
                            offset,
                        );
                    }
                    &Address::LabelBaseRegOffset { .. } => unreachable!(),
                    &Address::RegBaseImmOffset { .. } => unreachable!(),
                    &Address::RegBaseRegOffset { .. } => unreachable!(),
                    &Address::RegBaseRegIndex { .. } => unreachable!(),
                }
            }
            &Push { ref src } => match src {
                &OperandKind::Register(reg) => {
                    assert!(reg.is_physical());
                    let reg_name = self.register_name_map.get(&reg).unwrap();
                    println!("push {}", reg_name);
                }
                &OperandKind::ImmI8(imm) => println!("push {}", imm),
                &OperandKind::ImmI32(imm) => println!("push {}", imm),
                &OperandKind::ImmI64(imm) => println!("push {}", imm),
            },
            &Pop { ref dst } => {
                assert!(dst.is_physical());
                let dst_name = self.register_name_map.get(&dst).unwrap();
                println!("pop {}", dst_name);
            }
        }
    }
}

impl<'a> EmitX86AssemblyPass<'a> {
    fn new(emit_assembly_pass: &'a EmitAssemblyFunctionPass) -> EmitX86AssemblyPass<'a> {
        EmitX86AssemblyPass {
            register_name_map: &emit_assembly_pass.register_name_map,
            instruction_pointer_register: emit_assembly_pass.instruction_pointer_register,
        }
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

    fn emit_jump(
        &mut self,
        comp_op: &'static str,
        jump_op: &'static str,
        target: BasicBlockHandle,
        lhs: RegisterHandle,
        rhs: RegisterHandle,
    ) {
        self.emit_binop_reg_reg(comp_op, lhs, rhs);
        println!("{} {}", jump_op, target.get_name());
    }
}

#[derive(Debug)]
struct EmitAssemblyBasicBlockPass<'a> {
    emit_assembly_function_pass: &'a EmitAssemblyFunctionPass<'a>,
}

impl<'a> BasicBlockPass for EmitAssemblyBasicBlockPass<'a> {
    fn do_action(&mut self, basic_block: BasicBlockHandle) {
        println!("{}:", basic_block.get_name());
        basic_block.apply_instr_pass(&mut EmitX86AssemblyPass::new(
            self.emit_assembly_function_pass,
        ));
    }
}

impl<'a> EmitAssemblyBasicBlockPass<'a> {
    fn new(
        emit_assembly_function_pass: &'a EmitAssemblyFunctionPass<'a>,
    ) -> EmitAssemblyBasicBlockPass<'a> {
        EmitAssemblyBasicBlockPass {
            emit_assembly_function_pass,
        }
    }
}

#[derive(Debug)]
struct EmitAssemblyFunctionPass<'a> {
    register_name_map: &'a HashMap<RegisterHandle, &'static str>,
    base_pointer_register: RegisterHandle,
    stack_pointer_register: RegisterHandle,
    instruction_pointer_register: RegisterHandle,
    argument_registers: &'a Vec<HashMap<Type, RegisterHandle>>,
}

impl<'a> FunctionPass for EmitAssemblyFunctionPass<'a> {
    fn do_action(&mut self, function: FunctionHandle) {
        println!();

        match function.get_linkage() {
            &Linkage::Export => println!(".global {}", function.get_func_name()),
            &Linkage::Import => return,
            &Linkage::Private => (),
        }
        println!("{}:", function.get_func_name());

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
        println!(
            "sub {}, {}",
            stack_pointer_register,
            function.get_local_region().get_region_size().unwrap()
        );
        println!("push rbx");

        function.apply_basic_block_pass(&mut EmitAssemblyBasicBlockPass::new(self));
    }
}

impl<'a> EmitAssemblyFunctionPass<'a> {
    fn new(
        physical_register_name_map: &'a HashMap<RegisterHandle, &'static str>,
        base_pointer_register: RegisterHandle,
        stack_pointer_register: RegisterHandle,
        instruction_pointer_register: RegisterHandle,
        argument_registers: &'a Vec<HashMap<Type, RegisterHandle>>,
    ) -> EmitAssemblyFunctionPass<'a> {
        EmitAssemblyFunctionPass {
            register_name_map: physical_register_name_map,
            base_pointer_register,
            stack_pointer_register,
            instruction_pointer_register,
            argument_registers,
        }
    }
}

#[derive(Debug)]
pub struct EmitAssemblyModulePass {
    register_name_map: HashMap<RegisterHandle, &'static str>,
    base_pointer_register: RegisterHandle,
    stack_pointer_register: RegisterHandle,
    instruction_pointer_register: RegisterHandle,
    argument_registers: Vec<HashMap<Type, RegisterHandle>>,
}

impl ModulePass for EmitAssemblyModulePass {
    fn do_action(&mut self, module: ModuleHandle) {
        module.apply_module_pass(&mut ModuleInitPass::new());
        module.apply_function_pass(&mut EmitAssemblyFunctionPass::new(
            &self.register_name_map,
            self.base_pointer_register,
            self.stack_pointer_register,
            self.instruction_pointer_register,
            &self.argument_registers,
        ));
    }
}

impl EmitAssemblyModulePass {
    pub fn new(
        physical_register_name_map: HashMap<RegisterHandle, &'static str>,
        base_pointer_register: RegisterHandle,
        stack_pointer_register: RegisterHandle,
        instruction_pointer_register: RegisterHandle,
        argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    ) -> EmitAssemblyModulePass {
        EmitAssemblyModulePass {
            register_name_map: physical_register_name_map,
            base_pointer_register,
            stack_pointer_register,
            instruction_pointer_register,
            argument_registers,
        }
    }
}
