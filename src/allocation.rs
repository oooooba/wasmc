use std::collections::HashMap;

use context::Context;
use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, RegisterHandle};
use machineir::opcode::Opcode;
use machineir::operand::{Operand, OperandKind};
use pass::{BasicBlockPass, FunctionPass, InstrPass};

#[derive(Debug)]
pub struct SimpleRegisterAllocationPass {
    physical_registers: Vec<RegisterHandle>,
}

impl BasicBlockPass for SimpleRegisterAllocationPass {
    fn do_action(&mut self, mut basic_block: BasicBlockHandle) {
        let num_instrs = basic_block.get_instrs().len();
        let mut num_insertion = 0;

        for instr_i in 0..num_instrs {
            let mut instr = basic_block.get_mut_instrs()[instr_i + num_insertion];

            if instr.get_opcode().is_jump_instr() {
                let cond_reg = if let Some(cond) = instr.get_opcode().get_condition_register_operand() {
                    cond.get_as_register().unwrap()
                } else {
                    continue;
                };
                let preg = self.physical_registers[0];
                self.emit_load_instr(basic_block, instr_i + num_insertion, preg, cond_reg);
                num_insertion += 1;
                instr.get_mut_opcode().set_condition_operand(Operand::new_physical_register(preg));
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
                let preg = self.physical_registers[src_i - 1];
                self.emit_load_instr(basic_block, insertion_point, preg, vreg);
                num_insertion += 1;
                instr.get_mut_opcode().set_source_operand(src_i, Operand::new_physical_register(preg));
            }

            // replace destination register
            let preg = self.physical_registers[0];
            let vreg = instr.get_opcode().get_destination_register_operand().and_then(|o| o.get_as_register()).unwrap();
            instr.get_mut_opcode().set_destination_operand(Operand::new_physical_register(preg));

            // store destination register
            let insertion_point = instr_i + num_insertion + 1;
            self.emit_store_instr(basic_block, insertion_point, vreg, preg);
            num_insertion += 1;
        }
    }
}

impl SimpleRegisterAllocationPass {
    pub fn create(physical_registers: Vec<RegisterHandle>) -> Box<SimpleRegisterAllocationPass> {
        Box::new(SimpleRegisterAllocationPass {
            physical_registers: physical_registers,
        })
    }

    fn emit_load_instr(&self, mut basic_block: BasicBlockHandle, insertion_point: usize, preg: RegisterHandle, vreg: RegisterHandle) {
        assert!(preg.is_physical());
        assert!(!vreg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let load_instr = Context::create_instr(Opcode::Load(typ,
                                                            Operand::new_physical_register(preg),
                                                            Operand::new_register(vreg)), basic_block);
        basic_block.get_mut_instrs().insert(insertion_point, load_instr);
    }

    fn emit_store_instr(&self, mut basic_block: BasicBlockHandle, insertion_point: usize, vreg: RegisterHandle, preg: RegisterHandle) {
        assert!(!vreg.is_physical());
        assert!(preg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let store_instr = Context::create_instr(Opcode::Store(typ,
                                                              Operand::new_register(vreg),
                                                              Operand::new_physical_register(preg)), basic_block);
        basic_block.get_mut_instrs().insert(insertion_point, store_instr);
    }
}

#[derive(Debug)]
pub struct AnalyzeMemoryOffsetPass {}

impl InstrPass for AnalyzeMemoryOffsetPass {
    fn do_action(&mut self, instr: InstrHandle) {
        if instr.get_opcode().is_jump_instr() {
            if let Some(cond) = instr.get_opcode().get_condition_register_operand() {
                let mut cond_reg = cond.get_as_register().unwrap();
                let offset = self.decide_offset(cond_reg);
                cond_reg.set_offset(offset);
            }
            return;
        }

        for src_i in 1..instr.get_opcode().get_source_operands().len() + 1 {
            if let Some(mut src_reg) = instr.get_opcode().get_source_operand(src_i) {
                if !src_reg.is_register() {
                    continue;
                }
                let mut reg = src_reg.get_as_register().unwrap();
                let offset = self.decide_offset(reg);
                reg.set_offset(offset);
            }
        }

        let reg = instr.get_opcode().get_destination_register_operand().unwrap();
        let mut reg = reg.get_as_register().unwrap();
        let offset = self.decide_offset(reg);
        reg.set_offset(offset);
    }
}

impl AnalyzeMemoryOffsetPass {
    pub fn create() -> Box<AnalyzeMemoryOffsetPass> {
        Box::new(AnalyzeMemoryOffsetPass {})
    }

    fn decide_offset(&self, reg: RegisterHandle) -> usize {
        format!("{}", reg).parse::<usize>().unwrap() // ToDo: fix
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
pub struct PreEmitAssemblyPass {}

impl FunctionPass for PreEmitAssemblyPass {
    fn do_action(&mut self, _function: FunctionHandle) {
        println!(".intel_syntax noprefix");
        println!(".global {}", "entry_point");
        println!();
        println!("entry_point:");
        println!("push rbp");
        println!("mov rbp, rsp");
    }
}

impl PreEmitAssemblyPass {
    pub fn create() -> Box<PreEmitAssemblyPass> {
        Box::new(PreEmitAssemblyPass {})
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
            &Add(_, ref dst, ref src1, ref src2) => {
                assert_eq!(dst, src1);
                let dst = dst.get_as_physical_register().unwrap();
                assert!(dst.is_physical());
                match src2.get_kind() {
                    &OperandKind::PhysicalRegister(preg) => self.emit_binop_reg_reg("add", dst, preg),
                    _ => unimplemented!(),
                }
            }
            &Sub(_, ref dst, ref src1, ref src2) => {
                assert_eq!(dst, src1);
                let dst = dst.get_as_physical_register().unwrap();
                assert!(dst.is_physical());
                match src2.get_kind() {
                    &OperandKind::PhysicalRegister(preg) => self.emit_binop_reg_reg("sub", dst, preg),
                    _ => unimplemented!(),
                }
            }
            &Br(ref target) => {
                let target = target.get_as_label().unwrap();
                println!("jmp label_{}", target);
            }
            &BrIfZero(ref cond, ref target) => {
                let cond = cond.get_as_physical_register().unwrap();
                let target = target.get_as_label().unwrap();
                self.emit_binop_reg_reg("test", cond, cond);
                println!("jz label_{}", target);
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

                let src = src.get_as_register().unwrap();
                assert!(!src.is_physical());
                let src_offset = src.get_offset();

                println!("mov {}, dword ptr [{} - {}]", dst_name, self.base_pointer_register, src_offset * 4);
            }
            &Store(_, ref dst, ref src) => {
                let dst = dst.get_as_register().unwrap();
                assert!(!dst.is_physical());
                let dst_offset = dst.get_offset();

                let src = src.get_as_physical_register().unwrap();
                assert!(src.is_physical());
                let src_name = self.physical_register_name_map.get(&src).unwrap();

                println!("mov dword ptr [{} - {}], {}", self.base_pointer_register, dst_offset * 4, src_name);
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

#[derive(Debug)]
pub struct PostEmitAssemblyPass {}

impl FunctionPass for PostEmitAssemblyPass {
    fn do_action(&mut self, _function: FunctionHandle) {
        println!("pop rbp");
        println!("ret");
    }
}

impl PostEmitAssemblyPass {
    pub fn create() -> Box<PostEmitAssemblyPass> {
        Box::new(PostEmitAssemblyPass {})
    }
}
