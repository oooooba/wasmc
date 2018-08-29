use std::collections::HashMap;

use context::handle::{BasicBlockHandle, FunctionHandle, InstrHandle, RegisterHandle};
use context::Context;
use machineir::opcode::{BinaryOpKind, JumpCondKind, Opcode};
use machineir::operand::{MemoryKind, Operand, OperandKind};
use machineir::typ::Type;
use pass::FunctionPass;

#[derive(Debug)]
pub struct SimpleRegisterAllocationPass {
    physical_registers: Vec<HashMap<Type, RegisterHandle>>,
    physical_argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    physical_result_register: HashMap<Type, RegisterHandle>,
    virtual_register_indexes: HashMap<RegisterHandle, usize>,
}

impl FunctionPass for SimpleRegisterAllocationPass {
    fn do_action(&mut self, mut function: FunctionHandle) {
        for basic_block_i in 0..function.get_mut_basic_blocks().len() {
            let mut basic_block = function.get_mut_basic_blocks()[basic_block_i];

            let mut iter = basic_block.iterator();
            while let Some(mut instr) = iter.get() {
                let (new_opcode, num_advance) = match instr.get_opcode() {
                    &Opcode::Debug(..) => (None, 0),
                    &Opcode::Label(..) => (None, 0),
                    &Opcode::Copy { ref dst, ref src } => {
                        let new_src = match src.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr =
                                    self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg =
                                    *self.physical_result_register.get(vreg.get_typ()).unwrap();
                                let store_instr =
                                    self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (
                            Some(Opcode::Copy {
                                dst: new_dst,
                                src: new_src,
                            }),
                            1,
                        )
                    }
                    &Opcode::UnaryOp {
                        ref kind,
                        ref dst,
                        ref src,
                    } => {
                        let new_src = match src.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr =
                                    self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            &OperandKind::ConstI32(_) => src.clone(),
                            &OperandKind::ConstI64(_) => src.clone(),
                            _ => unimplemented!(),
                        };

                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg =
                                    *self.physical_result_register.get(vreg.get_typ()).unwrap();
                                let store_instr =
                                    self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (
                            Some(Opcode::UnaryOp {
                                kind: kind.clone(),
                                dst: new_dst,
                                src: new_src,
                            }),
                            1,
                        )
                    }
                    &Opcode::BinaryOp {
                        ref kind,
                        ref dst,
                        ref src1,
                        ref src2,
                    } => {
                        let new_src1 = match src1.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr =
                                    self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        let new_src2 = match src2.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let index = match kind {
                                    &BinaryOpKind::Shl
                                    | &BinaryOpKind::Shr
                                    | &BinaryOpKind::Sar => 2,
                                    _ => 1,
                                };
                                let preg = self.allocate_physical_register(vreg, index);
                                let load_instr =
                                    self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            &OperandKind::ConstI32(_) => src2.clone(),
                            &OperandKind::ConstI64(_) => src2.clone(),
                            _ => unimplemented!(),
                        };

                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg =
                                    *self.physical_result_register.get(vreg.get_typ()).unwrap();
                                let store_instr =
                                    self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (
                            Some(Opcode::BinaryOp {
                                kind: kind.clone(),
                                dst: new_dst,
                                src1: new_src1,
                                src2: new_src2,
                            }),
                            1,
                        )
                    }
                    &Opcode::Load { ref dst, ref src } => {
                        let new_dst = match dst.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg =
                                    *self.physical_result_register.get(vreg.get_typ()).unwrap();
                                let store_instr =
                                    self.create_store_instr(basic_block, vreg, preg, function);
                                iter.insert_after(store_instr);
                                Operand::new_physical_register(preg)
                            }
                            _ => unimplemented!(),
                        };

                        (
                            Some(Opcode::Load {
                                dst: new_dst,
                                src: src.clone(),
                            }),
                            1,
                        )
                    }
                    &Opcode::Store { ref dst, ref src } => {
                        let new_src = match src.get_kind() {
                            &OperandKind::Register(vreg) => {
                                let preg = self.allocate_physical_register(vreg, 0);
                                let load_instr =
                                    self.create_load_instr(basic_block, preg, vreg, function);
                                iter.insert_before(load_instr);
                                Operand::new_physical_register(preg)
                            }
                            &OperandKind::ConstI32(_) => src.clone(),
                            _ => unimplemented!(),
                        };

                        (
                            Some(Opcode::Store {
                                dst: dst.clone(),
                                src: new_src,
                            }),
                            0,
                        )
                    }
                    &Opcode::Jump {
                        ref kind,
                        ref target,
                    } => {
                        use self::JumpCondKind::*;
                        let new_cond_kind = match kind {
                            &Unconditional => Unconditional,
                            &Eq0(reg) | &Neq0(reg) => {
                                assert!(!reg.is_physical());
                                let preg = self.allocate_physical_register(reg, 0);
                                let load_instr =
                                    self.create_load_instr(basic_block, preg, reg, function);
                                iter.insert_before(load_instr);
                                match kind {
                                    &Eq0(_) => Eq0(preg),
                                    &Neq0(_) => Neq0(preg),
                                    _ => unreachable!(),
                                }
                            }
                            &Eq(reg1, reg2)
                            | &Neq(reg1, reg2)
                            | &LtU(reg1, reg2)
                            | &LeU(reg1, reg2)
                            | &GtS(reg1, reg2)
                            | &GeS(reg1, reg2)
                            | &GeU(reg1, reg2) => {
                                assert!(!reg1.is_physical());
                                let preg1 = self.allocate_physical_register(reg1, 0);
                                let load_instr1 =
                                    self.create_load_instr(basic_block, preg1, reg1, function);
                                iter.insert_before(load_instr1);

                                assert!(!reg2.is_physical());
                                let preg2 = self.allocate_physical_register(reg2, 1);
                                let load_instr2 =
                                    self.create_load_instr(basic_block, preg2, reg2, function);
                                iter.insert_before(load_instr2);

                                match kind {
                                    &Eq(_, _) => Eq(preg1, preg2),
                                    &Neq(_, _) => Neq(preg1, preg2),
                                    &LtU(_, _) => LtU(preg1, preg2),
                                    &LeU(_, _) => LeU(preg1, preg2),
                                    &GtS(_, _) => GtS(preg1, preg2),
                                    &GeS(_, _) => GeS(preg1, preg2),
                                    &GeU(_, _) => GeU(preg1, preg2),
                                    _ => unreachable!(),
                                }
                            }
                        };

                        (
                            Some(Opcode::Jump {
                                kind: new_cond_kind,
                                target: target.clone(),
                            }),
                            0,
                        )
                    }
                    &Opcode::Call {
                        ref func,
                        ref result,
                        ref args,
                    } => {
                        let mut new_args = vec![];
                        for (i, arg) in args.iter().enumerate() {
                            let vreg = arg.get_as_register().unwrap();
                            let preg = self.allocate_physical_argument_register(vreg, i);
                            let load_instr =
                                self.create_load_instr(basic_block, preg, vreg, function);
                            iter.insert_before(load_instr);
                            new_args.push(Operand::new_physical_register(preg));
                        }

                        let new_result = if let &Some(ref result) = result {
                            let vreg = result.get_as_register().unwrap();
                            let preg = *self.physical_result_register.get(vreg.get_typ()).unwrap();
                            let store_instr =
                                self.create_store_instr(basic_block, vreg, preg, function);
                            iter.insert_after(store_instr);
                            Some(Operand::new_physical_register(preg))
                        } else {
                            None
                        };

                        (
                            Some(Opcode::Call {
                                func: func.clone(),
                                result: new_result,
                                args: new_args,
                            }),
                            1,
                        )
                    }
                    &Opcode::Return {
                        result: Some(ref result),
                    } => {
                        let vreg = result.get_as_register().unwrap();
                        let preg = *self.physical_result_register.get(vreg.get_typ()).unwrap();
                        let load_instr = self.create_load_instr(basic_block, preg, vreg, function);
                        iter.insert_before(load_instr);
                        let new_result = Operand::new_physical_register(preg);

                        (
                            Some(Opcode::Return {
                                result: Some(new_result),
                            }),
                            0,
                        )
                    }
                    &Opcode::Return { .. } => (None, 0),
                };

                if let Some(new_opcode) = new_opcode {
                    instr.set_opcode(new_opcode);
                }

                iter.advance(); // current instr to next instr
                for _ in 0..num_advance {
                    // skip inserted instrs
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
        physical_result_register: HashMap<Type, RegisterHandle>,
    ) -> Box<SimpleRegisterAllocationPass> {
        Box::new(SimpleRegisterAllocationPass {
            physical_registers,
            physical_argument_registers,
            physical_result_register,
            virtual_register_indexes: HashMap::new(),
        })
    }

    fn get_or_create_virtual_register_index(
        &mut self,
        vreg: RegisterHandle,
        mut function: FunctionHandle,
    ) -> usize {
        assert!(!vreg.is_physical());
        let mut vreg_index = self.virtual_register_indexes.get(&vreg).map(|i| *i);
        if vreg_index.is_none() {
            let new_index = function.get_local_variables().len();
            self.virtual_register_indexes.insert(vreg, new_index);
            function
                .get_mut_local_variables()
                .insert(new_index, vreg.get_typ().clone());
            vreg_index = Some(new_index);
        }
        vreg_index.unwrap()
    }

    fn create_load_instr(
        &mut self,
        basic_block: BasicBlockHandle,
        preg: RegisterHandle,
        vreg: RegisterHandle,
        function: FunctionHandle,
    ) -> InstrHandle {
        assert!(preg.is_physical());
        assert!(!vreg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let vreg_index = self.get_or_create_virtual_register_index(vreg, function);
        Context::create_instr(
            Opcode::Load {
                dst: Operand::new_physical_register(preg),
                src: Operand::new_memory(vreg_index, typ, MemoryKind::Local),
            },
            basic_block,
        )
    }

    fn create_store_instr(
        &mut self,
        basic_block: BasicBlockHandle,
        vreg: RegisterHandle,
        preg: RegisterHandle,
        function: FunctionHandle,
    ) -> InstrHandle {
        assert!(!vreg.is_physical());
        assert!(preg.is_physical());
        let typ = vreg.get_typ().clone();
        assert_eq!(&typ, preg.get_typ());
        let vreg_index = self.get_or_create_virtual_register_index(vreg, function);
        Context::create_instr(
            Opcode::Store {
                dst: Operand::new_memory(vreg_index, typ, MemoryKind::Local),
                src: Operand::new_physical_register(preg),
            },
            basic_block,
        )
    }

    fn allocate_physical_register(&self, vreg: RegisterHandle, index: usize) -> RegisterHandle {
        assert!(!vreg.is_physical());
        let preg = *self.physical_registers[index].get(vreg.get_typ()).unwrap();
        assert!(preg.is_physical());
        preg
    }

    fn allocate_physical_argument_register(
        &self,
        vreg: RegisterHandle,
        index: usize,
    ) -> RegisterHandle {
        assert!(!vreg.is_physical());
        let preg = *self.physical_argument_registers[index]
            .get(vreg.get_typ())
            .unwrap();
        assert!(preg.is_physical());
        preg
    }
}
