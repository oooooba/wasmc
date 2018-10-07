use std::cmp;
use std::collections::{HashMap, VecDeque};

use context::handle::{
    BasicBlockHandle, FunctionHandle, InstrHandle, RegisterHandle, VariableHandle,
};
use context::Context;
use machineir::module::Linkage;
use machineir::opcode::{
    Address, BinaryOpKind, CallTargetKind, CastKind, JumpCondKind, Opcode, OperandKind,
};
use machineir::region::RegionKind;
use machineir::typ::Type;
use pass::FunctionPass;

#[derive(Debug)]
pub struct MemoryAccessInstrInsertionPass<'a> {
    physical_registers: &'a Vec<HashMap<Type, RegisterHandle>>,
    physical_argument_registers: &'a Vec<HashMap<Type, RegisterHandle>>,
    physical_result_register: &'a HashMap<Type, RegisterHandle>,
    physical_stack_pointer_register: RegisterHandle,
    virtual_register_to_local_variable_map: HashMap<RegisterHandle, VariableHandle>,
}

impl<'a> FunctionPass for MemoryAccessInstrInsertionPass<'a> {
    fn do_action(&mut self, mut function: FunctionHandle) {
        for basic_block_i in 0..function.get_mut_basic_blocks().len() {
            let mut basic_block = function.get_mut_basic_blocks()[basic_block_i];

            let mut new_instrs = VecDeque::new();
            for instr in basic_block.get_instrs() {
                let instr = *instr;
                match instr.clone().get_mut_opcode() {
                    &mut Opcode::Debug(..) => new_instrs.push_back(instr),
                    &mut Opcode::Label(..) => new_instrs.push_back(instr),
                    &mut Opcode::Copy { dst, src } => {
                        let var_dst = self.registry_as_local_variable(function, dst);
                        let var_src = self.registry_as_local_variable(function, src);
                        let p_tmp = *self
                            .physical_result_register
                            .get(var_src.get_type())
                            .unwrap();
                        new_instrs.push_back(self.create_load_instr(basic_block, p_tmp, var_src));
                        new_instrs.push_back(self.create_store_instr(basic_block, var_dst, p_tmp));
                    }
                    &mut Opcode::Const { ref mut dst, .. } => {
                        let v_dst = self.registry_as_local_variable(function, *dst);
                        let p_dst = *self.physical_result_register.get(v_dst.get_type()).unwrap();
                        *dst = p_dst;
                        new_instrs.push_back(instr);
                        new_instrs.push_back(self.create_store_instr(basic_block, v_dst, p_dst));
                    }
                    &mut Opcode::Cast {
                        ref mut dst,
                        ref mut src,
                        ..
                    } => {
                        let v_src = self.registry_as_local_variable(function, *src);
                        let p_src = self.allocate_physical_register(0, v_src.get_type());
                        *src = p_src;
                        new_instrs.push_back(self.create_load_instr(basic_block, p_src, v_src));

                        let v_dst = self.registry_as_local_variable(function, *dst);
                        let p_dst = *self.physical_result_register.get(v_dst.get_type()).unwrap();
                        *dst = p_dst;
                        new_instrs.push_back(instr);
                        new_instrs.push_back(self.create_store_instr(basic_block, v_dst, p_dst));
                    }
                    &mut Opcode::BinaryOp {
                        ref kind,
                        ref mut dst,
                        ref mut src1,
                        ref mut src2,
                    } => {
                        let v_src1 = self.registry_as_local_variable(function, *src1);
                        let p_src1 = self.allocate_physical_register(0, v_src1.get_type());
                        *src1 = p_src1;
                        new_instrs.push_back(self.create_load_instr(basic_block, p_src1, v_src1));

                        match src2 {
                            &mut OperandKind::Register(ref mut src2) => {
                                let v_src2 = self.registry_as_local_variable(function, *src2);
                                let index = match kind {
                                    &BinaryOpKind::Sll
                                    | &BinaryOpKind::Srl
                                    | &BinaryOpKind::Sra => 2,
                                    _ => 1,
                                };
                                let p_src2 =
                                    self.allocate_physical_register(index, v_src2.get_type());
                                *src2 = p_src2;
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_src2,
                                    v_src2,
                                ));
                            }
                            &mut OperandKind::ImmI8(_) => {}
                            &mut OperandKind::ImmI32(_) => {}
                            &mut OperandKind::ImmI64(_) => {}
                        };

                        let v_dst = self.registry_as_local_variable(function, *dst);
                        let p_dst = *self.physical_result_register.get(v_dst.get_type()).unwrap();
                        *dst = p_dst;
                        new_instrs.push_back(instr);
                        new_instrs.push_back(self.create_store_instr(basic_block, v_dst, p_dst));
                    }
                    &mut Opcode::Load {
                        ref mut dst,
                        ref mut src,
                    } => {
                        match src {
                            &mut Address::Var(_) => {}
                            &mut Address::LabelBaseImmOffset { .. } => unimplemented!(),
                            &mut Address::LabelBaseRegOffset { base, offset } => {
                                let v_offset = self.registry_as_local_variable(function, offset);

                                let p_base =
                                    *self.physical_registers[0].get(&Type::Pointer).unwrap();
                                new_instrs.push_back(self.create_addressof_instr(
                                    basic_block,
                                    p_base,
                                    base.get_variable(),
                                ));

                                let p_offset =
                                    self.allocate_physical_register(1, v_offset.get_type());
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_offset,
                                    v_offset,
                                ));

                                *src = Address::RegBaseRegOffset {
                                    base: p_base,
                                    offset: p_offset,
                                };
                            }
                            &mut Address::RegBaseImmOffset { .. } => unimplemented!(),
                            &mut Address::RegBaseRegOffset { .. } => unimplemented!(),
                            &mut Address::RegBaseRegIndex { .. } => unimplemented!(),
                        };

                        let v_dst = self.registry_as_local_variable(function, *dst);
                        let p_dst = *self.physical_result_register.get(v_dst.get_type()).unwrap();
                        *dst = p_dst;
                        new_instrs.push_back(instr);
                        new_instrs.push_back(self.create_store_instr(basic_block, v_dst, p_dst));
                    }
                    &mut Opcode::Store {
                        ref mut dst,
                        ref mut src,
                    } => {
                        let v_src = self.registry_as_local_variable(function, *src);
                        let p_src = *self.physical_result_register.get(v_src.get_type()).unwrap();
                        *src = p_src;
                        new_instrs.push_back(self.create_load_instr(basic_block, p_src, v_src));

                        match dst {
                            &mut Address::Var(_) => {}
                            &mut Address::LabelBaseImmOffset { .. } => unimplemented!(),
                            &mut Address::LabelBaseRegOffset { base, offset } => {
                                let v_offset = self.registry_as_local_variable(function, offset);

                                let p_base =
                                    *self.physical_registers[1].get(&Type::Pointer).unwrap();
                                new_instrs.push_back(self.create_addressof_instr(
                                    basic_block,
                                    p_base,
                                    base.get_variable(),
                                ));

                                let p_offset =
                                    self.allocate_physical_register(2, v_offset.get_type());
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_offset,
                                    v_offset,
                                ));

                                *dst = Address::RegBaseRegOffset {
                                    base: p_base,
                                    offset: p_offset,
                                };
                            }
                            &mut Address::RegBaseImmOffset { .. } => unimplemented!(),
                            &mut Address::RegBaseRegOffset { .. } => unimplemented!(),
                            &mut Address::RegBaseRegIndex { .. } => unimplemented!(),
                        };
                        new_instrs.push_back(instr);
                    }
                    &mut Opcode::Jump { ref mut kind, .. } => {
                        use self::JumpCondKind::*;
                        match kind {
                            &mut Unconditional => {}
                            &mut Eq0(ref mut cond) | &mut Neq0(ref mut cond) => {
                                assert!(!cond.is_physical());
                                let v_cond = self.registry_as_local_variable(function, *cond);
                                let p_cond = self.allocate_physical_register(0, v_cond.get_type());
                                *cond = p_cond;
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_cond,
                                    v_cond,
                                ));
                            }
                            &mut Eq(ref mut lhs, ref mut rhs)
                            | &mut Neq(ref mut lhs, ref mut rhs)
                            | &mut LtS(ref mut lhs, ref mut rhs)
                            | &mut LtU(ref mut lhs, ref mut rhs)
                            | &mut LeS(ref mut lhs, ref mut rhs)
                            | &mut LeU(ref mut lhs, ref mut rhs)
                            | &mut GtS(ref mut lhs, ref mut rhs)
                            | &mut GtU(ref mut lhs, ref mut rhs)
                            | &mut GeS(ref mut lhs, ref mut rhs)
                            | &mut GeU(ref mut lhs, ref mut rhs) => {
                                assert!(!lhs.is_physical());
                                let v_lhs = self.registry_as_local_variable(function, *lhs);
                                let p_lhs = self.allocate_physical_register(0, v_lhs.get_type());
                                *lhs = p_lhs;
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_lhs,
                                    v_lhs,
                                ));

                                assert!(!rhs.is_physical());
                                let v_rhs = self.registry_as_local_variable(function, *rhs);
                                let p_rhs = self.allocate_physical_register(1, v_rhs.get_type());
                                *rhs = p_rhs;
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_rhs,
                                    v_rhs,
                                ));
                            }
                            &mut Table(_, ref mut index) => {
                                assert!(!index.is_physical());
                                let v_index = self.registry_as_local_variable(function, *index);
                                let p_index =
                                    self.allocate_physical_register(0, v_index.get_type());
                                *index = p_index;
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_index,
                                    v_index,
                                ));
                            }
                        }
                        new_instrs.push_back(instr);
                    }
                    &mut Opcode::Call {
                        ref mut func,
                        ref mut result,
                        ref mut args,
                    } => {
                        match func {
                            &mut CallTargetKind::Function(_) => {}
                            &mut CallTargetKind::Indirect(ref mut addr) => match addr {
                                &mut Address::Var(_) => {}
                                &mut Address::LabelBaseImmOffset { .. } => {}
                                &mut Address::LabelBaseRegOffset { .. } => {}
                                &mut Address::RegBaseImmOffset { .. } => {}
                                &mut Address::RegBaseRegOffset { .. } => {}
                                &mut Address::RegBaseRegIndex {
                                    ref mut base,
                                    ref mut index,
                                    ..
                                } => {
                                    let v_base = self.registry_as_local_variable(function, *base);
                                    let p_base =
                                        self.allocate_physical_register(0, v_base.get_type());
                                    *base = p_base;
                                    new_instrs.push_back(self.create_load_instr(
                                        basic_block,
                                        p_base,
                                        v_base,
                                    ));

                                    let v_index = self.registry_as_local_variable(function, *index);
                                    let p_index =
                                        self.allocate_physical_register(1, v_index.get_type());
                                    *index = p_index;
                                    new_instrs.push_back(self.create_load_instr(
                                        basic_block,
                                        p_index,
                                        v_index,
                                    ));
                                }
                            },
                        }

                        for i in 0..cmp::min(args.len(), self.physical_argument_registers.len()) {
                            let v_arg = self.registry_as_local_variable(function, args[i]);
                            let p_arg =
                                self.allocate_physical_argument_register(i, v_arg.get_type());
                            args[i] = p_arg;
                            new_instrs.push_back(self.create_load_instr(basic_block, p_arg, v_arg));
                        }

                        let mut num_push_instrs = 0;
                        for i in (self.physical_argument_registers.len()..args.len()).rev() {
                            let v_arg = self.registry_as_local_variable(function, args[i]);
                            let p_arg =
                                *self.physical_result_register.get(v_arg.get_type()).unwrap();
                            new_instrs.push_back(self.create_load_instr(basic_block, p_arg, v_arg));
                            let p_arg = if p_arg.get_typ().get_size() == Type::Pointer.get_size() {
                                p_arg
                            } else {
                                let tmp =
                                    *self.physical_result_register.get(&Type::Pointer).unwrap();
                                new_instrs.push_back(Context::create_instr(
                                    Opcode::Cast {
                                        kind: CastKind::SignExtension,
                                        dst: tmp,
                                        src: p_arg,
                                    },
                                    basic_block,
                                ));
                                tmp
                            };
                            new_instrs.push_back(Context::create_instr(
                                Opcode::Push {
                                    src: OperandKind::Register(p_arg),
                                },
                                basic_block,
                            ));
                            num_push_instrs += 1;
                        }

                        new_instrs.push_back(instr);

                        if num_push_instrs != 0 {
                            new_instrs.push_back(Context::create_instr(
                                Opcode::BinaryOp {
                                    kind: BinaryOpKind::Add,
                                    dst: self.physical_stack_pointer_register,
                                    src1: self.physical_stack_pointer_register,
                                    src2: OperandKind::ImmI64(
                                        (num_push_instrs * Type::Pointer.get_size()) as u64,
                                    ),
                                },
                                basic_block,
                            ));
                        }

                        if let &mut Some(ref mut result) = result {
                            let v_result = self.registry_as_local_variable(function, *result);
                            let p_result = *self
                                .physical_result_register
                                .get(v_result.get_type())
                                .unwrap();
                            *result = p_result;
                            new_instrs.push_back(self.create_store_instr(
                                basic_block,
                                v_result,
                                p_result,
                            ));
                        }
                    }
                    &mut Opcode::Return { ref mut result } => {
                        if let &mut Some(ref mut result) = result {
                            let v_result = self.registry_as_local_variable(function, *result);
                            let p_result = *self
                                .physical_result_register
                                .get(v_result.get_type())
                                .unwrap();
                            *result = p_result;
                            new_instrs.push_back(self.create_load_instr(
                                basic_block,
                                p_result,
                                v_result,
                            ));
                        }
                        new_instrs.push_back(instr);
                    }
                    &mut Opcode::AddressOf { .. } => unimplemented!(),
                    &mut Opcode::Push { ref mut src } => {
                        match src {
                            &mut OperandKind::Register(ref mut src) => {
                                let v_src = self.registry_as_local_variable(function, *src);
                                let p_src = self.allocate_physical_register(1, v_src.get_type());
                                *src = p_src;
                                new_instrs.push_back(self.create_load_instr(
                                    basic_block,
                                    p_src,
                                    v_src,
                                ));
                            }
                            &mut OperandKind::ImmI8(_) => {}
                            &mut OperandKind::ImmI32(_) => {}
                            &mut OperandKind::ImmI64(_) => {}
                        }
                        new_instrs.push_back(instr);
                    }
                    &mut Opcode::Pop { ref mut dst } => {
                        let v_dst = self.registry_as_local_variable(function, *dst);
                        let p_dst = *self.physical_result_register.get(v_dst.get_type()).unwrap();
                        *dst = p_dst;
                        new_instrs.push_back(instr);
                        new_instrs.push_back(self.create_store_instr(basic_block, v_dst, p_dst));
                    }
                }
            }
            basic_block.set_instrs(new_instrs);
        }
    }
}

impl<'a> MemoryAccessInstrInsertionPass<'a> {
    pub fn new(
        simple_register_allocation_pass: &'a SimpleRegisterAllocationPass,
    ) -> MemoryAccessInstrInsertionPass<'a> {
        MemoryAccessInstrInsertionPass {
            physical_registers: &simple_register_allocation_pass.physical_registers,
            physical_argument_registers: &simple_register_allocation_pass
                .physical_argument_registers,
            physical_result_register: &simple_register_allocation_pass.physical_result_register,
            physical_stack_pointer_register: simple_register_allocation_pass
                .physical_stack_pointer_register,
            virtual_register_to_local_variable_map: HashMap::new(),
        }
    }

    fn create_load_instr(
        &mut self,
        basic_block: BasicBlockHandle,
        reg: RegisterHandle,
        var: VariableHandle,
    ) -> InstrHandle {
        assert!(reg.is_physical());
        assert_eq!(var.get_type(), reg.get_typ());
        Context::create_instr(
            Opcode::Load {
                dst: reg,
                src: Address::Var(var),
            },
            basic_block,
        )
    }

    fn create_store_instr(
        &mut self,
        basic_block: BasicBlockHandle,
        var: VariableHandle,
        reg: RegisterHandle,
    ) -> InstrHandle {
        assert!(reg.is_physical());
        assert_eq!(var.get_type(), reg.get_typ());
        Context::create_instr(
            Opcode::Store {
                dst: Address::Var(var),
                src: reg,
            },
            basic_block,
        )
    }

    fn create_addressof_instr(
        &mut self,
        basic_block: BasicBlockHandle,
        reg: RegisterHandle,
        var: VariableHandle,
    ) -> InstrHandle {
        assert!(reg.is_physical());
        Context::create_instr(
            Opcode::AddressOf {
                dst: reg,
                location: Address::Var(var),
            },
            basic_block,
        )
    }

    fn allocate_physical_register(&mut self, index: usize, typ: &Type) -> RegisterHandle {
        let preg = *self.physical_registers[index].get(typ).unwrap();
        assert!(preg.is_physical());
        preg
    }

    fn allocate_physical_argument_register(&mut self, index: usize, typ: &Type) -> RegisterHandle {
        let preg = *self.physical_argument_registers[index].get(typ).unwrap();
        assert!(preg.is_physical());
        preg
    }

    fn registry_as_local_variable(
        &mut self,
        function: FunctionHandle,
        register: RegisterHandle,
    ) -> VariableHandle {
        assert!(!register.is_physical());
        if let Some(var) = self.virtual_register_to_local_variable_map.get(&register) {
            return *var;
        }
        let var = function
            .get_local_region()
            .create_variable(register.get_typ().clone(), None);
        self.virtual_register_to_local_variable_map
            .insert(register, var);
        var
    }
}

#[derive(Debug)]
pub struct FuncArgsStoreInstrInsertionPass<'a> {
    argument_registers: &'a Vec<HashMap<Type, RegisterHandle>>,
    base_pointer_register: RegisterHandle,
    temporary_register: &'a HashMap<Type, RegisterHandle>,
}

impl<'a> FunctionPass for FuncArgsStoreInstrInsertionPass<'a> {
    fn do_action(&mut self, function: FunctionHandle) {
        if function.get_linkage() == &Linkage::Import {
            return;
        }

        assert_eq!(
            function.get_parameter_types().len(),
            function.get_parameter_variables().len()
        );

        let mut entry_block = function.get_basic_blocks()[0];
        let mut new_entry_instrs = VecDeque::new();

        for i in 0..cmp::min(
            function.get_parameter_types().len(),
            self.argument_registers.len(),
        ) {
            let var = function.get_parameter_variables()[i];
            let instr = Context::create_instr(
                Opcode::Store {
                    dst: Address::Var(var),
                    src: *self.argument_registers[i].get(var.get_type()).unwrap(),
                },
                entry_block,
            );
            new_entry_instrs.push_back(instr);
        }

        for i in self.argument_registers.len()..function.get_parameter_types().len() {
            let var = function.get_parameter_variables()[i];
            let index = i - self.argument_registers.len();
            let tmp = *self.temporary_register.get(var.get_type()).unwrap();
            let load_instr = Context::create_instr(
                Opcode::Load {
                    dst: tmp,
                    src: Address::RegBaseImmOffset {
                        base: self.base_pointer_register,
                        offset: (0x10 + index * Type::Pointer.get_size()) as isize,
                    },
                },
                entry_block,
            );
            let store_instr = Context::create_instr(
                Opcode::Store {
                    dst: Address::Var(var),
                    src: tmp,
                },
                entry_block,
            );
            new_entry_instrs.push_back(load_instr);
            new_entry_instrs.push_back(store_instr);
        }

        new_entry_instrs.append(entry_block.get_mut_instrs());
        entry_block.set_instrs(new_entry_instrs);
    }
}

impl<'a> FuncArgsStoreInstrInsertionPass<'a> {
    pub fn new(
        argument_registers: &'a Vec<HashMap<Type, RegisterHandle>>,
        base_pointer_register: RegisterHandle,
        temporary_register: &'a HashMap<Type, RegisterHandle>,
    ) -> FuncArgsStoreInstrInsertionPass<'a> {
        FuncArgsStoreInstrInsertionPass {
            argument_registers,
            base_pointer_register,
            temporary_register,
        }
    }
}

#[derive(Debug)]
pub struct VariableAddressLoweringPass {
    base_pointer_register: RegisterHandle,
}

impl FunctionPass for VariableAddressLoweringPass {
    fn do_action(&mut self, function: FunctionHandle) {
        for basic_block in function.get_basic_blocks() {
            for instr in basic_block.get_instrs() {
                let instr = *instr;
                match instr.clone().get_mut_opcode() {
                    &mut Opcode::Load {
                        src: ref mut addr, ..
                    }
                    | &mut Opcode::Store {
                        dst: ref mut addr, ..
                    } => match addr {
                        &mut Address::Var(var) => {
                            let region = var.get_region();
                            let offset = *region.get_offset_map().get(&var).unwrap();
                            match region.get_kind() {
                                &RegionKind::Local => {
                                    *addr = Address::RegBaseImmOffset {
                                        base: self.base_pointer_register,
                                        offset: -(offset as isize),
                                    }
                                }
                                &RegionKind::MutableGlobal | &RegionKind::ReadOnlyGlobal => {
                                    *addr = Address::LabelBaseImmOffset {
                                        base: region,
                                        offset: offset as isize,
                                    }
                                }
                                &RegionKind::VariableSizedGlobal { .. } => unimplemented!(),
                            }
                        }
                        &mut Address::LabelBaseImmOffset { .. } => unimplemented!(),
                        &mut Address::LabelBaseRegOffset { .. } => {}
                        &mut Address::RegBaseImmOffset { .. } => {}
                        &mut Address::RegBaseRegOffset { .. } => {}
                        &mut Address::RegBaseRegIndex { .. } => {}
                    },
                    _ => {}
                }
            }
        }
    }
}

impl VariableAddressLoweringPass {
    pub fn new(base_pointer_register: RegisterHandle) -> VariableAddressLoweringPass {
        VariableAddressLoweringPass {
            base_pointer_register,
        }
    }
}

#[derive(Debug)]
pub struct SimpleRegisterAllocationPass {
    physical_registers: Vec<HashMap<Type, RegisterHandle>>,
    physical_argument_registers: Vec<HashMap<Type, RegisterHandle>>,
    physical_result_register: HashMap<Type, RegisterHandle>,
    physical_base_pointer_register: RegisterHandle,
    physical_stack_pointer_register: RegisterHandle,
}

impl FunctionPass for SimpleRegisterAllocationPass {
    fn do_action(&mut self, function: FunctionHandle) {
        function.apply_function_pass(&mut MemoryAccessInstrInsertionPass::new(self));
        function.apply_function_pass(&mut FuncArgsStoreInstrInsertionPass::new(
            &self.physical_argument_registers,
            self.physical_base_pointer_register,
            &self.physical_result_register,
        ));
        function.get_local_region().calculate_variable_offset();
        function.apply_function_pass(&mut VariableAddressLoweringPass::new(
            self.physical_base_pointer_register,
        ))
    }
}

impl SimpleRegisterAllocationPass {
    pub fn new(
        physical_registers: Vec<HashMap<Type, RegisterHandle>>,
        physical_argument_registers: Vec<HashMap<Type, RegisterHandle>>,
        physical_result_register: HashMap<Type, RegisterHandle>,
        physical_base_pointer_register: RegisterHandle,
        physical_stack_pointer_register: RegisterHandle,
    ) -> SimpleRegisterAllocationPass {
        SimpleRegisterAllocationPass {
            physical_registers,
            physical_argument_registers,
            physical_result_register,
            physical_base_pointer_register,
            physical_stack_pointer_register,
        }
    }
}
