use std::collections::HashMap;

use context::handle::{
    BasicBlockHandle, FunctionHandle, InstrHandle, ModuleHandle, RegionHandle, RegisterHandle,
    VariableHandle,
};
use context::Context;
use machineir::module::Linkage;
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
use wasmir::{Exportdesc, Func, Funcidx, Globalidx, Importdesc, Labelidx, Memidx, Typeidx};

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
struct MemoryInstance {
    instance_region: RegionHandle,
    var_min: u32,
    var_max: Option<u32>,
    initial_image_region: RegionHandle,
    var_offset: u32,
}

#[derive(Debug)]
pub struct WasmToMachine {
    operand_stack: OperandStack,
    current_basic_block: BasicBlockHandle,
    entry_block: BasicBlockHandle,
    basic_block_to_continuation: HashMap<BasicBlockHandle, (BasicBlockHandle, Vec<RegisterHandle>)>,
    current_function: FunctionHandle,
    module: ModuleHandle,
    local_variables: Vec<VariableHandle>,
    global_variables: Vec<VariableHandle>,
    function_types: Vec<(Vec<Type>, Vec<Type>)>,
    num_import_functions: usize,
    memory_instances: Vec<MemoryInstance>,
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
        machine_module
            .create_function(func_name, parameter_types, result_types)
            .set_linkage(linkage);
    }

    fn declare_functions(
        wasm_module: &wasmir::Module,
        function_types: &Vec<(Vec<Type>, Vec<Type>)>,
        machine_module: ModuleHandle,
        export_funcs: HashMap<Funcidx, String>,
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
            let index = num_import_functions + i;
            let (name, linkage) = if let Some(name) = export_funcs.get(&Funcidx::new(index as u32))
            {
                (name.clone(), Linkage::Export)
            } else {
                (format!("f_{}", index), Linkage::Private)
            };
            let typeidx = *func.get_type();
            WasmToMachine::declare_function(name, typeidx, linkage, function_types, machine_module);
        }
        let num_define_functions = machine_module.get_functions().len() - num_import_functions;

        (num_import_functions, num_define_functions)
    }

    fn declare_memory_instances(
        wasm_module: &wasmir::Module,
        mut machine_module: ModuleHandle,
        mut export_mems: HashMap<Memidx, String>,
    ) -> Vec<MemoryInstance> {
        let mut data_map = HashMap::new();
        for data in wasm_module.get_data().iter() {
            let index = data.get_data().as_index();
            data_map.insert(index, data);
        }

        assert!(wasm_module.get_mems().len() == 0 || wasm_module.get_mems().len() == 1);
        let mut memory_instances = vec![];
        for (i, mem) in wasm_module.get_mems().iter().enumerate() {
            let mut instance_region =
                machine_module.create_global_region(RegionKind::MutableGlobal);
            {
                if let Some(name) = export_mems.remove(&Memidx::new(0)) {
                    instance_region.set_name(name).set_linkage(Linkage::Export);
                }

                let member_types = vec![Type::Pointer, Type::I64, Type::I64];
                let mut offset = 0;
                for typ in member_types {
                    let type_size = typ.get_size();
                    let var = instance_region.create_variable(typ, None);
                    instance_region.get_mut_offset_map().insert(var, offset);
                    offset += type_size;
                }
            }

            let var_min = mem.get_type().get_lim().get_min();
            let var_max = mem.get_type().get_lim().get_max().clone();

            let mut initial_image_region =
                machine_module.create_global_region(RegionKind::ReadOnlyGlobal);
            let var_offset = if let Some(data) = data_map.get(&i) {
                for (i, &init) in data.get_init().iter().enumerate() {
                    let var = initial_image_region
                        .create_variable(Type::I8, Some(ConstKind::ConstI8(init)));
                    initial_image_region.get_mut_offset_map().insert(var, i);
                }

                let offset = data.get_offset();
                assert_eq!(offset.get_instr_sequences().len(), 1);
                let offset = match &offset.get_instr_sequences()[0] {
                    &WasmInstr::Const(ref cst) => match cst {
                        &Const::I32(i) => i,
                        &Const::I64(i) => i as u32,
                    },
                    _ => unreachable!(),
                };
                offset
            } else {
                0
            };

            memory_instances.push(MemoryInstance {
                instance_region,
                var_min,
                var_max,
                initial_image_region,
                var_offset,
            });
        }

        memory_instances
    }

    fn declare_global_variables(
        wasm_module: &wasmir::Module,
        mut machine_module: ModuleHandle,
        export_globals: HashMap<Globalidx, String>,
    ) -> Vec<VariableHandle> {
        let export_mutable_region = machine_module
            .create_global_region(RegionKind::MutableGlobal)
            .set_linkage(Linkage::Export);
        let private_mutable_region = machine_module
            .create_global_region(RegionKind::MutableGlobal)
            .set_linkage(Linkage::Private);
        let export_readonly_region = machine_module
            .create_global_region(RegionKind::ReadOnlyGlobal)
            .set_linkage(Linkage::Export);
        let private_readonly_region = machine_module
            .create_global_region(RegionKind::ReadOnlyGlobal)
            .set_linkage(Linkage::Private);

        let mut export_mutable_offset = 0;
        let mut private_mutable_offset = 0;
        let mut export_readonly_offset = 0;
        let mut private_readonly_offset = 0;

        let mut global_variables = vec![];
        for (i, global) in wasm_module.get_globals().iter().enumerate() {
            let index = Globalidx::new(i as u32);
            let mut region = match (
                global.get_type().mutability(),
                export_globals.contains_key(&index),
            ) {
                (&Mut::Var, true) => export_mutable_region,
                (&Mut::Var, false) => private_mutable_region,
                (&Mut::Const, true) => export_readonly_region,
                (&Mut::Const, false) => private_readonly_region,
            };

            let typ = WasmToMachine::map_valtype(global.get_type().valtype());
            let expr = global.get_init();
            assert_eq!(expr.get_instr_sequences().len(), 1);
            let init_val = match &expr.get_instr_sequences()[0] {
                &WasmInstr::Const(ref cst) => match cst {
                    &Const::I32(i) => ConstKind::ConstI32(i),
                    &Const::I64(i) => ConstKind::ConstI64(i),
                },
                _ => unreachable!(),
            };

            let offset = if region == export_mutable_region {
                &mut export_mutable_offset
            } else if region == private_mutable_region {
                &mut private_mutable_offset
            } else if region == export_readonly_region {
                &mut export_readonly_offset
            } else if region == private_readonly_region {
                &mut private_readonly_offset
            } else {
                unreachable!()
            };
            *offset = *offset + typ.get_size();
            let offset = *offset;

            let var = region.create_variable(typ, Some(init_val));
            region.get_mut_offset_map().insert(var, offset);

            global_variables.push(var);
        }

        global_variables
    }

    pub fn new(wasmir_module: &wasmir::Module) -> WasmToMachine {
        let mut module = Context::create_module();

        let (parameter_types, result_types) =
            WasmToMachine::map_functype(&Functype::new(vec![], vec![]));
        let dummy_function =
            Context::create_function("".to_string(), parameter_types, result_types, module);
        let dummy_block = Context::create_basic_block(dummy_function);

        let mut export_funcs = HashMap::new();
        let mut export_tables = HashMap::new();
        let mut export_mems = HashMap::new();
        let mut export_globals = HashMap::new();
        for export in wasmir_module.get_exports().iter() {
            let name = export.get_name().clone();
            match export.get_desc() {
                &Exportdesc::Func(funcidx) => export_funcs.insert(funcidx, name),
                &Exportdesc::Table(tableidx) => export_tables.insert(tableidx, name),
                &Exportdesc::Mem(memidx) => export_mems.insert(memidx, name),
                &Exportdesc::Global(globalidx) => export_globals.insert(globalidx, name),
            };
        }

        let global_variables =
            WasmToMachine::declare_global_variables(wasmir_module, module, export_globals);

        let table = if wasmir_module.get_tables().len() == 0 {
            Context::create_region(RegionKind::VariableSizedGlobal { min: 0, max: None })
        } else {
            assert_eq!(wasmir_module.get_tables().len(), 1);
            let tab = &wasmir_module.get_tables()[0];
            Context::create_region(RegionKind::VariableSizedGlobal {
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
            WasmToMachine::declare_functions(wasmir_module, &function_types, module, export_funcs);

        let memory_instances =
            WasmToMachine::declare_memory_instances(wasmir_module, module, export_mems);

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
            memory_instances,
        }
    }

    pub fn finalize(mut self) -> ModuleHandle {
        self.emit_memory_instance_initialization_function();
        self.module
    }

    fn emit_memory_instance_initialization_function(&mut self) {
        let wasmc_allocate_memory_function = self
            .module
            .create_function(
                "_wasmc_allocate_memory".to_string(),
                vec![Type::Pointer, Type::I32, Type::I64],
                vec![],
            ).set_linkage(Linkage::Import);
        let wasmc_initialize_memory_function = self
            .module
            .create_function(
                "_wasmc_initialize_memory".to_string(),
                vec![Type::Pointer, Type::I32, Type::Pointer, Type::I64],
                vec![],
            ).set_linkage(Linkage::Import);

        let mut function = self
            .module
            .create_function("_wasmc_memory_setup".to_string(), vec![], vec![])
            .set_program_initializer()
            .set_linkage(Linkage::Private);
        let mut body = function.create_entry_block();

        for memory_instance in self.memory_instances.iter() {
            let reg_memory_instance = Context::create_register(Type::Pointer);
            body.emit_instr(Opcode::AddressOf {
                dst: reg_memory_instance,
                location: Address::LabelBaseImmOffset {
                    base: memory_instance.instance_region,
                    offset: 0,
                },
            });

            {
                let reg_min_num_pages = Context::create_register(Type::I32);
                body.emit_instr(Opcode::Const {
                    dst: reg_min_num_pages,
                    src: ConstKind::ConstI32(memory_instance.var_min),
                });

                let reg_max_num_pages = Context::create_register(Type::I64);
                let n = memory_instance.var_max.map(|n| n as i64).unwrap_or(-1);
                body.emit_instr(Opcode::Const {
                    dst: reg_max_num_pages,
                    src: ConstKind::ConstI64(n as u64),
                });

                body.emit_instr(Opcode::Call {
                    func: CallTargetKind::Function(wasmc_allocate_memory_function),
                    result: None,
                    args: vec![reg_memory_instance, reg_min_num_pages, reg_max_num_pages],
                });
            }
            {
                let reg_offset = Context::create_register(Type::I32);
                body.emit_instr(Opcode::Const {
                    dst: reg_offset,
                    src: ConstKind::ConstI32(memory_instance.var_offset),
                });

                let mut initial_image_region = memory_instance.initial_image_region;

                let reg_initial_vec = Context::create_register(Type::Pointer);
                body.emit_instr(Opcode::AddressOf {
                    dst: reg_initial_vec,
                    location: Address::LabelBaseImmOffset {
                        base: initial_image_region,
                        offset: 0,
                    },
                });

                let reg_initial_vec_len = Context::create_register(Type::I64);
                let len = initial_image_region.get_offset_map().len(); // ToDo: fix
                let const_instr = Context::create_instr(
                    Opcode::Const {
                        dst: reg_initial_vec_len,
                        src: ConstKind::ConstI64(len as u64),
                    },
                    body,
                );
                body.add_instr(const_instr);

                let call_instr = Context::create_instr(
                    Opcode::Call {
                        func: CallTargetKind::Function(wasmc_initialize_memory_function),
                        result: None,
                        args: vec![
                            reg_memory_instance,
                            reg_offset,
                            reg_initial_vec,
                            reg_initial_vec_len,
                        ],
                    },
                    body,
                );
                body.add_instr(call_instr);
            }
        }
        let return_instr = Context::create_instr(Opcode::Return { result: None }, body);
        body.add_instr(return_instr);
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
        let then_block = Context::create_basic_block(self.current_function);
        let else_block = Context::create_basic_block(self.current_function);
        let merge_block = Context::create_basic_block(self.current_function);

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
        let new_block = Context::create_basic_block(self.current_function);
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
                let expr_block = Context::create_basic_block(self.current_function);
                let cont_block = Context::create_basic_block(self.current_function);

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
                let body_block = Context::create_basic_block(self.current_function);
                let exit_block = Context::create_basic_block(self.current_function);

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

                let new_block = Context::create_basic_block(self.current_function);
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

                let new_block = Context::create_basic_block(self.current_function);
                self.switch_current_basic_block_to(new_block);
                self.emit_on_current_basic_block(Opcode::Debug("unreachable block".to_string()));
            }
            &WasmInstr::GetLocal(ref localidx) => {
                let index = localidx.as_index();
                let var = self.local_variables[index];
                let dst = Context::create_register(var.get_type().clone());
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
                assert_eq!(src.get_type(), var.get_type());
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
                assert_eq!(src.get_type(), var.get_type());
                self.operand_stack.push_value(src);
                self.emit_on_current_basic_block(Opcode::Store {
                    dst: Address::Var(var),
                    src,
                });
            }
            &WasmInstr::GetGlobal(globalidx) => {
                let index = globalidx.as_index();
                let var = self.global_variables[index];
                let dst = Context::create_register(var.get_type().clone());
                self.operand_stack.push_value(dst);
                self.emit_on_current_basic_block(Opcode::Load {
                    dst,
                    src: Address::Var(var),
                });
            }
            &WasmInstr::SetGlobal(globalidx) => {
                let index = globalidx.as_index();
                let var = self.global_variables[index];
                let src = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                assert_eq!(src.get_type(), var.get_type());
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

                let memory_region = self.memory_instances[0].instance_region;
                let memory = Context::create_register(Type::Pointer);
                self.emit_on_current_basic_block(Opcode::Load {
                    dst: memory,
                    src: Address::LabelBaseImmOffset {
                        base: memory_region,
                        offset: 0,
                    },
                });

                let dst = match attr {
                    &Loadattr::I32 => Context::create_register(Type::I32),
                    &Loadattr::I64 => Context::create_register(Type::I64),
                    &Loadattr::I32x8S | &Loadattr::I32x8U => Context::create_register(Type::I8),
                };

                self.emit_on_current_basic_block(Opcode::Load {
                    dst,
                    src: Address::RegBaseRegOffset {
                        base: memory,
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
                        assert_eq!(value.get_type(), &Type::I32);
                        value
                    }
                    &Storeattr::I64 => {
                        assert_eq!(value.get_type(), &Type::I64);
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

                let memory_region = self.memory_instances[0].instance_region;
                let memory = Context::create_register(Type::Pointer);
                self.emit_on_current_basic_block(Opcode::Load {
                    dst: memory,
                    src: Address::LabelBaseImmOffset {
                        base: memory_region,
                        offset: 0,
                    },
                });

                self.emit_on_current_basic_block(Opcode::Store {
                    dst: Address::RegBaseRegOffset {
                        base: memory,
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

                panic!()
                /*
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
                let table_variable =
                    self.module.get_indirect_function_tables()[0].get_variable_deprecated();
                self.emit_on_current_basic_block(Opcode::Call {
                    func: CallTargetKind::Indirect(Address::RegBaseRegIndex {
                        base: table_variable,
                        index,
                        scale: Type::Pointer,
                    }),
                    result,
                    args,
                });
                */
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
                assert_eq!(cond.get_type(), &Type::I32);
                let val_false = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                let val_true = match self.operand_stack.pop().unwrap() {
                    StackElem::Value(reg) => reg,
                    StackElem::Label(_) => unreachable!(),
                };
                assert_eq!(val_true.get_type(), val_false.get_type());

                let result = Context::create_register(val_true.get_type().clone());
                let bb_true = Context::create_basic_block(self.current_function);
                let bb_false = Context::create_basic_block(self.current_function);
                let bb_merge = Context::create_basic_block(self.current_function);

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
        self.current_basic_block.emit_instr(opcode)
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
            StackElem::Value(reg) if reg.get_type() == &Type::I32 => reg,
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

    fn emit_func(&mut self, func: &Func, function: FunctionHandle) {
        let mut local_variables = function.get_parameter_variables().clone();
        for valtype in func.get_locals().iter() {
            let typ = WasmToMachine::map_valtype(valtype);
            let init_val = match typ {
                Type::I8 => ConstKind::ConstI8(0),
                Type::I32 => ConstKind::ConstI32(0),
                Type::I64 => ConstKind::ConstI64(0),
                Type::Pointer => ConstKind::ConstI64(0),
            };
            let var = function
                .get_local_region()
                .create_variable(typ, Some(init_val));
            local_variables.push(var);
        }

        let entry_block = Context::create_basic_block(function);
        let exit_block = Context::create_basic_block(function);

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

    pub fn emit(&mut self, module: &wasmir::Module) {
        for (i, func) in module.get_funcs().iter().enumerate() {
            let function = self.module.get_functions()[self.num_import_functions + i];
            self.emit_func(func, function);
        }
    }
}
