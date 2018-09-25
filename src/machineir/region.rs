use std::collections::HashMap;
use std::fmt;

use context::handle::{RegionHandle, RegisterHandle};
use context::Context;
use machineir::opcode::Opcode;
use machineir::typ::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RegionKind {
    Local,
    StaticGlobal,
    DynamicGlobal { min: usize, max: Option<usize> },
}

#[derive(PartialEq, Eq, Debug)]
pub struct Region {
    handle: RegionHandle,
    name: String,
    kind: RegionKind,
    variable: RegisterHandle,
    offset_map: HashMap<RegisterHandle, isize>,
    initial_value_map: HashMap<RegisterHandle, Opcode>,
}

impl Region {
    pub fn new(handle: RegionHandle, kind: RegionKind) -> Region {
        let variable = Context::create_register(Type::Pointer);
        Region {
            handle,
            name: format!("region_{}", handle),
            kind,
            variable,
            offset_map: HashMap::new(),
            initial_value_map: HashMap::new(),
        }
    }

    pub fn get_handle(&self) -> &RegionHandle {
        &self.handle
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn set_name(&mut self, name: String) -> RegionHandle {
        self.name = name;
        self.handle
    }

    pub fn get_kind(&self) -> &RegionKind {
        &self.kind
    }

    pub fn get_offset_map(&self) -> &HashMap<RegisterHandle, isize> {
        &self.offset_map
    }

    pub fn get_mut_offset_map(&mut self) -> &mut HashMap<RegisterHandle, isize> {
        &mut self.offset_map
    }

    pub fn get_initial_value_map(&self) -> &HashMap<RegisterHandle, Opcode> {
        &self.initial_value_map
    }

    pub fn get_mut_initial_value_map(&mut self) -> &mut HashMap<RegisterHandle, Opcode> {
        &mut self.initial_value_map
    }

    pub fn get_variable(&self) -> RegisterHandle {
        self.variable
    }

    pub fn calculate_variable_offset(&mut self) -> usize {
        let word_size = 8;
        let mut tmp_pairs = vec![];
        let mut len_buffer = word_size;
        for var in self.offset_map.keys() {
            tmp_pairs.push((*var, -(len_buffer as isize)));
            let typ = var.get_typ();
            len_buffer += ((typ.get_size() + word_size - 1) / word_size) * word_size;
        }
        for (var, offset) in tmp_pairs.into_iter() {
            self.offset_map.insert(var, offset);
        }
        len_buffer
    }
}

impl fmt::Display for Region {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
