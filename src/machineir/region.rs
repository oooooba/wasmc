use std::collections::HashMap;
use std::fmt;

use context::handle::{RegionHandle, RegisterHandle, VariableHandle};
use context::Context;
use machineir::opcode::Opcode;
use machineir::typ::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RegionKind {
    Local,
    MutableGlobal,
    ReadOnlyGlobal,
    VariableSizedGlobal { min: usize, max: Option<usize> },
}

#[derive(PartialEq, Eq, Debug)]
pub struct Region {
    handle: RegionHandle,
    name: String,
    kind: RegionKind,
    variable: VariableHandle,
    variable_deprecated: RegisterHandle,
    offset_map_deprecated: HashMap<RegisterHandle, usize>,
    initial_value_map_deprecated: HashMap<RegisterHandle, Opcode>,
    region_size: Option<usize>,
}

impl Region {
    pub fn new(handle: RegionHandle, kind: RegionKind) -> Region {
        let variable = Context::create_variable(Type::Pointer, handle);
        let variable_deprecated = Context::create_register(Type::Pointer);
        Region {
            handle,
            name: format!("region_{}", handle),
            kind,
            variable,
            variable_deprecated,
            offset_map_deprecated: HashMap::new(),
            initial_value_map_deprecated: HashMap::new(),
            region_size: None,
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

    pub fn get_offset_map_deprecated(&self) -> &HashMap<RegisterHandle, usize> {
        &self.offset_map_deprecated
    }

    pub fn get_mut_offset_map_deprecated(&mut self) -> &mut HashMap<RegisterHandle, usize> {
        self.region_size = None;
        &mut self.offset_map_deprecated
    }

    pub fn get_initial_value_map_deprecated(&self) -> &HashMap<RegisterHandle, Opcode> {
        &self.initial_value_map_deprecated
    }

    pub fn get_mut_initial_value_map_deprecated(&mut self) -> &mut HashMap<RegisterHandle, Opcode> {
        &mut self.initial_value_map_deprecated
    }

    pub fn get_variable(&self) -> VariableHandle {
        self.variable
    }

    pub fn get_variable_deprecated(&self) -> RegisterHandle {
        self.variable_deprecated
    }

    pub fn get_region_size(&self) -> &Option<usize> {
        &self.region_size
    }

    pub fn calculate_variable_offset(&mut self) -> usize {
        let word_size = 8;
        let mut tmp_pairs = vec![];
        let mut len_buffer = word_size;
        for var in self.offset_map_deprecated.keys() {
            tmp_pairs.push((*var, len_buffer));
            let typ = var.get_typ();
            len_buffer += ((typ.get_size() + word_size - 1) / word_size) * word_size;
        }
        for (var, offset) in tmp_pairs.into_iter() {
            self.offset_map_deprecated.insert(var, offset);
        }
        self.region_size = Some(len_buffer);
        len_buffer
    }
}

impl fmt::Display for Region {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
