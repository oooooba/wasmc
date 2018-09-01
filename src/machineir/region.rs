use std::collections::HashMap;
use std::fmt;

use context::handle::{RegionHandle, RegisterHandle};
use context::Context;
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
    kind: RegionKind,
    variable: RegisterHandle,
    offset_map: HashMap<RegisterHandle, usize>,
}

impl Region {
    pub fn new(handle: RegionHandle, kind: RegionKind) -> Region {
        let variable = Context::create_register(Type::I8);
        Region {
            handle,
            kind,
            variable,
            offset_map: HashMap::new(),
        }
    }

    pub fn get_handle(&self) -> &RegionHandle {
        &self.handle
    }

    pub fn get_kind(&self) -> &RegionKind {
        &self.kind
    }

    pub fn get_offset_map(&self) -> &HashMap<RegisterHandle, usize> {
        &self.offset_map
    }

    pub fn get_mut_offset_map(&mut self) -> &mut HashMap<RegisterHandle, usize> {
        &mut self.offset_map
    }

    pub fn get_variable(&self) -> RegisterHandle {
        self.variable
    }

    pub fn calculate_variable_offset(&mut self) -> usize {
        let word_size = 8;
        let mut tmp_pairs = vec![];
        let mut len_buffer = word_size;
        for var in self.offset_map.keys() {
            tmp_pairs.push((*var, len_buffer));
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
