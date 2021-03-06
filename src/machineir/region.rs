use std::collections::HashMap;
use std::fmt;

use context::handle::{RegionHandle, RegisterHandle, VariableHandle};
use context::Context;
use machineir::module::Linkage;
use machineir::opcode::{ConstKind, Opcode};
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
    offset_map: HashMap<VariableHandle, usize>,
    initial_value_map: HashMap<VariableHandle, Option<ConstKind>>,
    initial_value_map_deprecated: HashMap<RegisterHandle, Opcode>,
    region_size: Option<usize>,
    linkage: Linkage,
}

impl Region {
    pub fn new(handle: RegionHandle, kind: RegionKind) -> Region {
        let variable = Context::create_variable(Type::Pointer, handle);
        Region {
            handle,
            name: format!("region_{}", handle),
            kind,
            variable,
            offset_map: HashMap::new(),
            initial_value_map: HashMap::new(),
            initial_value_map_deprecated: HashMap::new(),
            region_size: None,
            linkage: Linkage::Private,
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

    pub fn get_offset_map(&self) -> &HashMap<VariableHandle, usize> {
        &self.offset_map
    }

    pub fn get_mut_offset_map(&mut self) -> &mut HashMap<VariableHandle, usize> {
        self.region_size = None;
        &mut self.offset_map
    }

    pub fn get_initial_value_map(&self) -> &HashMap<VariableHandle, Option<ConstKind>> {
        &self.initial_value_map
    }

    pub fn get_variable(&self) -> VariableHandle {
        self.variable
    }

    pub fn get_region_size(&self) -> &Option<usize> {
        &self.region_size
    }

    pub fn get_linkage(&self) -> &Linkage {
        &self.linkage
    }

    pub fn set_linkage(&mut self, linkage: Linkage) -> RegionHandle {
        self.linkage = linkage;
        self.handle
    }

    pub fn calculate_variable_offset(&mut self) -> usize {
        let word_size = 8;
        let mut tmp_pairs = vec![];
        let mut len_buffer = word_size;

        for var in self.offset_map.keys() {
            tmp_pairs.push((*var, len_buffer));
            let typ = var.get_type();
            len_buffer += ((typ.get_size() + word_size - 1) / word_size) * word_size;
        }
        for (var, offset) in tmp_pairs.into_iter() {
            self.offset_map.insert(var, offset);
        }

        self.region_size = Some(len_buffer);
        len_buffer
    }

    pub fn create_variable(
        &mut self,
        typ: Type,
        initial_value: Option<ConstKind>,
    ) -> VariableHandle {
        self.region_size = None;
        let var = Context::create_variable(typ, self.handle);
        self.offset_map.insert(var, 0);
        self.initial_value_map.insert(var, initial_value);
        var
    }
}

impl fmt::Display for Region {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
