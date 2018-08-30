use std::fmt;

use context::handle::RegionHandle;

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
}

impl Region {
    pub fn new(handle: RegionHandle, kind: RegionKind) -> Region {
        Region { handle, kind }
    }

    pub fn get_handle(&self) -> &RegionHandle {
        &self.handle
    }

    pub fn get_kind(&self) -> &RegionKind {
        &self.kind
    }
}

impl fmt::Display for Region {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}
