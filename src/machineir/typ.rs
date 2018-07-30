#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    I8,
    I32,
    I64,
}

impl Type {
    pub fn get_size(&self) -> usize {
        use self::Type::*;
        match self {
            &I8 => 1,
            &I32 => 4,
            &I64 => 8,
        }
    }

    pub fn get_ptr_notation(&self) -> &'static str {
        match self {
            &Type::I8 => "byte",
            &Type::I32 => "dword",
            &Type::I64 => "qword",
        }
    }
}
