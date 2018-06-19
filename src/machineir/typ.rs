#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    I32,
}

impl Type {
    pub fn get_size(&self) -> usize {
        use self::Type::*;
        match self {
            &I32 => 4,
        }
    }
}
