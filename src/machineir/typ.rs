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

#[derive(Debug, PartialEq, Eq)]
pub struct ResultType(Vec<Type>);

impl ResultType {
    pub fn new(t: Vec<Type>) -> ResultType {
        ResultType(t)
    }

    pub fn typs(&self) -> &Vec<Type> {
        &self.0
    }
}
