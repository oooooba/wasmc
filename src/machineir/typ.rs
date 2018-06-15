#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    I32,
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
