use std::fmt;

use context::handle::RegisterHandle;
use machineir::operand::Operand;
use machineir::typ::Type;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOpKind {
    Const,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, PartialEq, Eq)]
pub enum JumpCondKind {
    Unconditional,
    Eq0(RegisterHandle),
    Neq0(RegisterHandle),
    Neq(RegisterHandle, RegisterHandle),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Debug(String),
    Label(String),
    Copy(Type, Operand, Operand),
    Load(Type, Operand, Operand),
    Store(Type, Operand, Operand),
    Eq(Type, Operand, Operand, Operand),
    UnaryOp { typ: Type, kind: UnaryOpKind, dst: Operand, src: Operand },
    BinaryOp { typ: Type, kind: BinaryOpKind, dst: Operand, src1: Operand, src2: Operand },
    Jump { kind: JumpCondKind, target: Operand },
    Call { func: String, typ: Type, result: Option<Operand>, args: Vec<Operand> },
    Return { typ: Type, result: Option<Operand> },
}

impl Opcode {
    pub fn get_source_operand(&self, index: usize) -> Option<&Operand> {
        use self::Opcode::*;
        match self {
            &Copy(_, _, ref operand) if index == 1 => Some(operand),
            &Load(_, _, ref operand) if index == 1 => Some(operand),
            &Store(_, _, ref operand) if index == 1 => Some(operand),
            &Eq(_, _, ref operand, _) if index == 1 => Some(operand),
            &Eq(_, _, _, ref operand) if index == 2 => Some(operand),
            &BinaryOp { ref src1, .. } if index == 1 => Some(src1),
            &BinaryOp { ref src2, .. } if index == 2 => Some(src2),
            &Call { ref args, .. }if 1 <= index && index <= args.len() => Some(&args[index - 1]),
            _ => None,
        }
    }

    pub fn get_mut_source_operand(&mut self, index: usize) -> Option<&mut Operand> {
        use self::Opcode::*;
        match self {
            &mut Copy(_, _, ref mut operand) if index == 1 => Some(operand),
            &mut Load(_, _, ref mut operand) if index == 1 => Some(operand),
            &mut Store(_, _, ref mut operand) if index == 1 => Some(operand),
            &mut Eq(_, _, ref mut operand, _) if index == 1 => Some(operand),
            &mut Eq(_, _, _, ref mut operand) if index == 2 => Some(operand),
            &mut BinaryOp { ref mut src1, .. } if index == 1 => Some(src1),
            &mut BinaryOp { ref mut src2, .. } if index == 2 => Some(src2),
            &mut Call { ref mut args, .. } if 1 <= index && index <= args.len() => Some(&mut args[index - 1]),
            _ => None,
        }
    }

    pub fn set_source_operand(&mut self, index: usize, new_operand: Operand) {
        use self::Opcode::*;
        match self {
            &mut Copy(_, _, ref mut operand) if index == 1 => *operand = new_operand,
            &mut Load(_, _, ref mut operand) if index == 1 => *operand = new_operand,
            &mut Store(_, _, ref mut operand) if index == 1 => *operand = new_operand,
            &mut Eq(_, _, ref mut operand, _) if index == 1 => *operand = new_operand,
            &mut Eq(_, _, _, ref mut operand) if index == 2 => *operand = new_operand,
            &mut BinaryOp { ref mut src1, .. } if index == 1 => *src1 = new_operand,
            &mut BinaryOp { ref mut src2, .. } if index == 2 => *src2 = new_operand,
            &mut Call { ref mut args, .. } if 1 <= index && index <= args.len() => args[index - 1] = new_operand,
            _ => panic!(),
        }
    }

    pub fn get_source_operands(&self) -> Vec<&Operand> {
        use self::Opcode::*;
        match self {
            &Copy(_, _, ref operand) => vec![operand],
            &Load(_, _, ref operand) => vec![operand],
            &Store(_, _, ref operand) => vec![operand],
            &Eq(_, _, ref operand1, ref operand2) => vec![operand1, operand2],
            &BinaryOp { ref src1, ref src2, .. } => vec![src1, src2],
            &Call { ref args, .. } => args.iter().map(|arg| arg).collect(),
            _ => vec![],
        }
    }

    pub fn get_type(&self) -> Option<&Type> {
        use self::Opcode::*;
        match self {
            &Copy(ref typ, _, _) => Some(typ),
            &Load(ref typ, _, _) => Some(typ),
            &Store(ref typ, _, _) => Some(typ),
            &Eq(ref typ, _, _, _) => Some(typ),
            _ => None,
        }
    }

    pub fn get_destination_register_operand(&self) -> Option<&Operand> {
        use self::Opcode::*;
        match self {
            &Copy(_, ref dst, _) if dst.is_register() => Some(dst),
            &Load(_, ref dst, _) if dst.is_register() => Some(dst),
            &Store(_, ref dst, _) if dst.is_register() => Some(dst),
            &Eq(_, ref dst, _, _) if dst.is_register() => Some(dst),
            &BinaryOp { ref dst, .. } if dst.is_register() => Some(dst),
            &Call { result: Some(ref dst), .. } if dst.is_register() => Some(dst),
            _ => None,
        }
    }

    pub fn get_mut_destination_register_operand(&mut self) -> Option<&mut Operand> {
        use self::Opcode::*;
        match self {
            &mut Copy(_, ref mut dst, _) if dst.is_register() => Some(dst),
            &mut Load(_, ref mut dst, _) if dst.is_register() => Some(dst),
            &mut Store(_, ref mut dst, _) if dst.is_register() => Some(dst),
            &mut Eq(_, ref mut dst, _, _) if dst.is_register() => Some(dst),
            &mut BinaryOp { ref mut dst, .. } if dst.is_register() => Some(dst),
            _ => None,
        }
    }

    pub fn set_destination_operand(&mut self, new_operand: Operand) {
        use self::Opcode::*;
        match self {
            &mut Copy(_, ref mut operand, _) => *operand = new_operand,
            &mut Load(_, ref mut operand, _) => *operand = new_operand,
            &mut Store(_, ref mut operand, _) => *operand = new_operand,
            &mut Eq(_, ref mut operand, _, _) => *operand = new_operand,
            &mut BinaryOp { ref mut dst, .. } => *dst = new_operand,
            &mut Call { result: Some(ref mut dst), .. } => *dst = new_operand,
            _ => panic!(),
        }
    }

    pub fn get_destination_register(&self) -> Option<RegisterHandle> {
        use self::Opcode::*;
        match self {
            &Copy(_, ref dst, _) if dst.is_register() => Some(dst.get_as_register().unwrap()),
            &Load(_, ref dst, _) if dst.is_register() => Some(dst.get_as_register().unwrap()),
            &Store(_, ref dst, _) if dst.is_register() => Some(dst.get_as_register().unwrap()),
            &Eq(_, ref dst, _, _) if dst.is_register() => Some(dst.get_as_register().unwrap()),
            &BinaryOp { ref dst, .. } if dst.is_register() => Some(dst.get_as_register().unwrap()),
            _ => None,
        }
    }

    pub fn get_source_register_operands(&self) -> Vec<&Operand> {
        use self::Opcode::*;
        match self {
            &Copy(_, _, ref src) if src.is_register() => vec![src],
            &Load(_, _, ref src) if src.is_register() => vec![src],
            &Store(_, _, ref src) if src.is_register() => vec![src],
            &Eq(_, _, ref src1, ref src2) if src1.is_register() && src2.is_register() => vec![src1, src2],
            &Eq(_, _, ref src1, _) if src1.is_register() => vec![src1],
            &Eq(_, _, _, ref src2) if src2.is_register() => vec![src2],
            &BinaryOp { ref src1, ref src2, .. } if src1.is_register() && src2.is_register() => vec![src1, src2],
            &BinaryOp { ref src1, .. } if src1.is_register() => vec![src1],
            &BinaryOp { ref src2, .. } if src2.is_register() => vec![src2],
            _ => vec![],
        }
    }

    pub fn get_mut_source_register_operands(&mut self) -> Vec<&mut Operand> {
        use self::Opcode::*;
        match self {
            &mut Copy(_, _, ref mut src) if src.is_register() => vec![src],
            &mut Load(_, _, ref mut src) if src.is_register() => vec![src],
            &mut Store(_, _, ref mut src) if src.is_register() => vec![src],
            &mut Eq(_, _, ref mut src1, ref mut src2) if src1.is_register() && src2.is_register() => vec![src1, src2],
            &mut Eq(_, _, ref mut src1, _) if src1.is_register() => vec![src1],
            &mut Eq(_, _, _, ref mut src2) if src2.is_register() => vec![src2],
            &mut BinaryOp { ref mut src1, ref mut src2, .. } if src1.is_register() && src2.is_register() => vec![src1, src2],
            &mut BinaryOp { ref mut src1, .. } if src1.is_register() => vec![src1],
            &mut BinaryOp { ref mut src2, .. } if src2.is_register() => vec![src2],
            _ => vec![],
        }
    }

    pub fn get_source_registers(&self) -> Vec<RegisterHandle> {
        use self::Opcode::*;
        match self {
            &Copy(_, _, ref src) if src.is_register() => vec![src.get_as_register().unwrap()],
            &Load(_, _, ref src) if src.is_register() => vec![src.get_as_register().unwrap()],
            &Store(_, _, ref src) if src.is_register() => vec![src.get_as_register().unwrap()],
            &Eq(_, _, ref src1, ref src2) if src1.is_register() && src2.is_register() =>
                vec![src1.get_as_register().unwrap(), src2.get_as_register().unwrap()],
            &Eq(_, _, ref src1, _) if src1.is_register() => vec![src1.get_as_register().unwrap()],
            &Eq(_, _, _, ref src2) if src2.is_register() => vec![src2.get_as_register().unwrap()],
            &BinaryOp { ref src1, ref src2, .. } if src1.is_register() && src2.is_register() =>
                vec![src1.get_as_register().unwrap(), src2.get_as_register().unwrap()],
            &BinaryOp { ref src1, .. } if src1.is_register() => vec![src1.get_as_register().unwrap()],
            &BinaryOp { ref src2, .. } if src2.is_register() => vec![src2.get_as_register().unwrap()],
            _ => vec![],
        }
    }

    pub fn get_registers(&self) -> Vec<RegisterHandle> {
        let mut registers = vec![];
        if let Some(register) = self.get_destination_register() {
            registers.push(register);
        }
        registers.append(&mut self.get_source_registers());
        registers
    }

    pub fn is_copy_instr(&self) -> bool {
        use self::Opcode::*;
        match self {
            &Copy(_, _, _) => true,
            _ => false,
        }
    }

    pub fn print(&self) {
        use self::Opcode::*;
        match self {
            &Copy(_, ref dst, ref src) => {
                print!("copy  ");
                dst.print();
                print!(", ");
                src.print();
            }
            &Load(_, ref dst, ref src) => {
                print!("load  ");
                dst.print();
                print!(", ");
                src.print();
            }
            &Store(_, ref dst, ref src) => {
                print!("store  ");
                dst.print();
                print!(", ");
                src.print();
            }
            _ => unimplemented!(),
        }
    }
}

macro_rules! format {
    (0)=>("{:<8}");
    (1)=>("{:<8}{}");
    (2)=>("{:<8}{}, {}");
    (3)=>("{:<8}{}, {}, {}");
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Opcode::*;
        match self {
            &Debug(ref msg) => write!(f, format!(1), "debug", msg),
            &Label(ref label) => write!(f, format!(1), "label", label),
            &Copy(_, ref dst, ref src) => write!(f, format!(2), "copy", dst, src),
            &Load(_, ref dst, ref src) => write!(f, format!(2), "load", dst, src),
            &Store(_, ref dst, ref src) => write!(f, format!(2), "store", dst, src),
            &Eq(_, ref _dst, ref _src1, ref _src2) => unimplemented!(),
            &UnaryOp { .. } => unimplemented!(),
            &BinaryOp { .. } => unimplemented!(),
            &Jump { .. } => unimplemented!(),
            &Call { .. } => unimplemented!(),
            &Return { .. } => unimplemented!(),
        }
    }
}
