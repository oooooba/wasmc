use std::collections::VecDeque;
use std::fmt;

use context::handle::{BasicBlockHandle, InstrHandle, RegisterHandle};

#[derive(PartialEq, Eq, Hash)]
pub enum BasicBlockKind {
    ExprBlock(BasicBlockHandle),
    ContinuationBlock(Vec<RegisterHandle>),
}

impl fmt::Debug for BasicBlockKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::BasicBlockKind::*;
        match self {
            &ExprBlock(ref continuation) => write!(f, "ExprBlock({:?})", continuation),
            &ContinuationBlock(ref registers) => {
                write!(f, "ContinuationBlock([")?;
                let mut is_first = true;
                for register in registers.iter() {
                    if is_first {
                        is_first = false
                    } else {
                        write!(f, " ,")?;
                    }
                    write!(f, "{:?}", register)?;
                }
                write!(f, "])")
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct BasicBlock {
    handle: BasicBlockHandle,
    instrs: VecDeque<InstrHandle>,
    kind: BasicBlockKind,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "BasicBlock[{}]", self.handle)?;
        writeln!(f, "  label_{}:", self.handle)?;
        for instr in self.instrs.iter() {
            writeln!(f, "    {}: {}", instr, instr.get())?;
        }
        Ok(())
    }
}

impl BasicBlock {
    pub fn new(handle: BasicBlockHandle, kind: BasicBlockKind) -> BasicBlock {
        BasicBlock {
            handle: handle,
            instrs: VecDeque::new(),
            kind: kind,
        }
    }

    pub fn get_handle(&self) -> &BasicBlockHandle {
        &self.handle
    }

    pub fn get_instrs(&self) -> &VecDeque<InstrHandle> {
        &self.instrs
    }

    pub fn get_mut_instrs(&mut self) -> &mut VecDeque<InstrHandle> {
        &mut self.instrs
    }

    pub fn get_kind(&self) -> &BasicBlockKind {
        &self.kind
    }

    pub fn get_continuation_block(&self) -> Option<&BasicBlockHandle> {
        match self.get_kind() {
            &BasicBlockKind::ExprBlock(ref continuation) => Some(continuation),
            _ => None,
        }
    }

    pub fn get_result_registers(&self) -> Option<&Vec<RegisterHandle>> {
        match self.get_kind() {
            &BasicBlockKind::ContinuationBlock(ref registers) => Some(registers),
            _ => None,
        }
    }

    pub fn add_instr(&mut self, instr: InstrHandle) {
        self.instrs.push_back(instr)
    }

    pub fn print(&self) {
        println!("BasicBlock ({:?})", self.handle);
        for instr in self.instrs.iter() {
            print!("    ");
            instr.print();
            print!(" @ ");
            instr.get().print();
            println!();
        }
    }
}
