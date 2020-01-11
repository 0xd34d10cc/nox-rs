use std::collections::{HashMap, HashSet};

use crate::ops::{LogicOp, Op};
use crate::types::{Int, Var};

pub type Label = usize;
pub type Labels = HashMap<Label, usize>; // Label -> instruction index

// Stack machine instruction
#[derive(Debug, Clone)]
pub enum Instruction {
    Label(Label),
    Jump(Label),
    JumpIfZero(Label),
    JumpIfNotZero(Label),
    Op(Op),
    LogicOp(LogicOp),
    Const(Int),
    Read,
    Write,
    Load(Var),
    Store(Var),
    Call(Label),
    Begin { args: Vec<Var>, locals: Vec<Var> },
    End,
}

pub struct Program {
    pub labels: Labels,
    pub instructions: Vec<Instruction>,

    pub(super) globals: HashSet<Var>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            labels: Labels::new(),
            instructions: Vec::new(),
            globals: HashSet::new(),
        }
    }

    pub fn instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }

    pub fn push(&mut self, instruction: Instruction) {
        let label = match &instruction {
            Instruction::Label(label) => Some(*label),
            _ => None,
        };

        self.instructions.push(instruction);

        if let Some(label) = label {
            debug_assert_eq!(self.labels.get(&label), None);
            self.labels.insert(label, self.instructions.len() - 1);
        }
    }

    pub fn globals(&self) -> impl Iterator<Item=&Var> {
        self.globals.iter()
    }
}
