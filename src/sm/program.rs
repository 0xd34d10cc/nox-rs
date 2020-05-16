use crate::ops::{LogicOp, Op};
use crate::types::{Int, Var};

pub type Label = usize;
pub type Labels = fnv::FnvHashMap<Label, usize>; // Label -> instruction index
pub type Functions = fnv::FnvHashMap<Var, Function>;
pub type Globals = fnv::FnvHashSet<Var>;

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
    Ignore,
}

// TODO: use in JIT
#[allow(unused)]
pub struct Function {
    pub(in crate::sm) args: Vec<Var>,
    pub(in crate::sm) locals: Vec<Var>,
    pub(in crate::sm) returns_value: bool,
    pub(in crate::sm) entry: Label,
}

pub struct Program {
    pub(in crate::sm) labels: Labels,
    pub(in crate::sm) instructions: Vec<Instruction>,
    pub(in crate::sm) functions: Functions,
    pub(in crate::sm) globals: Globals,
}

impl Program {
    pub fn new() -> Self {
        Program {
            labels: Labels::default(),
            instructions: Vec::new(),
            functions: Functions::default(),
            globals: Globals::default(),
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

    pub fn globals(&self) -> impl Iterator<Item = &Var> {
        self.globals.iter()
    }
}
