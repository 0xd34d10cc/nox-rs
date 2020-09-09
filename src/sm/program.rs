use crate::ops::{LogicOp, Op};
use crate::types::{Int, Var};

// NOTE: the inner value here is _not_ an index
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Label(pub usize);
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

#[derive(Debug)]
pub struct Function {
    pub returns_value: bool,
    pub entry: Label,
}

#[derive(Debug)]
pub struct Program {
    labels: Labels,
    instructions: Vec<Instruction>,
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

    pub fn labels(&self) -> &Labels {
        &self.labels
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn globals(&self) -> &Globals {
        &self.globals
    }

    // TODO: use in JIT
    #[allow(unused)]
    pub fn functions(&self) -> &Functions {
        &self.functions
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
}
