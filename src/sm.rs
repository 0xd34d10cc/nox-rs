use crate::context::ExecutionContext;
use crate::ops::{LogicOp, Op};
use crate::statement;
use crate::types::{Int, Var};

// Stack machine instruction
#[derive(Debug, Clone)]
pub enum Instruction {
    Op(Op),
    LogicOp(LogicOp),
    Const(Int),
    Read,
    Write,
    Load(Var),
    Store(Var),
}

type Program = Vec<Instruction>;
type Stack = Vec<Int>;

// convert from statement ast to list of stack machine instructions
fn compile(statements: statement::Program) -> Program {
    todo!()
}

pub struct StackMachine<C> {
    context: C,
    stack: Stack,
}

impl<C> StackMachine<C>
where
    C: ExecutionContext,
{
    pub fn new(context: C) -> Self {
        StackMachine {
            context,
            stack: Stack::new(),
        }
    }

    pub fn run(&mut self, program: Program) {
        todo!()
    }
}
