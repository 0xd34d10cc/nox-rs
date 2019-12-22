use std::error::Error;

use crate::context::ExecutionContext;
use crate::expr::Expr;
use crate::ops::{LogicOp, Op};
use crate::statement::{self, Statement};
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

pub type Program = Vec<Instruction>;
type Stack = Vec<Int>;

fn compile_expr(e: &Expr, program: &mut Program) {
    match e {
        Expr::Const(n) => program.push(Instruction::Const(*n)),
        Expr::Var(var) => program.push(Instruction::Load(var.clone())),
        Expr::Op(op, lhs, rhs) => {
            compile_expr(lhs, program);
            compile_expr(rhs, program);
            program.push(Instruction::Op(*op));
        }
        Expr::LogicOp(op, lhs, rhs) => {
            compile_expr(lhs, program);
            compile_expr(rhs, program);
            program.push(Instruction::LogicOp(*op));
        }
    }
}

// convert from statement ast to list of stack machine instructions
pub fn compile(statements: &statement::Program) -> Program {
    let mut program = Program::new();

    for statement in statements {
        match statement {
            Statement::Read(var) => {
                program.push(Instruction::Read);
                program.push(Instruction::Store(var.clone()));
            }
            Statement::Write(expr) => {
                compile_expr(expr, &mut program);
                program.push(Instruction::Write);
            }
            Statement::Assign(var, expr) => {
                compile_expr(expr, &mut program);
                program.push(Instruction::Store(var.clone()));
            }
        }
    }

    program
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

    pub fn push(&mut self, value: Int) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Int> {
        self.stack.pop()
    }

    pub fn execute(&mut self, instruction: &Instruction) -> Result<(), Box<dyn Error>> {
        match instruction {
            Instruction::Op(op) => {
                let rhs = self.pop().ok_or("Empty stack (arithmetic, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (arithmetic, lhs)")?;
                let value = op.apply(lhs, rhs)?;
                self.push(value);
            }
            Instruction::LogicOp(op) => {
                let rhs = self.pop().ok_or("Empty stack (logic, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (logic, lhs)")?;
                let value = op.apply(lhs, rhs);
                self.push(Int::from(value));
            }
            Instruction::Const(n) => self.push(*n),
            Instruction::Read => {
                let value = self.context.read().ok_or("No input (read)")?;
                self.push(value);
            }
            Instruction::Write => {
                let value = self.pop().ok_or("Empty stack (write)")?;
                self.context.write(value);
            }
            Instruction::Load(var) => {
                let value = self
                    .context
                    .get(var)
                    .ok_or_else(|| format!("Variable {} is not defined", var))?;
                self.push(value);
            }
            Instruction::Store(var) => {
                let value = self.pop().ok_or("Empty stack (store)")?;
                self.context.set(var, value);
            }
        }
        Ok(())
    }

    pub fn run(&mut self, program: &Program) -> Result<(), Box<dyn Error>> {
        for instruction in program {
            self.execute(instruction)?;
        }

        Ok(())
    }
}
