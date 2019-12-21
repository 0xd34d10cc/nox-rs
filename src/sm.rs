use crate::context::ExecutionContext;
use crate::ops::{LogicOp, Op};
use crate::expr::Expr;
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

type Program = Vec<Instruction>;
type Stack = Vec<Int>;

fn compile_expr(e: &Expr, program: &mut Program) {
  match e {
    Expr::Const(n) => program.push(Instruction::Const(*n)),
    Expr::Var(var) => program.push(Instruction::Load(var.clone())),
    Expr::Op(op, lhs, rhs) => {
      compile_expr(lhs, program);
      compile_expr(rhs, program);
      program.push(Instruction::Op(*op));
    },
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
      },
      Statement::Write(expr) => {
        compile_expr(expr, &mut program);
        program.push(Instruction::Write);
      },
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

    pub fn run(&mut self, program: Program) {
        todo!()
    }
}
