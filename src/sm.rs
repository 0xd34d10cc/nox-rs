use std::collections::HashMap;
use std::error::Error;

use crate::context::ExecutionContext;
use crate::expr::Expr;
use crate::ops::{LogicOp, Op};
use crate::statement::{self, Statement};
use crate::types::{Int, Var};

pub type Label = usize;
type Labels = HashMap<Label, usize>; // Label -> instruction index

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
}

pub struct Program {
    labels: Labels,
    instructions: Vec<Instruction>,
}

impl Program {
    fn new() -> Self {
        Program {
            labels: HashMap::new(),
            instructions: Vec::new(),
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
}

type Stack = Vec<Int>;

struct CompilationContext {
    label: Label,
}

impl CompilationContext {
    fn new() -> Self {
        CompilationContext { label: 0 as Label }
    }

    fn gen_label(&mut self) -> Label {
        self.label += 1;
        self.label
    }

    fn compile_expr(&self, e: &Expr, program: &mut Program) {
        match e {
            Expr::Const(n) => program.push(Instruction::Const(*n)),
            Expr::Var(var) => program.push(Instruction::Load(var.clone())),
            Expr::Op(op, lhs, rhs) => {
                self.compile_expr(lhs, program);
                self.compile_expr(rhs, program);
                program.push(Instruction::Op(*op));
            }
            Expr::LogicOp(op, lhs, rhs) => {
                self.compile_expr(lhs, program);
                self.compile_expr(rhs, program);
                program.push(Instruction::LogicOp(*op));
            }
        }
    }

    fn compile_into(&mut self, statements: &statement::Program, program: &mut Program) {
        for statement in statements {
            match statement {
                Statement::Skip => { /* do nothing, successfully */ }
                Statement::IfElse {
                    condition,
                    if_true,
                    if_false,
                } => {
                    let has_false_branch = !if_false.is_empty();
                    let end_label = self.gen_label();
                    let false_label = if has_false_branch {
                        self.gen_label()
                    } else {
                        end_label
                    };

                    self.compile_expr(condition, program);
                    program.push(Instruction::JumpIfZero(false_label));
                    self.compile_into(if_true, program);

                    if !if_false.is_empty() {
                        program.push(Instruction::Jump(end_label));

                        program.push(Instruction::Label(false_label));
                        self.compile_into(if_false, program);
                    }

                    program.push(Instruction::Label(end_label));
                }
                Statement::While { condition, body } => {
                    let condition_label = self.gen_label();
                    program.push(Instruction::Jump(condition_label));

                    let body_label = self.gen_label();
                    program.push(Instruction::Label(body_label));
                    self.compile_into(body, program);

                    program.push(Instruction::Label(condition_label));
                    self.compile_expr(condition, program);
                    program.push(Instruction::JumpIfNotZero(body_label));
                }
                Statement::DoWhile { body, condition } => {
                    let body_label = self.gen_label();
                    program.push(Instruction::Label(body_label));

                    self.compile_into(body, program);
                    self.compile_expr(condition, program);
                    program.push(Instruction::JumpIfNotZero(body_label));
                }
                Statement::Read(var) => {
                    program.push(Instruction::Read);
                    program.push(Instruction::Store(var.clone()));
                }
                Statement::Write(expr) => {
                    self.compile_expr(expr, program);
                    program.push(Instruction::Write);
                }
                Statement::Assign(var, expr) => {
                    self.compile_expr(expr, program);
                    program.push(Instruction::Store(var.clone()));
                }
            }
        }
    }
}

// convert from statement ast to list of stack machine instructions
pub fn compile(statements: &statement::Program) -> Program {
    let mut program = Program::new();
    CompilationContext::new().compile_into(statements, &mut program);
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

    fn execute(
        &mut self,
        instruction: &Instruction,
        labels: &Labels,
    ) -> Result<Option<usize>, Box<dyn Error>> {
        match instruction {
            Instruction::Label(_) => { /* ignore */ }
            Instruction::Jump(label) => {
                let location = labels.get(label).ok_or("Invalid label (jump)")?;
                return Ok(Some(*location));
            }
            Instruction::JumpIfZero(label) => {
                let v = self.pop().ok_or("Empty stack (jz)")?;
                if v == 0 {
                    let location = labels.get(label).ok_or("Invalid label (jz)")?;
                    return Ok(Some(*location));
                }
            }
            Instruction::JumpIfNotZero(label) => {
                let v = self.pop().ok_or("Empty stack (jnz)")?;
                if v != 0 {
                    let location = labels.get(label).ok_or("Invalid label (jnz)")?;
                    return Ok(Some(*location));
                }
            }
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
        };

        Ok(None)
    }

    pub fn run(&mut self, program: &Program) -> Result<(), Box<dyn Error>> {
        let mut pc = 0;
        while pc < program.instructions.len() {
            if let Some(location) = self.execute(&program.instructions[pc], &program.labels)? {
                pc = location;
            } else {
                pc += 1;
            }
        }

        Ok(())
    }
}
