use std::collections::HashMap;

use crate::context::{InputStream, Memory, OutputStream};
use crate::expr::Expr;
use crate::ops::{LogicOp, Op};
use crate::statement::{self, Function, Statement};
use crate::types::{Int, Result, Var};

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
    Call(Label),
    Begin { args: Vec<Var>, locals: Vec<Var> },
    End,
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
    labels: HashMap<Var, Label>,
    label: Label,
}

impl CompilationContext {
    fn new() -> Self {
        CompilationContext {
            label: 0 as Label,
            labels: HashMap::new(),
        }
    }

    fn gen_named_label(&mut self, name: &Var) -> Label {
        self.labels.get(name).copied().unwrap_or_else(|| {
            let label = self.gen_label();
            self.labels.insert(name.clone(), label);
            label
        })
    }

    // generate local (unnamed) label
    fn gen_label(&mut self) -> Label {
        self.label += 1;
        self.label
    }

    fn compile_expr(&mut self, e: &Expr, program: &mut Program) {
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
            },
            Expr::Call(name, args) => {
                for arg in args {
                    self.compile_expr(arg, program);
                }
                let label = self.gen_named_label(name);
                program.push(Instruction::Call(label));
            }
        }
    }

    fn compile_statements(&mut self, statements: &[Statement], program: &mut Program) {
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
                    self.compile_statements(if_true, program);

                    if !if_false.is_empty() {
                        program.push(Instruction::Jump(end_label));

                        program.push(Instruction::Label(false_label));
                        self.compile_statements(if_false, program);
                    }

                    program.push(Instruction::Label(end_label));
                }
                Statement::While { condition, body } => {
                    let condition_label = self.gen_label();
                    program.push(Instruction::Jump(condition_label));

                    let body_label = self.gen_label();
                    program.push(Instruction::Label(body_label));
                    self.compile_statements(body, program);

                    program.push(Instruction::Label(condition_label));
                    self.compile_expr(condition, program);
                    program.push(Instruction::JumpIfNotZero(body_label));
                }
                Statement::DoWhile { body, condition } => {
                    let body_label = self.gen_label();
                    program.push(Instruction::Label(body_label));

                    self.compile_statements(body, program);
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
                Statement::Call { name, args } => {
                    let target = self.gen_named_label(name);

                    for arg in args {
                        self.compile_expr(arg, program);
                    }

                    program.push(Instruction::Call(target))
                }
                Statement::Return(e) => {
                    if let Some(e) = e {
                        self.compile_expr(e, program);
                    }
                    program.push(Instruction::End);
                }
            }
        }
    }

    fn compile_function(&mut self, function: &Function, program: &mut Program) {
        let Function {
            name,
            args,
            locals,
            body,
        } = function;
        let label = self.gen_named_label(name);

        program.push(Instruction::Label(label));
        program.push(Instruction::Begin {
            args: args.clone(),
            locals: locals.clone(),
        });

        // args are passed through stack in reversed order
        // (because they are pushed in direct order)
        for arg in args.iter().rev() {
            program.push(Instruction::Store(arg.clone()));
        }

        self.compile_statements(body, program);
        program.push(Instruction::End);
    }
}

// convert from statement ast to list of stack machine instructions
pub fn compile(statements: &statement::Program) -> Result<Program> {
    let mut program = Program::new();
    let mut context = CompilationContext::new();
    let main = statements.entry()
        .ok_or("No main function found (sm::compile)")?;

    context.compile_function(main, &mut program);

    for function in statements.functions() {
        context.compile_function(function, &mut program);
    }

    Ok(program)
}

pub struct StackMachine<'a, I, O> {
    memory: &'a mut Memory,
    input: &'a mut I,
    output: &'a mut O,
    stack: Stack,
    control_stack: Vec<Label /* return address */>,
}

enum Retcode {
    Continue,
    Jump(Label),
    Return,
}

impl<I, O> StackMachine<'_, I, O>
where
    I: InputStream,
    O: OutputStream,
{
    pub fn new<'a>(
        memory: &'a mut Memory,
        input: &'a mut I,
        output: &'a mut O,
    ) -> StackMachine<'a, I, O> {
        StackMachine {
            memory,
            input,
            output,
            stack: Stack::new(),
            control_stack: Vec::new(),
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
        pc: Label,
    ) -> Result<Retcode> {
        match instruction {
            Instruction::Label(_) => { /* ignore */ }
            Instruction::Jump(label) => {
                let location = labels.get(label).ok_or("Invalid label (jump)")?;
                return Ok(Retcode::Jump(*location));
            }
            Instruction::JumpIfZero(label) => {
                let v = self.pop().ok_or("Empty stack (jz)")?;
                if v == 0 {
                    let location = labels.get(label).ok_or("Invalid label (jz)")?;
                    return Ok(Retcode::Jump(*location));
                }
            }
            Instruction::JumpIfNotZero(label) => {
                let v = self.pop().ok_or("Empty stack (jnz)")?;
                if v != 0 {
                    let location = labels.get(label).ok_or("Invalid label (jnz)")?;
                    return Ok(Retcode::Jump(*location));
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
                let value = self.input.read().ok_or("No input (read)")?;
                self.push(value);
            }
            Instruction::Write => {
                let value = self.pop().ok_or("Empty stack (write)")?;
                self.output.write(value);
            }
            Instruction::Load(var) => {
                let value = self
                    .memory
                    .load(var)
                    .ok_or_else(|| format!("Variable {} is not defined", var))?;
                self.push(value);
            }
            Instruction::Store(var) => {
                let value = self.pop().ok_or("Empty stack (store)")?;
                self.memory.store(var, value);
            }
            Instruction::Call(label) => {
                self.control_stack.push(pc + 1);
                let location = labels.get(label).ok_or("Invalid label (call)")?;
                return Ok(Retcode::Jump(*location));
            }
            Instruction::Begin { args, locals } => {
                let local_names = args.iter().chain(locals.iter()).cloned().collect();
                self.memory.push_scope(local_names);
            }
            Instruction::End => {
                self.memory.pop_scope();
                match self.control_stack.pop() {
                    Some(location) => return Ok(Retcode::Jump(location)),
                    None => return Ok(Retcode::Return),
                }
            }
        };

        Ok(Retcode::Continue)
    }

    pub fn run(&mut self, program: &Program) -> Result<()> {
        let mut pc = 0;
        while pc < program.instructions.len() {
            match self.execute(&program.instructions[pc], &program.labels, pc)? {
                Retcode::Continue => pc += 1,
                Retcode::Jump(location) => pc = location,
                Retcode::Return => return Ok(()),
            };
        }

        Ok(())
    }
}
