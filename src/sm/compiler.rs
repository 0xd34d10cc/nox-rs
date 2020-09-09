use std::collections::HashMap;

use super::program::{Function, Instruction, Label, Program};
use crate::statement::{self, Expr};
use crate::syntax::Statement;
use crate::types::Var;

pub struct Compiler {
    labels: HashMap<Var, Label>,
    label: Label,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            label: Label(0),
            labels: HashMap::new(),
        }
    }

    pub fn compile(&mut self, source: &statement::Program) -> crate::types::Result<Program> {
        let mut target = Program::new();
        let main = source.entry().ok_or("No entry function")?;

        self.compile_function(source, main, &mut target);

        for function in source.functions() {
            self.compile_function(source, function, &mut target);
        }

        target.globals = source.globals().cloned().collect();
        Ok(target)
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
        self.label = Label(self.label.0 + 1);
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
            }
            Expr::Call(name, args) => {
                for arg in args {
                    self.compile_expr(arg, program);
                }
                let label = self.gen_named_label(name);
                program.push(Instruction::Call(label));
            }
        }
    }

    fn compile_statements(
        &mut self,
        source: &statement::Program,
        function_end_label: Label,
        statements: &[Statement],
        program: &mut Program,
    ) {
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
                    self.compile_statements(source, function_end_label, if_true, program);

                    if !if_false.is_empty() {
                        program.push(Instruction::Jump(end_label));

                        program.push(Instruction::Label(false_label));
                        self.compile_statements(source, function_end_label, if_false, program);
                    }

                    program.push(Instruction::Label(end_label));
                }
                Statement::While { condition, body } => {
                    let condition_label = self.gen_label();
                    program.push(Instruction::Jump(condition_label));

                    let body_label = self.gen_label();
                    program.push(Instruction::Label(body_label));
                    self.compile_statements(source, function_end_label, body, program);

                    program.push(Instruction::Label(condition_label));
                    self.compile_expr(condition, program);
                    program.push(Instruction::JumpIfNotZero(body_label));
                }
                Statement::DoWhile { body, condition } => {
                    let body_label = self.gen_label();
                    program.push(Instruction::Label(body_label));

                    self.compile_statements(source, function_end_label, body, program);
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

                    program.push(Instruction::Call(target));

                    if source
                        .get(name)
                        .expect("Call to unknown function")
                        .returns_value
                    {
                        program.push(Instruction::Ignore);
                    }
                }
                Statement::Return(e) => {
                    if let Some(e) = e {
                        self.compile_expr(e, program);
                    }
                    program.push(Instruction::Jump(function_end_label));
                }
            }
        }
    }

    fn compile_function(
        &mut self,
        source: &statement::Program,
        function: &statement::Function,
        program: &mut Program,
    ) {
        let statement::Function {
            name,
            returns_value,
            args,
            locals,
            body,
        } = function;
        let label = self.gen_named_label(name);
        let end_label = self.gen_label();

        program.functions.insert(
            name.clone(),
            Function {
                returns_value: *returns_value,
                entry: label,
            },
        );

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

        self.compile_statements(source, end_label, body, program);

        program.push(Instruction::Label(end_label));
        program.push(Instruction::End);
    }
}
