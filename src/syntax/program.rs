use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list};
use nom::sequence::{delimited, preceded};

use super::statement::Statement as PascalStatement;
use super::{key, variable, Input, Parsed};
use crate::ops::LogicOp;
use crate::statement::Expr;
use crate::types::Var;

#[derive(Debug, Clone)]
pub enum Statement {
    Skip,
    IfElse {
        condition: Expr,
        if_true: Vec<Statement>,
        if_false: Vec<Statement>,
    },
    While {
        condition: Expr,
        body: Vec<Statement>,
    },
    DoWhile {
        body: Vec<Statement>,
        condition: Expr,
    },
    Assign(Var, Expr),
    Read(Var),
    Write(Expr),
    Call {
        name: Var,
        args: Vec<Expr>,
    },
    Return(Option<Expr>),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Var,
    pub args: Vec<Var>,
    pub locals: Vec<Var>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub entry: Var, /* name of "main" function */
}

impl Program {
    #[cfg(test)]
    pub fn from_main(statements: Vec<Statement>) -> Self {
        Program {
            functions: vec![Function {
                name: "main".into(),
                args: Vec::new(),
                locals: Vec::new(),
                body: statements,
            }],
            entry: "main".into(),
        }
    }

    #[cfg(test)]
    pub fn parse(input: Input) -> super::Result<Program> {
        crate::syntax::parse("program", program, input)
    }

    pub fn entry(&self) -> Option<&Function> {
        self.get(&self.entry)
    }

    pub fn get(&self, function: &Var) -> Option<&Function> {
        self.functions.iter().find(|f| &f.name == function)
    }
}

fn convert(statements: Vec<PascalStatement>) -> Vec<Statement> {
    let mut results = Vec::with_capacity(statements.len());
    for statement in statements {
        convert_into(statement, &mut results);
    }
    results
}

fn convert_into(statement: PascalStatement, program: &mut Vec<Statement>) {
    match statement {
        PascalStatement::Skip => program.push(Statement::Skip),
        PascalStatement::IfElse {
            condition: root_condition,
            if_true,
            mut elifs,
            if_false,
        } => {
            // build if-else chain
            let if_else = if let Some((condition, body)) = elifs.pop() {
                let mut root = Statement::IfElse {
                    condition,
                    if_true: convert(body),
                    if_false: convert(if_false),
                };

                for (condition, body) in elifs.into_iter().rev() {
                    root = Statement::IfElse {
                        condition,
                        if_true: convert(body),
                        if_false: vec![root],
                    };
                }

                Statement::IfElse {
                    condition: root_condition,
                    if_true: convert(if_true),
                    if_false: vec![root],
                }
            } else {
                // simple case
                Statement::IfElse {
                    condition: root_condition,
                    if_true: convert(if_true),
                    if_false: convert(if_false),
                }
            };

            program.push(if_else)
        }
        PascalStatement::While { condition, body } => program.push(Statement::While {
            condition,
            body: convert(body),
        }),
        PascalStatement::For {
            init,
            condition,
            post_step,
            mut body,
        } => {
            convert_into(*init, program);
            body.push(*post_step);
            program.push(Statement::While {
                condition,
                body: convert(body),
            })
        }
        PascalStatement::RepeatUntil { body, condition } => program.push(Statement::DoWhile {
            body: convert(body),
            // until == while condition is false
            condition: Expr::LogicOp(LogicOp::Eq, Box::new(condition), Box::new(Expr::Const(0))),
        }),
        PascalStatement::Assign(to, from) => program.push(Statement::Assign(to, from)),
        PascalStatement::Read(into) => program.push(Statement::Read(into)),
        PascalStatement::Write(e) => program.push(Statement::Write(e)),
        PascalStatement::Call { name, args } => program.push(Statement::Call { name, args }),
        PascalStatement::Return(e) => program.push(Statement::Return(e)),
    }
}

pub fn statements1(input: Input) -> Parsed<Vec<Statement>> {
    map(super::statement::statements1, convert)(input)
}

pub fn statements(input: Input) -> Parsed<Vec<Statement>> {
    map(super::statement::statements, convert)(input)
}

// Program ::= (Statements | Definition)*
//
// Definition ::= Function
// Function ::= 'fun' Var '(' Var* ')' ['locals' Var*] '{' Statements '}'
//
pub fn program(input: Input) -> Parsed<Program> {
    let (input, mut functions) = many0(function)(input)?;
    let (input, main) = statements1(input)?;

    let entry: Var = Var::from("main");
    functions.push(Function {
        name: entry.clone(),
        args: Vec::new(),
        locals: Vec::new(),
        body: main,
    });

    Ok((input, Program { functions, entry }))
}

fn function(input: Input) -> Parsed<Function> {
    let (input, name) = preceded(key("fun "), variable)(input)?;
    let (input, args) = delimited(key("("), separated_list(key(","), variable), key(")"))(input)?;
    let (input, locals) = opt(preceded(key("local"), separated_list(key(","), variable)))(input)?;
    let (input, body) = delimited(key("{"), statements, key("}"))(input)?;
    Ok((
        input,
        Function {
            name,
            args,
            locals: locals.unwrap_or_default(),
            body,
        },
    ))
}
