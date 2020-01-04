use std::error::Error;

use crate::context::ExecutionContext;
use crate::expr::Expr;
use crate::types::Var;

// abstract statement
#[derive(Debug, Clone)]
pub enum Statement {
    Skip,
    IfElse {
        condition: Expr,
        if_true: Vec<Statement>,
        if_false: Vec<Statement>, // <- all elif's are here
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
}

impl Statement {
    pub fn eval<C>(&self, context: &mut C) -> Result<(), Box<dyn Error>>
    where
        C: ExecutionContext,
    {
        match self {
            Statement::Skip => { /* do nothing */ }
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let c = condition.eval(context)?;
                if c != 0 {
                    run(if_true, context)?;
                } else {
                    run(if_false, context)?;
                }
            }
            Statement::While { condition, body } => {
                while condition.eval(context)? != 0 {
                    run(body, context)?;
                }
            }
            Statement::DoWhile { body, condition } => loop {
                run(body, context)?;
                if condition.eval(context)? == 0 {
                    break;
                }
            },
            Statement::Assign(name, value) => {
                let value = value.eval(context)?;
                context.set(name, value);
            }
            Statement::Read(name) => {
                let value = context
                    .read()
                    .ok_or_else(|| format!("Failed to read {}: no input", name))?;
                context.set(name, value);
            }
            Statement::Write(expr) => {
                let value = expr.eval(context)?;
                context.write(value);
            }
        };

        Ok(())
    }
}

pub type Program = Vec<Statement>;

pub fn parse(input: &[u8]) -> Result<Program, Box<dyn Error>> {
    let (rest, program) = parse::program(input).map_err(|e| {
        format!(
            "Failed to parse statement {}: {:?}",
            String::from_utf8_lossy(input),
            e
        )
    })?;

    if !rest.is_empty() {
        return Err(format!(
            "Incomplete parse of statement\nProgram: {:?}\nOriginal: \n{}\nRest: \n{}",
            program,
            String::from_utf8_lossy(input),
            String::from_utf8_lossy(rest)
        )
        .into());
    }

    Ok(program)
}

pub fn run<C>(program: &Program, context: &mut C) -> Result<(), Box<dyn Error>>
where
    C: ExecutionContext,
{
    for statement in program {
        statement.eval(context)?;
    }

    Ok(())
}

pub mod parse {
    // Program ::= Statements
    // Statements ::= Statement (';' Statement)*
    // Statement ::= Skip | IfElse | While | For | Assign | Read | Write
    // Skip ::= 'skip'
    // IfElse ::= 'if' Expr 'then' Statements  ('elif' Expr 'then' Statements)* ['else' Statements] 'fi'
    // While ::= 'while' Expr 'do' Statements 'od'
    // For ::= 'for' Statement ',' Expr ',' Statement 'do' Statements 'od'
    // RepeatUntil ::= 'repeat' Statements 'until' Expr
    // Assign ::= Var '=' Expr
    // Read ::= 'read(' Var ')'
    // Write ::= 'write(' Expr ')'
    // Var ::= Char+

    use super::{Expr, Program, Var};
    use crate::expr::parse::expr;
    use crate::ops::LogicOp;
    use crate::types::parse::variable;

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::combinator::{map, opt};
    use nom::multi::{fold_many0, many0};
    use nom::sequence::{delimited, preceded};
    use nom::IResult;

    // local statement enum, for concrete syntax only
    #[derive(Debug, Clone)]
    enum Statement {
        Skip,
        IfElse {
            condition: Expr,
            if_true: Vec<Statement>,
            if_false: Vec<Statement>, // <- all elif's are here
        },
        While {
            condition: Expr,
            body: Vec<Statement>,
        },
        For {
            init: Box<Statement>,
            condition: Expr,
            post_step: Box<Statement>,
            body: Vec<Statement>,
        },
        RepeatUntil {
            body: Vec<Statement>,
            condition: Expr,
        },
        Assign(Var, Expr),
        Read(Var),
        Write(Expr),
    }

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
    }

    fn convert(statements: Vec<Statement>) -> Vec<super::Statement> {
        let mut results = Vec::with_capacity(statements.len());
        for statement in statements {
            convert_into(statement, &mut results);
        }
        results
    }

    fn convert_into(statement: Statement, program: &mut Vec<super::Statement>) {
        match statement {
            Statement::Skip => program.push(super::Statement::Skip),
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => program.push(super::Statement::IfElse {
                condition,
                if_true: convert(if_true),
                if_false: convert(if_false),
            }),
            Statement::While { condition, body } => program.push(super::Statement::While {
                condition,
                body: convert(body),
            }),
            Statement::For {
                init,
                condition,
                post_step,
                mut body,
            } => {
                convert_into(*init, program);
                body.push(*post_step);
                program.push(super::Statement::While {
                    condition,
                    body: convert(body),
                })
            }
            Statement::RepeatUntil { body, condition } => program.push(super::Statement::DoWhile {
                body: convert(body),
                // until == while condition is false
                condition: Expr::LogicOp(
                    LogicOp::Eq,
                    Box::new(condition),
                    Box::new(Expr::Const(0)),
                ),
            }),
            Statement::Assign(to, from) => program.push(super::Statement::Assign(to, from)),
            Statement::Read(into) => program.push(super::Statement::Read(into)),
            Statement::Write(e) => program.push(super::Statement::Write(e)),
        }
    }

    pub fn program(input: &[u8]) -> IResult<&[u8], Program> {
        map(statements, |statements| convert(statements))(input)
    }

    fn statements(input: &[u8]) -> IResult<&[u8], Vec<Statement>> {
        let (input, first) = statement(input)?;
        fold_many0(
            preceded(key(";"), statement),
            vec![first],
            |mut program, statement| {
                program.push(statement);
                program
            },
        )(input)
    }

    fn statement(input: &[u8]) -> IResult<&[u8], Statement> {
        preceded(
            spaces,
            alt((
                skip,
                while_,
                for_,
                repeat_until,
                if_else,
                assign,
                read,
                write,
            )),
        )(input)
    }

    fn key<'a>(key: &'a str) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], &'a [u8]> {
        preceded(spaces, tag(key))
    }

    fn skip(input: &[u8]) -> IResult<&[u8], Statement> {
        map(tag("skip"), |_| Statement::Skip)(input)
    }

    fn while_(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, condition) = preceded(key("while"), expr)(input)?;
        let (input, body) = delimited(key("do"), statements, key("od"))(input)?;
        Ok((input, Statement::While { condition, body }))
    }

    fn for_(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, init) = preceded(key("for"), statement)(input)?;
        let (input, condition) = preceded(key(","), expr)(input)?;
        let (input, post_step) = preceded(key(","), statement)(input)?;
        let (input, body) = delimited(key("do"), statements, key("od"))(input)?;
        Ok((
            input,
            Statement::For {
                init: Box::new(init),
                condition,
                post_step: Box::new(post_step),
                body,
            },
        ))
    }

    fn repeat_until(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, body) = preceded(key("repeat"), statements)(input)?;
        let (input, condition) = preceded(key("until"), expr)(input)?;
        Ok((input, Statement::RepeatUntil { body, condition }))
    }

    fn if_else(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, root_condition) = preceded(key("if"), expr)(input)?;
        let (input, if_true) = preceded(key("then"), statements)(input)?;
        let (input, mut elifs) = many0(elif)(input)?;
        let (input, else_) = opt(else_)(input)?;
        let (input, _) = key("fi")(input)?;

        // build if-else chain
        let if_else = if let Some((condition, body)) = elifs.pop() {
            let mut root = Statement::IfElse {
                condition: condition,
                if_true: body,
                if_false: else_.unwrap_or_default(),
            };

            for (condition, body) in elifs.into_iter().rev() {
                root = Statement::IfElse {
                    condition,
                    if_true: body,
                    if_false: vec![root],
                };
            }

            Statement::IfElse {
                condition: root_condition,
                if_true: if_true,
                if_false: vec![root],
            }
        } else {
            // simple case
            Statement::IfElse {
                condition: root_condition,
                if_true,
                if_false: else_.unwrap_or_default(),
            }
        };

        Ok((input, if_else))
    }

    fn elif(input: &[u8]) -> IResult<&[u8], (Expr, Vec<Statement>)> {
        let (input, condition) = preceded(key("elif"), expr)(input)?;
        let (input, body) = preceded(key("then"), statements)(input)?;
        Ok((input, (condition, body)))
    }

    fn else_(input: &[u8]) -> IResult<&[u8], Vec<Statement>> {
        preceded(key("else"), statements)(input)
    }

    fn assign(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, var) = variable(input)?;
        let (input, e) = preceded(key(":="), expr)(input)?;
        Ok((input, Statement::Assign(var, e)))
    }

    fn read(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, _) = key("read")(input)?;
        let (input, var) = delimited(key("("), variable, key(")"))(input)?;
        Ok((input, Statement::Read(var)))
    }

    fn write(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, _) = key("write")(input)?;
        let (input, e) = delimited(key("("), expr, key(")"))(input)?;
        Ok((input, Statement::Write(e)))
    }
}
