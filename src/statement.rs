use std::error::Error;

use crate::context::ExecutionContext;
use crate::expr::Expr;
use crate::types::Var;

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
            "Incomplete parse of statement {}: {}",
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
    // Statement ::= Skip | IfElse | While | Assign | Read | Write
    // Skip ::= 'skip'
    // IfElse ::= 'if' Expr 'then' Statements  ('elif' Expr 'then' Statements)* ['else' Statements] 'fi'
    // While ::= 'while' Expr 'do' Statements 'od'
    // Assign ::= Var '=' Expr
    // Read ::= 'read(' Var ')'
    // Write ::= 'write(' Expr ')'
    // Var ::= Char+

    use super::*;
    use crate::expr::parse::expr;
    use crate::types::parse::variable;

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::combinator::{map, opt};
    use nom::multi::{fold_many0, many0};
    use nom::sequence::tuple;
    use nom::IResult;

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
    }

    pub fn program(input: &[u8]) -> IResult<&[u8], Program> {
        statements(input)
    }

    fn statements(input: &[u8]) -> IResult<&[u8], Vec<Statement>> {
        let (input, _) = spaces(input)?;
        let (input, first) = statement(input)?;
        fold_many0(
            tuple((spaces, tag(";"), spaces, statement, spaces)),
            vec![first],
            |mut program, (_, _, _, statement, _)| {
                program.push(statement);
                program
            },
        )(input)
    }

    fn statement(input: &[u8]) -> IResult<&[u8], Statement> {
        alt((skip, while_, if_else, assign, read, write))(input)
    }

    fn skip(input: &[u8]) -> IResult<&[u8], Statement> {
        map(tag("skip"), |_| Statement::Skip)(input)
    }

    fn if_else(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, (_, root_condition, _, _, if_true)) =
            tuple((tag("if"), expr, spaces, tag("then"), statements))(input)?;

        let (input, mut elifs) = many0(elif)(input)?;
        let (input, else_) = opt(else_)(input)?;
        let (input, _) = tuple((spaces, tag("fi")))(input)?;

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
        let (input, (_, _, _, condition, _, _, body)) = tuple((
            spaces,
            tag("elif"),
            spaces,
            expr,
            spaces,
            tag("then"),
            statements,
        ))(input)?;
        Ok((input, (condition, body)))
    }

    fn else_(input: &[u8]) -> IResult<&[u8], Vec<Statement>> {
        let (input, (_, _, _, body)) = tuple((spaces, tag("else"), spaces, statements))(input)?;
        Ok((input, body))
    }

    fn while_(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, (_, condition, _, _, body, _, _)) = tuple((
            tag("while"),
            expr,
            spaces,
            tag("do"),
            statements,
            spaces,
            tag("od"),
        ))(input)?;
        Ok((input, Statement::While { condition, body }))
    }

    fn assign(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, (var, _, _, _, expr)) =
            tuple((variable, spaces, tag(":="), spaces, expr))(input)?;
        Ok((input, Statement::Assign(var, expr)))
    }

    fn read(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, (_, _, _, var, _)) =
            tuple((tag("read"), spaces, tag("("), variable, tag(")")))(input)?;
        Ok((input, Statement::Read(var)))
    }

    fn write(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, (_, _, _, e, _)) =
            tuple((tag("write"), spaces, tag("("), expr, tag(")")))(input)?;
        Ok((input, Statement::Write(e)))
    }
}
