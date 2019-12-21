use std::error::Error;

#[cfg(test)]
use crate::context::ExecutionContext;
use crate::expr::Expr;
use crate::types::Var;

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Var, Expr),
    Read(Var),
    Write(Expr),
}

impl Statement {
    #[cfg(test)]
    pub fn eval<C>(&self, context: &mut C) -> Result<(), Box<dyn Error>>
    where
        C: ExecutionContext,
    {
        match self {
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

#[cfg(test)]
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
    // Program ::= Statement (';' Statement)*
    // Statement ::= Assign | Read | Write
    // Assign ::= Var '=' Expr
    // Read ::= 'read(' Var ')'
    // Write ::= 'write(' Expr ')'
    // Var ::= Char+

    use super::*;
    use crate::expr::parse::expr;
    use crate::types::parse::variable;

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::multi::fold_many0;
    use nom::sequence::tuple;
    use nom::IResult;

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
    }

    pub fn program(input: &[u8]) -> IResult<&[u8], Program> {
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
        alt((assign, read, write))(input)
    }

    fn assign(input: &[u8]) -> IResult<&[u8], Statement> {
        let (rest, (var, _, _, _, expr)) =
            tuple((variable, spaces, tag(":="), spaces, expr))(input)?;
        Ok((rest, Statement::Assign(var, expr)))
    }

    fn read(input: &[u8]) -> IResult<&[u8], Statement> {
        let (rest, (_, _, _, var, _)) =
            tuple((tag("read"), spaces, tag("("), variable, tag(")")))(input)?;
        Ok((rest, Statement::Read(var)))
    }

    fn write(input: &[u8]) -> IResult<&[u8], Statement> {
        let (rest, (_, _, _, e, _)) =
            tuple((tag("write"), spaces, tag("("), expr, tag(")")))(input)?;
        Ok((rest, Statement::Write(e)))
    }
}
