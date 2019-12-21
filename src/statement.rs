use std::error::Error;

use crate::context::ExecutionContext;
use crate::expr::Expr;
use crate::types::Var;

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Var, Expr),
    Read(Var),
    Write(Expr),
    Seq(Box<Statement>, Box<Statement>), // TODO: use vector instead of this
}

// parse statement
impl Statement {
    pub fn parse(input: &[u8]) -> Result<Statement, Box<dyn Error>> {
        let (rest, statement) = parse::statements(input).map_err(|e| {
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

        Ok(statement)
    }

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
          let value = context.read().ok_or_else(|| format!("Failed to read {}: no input", name))?;
          context.set(name, value);
        }
        Statement::Write(expr) => {
          let value = expr.eval(context)?;
          context.write(value);
        }
        Statement::Seq(first, second) => {
          first.eval(context)?;
          second.eval(context)?;
        }
      }

      Ok(())
    }
}

mod parse {
    // Statements ::= Statement (';' Statement)*
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

    pub fn statements(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, first) = statement(input)?;
        fold_many0(
            tuple((tag(";"), spaces, statement)),
            first,
            |acc, (_, _, statement)| Statement::Seq(Box::new(acc), Box::new(statement)),
        )(input)
    }

    fn statement(input: &[u8]) -> IResult<&[u8], Statement> {
        alt((assign, read, write))(input)
    }

    fn assign(input: &[u8]) -> IResult<&[u8], Statement> {
        let (rest, (var, _,  _, _, expr)) = tuple((variable, spaces, tag("="), spaces, expr))(input)?;
        Ok((rest, Statement::Assign(var, expr)))
    }

    fn read(input: &[u8]) -> IResult<&[u8], Statement> {
        let (rest, (_, var, _)) = tuple((tag("read("), variable, tag(")")))(input)?;
        Ok((rest, Statement::Read(var)))
    }

    fn write(input: &[u8]) -> IResult<&[u8], Statement> {
        let (rest, (_, e, _)) = tuple((tag("write("), expr, tag(")")))(input)?;
        Ok((rest, Statement::Write(e)))
    }
}
