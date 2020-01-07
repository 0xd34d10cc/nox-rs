use crate::context::Memory;
use crate::ops::{LogicOp, Op};
use crate::types::{Int, Result, Var};

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    Const(Int),
    Op(Op, Box<Expr>, Box<Expr>),
    LogicOp(LogicOp, Box<Expr>, Box<Expr>),
}

impl Expr {
    #[cfg(test)]
    pub fn parse(input: &[u8]) -> Result<Expr> {
        let (rest, e) = self::parse::expr(input).map_err(|e| {
            // TODO: find out how to pretty print nom errors
            format!(
                "Parsing of {} failed: {:?}",
                String::from_utf8_lossy(input),
                e
            )
        })?;

        if !rest.is_empty() {
            return Err(format!(
                "Incomplete parse of expression {}: {}",
                String::from_utf8_lossy(input),
                String::from_utf8_lossy(rest)
            )
            .into());
        }

        Ok(e)
    }

    pub fn eval(&self, memory: &Memory) -> Result<Int> {
        match self {
            Expr::Var(name) => {
                let val = memory
                    .load(name)
                    .ok_or_else(|| format!("Variable {} is not defined", name))?;
                Ok(val)
            }
            Expr::Const(v) => Ok(*v),
            Expr::Op(op, lhs, rhs) => {
                let left = lhs.eval(memory)?;
                let right = rhs.eval(memory)?;
                let v = op.apply(left, right)?;
                Ok(v)
            }
            Expr::LogicOp(op, lhs, rhs) => {
                let left = lhs.eval(memory)?;
                let right = rhs.eval(memory)?;
                let v = op.apply(left, right);
                Ok(Int::from(v))
            }
        }
    }
}

pub mod parse {
    // Expr ::= Disj
    // Disj ::= Conj ('!!' Conj)*
    // Conj ::= Comp ('&&' Comp)*
    // Comp ::= Arithm ( '<'  Arithm
    //                 | '<=' Arithm
    //                 | '>'  Arithm
    //                 | '>=' Arithm
    //                 | '==' Arithm
    //                 | '!=' Arithm)*
    //
    // Arithm ::= Term ('+' Term | '-' Term)*
    // Term ::= Factor ('*' Factor | '/' Factor | '%' Factor)*
    // Factor ::= ['-'] (Var | Number | '(' Expr ')')
    //
    // Number ::= Digit+
    // Var ::= Alpha (Alpha | Digit)*

    use super::*;
    use crate::types::parse::{integer, variable};

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::combinator::{map, opt};
    use nom::multi::fold_many0;
    use nom::sequence::{preceded, tuple};
    use nom::IResult;

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
    }

    fn factor(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, minus) = opt(tag(b"-"))(input)?;
        let (input, node) = alt((
            map(variable, Expr::Var),
            map(integer, Expr::Const),
            map(tuple((tag("("), expr, tag(")"))), |(_, e, _)| e),
        ))(input)?;

        let node = if minus.is_some() {
            Expr::Op(Op::Sub, Box::new(Expr::Const(0)), Box::new(node))
        } else {
            node
        };

        Ok((input, node))
    }

    fn mul_div_or_mod(input: &[u8]) -> IResult<&[u8], Op> {
        match alt((tag("*"), tag("/"), tag("%")))(input)? {
            (input, b"*") => Ok((input, Op::Mul)),
            (input, b"/") => Ok((input, Op::Div)),
            (input, b"%") => Ok((input, Op::Mod)),
            _ => unreachable!(),
        }
    }

    fn term(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = factor(input)?;
        let (input, _) = spaces(input)?;
        fold_many0(
            tuple((mul_div_or_mod, spaces, factor, spaces)),
            lhs,
            |lhs, (op, _, rhs, _)| Expr::Op(op, Box::new(lhs), Box::new(rhs)),
        )(input)
    }

    fn add_or_sub(input: &[u8]) -> IResult<&[u8], Op> {
        match alt((tag("+"), tag("-")))(input)? {
            (input, b"+") => Ok((input, Op::Add)),
            (input, b"-") => Ok((input, Op::Sub)),
            _ => unreachable!(),
        }
    }

    fn arithmetic(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = term(input)?;
        let (input, _) = spaces(input)?;
        fold_many0(
            tuple((add_or_sub, spaces, term, spaces)),
            lhs,
            |lhs, (op, _, rhs, _)| Expr::Op(op, Box::new(lhs), Box::new(rhs)),
        )(input)
    }

    fn disjunction_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
        let (input, _) = alt((tag("!!"), tag("||")))(input)?;
        Ok((input, LogicOp::Or))
    }

    fn conjunction_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
        let (input, _) = tag("&&")(input)?;
        Ok((input, LogicOp::And))
    }

    fn comparison_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
        let (input, op) = alt((
            tag("<="),
            tag(">="),
            tag("=="),
            tag("!="),
            tag("<"),
            tag(">"),
        ))(input)?;

        let op = match op {
            b"<" => LogicOp::Less,
            b"<=" => LogicOp::LessOrEqual,
            b">" => LogicOp::Greater,
            b">=" => LogicOp::GreaterOrEqual,
            b"==" => LogicOp::Eq,
            b"!=" => LogicOp::NotEq,
            _ => unreachable!(),
        };

        Ok((input, op))
    }

    fn comparison(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = arithmetic(input)?;
        let (input, _) = spaces(input)?;
        fold_many0(
            tuple((comparison_op, spaces, arithmetic, spaces)),
            lhs,
            |lhs, (op, _, rhs, _)| Expr::LogicOp(op, Box::new(lhs), Box::new(rhs)),
        )(input)
    }

    fn conjunction(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = comparison(input)?;
        let (input, _) = spaces(input)?;
        fold_many0(
            tuple((conjunction_op, spaces, comparison, spaces)),
            lhs,
            |lhs, (op, _, rhs, _)| Expr::LogicOp(op, Box::new(lhs), Box::new(rhs)),
        )(input)
    }

    fn disjunction(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = conjunction(input)?;
        let (input, _) = spaces(input)?;
        fold_many0(
            tuple((disjunction_op, spaces, conjunction, spaces)),
            lhs,
            |lhs, (op, _, rhs, _)| Expr::LogicOp(op, Box::new(lhs), Box::new(rhs)),
        )(input)
    }

    pub fn expr(input: &[u8]) -> IResult<&[u8], Expr> {
        preceded(spaces, disjunction)(input)
    }
}
