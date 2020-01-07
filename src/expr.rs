use crate::context::{InputStream, OutputStream};
use crate::ops::{LogicOp, Op};
use crate::statement::ExecutionContext;
use crate::types::{Int, Result, Var};

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    Const(Int),
    Op(Op, Box<Expr>, Box<Expr>),
    LogicOp(LogicOp, Box<Expr>, Box<Expr>),
    Call(Var, Vec<Expr>),
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

    pub fn eval<I, O>(&self, context: &mut ExecutionContext<'_, I, O>) -> Result<Int>
    where
        I: InputStream,
        O: OutputStream,
    {
        match self {
            Expr::Var(name) => {
                let val = context
                    .memory()
                    .load(name)
                    .ok_or_else(|| format!("Variable {} is not defined", name))?;
                Ok(val)
            }
            Expr::Const(v) => Ok(*v),
            Expr::Op(op, lhs, rhs) => {
                let left = lhs.eval(context)?;
                let right = rhs.eval(context)?;
                let v = op.apply(left, right)?;
                Ok(v)
            }
            Expr::LogicOp(op, lhs, rhs) => {
                let left = lhs.eval(context)?;
                let right = rhs.eval(context)?;
                let v = op.apply(left, right);
                Ok(Int::from(v))
            }
            Expr::Call(name, args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.eval(context))
                    .collect::<Result<_>>()?;

                let retval = context
                    .call(name, &args)?
                    .ok_or_else(|| format!("Call to {} procedure inside expression", name))?;

                Ok(retval)
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
    // Factor ::= ['-'] (Call | Var | Number | '(' Expr ')')
    //
    // Call ::= Var '(' Expr* ')'
    // Number ::= Digit+
    // Var ::= Alpha (Alpha | Digit)*

    use super::*;
    use crate::types::parse::{integer, variable};

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::combinator::{map, opt};
    use nom::multi::{fold_many0, separated_list};
    use nom::sequence::{delimited, pair, preceded};
    use nom::IResult;

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
    }

    fn key<'a>(key: &'a str) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], &'a [u8]> {
        preceded(spaces, tag(key))
    }

    fn factor(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, minus) = opt(key("-"))(input)?;
        let (input, node) = alt((
            call,
            map(variable, Expr::Var),
            map(integer, Expr::Const),
            delimited(key("("), expr, key(")")),
        ))(input)?;

        let node = if minus.is_some() {
            Expr::Op(Op::Sub, Box::new(Expr::Const(0)), Box::new(node))
        } else {
            node
        };

        Ok((input, node))
    }

    fn call(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, name) = variable(input)?;
        let (input, args) = delimited(key("("), separated_list(key(","), expr), key(")"))(input)?;
        Ok((input, Expr::Call(name, args)))
    }

    fn mul_div_or_mod(input: &[u8]) -> IResult<&[u8], Op> {
        match alt((key("*"), key("/"), key("%")))(input)? {
            (input, b"*") => Ok((input, Op::Mul)),
            (input, b"/") => Ok((input, Op::Div)),
            (input, b"%") => Ok((input, Op::Mod)),
            _ => unreachable!(),
        }
    }

    fn term(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = factor(input)?;
        fold_many0(pair(mul_div_or_mod, factor), lhs, |lhs, (op, rhs)| {
            Expr::Op(op, Box::new(lhs), Box::new(rhs))
        })(input)
    }

    fn add_or_sub(input: &[u8]) -> IResult<&[u8], Op> {
        match alt((key("+"), key("-")))(input)? {
            (input, b"+") => Ok((input, Op::Add)),
            (input, b"-") => Ok((input, Op::Sub)),
            _ => unreachable!(),
        }
    }

    fn arithmetic(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = term(input)?;
        fold_many0(pair(add_or_sub, term), lhs, |lhs, (op, rhs)| {
            Expr::Op(op, Box::new(lhs), Box::new(rhs))
        })(input)
    }

    fn disjunction_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
        let (input, _) = alt((key("!!"), key("||")))(input)?;
        Ok((input, LogicOp::Or))
    }

    fn conjunction_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
        let (input, _) = key("&&")(input)?;
        Ok((input, LogicOp::And))
    }

    fn comparison_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
        let (input, op) = alt((
            key("<="),
            key(">="),
            key("=="),
            key("!="),
            key("<"),
            key(">"),
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
        fold_many0(pair(comparison_op, arithmetic), lhs, |lhs, (op, rhs)| {
            Expr::LogicOp(op, Box::new(lhs), Box::new(rhs))
        })(input)
    }

    fn conjunction(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = comparison(input)?;
        fold_many0(pair(conjunction_op, comparison), lhs, |lhs, (op, rhs)| {
            Expr::LogicOp(op, Box::new(lhs), Box::new(rhs))
        })(input)
    }

    fn disjunction(input: &[u8]) -> IResult<&[u8], Expr> {
        let (input, lhs) = conjunction(input)?;
        fold_many0(pair(disjunction_op, conjunction), lhs, |lhs, (op, rhs)| {
            Expr::LogicOp(op, Box::new(lhs), Box::new(rhs))
        })(input)
    }

    pub fn expr(input: &[u8]) -> IResult<&[u8], Expr> {
        preceded(spaces, disjunction)(input)
    }
}
