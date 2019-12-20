use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::combinator::{map, map_res, opt};
use nom::multi::fold_many0;
use nom::sequence::tuple;
use nom::IResult;

#[cfg(test)]
mod regression;

// TODO: deal with precedence of logic operations

// Expr ::= Logic
// Logic ::= Arithm ( '<'  Arithm
//                  | '<=' Arithm
//                  | '>'  Arithm
//                  | '>=' Arithm
//                  | '==' Arithm
//                  | '!=' Arithm
//                  | '&&' Arithm
//                  | '||' Arithm)*
//
// Arithm ::= Term ('+' Term | '-' Term)*
// Term ::= Factor ('*' Factor | '/' Factor | '%' Factor)*
// Factor ::= ['-'] (Var | Number | '(' Expr ')')
//
// Number ::= Digit+
// Var ::= Alpha (Alpha | Digit)*

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum LogicOp {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Eq,
    NotEq,
    And,
    Or,
}

// TODO: emulate OCaml Int type (which is 63-bit wide (?))
pub type Int = i64;

#[derive(Debug, Clone)]
pub enum Node {
    Var(String),
    Const(Int),
    Op(Op, Box<Node>, Box<Node>),
    LogicOp(LogicOp, Box<Node>, Box<Node>),
}

fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|c| (c as char).is_whitespace())(input)
}

fn variable(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, first) = take_while1(|c| (c as char).is_alphabetic())(input)?;
    let (input, second) = take_while(|c| (c as char).is_alphanumeric())(input)?;
    let first = std::str::from_utf8(first).unwrap();
    let second = std::str::from_utf8(second).unwrap();
    let name = String::from(first) + second;

    Ok((input, Node::Var(name)))
}

fn integer(input: &[u8]) -> IResult<&[u8], Node> {
    map(
        map_res(take_while1(|c| (c as char).is_numeric()), |number| {
            std::str::from_utf8(number).unwrap().parse::<Int>()
        }),
        |n| Node::Const(n),
    )(input)
}

fn factor(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, minus) = opt(tag(b"-"))(input)?;
    let (input, node) = alt((
        variable,
        integer,
        map(
            tuple((spaces, tag("("), spaces, expr, spaces, tag(")"), spaces)),
            |(_, _, _, e, _, _, _)| e,
        ),
    ))(input)?;

    let node = if minus.is_some() {
        Node::Op(Op::Sub, Box::new(Node::Const(0)), Box::new(node))
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

fn term(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, lhs) = factor(input)?;
    let (input, _) = spaces(input)?;
    fold_many0(
        tuple((mul_div_or_mod, spaces, factor, spaces)),
        lhs,
        |lhs, (op, _, rhs, _)| Node::Op(op, Box::new(lhs), Box::new(rhs)),
    )(input)
}

fn add_or_sub(input: &[u8]) -> IResult<&[u8], Op> {
    match alt((tag("+"), tag("-")))(input)? {
        (input, b"+") => Ok((input, Op::Add)),
        (input, b"-") => Ok((input, Op::Sub)),
        _ => unreachable!(),
    }
}

fn arithmetic(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, lhs) = term(input)?;
    let (input, _) = spaces(input)?;
    fold_many0(
        tuple((add_or_sub, spaces, term, spaces)),
        lhs,
        |lhs, (op, _, rhs, _)| Node::Op(op, Box::new(lhs), Box::new(rhs)),
    )(input)
}

fn logic_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
    match alt((
        tag("<="),
        tag(">="),
        tag("=="),
        tag("!="),
        tag("&&"),
        tag("||"),
        tag("<"),
        tag(">"),
    ))(input)?
    {
        (input, b"<") => Ok((input, LogicOp::Less)),
        (input, b"<=") => Ok((input, LogicOp::LessOrEqual)),
        (input, b">") => Ok((input, LogicOp::Greater)),
        (input, b">=") => Ok((input, LogicOp::GreaterOrEqual)),
        (input, b"==") => Ok((input, LogicOp::Eq)),
        (input, b"!=") => Ok((input, LogicOp::NotEq)),
        (input, b"&&") => Ok((input, LogicOp::And)),
        (input, b"||") => Ok((input, LogicOp::Or)),
        _ => unreachable!(),
    }
}

fn logic(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, lhs) = arithmetic(input)?;
    let (input, _) = spaces(input)?;
    fold_many0(
        tuple((logic_op, spaces, arithmetic, spaces)),
        lhs,
        |lhs, (op, _, rhs, _)| Node::LogicOp(op, Box::new(lhs), Box::new(rhs)),
    )(input)
}

pub fn expr(input: &[u8]) -> IResult<&[u8], Node> {
    logic(input)
}

pub type Context = HashMap<String, Int>;

pub fn eval(node: Node, context: &Context) -> Result<Int, Box<dyn std::error::Error>> {
    match node {
        Node::Var(name) => {
            let val = context
                .get(&name)
                .cloned()
                .ok_or_else(|| format!("Variable {} is not defined", name))?;
            Ok(val)
        }
        Node::Const(v) => Ok(v),
        Node::Op(op, lhs, rhs) => {
            let left = eval(*lhs, context)?;
            let right = eval(*rhs, context)?;

            let result = match op {
                Op::Add => left.wrapping_add(right),
                Op::Sub => left.wrapping_sub(right),
                Op::Mul => left.wrapping_mul(right),
                Op::Div => {
                    if right == 0 {
                        return Err("Attempt to divide by 0".into());
                    }

                    left / right
                }
                Op::Mod => {
                    if right == 0 {
                        return Err("Attempt to mod by 0".into());
                    }

                    left % right
                }
            };

            Ok(result)
        }
        Node::LogicOp(op, lhs, rhs) => {
            let left = eval(*lhs, context)?;
            let right = eval(*rhs, context)?;

            let result = match op {
                LogicOp::Less => left < right,
                LogicOp::LessOrEqual => left <= right,
                LogicOp::Greater => left > right,
                LogicOp::GreaterOrEqual => left >= right,
                LogicOp::Eq => left == right,
                LogicOp::NotEq => left != right,
                LogicOp::And => left != 0 && right != 0,
                LogicOp::Or => left != 0 || right != 0,
            };

            Ok(result as Int)
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let context = Context::new();
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        let e = expr(line.as_bytes());
        println!("{:?}", e);

        if let Ok((_, node)) = e {
            println!("{:?}", eval(node, &context));
        }
    }
}
