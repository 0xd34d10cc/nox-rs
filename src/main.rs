use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1, take_while};
use nom::combinator::{map, map_res, opt};
use nom::multi::fold_many0;
use nom::sequence::tuple;
use nom::IResult;

mod regression;

// TODO: deal with precedence of logic operations

// Expr ::= Logic
// Logic ::= Arithm ( '<' Arithm
//                  | '<=' Arithm
//                  | '>' Arithm
//                  | '>=' Arithm
//                  | '==' Arithm
//                  | '!=' Arithm
//                  | '&&' Arithm
//                  | '||' Arithm)*
//
// Arithm ::= Term ('+' Term | '-' Term)*
// Term ::= Factor ('*' Factor | '/' Factor | '%' Factor)*
// Factor ::= ['-'] (Var | Number | '(' Arithm ')')
//
// Number ::= Digit+
// Var ::= Alpha (Alpha | Digit)*


#[derive(Debug, Clone)]
enum Value {
    Int(i32),
}

#[derive(Debug, Clone)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
enum LogicOp {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Eq,
    NotEq,
    And,
    Or,
}

#[derive(Debug, Clone)]
enum Node {
    Var(String),
    Const(Value),
    Op(Op, Box<Node>, Box<Node>),
    LogicOp(LogicOp, Box<Node>, Box<Node>),
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
            std::str::from_utf8(number).unwrap().parse::<i32>()
        }),
        |n| Node::Const(Value::Int(n)),
    )(input)
}

fn factor(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, minus) = opt(tag(b"-"))(input)?;
    let (input, node) = alt((
        variable,
        integer,
        map(tuple((tag("("), arithmetic, tag(")"))), |(_, e, _)| e),
    ))(input)?;

    let node = if minus.is_some() {
        Node::Op(
            Op::Sub,
            Box::new(Node::Const(Value::Int(0))),
            Box::new(node),
        )
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
    fold_many0(tuple((mul_div_or_mod, factor)), lhs, |lhs, (op, rhs)| {
        Node::Op(op, Box::new(lhs), Box::new(rhs))
    })(input)
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
    fold_many0(tuple((add_or_sub, term)), lhs, |lhs, (op, rhs)| {
        Node::Op(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

fn logic_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
    match alt((tag("<"), tag(">"), tag("<="), tag(">="),
               tag("=="), tag("!="),
               tag("&&"), tag("||")))(input)? {
        (input, b"<") => Ok((input, LogicOp::Less)),
        (input, b">") => Ok((input, LogicOp::Greater)),
        (input, b"<=") => Ok((input, LogicOp::LessOrEqual)),
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
    fold_many0(tuple((logic_op, arithmetic)), lhs, |lhs, (op, rhs)| {
        Node::LogicOp(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

fn expr(input: &[u8]) -> IResult<&[u8], Node> {
    logic(input)
}

type Context = HashMap<String, Value>;

fn eval(node: Node, context: &Context) -> Result<Value, Box<dyn std::error::Error>> {
    match node {
        Node::Var(name) => {
            let val = context.get(&name)
                .cloned()
                .ok_or_else(|| format!("Variable {} is not defined", name))?;
            Ok(val)
        },
        Node::Const(v) => Ok(v),
        Node::Op(op, lhs, rhs) => {
            let Value::Int(left) = eval(*lhs, context)?;
            let Value::Int(right) = eval(*rhs, context)?;

            let result = match op {
                Op::Add => left + right,
                Op::Sub => left - right,
                Op::Mul => left * right,
                Op::Div => {
                    if right == 0 {
                        return Err("Attempt to divide by 0".into());
                    }

                    left / right
                },
                Op::Mod => {
                    if right == 0 {
                        return Err("Attempt to mod by 0".into());
                    }

                    left % right
                }
            };

            Ok(Value::Int(result))
        }
        Node::LogicOp(op, lhs, rhs) => {
            let Value::Int(left) = eval(*lhs, context)?;
            let Value::Int(right) = eval(*rhs, context)?;

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

            Ok(Value::Int(result as i32))
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
