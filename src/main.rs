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

pub type Int = i64;
pub type Var = String;

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Var, Expr),
    Read(Var),
    Write(Var),
    Seq(Box<Statement>, Box<Statement>)
}

// Execution context: model for memory, input & output streams
trait ExecutionContext {
    fn get(&self, name: &str) -> Option<Int>;
    fn set(&mut self, name: &str, value: Int);

    fn read(&mut self) -> Option<Int>;
    fn write(&mut self, value: Int);
}

// parse statement
fn statement(input: &[u8]) -> IResult<&[u8], Statement> {
    todo!()
}

// execute program (statement language)
fn exec<C>(program: Statement, context: &mut C) where C: ExecutionContext {
    todo!()
}

// Stack machine instruction
#[derive(Debug, Clone)]
pub enum Instruction {
    Op(Op),
    LogicOp(LogicOp),
    Const(Int),
    Read,
    Write,
    Load(Var),
    Store(Var)
}

type Program = Vec<Instruction>;

// convert from statement ast to list of stack machine instructions
fn compile(statements: Statement) -> Program {
    todo!()
}

// run stack machine program within given execution context
fn run<C>(program: Program, context: &mut C, stack: Vec<Int>) where C: ExecutionContext {
    todo!()
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    Const(Int),
    Op(Op, Box<Expr>, Box<Expr>),
    LogicOp(LogicOp, Box<Expr>, Box<Expr>),
}

fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|c| (c as char).is_whitespace())(input)
}

fn variable(input: &[u8]) -> IResult<&[u8], Expr> {
    let (input, first) = take_while1(|c| (c as char).is_alphabetic())(input)?;
    let (input, second) = take_while(|c| (c as char).is_alphanumeric())(input)?;
    let first = std::str::from_utf8(first).unwrap();
    let second = std::str::from_utf8(second).unwrap();
    let name = Var::from(first) + second;

    Ok((input, Expr::Var(name)))
}

fn integer(input: &[u8]) -> IResult<&[u8], Expr> {
    let (input, n) = map_res(take_while1(|c| (c as char).is_numeric()), |number| {
        std::str::from_utf8(number).unwrap().parse::<Int>()
    })(input)?;

    Ok((input, Expr::Const(n)))
}

fn factor(input: &[u8]) -> IResult<&[u8], Expr> {
    let (input, minus) = opt(tag(b"-"))(input)?;
    let (input, node) = alt((
        variable,
        integer,
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

fn logic_op(input: &[u8]) -> IResult<&[u8], LogicOp> {
    let (input, op) = alt((
        tag("<="),
        tag(">="),
        tag("=="),
        tag("!="),
        tag("&&"),
        tag("||"),
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
        b"&&" => LogicOp::And,
        b"||" => LogicOp::Or,
        _ => unreachable!(),
    };

    Ok((input, op))
}

fn logic(input: &[u8]) -> IResult<&[u8], Expr> {
    let (input, lhs) = arithmetic(input)?;
    let (input, _) = spaces(input)?;
    fold_many0(
        tuple((logic_op, spaces, arithmetic, spaces)),
        lhs,
        |lhs, (op, _, rhs, _)| Expr::LogicOp(op, Box::new(lhs), Box::new(rhs)),
    )(input)
}

pub fn expr(input: &[u8]) -> IResult<&[u8], Expr> {
    logic(input)
}

pub type Env = HashMap<Var, Int>;

pub fn eval(expr: Expr, env: &Env) -> Result<Int, Box<dyn std::error::Error>> {
    match expr {
        Expr::Var(name) => {
            let val = env
                .get(&name)
                .copied()
                .ok_or_else(|| format!("Variable {} is not defined", name))?;
            Ok(val)
        }
        Expr::Const(v) => Ok(v),
        Expr::Op(op, lhs, rhs) => {
            let left = eval(*lhs, env)?;
            let right = eval(*rhs, env)?;

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
        Expr::LogicOp(op, lhs, rhs) => {
            let left = eval(*lhs, env)?;
            let right = eval(*rhs, env)?;

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
    let env = Env::new();
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        let e = expr(line.as_bytes());
        println!("{:?}", e);

        if let Ok((_, expr)) = e {
            println!("{:?}", eval(expr, &env));
        }
    }
}
