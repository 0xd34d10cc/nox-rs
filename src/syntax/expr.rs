use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{fold_many0, separated_list};
use nom::sequence::{delimited, pair, preceded};

use super::{integer, key, spaces, variable, Input, Parsed};
use crate::ops::{LogicOp, Op};
use crate::statement::Expr;

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
// Call ::= Var '(' Expr* ')'

fn factor(input: Input) -> Parsed<Expr> {
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

fn call(input: Input) -> Parsed<Expr> {
    let (input, name) = variable(input)?;
    let (input, args) = delimited(key("("), separated_list(key(","), expr), key(")"))(input)?;
    Ok((input, Expr::Call(name, args)))
}

fn mul_div_or_mod(input: Input) -> Parsed<Op> {
    match alt((key("*"), key("/"), key("%")))(input)? {
        (input, "*") => Ok((input, Op::Mul)),
        (input, "/") => Ok((input, Op::Div)),
        (input, "%") => Ok((input, Op::Mod)),
        _ => unreachable!(),
    }
}

fn term(input: Input) -> Parsed<Expr> {
    let (input, lhs) = factor(input)?;
    fold_many0(pair(mul_div_or_mod, factor), lhs, |lhs, (op, rhs)| {
        Expr::Op(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

fn add_or_sub(input: Input) -> Parsed<Op> {
    match alt((key("+"), key("-")))(input)? {
        (input, "+") => Ok((input, Op::Add)),
        (input, "-") => Ok((input, Op::Sub)),
        _ => unreachable!(),
    }
}

fn arithmetic(input: Input) -> Parsed<Expr> {
    let (input, lhs) = term(input)?;
    fold_many0(pair(add_or_sub, term), lhs, |lhs, (op, rhs)| {
        Expr::Op(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

fn disjunction_op(input: Input) -> Parsed<LogicOp> {
    let (input, _) = alt((key("!!"), key("||")))(input)?;
    Ok((input, LogicOp::Or))
}

fn conjunction_op(input: Input) -> Parsed<LogicOp> {
    let (input, _) = key("&&")(input)?;
    Ok((input, LogicOp::And))
}

fn comparison_op(input: Input) -> Parsed<LogicOp> {
    let (input, op) = alt((
        key("<="),
        key(">="),
        key("=="),
        key("!="),
        key("<"),
        key(">"),
    ))(input)?;

    let op = match op {
        "<" => LogicOp::Less,
        "<=" => LogicOp::LessOrEqual,
        ">" => LogicOp::Greater,
        ">=" => LogicOp::GreaterOrEqual,
        "==" => LogicOp::Eq,
        "!=" => LogicOp::NotEq,
        _ => unreachable!(),
    };

    Ok((input, op))
}

fn comparison(input: Input) -> Parsed<Expr> {
    let (input, lhs) = arithmetic(input)?;
    fold_many0(pair(comparison_op, arithmetic), lhs, |lhs, (op, rhs)| {
        Expr::LogicOp(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

fn conjunction(input: Input) -> Parsed<Expr> {
    let (input, lhs) = comparison(input)?;
    fold_many0(pair(conjunction_op, comparison), lhs, |lhs, (op, rhs)| {
        Expr::LogicOp(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

fn disjunction(input: Input) -> Parsed<Expr> {
    let (input, lhs) = conjunction(input)?;
    fold_many0(pair(disjunction_op, conjunction), lhs, |lhs, (op, rhs)| {
        Expr::LogicOp(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

pub fn expr(input: Input) -> Parsed<Expr> {
    preceded(spaces, disjunction)(input)
}
