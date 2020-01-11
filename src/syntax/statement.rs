// Statements ::= Statement (';' Statement)*
// Statement ::= Skip | IfElse | While | For | Assign | Read | Write | Call
// Skip ::= 'skip'
// IfElse ::= 'if' Expr 'then' Statements  ('elif' Expr 'then' Statements)* ['else' Statements] 'fi'
// While ::= 'while' Expr 'do' Statements 'od'
// For ::= 'for' Statement ',' Expr ',' Statement 'do' Statements 'od'
// RepeatUntil ::= 'repeat' Statements 'until' Expr
// Assign ::= Var '=' Expr
// Read ::= 'read(' Var ')'
// Write ::= 'write(' Expr ')'
// Call ::= Var '(' Expr* ')'
// Var ::= Char+

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list, separated_nonempty_list};
use nom::sequence::{delimited, preceded};

use super::expr;
use super::types::variable;
use super::{key, spaces, Input, Parsed};
use crate::statement::Expr;
use crate::types::Var;

#[derive(Debug, Clone)]
pub enum Statement {
    Skip,
    IfElse {
        condition: Expr,
        if_true: Vec<Statement>,
        elifs: Vec<(Expr, Vec<Statement>)>,
        if_false: Vec<Statement>,
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
    Call {
        name: Var,
        args: Vec<Expr>,
    },
    Return(Option<Expr>),
}

pub fn statements1(input: Input) -> Parsed<Vec<Statement>> {
    separated_nonempty_list(key(";"), statement)(input)
}

pub fn statements(input: Input) -> Parsed<Vec<Statement>> {
    separated_list(key(";"), statement)(input)
}

pub fn statement(input: Input) -> Parsed<Statement> {
    preceded(
        spaces,
        alt((
            skip,
            while_,
            for_,
            repeat_until,
            if_else,
            return_,
            assign,
            read,
            write,
            call,
        )),
    )(input)
}

fn skip(input: Input) -> Parsed<Statement> {
    map(tag("skip"), |_| Statement::Skip)(input)
}

fn while_(input: Input) -> Parsed<Statement> {
    let (input, condition) = preceded(key("while"), expr)(input)?;
    let (input, body) = delimited(key("do"), statements, key("od"))(input)?;
    Ok((input, Statement::While { condition, body }))
}

fn for_(input: Input) -> Parsed<Statement> {
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

fn repeat_until(input: Input) -> Parsed<Statement> {
    let (input, body) = preceded(key("repeat"), statements)(input)?;
    let (input, condition) = preceded(key("until"), expr)(input)?;
    Ok((input, Statement::RepeatUntil { body, condition }))
}

fn if_else(input: Input) -> Parsed<Statement> {
    let (input, root_condition) = preceded(key("if"), expr)(input)?;
    let (input, if_true) = preceded(key("then"), statements)(input)?;
    let (input, elifs) = many0(elif)(input)?;
    let (input, else_) = opt(else_)(input)?;
    let (input, _) = key("fi")(input)?;

    Ok((
        input,
        Statement::IfElse {
            condition: root_condition,
            if_true,
            elifs,
            if_false: else_.unwrap_or_default(),
        },
    ))
}

fn elif(input: Input) -> Parsed<(Expr, Vec<Statement>)> {
    let (input, condition) = preceded(key("elif"), expr)(input)?;
    let (input, body) = preceded(key("then"), statements)(input)?;
    Ok((input, (condition, body)))
}

fn else_(input: Input) -> Parsed<Vec<Statement>> {
    preceded(key("else"), statements)(input)
}

fn return_(input: Input) -> Parsed<Statement> {
    let (input, e) = preceded(key("return"), opt(expr))(input)?;
    Ok((input, Statement::Return(e)))
}

fn assign(input: Input) -> Parsed<Statement> {
    let (input, var) = variable(input)?;
    let (input, e) = preceded(key(":="), expr)(input)?;
    Ok((input, Statement::Assign(var, e)))
}

fn read(input: Input) -> Parsed<Statement> {
    let (input, _) = key("read")(input)?;
    let (input, var) = delimited(key("("), variable, key(")"))(input)?;
    Ok((input, Statement::Read(var)))
}

fn write(input: Input) -> Parsed<Statement> {
    let (input, _) = key("write")(input)?;
    let (input, e) = delimited(key("("), expr, key(")"))(input)?;
    Ok((input, Statement::Write(e)))
}

fn call(input: Input) -> Parsed<Statement> {
    let (input, name) = variable(input)?;
    let (input, args) = delimited(key("("), separated_list(key(","), expr), key(")"))(input)?;
    Ok((input, Statement::Call { name, args }))
}
