use nom::bytes::complete::{take_while, take_while1};
use nom::combinator::{map_res, verify};
use nom::sequence::preceded;

use super::{spaces, Input, Parsed};
use crate::types::{Int, Var};

fn is_keyword(s: &str) -> bool {
    match s {
        "if" | "fi" | "elif" | "else" | "do" | "od" | "while" | "for" | "repeat" | "until"
        | "skip" | "write" | "read" | "main" => true,
        _ => false,
    }
}

pub fn variable(input: Input) -> Parsed<Var> {
    verify(preceded(spaces, identifier), |v| !is_keyword(&v))(input)
}

fn identifier(input: Input) -> Parsed<Var> {
    let (input, first) = take_while1(|c: char| c.is_alphabetic() || c == '_')(input)?;
    let (input, second) = take_while(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    let name = [first, second].concat();
    let name = Var::from(name.as_str());
    Ok((input, name))
}

pub fn integer(input: Input) -> Parsed<Int> {
    let (input, _) = spaces(input)?;
    let (input, n) = map_res(take_while1::<_, Input, _>(|c| c.is_numeric()), |number| {
        number.parse::<Int>()
    })(input)?;

    Ok((input, n))
}
