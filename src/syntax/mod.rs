use std::fmt::Debug;

use thiserror::Error;

mod expr;
mod program;
mod statement;
mod types;

pub type Input<'a> = &'a str;
pub type ParseError<'a> = nom::error::VerboseError<Input<'a>>;
pub type Parsed<'a, O> = nom::IResult<Input<'a>, O, ParseError<'a>>;

pub use self::expr::expr;
pub use self::program::{program, statements, statements1, Function, Program, Statement};
pub use self::types::{integer, variable};

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    Failed(String),

    #[error("Incomplete parse of {what}:\nParsed: {parsed}\nRest: {rest}")]
    Incomplete {
        what: &'static str,
        parsed: String,
        rest: String,
    },
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn spaces(input: Input) -> Parsed<Input> {
    nom::character::complete::multispace0(input)
}

// see https://github.com/rust-lang/rust-clippy/issues/2944
#[allow(clippy::needless_lifetimes)]
pub fn key<'a>(key: &'a str) -> impl Fn(Input<'a>) -> Parsed<Input> {
    nom::sequence::preceded(spaces, nom::bytes::complete::tag(key))
}

fn err(e: nom::Err<ParseError>, what: &str, input: &str) -> Error {
    let error = match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => format!(
            "Failed to parse {}:\n{}",
            what,
            nom::error::convert_error(input, e)
        ),
        nom::Err::Incomplete(needed) => format!("Incomplete parse of {}: {:?}", what, needed),
    };

    Error::Failed(error)
}

fn incomplete<T: Debug>(value: T, what: &'static str, rest: Input) -> Error {
    Error::Incomplete {
        what,
        parsed: format!("{:?}", value),
        rest: rest.to_string(),
    }
}

pub fn parse<P, T: Debug>(what: &'static str, parser: P, input: Input) -> Result<T>
where
    P: Fn(Input) -> Parsed<T>,
{
    let (input, v) = parser(input).map_err(|e| err(e, what, input))?;

    if !input.is_empty() {
        return Err(incomplete(v, what, input));
    }

    Ok(v)
}
