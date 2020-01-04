pub type Int = i64;
pub type Var = String;

pub mod parse {
    use super::*;
    use nom::bytes::complete::{take_while, take_while1};
    use nom::combinator::{map_res, verify};
    use nom::IResult;

    pub fn is_keyword(s: &str) -> bool {
        match s {
            "if" | "fi" | "elif" | "else" | "do" | "od" | "while" | "for" | "repeat" | "until"
            | "skip" | "write" | "read" => true,
            _ => false,
        }
    }

    pub fn variable(input: &[u8]) -> IResult<&[u8], Var> {
        verify(identifier, |v| !is_keyword(&v))(input)
    }

    fn identifier(input: &[u8]) -> IResult<&[u8], Var> {
        let (input, first) = take_while1(|c| (c as char).is_alphabetic() || c == b'_')(input)?;
        let (input, second) = take_while(|c| (c as char).is_alphanumeric() || c == b'_')(input)?;
        let first = std::str::from_utf8(first).unwrap();
        let second = std::str::from_utf8(second).unwrap();
        let name = Var::from(first) + second;
        Ok((input, name))
    }

    pub fn integer(input: &[u8]) -> IResult<&[u8], Int> {
        let (input, n) = map_res(take_while1(|c| (c as char).is_numeric()), |number| {
            std::str::from_utf8(number).unwrap().parse::<Int>()
        })(input)?;

        Ok((input, n))
    }
}
