pub type Input<'a> = &'a str;
pub type Error<'a> = nom::error::VerboseError<Input<'a>>;
pub type Parsed<'a, O> = nom::IResult<Input<'a>, O, Error<'a>>;

pub fn spaces(input: Input) -> Parsed<Input> {
    nom::character::complete::multispace0(input)
}

pub fn key<'a>(key: &'a str) -> impl Fn(Input<'a>) -> Parsed<Input> {
    nom::sequence::preceded(spaces, nom::bytes::complete::tag(key))
}

pub fn format_err(e: nom::Err<Error>, what: &str, input: &str) -> String {
    match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => format!(
            "Failed to parse {}:\n{}",
            what,
            nom::error::convert_error(input, e)
        ),
        nom::Err::Incomplete(needed) => format!("Incomplete parse of {}: {:?}", what, needed),
    }
}
