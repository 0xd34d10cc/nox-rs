use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::combinator::{map, map_res, opt};
use nom::multi::fold_many0;
use nom::sequence::tuple;
use nom::IResult;

// Arithm ::= Term ('+' Term | '-' Term)*
// Term ::= Factor ('*' Factor | '/' Factor)*
// Factor ::= ['-'] (Number | '(' Arithm ')')
//
// Number ::= Digit+

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
}

#[derive(Debug, Clone)]
enum Node {
    Constant(Value),
    ArithmeticOperation(Op, Box<Node>, Box<Node>),
}

fn integer(input: &[u8]) -> IResult<&[u8], Node> {
    map(
        map_res(take_while1(|c| (c as char).is_numeric()), |number| {
            std::str::from_utf8(number).unwrap().parse::<i32>()
        }),
        |n| Node::Constant(Value::Int(n)),
    )(input)
}

fn factor(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, minus) = opt(tag(b"-"))(input)?;
    let (input, node) = alt((
        integer,
        map(tuple((tag("("), arithmetic, tag(")"))), |(_, e, _)| e),
    ))(input)?;

    let node = if minus.is_some() {
        Node::ArithmeticOperation(
            Op::Sub,
            Box::new(Node::Constant(Value::Int(0))),
            Box::new(node),
        )
    } else {
        node
    };

    Ok((input, node))
}

fn mul_or_div(input: &[u8]) -> IResult<&[u8], Op> {
    match alt((tag("*"), tag("/")))(input)? {
        (input, b"*") => Ok((input, Op::Mul)),
        (input, b"/") => Ok((input, Op::Div)),
        _ => unreachable!(),
    }
}

fn term(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, lhs) = factor(input)?;
    fold_many0(tuple((mul_or_div, factor)), lhs, |lhs, (op, rhs)| {
        Node::ArithmeticOperation(op, Box::new(lhs), Box::new(rhs))
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
        Node::ArithmeticOperation(op, Box::new(lhs), Box::new(rhs))
    })(input)
}

fn eval(node: Node) -> Result<Value, Box<dyn std::error::Error>> {
    match node {
        Node::Constant(v) => Ok(v),
        Node::ArithmeticOperation(op, lhs, rhs) => {
            let Value::Int(left) = eval(*lhs)?;
            let Value::Int(right) = eval(*rhs)?;

            let result = match op {
                Op::Add => left + right,
                Op::Sub => left - right,
                Op::Mul => left * right,
                Op::Div => {
                   if right == 0 {
                      return Err("Attempt to divide by 0".into());
                   }

                   left / right
                }
            };

            Ok(Value::Int(result))
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        let e = arithmetic(line.as_bytes());
        println!("{:?}", e);

        if let Ok((_, node)) = e {
            println!("{:?}", eval(node));
        }
    }
}
