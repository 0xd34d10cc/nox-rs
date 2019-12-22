mod context;
mod expr;
mod ops;
#[cfg(test)]
mod regression;
mod sm;
mod statement;
mod types;
mod x86;

use std::error::Error;
use std::io;

use crate::context::{Env, ExecutionContext};
use crate::x86::Compiler;

fn run<C>(line: &str, _context: &mut C) -> Result<(), Box<dyn Error>>
where
    C: ExecutionContext,
{
    let program = statement::parse(line.as_bytes())?;
    println!("Statements program:\n{:#?}", program);

    let program = sm::compile(&program);
    println!("Stack machine program:\n{:#?}", program);

    let program = Compiler::new().compile(&program)?;
    println!("Assembly:\n{}", program);

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut context = (Env::new(), io::stdin(), io::stdout());
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        if let Err(e) = run(line.trim(), &mut context) {
            println!("Failure: {:?}", e);
        }
    }
}
