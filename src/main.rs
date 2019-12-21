mod context;
mod expr;
mod ops;
#[cfg(test)]
mod regression;
mod sm;
mod statement;
mod types;

use std::error::Error;
use std::io;

use crate::context::{Env, ExecutionContext};
use crate::sm::StackMachine;

fn run<C>(line: &str, context: &mut C) -> Result<(), Box<dyn Error>>
where
    C: ExecutionContext,
{
    let program = statement::parse(line.as_bytes())?;
    println!("Statements program: {:#?}", program);

    let program = sm::compile(&program);
    println!("Stack machine program: {:#?}", program);
    println!("Running...");
    let mut machine = StackMachine::new(context);
    println!("Result: {:?}", machine.run(&program));

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
