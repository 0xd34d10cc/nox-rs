mod context;
mod expr;
mod ops;
#[cfg(test)]
mod regression;
mod sm;
mod statement;
mod types;

use std::io;

use crate::context::Env;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = (Env::new(), io::stdin(), io::stdout());
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        let program = statement::parse(line.trim().as_bytes());
        println!("Program: {:?}", program);

        if let Ok(program) = program {
            println!("{:?}", statement::run(&program, &mut context));
        }
    }
}
