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
use crate::statement::Statement;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut context = (Env::new(), io::stdin() , io::stdout());
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        let s = Statement::parse(line.trim().as_bytes());
        println!("AST: {:?}", s);

        if let Ok(s) = s {
            println!("Executing...");
            println!("{:?}", s.eval(&mut context));
        }
    }
}
