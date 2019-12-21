mod context;
mod expr;
mod ops;
#[cfg(test)]
mod regression;
mod sm;
mod statement;
mod types;

use crate::context::Env;
use crate::statement::Statement;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let env = Env::new();
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        let s = Statement::parse(line.trim().as_bytes());
        println!("{:?}", s);

        if let Ok(s) = s {
            // println!("{:?}", s.eval(&env));
        }
    }
}
