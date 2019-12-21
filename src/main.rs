mod context;
mod expr;
mod ops;
#[cfg(test)]
mod regression;
mod sm;
mod statement;
mod types;

use crate::context::Env;
use crate::expr::Expr;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let env = Env::new();
    loop {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        let e = Expr::parse(line.as_bytes());
        println!("{:?}", e);

        if let Ok(e) = e {
            println!("{:?}", e.eval(&env));
        }
    }
}
