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
use std::io::Read;

use crate::x86::Compiler;

fn compile(line: &str) -> Result<(), Box<dyn Error>> {
    let program = statement::parse(line.as_bytes())?;
    let program = sm::compile(&program);
    let program = Compiler::new().compile(&program)?;
    println!("{}", program);

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut program = String::new();
    std::io::stdin().read_to_string(&mut program)?;
    if let Err(e) = compile(program.trim()) {
        eprintln!("Failure: {:?}", e);
    }
    Ok(())
}
