#![feature(proc_macro_hygiene)] // for dynasm

mod context;
mod expr;
mod interpreter;
mod jit;
mod nom;
mod ops;
#[cfg(test)]
mod regression;
mod sm;
mod statement;
mod typecheck;
mod types;

use std::error::Error;

use crate::interpreter::Interpreter;

fn main() -> Result<(), Box<dyn Error>> {
    if let Err(e) = Interpreter::new().run() {
        eprintln!("Failure: {}", e);
    }

    Ok(())
}
