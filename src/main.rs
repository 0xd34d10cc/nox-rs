#![feature(proc_macro_hygiene)] // for dynasm

#[cfg(test)]
mod regression;

mod interpreter;
mod io;
mod jit;
mod memory;
mod ops;
mod sm;
mod statement;
mod syntax;
mod types;

use std::error::Error;

use crate::interpreter::Interpreter;

fn main() -> Result<(), Box<dyn Error>> {
    if let Err(e) = Interpreter::new().run() {
        eprintln!("Failure: {}", e);
    }

    Ok(())
}
