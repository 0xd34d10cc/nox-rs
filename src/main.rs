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

use crate::interpreter::Interpreter;

fn main() {
    if let Err(e) = Interpreter::new().run() {
        eprintln!("Failure: {}", e);
    }
}
