use std::error::Error;

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        Ok(())
    }
}
