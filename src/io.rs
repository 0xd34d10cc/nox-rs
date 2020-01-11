use std::io::{self, Stdin, Stdout, Write};

use crate::types::Int;

pub trait InputStream {
    fn read(&mut self) -> Option<Int>;
}

pub trait OutputStream {
    fn write(&mut self, value: Int);
}

// IO streams implementations
impl InputStream for Stdin {
    fn read(&mut self) -> Option<Int> {
        print!("I: ");
        io::stdout().flush().ok()?;

        let mut line = String::new();
        self.read_line(&mut line).ok()?;
        line.trim().parse::<Int>().ok()
    }
}

impl OutputStream for Stdout {
    fn write(&mut self, value: Int) {
        writeln!(self, "O: {}", value).unwrap();
    }
}

impl InputStream for Vec<Int> {
    fn read(&mut self) -> Option<Int> {
        self.pop()
    }
}

impl OutputStream for Vec<Int> {
    fn write(&mut self, value: Int) {
        self.push(value)
    }
}

pub struct EmptyInput;
impl InputStream for EmptyInput {
    fn read(&mut self) -> Option<Int> {
        None
    }
}

pub struct IgnoreOutput;
impl OutputStream for IgnoreOutput {
    fn write(&mut self, _: Int) {}
}
