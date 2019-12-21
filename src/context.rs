use std::collections::HashMap;
use std::io::{Stdin, Stdout};

use crate::types::{Int, Var};

pub trait Memory {
    fn get(&self, name: &str) -> Option<Int>;
    fn set(&mut self, name: &str, value: Int);
}

pub trait InputStream {
    fn read(&mut self) -> Option<Int>;
}

pub trait OutputStream {
    fn write(&mut self, value: Int);
}

// Execution context: model for memory, input & output streams
pub trait ExecutionContext: Memory + InputStream + OutputStream {}

pub type Env = HashMap<Var, Int>;

impl Memory for Env {
    fn get(&self, name: &str) -> Option<Int> {
        HashMap::get(self, name).copied()
    }

    fn set(&mut self, name: &str, value: Int) {
        HashMap::insert(self, name.to_string(), value);
    }
}

// boilerplate to make &mut T work
impl<T> Memory for &mut T
where
    T: Memory,
{
    fn get(&self, name: &str) -> Option<Int> {
        <T as Memory>::get(self, name)
    }

    fn set(&mut self, name: &str, value: Int) {
        <T as Memory>::set(self, name, value)
    }
}

impl<T> InputStream for &mut T
where
    T: InputStream,
{
    fn read(&mut self) -> Option<Int> {
        <T as InputStream>::read(self)
    }
}

impl<T> OutputStream for &mut T
where
    T: OutputStream,
{
    fn write(&mut self, value: Int) {
        <T as OutputStream>::write(self, value)
    }
}

impl<T> ExecutionContext for &mut T where T: ExecutionContext {}

// boilerplate to make tuple of (memory, input, output) implement ExecutionContext
impl<M, I, O> Memory for (M, I, O)
where
    M: Memory,
{
    fn get(&self, name: &str) -> Option<Int> {
        self.0.get(name)
    }

    fn set(&mut self, name: &str, value: Int) {
        self.0.set(name, value)
    }
}

impl<M, I, O> InputStream for (M, I, O)
where
    I: InputStream,
{
    fn read(&mut self) -> Option<Int> {
        self.1.read()
    }
}

impl<M, I, O> OutputStream for (M, I, O)
where
    O: OutputStream,
{
    fn write(&mut self, value: Int) {
        self.2.write(value)
    }
}

impl<M, I, O> ExecutionContext for (M, I, O)
where
    M: Memory,
    I: InputStream,
    O: OutputStream,
{
}

// IO streams implementations
impl InputStream for Stdin {
    fn read(&mut self) -> Option<Int> {
        let mut line = String::new();
        self.read_line(&mut line).ok()?;
        line.trim().parse::<Int>().ok()
    }
}

impl OutputStream for Stdout {
    fn write(&mut self, value: Int) {
        use std::io::Write;
        writeln!(self, "{}", value).unwrap();
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

impl InputStream for () {
    fn read(&mut self) -> Option<Int> {
        None
    }
}

impl OutputStream for () {
    fn write(&mut self, _: Int) {}
}
