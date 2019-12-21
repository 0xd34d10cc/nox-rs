use std::collections::HashMap;

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
