use std::collections::{HashMap, HashSet};
use std::io::{self, Stdin, Stdout, Write};

use crate::types::{Int, Var};

type Vars = HashMap<Var, Int>;

#[derive(Debug)]
pub struct Memory {
    globals: Vars,
    locals: Vec<(HashSet<Var>, Vars)>
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            globals: Vars::new(),
            locals: Vec::new(),
        }
    }

    #[cfg(test)]
    pub fn globals(&self) -> &Vars {
        &self.globals
    }

    pub fn globals_mut(&mut self) -> &mut Vars {
        &mut self.globals
    }

    pub fn clear(&mut self) {
        self.globals.clear();
        self.locals.clear();
    }

    pub fn push_scope(&mut self, local_names: HashSet<Var>) {
        self.locals.push((local_names, Vars::new()));
    }

    pub fn pop_scope(&mut self) {
        self.locals.pop();
    }

    pub fn load(&self, name: &Var) -> Option<Int> {
        self.storage(name).get(name).copied()
    }

    pub fn store(&mut self, name: &Var, value: Int) {
        self.storage_mut(name).insert(name.clone(), value);
    }

    fn storage(&self, name: &Var) -> &Vars {
        match self.locals.last() {
            Some((local_names, ref vars)) if local_names.contains(name) => vars,
            _ => &self.globals
        }
    }

    fn storage_mut(&mut self, name: &Var) -> &mut Vars {
        match self.locals.last_mut() {
            Some((local_names, ref mut vars)) if local_names.contains(name) => vars,
            _ => &mut self.globals,
        }
    }
}

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
