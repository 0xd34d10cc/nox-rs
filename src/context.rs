use std::collections::{HashMap, HashSet};
use std::io::{self, Stdin, Stdout, Write};

use crate::types::{Int, Var};

pub trait Memory {
    fn load(&self, name: &Var) -> Option<Int>;
    fn store(&mut self, name: &Var, value: Int);
    fn scope(&mut self, vars: HashSet<Var>) -> Scope;
}

impl<M> Memory for &mut M where M: Memory {
    fn load(&self, name: &Var) -> Option<Int> {
        <M as Memory>::load(self, name)
    }

    fn store(&mut self, name: &Var, value: Int) {
        <M as Memory>::store(self, name, value)
    }

    fn scope(&mut self, vars: HashSet<Var>) -> Scope {
        <M as Memory>::scope(self, vars)
    }
}

pub struct Scope<'a> {
    globals: &'a mut dyn Memory,
    local_names: HashSet<Var>,
    locals: Env
}

impl Scope<'_> {
    pub fn new<'a>(globals: &'a mut dyn Memory, local_names: HashSet<Var>) -> Scope<'a> {
        Scope {
            globals,
            local_names,
            locals: Env::new()
        }
    }
}

impl<'a> Memory for Scope<'a> {
    fn load(&self, name: &Var) -> Option<Int> {
        if self.local_names.contains(name) {
            self.locals.get(name).copied()
        }
        else {
            self.globals.load(name)
        }
    }

    fn store(&mut self, name: &Var, value: Int) {
        if self.local_names.contains(name) {
            self.locals.insert(name.clone(), value);
        }
        else {
            self.globals.store(name, value);
        }
    }

    fn scope(&mut self, local_names: HashSet<Var>) -> Scope {
        Scope::new(self.globals, local_names)
    }
}

pub trait InputStream {
    fn read(&mut self) -> Option<Int>;
}

pub trait OutputStream {
    fn write(&mut self, value: Int);
}

pub type Env = HashMap<Var, Int>;

impl Memory for Env {
    fn load(&self, name: &Var) -> Option<Int> {
        self.get(name).copied()
    }

    fn store(&mut self, name: &Var, value: Int) {
        self.insert(name.clone(), value);
    }

    fn scope(&mut self, local_names: HashSet<Var>) -> Scope {
        Scope::new(self, local_names)
    }
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

impl InputStream for () {
    fn read(&mut self) -> Option<Int> {
        None
    }
}

impl OutputStream for () {
    fn write(&mut self, _: Int) {}
}
