use std::collections::{HashMap, HashSet};
use std::io::{self, Stdin, Stdout, Write};
use std::iter::FromIterator;

use crate::types::{Int, Result, Var};

pub trait Memory {
    fn load(&self, name: &Var) -> Option<Int>;
    fn store(&mut self, name: &Var, value: Int);
    fn scope<F, L>(&mut self, locals: L, f: F) -> Result<()>
    where
        F: FnMut(&mut Self) -> Result<()>,
        L: IntoIterator<Item = Var>;
}

pub trait InputStream {
    fn read(&mut self) -> Option<Int>;
}

pub trait OutputStream {
    fn write(&mut self, value: Int);
}

type Vars = HashMap<Var, Int>;

#[derive(Default, Debug)]
pub struct Env {
    globals: Vars,
    locals: Vec<(Vars, HashSet<Var>)>,
}

impl Env {
    pub fn new() -> Self {
        Env::default()
    }

    #[cfg(test)]
    pub fn globals(&self) -> impl Iterator<Item = (&Var, &Int)> {
        self.globals.iter()
    }

    pub fn delete(&mut self, name: &Var) -> Option<Int> {
        self.storage_mut(name).remove(name)
    }

    pub fn clear(&mut self) {
        self.globals.clear();
        self.locals.clear();
    }

    fn storage_mut(&mut self, name: &Var) -> &mut Vars {
        match self.locals.last_mut() {
            Some((ref mut values, names)) if names.contains(name) => values,
            _ => &mut self.globals,
        }
    }

    fn storage(&self, name: &Var) -> &Vars {
        match self.locals.last() {
            Some((values, names)) if names.contains(name) => values,
            _ => &self.globals,
        }
    }
}

impl FromIterator<(Var, Int)> for Env {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (Var, Int)>,
    {
        let globals = iter.into_iter().collect();
        Env {
            globals,
            locals: Vec::new(),
        }
    }
}

impl Memory for Env {
    fn load(&self, name: &Var) -> Option<Int> {
        self.storage(name).get(name).copied()
    }

    fn store(&mut self, name: &Var, value: Int) {
        self.storage_mut(name).insert(name.clone(), value);
    }

    fn scope<F, L>(&mut self, locals: L, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
        L: IntoIterator<Item = Var>,
    {
        let local_names = locals.into_iter().collect();
        self.locals.push((Vars::new(), local_names));
        let r = f(self);
        self.locals.pop();
        r
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
