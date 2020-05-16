use std::rc::Rc;
use std::ops::Deref;
use std::fmt;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
pub type Int = i64;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Var(Rc<String>);

impl Deref for Var {
    type Target = str;

    fn deref(&self) -> &str {
        &*self.0
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Var({})", self.0)
    }
}

impl From<&str> for Var {
    fn from(name: &str) -> Var {
        Var(Rc::new(String::from(name)))
    }
}

impl From<String> for Var {
    fn from(name: String) -> Var {
        Var(Rc::new(name))
    }
}