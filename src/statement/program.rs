use std::collections::HashSet;

use thiserror::Error;

use super::control_flow_pass::ControlFlowPass;
use super::init_pass::InitPass;
use crate::syntax::{self, Statement};
use crate::types::Var;

// TODO: move to separate file
#[derive(Debug, Error)]
pub enum Warning {
    #[error("Unused function '{0}'")]
    UnusedFunction(Var),

    #[error("Unused variable '{0}'")]
    UnusedVariable(Var),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum Error {
    #[error("No entry function")]
    NoEntryFunction,

    #[error("Reference to unknown variable '{0}'")]
    UndefinedVar(Var),

    #[error("Call to undefined function: {0}")]
    UndefinedFunction(Var),

    #[error("Reference to uninitialized variable '{0}'")]
    UninitializedVar(Var),

    #[error("Invalid number of arguments for '{name}' expected {expected} found {found}")]
    InvalidNumberOfArgs {
        name: Var,
        expected: usize,
        found: usize,
    },

    #[error("Not all control paths of function '{0}' return value")]
    NotAllControlPathsReturn(Var),

    #[error("Function {0} has return statement without value")]
    ReturnWithoutValue(Var),
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Function {
    pub name: Var,
    pub returns_value: bool,
    pub locals: Vec<Var>,
    pub args: Vec<Var>,
    pub body: Vec<Statement>,
}

pub struct Program {
    pub functions: Vec<Function>,

    /// name of 'main' function
    pub entry: Var,

    globals: HashSet<Var>,
}

impl Program {
    /// Create program from AST
    pub fn from(program: syntax::Program) -> Result<(Vec<Warning>, Self)> {
        Self::with_globals(program, std::iter::empty())
    }

    pub fn with_globals(
        program: syntax::Program,
        globals: impl Iterator<Item = Var>,
    ) -> Result<(Vec<Warning>, Self)> {
        let (warnings, functions, globals) = InitPass::new(&program, globals).run()?;
        let (warnings, fns) = ControlFlowPass::new(&program, functions, warnings).run()?;

        let mut functions = Vec::new();
        for f in program.functions {
            let returns_value = fns.contains(&f.name);
            functions.push(Function {
                name: f.name,
                returns_value,
                args: f.args,
                locals: f.locals,
                body: f.body,
            });
        }

        let program = Program {
            functions,
            entry: program.entry,
            globals,
        };

        Ok((warnings, program))
    }

    #[cfg(test)]
    pub fn from_main(
        main: Vec<Statement>,
        globals: impl Iterator<Item = Var>,
    ) -> Result<(Vec<Warning>, Self)> {
        let program = syntax::Program::from_main(main);
        Self::with_globals(program, globals)
    }

    /// Get function by name
    pub fn get(&self, function: &Var) -> Option<&Function> {
        self.functions.iter().find(|f| &f.name == function)
    }

    /// Get 'main' function
    pub fn entry(&self) -> Option<&Function> {
        self.get(&self.entry)
    }

    /// Returns iterator over functions (entry function is not included)
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        let entry = self.entry.clone();
        self.functions.iter().filter(move |f| f.name != entry)
    }

    pub fn globals(&self) -> impl Iterator<Item = &Var> {
        self.globals.iter()
    }
}
