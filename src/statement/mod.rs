use thiserror::Error;

mod control_flow_pass;
mod execution;
mod expr;
mod init_pass;
mod program;

use crate::io::{InputStream, OutputStream};
use crate::memory::ScopedMemory;
use crate::syntax;
use crate::types::Int;

use self::execution::{ExecutionContext, ExecutionResult};
pub use self::expr::Expr;
pub use self::program::{Error as TypeError, Function, Program};

#[derive(Debug, Error)]
pub enum CompilationError {
    #[error("Parsing failure: {0}")]
    Parse(#[from] syntax::Error),

    #[error("Type error: {0}")]
    Type(#[from] TypeError),
}

#[cfg(test)]
pub fn compile(program: &str) -> Result<Program, CompilationError> {
    let program = syntax::Program::parse(program)?;
    let (_warnings, program) = Program::from(program)?;
    Ok(program)
}

pub fn run<I, O>(
    program: &Program,
    memory: &mut ScopedMemory,
    input: &mut I,
    output: &mut O,
) -> ExecutionResult<Option<Int>>
where
    I: InputStream,
    O: OutputStream,
{
    ExecutionContext::new(program, memory, input, output).run()
}
