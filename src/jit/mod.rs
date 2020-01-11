mod runtime;
mod program;
mod compiler;

pub use self::program::Program;
pub use self::runtime::Runtime;
use self::compiler::Compiler;

use crate::types::Result;
use crate::memory::Memory;

pub fn compile<'a>(program: &crate::sm::Program, runtime: &'a mut Runtime, memory: &'a mut Memory) -> Result<Program<'a>> {
    Compiler::new(runtime, memory).compile(program)
}