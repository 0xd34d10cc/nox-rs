mod compiler;
mod program;
mod runtime;

use self::compiler::Compiler;
pub use self::program::Program;
pub use self::runtime::Runtime;

use crate::memory::Memory;
use crate::types::Result;

pub fn compile<'a>(
    program: &crate::sm::Program,
    runtime: &'a mut Runtime,
    memory: &'a mut Memory,
) -> Result<Program<'a>> {
    Compiler::new(runtime, memory).compile(program)
}
