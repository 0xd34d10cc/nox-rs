mod compiler;
mod machine;
mod program;

use crate::memory::ScopedMemory;
use crate::io::{InputStream, OutputStream};
use crate::types::Result;

use self::compiler::Compiler;
use self::machine::StackMachine;
pub use self::program::{Program, Instruction, Label};

pub fn compile(program: &crate::statement::Program) -> Result<Program> {
    Compiler::new().compile(program)
}

pub fn run<I, O>(program: &Program, memory: &mut ScopedMemory, input: &mut I, output: &mut O) -> Result<()>
where
    I: InputStream,
    O: OutputStream,
{
    StackMachine::new(memory, input, output).run(program)
}
