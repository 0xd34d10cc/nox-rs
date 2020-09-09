use std::marker::PhantomData;
use std::mem;

use capstone::prelude::*;
use dynasmrt::{AssemblyOffset, ExecutableBuffer};

use super::runtime::Runtime;
use crate::memory::Memory;

pub struct Program<'a> {
    program: ExecutableBuffer,
    entrypoint: AssemblyOffset,

    // these fields are here only for memory safety gurantees
    _memory: PhantomData<&'a mut Memory>,
    _runtime: PhantomData<&'a mut Runtime>,
}

impl Program<'_> {
    pub fn run(&self) -> i64 {
        let main_fn: extern "win64" fn() -> i64 =
            unsafe { mem::transmute(self.program.ptr(self.entrypoint)) };

        main_fn()
    }

    pub fn disassemble(&self) -> Vec<String> {
        let cs = Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .syntax(arch::x86::ArchSyntax::Intel)
            .detail(true)
            .build()
            .expect("Failed to create Capstone object");

        let begin = self.program.as_ptr() as usize;
        let entry = self.program.ptr(self.entrypoint) as usize;
        debug_assert!(entry >= begin);

        let offset = entry - begin;
        let base_address = entry as u64;

        let instructions = cs
            .disasm_all(&self.program[offset..], base_address)
            .expect("Failed to disassemble");

        instructions
            .iter()
            .map(|instruction| instruction.to_string())
            .collect()
    }

    // Preconditions:
    // 1. The program is memory safe
    // 2. Entrypoint is offset in program buffer at which the main function is located
    // 3. memory - the only memory block which could be modified by program
    // 4. runtime - the runtime this program is linked to
    pub unsafe fn from_parts<'a>(
        program: ExecutableBuffer,
        entrypoint: AssemblyOffset,
        _memory: &'a mut Memory,
        _runtime: &'a mut Runtime,
    ) -> Program<'a> {
        Program {
            program,
            entrypoint,

            _memory: PhantomData,
            _runtime: PhantomData,
        }
    }
}
