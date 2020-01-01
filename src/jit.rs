#[cfg(not(target_arch = "x86_64"))]
compile_error!("JIT is implemented only for x86-64");

use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::io::{self, Write};
use std::mem;

use capstone::prelude::*;
use dynasm::dynasm;
use dynasmrt::x64::Assembler;
use dynasmrt::{AssemblyOffset, DynasmApi, ExecutableBuffer};

use crate::ops::{LogicOp, Op};
use crate::sm;
use crate::types::{Int, Var};

pub struct Runtime {
    write: extern "win64" fn(Int),
    read: extern "win64" fn() -> Int,
}

impl Runtime {
    pub fn stdio() -> Self {
        extern "win64" fn rt_write(val: Int) {
            println!("O: {}", val);
        }

        extern "win64" fn rt_read() -> Int {
            print!("I: ");
            io::stdout().flush().ok();

            let mut line = String::new();
            let val = io::stdin()
                .read_line(&mut line)
                .ok()
                .and_then(|_| line.trim().parse::<Int>().ok());

            match val {
                None => -1,
                Some(v) => v,
            }
        }

        Runtime {
            write: rt_write,
            read: rt_read,
        }
    }
}

pub struct Program {
    memory: ExecutableBuffer,
    globals: Globals,
    entrypoint: AssemblyOffset,
}

impl Program {
    pub fn run(&self) -> i32 {
        let main_fn: extern "win64" fn() -> i32 =
            unsafe { mem::transmute(self.memory.ptr(self.entrypoint)) };

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

        let begin = self.memory.as_ptr() as usize;
        let entry = self.memory.ptr(self.entrypoint) as usize;
        debug_assert!(entry >= begin);

        let offset = entry - begin;
        let base_address = 0x1000;

        let instructions = cs
            .disasm_all(&self.memory[offset..], base_address)
            .expect("Failed to disassemble");

        instructions
            .iter()
            .map(|instruction| instruction.to_string())
            .collect()
    }

    pub fn globals(&self) -> &Globals {
        &self.globals
    }

    unsafe fn from_parts(
        memory: ExecutableBuffer,
        globals: Globals,
        entrypoint: AssemblyOffset,
    ) -> Self {
        Program {
            memory,
            globals,
            entrypoint,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(unused)]
enum Register {
    RAX = 0x00,
    RCX = 0x01,
    RDX = 0x02,
    RBX = 0x03,
    RSP = 0x04,
    RBP = 0x05,
    RSI = 0x06,
    RDI = 0x07,
    R8 = 0x08,
    R9 = 0x09,
    R10 = 0x0A,
    R11 = 0x0B,
    R12 = 0x0C,
    R13 = 0x0D,
    R14 = 0x0E,
    R15 = 0x0F,
}

type StackOffset = i32;
const WORD: i32 = 8;

#[derive(Debug, Clone, Copy)]
enum Operand {
    Register(Register),
    Stack(StackOffset),
}

impl Operand {
    fn store_const(self, c: Int, ops: &mut Assembler) {
        let c = c as i32; // TODO: check overflow
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov Rd(r as u8), c
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov DWORD [rsp - offset], c
            ),
        }
    }

    fn store_into(self, register: Register, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov Rd(register as u8), Rd(r as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov Rd(register as u8), [rsp - offset]
            ),
        }
    }

    fn load_from(self, register: Register, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov Rd(r as u8), Rd(register as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov [rsp - offset], Rd(register as u8)
            ),
        }
    }

    fn load_from_memory(self, location: *mut i32, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov Rd(r as u8), DWORD [location as _]
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov eax, DWORD [location as _]
                ; mov [rsp - offset], eax
            ),
        }
    }

    fn store_to_memory(self, location: *mut i32, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov DWORD [location as _], Rd(r as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov eax, [rsp - offset]
                ; mov DWORD [location as _], eax
            ),
        }
    }
}

#[derive(Default, Debug)]
pub struct Globals {
    values: Box<[i32]>,
    map: HashMap<Var, usize>,
}

impl fmt::Display for Globals {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "base: 0x{:x}", self.values.as_ptr() as usize)?;
        for (name, index) in self.map.iter() {
            writeln!(
                f,
                "{} @ 0x{:x} -> {}",
                name, &self.values[*index] as *const i32 as usize, self.values[*index]
            )?;
        }
        Ok(())
    }
}

impl Globals {
    fn allocate(program: &sm::Program) -> Result<Globals, Box<dyn Error>> {
        let mut globals = Globals::default();
        let mut index = 0;

        for instruction in program {
            match instruction {
                sm::Instruction::Store(var) => {
                    globals.map.insert(var.clone(), index);
                    index += 1;
                }
                sm::Instruction::Load(var) => {
                    if !globals.map.contains_key(var) {
                        return Err(format!(
                            "Attempt to read from uninitialized variable: {}",
                            var
                        )
                        .into());
                    }
                }
                _ => { /* ignore */ }
            }
        }

        globals.values = vec![0; index].into_boxed_slice();
        Ok(globals)
    }

    fn get_ptr(&mut self, var: &Var) -> Option<*mut i32> {
        let index = self.map.get(var)?;
        let ptr = &mut self.values[*index] as *mut i32;
        Some(ptr)
    }
}

struct CompilationContext {
    free_registers: Vec<Register>,
    stack: Vec<Operand>,
    globals: Globals,
}

impl CompilationContext {
    fn new(globals: Globals) -> Self {
        CompilationContext {
            free_registers: vec![Register::R8, Register::R9, Register::R10, Register::R11],
            stack: Vec::new(),
            globals,
        }
    }

    fn push(&mut self, op: Operand) {
        if let Operand::Register(r) = op {
            self.free_registers.retain(|&register| register != r);
        }

        self.stack.push(op);
    }

    fn pop(&mut self) -> Option<Operand> {
        let op = self.stack.pop();
        if let Some(Operand::Register(r)) = op {
            self.free_registers.push(r);
        }
        op
    }

    fn allocate(&mut self) -> Operand {
        let op = if let Some(r) = self.free_registers.pop() {
            Operand::Register(r)
        } else {
            match self.stack.last().expect("Empty stack (allocate)") {
                Operand::Stack(offset) => Operand::Stack(offset + WORD),
                _ => Operand::Stack(0),
            }
        };

        self.push(op);
        op
    }
}

pub struct Compiler {
    runtime: Runtime,
}

impl Compiler {
    pub fn new(runtime: Runtime) -> Self {
        Compiler { runtime }
    }

    fn div(&mut self, lhs: Operand, rhs: Operand, ops: &mut Assembler) {
        lhs.store_into(Register::RAX, ops);
        dynasm!(ops
            ; cdq // cltd is alias for cdq
        );
        match rhs {
            Operand::Register(r) => dynasm!(ops
                ; idiv Rd(r as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov ebx, [rsp - offset]
                ; idiv ebx
            ),
        }
    }

    // compile 'and' or 'or' operation
    fn and_or(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        is_and: bool,
        context: &mut CompilationContext,
        ops: &mut Assembler,
    ) {
        match (lhs, rhs) {
            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let (l, r) = (lhs as u8, rhs as u8);
                dynasm!(ops
                    ; test Rd(l), Rd(l)
                    ; setne Rb(l)
                    ; test Rd(r), Rd(r)
                    ; setne Rb(r)
                );

                if is_and {
                    dynasm!(ops
                        ; and Rd(l), Rd(r)
                    )
                } else {
                    dynasm!(ops
                        ; or Rd(l), Rd(r)
                    )
                }

                context.push(Operand::Register(lhs))
            }
            // TODO: optimize for more cases, this fallback should happen only if
            //       both operands are on stack
            _ => {
                lhs.store_into(Register::RAX, ops);
                rhs.store_into(Register::RBX, ops);
                dynasm!(ops
                    ; test eax, eax
                    ; setne al
                    ; test ebx, ebx
                    ; setne bl
                );

                if is_and {
                    dynasm!(ops
                        ; and eax, ebx
                    )
                } else {
                    dynasm!(ops
                        ; or eax, ebx
                    )
                }

                let dst = context.allocate();
                dst.load_from(Register::RAX, ops);
            }
        }
    }

    fn compile_instruction(
        &mut self,
        ops: &mut Assembler,
        context: &mut CompilationContext,
        instruction: &sm::Instruction,
    ) -> Result<(), Box<dyn Error>> {
        match instruction {
            sm::Instruction::Const(c) => {
                let dst = context.allocate();
                dst.store_const(*c, ops);
            }
            sm::Instruction::Write => {
                let src = context.pop().ok_or("Empty stack (write)")?;
                src.store_into(Register::RCX, ops);
                dynasm!(ops
                    ; mov rax, QWORD self.runtime.write as _
                    ; call rax
                )
            }
            sm::Instruction::Read => {
                let dst = context.allocate();
                dynasm!(ops
                    ; mov rax, QWORD self.runtime.read as _
                    ; call rax
                );
                dst.load_from(Register::RAX, ops);
            }
            sm::Instruction::Store(var) => {
                let src = context.pop().ok_or("Empty stack (store)")?;
                let dst = context
                    .globals
                    .get_ptr(var)
                    .ok_or("Failed to find global (store)")?;
                src.store_to_memory(dst, ops);
            }
            sm::Instruction::Load(var) => {
                let src = context
                    .globals
                    .get_ptr(var)
                    .ok_or("Failed to find global (load)")?;
                let dst = context.allocate();
                dst.load_from_memory(src, ops);
            }
            sm::Instruction::Op(Op::Div) => {
                let rhs = context.pop().ok_or("Empty stack (div, rhs)")?;
                let lhs = context.pop().ok_or("Empty stack (div, lhs)")?;
                let dst = context.allocate();
                self.div(lhs, rhs, ops);
                dst.load_from(Register::RAX, ops);
            }
            sm::Instruction::Op(Op::Mod) => {
                let rhs = context.pop().ok_or("Empty stack (mod, rhs)")?;
                let lhs = context.pop().ok_or("Empty stack (mod, lhs)")?;
                let dst = context.allocate();
                self.div(lhs, rhs, ops);
                dst.load_from(Register::RDX, ops);
            }
            sm::Instruction::Op(op) => {
                let compile_op = |lhs, rhs, ops: &mut Assembler| {
                    match rhs {
                        Operand::Register(rhs) => {
                            match op {
                                Op::Add => dynasm!(ops
                                    ; add Rd(lhs as u8), Rd(rhs as u8)
                                ),
                                Op::Sub => dynasm!(ops
                                    ; sub Rd(lhs as u8), Rd(rhs as u8)
                                ),
                                Op::Mul => dynasm!(ops
                                    ; imul Rd(lhs as u8), Rd(rhs as u8)
                                ),
                                Op::Div | Op::Mod => unreachable!(), // handled above
                            }
                        }
                        Operand::Stack(offset) => {
                            match op {
                                Op::Add => dynasm!(ops
                                    ; add Rd(lhs as u8), [rsp - offset]
                                ),
                                Op::Sub => dynasm!(ops
                                    ; sub Rd(lhs as u8), [rsp - offset]
                                ),
                                Op::Mul => dynasm!(ops
                                    ; imul Rd(lhs as u8), [rsp - offset]
                                ),
                                Op::Div | Op::Mod => unreachable!(), // handled above
                            }
                        }
                    }
                };

                let rhs = context.pop().ok_or("Empty stack (op, rhs)")?;
                let lhs = context.pop().ok_or("Empty stack (op, lhs)")?;
                match lhs {
                    Operand::Register(lhs) => {
                        compile_op(lhs, rhs, ops);
                        context.push(Operand::Register(lhs));
                    }
                    Operand::Stack(_) => {
                        lhs.store_into(Register::RAX, ops);
                        compile_op(Register::RAX, rhs, ops);
                        let dst = context.allocate();
                        dst.load_from(Register::RAX, ops);
                    }
                };
            }
            sm::Instruction::LogicOp(LogicOp::And) => {
                let rhs = context.pop().ok_or("Empty stack (and, rhs)")?;
                let lhs = context.pop().ok_or("Empty stack (and, lhs)")?;
                self.and_or(lhs, rhs, true /* is_and */, context, ops);
            }
            sm::Instruction::LogicOp(LogicOp::Or) => {
                let rhs = context.pop().ok_or("Empty stack (or, rhs)")?;
                let lhs = context.pop().ok_or("Empty stack (or, lhs)")?;
                self.and_or(lhs, rhs, false /* is_and */, context, ops);
            }
            sm::Instruction::LogicOp(op) => {
                let set = |dst: Register, ops: &mut Assembler| match op {
                    LogicOp::Less => dynasm!(ops
                        ; setl Rb(dst as u8)
                    ),
                    LogicOp::LessOrEqual => dynasm!(ops
                        ; setle Rb(dst as u8)
                    ),
                    LogicOp::Greater => dynasm!(ops
                        ; setg Rb(dst as u8)
                    ),
                    LogicOp::GreaterOrEqual => dynasm!(ops
                        ; setge Rb(dst as u8)
                    ),
                    LogicOp::Eq => dynasm!(ops
                        ; sete Rb(dst as u8)
                    ),
                    LogicOp::NotEq => dynasm!(ops
                        ; setne Rb(dst as u8)
                    ),
                    LogicOp::And | LogicOp::Or => unreachable!(), // covered above
                };

                let rhs = context.pop().ok_or("Empty stack (logic, rhs)")?;
                let lhs = context.pop().ok_or("Empty stack (logic, lhs)")?;
                let dst = context.allocate();

                match (lhs, rhs) {
                    (Operand::Register(lhs), Operand::Register(rhs)) => dynasm!(ops
                        ; cmp Rd(lhs as u8), Rd(rhs as u8)
                    ),
                    (Operand::Register(lhs), Operand::Stack(offset)) => dynasm!(ops
                        ; cmp Rd(lhs as u8), [rsp - offset]
                    ),
                    (Operand::Stack(offset), Operand::Register(rhs)) => dynasm!(ops
                        ; cmp [rsp - offset], Rd(rhs as u8)
                    ),
                    (Operand::Stack(lhs), Operand::Stack(rhs)) => dynasm!(ops
                        ; mov eax, [rsp - lhs]
                        ; cmp eax, [rsp - rhs]
                    ),
                }

                match dst {
                    Operand::Register(dst) => set(dst, ops),
                    Operand::Stack(_) => {
                        set(Register::RAX, ops);
                        dst.load_from(Register::RAX, ops);
                    }
                }
            }
        };
        Ok(())
    }

    pub fn compile(&mut self, program: &sm::Program) -> Result<Program, Box<dyn Error>> {
        let mut ops = Assembler::new().unwrap();

        let globals = Globals::allocate(program)?;
        let mut context = CompilationContext::new(globals);

        // prologue
        let entrypoint = ops.offset();
        dynasm!(ops
            ; push rbp
            ; mov rbp, rsp
        );

        // actual code
        for instruction in program {
            self.compile_instruction(&mut ops, &mut context, instruction)?;
        }

        // epilogue
        dynasm!(ops
            ; pop rbp
        );

        // retcode
        dynasm!(ops
            ; mov eax, 0
            ; ret
        );

        ops.commit()?;
        let memory = ops
            .finalize()
            .expect("finalize() shouldn't fail if commit() was called before");
        Ok(unsafe { Program::from_parts(memory, context.globals, entrypoint) })
    }
}
