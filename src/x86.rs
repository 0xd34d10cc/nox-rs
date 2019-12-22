use std::collections::HashSet;
use std::error::Error;
use std::fmt::{self, Display};

use crate::sm;
use crate::types::{Int, Var};

#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Register {
    EBX = 0,
    ECX = 1,
    ESI = 2,
    EDI = 3,
    EAX = 4,
    EDX = 5,
    EBP = 6,
    ESP = 7,
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            Register::EBX => "ebx",
            Register::ECX => "ecx",
            Register::ESI => "esi",
            Register::EDI => "edi",
            Register::EAX => "eax",
            Register::EDX => "edx",
            Register::EBP => "ebp",
            Register::ESP => "esp"
        };

        write!(f, "{}", name)
    }
}

impl Register {
    fn to_index(&self) -> usize {
        *self as u8 as usize
    }

    fn from_index(i: usize) -> Option<Register> {
        match i {
            0 => Some(Register::EBX),
            1 => Some(Register::ECX),
            2 => Some(Register::ESI),
            3 => Some(Register::EDI),
            4 => Some(Register::EAX),
            5 => Some(Register::EDX),
            6 => Some(Register::EBP),
            7 => Some(Register::ESP),
            _ => None,
        }
    }
}

type Offset = isize;

#[derive(Debug, Clone)]
enum Operand {
    Register(Register),
    Stack(Offset),
    Named(Var),
    Literal(Int),
}

impl Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Register(r) => write!(f, "%{}", r),
            Operand::Literal(n) => write!(f, "${}", n),
            _ => todo!()
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone)]
enum Instruction {
    Mov(Operand /* to */, Operand /* from */),
    Op(Op, Operand, Operand),
    Cmp(Operand, Operand),
    Div(Operand),
    Cltd,
    Set(String, String),
    Push(Operand),
    Pop(Operand),
    Call(Var),
    Ret,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Mov(dst, src) => write!(f, "movl {}, {}", src, dst),
            Instruction::Push(op) => write!(f, "pushl {}", op),
            Instruction::Call(function) => write!(f, "call {}", function),
            Instruction::Ret => write!(f, "ret"),
            _ => todo!()
        }
    }
}

#[derive(Debug, Default)]
pub struct Program {
    text: Vec<Instruction>,
    globals: HashSet<Var>,
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "\t.globl main")?;

        // declare global variables
        for var in self.globals.iter() {
            writeln!(f, "\t.global_{}", var)?;
        }

        // actual program text
        writeln!(f, "\t.text")?;
        writeln!(f, "main:")?;

        for instruction in self.text.iter() {
            writeln!(f, "\t{}", instruction)?;
        }

        Ok(())

    }
}

// number of registers that we're allowed to use
const NUM_REGISTERS: usize = 3;
const WORD: usize = 4;

#[derive(Debug, Default)]
pub struct Compiler {
    free_registers: HashSet<Register>,
    stack: Vec<Operand>, // this is *symbolic* stack, it can contain registers
}

impl Compiler {
    pub fn new() -> Self {
        let mut c = Compiler::default();
        for i in 0..NUM_REGISTERS {
            c.free_registers.insert(Register::from_index(i).unwrap());
        }
        c
    }

    fn push(&mut self) -> Operand {
        // allocate register if it is free
        let op = match self.free_registers.iter().next() {
            Some(register) => Operand::Register(*register),
            None => match self.stack.last().unwrap() {
                Operand::Stack(offset) => Operand::Stack(offset + 1),
                _ => Operand::Stack(0)
            }
        };

        if let Operand::Register(r) = op {
            debug_assert!(self.free_registers.contains(&r));
            self.free_registers.remove(&r);
        }

        self.stack.push(op.clone());
        op
    }

    fn pop(&mut self) -> Option<Operand> {
        let op = self.stack.pop();
        if let Some(Operand::Register(r)) = op {
            debug_assert!(!self.free_registers.contains(&r));
            self.free_registers.insert(r);
        }
        op
    }

    fn compile_instruction(&mut self, program: &mut Program, instruction: &sm::Instruction) -> Result<(), Box<dyn Error>> {
        match instruction {
            sm::Instruction::Const(n) => {
                let op = self.push();
                program.text.push(Instruction::Mov(op, Operand::Literal(*n)));
            }
            sm::Instruction::Write => {
                let op = self.pop().ok_or("Empty stack (write)")?;
                program.text.push(Instruction::Push(op));
                program.text.push(Instruction::Call("nox_rt_write".to_string()));
            }
            _ => todo!()
        };

        Ok(())
    }

    pub fn compile(&mut self, source: &sm::Program) -> Result<Program, Box<dyn Error>> {
        let mut program = Program::default();
        for instruction in source {
            self.compile_instruction(&mut program, instruction)?;
        }
        program.text.push(Instruction::Ret);

        Ok(program)
    }
}
