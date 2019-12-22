use std::collections::HashSet;
use std::error::Error;
use std::fmt::{self, Display};

use crate::ops::Op;
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
            Operand::Named(name) => write!(f, "global_{}", name),
            Operand::Stack(offset) => write!(f, "{}(ebp)", -offset * WORD as isize)
        }
    }
}

#[derive(Debug, Clone)]
enum Instruction {
    Mov(Operand /* dst */, Operand /* src */),
    Add(Operand, Operand),
    Sub(Operand, Operand),
    Mul(Operand, Operand),
    And(Operand, Operand),
    Or(Operand, Operand),
    Xor(Operand, Operand),
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
            Instruction::Mov(dst, src) => write!(f, "movl \t{}, {}", src, dst),
            Instruction::Add(dst, src) => write!(f, "addl \t{}, {}", src, dst),
            Instruction::Sub(dst, src) => write!(f, "subl \t{}, {}", src, dst),
            Instruction::Mul(dst, src) => write!(f, "imull\t{}, {}", src, dst),
            Instruction::And(dst, src) => write!(f, "andl \t{}, {}", src, dst),
            Instruction::Or(dst, src) => write!(f, "orl  \t{}, {}", src, dst),
            Instruction::Xor(dst, src) => write!(f, "xorl \t{}, {}", src, dst),
            Instruction::Cmp(dst, src) => write!(f, "cmp  \t{}, {}", src, dst),
            Instruction::Div(right) => write!(f, "idivl\t{}", right),
            Instruction::Cltd => write!(f, "cltd"),
            Instruction::Set(what, where_) => write!(f, "set  \t{}, {}", what, where_),
            Instruction::Push(op) => write!(f, "pushl\t{}", op),
            Instruction::Pop(op) => write!(f, "popl \t{}", op),
            Instruction::Call(function) => write!(f, "call \t{}", function),
            Instruction::Ret => write!(f, "ret"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Program {
    text: Vec<(Instruction, String)>,
    globals: HashSet<Var>,
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, ".globl main")?;


        // declare global variables
        writeln!(f, ".data")?;
        for var in self.globals.iter() {
            writeln!(f, "\tglobal_{}:\t.zero 4", var)?;
        }

        // actual program text
        writeln!(f, ".text")?;
        writeln!(f, "main:")?;

        for (instruction, comment) in self.text.iter() {
            writeln!(f, "\t{}\t# {}", instruction, comment)?;
        }

        Ok(())

    }
}

const WORD: usize = 4;

#[derive(Debug)]
pub struct Compiler {
    free_registers: Vec<Register>,
    stack: Vec<Operand>, // this is *symbolic* stack, it can contain registers
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            free_registers: vec![Register::EBX, Register::ECX, Register::ESI],
            stack: Vec::new()
        }
    }

    fn allocate(&mut self) -> Operand {
        // allocate register if it is free
        let op = if let Some(r) = self.free_registers.pop() {
            Operand::Register(r)
        }
        else {
            match self.stack.last().unwrap() {
                Operand::Stack(offset) => Operand::Stack(offset + 1),
                _ => Operand::Stack(0)
            }
        };

        self.push(op.clone());
        op
    }

    fn push(&mut self, op: Operand) {
        if let Operand::Register(taken) = op {
            self.free_registers.retain(|&r| r != taken);
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

    fn compile_instruction(&mut self, program: &mut Program, instruction: &sm::Instruction) -> Result<(), Box<dyn Error>> {
        let comment = format!("{:?}", instruction);
        match instruction {
            sm::Instruction::Const(n) => {
                let op = self.allocate();
                program.text.push((Instruction::Mov(op, Operand::Literal(*n)), comment));
            }
            sm::Instruction::Write => {
                let op = self.pop().ok_or("Empty stack (write)")?;
                program.text.push((Instruction::Push(op), comment.clone()));
                program.text.push((Instruction::Call("nox_rt_write".to_string()), comment.clone()));
                program.text.push((Instruction::Pop(Operand::Register(Register::EDI)), comment))
            },
            sm::Instruction::Read => {
                let dst = self.allocate();
                program.text.push((Instruction::Call("nox_rt_read".to_string()), comment.clone()));
                program.text.push((Instruction::Mov(dst, Operand::Register(Register::EAX)), comment));
            },
            sm::Instruction::Load(var) => {
                let dst = self.allocate();
                if !program.globals.contains(var) {
                    return Err(format!("Attempt to read from uninitialized variable: {}", var).into());
                }
                program.text.push((Instruction::Mov(dst, Operand::Named(var.clone())), comment));
            }
            sm::Instruction::Store(var) => {
                let src = self.pop().ok_or("Empty stack (store)")?;
                program.globals.insert(var.clone());
                program.text.push((Instruction::Mov(Operand::Named(var.clone()), src), comment));
            },
            sm::Instruction::Op(op) => {
                let instruction = |lhs, rhs| {
                    match op {
                        Op::Add => Instruction::Add(lhs, rhs),
                        Op::Sub => Instruction::Sub(lhs, rhs),
                        Op::Mul => Instruction::Mul(lhs, rhs),
                        _ => todo!()
                    }
                };
                let rhs = self.pop().ok_or("Empty stack (add, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (add, lhs)")?;
                match lhs {
                    Operand::Register(r) => {
                        program.text.push((instruction(Operand::Register(r), rhs), comment));
                        self.push(Operand::Register(r));
                    },
                    lhs => {
                        // use EDI for operation then
                        let dst = self.allocate();
                        program.text.push((Instruction::Mov(Operand::Register(Register::EDI), lhs), comment.clone()));
                        program.text.push((instruction(Operand::Register(Register::EDI), rhs), comment.clone()));
                        program.text.push((Instruction::Mov(dst, Operand::Register(Register::EDI)), comment));
                    }
                }
            }
            _ => todo!()
        };

        Ok(())
    }

    pub fn compile(&mut self, source: &sm::Program) -> Result<Program, Box<dyn Error>> {
        let mut program = Program::default();
        // generate prologue
        program.text.push((Instruction::Push(Operand::Register(Register::EBP)), "prologue".into()));
        program.text.push((Instruction::Mov(Operand::Register(Register::EBP), Operand::Register(Register::ESP)), "prologue".into()));

        // actual code
        for instruction in source {
            self.compile_instruction(&mut program, instruction)?;
        }

        // generate epilogue
        program.text.push((Instruction::Pop(Operand::Register(Register::EBP)), "epilogue".into()));

        // return code
        program.text.push((Instruction::Mov(Operand::Register(Register::EAX), Operand::Literal(0)), "retcode".into()));
        program.text.push((Instruction::Ret, "retcode".into()));

        Ok(program)
    }
}
