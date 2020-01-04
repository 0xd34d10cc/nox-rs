use std::collections::HashSet;
use std::error::Error;
use std::fmt::{self, Display};

use crate::ops::{LogicOp, Op};
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

impl Register {
    fn is_volatile(&self) -> bool {
        match self {
            Register::EAX | Register::ECX | Register::EDX => true,
            _ => false,
        }
    }
}

impl Register {
    fn as_8bit(self) -> Option<&'static str> {
        match self {
            Register::EBX => Some("bl"),
            Register::ECX => Some("cl"),
            Register::ESI => None,
            Register::EDI => None,
            Register::EAX => Some("al"),
            Register::EDX => Some("dl"),
            Register::EBP => None,
            Register::ESP => None,
        }
    }
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
            Register::ESP => "esp",
        };

        write!(f, "{}", name)
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
            Operand::Stack(offset) => write!(f, "{}(%ebp)", -offset * WORD as isize),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Condition {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Eq,
    NotEq,
}

impl Condition {
    pub fn from_op(op: LogicOp) -> Option<Condition> {
        match op {
            LogicOp::Less => Some(Condition::Less),
            LogicOp::LessOrEqual => Some(Condition::LessOrEqual),
            LogicOp::Greater => Some(Condition::Greater),
            LogicOp::GreaterOrEqual => Some(Condition::GreaterOrEqual),
            LogicOp::Eq => Some(Condition::Eq),
            LogicOp::NotEq => Some(Condition::NotEq),
            LogicOp::And | LogicOp::Or => None,
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let suffix = match self {
            Condition::Less => "l",
            Condition::LessOrEqual => "le",
            Condition::Greater => "g",
            Condition::GreaterOrEqual => "ge",
            Condition::Eq => "e",
            Condition::NotEq => "ne",
        };

        write!(f, "{}", suffix)
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
    Cmp(Operand, Operand),
    Div(Operand),
    Cltd,
    Set(Condition, Register /* dst */),
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
            Instruction::Cmp(dst, src) => write!(f, "cmp  \t{}, {}", src, dst),
            Instruction::Div(right) => write!(f, "idivl\t{}", right),
            Instruction::Cltd => write!(f, "cltd"),
            Instruction::Set(condition, dst) => write!(
                f,
                "set{}b  \t%{}",
                condition,
                dst.as_8bit().unwrap_or_else(|| {
                    panic!("Failed to shrink {:?} to 8-bit", dst);
                })
            ),
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
    used_registers: Vec<Register>, // list of non-volatile registers which have been used at least once
    stack: Vec<Operand>,           // this is *symbolic* stack, it can contain registers
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            free_registers: vec![Register::EBX, Register::ECX, Register::ESI],
            used_registers: Vec::new(),
            stack: Vec::new(),
        }
    }

    fn allocate(&mut self) -> Operand {
        // allocate register if it is free
        let op = if let Some(r) = self.free_registers.pop() {
            Operand::Register(r)
        } else {
            match self.stack.last().unwrap() {
                Operand::Stack(offset) => Operand::Stack(offset + 1),
                _ => Operand::Stack(0),
            }
        };

        self.push(op.clone());
        op
    }

    fn push(&mut self, op: Operand) {
        if let Operand::Register(r) = op {
            self.free_registers.retain(|&register| register != r);

            if !r.is_volatile() && !self.used_registers.contains(&r) {
                self.used_registers.push(r);
            }
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

    fn compile_instruction(
        &mut self,
        program: &mut Program,
        instruction: &sm::Instruction,
    ) -> Result<(), Box<dyn Error>> {
        let comment = format!("{:?}", instruction);
        let text = &mut program.text;
        let mut push = |instruction| text.push((instruction, comment.clone()));
        match instruction {
            sm::Instruction::Label(_)
            | sm::Instruction::Jump(_)
            | sm::Instruction::JumpIfZero(_)
            | sm::Instruction::JumpIfNotZero(_) => todo!(),
            sm::Instruction::Const(n) => {
                let op = self.allocate();
                push(Instruction::Mov(op, Operand::Literal(*n)));
            }
            sm::Instruction::Write => {
                let op = self.pop().ok_or("Empty stack (write)")?;
                // write() opcode should be last in statement otherwise
                // stack values might change after "nox_rt_write" has returned the control,
                // because we're not saving volatile registers before call
                debug_assert!(self.stack.is_empty());
                push(Instruction::Push(op));
                push(Instruction::Call("nox_rt_write".to_string()));
                push(Instruction::Pop(Operand::Register(Register::EDI)))
            }
            sm::Instruction::Read => {
                let dst = self.allocate();
                push(Instruction::Call("nox_rt_read".to_string()));
                push(Instruction::Mov(dst, Operand::Register(Register::EAX)));
            }
            sm::Instruction::Load(var) => {
                let dst = self.allocate();
                if !program.globals.contains(var) {
                    return Err(
                        format!("Attempt to read from uninitialized variable: {}", var).into(),
                    );
                }
                if let Operand::Register(r) = dst {
                    push(Instruction::Mov(
                        Operand::Register(r),
                        Operand::Named(var.clone()),
                    ));
                } else {
                    // use EDI as temporary then...
                    push(Instruction::Mov(
                        Operand::Register(Register::EDI),
                        Operand::Named(var.clone()),
                    ));
                    push(Instruction::Mov(dst, Operand::Register(Register::EDI)));
                }
            }
            sm::Instruction::Store(var) => {
                let src = self.pop().ok_or("Empty stack (store)")?;
                program.globals.insert(var.clone());

                if let Operand::Stack(offset) = src {
                    // use EDI as temporary
                    push(Instruction::Mov(
                        Operand::Register(Register::EDI),
                        Operand::Stack(offset),
                    ));
                    push(Instruction::Mov(
                        Operand::Named(var.clone()),
                        Operand::Register(Register::EDI),
                    ));
                } else {
                    push(Instruction::Mov(Operand::Named(var.clone()), src));
                }
            }
            sm::Instruction::Op(Op::Div) => {
                let rhs = self.pop().ok_or("Empty stack (div, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (div, lhs)")?;
                let dst = self.allocate();

                push(Instruction::Mov(Operand::Register(Register::EAX), lhs));
                push(Instruction::Cltd); // sign-extend EAX to EDX:EAX
                push(Instruction::Div(rhs));
                push(Instruction::Mov(dst, Operand::Register(Register::EAX)));
            }
            sm::Instruction::Op(Op::Mod) => {
                // NOTE: copy-paste of Op::Div
                let rhs = self.pop().ok_or("Empty stack (div, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (div, lhs)")?;
                let dst = self.allocate();

                push(Instruction::Mov(Operand::Register(Register::EAX), lhs));
                push(Instruction::Cltd); // sign-extend EAX to EDX:EAX
                push(Instruction::Div(rhs));
                push(Instruction::Mov(dst, Operand::Register(Register::EDX)));
            }
            sm::Instruction::Op(op) => {
                let instruction = |lhs, rhs| match op {
                    Op::Add => Instruction::Add(lhs, rhs),
                    Op::Sub => Instruction::Sub(lhs, rhs),
                    Op::Mul => Instruction::Mul(lhs, rhs),
                    Op::Div | Op::Mod => unreachable!(), // handled above
                };
                let rhs = self.pop().ok_or("Empty stack (add, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (add, lhs)")?;
                match lhs {
                    Operand::Register(r) => {
                        push(instruction(Operand::Register(r), rhs));
                        self.push(Operand::Register(r));
                    }
                    lhs => {
                        // use EDI for operation then
                        let dst = self.allocate();
                        push(Instruction::Mov(Operand::Register(Register::EDI), lhs));
                        push(instruction(Operand::Register(Register::EDI), rhs));
                        push(Instruction::Mov(dst, Operand::Register(Register::EDI)));
                    }
                }
            }
            sm::Instruction::LogicOp(LogicOp::And) => {
                let rhs = self.pop().ok_or("Empty stack (and, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (and, lhs)")?;
                let dst = self.allocate();

                push(Instruction::Cmp(rhs, Operand::Literal(0)));
                push(Instruction::Mov(
                    Operand::Register(Register::EDX),
                    Operand::Literal(0),
                ));
                push(Instruction::Set(Condition::NotEq, Register::EDX));
                push(Instruction::Cmp(lhs, Operand::Literal(0)));
                push(Instruction::Mov(
                    Operand::Register(Register::EAX),
                    Operand::Literal(0),
                ));
                push(Instruction::Set(Condition::NotEq, Register::EAX));
                push(Instruction::And(
                    Operand::Register(Register::EDX),
                    Operand::Register(Register::EAX),
                ));
                push(Instruction::Mov(dst, Operand::Register(Register::EDX)));
            }
            sm::Instruction::LogicOp(LogicOp::Or) => {
                // NOTE: copy-paste of LogicOp::And
                let rhs = self.pop().ok_or("Empty stack (or, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (or, lhs)")?;
                let dst = self.allocate();

                push(Instruction::Cmp(rhs, Operand::Literal(0)));
                push(Instruction::Mov(
                    Operand::Register(Register::EDX),
                    Operand::Literal(0),
                ));
                push(Instruction::Set(Condition::NotEq, Register::EDX));
                push(Instruction::Cmp(lhs, Operand::Literal(0)));
                push(Instruction::Mov(
                    Operand::Register(Register::EAX),
                    Operand::Literal(0),
                ));
                push(Instruction::Set(Condition::NotEq, Register::EAX));
                push(Instruction::Or(
                    Operand::Register(Register::EDX),
                    Operand::Register(Register::EAX),
                ));
                push(Instruction::Mov(dst, Operand::Register(Register::EDX)));
            }
            sm::Instruction::LogicOp(op) => {
                let condition = Condition::from_op(*op).expect("Unexpected logic op");

                let rhs = self.pop().ok_or("Empty stack (comparison, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (comparison, lhs)")?;
                let dst = self.allocate();

                match (lhs, rhs) {
                    (Operand::Register(l), rhs) => {
                        push(Instruction::Cmp(Operand::Register(l), rhs))
                    }
                    (lhs, Operand::Register(r)) => {
                        push(Instruction::Cmp(lhs, Operand::Register(r)))
                    }
                    (lhs, rhs) => {
                        push(Instruction::Mov(Operand::Register(Register::EDI), lhs));
                        push(Instruction::Cmp(Operand::Register(Register::EDI), rhs));
                    }
                }
                push(Instruction::Mov(
                    Operand::Register(Register::EDX),
                    Operand::Literal(0),
                ));
                push(Instruction::Set(condition, Register::EDX));
                push(Instruction::Mov(dst, Operand::Register(Register::EDX)));
            }
        };

        Ok(())
    }

    pub fn compile(&mut self, source: &sm::Program) -> Result<Program, Box<dyn Error>> {
        let mut program = Program::default();
        // actual code
        for instruction in source.instructions() {
            self.compile_instruction(&mut program, instruction)?;
        }

        let mut prologue = Vec::new();
        // generate prologue
        for register in self.used_registers.iter() {
            prologue.push((
                Instruction::Push(Operand::Register(*register)),
                "prologue".into(),
            ));
        }

        prologue.push((
            Instruction::Push(Operand::Register(Register::EBP)),
            "prologue".into(),
        ));
        prologue.push((
            Instruction::Mov(
                Operand::Register(Register::EBP),
                Operand::Register(Register::ESP),
            ),
            "prologue".into(),
        ));

        prologue.append(&mut program.text);
        program.text = prologue;

        for register in self.used_registers.iter().rev() {
            program.text.push((
                Instruction::Pop(Operand::Register(*register)),
                "epilogue".into(),
            ));
        }

        // generate epilogue
        program.text.push((
            Instruction::Pop(Operand::Register(Register::EBP)),
            "epilogue".into(),
        ));

        // return code
        program.text.push((
            Instruction::Mov(Operand::Register(Register::EAX), Operand::Literal(0)),
            "retcode".into(),
        ));
        program.text.push((Instruction::Ret, "retcode".into()));

        Ok(program)
    }
}
