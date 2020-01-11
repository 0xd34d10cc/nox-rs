use crate::memory::ScopedMemory;
use crate::io::{InputStream, OutputStream};
use crate::types::{Int, Result};

use super::program::{Program, Labels, Instruction, Label};

type Stack = Vec<Int>;

pub struct StackMachine<'a, I, O> {
    memory: &'a mut ScopedMemory,
    input: &'a mut I,
    output: &'a mut O,
    stack: Stack,
    control_stack: Vec<Label /* return address */>,
}

enum Retcode {
    Continue,
    Jump(Label),
    Return,
}

impl<I, O> StackMachine<'_, I, O>
where
    I: InputStream,
    O: OutputStream,
{
    pub fn new<'a>(
        memory: &'a mut ScopedMemory,
        input: &'a mut I,
        output: &'a mut O,
    ) -> StackMachine<'a, I, O> {
        StackMachine {
            memory,
            input,
            output,
            stack: Stack::new(),
            control_stack: Vec::new(),
        }
    }

    pub fn push(&mut self, value: Int) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Int> {
        self.stack.pop()
    }

    fn execute(
        &mut self,
        instruction: &Instruction,
        labels: &Labels,
        pc: Label,
    ) -> Result<Retcode> {
        match instruction {
            Instruction::Label(_) => { /* ignore */ }
            Instruction::Jump(label) => {
                let location = labels.get(label).ok_or("Invalid label (jump)")?;
                return Ok(Retcode::Jump(*location));
            }
            Instruction::JumpIfZero(label) => {
                let v = self.pop().ok_or("Empty stack (jz)")?;
                if v == 0 {
                    let location = labels.get(label).ok_or("Invalid label (jz)")?;
                    return Ok(Retcode::Jump(*location));
                }
            }
            Instruction::JumpIfNotZero(label) => {
                let v = self.pop().ok_or("Empty stack (jnz)")?;
                if v != 0 {
                    let location = labels.get(label).ok_or("Invalid label (jnz)")?;
                    return Ok(Retcode::Jump(*location));
                }
            }
            Instruction::Op(op) => {
                let rhs = self.pop().ok_or("Empty stack (arithmetic, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (arithmetic, lhs)")?;
                let value = op.apply(lhs, rhs)?;
                self.push(value);
            }
            Instruction::LogicOp(op) => {
                let rhs = self.pop().ok_or("Empty stack (logic, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (logic, lhs)")?;
                let value = op.apply(lhs, rhs);
                self.push(Int::from(value));
            }
            Instruction::Const(n) => self.push(*n),
            Instruction::Read => {
                let value = self.input.read().ok_or("No input (read)")?;
                self.push(value);
            }
            Instruction::Write => {
                let value = self.pop().ok_or("Empty stack (write)")?;
                self.output.write(value);
            }
            Instruction::Load(var) => {
                let value = self
                    .memory
                    .load(var)
                    .ok_or_else(|| format!("Variable {} is not defined", var))?;
                self.push(value);
            }
            Instruction::Store(var) => {
                let value = self.pop().ok_or("Empty stack (store)")?;
                self.memory.store(var, value);
            }
            Instruction::Call(label) => {
                self.control_stack.push(pc + 1);
                let location = labels.get(label).ok_or("Invalid label (call)")?;
                return Ok(Retcode::Jump(*location));
            }
            Instruction::Begin { args, locals } => {
                let local_names = args.iter().chain(locals.iter()).cloned();
                self.memory.push_scope(local_names);
            }
            Instruction::End => {
                self.memory.pop_scope();
                match self.control_stack.pop() {
                    Some(location) => return Ok(Retcode::Jump(location)),
                    None => return Ok(Retcode::Return),
                }
            }
        };

        Ok(Retcode::Continue)
    }

    pub fn run(&mut self, program: &Program) -> Result<()> {
        use crate::memory::AllocationError;

        for global in program.globals() {
            match self.memory.globals_mut()
                .allocate(global.clone()) {
                Err(AllocationError::OutOfMemory) => return Err(AllocationError::OutOfMemory.into()),
                Err(AllocationError::AlreadyAllocated{ .. }) | Ok(_) => { /* we're fine */ }
            }
        }

        let mut pc = 0;
        while pc < program.instructions.len() {
            match self.execute(&program.instructions[pc], &program.labels, pc)? {
                Retcode::Continue => pc += 1,
                Retcode::Jump(location) => pc = location,
                Retcode::Return => return Ok(()),
            };
        }

        Ok(())
    }
}
