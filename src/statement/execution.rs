use thiserror::Error;

use super::expr;
use super::{Function, Program};
use crate::io::{InputStream, OutputStream};
use crate::memory::{AllocationError, ScopedMemory};
use crate::syntax::Statement;
use crate::types::{Int, Var};

#[derive(Debug, Error)]
pub enum ExecutionError {
    #[error("Failed to evaluate expression: {0}")]
    Expression(#[from] expr::Error),

    #[error("Allocation failure: {0}")]
    AllocationFailure(#[from] AllocationError),

    #[error("Unexpected end of input while reading {0}")]
    UnexpectedEndOfInput(Var),
}

pub type ExecutionResult<T> = std::result::Result<T, ExecutionError>;

enum Retcode {
    Finished,
    Return(Option<Int>),
}

pub struct ExecutionContext<'a, I, O> {
    program: &'a Program,
    memory: &'a mut ScopedMemory,
    input: &'a mut I,
    output: &'a mut O,
}

impl<I, O> ExecutionContext<'_, I, O>
where
    I: InputStream,
    O: OutputStream,
{
    pub fn new<'a>(
        program: &'a Program,
        memory: &'a mut ScopedMemory,
        input: &'a mut I,
        output: &'a mut O,
    ) -> ExecutionContext<'a, I, O> {
        ExecutionContext {
            program,
            memory,
            input,
            output,
        }
    }

    pub fn memory(&self) -> &ScopedMemory {
        &self.memory
    }

    pub fn run(&mut self) -> ExecutionResult<Option<Int>> {
        for global in self.program.globals() {
            match self.memory.globals_mut().allocate(global.clone()) {
                Err(AllocationError::OutOfMemory) => {
                    return Err(AllocationError::OutOfMemory.into())
                }
                Err(AllocationError::AlreadyAllocated { .. }) | Ok(_) => { /* we're fine */ }
            }
        }

        self.call(&self.program.entry, &[])
    }

    pub fn call(&mut self, function: &Var, args: &[Int]) -> ExecutionResult<Option<Int>> {
        let target = self
            .program
            .get(function)
            .expect("Call to unknown function");

        debug_assert_eq!(args.len(), target.args.len());
        self.execute_function(&target, args)
    }

    fn execute_function(
        &mut self,
        target: &Function,
        args: &[Int],
    ) -> ExecutionResult<Option<Int>> {
        let local_names = target.args.iter().chain(target.locals.iter()).cloned();

        self.memory.push_scope(local_names);

        for (name, value) in target.args.iter().zip(args.iter()) {
            self.memory.store(name, *value);
        }

        let e = match self.execute_all(&target.body)? {
            Retcode::Finished => None,
            Retcode::Return(e) => e,
        };
        self.memory.pop_scope();
        Ok(e)
    }

    fn execute_all(&mut self, statements: &[Statement]) -> ExecutionResult<Retcode> {
        for statement in statements {
            if let Retcode::Return(e) = self.execute(statement)? {
                return Ok(Retcode::Return(e));
            }
        }

        Ok(Retcode::Finished)
    }

    fn execute(&mut self, statement: &Statement) -> ExecutionResult<Retcode> {
        match statement {
            Statement::Skip => { /* do nothing */ }
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let c = condition.eval(self)?;
                if c != 0 {
                    return self.execute_all(if_true);
                } else {
                    return self.execute_all(if_false);
                };
            }
            Statement::While { condition, body } => {
                while condition.eval(self)? != 0 {
                    if let Retcode::Return(e) = self.execute_all(body)? {
                        return Ok(Retcode::Return(e));
                    }
                }
            }
            Statement::DoWhile { body, condition } => loop {
                if let Retcode::Return(e) = self.execute_all(body)? {
                    return Ok(Retcode::Return(e));
                }

                if condition.eval(self)? == 0 {
                    break;
                }
            },
            Statement::Assign(name, value) => {
                let value = value.eval(self)?;
                self.memory.store(name, value);
            }
            Statement::Read(name) => {
                let value = self
                    .input
                    .read()
                    .ok_or_else(|| ExecutionError::UnexpectedEndOfInput(name.clone()))?;
                self.memory.store(name, value);
            }
            Statement::Write(expr) => {
                let value = expr.eval(self)?;
                self.output.write(value);
            }
            Statement::Call { name, args } => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.eval(self))
                    .collect::<expr::Result<_>>()?;

                self.call(name, &args)?;
            }
            Statement::Return(e) => {
                let retval = if let Some(e) = e {
                    Some(e.eval(self)?)
                } else {
                    None
                };

                return Ok(Retcode::Return(retval));
            }
        };

        Ok(Retcode::Finished)
    }
}
