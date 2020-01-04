use std::cell::RefCell;
use std::rc::Rc;

mod expr;
mod simple_statements;

#[cfg(feature = "deep-expressions")]
mod deep_expressions;
#[cfg(feature = "generated")]
mod generated;

use crate::context::{Env, InputStream, OutputStream};
use crate::jit::{self, Runtime};
use crate::sm::{self, StackMachine};
use crate::statement;
use crate::types::Int;

#[derive(Clone)]
struct SharedInput {
    inner: Rc<RefCell<Vec<Int>>>,
}

impl SharedInput {
    fn new(values: Vec<Int>) -> Self {
        SharedInput {
            inner: Rc::new(RefCell::new(values)),
        }
    }

    fn take(&mut self) -> Vec<Int> {
        std::mem::replace(&mut *self.inner.borrow_mut(), Vec::new())
    }
}

impl InputStream for SharedInput {
    fn read(&mut self) -> Option<Int> {
        self.inner.borrow_mut().read()
    }
}

#[derive(Clone)]
struct SharedOutput {
    inner: Rc<RefCell<Vec<Int>>>,
}

impl SharedOutput {
    fn new(values: Vec<Int>) -> Self {
        SharedOutput {
            inner: Rc::new(RefCell::new(values)),
        }
    }

    fn take(&mut self) -> Vec<Int> {
        std::mem::replace(&mut *self.inner.borrow_mut(), Vec::new())
    }
}

impl OutputStream for SharedOutput {
    fn write(&mut self, value: Int) {
        self.inner.borrow_mut().write(value)
    }
}

pub fn run(program: &str, stdin: &[Int], stdout: &[Int], reads: usize) {
    let program = statement::parse(program.trim().as_bytes()).unwrap();
    let inputs = stdin.iter().rev().cloned().collect::<Vec<Int>>();
    let mut context = (Env::new(), inputs.clone(), Vec::new());
    statement::run(&program, &mut context).unwrap();
    let (_, i, o) = context;
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);

    let program = sm::compile(&program);
    let mut context = (Env::new(), inputs.clone(), Vec::new());
    let mut machine = StackMachine::new(&mut context);
    machine.run(&program).unwrap();
    assert_eq!(machine.pop(), None);
    let (_, i, o) = context;
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);

    let mut inputs = SharedInput::new(inputs);
    let mut outputs = SharedOutput::new(Vec::new());
    let rt = Runtime::new(Box::new(inputs.clone()), Box::new(outputs.clone()));
    let program = jit::Compiler::new().compile(&program, rt).unwrap();
    let retcode = program.run();
    let i = inputs.take();
    let o = outputs.take();
    assert_eq!(retcode, 0);
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);
}
