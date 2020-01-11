use std::cell::RefCell;
use std::rc::Rc;

mod bad_programs;
mod expr;
mod simple_statements;

#[cfg(feature = "deep-expressions")]
mod deep_expressions;
#[cfg(feature = "generated")]
mod generated;

use crate::context::{InputStream, Memory, OutputStream};
// use crate::jit::{self, Runtime};
use crate::sm::{self, StackMachine};
use crate::typecheck;
use crate::types::Int;

#[derive(Clone)]
struct SharedInput {
    inner: Rc<RefCell<Vec<Int>>>,
}

#[allow(unused)]
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

#[allow(unused)]
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
    let inputs = stdin.iter().rev().cloned().collect::<Vec<Int>>();

    // statements
    let (i, o, program) = {
        let program = typecheck::Program::compile(program.trim()).unwrap();
        let mut memory = Memory::new();
        let mut input = inputs.clone();
        let mut output = Vec::new();
        program.run(&mut memory, &mut input, &mut output).unwrap();
        (input, output, program)
    };
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);

    // stack machine
    let (i, o, _program) = {
        let program = sm::compile(&program).unwrap();
        let mut memory = Memory::new();
        let mut input = inputs.clone();
        let mut output = Vec::new();
        let mut machine = StackMachine::new(&mut memory, &mut input, &mut output);
        machine.run(&program).unwrap();
        assert_eq!(machine.pop(), None);
        (input, output, program)
    };
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);

    // JIT
    // let (i, o) = {
    //     let mut inputs = SharedInput::new(inputs);
    //     let mut outputs = SharedOutput::new(Vec::new());
    //     let rt = Runtime::new(Box::new(inputs.clone()), Box::new(outputs.clone()));
    //     let program = jit::Compiler::new().compile(&program, rt).unwrap();
    //     let retcode = program.run();
    //     let i = inputs.take();
    //     let o = outputs.take();
    //     assert_eq!(retcode, 0);
    //     (i, o)
    // };
    // assert_eq!(stdin.len() - i.len(), reads);
    // assert_eq!(o, stdout);
}
