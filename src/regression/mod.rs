use std::cell::RefCell;
use std::rc::Rc;

mod bad_programs;
mod expr;
mod simple_statements;

#[cfg(feature = "deep-expressions")]
mod deep_expressions;
#[cfg(feature = "generated")]
mod generated;

use crate::io::{InputStream, OutputStream};
use crate::memory::ScopedMemory;
// use crate::jit::{self, Runtime};
use crate::sm;
use crate::statement;
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
        let program = statement::compile(program.trim()).unwrap();
        let mut memory = ScopedMemory::new();
        let mut input = inputs.clone();
        let mut output = Vec::new();
        statement::run(&program, &mut memory, &mut input, &mut output).unwrap();
        (input, output, program)
    };
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);

    // stack machine
    let (i, o, _program) = {
        let program = sm::compile(&program).unwrap();
        let mut memory = ScopedMemory::new();
        let mut input = inputs;
        let mut output = Vec::new();
        sm::run(&program, &mut memory, &mut input, &mut output).unwrap();
        (input, output, program)
    };
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);

    // JIT
    // let (i, o) = {
    //     let mut inputs = SharedInput::new(inputs);
    //     let mut outputs = SharedOutput::new(Vec::new());
    //     let mut rt = Runtime::new(Box::new(inputs.clone()), Box::new(outputs.clone()));
    //     let mut memory = Memory::new();
    //     let program = jit::compile(&program, &mut *rt, &mut memory).unwrap();
    //     let retcode = program.run();
    //     let i = inputs.take();
    //     let o = outputs.take();
    //     assert_eq!(retcode, 0);
    //     (i, o)
    // };
    // assert_eq!(stdin.len() - i.len(), reads);
    // assert_eq!(o, stdout);
}
