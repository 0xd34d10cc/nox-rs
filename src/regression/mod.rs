mod deep_expressions;
mod expr;
mod simple_statements;

#[cfg(feature = "generated")]
mod generated;

use crate::context::Env;
use crate::sm::{self, StackMachine};
use crate::statement;
use crate::types::Int;

pub fn run(program: &str, stdin: &[Int], stdout: &[Int], reads: usize) {
    let program = statement::parse(program.as_bytes()).unwrap();
    let inputs = stdin.iter().rev().cloned().collect::<Vec<Int>>();
    let mut context = (Env::new(), inputs.clone(), Vec::new());
    statement::run(&program, &mut context).unwrap();
    let (_, i, o) = context;
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);

    let program = sm::compile(&program);
    let mut context = (Env::new(), inputs, Vec::new());
    let mut machine = StackMachine::new(&mut context);
    machine.run(&program).unwrap();
    assert_eq!(machine.pop(), None);
    assert_eq!(stdin.len() - i.len(), reads);
    assert_eq!(o, stdout);
}
