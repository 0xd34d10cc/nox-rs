use std::error::Error;
use std::io::{self, Stdin, Stdout};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::context::Memory;
use crate::jit::{self, Runtime};
use crate::sm;
use crate::statement;
use crate::typecheck;
use crate::types::Var;

#[derive(Debug)]
pub enum Command {
    Delete(Var),
    ResetEnv,
    ShowEnv,

    ShowStatements(statement::Program),
    RunStatements(statement::Program),

    ShowSMInstructions(statement::Program),
    RunSMInstructions(statement::Program),

    ShowJITAsm(statement::Program),
    RunJIT(statement::Program),
}

impl Command {
    fn parse(line: &str) -> Result<Command, Box<dyn Error>> {
        let (rest, line) =
            parse::input_line(line).map_err(|e| crate::nom::format_err(e, "command", line))?;

        if !rest.is_empty() {
            return Err(format!(
                "Incomplete parse of command:\nParsed: {:?}\nRest: {}",
                line, rest
            )
            .into());
        }

        Ok(line)
    }
}

pub struct Interpreter {
    memory: Memory,
    input: Stdin,
    output: Stdout,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            memory: Memory::new(),
            input: io::stdin(),
            output: io::stdout(),
        }
    }

    pub fn execute(&mut self, line: Command) -> Result<(), Box<dyn Error>> {
        match line {
            Command::Delete(var) => {
                if self.memory.globals_mut().remove(&var).is_none() {
                    println!("No such variable: {}", var);
                }
            }
            Command::ResetEnv => self.memory.clear(),
            Command::ShowEnv => println!("{:?}", self.memory),
            Command::ShowStatements(p) => println!("{:#?}", p),
            Command::RunStatements(p) => {
                typecheck::check(&p)?;
                if let Some(val) = p.run(&mut self.memory, &mut self.input, &mut self.output)? {
                    println!("Return code: {}", val);
                }
            }
            Command::ShowSMInstructions(p) => {
                for instruction in sm::compile(&p)?.instructions() {
                    println!("{:?}", instruction)
                }
            }
            Command::RunSMInstructions(p) => {
                typecheck::check(&p)?;
                let p = sm::compile(&p)?;
                let mut machine =
                    sm::StackMachine::new(&mut self.memory, &mut self.input, &mut self.output);
                machine.run(&p)?;
            }
            Command::ShowJITAsm(p) => {
                let p = sm::compile(&p)?;
                let p = jit::Compiler::new().compile(&p, Runtime::stdio())?;
                println!("Memory map:\n{}", p.globals());
                for instruction in p.disassemble() {
                    println!("{}", instruction);
                }
            }
            Command::RunJIT(p) => {
                typecheck::check(&p)?;
                let p = sm::compile(&p)?;
                let p = jit::Compiler::new().compile(&p, Runtime::stdio())?;
                let retcode = p.run();
                if retcode != 0 {
                    println!("Failure: {}", retcode);
                }
                println!("Memory map after execution:\n{}", p.globals());
            }
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), Box<dyn Error>> {
        let mut rl = Editor::<()>::new();
        if rl.load_history("history.txt").is_err() {
            println!("No previous history.");
        }

        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) if line.trim().is_empty() => {}
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    match Command::parse(line.as_str()) {
                        Ok(line) => {
                            if let Err(e) = self.execute(line) {
                                println!("Failed to execute line: {}", e);
                            }
                        }
                        Err(e) => println!("{}", e),
                    };
                }
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }
        rl.save_history("history.txt").unwrap();

        Ok(())
    }
}

mod parse {
    // Input ::= Delete     | Reset     | ShowEnv | RunExpr
    //         | ShowExpr   | RunStmt   | ShowStmt
    //         | ShowSMInsn | RunSMInsn
    //         | ShowJITAsm | RunJIT
    // Delete ::= ':del' Var
    // Reset ::= ':reset'
    // ShowEnv ::= ':env'
    // RunExpr ::= ':re' Expr | Expr
    // ShowExpr ::= ':se' Expr
    // RunStmt ::= ':rs' Statements | Statements
    // ShowStmt ::= ':ss' Statements
    // ShowJITAsm ::= :sj Statements
    // RunJIT ::= :rj Statements

    use super::Command;
    use crate::nom::{spaces, Input, Parsed};
    use crate::statement::parse::program;
    use crate::types::parse::variable;

    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::combinator::map;
    use nom::sequence::preceded;

    pub fn input_line(input: Input) -> Parsed<Command> {
        alt((
            command(":del", map(variable, Command::Delete)),
            command(":reset", map(spaces, |_| Command::ResetEnv)),
            command(":env", map(spaces, |_| Command::ShowEnv)),
            command(":rsm", map(program, Command::RunSMInstructions)),
            command(":ssm", map(program, Command::ShowSMInstructions)),
            command(":rs", map(program, Command::RunStatements)),
            command(":ss", map(program, Command::ShowStatements)),
            command(":rj", map(program, Command::RunJIT)),
            command(":sj", map(program, Command::ShowJITAsm)),
            map(program, Command::RunStatements),
        ))(input)
    }

    fn command<'a, P>(prefix: &'a str, parser: P) -> impl Fn(Input<'a>) -> Parsed<Command>
    where
        P: Fn(Input<'a>) -> Parsed<Command>,
    {
        preceded(tag(prefix), parser)
    }
}
