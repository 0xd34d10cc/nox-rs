use std::error::Error;
use std::io::{self, Stdin, Stdout};

use rustyline::error::ReadlineError;
use rustyline::Editor;
use thiserror::Error;

use crate::jit::{self, Runtime};
use crate::memory::ScopedMemory;
use crate::sm;
use crate::statement::{self, TypeError};
use crate::syntax::Program;
use crate::types::Var;

#[derive(Debug)]
pub enum Command {
    Delete(Var),
    ResetEnv,
    ShowEnv,

    ShowStatements(Program),
    RunStatements(Program),

    ShowSMInstructions(Program),
    RunSMInstructions(Program),

    ShowJITAsm(Program),
    RunJIT(Program),
}

impl Command {
    fn parse(line: &str) -> crate::syntax::Result<Command> {
        crate::syntax::parse("command", parse::input_line, line)
    }
}

#[derive(Debug, Error)]
pub enum CommandError {
    #[error("Parse error: {0}")]
    Parse(#[from] crate::syntax::Error),

    #[error("Compilation error: {0}")]
    Compilation(Box<dyn Error>),

    #[error("Type error: {0}")]
    Type(#[from] TypeError),

    #[error("Runtime error: {0}")]
    Runtime(Box<dyn Error>),
}

pub struct Interpreter {
    memory: ScopedMemory,
    input: Stdin,
    output: Stdout,
    runtime: Box<Runtime>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            memory: ScopedMemory::new(),
            input: io::stdin(),
            output: io::stdout(),
            runtime: Runtime::stdio(),
        }
    }

    pub fn execute(&mut self, line: Command) -> Result<(), CommandError> {
        match line {
            Command::Delete(var) => self.memory.globals_mut().deallocate(&var),
            Command::ResetEnv => self.memory.clear(),
            Command::ShowEnv => println!("{:?}", self.memory),
            Command::ShowStatements(p) => println!("{:#?}", p),
            Command::RunStatements(p) => {
                let (warnings, p) = statement::Program::from(p)?;
                if !warnings.is_empty() {
                    for warning in warnings {
                        println!("Warning: {}", warning);
                    }
                }

                let result =
                    statement::run(&p, &mut self.memory, &mut self.input, &mut self.output)
                        .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
                        .map_err(|e| CommandError::Runtime(e))?;

                if let Some(val) = result {
                    println!("Return code: {}", val);
                }
            }
            Command::ShowSMInstructions(p) => {
                let (warnings, p) = statement::Program::from(p)?;
                if !warnings.is_empty() {
                    for warning in warnings {
                        println!("Warning: {}", warning);
                    }
                }

                let p = sm::compile(&p).map_err(CommandError::Compilation)?;
                for instruction in p.instructions() {
                    println!("{:?}", instruction)
                }
            }
            Command::RunSMInstructions(p) => {
                let (warnings, p) = statement::Program::from(p)?;
                if !warnings.is_empty() {
                    for warning in warnings {
                        println!("Warning: {}", warning);
                    }
                }

                let p = sm::compile(&p).map_err(CommandError::Compilation)?;
                sm::run(&p, &mut self.memory, &mut self.input, &mut self.output)
                    .map_err(CommandError::Runtime)?
            }
            Command::ShowJITAsm(p) => {
                let (warnings, p) = statement::Program::from(p)?;
                if !warnings.is_empty() {
                    for warning in warnings {
                        println!("Warning: {}", warning);
                    }
                }

                let p = sm::compile(&p).map_err(CommandError::Compilation)?;
                let p = jit::compile(&p, &mut self.runtime, self.memory.globals_mut())
                    .map_err(CommandError::Compilation)?;

                for instruction in p.disassemble() {
                    println!("{}", instruction);
                }
            }
            Command::RunJIT(p) => {
                let (warnings, p) = statement::Program::from(p)?;
                if !warnings.is_empty() {
                    for warning in warnings {
                        println!("Warning: {}", warning);
                    }
                }

                let p = sm::compile(&p).map_err(CommandError::Compilation)?;
                let p = jit::compile(&p, &mut self.runtime, self.memory.globals_mut())
                    .map_err(CommandError::Compilation)?;

                let retcode = p.run();
                if retcode != 0 {
                    println!("Failure: {}", retcode);
                }
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

                    let result = Command::parse(line.as_str())
                        .map_err(CommandError::Parse)
                        .and_then(|command| self.execute(command));

                    if let Err(e) = result {
                        println!("{}", e);
                    }
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
    use crate::syntax::{program, spaces, variable, Input, Parsed};

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
