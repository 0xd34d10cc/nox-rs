use std::error::Error;
use std::io::{self, Stdin, Stdout};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::context::Env;
use crate::expr::Expr;
use crate::jit::{self, Runtime};
use crate::sm;
use crate::statement::{self, Statement};
use crate::types::Var;
use crate::x86;

pub enum InputLine {
    Delete(Var),
    ResetEnv,
    ShowEnv,

    ShowExpr(Expr),
    RunExpr(Expr),

    ShowStatements(statement::Program),
    RunStatements(statement::Program),

    ShowSMInstructions(statement::Program),
    RunSMInstructions(statement::Program),

    ShowAsm(statement::Program),

    ShowJITAsm(statement::Program),
    RunJIT(statement::Program),
}

impl InputLine {
    fn parse(line: &str) -> Result<InputLine, Box<dyn Error>> {
        let (rest, line) = parse::input_line(line.as_bytes())
            .map_err(|e| format!("Failed to parse input line: {:?}", e))?;

        if !rest.is_empty() {
            return Err(format!("Incomplete parse: {}", std::str::from_utf8(rest).unwrap()).into());
        }

        Ok(line)
    }
}

pub struct Interpreter {
    context: (Env, Stdin, Stdout),
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: (Env::new(), io::stdin(), io::stdout()),
        }
    }

    pub fn execute(&mut self, line: InputLine) -> Result<(), Box<dyn Error>> {
        match line {
            InputLine::Delete(var) => {
                if self.context.0.remove(&var).is_none() {
                    println!("No such variable: {}", var);
                }
            }
            InputLine::ResetEnv => self.context.0.clear(),
            InputLine::ShowEnv => println!("{:?}", self.context.0),
            InputLine::ShowExpr(e) => println!("{:?}", e),
            InputLine::RunExpr(e) => println!("{}", e.eval(&self.context.0)?),
            InputLine::ShowStatements(p) => {
                for statement in p {
                    match statement {
                        Statement::IfElse { .. } | Statement::While { .. } => {
                            println!("{:#?}", statement)
                        }
                        _ => println!("{:?}", statement),
                    };
                }
            }
            InputLine::RunStatements(p) => statement::run(&p, &mut self.context)?,
            InputLine::ShowSMInstructions(p) => {
                for instruction in sm::compile(&p).instructions() {
                    println!("{:?}", instruction)
                }
            }
            InputLine::RunSMInstructions(p) => {
                let p = sm::compile(&p);
                let mut machine = sm::StackMachine::new(&mut self.context);
                machine.run(&p)?;
            }
            InputLine::ShowAsm(p) => {
                let p = sm::compile(&p);
                let p = x86::Compiler::new().compile(&p)?;
                println!("{}", p);
            }
            InputLine::ShowJITAsm(p) => {
                let p = sm::compile(&p);
                let p = jit::Compiler::new().compile(&p, Runtime::stdio())?;
                println!("Memory map:\n{}", p.globals());
                for instruction in p.disassemble() {
                    println!("{}", instruction);
                }
            }
            InputLine::RunJIT(p) => {
                let p = sm::compile(&p);
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
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    match InputLine::parse(line.as_str()) {
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
    //         | ShowSMInsn | RunSMInsn | ShowAsm
    //         | ShowJITAsm | RunJIT
    // Delete ::= ':del' Var
    // Reset ::= ':reset'
    // ShowEnv ::= ':env'
    // RunExpr ::= ':re' Expr | Expr
    // ShowExpr ::= ':se' Expr
    // RunStmt ::= ':rs' Statements | Statements
    // ShowStmt ::= ':ss' Statements
    // ShowAsm ::= ':asm' Statements
    // ShowJITAsm ::= :sj Statements
    // RunJIT ::= :rj Statements

    use super::*;
    use crate::expr::parse::expr;
    use crate::statement::parse::program;
    use crate::types::parse::variable;

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::combinator::map;
    use nom::sequence::preceded;
    use nom::IResult;

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
    }

    pub fn input_line(input: &[u8]) -> IResult<&[u8], InputLine> {
        alt((
            command(":del", map(variable, InputLine::Delete)),
            command(":reset", map(spaces, |_| InputLine::ResetEnv)),
            command(":env", map(spaces, |_| InputLine::ShowEnv)),
            command(":re", map(expr, InputLine::RunExpr)),
            command(":se", map(expr, InputLine::ShowExpr)),
            command(":rsm", map(program, InputLine::RunSMInstructions)),
            command(":ssm", map(program, InputLine::ShowSMInstructions)),
            command(":rs", map(program, InputLine::RunStatements)),
            command(":ss", map(program, InputLine::ShowStatements)),
            command(":asm", map(program, InputLine::ShowAsm)),
            command(":rj", map(program, InputLine::RunJIT)),
            command(":sj", map(program, InputLine::ShowJITAsm)),
            map(program, InputLine::RunStatements),
            map(expr, InputLine::RunExpr),
        ))(input)
    }

    fn command<'a, P>(
        prefix: &'a str,
        parser: P,
    ) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], InputLine>
    where
        P: Fn(&'a [u8]) -> IResult<&'a [u8], InputLine>,
    {
        preceded(tag(prefix), parser)
    }
}
