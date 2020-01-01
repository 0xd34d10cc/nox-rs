use std::error::Error;
use std::io::{self, Write};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::context::{Env, InputStream, OutputStream};
use crate::expr::Expr;
use crate::jit::{self, Runtime};
use crate::sm;
use crate::statement;
use crate::types::{Int, Var};
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

struct ProgramInput;
impl InputStream for ProgramInput {
    fn read(&mut self) -> Option<Int> {
        print!("I: ");
        io::stdout().flush().ok()?;

        let mut line = String::new();
        io::stdin().read_line(&mut line).ok()?;
        line.trim().parse::<Int>().ok()
    }
}

struct ProgramOutput;
impl OutputStream for ProgramOutput {
    fn write(&mut self, value: Int) {
        println!("O: {}", value);
    }
}

pub struct Interpreter {
    context: (Env, ProgramInput, ProgramOutput),
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: (Env::new(), ProgramInput, ProgramOutput),
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
                    println!("{:?}", statement);
                }
            }
            InputLine::RunStatements(p) => statement::run(&p, &mut self.context)?,
            InputLine::ShowSMInstructions(p) => {
                for instruction in sm::compile(&p) {
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
                let p = jit::Compiler::new(Runtime::stdio()).compile(&p)?;
                println!("Env:\n{}", p.globals());
                for instruction in p.disassemble() {
                    println!("{}", instruction);
                }
            }
            InputLine::RunJIT(p) => {
                let p = sm::compile(&p);
                let p = jit::Compiler::new(Runtime::stdio()).compile(&p)?;
                let retcode = p.run();
                if retcode != 0 {
                    println!("Failure: {}", retcode);
                }
                println!("Env:\n{}", p.globals());
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
    use nom::sequence::tuple;
    use nom::IResult;

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
    }

    pub fn input_line(input: &[u8]) -> IResult<&[u8], InputLine> {
        alt((
            delete,
            reset,
            show_env,
            run_expr,
            show_expr,
            show_sm_instructions,
            run_sm_instructions,
            run_statements,
            show_statements,
            show_asm,
            show_jit_asm,
            run_jit,
            just_statements,
            just_expression,
        ))(input)
    }

    fn delete(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, var)) = tuple((tag(":del"), spaces, variable))(input)?;
        Ok((rest, InputLine::Delete(var)))
    }

    fn reset(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, _) = tag(":reset")(input)?;
        Ok((rest, InputLine::ResetEnv))
    }

    fn show_env(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, _) = tag(":env")(input)?;
        Ok((rest, InputLine::ShowEnv))
    }

    fn run_expr(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, e)) = tuple((tag(":re"), spaces, expr))(input)?;
        Ok((rest, InputLine::RunExpr(e)))
    }

    fn just_expression(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, e) = expr(input)?;
        Ok((rest, InputLine::RunExpr(e)))
    }

    fn show_expr(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, e)) = tuple((tag(":se"), spaces, expr))(input)?;
        Ok((rest, InputLine::ShowExpr(e)))
    }

    fn run_statements(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, p)) = tuple((tag(":rs"), spaces, program))(input)?;
        Ok((rest, InputLine::RunStatements(p)))
    }

    fn just_statements(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, p) = program(input)?;
        Ok((rest, InputLine::RunStatements(p)))
    }

    fn show_statements(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, p)) = tuple((tag(":ss"), spaces, program))(input)?;
        Ok((rest, InputLine::ShowStatements(p)))
    }

    fn show_sm_instructions(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, p)) = tuple((tag(":ssm"), spaces, program))(input)?;
        Ok((rest, InputLine::ShowSMInstructions(p)))
    }

    fn run_sm_instructions(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, p)) = tuple((tag(":rsm"), spaces, program))(input)?;
        Ok((rest, InputLine::RunSMInstructions(p)))
    }

    fn show_asm(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, p)) = tuple((tag(":asm"), spaces, program))(input)?;
        Ok((rest, InputLine::ShowAsm(p)))
    }

    fn show_jit_asm(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, p)) = tuple((tag(":sj"), spaces, program))(input)?;
        Ok((rest, InputLine::ShowJITAsm(p)))
    }

    fn run_jit(input: &[u8]) -> IResult<&[u8], InputLine> {
        let (rest, (_, _, p)) = tuple((tag(":rj"), spaces, program))(input)?;
        Ok((rest, InputLine::RunJIT(p)))
    }
}
