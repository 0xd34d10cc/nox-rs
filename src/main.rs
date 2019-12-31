mod context;
mod expr;
mod interpreter;
mod ops;
#[cfg(feature = "regression")]
mod regression;
mod sm;
mod statement;
mod types;
mod x86;

use std::error::Error;
use std::path::{Path, PathBuf};

use structopt::clap::arg_enum;
use structopt::StructOpt;

use crate::interpreter::Interpreter;

arg_enum! {
    #[derive(Debug, Clone, Copy)]
    enum Target {
        Statement,
        StackMachine,
        Asm
    }
}

#[derive(StructOpt, Debug)]
enum Command {
    Compile {
        #[structopt(parse(from_os_str))]
        file: PathBuf,

        #[structopt(short = "t", long = "target", possible_values = &Target::variants(), case_insensitive = true, default_value = "asm")]
        target: Target,
    },
    Interpret {
        #[structopt(parse(from_os_str))]
        file: PathBuf,
    },
    SM {
        #[structopt(parse(from_os_str))]
        file: PathBuf,
    },
}

#[derive(StructOpt, Debug)]
#[structopt(name = "nox", about = "yet another toy language")]
struct Opts {
    #[structopt(subcommand)]
    command: Option<Command>,
}

fn compile(file: &Path, target: Target) -> Result<(), Box<dyn Error>> {
    let program = std::fs::read_to_string(file)?;
    let program = statement::parse(program.as_bytes())?;
    if let Target::Statement = target {
        for statement in program {
            println!("{:?}", statement);
        }
        return Ok(());
    }

    let program = sm::compile(&program);
    if let Target::StackMachine = target {
        for instruction in program {
            println!("{:?}", instruction);
        }
        return Ok(());
    }

    let program = x86::Compiler::new().compile(&program)?;
    println!("{}", program);

    Ok(())
}

use crate::context::{Env, InputStream, OutputStream};
use crate::types::Int;
use std::io::Write;
struct TestInput;
impl InputStream for TestInput {
    fn read(&mut self) -> Option<Int> {
        print!("> ");
        std::io::stdout().flush().ok()?;
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).ok()?;
        line.trim().parse::<Int>().ok()
    }
}

struct TestOutput;
impl OutputStream for TestOutput {
    fn write(&mut self, val: Int) {
        println!("{}", val);
    }
}

fn interpret(file: &Path) -> Result<(), Box<dyn Error>> {
    let program = std::fs::read_to_string(file)?;
    let program = statement::parse(program.as_bytes())?;
    let mut context = (Env::new(), TestInput, TestOutput);
    statement::run(&program, &mut context)?;
    Ok(())
}

fn run_sm(file: &Path) -> Result<(), Box<dyn Error>> {
    let program = std::fs::read_to_string(file)?;
    let program = statement::parse(program.as_bytes())?;
    let program = sm::compile(&program);
    let context = (Env::new(), TestInput, TestOutput);
    let mut machine = sm::StackMachine::new(context);
    machine.run(&program)?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::from_args();

    let result = if let Some(command) = opts.command {
        match command {
            Command::Compile { file, target } => compile(&file, target),
            Command::Interpret { file } => interpret(&file),
            Command::SM { file } => run_sm(&file),
        }
    } else {
        Interpreter::new().run()
    };

    if let Err(e) = result {
        eprintln!("Failure: {}", e);
    }

    Ok(())
}
