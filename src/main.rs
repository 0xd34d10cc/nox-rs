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

#[derive(StructOpt)]
#[structopt(name = "nox", about = "yet another toy language")]
enum Opts {
    Compile {
        #[structopt(parse(from_os_str))]
        file: PathBuf,

        #[structopt(short = "t", long = "target", possible_values = &Target::variants(), case_insensitive = true, default_value = "asm")]
        target: Target,
    },
    Interpret,
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

fn main() -> Result<(), Box<dyn Error>> {
    let result = match Opts::from_args() {
        Opts::Compile { file, target } => compile(&file, target),
        Opts::Interpret => Interpreter::new().run(),
    };

    if let Err(e) = result {
        eprintln!("Failure: {}", e);
    }

    Ok(())
}
