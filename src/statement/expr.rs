use thiserror::Error;

use super::execution::{ExecutionContext, ExecutionError};
use crate::io::{InputStream, OutputStream};
use crate::ops::{self, LogicOp, Op};
use crate::types::{Int, Var};

#[derive(Debug, Error)]
pub enum Error {
    #[error("Operation error: {0}")]
    OperationError(#[from] ops::Error),

    #[error("Variable '{0}' is not defined")]
    UndefinedVar(Var),

    #[error("Call to procedure {0} inside expression")]
    CallToProcedure(Var),

    #[error("Execution failure: {0}")]
    Execution(#[from] Box<ExecutionError>),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    Const(Int),
    Op(Op, Box<Expr>, Box<Expr>),
    LogicOp(LogicOp, Box<Expr>, Box<Expr>),
    Call(Var, Vec<Expr>),
}

impl Expr {
    #[cfg(test)]
    pub fn parse(input: crate::syntax::Input) -> crate::syntax::Result<Expr> {
        crate::syntax::parse("expression", crate::syntax::expr, input)
    }

    pub fn eval<I, O>(&self, context: &mut ExecutionContext<'_, I, O>) -> Result<Int>
    where
        I: InputStream,
        O: OutputStream,
    {
        match self {
            Expr::Var(name) => {
                let val = context
                    .memory()
                    .load(name)
                    .ok_or_else(|| Error::UndefinedVar(name.clone()))?;
                Ok(val)
            }
            Expr::Const(v) => Ok(*v),
            Expr::Op(op, lhs, rhs) => {
                let left = lhs.eval(context)?;
                let right = rhs.eval(context)?;
                let v = op.apply(left, right)?;

                Ok(v)
            }
            Expr::LogicOp(op, lhs, rhs) => {
                let left = lhs.eval(context)?;
                let right = rhs.eval(context)?;
                let v = op.apply(left, right);
                Ok(Int::from(v))
            }
            Expr::Call(name, args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.eval(context))
                    .collect::<Result<_>>()?;

                let retval = context
                    .call(name, &args)
                    .map_err(Box::new)?
                    .ok_or_else(|| Error::CallToProcedure(name.clone()))?;

                Ok(retval)
            }
        }
    }
}

pub mod parse {}
