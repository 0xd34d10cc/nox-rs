use std::error::Error;

use crate::context::ExecutionContext;
use crate::expr::Expr;
use crate::types::Var;

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Var, Expr),
    Read(Var),
    Write(Var),
    Seq(Box<Statement>, Box<Statement>),
}

// parse statement
impl Statement {
    pub fn parse(input: &[u8]) -> Result<Statement, Box<dyn Error>> {
        todo!()
    }

    pub fn eval<C>(&self, context: &mut C)
    where
        C: ExecutionContext,
    {
        todo!()
    }
}
