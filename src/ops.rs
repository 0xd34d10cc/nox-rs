use snafu::Snafu;

use crate::types::Int;

#[derive(Debug, Snafu)]
enum Error {
    #[snafu(display("Attempt to divide by 0"))]
    DivisionByZero,

    #[snafu(display("Attempt to mod by 0"))]
    ModZero
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl Op {
    pub fn apply(self, lhs: Int, rhs: Int) -> Result<Int, &'static str> {
        let n = match self {
            Op::Add => lhs.wrapping_add(rhs),
            Op::Sub => lhs.wrapping_sub(rhs),
            Op::Mul => lhs.wrapping_mul(rhs),
            Op::Div => {
                if rhs == 0 {
                    return Err("Attempt to divide by 0");
                }

                lhs / rhs
            }
            Op::Mod => {
                if rhs == 0 {
                    return Err("Attempt to mod by 0");
                }

                lhs % rhs
            }
        };

        Ok(n)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LogicOp {
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Eq,
    NotEq,
    And,
    Or,
}

impl LogicOp {
    pub fn apply(self, lhs: Int, rhs: Int) -> bool {
        match self {
            LogicOp::Less => lhs < rhs,
            LogicOp::LessOrEqual => lhs <= rhs,
            LogicOp::Greater => lhs > rhs,
            LogicOp::GreaterOrEqual => lhs >= rhs,
            LogicOp::Eq => lhs == rhs,
            LogicOp::NotEq => lhs != rhs,
            LogicOp::And => lhs != 0 && rhs != 0,
            LogicOp::Or => lhs != 0 || rhs != 0,
        }
    }
}
