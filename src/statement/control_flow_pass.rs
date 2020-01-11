use std::collections::{HashSet, HashMap};

use crate::types::Var;
use crate::syntax::{self, Statement};
use super::program::{Warning, Error, Result};

pub struct ControlFlowPass<'a> {
    program: &'a syntax::Program,
    functions: HashMap<Var, bool>,
    warnings: Vec<Warning>,
}

impl ControlFlowPass<'_> {
    pub fn new<'a>(
        program: &'a syntax::Program,
        functions: HashMap<Var, bool>,
        warnings: Vec<Warning>,
    ) -> ControlFlowPass<'a> {
        ControlFlowPass {
            program,
            functions: functions,
            warnings: warnings,
        }
    }

    pub fn run(mut self) -> Result<(Vec<Warning>, HashSet<Var>)> {
        for function in self.program.functions.iter() {
            self.check_function(function)?;
        }

        let functions = std::mem::replace(&mut self.functions, HashMap::new());
        let functions = functions
            .into_iter()
            .filter(|(_, should_return)| *should_return)
            .map(|(name, _)| name)
            .collect();

        Ok((self.warnings, functions))
    }

    fn check_function(&mut self, f: &syntax::Function) -> Result<()> {
        let should_return = self.functions.get(&f.name).copied().unwrap();
        let actually_returns = self.always_returns(&f.body);

        if !should_return && actually_returns {
            // this function was used only in *procedure* context,
            //  but it actually returns value
            *self.functions.get_mut(&f.name).unwrap() = true;
        }

        if should_return && !actually_returns {
            return Err(Error::NotAllControlPathsReturn(f.name.clone()));
        }

        self.validate_returns(&f.name, &f.body, actually_returns)?;

        Ok(())
    }

    fn always_returns(&self, block: &[Statement]) -> bool {
        for statement in block {
            match statement {
                Statement::IfElse {
                    if_true, if_false, ..
                } => {
                    if self.always_returns(&if_true) && self.always_returns(&if_false) {
                        return true;
                    }
                }
                Statement::Return(e) => return e.is_some(),
                Statement::DoWhile { body, .. } => {
                    if self.always_returns(&body) {
                        return true;
                    }
                }
                Statement::Assign(_, _)
                | Statement::While { .. }
                | Statement::Read(_)
                | Statement::Write(_)
                | Statement::Call { .. }
                | Statement::Skip => { /* can't return */ }
            }
        }

        return false;
    }

    fn validate_returns(&self, name: &str, block: &[Statement], should_return: bool) -> Result<()> {
        for statement in block {
            match statement {
                Statement::IfElse {
                    if_true, if_false, ..
                } => {
                    self.validate_returns(name, &if_true, should_return)?;
                    self.validate_returns(name, &if_false, should_return)?;
                }
                Statement::Return(e) => {
                    if e.is_some() != should_return {
                        if should_return {
                            return Err(Error::ReturnWithoutValue(Var::from(name)));
                        } else {
                            return Err(Error::NotAllControlPathsReturn(Var::from(name)));
                        }
                    }
                }
                Statement::DoWhile { body, .. } | Statement::While { body, .. } => {
                    self.validate_returns(name, &body, should_return)?;
                }
                Statement::Assign(_, _)
                | Statement::Read(_)
                | Statement::Write(_)
                | Statement::Call { .. }
                | Statement::Skip => { /* can't return */ }
            }
        }

        Ok(())
    }
}
