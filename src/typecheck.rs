use std::collections::{HashMap, HashSet};

use crate::types::{Result, Var};
use crate::statement::{Program, Function, Statement};
use crate::expr::Expr;


type Vars = HashMap<Var, bool /* is_initialized */>;

struct Symbols {
    globals: Vars,
    locals: Vec<Vars>,
}

impl Symbols {
    pub fn new() -> Self {
        Symbols {
            globals: Vars::new(),
            locals: Vec::new()
        }
    }

    pub fn enter_scope(&mut self, args: &[Var], locals: &[Var]) {
        let local_names = args.iter().cloned().map(|arg| (arg, true))
            .chain(locals.iter().cloned().map(|local| (local, false)))
            .collect();
        self.locals.push(local_names);
    }

    pub fn leave_scope(&mut self) {
        self.locals.pop();
    }

    fn storage(&self, var: &Var) -> &Vars {
        match self.locals.last() {
            Some(locals) if locals.contains_key(var) => locals,
            _ => &self.globals
        }
    }

    fn storage_mut(&mut self, var: &Var) -> &mut Vars {
        match self.locals.last_mut() {
            Some(locals) if locals.contains_key(var) => locals,
            _ => &mut self.globals
        }
    }

    pub fn initialize(&mut self, var: &Var) {
        let storage = self.storage_mut(var);
        if let Some(i) = storage.get_mut(var) {
            *i = true;
        } else {
            storage.insert(var.clone(), true);
        }
    }

    pub fn reference(&self, var: &Var) -> Result<()> {
        // 1. check that it actually exist
        let init = self.storage(var)
            .get(var)
            .ok_or_else(|| format!("Reference of unknown variable: {}", var))?;

        if !init {
            return Err(format!(
                "Attempt to reference uninitialized variable: {}",
                var
            ).into());
        }

        Ok(())
    }
}

struct TypeChecker<'a> {
    program: &'a Program,
    symbols: Symbols,
    checked_functions: HashSet<Var>
}

impl TypeChecker<'_> {
    pub fn new<'a>(program: &'a Program) -> TypeChecker<'a> {
        TypeChecker {
            program,
            symbols: Symbols::new(),
            checked_functions: HashSet::new(),
        }
    }

    pub fn check(&mut self) -> Result<()> {
        for function in self.program.functions.iter() {
            self.check_function(function)?;
        }

        Ok(())
    }

    fn check_function(&mut self, function: &Function) -> Result<()> {
        if self.checked_functions.contains(&function.name) {
            return Ok(());
        }

        self.checked_functions.insert(function.name.clone());

        self.symbols.enter_scope(&function.args, &function.locals);
        let r = self.check_statements(&function.body);
        self.symbols.leave_scope();
        r
    }

    fn check_statements(&mut self, statements: &[Statement]) -> Result<()> {
        for statement in statements {
            self.check_statement(statement)?;
        }
        Ok(())
    }

    fn check_call(&self, function: &Var, args: usize) -> Result<()> {
        let f = self.program.get(function)
            .ok_or_else(|| format!("Call to unknown function: {}", function))?;

        if f.args.len() != args {
            return Err(format!("Invalid number of arguments in call to {}: expected {} found {}", function, f.args.len(), args).into());
        }

        Ok(())
    }

    fn check_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Skip => { /* nothing to check */ },
            Statement::IfElse {
                condition,
                if_true,
                if_false
            } => {
                self.check_expr(condition)?;
                self.check_statements(if_true)?;
                self.check_statements(if_false)?;
            },
            Statement::While { condition, body } => {
                self.check_expr(condition)?;
                self.check_statements(body)?;
            },
            Statement::DoWhile { body, condition } => {
                self.check_statements(body)?;
                self.check_expr(condition)?;
            },
            Statement::Assign(x, e) => {
                self.check_expr(e)?;
                self.symbols.initialize(x);
            }
            Statement::Read(x) => self.symbols.initialize(x),
            Statement::Write(e) => self.check_expr(e)?,
            Statement::Call { name, args } => {
                self.check_call(name, args.len())?;
                for arg in args {
                    self.check_expr(arg)?;
                }

                let f = self.program.get(name).unwrap(); // checked by check_call()
                self.check_function(f)?;
            },
            Statement::Return(e) => {
                if let Some(e) = e {
                    self.check_expr(e)?;
                }
            }
        };

        Ok(())
    }

    fn check_expr(&self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Var(v) => self.symbols.reference(v)?,
            Expr::Const(_) => {},
            Expr::Op(_, lhs, rhs) | Expr::LogicOp(_, lhs, rhs) => {
                self.check_expr(&*lhs)?;
                self.check_expr(&*rhs)?;
            },
            Expr::Call(name, args) => {
                for arg in args {
                    self.check_expr(arg)?;
                }

                self.check_call(name, args.len())?;
            }
        };

        Ok(())
    }
}

pub fn check(program: &Program) -> Result<()> {
    TypeChecker::new(program).check()
}