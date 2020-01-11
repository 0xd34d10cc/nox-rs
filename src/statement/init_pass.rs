use std::collections::{HashMap, HashSet};

use super::expr::Expr;
use super::program::{Error, Result, Warning};
use crate::syntax::{self, Statement};
use crate::types::Var;

struct VariableState {
    initialized: bool,
    referenced: bool,
}

impl VariableState {
    fn initialized() -> Self {
        VariableState {
            initialized: true,
            referenced: false,
        }
    }

    fn uninitialized() -> Self {
        VariableState {
            initialized: false,
            referenced: false,
        }
    }

    fn reference(&mut self) {
        self.referenced = true;
    }

    fn initialize(&mut self) {
        self.initialized = true;
    }
}

type Vars = HashMap<Var, VariableState>;

struct Symbols {
    globals: Vars,
    locals: Vec<Vars>,
}

impl Symbols {
    pub fn new(globals: impl Iterator<Item = Var>) -> Self {
        Symbols {
            globals: globals
                .map(|name| (name, VariableState::initialized()))
                .collect(),
            locals: Vec::new(),
        }
    }

    pub fn enter_scope(&mut self, args: &[Var], locals: &[Var]) {
        let args = args
            .iter()
            .cloned()
            .map(|arg| (arg, VariableState::initialized()));

        let locals = locals
            .iter()
            .cloned()
            .map(|local| (local, VariableState::uninitialized()));

        let local_names = args.chain(locals).collect();
        self.locals.push(local_names);
    }

    pub fn leave_scope(&mut self) {
        self.locals.pop();
    }

    pub fn globals(&self) -> &Vars {
        &self.globals
    }

    pub fn locals(&self) -> Option<&Vars> {
        self.locals.last()
    }

    fn storage_mut(&mut self, var: &Var) -> &mut Vars {
        match self.locals.last_mut() {
            Some(locals) if locals.contains_key(var) => locals,
            _ => &mut self.globals,
        }
    }

    pub fn initialize(&mut self, var: &Var) {
        let storage = self.storage_mut(var);
        if let Some(state) = storage.get_mut(var) {
            state.initialize()
        } else {
            storage.insert(var.clone(), VariableState::initialized());
        }
    }

    pub fn reference(&mut self, var: &Var) -> Result<()> {
        let state = self
            .storage_mut(var)
            .get_mut(var)
            .ok_or_else(|| Error::UndefinedVar(var.clone()))?;

        if !state.initialized {
            return Err(Error::UninitializedVar(var.clone()));
        }

        state.reference();
        Ok(())
    }
}

// check that all variables & functions are defined & initialized
pub struct InitPass<'a> {
    program: &'a syntax::Program,
    symbols: Symbols,
    checked_functions: HashMap<Var, bool /* returns value */>,
    warnings: Vec<Warning>,
}

impl InitPass<'_> {
    pub fn new<'a>(
        program: &'a syntax::Program,
        globals: impl Iterator<Item = Var>,
    ) -> InitPass<'a> {
        InitPass {
            program,
            symbols: Symbols::new(globals),
            checked_functions: HashMap::new(),
            warnings: Vec::new(),
        }
    }

    pub fn run(
        mut self,
    ) -> Result<(
        Vec<Warning>,
        HashMap<Var, bool /* should return value? */>,
        HashSet<Var>, /* globals */
    )> {
        let entry = self.program.entry().ok_or(Error::NoEntryFunction)?;
        self.check_function(entry)?;

        for function in self.program.functions.iter() {
            if !self.checked_functions.contains_key(&function.name) {
                self.check_function(function)?;

                self.warnings
                    .push(Warning::UnusedFunction(function.name.clone()));
            }
        }

        for (name, state) in self.symbols.globals() {
            if !state.referenced {
                self.warnings.push(Warning::UnusedVariable(name.clone()));
            }
        }

        let globals = self
            .symbols
            .globals
            .into_iter()
            .map(|(name, _state)| name)
            .collect();

        Ok((self.warnings, self.checked_functions, globals))
    }

    fn check_function(&mut self, function: &syntax::Function) -> Result<()> {
        if self.checked_functions.contains_key(&function.name) {
            // TODO: add check for duplicate function (with same name)
            return Ok(());
        }

        self.checked_functions.insert(function.name.clone(), false);

        self.symbols.enter_scope(&function.args, &function.locals);
        let has_returns = self.check_statements(&function.body);

        for (name, state) in self.symbols.locals().unwrap() {
            if !state.referenced {
                self.warnings.push(Warning::UnusedVariable(name.clone()));
            }
        }

        self.symbols.leave_scope();

        if has_returns? {
            let has_returns = self.checked_functions.get_mut(&function.name).unwrap();
            *has_returns = true;
        }

        Ok(())
    }

    fn check_statements(&mut self, statements: &[Statement]) -> Result<bool> {
        let mut has_return = false;
        for statement in statements {
            has_return |= self.check_statement(statement)?;
        }

        Ok(has_return)
    }

    fn check_call(&mut self, function: &Var, args: usize) -> Result<()> {
        let f = self
            .program
            .get(function)
            .ok_or_else(|| Error::UndefinedFunction(function.clone()))?;

        if f.args.len() != args {
            return Err(Error::InvalidNumberOfArgs {
                name: f.name.clone(),
                expected: f.args.len(),
                found: args,
            });
        }

        self.check_function(f)?;
        Ok(())
    }

    fn check_statement(&mut self, statement: &Statement) -> Result<bool> {
        match statement {
            Statement::Skip => { /* nothing to check */ }
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                self.check_expr(condition)?;
                self.check_statements(if_true)?;
                self.check_statements(if_false)?;
            }
            Statement::While { condition, body } => {
                self.check_expr(condition)?;
                self.check_statements(body)?;
            }
            Statement::DoWhile { body, condition } => {
                self.check_statements(body)?;
                self.check_expr(condition)?;
            }
            Statement::Assign(x, e) => {
                self.check_expr(e)?;
                self.symbols.initialize(x);
            }
            Statement::Read(x) => self.symbols.initialize(x),
            Statement::Write(e) => self.check_expr(e)?,
            Statement::Call { name, args } => {
                for arg in args {
                    self.check_expr(arg)?;
                }

                self.check_call(name, args.len())?;
            }
            Statement::Return(e) => {
                if let Some(e) = e {
                    self.check_expr(e)?;
                    return Ok(true);
                }
            }
        };

        Ok(false)
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Var(v) => self.symbols.reference(v)?,
            Expr::Const(_) => {}
            Expr::Op(_, lhs, rhs) | Expr::LogicOp(_, lhs, rhs) => {
                self.check_expr(&*lhs)?;
                self.check_expr(&*rhs)?;
            }
            Expr::Call(name, args) => {
                for arg in args {
                    self.check_expr(arg)?;
                }

                self.check_call(name, args.len())?;

                // since it is used inside expression this function has to return a value
                *self.checked_functions.get_mut(name).unwrap() = true;
            }
        };

        Ok(())
    }
}
