use std::collections::{HashMap, HashSet};

use snafu::Snafu;

use crate::context::{InputStream, Memory, OutputStream};
use crate::expr::Expr;
use crate::statement::{self, Function, Statement};
use crate::types::{self, Int, Var};

#[derive(Debug, Snafu)]
pub enum Warning {
    #[snafu(display("Unused function '{}'", name))]
    UnusedFunction { name: Var },

    #[snafu(display("Unused variable '{}'", name))]
    UnusedVariable { name: Var },
}

#[derive(Debug, Snafu, PartialEq, Eq)]
pub enum Error {
    #[snafu(display("No entry function"))]
    NoEntryFunction,

    #[snafu(display("Reference to unknown variable '{}'", name))]
    UndefinedVar { name: Var },

    #[snafu(display("Call to undefined function: {}", name))]
    UndefinedFunction { name: Var },

    #[snafu(display("Reference to uninitialized variable '{}'", name))]
    UninitializedVar { name: Var },

    #[snafu(display(
        "Invalid number of arguments for '{}' expected {} found {}",
        name,
        expected,
        found
    ))]
    InvalidNumberOfArgs {
        name: Var,
        expected: usize,
        found: usize,
    },

    #[snafu(display("Not all control paths of function '{}' return value", name))]
    NotAllControlPathsReturn { name: Var },

    #[snafu(display("Function {} has return statement without value", name))]
    ReturnWithoutValue { name: Var },
}

type Result<T> = std::result::Result<T, Error>;

pub struct Program {
    program: statement::Program,

    #[allow(unused)]
    functions: HashSet<Var>, /* names of functions which return value on all paths */
}

impl Program {
    pub fn entry(&self) -> &Function {
        self.program.entry().unwrap() // typechecked
    }

    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.program.functions()
    }

    pub fn run<I, O>(
        &self,
        memory: &mut Memory,
        input: &mut I,
        output: &mut O,
    ) -> types::Result<Option<Int>>
    where
        I: InputStream,
        O: OutputStream,
    {
        self.program.run(memory, input, output)
    }
}

pub fn check(program: statement::Program) -> Result<(Vec<Warning>, Program)> {
    let (warnings, functions) = InitChecker::new(&program).check()?;
    let (warnings, functions) = ControlFlowChecker::new(&program, functions, warnings).check()?;

    let program = Program { program, functions };

    Ok((warnings, program))
}

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
    pub fn new() -> Self {
        Symbols {
            globals: Vars::new(),
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
            .ok_or_else(|| Error::UndefinedVar { name: var.clone() })?;

        if !state.initialized {
            return Err(Error::UninitializedVar { name: var.clone() });
        }

        state.reference();
        Ok(())
    }
}

// check that all variables & functions are defined & initialized
struct InitChecker<'a> {
    program: &'a statement::Program,
    symbols: Symbols,
    checked_functions: HashMap<Var, bool /* returns value */>,
    warnings: Vec<Warning>,
}

impl InitChecker<'_> {
    pub fn new<'a>(program: &'a statement::Program) -> InitChecker<'a> {
        InitChecker {
            program,
            symbols: Symbols::new(),
            checked_functions: HashMap::new(),
            warnings: Vec::new(),
        }
    }

    pub fn check(&mut self) -> Result<(Vec<Warning>, HashMap<Var, bool /* should value? */>)> {
        let entry = self.program.entry().ok_or(Error::NoEntryFunction)?;
        self.check_function(entry)?;

        for function in self.program.functions.iter() {
            if !self.checked_functions.contains_key(&function.name) {
                self.check_function(function)?;

                self.warnings.push(Warning::UnusedFunction {
                    name: function.name.clone(),
                });
            }
        }

        for (name, state) in self.symbols.globals() {
            if !state.referenced {
                self.warnings
                    .push(Warning::UnusedVariable { name: name.clone() });
            }
        }

        let functions = std::mem::replace(&mut self.checked_functions, HashMap::new());
        let warnings = std::mem::replace(&mut self.warnings, Vec::new());
        Ok((warnings, functions))
    }

    fn check_function(&mut self, function: &Function) -> Result<()> {
        if self.checked_functions.contains_key(&function.name) {
            // TODO: add check for duplicate function (with same name)
            return Ok(());
        }

        self.checked_functions.insert(function.name.clone(), false);

        self.symbols.enter_scope(&function.args, &function.locals);
        let has_returns = self.check_statements(&function.body);

        for (name, state) in self.symbols.locals().unwrap() {
            if !state.referenced {
                self.warnings
                    .push(Warning::UnusedVariable { name: name.clone() });
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
            .ok_or_else(|| Error::UndefinedFunction {
                name: function.clone(),
            })?;

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

struct ControlFlowChecker<'a> {
    program: &'a statement::Program,
    functions: HashMap<Var, bool>,
    warnings: Vec<Warning>,
}

impl ControlFlowChecker<'_> {
    fn new<'a>(
        program: &'a statement::Program,
        functions: HashMap<Var, bool>,
        warnings: Vec<Warning>,
    ) -> ControlFlowChecker<'a> {
        ControlFlowChecker {
            program,
            functions: functions,
            warnings: warnings,
        }
    }

    fn check(&mut self) -> Result<(Vec<Warning>, HashSet<Var>)> {
        for function in self.program.functions.iter() {
            self.check_function(function)?;
        }

        let functions = std::mem::replace(&mut self.functions, HashMap::new());
        let functions = functions
            .into_iter()
            .filter(|(_, should_return)| *should_return)
            .map(|(name, _)| name)
            .collect();

        let warnings = std::mem::replace(&mut self.warnings, Vec::new());
        Ok((warnings, functions))
    }

    fn check_function(&mut self, f: &Function) -> Result<()> {
        let should_return = self.functions.get(&f.name).copied().unwrap();
        let actually_returns = self.always_returns(&f.body);

        if !should_return && actually_returns {
            // this function was used only in *procedure* context,
            //  but it actually returns value
            *self.functions.get_mut(&f.name).unwrap() = true;
        }

        if should_return && !actually_returns {
            return Err(Error::NotAllControlPathsReturn {
                name: f.name.clone(),
            });
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
                            return Err(Error::ReturnWithoutValue {
                                name: Var::from(name),
                            });
                        } else {
                            return Err(Error::NotAllControlPathsReturn {
                                name: Var::from(name),
                            });
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
