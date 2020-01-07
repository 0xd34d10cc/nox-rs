use crate::context::{InputStream, Memory, OutputStream};
use crate::expr::Expr;
use crate::types::{Int, Result, Var};

// abstract statement
#[derive(Debug, Clone)]
pub enum Statement {
    Skip,
    IfElse {
        condition: Expr,
        if_true: Vec<Statement>,
        if_false: Vec<Statement>,
    },
    While {
        condition: Expr,
        body: Vec<Statement>,
    },
    DoWhile {
        body: Vec<Statement>,
        condition: Expr,
    },
    Assign(Var, Expr),
    Read(Var),
    Write(Expr),
    Call {
        name: Var,
        args: Vec<Expr>,
    },
    Return(Option<Expr>),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Var,
    pub args: Vec<Var>,
    pub locals: Vec<Var>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub entry: Var, /* name of "main" function */
}

impl Program {
    #[cfg(test)]
    pub fn from_main(statements: Vec<Statement>) -> Self {
        Program {
            functions: vec![Function {
                name: "main".into(),
                args: Vec::new(),
                locals: Vec::new(),
                body: statements,
            }],
            entry: "main".into(),
        }
    }

    #[cfg(test)]
    pub fn compile(input: crate::nom::Input) -> Result<crate::typecheck::Program> {
        Program::parse(input)
            .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
            .and_then(|program| {
                let (_warnings, program) = crate::typecheck::check(program)?;
                Ok(program)
            })
    }

    #[cfg(test)]
    pub fn parse(input: crate::nom::Input) -> crate::nom::Result<Program> {
        crate::nom::parse("program", parse::program, input)
    }

    pub fn entry(&self) -> Option<&Function> {
        self.get(&self.entry)
    }

    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        let entry = self.entry.clone();
        self.functions.iter().filter(move |f| f.name != entry)
    }

    pub fn get(&self, function: &Var) -> Option<&Function> {
        self.functions.iter().find(|f| &f.name == function)
    }

    pub fn run<I, O>(
        &self,
        memory: &mut Memory,
        input: &mut I,
        output: &mut O,
    ) -> Result<Option<Int>>
    where
        I: InputStream,
        O: OutputStream,
    {
        ExecutionContext::new(self, memory, input, output).call(&self.entry, &[])
    }
}

enum Retcode {
    Finished,
    Return(Option<Int>),
}

pub struct ExecutionContext<'a, I, O> {
    program: &'a Program,
    memory: &'a mut Memory,
    input: &'a mut I,
    output: &'a mut O,
}

impl<I, O> ExecutionContext<'_, I, O>
where
    I: InputStream,
    O: OutputStream,
{
    pub fn new<'a>(
        program: &'a Program,
        memory: &'a mut Memory,
        input: &'a mut I,
        output: &'a mut O,
    ) -> ExecutionContext<'a, I, O> {
        ExecutionContext {
            program,
            memory,
            input,
            output,
        }
    }

    pub fn memory(&self) -> &Memory {
        &self.memory
    }

    pub fn call(&mut self, function: &Var, args: &[Int]) -> Result<Option<Int>> {
        let target = self
            .program
            .get(function)
            .ok_or_else(|| format!("Call to unknown function: {}", function))?;

        if args.len() != target.args.len() {
            return Err(format!(
                "Invalid number of arguments in call to {}: expected {} found {}",
                function,
                target.args.len(),
                args.len()
            )
            .into());
        }

        self.execute_function(&target, args)
    }

    fn execute_function(&mut self, target: &Function, args: &[Int]) -> Result<Option<Int>> {
        let local_names = target
            .args
            .iter()
            .chain(target.locals.iter())
            .cloned()
            .collect();
        self.memory.push_scope(local_names);

        for (name, value) in target.args.iter().zip(args.iter()) {
            self.memory.store(name, *value);
        }

        let e = match self.execute_all(&target.body)? {
            Retcode::Finished => None,
            Retcode::Return(e) => e,
        };
        self.memory.pop_scope();
        Ok(e)
    }

    fn execute_all(&mut self, statements: &[Statement]) -> Result<Retcode> {
        for statement in statements {
            if let Retcode::Return(e) = self.execute(statement)? {
                return Ok(Retcode::Return(e));
            }
        }

        Ok(Retcode::Finished)
    }

    fn execute(&mut self, statement: &Statement) -> Result<Retcode> {
        match statement {
            Statement::Skip => { /* do nothing */ }
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let c = condition.eval(self)?;
                if c != 0 {
                    return self.execute_all(if_true);
                } else {
                    return self.execute_all(if_false);
                };
            }
            Statement::While { condition, body } => {
                while condition.eval(self)? != 0 {
                    if let Retcode::Return(e) = self.execute_all(body)? {
                        return Ok(Retcode::Return(e));
                    }
                }
            }
            Statement::DoWhile { body, condition } => loop {
                if let Retcode::Return(e) = self.execute_all(body)? {
                    return Ok(Retcode::Return(e));
                }

                if condition.eval(self)? == 0 {
                    break;
                }
            },
            Statement::Assign(name, value) => {
                let value = value.eval(self)?;
                self.memory.store(name, value);
            }
            Statement::Read(name) => {
                let value = self
                    .input
                    .read()
                    .ok_or_else(|| format!("Failed to read {}: no input", name))?;
                self.memory.store(name, value);
            }
            Statement::Write(expr) => {
                let value = expr.eval(self)?;
                self.output.write(value);
            }
            Statement::Call { name, args } => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.eval(self))
                    .collect::<Result<_>>()?;

                self.call(name, &args)?;
            }
            Statement::Return(e) => {
                let retval = if let Some(e) = e {
                    Some(e.eval(self)?)
                } else {
                    None
                };

                return Ok(Retcode::Return(retval));
            }
        };

        Ok(Retcode::Finished)
    }
}

pub mod parse {
    // Program ::= (Statements | Definition)*
    //
    // Definition ::= Function
    // Function ::= 'fun' Var '(' Var* ')' ['locals' Var*] '{' Statements '}'
    //
    // Statements ::= Statement (';' Statement)*
    // Statement ::= Skip | IfElse | While | For | Assign | Read | Write | Call
    // Skip ::= 'skip'
    // IfElse ::= 'if' Expr 'then' Statements  ('elif' Expr 'then' Statements)* ['else' Statements] 'fi'
    // While ::= 'while' Expr 'do' Statements 'od'
    // For ::= 'for' Statement ',' Expr ',' Statement 'do' Statements 'od'
    // RepeatUntil ::= 'repeat' Statements 'until' Expr
    // Assign ::= Var '=' Expr
    // Read ::= 'read(' Var ')'
    // Write ::= 'write(' Expr ')'
    // Call ::= Var '(' Expr* ')'
    // Var ::= Char+

    use super::{Expr, Function, Program, Var};
    use crate::expr::parse::expr;
    use crate::nom::{key, spaces, Input, Parsed};
    use crate::ops::LogicOp;
    use crate::types::parse::variable;

    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::combinator::{map, opt};
    use nom::multi::{many0, separated_list, separated_nonempty_list};
    use nom::sequence::{delimited, preceded};

    // local statement enum, for concrete syntax only
    #[derive(Debug, Clone)]
    enum Statement {
        Skip,
        IfElse {
            condition: Expr,
            if_true: Vec<Statement>,
            elifs: Vec<(Expr, Vec<Statement>)>,
            if_false: Vec<Statement>,
        },
        While {
            condition: Expr,
            body: Vec<Statement>,
        },
        For {
            init: Box<Statement>,
            condition: Expr,
            post_step: Box<Statement>,
            body: Vec<Statement>,
        },
        RepeatUntil {
            body: Vec<Statement>,
            condition: Expr,
        },
        Assign(Var, Expr),
        Read(Var),
        Write(Expr),
        Call {
            name: Var,
            args: Vec<Expr>,
        },
        Return(Option<Expr>),
    }

    fn convert(statements: Vec<Statement>) -> Vec<super::Statement> {
        let mut results = Vec::with_capacity(statements.len());
        for statement in statements {
            convert_into(statement, &mut results);
        }
        results
    }

    fn convert_into(statement: Statement, program: &mut Vec<super::Statement>) {
        match statement {
            Statement::Skip => program.push(super::Statement::Skip),
            Statement::IfElse {
                condition: root_condition,
                if_true,
                mut elifs,
                if_false,
            } => {
                // build if-else chain
                let if_else = if let Some((condition, body)) = elifs.pop() {
                    let mut root = super::Statement::IfElse {
                        condition,
                        if_true: convert(body),
                        if_false: convert(if_false),
                    };

                    for (condition, body) in elifs.into_iter().rev() {
                        root = super::Statement::IfElse {
                            condition,
                            if_true: convert(body),
                            if_false: vec![root],
                        };
                    }

                    super::Statement::IfElse {
                        condition: root_condition,
                        if_true: convert(if_true),
                        if_false: vec![root],
                    }
                } else {
                    // simple case
                    super::Statement::IfElse {
                        condition: root_condition,
                        if_true: convert(if_true),
                        if_false: convert(if_false),
                    }
                };

                program.push(if_else)
            }
            Statement::While { condition, body } => program.push(super::Statement::While {
                condition,
                body: convert(body),
            }),
            Statement::For {
                init,
                condition,
                post_step,
                mut body,
            } => {
                convert_into(*init, program);
                body.push(*post_step);
                program.push(super::Statement::While {
                    condition,
                    body: convert(body),
                })
            }
            Statement::RepeatUntil { body, condition } => program.push(super::Statement::DoWhile {
                body: convert(body),
                // until == while condition is false
                condition: Expr::LogicOp(
                    LogicOp::Eq,
                    Box::new(condition),
                    Box::new(Expr::Const(0)),
                ),
            }),
            Statement::Assign(to, from) => program.push(super::Statement::Assign(to, from)),
            Statement::Read(into) => program.push(super::Statement::Read(into)),
            Statement::Write(e) => program.push(super::Statement::Write(e)),
            Statement::Call { name, args } => program.push(super::Statement::Call { name, args }),
            Statement::Return(e) => program.push(super::Statement::Return(e)),
        }
    }

    pub fn program(input: Input) -> Parsed<Program> {
        let (input, mut fns) = many0(function)(input)?;
        let (input, main) = statements1(input)?;

        let entry: Var = "main".into();
        fns.push(Function {
            name: entry.clone(),
            args: Vec::new(),
            locals: Vec::new(),
            body: convert(main),
        });

        Ok((
            input,
            Program {
                functions: fns,
                entry,
            },
        ))
    }

    fn statements1(input: Input) -> Parsed<Vec<Statement>> {
        separated_nonempty_list(key(";"), statement)(input)
    }

    fn statements(input: Input) -> Parsed<Vec<Statement>> {
        separated_list(key(";"), statement)(input)
    }

    fn statement(input: Input) -> Parsed<Statement> {
        preceded(
            spaces,
            alt((
                skip,
                while_,
                for_,
                repeat_until,
                if_else,
                return_,
                assign,
                read,
                write,
                call,
            )),
        )(input)
    }

    fn skip(input: Input) -> Parsed<Statement> {
        map(tag("skip"), |_| Statement::Skip)(input)
    }

    fn while_(input: Input) -> Parsed<Statement> {
        let (input, condition) = preceded(key("while"), expr)(input)?;
        let (input, body) = delimited(key("do"), statements, key("od"))(input)?;
        Ok((input, Statement::While { condition, body }))
    }

    fn for_(input: Input) -> Parsed<Statement> {
        let (input, init) = preceded(key("for"), statement)(input)?;
        let (input, condition) = preceded(key(","), expr)(input)?;
        let (input, post_step) = preceded(key(","), statement)(input)?;
        let (input, body) = delimited(key("do"), statements, key("od"))(input)?;
        Ok((
            input,
            Statement::For {
                init: Box::new(init),
                condition,
                post_step: Box::new(post_step),
                body,
            },
        ))
    }

    fn repeat_until(input: Input) -> Parsed<Statement> {
        let (input, body) = preceded(key("repeat"), statements)(input)?;
        let (input, condition) = preceded(key("until"), expr)(input)?;
        Ok((input, Statement::RepeatUntil { body, condition }))
    }

    fn if_else(input: Input) -> Parsed<Statement> {
        let (input, root_condition) = preceded(key("if"), expr)(input)?;
        let (input, if_true) = preceded(key("then"), statements)(input)?;
        let (input, elifs) = many0(elif)(input)?;
        let (input, else_) = opt(else_)(input)?;
        let (input, _) = key("fi")(input)?;

        Ok((
            input,
            Statement::IfElse {
                condition: root_condition,
                if_true,
                elifs,
                if_false: else_.unwrap_or_default(),
            },
        ))
    }

    fn elif(input: Input) -> Parsed<(Expr, Vec<Statement>)> {
        let (input, condition) = preceded(key("elif"), expr)(input)?;
        let (input, body) = preceded(key("then"), statements)(input)?;
        Ok((input, (condition, body)))
    }

    fn else_(input: Input) -> Parsed<Vec<Statement>> {
        preceded(key("else"), statements)(input)
    }

    fn return_(input: Input) -> Parsed<Statement> {
        let (input, e) = preceded(key("return"), opt(expr))(input)?;
        Ok((input, Statement::Return(e)))
    }

    fn assign(input: Input) -> Parsed<Statement> {
        let (input, var) = variable(input)?;
        let (input, e) = preceded(key(":="), expr)(input)?;
        Ok((input, Statement::Assign(var, e)))
    }

    fn read(input: Input) -> Parsed<Statement> {
        let (input, _) = key("read")(input)?;
        let (input, var) = delimited(key("("), variable, key(")"))(input)?;
        Ok((input, Statement::Read(var)))
    }

    fn write(input: Input) -> Parsed<Statement> {
        let (input, _) = key("write")(input)?;
        let (input, e) = delimited(key("("), expr, key(")"))(input)?;
        Ok((input, Statement::Write(e)))
    }

    fn call(input: Input) -> Parsed<Statement> {
        let (input, name) = variable(input)?;
        let (input, args) = delimited(key("("), separated_list(key(","), expr), key(")"))(input)?;
        Ok((input, Statement::Call { name, args }))
    }

    fn function(input: Input) -> Parsed<Function> {
        let (input, name) = preceded(key("fun "), variable)(input)?;
        let (input, args) =
            delimited(key("("), separated_list(key(","), variable), key(")"))(input)?;
        let (input, locals) =
            opt(preceded(key("local"), separated_list(key(","), variable)))(input)?;
        let (input, body) = delimited(key("{"), statements, key("}"))(input)?;
        Ok((
            input,
            Function {
                name,
                args,
                locals: locals.unwrap_or_default(),
                body: convert(body),
            },
        ))
    }
}
