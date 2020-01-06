use crate::context::{InputStream, Memory, OutputStream};
use crate::expr::Expr;
use crate::types::{Result, Var};

// abstract statement
#[derive(Debug, Clone)]
pub enum Statement {
    Skip,
    IfElse {
        condition: Expr,
        if_true: Vec<Statement>,
        if_false: Vec<Statement>, // <- all elif's are here
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
    pub entry: usize, /* index of "main" function */
}

impl Program {
    #[cfg(test)]
    pub fn from_main(statements: Vec<Statement>) -> Self {
        Program {
            functions: vec![Function {
                name: "main".to_string(),
                args: Vec::new(),
                locals: Vec::new(),
                body: statements,
            }],
            entry: 0,
        }
    }

    #[cfg(test)]
    pub fn parse(input: &[u8]) -> Result<Program> {
        let (rest, program) = parse::program(input).map_err(|e| {
            format!(
                "Failed to parse statement {}: {:?}",
                String::from_utf8_lossy(input),
                e
            )
        })?;

        if !rest.is_empty() {
            return Err(format!(
                "Incomplete parse of statement\nProgram: {:?}\nOriginal: \n{}\nRest: \n{}",
                program,
                String::from_utf8_lossy(input),
                String::from_utf8_lossy(rest)
            )
            .into());
        }

        Ok(program)
    }

    pub fn run<M, I, O>(&self, memory: &mut M, input: &mut I, output: &mut O) -> Result<()>
    where
        M: Memory,
        I: InputStream,
        O: OutputStream,
    {
        let main = self
            .functions
            .get(self.entry)
            .ok_or("No main function found")?;

        self.execute_all(&main.body, memory, input, output)
    }

    fn execute_all<M, I, O>(
        &self,
        statements: &[Statement],
        memory: &mut M,
        input: &mut I,
        output: &mut O,
    ) -> Result<()>
    where
        M: Memory,
        I: InputStream,
        O: OutputStream,
    {
        for statement in statements {
            self.execute(statement, memory, input, output)?;
        }

        Ok(())
    }

    fn execute<M, I, O>(
        &self,
        statement: &Statement,
        memory: &mut M,
        input: &mut I,
        output: &mut O,
    ) -> Result<()>
    where
        M: Memory,
        I: InputStream,
        O: OutputStream,
    {
        match statement {
            Statement::Skip => { /* do nothing */ }
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let c = condition.eval(memory)?;
                if c != 0 {
                    self.execute_all(if_true, memory, input, output)?;
                } else {
                    self.execute_all(if_false, memory, input, output)?;
                }
            }
            Statement::While { condition, body } => {
                while condition.eval(memory)? != 0 {
                    self.execute_all(body, memory, input, output)?;
                }
            }
            Statement::DoWhile { body, condition } => loop {
                self.execute_all(body, memory, input, output)?;
                if condition.eval(memory)? == 0 {
                    break;
                }
            },
            Statement::Assign(name, value) => {
                let value = value.eval(memory)?;
                memory.store(name, value);
            }
            Statement::Read(name) => {
                let value = input
                    .read()
                    .ok_or_else(|| format!("Failed to read {}: no input", name))?;
                memory.store(name, value);
            }
            Statement::Write(expr) => {
                let value = expr.eval(memory)?;
                output.write(value);
            }
            Statement::Call { name, args } => {
                let target = self
                    .functions
                    .iter()
                    .find(|f| &f.name == name)
                    .ok_or_else(|| format!("Call to unknown function: {}", name))?;

                if target.args.len() != args.len() {
                    return Err(format!(
                        "Invalid number of arguments in call to {}: expected {} found {}",
                        name,
                        target.args.len(),
                        args.len()
                    )
                    .into());
                }

                let args: Vec<_> = args.iter().map(|arg| arg.eval(memory)).collect();
                let local_names = target.locals.iter().chain(target.args.iter()).cloned().collect();
                let mut scope = memory.scope(local_names);
                for (arg, value) in target.args.iter().zip(args.into_iter()) {
                    scope.store(arg, value?);
                }

                self.execute_all(&target.body, &mut scope, input, output)?;
            }
        };

        Ok(())
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
    use crate::ops::LogicOp;
    use crate::types::parse::variable;

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::combinator::{map, opt};
    use nom::multi::{many0, separated_list, separated_nonempty_list};
    use nom::sequence::{delimited, preceded};
    use nom::IResult;

    // local statement enum, for concrete syntax only
    #[derive(Debug, Clone)]
    enum Statement {
        Skip,
        IfElse {
            condition: Expr,
            if_true: Vec<Statement>,
            if_false: Vec<Statement>, // <- all elif's are here
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
    }

    fn spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
        take_while(|c| (c as char).is_whitespace())(input)
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
                condition,
                if_true,
                if_false,
            } => program.push(super::Statement::IfElse {
                condition,
                if_true: convert(if_true),
                if_false: convert(if_false),
            }),
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
        }
    }

    pub fn program(input: &[u8]) -> IResult<&[u8], Program> {
        let (input, mut fns) = many0(function)(input)?;
        let (input, main) = statements(input)?;

        let entry = fns.len();
        fns.push(Function {
            name: "main".into(),
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

    fn statements(input: &[u8]) -> IResult<&[u8], Vec<Statement>> {
        separated_nonempty_list(key(";"), statement)(input)
    }

    fn statement(input: &[u8]) -> IResult<&[u8], Statement> {
        preceded(
            spaces,
            alt((
                skip,
                while_,
                for_,
                repeat_until,
                if_else,
                assign,
                read,
                write,
                call,
            )),
        )(input)
    }

    fn key<'a>(key: &'a str) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], &'a [u8]> {
        preceded(spaces, tag(key))
    }

    fn skip(input: &[u8]) -> IResult<&[u8], Statement> {
        map(tag("skip"), |_| Statement::Skip)(input)
    }

    fn while_(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, condition) = preceded(key("while"), expr)(input)?;
        let (input, body) = delimited(key("do"), statements, key("od"))(input)?;
        Ok((input, Statement::While { condition, body }))
    }

    fn for_(input: &[u8]) -> IResult<&[u8], Statement> {
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

    fn repeat_until(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, body) = preceded(key("repeat"), statements)(input)?;
        let (input, condition) = preceded(key("until"), expr)(input)?;
        Ok((input, Statement::RepeatUntil { body, condition }))
    }

    fn if_else(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, root_condition) = preceded(key("if"), expr)(input)?;
        let (input, if_true) = preceded(key("then"), statements)(input)?;
        let (input, mut elifs) = many0(elif)(input)?;
        let (input, else_) = opt(else_)(input)?;
        let (input, _) = key("fi")(input)?;

        // build if-else chain
        let if_else = if let Some((condition, body)) = elifs.pop() {
            let mut root = Statement::IfElse {
                condition,
                if_true: body,
                if_false: else_.unwrap_or_default(),
            };

            for (condition, body) in elifs.into_iter().rev() {
                root = Statement::IfElse {
                    condition,
                    if_true: body,
                    if_false: vec![root],
                };
            }

            Statement::IfElse {
                condition: root_condition,
                if_true,
                if_false: vec![root],
            }
        } else {
            // simple case
            Statement::IfElse {
                condition: root_condition,
                if_true,
                if_false: else_.unwrap_or_default(),
            }
        };

        Ok((input, if_else))
    }

    fn elif(input: &[u8]) -> IResult<&[u8], (Expr, Vec<Statement>)> {
        let (input, condition) = preceded(key("elif"), expr)(input)?;
        let (input, body) = preceded(key("then"), statements)(input)?;
        Ok((input, (condition, body)))
    }

    fn else_(input: &[u8]) -> IResult<&[u8], Vec<Statement>> {
        preceded(key("else"), statements)(input)
    }

    fn assign(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, var) = variable(input)?;
        let (input, e) = preceded(key(":="), expr)(input)?;
        Ok((input, Statement::Assign(var, e)))
    }

    fn read(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, _) = key("read")(input)?;
        let (input, var) = delimited(key("("), variable, key(")"))(input)?;
        Ok((input, Statement::Read(var)))
    }

    fn write(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, _) = key("write")(input)?;
        let (input, e) = delimited(key("("), expr, key(")"))(input)?;
        Ok((input, Statement::Write(e)))
    }

    fn call(input: &[u8]) -> IResult<&[u8], Statement> {
        let (input, name) = variable(input)?;
        let (input, args) = delimited(key("("), separated_list(key(","), expr), key(")"))(input)?;
        Ok((input, Statement::Call { name, args }))
    }

    fn function(input: &[u8]) -> IResult<&[u8], Function> {
        let (input, name) = preceded(key("fun "), variable)(input)?;
        let (input, args) =
            delimited(key("("), separated_list(key(","), variable), key(")"))(input)?;
        let (input, locals) =
            opt(preceded(key("locals"), separated_list(key(","), variable)))(input)?;
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
