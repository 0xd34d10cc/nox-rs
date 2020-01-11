use crate::statement::{self, CompilationError, TypeError as Error};

fn compile(program: &str) -> Result<(), Error> {
    match statement::compile(program.trim()) {
        Err(CompilationError::Parse(e)) => panic!("{}", e),
        Err(CompilationError::Type(e)) => Err(e),
        Ok(_) => Ok(()),
    }
}

#[test]
fn undefined_variable() {
    assert_eq!(
        Err(Error::UndefinedVar("x".into())),
        compile(
            "
            write(x)
            "
        )
    );
}

#[test]
fn possibly_uninitialilzed_local() {
    assert_eq!(
        Ok(()),
        compile(
            "
            fun kek(b) local x {
                if b == 0 then
                    x := 3
                fi;

                write(x)
            }

            kek(42)
            "
        )
    );
}

#[test]
fn possibly_uninitialilzed_global() {
    assert_eq!(
        Ok(()),
        compile(
            "
            fun kek(b) {
                if b == 0 then
                    x := 3
                fi
            }

            kek(42);
            write(x)
            "
        )
    );
}

#[test]
fn uninitialized_local() {
    assert_eq!(
        Err(Error::UninitializedVar("x".into())),
        compile(
            "
            fun kek(b) local x {
                write(x)
            }

            kek(42)
            "
        )
    );
}

#[test]
fn uninitialized_global() {
    assert_eq!(
        Err(Error::UndefinedVar("x".into())),
        compile(
            "
            fun kek() {
                x := 42
            }

            write(x);
            kek()
            "
        )
    );
}

#[test]
fn invalid_number_of_args() {
    assert_eq!(
        Err(Error::InvalidNumberOfArgs {
            name: "kek".into(),
            expected: 1,
            found: 0
        }),
        compile(
            "
            fun kek(b) local x {
                write(x)
            }

            kek()
            "
        )
    );
}

#[test]
fn undefined_function() {
    assert_eq!(
        Err(Error::UndefinedFunction("foo".into())),
        compile(
            "
            fun kek(b) {
                foo()
            }

            kek(42)
            "
        )
    );
}

#[test]
fn not_all_control_paths_return_value() {
    assert_eq!(
        Err(Error::NotAllControlPathsReturn("kek".into())),
        compile(
            "
            fun kek(b) {
                if b == 0 then
                    return 42
                fi
            }

            kek(0)
            "
        )
    );
}

#[test]
fn use_of_function_as_procedure() {
    assert_eq!(
        Ok(()),
        compile(
            "
            fun kek(b) {
                return 42
            }

            kek(0)
            "
        )
    );
}

#[test]
fn use_of_procedure_in_expression() {
    assert_eq!(
        Err(Error::NotAllControlPathsReturn("kek".into())),
        compile(
            "
            fun kek(b) {
                if b == 0 then
                    write(42)
                fi
            }

            write(kek(0))
            "
        )
    );
}

#[test]
fn complete_if_else() {
    assert_eq!(
        Ok(()),
        compile(
            "
            fun kek(b) {
                if b == 0 then
                    return 42
                else
                    return 1
                fi
            }

            write(kek(0))
            "
        )
    );
}

#[test]
fn complete_do_while() {
    assert_eq!(
        Ok(()),
        compile(
            "
            fun kek(b) {
                repeat
                    return 42
                until 1
            }

            write(kek(0))
            "
        )
    );
}

#[test]
fn incomplete_while() {
    assert_eq!(
        Err(Error::NotAllControlPathsReturn("kek".into())),
        compile(
            "
            fun kek(b) {
                while 1 do
                    return 42
                od
            }

            write(kek(0))
            "
        )
    );
}
