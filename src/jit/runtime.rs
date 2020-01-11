use std::io;

use crate::io::{InputStream, OutputStream};
use crate::types::Int;

pub struct Runtime {
    input: Box<dyn InputStream>,
    output: Box<dyn OutputStream>,
}

impl Runtime {
    pub unsafe extern "win64" fn read(p: *mut Runtime) -> Int {
        let rt = &mut *p;
        rt.input.read().unwrap_or(-1)
    }

    pub unsafe extern "win64" fn write(p: *mut Runtime, val: Int) {
        let rt = &mut *p;
        rt.output.write(val as Int)
    }

    pub fn stdio() -> Box<Self> {
        Runtime::new(Box::new(io::stdin()), Box::new(io::stdout()))
    }

    pub fn new(input: Box<dyn InputStream>, output: Box<dyn OutputStream>) -> Box<Self> {
        Box::new(Runtime { input, output })
    }
}
