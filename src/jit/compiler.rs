#[cfg(not(target_arch = "x86_64"))]
compile_error!("JIT is implemented only for x86-64");

use std::collections::HashMap;

use dynasm::dynasm;
use dynasmrt::x64::Assembler;
use dynasmrt::{DynamicLabel, DynasmApi, DynasmLabelApi};

use super::{Program, Runtime};
use crate::memory::Memory;
use crate::ops::{LogicOp, Op};
use crate::sm;
use crate::types::{Int, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(unused)]
enum Register {
    RAX = 0x00,
    RCX = 0x01,
    RDX = 0x02,
    RBX = 0x03,
    RSP = 0x04,
    RBP = 0x05,
    RSI = 0x06,
    RDI = 0x07,
    R8 = 0x08,
    R9 = 0x09,
    R10 = 0x0A,
    R11 = 0x0B,
    R12 = 0x0C,
    R13 = 0x0D,
    R14 = 0x0E,
    R15 = 0x0F,
}

type StackOffset = i32;
const WORD: i32 = 8;

#[derive(Debug, Clone, Copy)]
enum Operand {
    Register(Register),
    Stack(StackOffset),
}

impl Operand {
    fn store_const(self, c: Int, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov Rq(r as u8), QWORD c
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov rax, QWORD c
                ; mov QWORD [rbp - offset], rax
            ),
        }
    }

    fn store_into(self, register: Register, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov Rq(register as u8), Rq(r as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov Rq(register as u8), [rbp - offset]
            ),
        }
    }

    fn load_from(self, register: Register, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov Rq(r as u8), Rq(register as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov [rbp - offset], Rq(register as u8)
            ),
        }
    }

    fn load_from_memory(self, location: *mut Int, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov rax, QWORD location as _
                ; mov Rq(r as u8), [rax]
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov rax, QWORD location as _
                ; mov rbx, QWORD [rax]
                ; mov [rbp - offset], rbx
            ),
        }
    }

    fn store_to_memory(self, location: *mut Int, ops: &mut Assembler) {
        match self {
            Operand::Register(r) => dynasm!(ops
                ; mov rax, QWORD location as _
                ; mov [rax], Rq(r as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov rax, QWORD location as _
                ; mov rbx, [rbp - offset]
                ; mov [rax], rbx
            ),
        }
    }
}

pub struct Compiler<'a> {
    free_registers: Vec<Register>,
    stack: Vec<Operand>,
    globals: &'a mut Memory,
    runtime: &'a mut Runtime,
    labels: HashMap<sm::Label, DynamicLabel>,
}

impl<'a> Compiler<'a> {
    pub fn new<'b>(runtime: &'b mut Runtime, globals: &'b mut Memory) -> Compiler<'b> {
        Compiler {
            free_registers: vec![Register::R12, Register::R13, Register::R14, Register::R15],
            stack: Vec::new(),
            globals,
            runtime,
            labels: HashMap::new(),
        }
    }

    fn push(&mut self, op: Operand) {
        if let Operand::Register(r) = op {
            self.free_registers.retain(|&register| register != r);
        }

        self.stack.push(op);
    }

    fn pop(&mut self) -> Option<Operand> {
        let op = self.stack.pop();
        if let Some(Operand::Register(r)) = op {
            self.free_registers.push(r);
        }
        op
    }

    fn allocate(&mut self) -> Operand {
        let op = if let Some(r) = self.free_registers.pop() {
            Operand::Register(r)
        } else {
            match self.stack.last().expect("Empty stack (allocate)") {
                Operand::Stack(offset) => Operand::Stack(offset + WORD),
                _ => Operand::Stack(WORD),
            }
        };

        self.push(op);
        op
    }

    fn dyn_label(&mut self, label: sm::Label, ops: &mut Assembler) -> DynamicLabel {
        self.labels.get(&label).cloned().unwrap_or_else(|| {
            let dyn_label = ops.new_dynamic_label();
            self.labels.insert(label, dyn_label);
            dyn_label
        })
    }

    fn div(&mut self, lhs: Operand, rhs: Operand, ops: &mut Assembler) {
        lhs.store_into(Register::RAX, ops);
        dynasm!(ops
            ; cqo // sign-extend RAX into RDX:RAX
        );
        match rhs {
            Operand::Register(r) => dynasm!(ops
                ; idiv Rq(r as u8)
            ),
            Operand::Stack(offset) => dynasm!(ops
                ; mov rbx, [rbp - offset]
                ; idiv rbx
            ),
        }
    }

    // compile 'and' or 'or' operation
    fn and_or(&mut self, lhs: Operand, rhs: Operand, is_and: bool, ops: &mut Assembler) {
        match (lhs, rhs) {
            (Operand::Register(lhs), Operand::Register(rhs)) => {
                let (l, r) = (lhs as u8, rhs as u8);
                dynasm!(ops
                    ; test Rq(l), Rq(l)
                    ; setne Rb(l)
                    ; and Rq(l), 0xff
                    ; test Rq(r), Rq(r)
                    ; setne Rb(r)
                    ; and Rq(r), 0xff
                );

                if is_and {
                    dynasm!(ops
                        ; and Rq(l), Rq(r)
                    )
                } else {
                    dynasm!(ops
                        ; or Rq(l), Rq(r)
                    )
                }

                self.push(Operand::Register(lhs))
            }
            // TODO: optimize for more cases, this fallback should happen only if
            //       both operands are on stack
            _ => {
                lhs.store_into(Register::RAX, ops);
                rhs.store_into(Register::RBX, ops);
                dynasm!(ops
                    ; test rax, rax
                    ; setne al
                    ; and rax, 0xff
                    ; test rbx, rbx
                    ; setne bl
                    ; and rbx, 0xff
                );

                if is_and {
                    dynasm!(ops
                        ; and rax, rbx
                    )
                } else {
                    dynasm!(ops
                        ; or rax, rbx
                    )
                }

                let dst = self.allocate();
                dst.load_from(Register::RAX, ops);
            }
        }
    }

    fn compile_instruction(
        &mut self,
        ops: &mut Assembler,
        instruction: &sm::Instruction,
    ) -> Result<()> {
        match instruction {
            sm::Instruction::Label(label) => {
                let offset = ops.offset();
                let dyn_label = self.dyn_label(*label, ops);
                ops.labels_mut().define_dynamic(dyn_label, offset)?;
            }
            sm::Instruction::Jump(label) => {
                let dyn_label = self.dyn_label(*label, ops);
                dynasm!(ops
                    ; jmp =>dyn_label
                );
            }
            sm::Instruction::JumpIfZero(label) => {
                let dyn_label = self.dyn_label(*label, ops);
                let top = self.pop().ok_or("Empty stack (jz)")?;
                match top {
                    Operand::Register(r) => dynasm!(ops
                        ; test Rq(r as u8), Rq(r as u8)
                    ),
                    Operand::Stack(_) => {
                        top.store_into(Register::RAX, ops);
                        dynasm!(ops
                            ; test rax, rax
                        )
                    }
                };
                dynasm!(ops
                    ; jz =>dyn_label
                )
            }
            sm::Instruction::JumpIfNotZero(label) => {
                let dyn_label = self.dyn_label(*label, ops);
                let top = self.pop().ok_or("Empty stack (jnz)")?;
                match top {
                    Operand::Register(r) => dynasm!(ops
                        ; test Rq(r as u8), Rq(r as u8)
                    ),
                    Operand::Stack(_) => {
                        top.store_into(Register::RAX, ops);
                        dynasm!(ops
                            ; test rax, rax
                        )
                    }
                };
                dynasm!(ops
                    ; jnz =>dyn_label
                )
            }
            sm::Instruction::Const(c) => {
                let dst = self.allocate();
                dst.store_const(*c, ops);
            }
            sm::Instruction::Write => {
                let src = self.pop().ok_or("Empty stack (write)")?;
                debug_assert!(self.stack.is_empty());
                let rt = &mut *self.runtime as *mut Runtime;
                src.store_into(Register::RDX, ops);
                dynasm!(ops
                    ; mov rcx, QWORD rt as _
                    ; mov rax, QWORD Runtime::write as usize as i64
                    ; call rax
                )
            }
            sm::Instruction::Read => {
                debug_assert!(self.stack.is_empty());
                let dst = self.allocate();
                let rt = &mut *self.runtime as *mut Runtime;
                dynasm!(ops
                    ; mov rcx, QWORD rt as _
                    ; mov rax, QWORD Runtime::read as usize as i64
                    ; call rax
                );
                dst.load_from(Register::RAX, ops);
            }
            sm::Instruction::Store(var) => {
                let src = self.pop().ok_or("Empty stack (store)")?;
                let dst = self
                    .globals
                    .get_ptr(var)
                    .ok_or("Failed to find global (store)")?;
                src.store_to_memory(dst, ops);
            }
            sm::Instruction::Load(var) => {
                let src = self
                    .globals
                    .get_ptr(var)
                    .ok_or("Failed to find global (load)")?;
                let dst = self.allocate();
                dst.load_from_memory(src, ops);
            }
            sm::Instruction::Op(Op::Div) => {
                let rhs = self.pop().ok_or("Empty stack (div, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (div, lhs)")?;
                let dst = self.allocate();
                self.div(lhs, rhs, ops);
                dst.load_from(Register::RAX, ops);
            }
            sm::Instruction::Op(Op::Mod) => {
                let rhs = self.pop().ok_or("Empty stack (mod, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (mod, lhs)")?;
                let dst = self.allocate();
                self.div(lhs, rhs, ops);
                dst.load_from(Register::RDX, ops);
            }
            sm::Instruction::Op(op) => {
                let compile_op = |lhs, rhs, ops: &mut Assembler| {
                    match rhs {
                        Operand::Register(rhs) => {
                            match op {
                                Op::Add => dynasm!(ops
                                    ; add Rq(lhs as u8), Rq(rhs as u8)
                                ),
                                Op::Sub => dynasm!(ops
                                    ; sub Rq(lhs as u8), Rq(rhs as u8)
                                ),
                                Op::Mul => dynasm!(ops
                                    ; imul Rq(lhs as u8), Rq(rhs as u8)
                                ),
                                Op::Div | Op::Mod => unreachable!(), // handled above
                            }
                        }
                        Operand::Stack(offset) => {
                            match op {
                                Op::Add => dynasm!(ops
                                    ; add Rq(lhs as u8), [rbp - offset]
                                ),
                                Op::Sub => dynasm!(ops
                                    ; sub Rq(lhs as u8), [rbp - offset]
                                ),
                                Op::Mul => dynasm!(ops
                                    ; imul Rq(lhs as u8), [rbp - offset]
                                ),
                                Op::Div | Op::Mod => unreachable!(), // handled above
                            }
                        }
                    }
                };

                let rhs = self.pop().ok_or("Empty stack (op, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (op, lhs)")?;
                match lhs {
                    Operand::Register(lhs) => {
                        compile_op(lhs, rhs, ops);
                        self.push(Operand::Register(lhs));
                    }
                    Operand::Stack(_) => {
                        lhs.store_into(Register::RAX, ops);
                        compile_op(Register::RAX, rhs, ops);
                        let dst = self.allocate();
                        dst.load_from(Register::RAX, ops);
                    }
                };
            }
            sm::Instruction::LogicOp(LogicOp::And) => {
                let rhs = self.pop().ok_or("Empty stack (and, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (and, lhs)")?;
                self.and_or(lhs, rhs, true /* is_and */, ops);
            }
            sm::Instruction::LogicOp(LogicOp::Or) => {
                let rhs = self.pop().ok_or("Empty stack (or, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (or, lhs)")?;
                self.and_or(lhs, rhs, false /* is_and */, ops);
            }
            sm::Instruction::LogicOp(op) => {
                let set = |dst: Register, ops: &mut Assembler| {
                    match op {
                        LogicOp::Less => dynasm!(ops
                            ; setl Rb(dst as u8)
                        ),
                        LogicOp::LessOrEqual => dynasm!(ops
                            ; setle Rb(dst as u8)
                        ),
                        LogicOp::Greater => dynasm!(ops
                            ; setg Rb(dst as u8)
                        ),
                        LogicOp::GreaterOrEqual => dynasm!(ops
                            ; setge Rb(dst as u8)
                        ),
                        LogicOp::Eq => dynasm!(ops
                            ; sete Rb(dst as u8)
                        ),
                        LogicOp::NotEq => dynasm!(ops
                            ; setne Rb(dst as u8)
                        ),
                        LogicOp::And | LogicOp::Or => unreachable!(), // covered above
                    };
                    dynasm!(ops
                        ; and Rq(dst as u8), 0xff
                    );
                };

                let rhs = self.pop().ok_or("Empty stack (logic, rhs)")?;
                let lhs = self.pop().ok_or("Empty stack (logic, lhs)")?;
                let dst = self.allocate();

                match (lhs, rhs) {
                    (Operand::Register(lhs), Operand::Register(rhs)) => dynasm!(ops
                        ; cmp Rq(lhs as u8), Rq(rhs as u8)
                    ),
                    (Operand::Register(lhs), Operand::Stack(offset)) => dynasm!(ops
                        ; cmp Rq(lhs as u8), [rbp - offset]
                    ),
                    (Operand::Stack(offset), Operand::Register(rhs)) => dynasm!(ops
                        ; cmp [rbp - offset], Rq(rhs as u8)
                    ),
                    (Operand::Stack(lhs), Operand::Stack(rhs)) => dynasm!(ops
                        ; mov rax, [rbp - lhs]
                        ; cmp rax, [rbp - rhs]
                    ),
                }

                match dst {
                    Operand::Register(dst) => set(dst, ops),
                    Operand::Stack(_) => {
                        set(Register::RAX, ops);
                        dst.load_from(Register::RAX, ops);
                    }
                }
            }
            sm::Instruction::Call(_) => todo!(),
            sm::Instruction::Begin { .. } => todo!(),
            sm::Instruction::End => todo!(),
            sm::Instruction::Ignore => { /* this instruction invalidates Register::RAX */ }
        };
        Ok(())
    }

    pub fn compile(mut self, program: &sm::Program) -> Result<Program<'a>> {
        // allocate globals
        use crate::memory::AllocationError;
        for global in program.globals() {
            match self.globals.allocate(global) {
                Err(AllocationError::OutOfMemory) => {
                    return Err(AllocationError::OutOfMemory.into())
                }
                Err(AllocationError::AlreadyAllocated { .. }) | Ok(_) => { /* we're fine */ }
            }
        }

        let mut ops = Assembler::new().unwrap();

        // prologue
        let entrypoint = ops.offset();
        dynasm!(ops
            // stack alignment
            ; push rsp
            ; push QWORD [rsp]
            ; and rsp, !0x10 + 1
            // save non-volatile registers
            // TODO: don't save registers that we're not going to use
            ; push rbx
            ; push rdi
            ; push rsi
            ; push r12
            ; push r13
            ; push r14
            ; push r15
            // prepare base pointer
            ; push rbp
            ; mov rbp, rsp
        );

        // actual code
        for instruction in program.instructions() {
            self.compile_instruction(&mut ops, instruction)?;
        }

        // epilogue
        dynasm!(ops
            // restore base pointer
            ; pop rbp
            // restore non-volatile registers
            ; pop r15
            ; pop r14
            ; pop r13
            ; pop r12
            ; pop rsi
            ; pop rdi
            ; pop rbx
            // restore previous rsp value
            ; mov rsp, [rsp+8]
        );

        // retcode
        dynasm!(ops
            ; mov rax, 0
            ; ret
        );

        ops.commit()?;
        let memory = ops
            .finalize()
            .expect("finalize() shouldn't fail if commit() have not failed");

        Ok(unsafe { Program::from_parts(memory, entrypoint, self.globals, self.runtime) })
    }
}
