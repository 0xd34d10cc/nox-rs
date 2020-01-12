use std::collections::HashMap;

use slab::Slab;
use thiserror::Error;

use crate::types::{Int, Var};

type Key = usize;
type AllocationIndex = HashMap<Var, Key>;

#[derive(Debug, Error)]
pub enum AllocationError {
    #[error("Out of memory")]
    OutOfMemory,

    #[error("Variable is already allocated at {location}")]
    AlreadyAllocated { location: Key },
}

type Result<T> = std::result::Result<T, AllocationError>;

#[derive(Debug, Clone)]
pub struct Memory {
    memory: Slab<Int>,
    size: usize,

    // globals
    index: AllocationIndex,
}

const SIZE: usize = 1024;

impl Memory {
    pub fn new() -> Self {
        Memory {
            memory: Slab::with_capacity(SIZE),
            size: SIZE,
            index: AllocationIndex::new(),
        }
    }

    #[cfg(test)]
    pub fn with_globals(globals: impl Iterator<Item = Var>) -> Self {
        let mut memory = Slab::with_capacity(SIZE);
        let mut index = AllocationIndex::new();

        for name in globals {
            let key = memory.insert(0);
            index.insert(name, key);
        }

        Memory {
            memory,
            size: SIZE,
            index,
        }
    }

    pub fn globals(&self) -> impl Iterator<Item = &Var> {
        self.index.keys()
    }

    pub fn load(&self, name: &Var) -> Option<Int> {
        self.index.get(name).and_then(|&key| self.direct_load(key))
    }

    pub fn store(&mut self, name: &Var, value: Int) {
        let key = self
            .index
            .get(name)
            .copied()
            .expect("Attempt to store to undefined variable");

        self.direct_store(key, value);
    }

    pub fn allocate(&mut self, name: &Var) -> Result<()> {
        if let Some(key) = self.index.get(name) {
            return Err(AllocationError::AlreadyAllocated { location: *key });
        }

        let key = self.allocate_direct()?;
        self.index.insert(name.clone(), key);
        Ok(())
    }

    pub fn deallocate(&mut self, name: &Var) {
        if let Some(key) = self.index.remove(name) {
            self.deallocate_direct(key);
        }
    }

    pub fn get_ptr(&mut self, name: &Var) -> Option<*mut Int> {
        let key = self.index.get(name).copied()?;
        let location = self.memory.get_mut(key)?;
        Some(location as *mut Int)
    }

    pub fn clear(&mut self) {
        self.memory.clear();
        self.index.clear();
    }

    fn direct_load(&self, key: Key) -> Option<Int> {
        self.memory.get(key).copied()
    }

    fn direct_store(&mut self, key: Key, value: Int) {
        let location = self.memory.get_mut(key).expect("Invalid varaible location");

        *location = value;
    }

    fn allocate_direct(&mut self) -> Result<Key> {
        if self.memory.len() >= self.size {
            return Err(AllocationError::OutOfMemory);
        }

        Ok(self.memory.insert(0))
    }

    fn deallocate_direct(&mut self, key: Key) {
        debug_assert!(self.memory.contains(key));
        self.memory.remove(key);
    }
}

#[derive(Debug)]
pub struct ScopedMemory {
    globals: Memory,
    locals: Vec<AllocationIndex>,
}

impl ScopedMemory {
    pub fn new() -> Self {
        ScopedMemory {
            globals: Memory::new(),
            locals: Vec::new(),
        }
    }

    #[cfg(test)]
    pub fn with_globals(globals: impl Iterator<Item = Var>) -> Self {
        ScopedMemory {
            globals: Memory::with_globals(globals),
            locals: Vec::new(),
        }
    }

    pub fn load(&self, name: &Var) -> Option<Int> {
        match self.local_index(name) {
            Some(key) => self.globals.direct_load(key),
            None => self.globals.load(name),
        }
    }

    pub fn store(&mut self, name: &Var, value: Int) {
        match self.local_index(name) {
            Some(key) => self.globals.direct_store(key, value),
            None => self.globals.store(name, value),
        }
    }

    pub fn push_scope(&mut self, locals: impl Iterator<Item = Var>) {
        let mut index = AllocationIndex::new();
        for name in locals {
            let key = self.globals.allocate_direct().unwrap();
            index.insert(name, key);
        }

        self.locals.push(index);
    }

    pub fn pop_scope(&mut self) {
        if let Some(locals) = self.locals.pop() {
            for (_name, key) in locals {
                self.globals.deallocate_direct(key);
            }
        }
    }

    pub fn clear(&mut self) {
        self.globals.clear();
        self.locals.clear();
    }

    pub fn globals(&self) -> impl Iterator<Item = &Var> {
        self.globals.globals()
    }

    pub fn globals_mut(&mut self) -> &mut Memory {
        &mut self.globals
    }

    fn local_index(&self, name: &Var) -> Option<Key> {
        self.locals
            .last()
            .and_then(|index| index.get(name).copied())
    }
}
