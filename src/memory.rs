use std::collections::hash_map::Entry;

use slab::Slab;
use thiserror::Error;

use crate::types::{Int, Var};

type Key = usize; // offset into |memory| at which the variable is located
type AllocationIndex = fnv::FnvHashMap<Var, Key>;

#[derive(Debug, Error)]
pub enum AllocationError {
    #[error("Out of memory")]
    OutOfMemory,

    #[error("Variable is already allocated at {location}")]
    AlreadyAllocated { location: Key },
}

type Result<T> = std::result::Result<T, AllocationError>;

#[derive(Debug, Clone)]
struct MemoryBlock {
    memory: Slab<Int>,
    size: usize,
}

impl MemoryBlock {
    fn with_capacity(size: usize) -> Self {
        MemoryBlock {
            memory: Slab::with_capacity(size),
            size,
        }
    }

    fn get_mut(&mut self, key: Key) -> Option<&mut Int> {
        self.memory.get_mut(key)
    }

    fn clear(&mut self) {
        self.memory.clear();
    }

    fn load(&self, key: Key) -> Option<Int> {
        self.memory.get(key).copied()
    }

    fn store(&mut self, key: Key, value: Int) {
        let location = self.memory.get_mut(key).expect("Invalid varaible location");

        *location = value;
    }

    fn allocate(&mut self) -> Result<Key> {
        if self.memory.len() >= self.size {
            return Err(AllocationError::OutOfMemory);
        }

        Ok(self.memory.insert(0))
    }

    fn deallocate(&mut self, key: Key) {
        debug_assert!(self.memory.contains(key));
        self.memory.remove(key);
    }
}

#[derive(Debug, Clone)]
pub struct Memory {
    block: MemoryBlock,
    index: AllocationIndex,
}

const DEFAULT_MEMORY_SIZE: usize = 1024;

impl Memory {
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_MEMORY_SIZE)
    }

    pub fn with_capacity(size: usize) -> Self {
        Memory {
            block: MemoryBlock::with_capacity(size),
            index: AllocationIndex::default(),
        }
    }

    #[cfg(test)]
    pub fn with_globals(globals: impl Iterator<Item = Var>) -> Self {
        let mut memory = Self::new();

        for name in globals {
            memory.allocate(&name).expect("Too many test variables");
        }

        memory
    }

    pub fn iter(&self) -> impl Iterator<Item = &Var> {
        self.index.keys()
    }

    pub fn load(&self, name: &Var) -> Option<Int> {
        self.index.get(name).and_then(|&key| self.block.load(key))
    }

    pub fn store(&mut self, name: &Var, value: Int) {
        let key = self
            .index
            .get(name)
            .copied()
            .expect("Attempt to store to undefined variable");

        self.block.store(key, value);
    }

    pub fn allocate(&mut self, name: &Var) -> Result<()> {
        match self.index.entry(name.clone()) {
            Entry::Occupied(v) => Err(AllocationError::AlreadyAllocated { location: *v.get() }),
            Entry::Vacant(v) => {
                let key = self.block.allocate()?;
                v.insert(key);
                Ok(())
            }
        }
    }

    pub fn deallocate(&mut self, name: &Var) {
        if let Some(key) = self.index.remove(name) {
            self.block.deallocate(key);
        }
    }

    pub fn get_mut(&mut self, name: &Var) -> Option<&mut Int> {
        let key = self.index.get(name).copied()?;
        let location = self.block.get_mut(key)?;
        Some(location)
    }

    pub fn get_ptr(&mut self, name: &Var) -> Option<*mut Int> {
        self.get_mut(name).map(|r| r as *mut Int)
    }

    pub fn clear(&mut self) {
        self.block.clear();
        self.index.clear();
    }
}

#[derive(Debug)]
pub struct ScopedMemory {
    globals: Memory,
    locals: Vec<AllocationIndex>,
    // Current number of scopes. Might not be same as locals.len()
    // because we cache allocations
    size: usize,
}

impl ScopedMemory {
    pub fn new() -> Self {
        ScopedMemory {
            globals: Memory::new(),
            locals: Vec::new(),
            size: 0,
        }
    }

    #[cfg(test)]
    pub fn with_globals(globals: impl Iterator<Item = Var>) -> Self {
        ScopedMemory {
            globals: Memory::with_globals(globals),
            locals: Vec::new(),
            size: 0,
        }
    }

    pub fn load(&self, name: &Var) -> Option<Int> {
        match self.local_index(name) {
            Some(key) => self.globals.block.load(key),
            None => self.globals.load(name),
        }
    }

    pub fn store(&mut self, name: &Var, value: Int) {
        match self.local_index(name) {
            Some(key) => self.globals.block.store(key, value),
            None => self.globals.store(name, value),
        }
    }

    pub fn push_scope(&mut self, locals: impl Iterator<Item = Var>) {
        if self.locals.len() == self.size {
            self.locals.push(AllocationIndex::with_capacity_and_hasher(
                16,
                Default::default(),
            ));
        } else {
            let locals = &mut self.locals[self.size];
            for (_name, key) in locals.iter() {
                self.globals.block.deallocate(*key);
            }

            locals.clear();
        }

        let index = &mut self.locals[self.size];
        for name in locals {
            let key = self.globals.block.allocate().unwrap();
            index.insert(name, key);
        }

        self.size += 1;
    }

    pub fn pop_scope(&mut self) {
        // cache the AllocationIndex so that next push_scope won't allocate
        self.size -= 1;
    }

    pub fn clear(&mut self) {
        self.globals.clear();
        self.locals.clear();
    }

    pub fn globals(&self) -> &Memory {
        &self.globals
    }

    pub fn globals_mut(&mut self) -> &mut Memory {
        &mut self.globals
    }

    fn local_index(&self, name: &Var) -> Option<Key> {
        self.locals
            .get(self.size.wrapping_sub(1))
            .and_then(|index| index.get(name).copied())
    }
}
