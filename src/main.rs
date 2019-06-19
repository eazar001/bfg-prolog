use self::Cell::*;
use self::Mode::{Read, Write};
use std::collections::HashMap;


type HeapAddress = usize;

type RegisterAddress = usize;

type FunctorArity = usize;

type FunctorName = String;

#[derive(Debug, Clone)]
pub struct Functor(FunctorName, FunctorArity);

#[derive(Debug, Clone)]
pub enum Cell {
    Str(HeapAddress),
    Ref(HeapAddress),
    Func(Functor)
}

#[derive(Debug, Copy, Clone)]
pub enum StoreAddress {
    Heap(HeapAddress),
    X(RegisterAddress)
}

#[derive(Debug, Copy, Clone)]
pub enum Mode {
    Read,
    Write
}

// the "global stack"
#[derive(Debug, Clone)]
pub struct Heap {
    pub h: HeapAddress,
    pub cells: Vec<Cell>,
    mode: Mode,
    fail: bool
}

#[derive(Debug, Clone)]
pub struct Registers {
    // variable register mapping heap location to cell data (x-register)
    pub x: HashMap<RegisterAddress, Cell>,
    // subterm register containing heap address of next subterm to be matched (s-register)
    pub s: usize
}

#[derive(Debug, Clone)]
pub struct Env {
    pub heap: Heap,
    pub registers: Registers
}

impl Env {
    pub fn new() -> Env {
        Env {
            heap: Heap::new(Vec::new()),
            registers: Registers::new()
        }
    }

    // put_structure f/n, Xi
    #[allow(dead_code)]
    pub fn put_structure(&mut self, functor: Functor, register: RegisterAddress) {
        let h = &self.heap.h;

        // HEAP[H] <- <STR, H+1>
        self.heap.cells.push(Str(*h+1));

        // HEAP[H+1] <- f/n
        self.heap.cells.push(Cell::Func(functor));

        // Xi <- HEAP[H]
        self.registers.insert_x(register, self.heap.cells[*h].clone());

        // H <- H + 2
        self.heap.h += 2;
    }

    // set_variable Xi
    #[allow(dead_code)]
    pub fn set_variable(&mut self, register: RegisterAddress) {
        let h = &self.heap.h;

        // HEAP[H] <- <REF, H>
        self.heap.cells.push(Ref(*h));

        // Xi <- HEAP[H]
        self.registers.insert_x(register, self.heap.cells[*h].clone());

        // H <- H + 1
        self.heap.h += 1;
    }

    // set_value Xi
    #[allow(dead_code)]
    pub fn set_value(&mut self, register: RegisterAddress) {
        // HEAP[H] <- Xi
        self.heap.cells.push(self.registers.get_x(register).unwrap().clone());

        // H <- H + 1
        self.heap.h += 1;
    }

    #[allow(dead_code)]
    pub fn deref(&self, mut address: StoreAddress) -> StoreAddress {
        loop {
            let cell = match address {
                StoreAddress::Heap(addr) => &self.heap.cells[addr],
                StoreAddress::X(addr) => self.registers.get_x(addr).unwrap()
            };

            let a = match address {
                StoreAddress::Heap(addr) => addr,
                StoreAddress::X(addr) => addr
            };

            match *cell {
                Ref(value) => {
                    if value != a {
                        address = StoreAddress::Heap(a);
                    } else {
                        return StoreAddress::Heap(a)
                    }
                },
                Str(_) => return StoreAddress::Heap(a),
                Func(_) => return StoreAddress::Heap(a)
            }
        }
    }

    // get_structure f/n, Xi
    #[allow(dead_code)]
    pub fn get_structure(&mut self, functor: Functor, register: usize) {
        let (cell, address) = match self.deref(StoreAddress::X(register)) {
            StoreAddress::Heap(addr) => (&self.heap.cells[addr], addr),
            StoreAddress::X(addr) => (self.registers.get_x(register).unwrap(), addr),
        };

        match *cell {
            Ref(_) => {
                let h = &self.heap.h;

                self.heap.cells.push(Str(*h+1));
                self.heap.cells.push(Func(functor));
                Self::bind(address, *h);

                self.heap.h += 2;
                self.heap.mode = Write;
            },
            Str(a) => {
                match &self.heap.cells[a] {
                    Func(Functor(s_name, s_arity)) => {
                        let Functor(f_name, f_arity) = functor;

                        if *s_name == f_name && *s_arity == f_arity {
                            self.registers.s += 1;
                            self.heap.mode = Read;
                        } else {
                            self.heap.fail = true;
                        }
                    }
                    _ => {
                        self.heap.fail = true;
                    }
                }
            }
            Func(_) => {
                self.heap.fail = true;
            }
        }
    }

    // unify_variable Xi
    #[allow(dead_code)]
    pub fn unify_variable(&mut self, register: RegisterAddress) {
        match self.heap.mode {
            Read => {
                let s = &self.registers.s;
                let cell = self.heap.cells[*s].clone();

                self.registers.insert_x(register, cell);
            },
            Write => {
                let h = &self.heap.h;

                self.heap.cells.push(Ref(*h));
                self.registers.insert_x(register, self.heap.cells[*h].clone());
                self.heap.h += 1;
            }
        }

        self.registers.s += 1;
    }

    // unify_value Xi
    #[allow(dead_code)]
    fn unify_value(&mut self, register: RegisterAddress) {
        match self.heap.mode {
            Read => {
                let s = self.registers.s;
                self.unify(register, s)
            },
            Write => {
                self.heap.cells.push(self.registers.get_x(register).unwrap().clone());
                self.heap.h += 1;
            }
        }

        self.registers.s += 1;
    }

    #[allow(dead_code)]
    fn bind(_address: usize, _heap_address: usize) {
        unimplemented!()
    }

    #[allow(dead_code)]
    fn unify(&mut self, _register: usize, _s: usize) {
        unimplemented!()
    }
}

pub trait Store {
    fn get(&self, slot: usize) -> Option<&Cell>;
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            x: HashMap::new(),
            s: 0
        }
    }

    pub fn get_x(&self, register: RegisterAddress) -> Option<&Cell> {
        self.x.get(&register)
    }

    pub fn insert_x(&mut self, register: RegisterAddress, cell: Cell) -> Option<Cell> {
        self.x.insert(register, cell)
    }
}

impl Heap {
    pub fn new(heap: Vec<Cell>) -> Heap {
        Heap {
            h: 0,
            cells: heap,
            mode: Read,
            fail: false
        }
    }
}


fn main() {
    let mut env = Env::new();

    env.put_structure(Functor(String::from("name"), 0), 0);
    env.put_structure(Functor(String::from("foo"), 2), 0);

    println!("{:?}", env);
}
