#![allow(dead_code)]

use self::Cell::*;
use self::Store::*;
use self::Mode::{Read, Write};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};


// heap address represented as usize that corresponds to the vector containing cell data
type HeapAddress = usize;
// x-register address which identifies the register that holds the cell data in the corresponding variable
type Register = usize;
type FunctorArity = usize;
type FunctorName = String;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Functor(FunctorName, FunctorArity);

#[derive(Debug, Clone, Eq, PartialEq)]
enum Cell {
    Str(HeapAddress),
    Ref(HeapAddress),
    Func(Functor)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Store {
    HeapAddr(HeapAddress),
    XAddr(Register)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Mode {
    Read,
    Write
}

// the "global stack"
#[derive(Debug, Clone, Eq, PartialEq)]
struct Heap {
    // the "h" counter contains the location of the next cell to be pushed onto the heap
    h: HeapAddress,
    // all the data that resides on the heap
    cells: Vec<Cell>,
    mode: Mode
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Registers {
    // variable register mapping a variable to cell data (x-register)
    x: HashMap<Register, Cell>,
    // subterm register containing heap address of next subterm to be matched (s-register)
    s: Register,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Env {
    heap: Heap,
    // the "push-down-list" contains StoreAddresses and serves as a unification stack
    pdl: Vec<Store>,
    registers: Registers,
    fail: bool,
}

impl Display for Functor {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Ok(write!(f, "{}/{}", self.name(), self.arity())?)
    }
}

impl Functor {
    fn name(&self) -> &str {
        &self.0
    }

    fn arity(&self) -> usize {
        self.1
    }
}

impl Env {
    fn new() -> Env {
        Env {
            heap: Heap::new(),
            pdl: Vec::new(),
            registers: Registers::new(),
            fail: false
        }
    }

    fn push_heap(&mut self, cell: Cell) {
        self.heap.cells.push(cell);
    }

    fn push_foo(cells: &mut Vec<Cell>, cell: Cell) {
        cells.push(cell);
    }

    fn get_x(&self, register: Register) -> Option<&Cell> {
        self.registers.get_x(register)
    }

    fn insert_x(&mut self, register: Register, cell: Cell) -> Option<Cell> {
        self.registers.insert_x(register, cell)
    }

    fn get_s(&self) -> Register {
        self.registers.s
    }

    fn inc_s(&mut self, value: usize) {
        self.registers.s += value;
    }

    fn heap_counter(&self) -> usize {
        self.heap.h
    }

    fn inc_heap_counter(&mut self, value: usize) {
        self.heap.h += value;
    }

    fn set_mode(&mut self, mode: Mode) {
        self.heap.mode = mode;
    }

    fn empty_pdl(&mut self) -> bool {
        self.pdl.is_empty()
    }

    fn push_pdl(&mut self, address: Store) {
        self.pdl.push(address);
    }

    
    fn pop_pdl(&mut self) -> Option<Store> {
        self.pdl.pop()
    }

    // put_structure f/n, Xi
    fn put_structure(&mut self, functor: Functor, register: Register) {
        let h = self.heap_counter();

        // HEAP[H] <- <STR, H+1>
        self.push_heap(Str(h+1));

        // HEAP[H+1] <- f/n
        self.push_heap(Func(functor));

        // Xi <- HEAP[H]
        self.insert_x(register, self.heap.cells[h].clone());

        // H <- H + 2
        self.inc_heap_counter(2);
    }

    // set_variable Xi
    fn set_variable(&mut self, register: Register) {
        let h = self.heap_counter();

        // HEAP[H] <- <REF, H>
        self.push_heap(Ref(h));

        // Xi <- HEAP[H]
        self.insert_x(register, Ref(h));

        // H <- H + 1
        self.inc_heap_counter(1);
    }

    // set_value Xi
    fn set_value(&mut self, register: Register) {
        // HEAP[H] <- Xi
        self.push_heap(
            self.get_x(register)
                .expect(&format!("Illegal access: register {}, does not exist", register))
                .clone()
        );

        // H <- H + 1
        self.inc_heap_counter(1);
    }

    fn deref(&self, mut address: Store) -> Store {
        loop {
            let (cell, a) = match address {
                HeapAddr(addr) => (&self.heap.cells[addr], addr),
                XAddr(addr) => {
                    let c = self.get_x(addr)
                        .expect(&format!("Illegal access: register {}, does not exist", addr));

                    (c, addr)
                }
            };

            if let Ref(value) = *cell {
                if value != a {
                    address = HeapAddr(value)
                } else {
                    return HeapAddr(a)
                }
            } else {
                return HeapAddr(a)
            }
        }
    }

    // get_structure f/n, Xi
    fn get_structure(&mut self, functor: Functor, register: usize) {
        let (cell, address) = match self.deref(XAddr(register)) {
            HeapAddr(addr) => (&self.heap.cells[addr], addr),
            XAddr(addr) => (self.registers.get_x(addr).unwrap(), addr),
        };

        match *cell {
            Ref(_) => {
                let h = self.heap_counter();

                self.push_heap(Str(h+1));
                self.push_heap(Func(functor));
                self.bind(HeapAddr(address), HeapAddr(h));

                self.inc_heap_counter(2);
                self.set_mode(Write);
            },
            Str(a) => {
                match &self.heap.cells[a] {
                    Func(s_functor) => {
                        if s_functor == &functor {
                            self.inc_s(1);
                            self.set_mode(Read);
                        } else {
                            self.fail = true;
                        }
                    }
                    _ => {
                        self.fail = true;
                    }
                }
            },
            Func(_) => {
                self.fail = true;
            }
        }
    }

    // unify_variable Xi
    fn unify_variable(&mut self, register: Register) {
        match self.heap.mode {
            Read => {
                let s = self.get_s();
                let cell = self.heap.cells[s].clone();

                self.insert_x(register, cell);
            },
            Write => {
                let h = self.heap_counter();

                self.push_heap(Ref(h));
                self.insert_x(register, self.heap.cells[h].clone());
                self.inc_heap_counter(1);
            }
        }

        self.inc_s(1);
    }

    // unify_value Xi
    fn unify_value(&mut self, register: Register) {
        match self.heap.mode {
            Read => {
                let s = self.get_s();
                self.unify(XAddr(register), HeapAddr(s))
            },
            Write => {
                self.push_heap(self.registers.get_x(register).unwrap().clone());
                self.inc_heap_counter(1);
            }
        }

        self.inc_s(1);
    }

    fn unify(&mut self, a1: Store, a2: Store) {
        self.push_pdl(a1);
        self.push_pdl(a2);

        self.fail = false;

        while !(self.empty_pdl() || self.fail) {
            let (a1, a2) = (self.pop_pdl().unwrap(), self.pop_pdl().unwrap());

            let d1 = self.deref(a1);
            let d2 = self.deref(a2);

            if d1 != d2 {
                let c1 = self.get_store_cell(d1);
                let c2 = self.get_store_cell(d2);

                let v1 = match c1 {
                    Str(addr) => *addr,
                    Ref(addr) => *addr,
                    Func(_) => panic!("something went wrong")
                };

                let v2 = match c2 {
                    Str(addr) => *addr,
                    Ref(addr) => *addr,
                    Func(_) => panic!("something went wrong")
                };

                match (c1, c2) {
                    (Ref(_), _) => self.bind(d1, d2),
                    (_, Ref(_)) => self.bind(d1, d2),
                    _ => {
                        let f1 = self.get_functor(c1);
                        let f2 = self.get_functor(c2);

                        if f1 == f2 {
                            let Functor(_, f1_arity) = f1;
                            for i in 1..=f1_arity {
                                self.push_pdl(HeapAddr(v1+i));
                                self.push_pdl(HeapAddr(v2+i));
                            }
                        } else {
                            self.fail = true;
                        }
                    }
                }
            }
        }
    }

    // extracts functor only if cell is a structure or a functor, in which case this function is the identity function
    fn get_functor(&self, structure: &Cell) -> Functor {
        if let Str(address) = *structure {
            let cell = &self.heap.cells[address];

            match cell {
                Str(addr) => {
                    if let Func(f) = self.heap.cells[*addr].clone() {
                        f
                    } else {
                        panic!("something went wrong")
                    }
                },
                Func(f) => f.clone(),
                _ => panic!("something went wrong")
            }
        } else {
            panic!("something went wrong");
        }
    }

    fn get_store_cell(&self, address: Store) -> &Cell {
        match address {
            HeapAddr(addr) => &self.heap.cells[addr],
            XAddr(addr) => self.get_x(addr).unwrap()
        }
    }

    fn bind(&mut self, a1: Store, a2: Store) {
        let (c1, a1, c1_heap) = match a1 {
            HeapAddr(addr) => (&self.heap.cells[addr], addr, true),
            XAddr(addr) => (self.get_x(addr).unwrap(), addr, false)
        };

        let (c2, a2, c2_heap) = match a2 {
            HeapAddr(addr) => (&self.heap.cells[addr], addr, true),
            XAddr(addr) => (self.get_x(addr).unwrap(), addr, false)
        };

        let c1_is_ref = match c1 {
            Ref(_) => true,
            _ => false
        };

        let c2_is_ref = match c2 {
            Ref(_) => true,
            _ => false
        };

        if c1_is_ref && (!c2_is_ref || a2 < a1) {
            if c1_heap {
                self.heap.cells[a1] = c2.clone();
                self.trail(a1);
            } else {
                let c2 = c2.clone();
                self.insert_x(a1, c2);
            }
        } else if c2_heap {
                self.heap.cells[a2] = c1.clone();
                self.trail(a2);
        } else {
            let c1 = c1.clone();
            self.insert_x(a2, c1);
        }
    }

    fn trail(&self, _a: HeapAddress) {
        unimplemented!()
    }
}

impl Registers {
    fn new() -> Registers {
        Registers {
            x: HashMap::new(),
            s: 0
        }
    }

    fn get_x(&self, register: Register) -> Option<&Cell> {
        self.x.get(&register)
    }

    fn insert_x(&mut self, register: Register, cell: Cell) -> Option<Cell> {
        self.x.insert(register, cell)
    }
}

impl Heap {
    fn new() -> Heap {
        Heap {
            h: 0,
            cells: Vec::new(),
            mode: Read
        }
    }
}


fn main() {
    let env = Env::new();

    let f1 = Functor(String::from("foo"), 2);
    let f2 = f1.clone();

    println!("{0:?}\n{0}\n{1}\n{2:?}", &f1, f2.to_string(), env);
}


#[cfg(test)]
mod tests {
    use super::*;

    // set_variable Xi
    #[test]
    fn test_set_variable() {
        let mut env = Env::new();

        env.set_variable(0);

        let expected_heap_cells = vec![Ref(0)];
        let heap_cells = env.heap.cells;
        let registers = env.registers;

        assert_eq!(heap_cells, expected_heap_cells);
        register_is(&registers, 0, Ref(0));
    }

    // set_value Xi
    #[test]
    fn test_set_value() {
        let mut env = Env::new();

        env.set_variable(0);
        env.set_variable(1);

        env.set_value(0);
        env.set_value(1);

        let expected_heap_cells = vec![Ref(0), Ref(1), Ref(0), Ref(1)];
        let heap_cells = env.heap.cells;
        let registers = env.registers;

        assert_eq!(heap_cells, expected_heap_cells);
        register_is(&registers, 0, Ref(0));
        register_is(&registers, 1, Ref(1));
        assert_eq!(registers.x.len(), 2);
    }

    // put_structure f/n, Xi
    #[test]
    fn test_put_structure() {
        let mut env = Env::new();

        env.put_structure(Functor(String::from("foo"), 2), 0);
        env.set_variable(1);
        env.set_variable(2);
        env.set_value(1);

        let expected_heap_cells = vec![
            Str(1),
            Func(Functor(String::from("foo"), 2)),
            Ref(2),
            Ref(3),
            Ref(2)
        ];

        let heap_cells = env.heap.cells;
        let registers = env.registers;

        assert_eq!(heap_cells, expected_heap_cells);
        register_is(&registers, 0, Str(1));
        register_is(&registers, 1, Ref(2));
        register_is(&registers, 2, Ref(3));
        assert_eq!(registers.x.len(), 3);
    }

    #[test]
    fn test_m0_1() {
        // L0 program: p(Z, h(Z, W), f(W)).
        let mut env = Env::new();

        let h = String::from("h");
        let f = String::from("f");
        let p = String::from("p");

        // put_structure h/2, x3
        env.put_structure(Functor(h.clone(), 2), 2);
        // set_variable, x2
        env.set_variable(1);
        // set_variable, x5
        env.set_variable(4);
        // put_structure f/1, x4
        env.put_structure(Functor(f.clone(), 1), 3);
        // set_value, x5
        env.set_value(4);
        // put_structure p/3, x1
        env.put_structure(Functor(p.clone(), 3), 0);
        // set_value x2
        env.set_value(1);
        // set_value x3
        env.set_value(2);
        // set_value x4
        env.set_value(3);


        let expected_heap_cells = vec![
            Str(1),
            Func(Functor(h, 2)),
            Ref(2),
            Ref(3),
            Str(5),
            Func(Functor(f, 1)),
            Ref(3),
            Str(8),
            Func(Functor(p, 3)),
            Ref(2),
            Str(1),
            Str(5),
        ];

        let (heap_cells, registers) = (env.heap.cells, &env.registers);
        assert_eq!(heap_cells, expected_heap_cells);

        register_is(registers,0, Str(8));
        register_is(registers,1, Ref(2));
        register_is(registers,2, Str(1));
        register_is(registers,3, Str(5));
        register_is(registers,4, Ref(3));
    }

    #[ignore]
    #[test]
    fn test_m0_2() {
        // L0 Program: p(f(X), h(Y, f(a)), Y).
        let mut env = Env::new();

        let h = String::from("h");
        let f = String::from("f");
        let p = String::from("p");
        let a = String::from("a");

        // get_structure p/3, x1
        env.get_structure(Functor(p.clone(), 3), 0);
        // unify_variable x2
        env.unify_variable(1);
        // unify_variable x3
        env.unify_variable(2);
        // unify_variable x4
        env.unify_variable(3);
        // get_structure f/1, x2
        env.get_structure(Functor(f.clone(), 1), 1);
        // unify_variable x5
        env.unify_variable(4);
        // get_structure h/2, x3
        env.get_structure(Functor(h.clone(), 1), 2);
        // unify_value x4
        env.unify_value(3);
        // unify_variable x6
        env.unify_variable(5);
        // get_structure f/1, x6
        env.get_structure(Functor(f.clone(), 1), 5);
        // unify_variable x7
        env.unify_variable(6);
        // get_structure a/0, x7
        env.get_structure(Functor(a.clone(), 0), 6);
    }

    #[test]
    fn test_functor_eq() {
        let f1 = Functor(String::from("foo"), 1);
        let f2 = Functor(String::from("bar"), 1);

        assert_ne!(f1, f2);

        let f2 = Functor(String::from("foo"), 1);
        assert_eq!(f1, f2);

        let f2 = Functor(String::from("foo"), 2);
        assert_ne!(f1, f2);
    }

    fn register_is(registers: &Registers, register: Register, cell: Cell) {
        assert_eq!(registers.get_x(register).cloned().unwrap(), cell);
    }
}
