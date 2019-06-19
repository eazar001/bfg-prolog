use self::Cell::*;
use self::Mode::{Read, Write};
use std::collections::HashMap;


type HeapAddress = usize;
type RegisterAddress = usize;
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
enum StoreAddress {
    Heap(HeapAddress),
    X(RegisterAddress)
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
    x: HashMap<RegisterAddress, Cell>,
    // subterm register containing heap address of next subterm to be matched (s-register)
    s: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Env {
    heap: Heap,
    // the "push-down-list" contains StoreAddresses and serves as a unification stack
    pdl: Vec<StoreAddress>,
    registers: Registers,
    fail: bool,
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

    #[allow(dead_code)]
    fn empty_pdl(&mut self) -> bool {
        self.pdl.is_empty()
    }

    #[allow(dead_code)]
    fn push_pdl(&mut self, address: StoreAddress) {
        self.pdl.push(address);
    }

    #[allow(dead_code)]
    fn pop_pdl(&mut self) -> Option<StoreAddress> {
        self.pdl.pop()
    }

    // put_structure f/n, Xi
    #[allow(dead_code)]
    fn put_structure(&mut self, functor: Functor, register: RegisterAddress) {
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
    fn set_variable(&mut self, register: RegisterAddress) {
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
    fn set_value(&mut self, register: RegisterAddress) {
        // HEAP[H] <- Xi
        self.heap.cells.push(self.registers.get_x(register).unwrap().clone());

        // H <- H + 1
        self.heap.h += 1;
    }

    #[allow(dead_code)]
    fn deref(&self, mut address: StoreAddress) -> StoreAddress {
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
                        address = StoreAddress::Heap(value);
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
    fn get_structure(&mut self, functor: Functor, register: usize) {
        let (cell, address) = match self.deref(StoreAddress::X(register)) {
            StoreAddress::Heap(addr) => (&self.heap.cells[addr], addr),
            StoreAddress::X(addr) => (self.registers.get_x(register).unwrap(), addr),
        };

        match *cell {
            Ref(_) => {
                let h = &self.heap.h;

                self.heap.cells.push(Str(*h+1));
                self.heap.cells.push(Func(functor));
                Self::bind(StoreAddress::Heap(address), StoreAddress::Heap(*h));

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
                            self.fail = true;
                        }
                    }
                    _ => {
                        self.fail = true;
                    }
                }
            }
            Func(_) => {
                self.fail = true;
            }
        }
    }

    // unify_variable Xi
    #[allow(dead_code)]
    fn unify_variable(&mut self, register: RegisterAddress) {
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
                self.unify(StoreAddress::X(register), StoreAddress::Heap(s))
            },
            Write => {
                self.heap.cells.push(self.registers.get_x(register).unwrap().clone());
                self.heap.h += 1;
            }
        }

        self.registers.s += 1;
    }

    #[allow(dead_code)]
    fn unify(&mut self, a1: StoreAddress, a2: StoreAddress) {
        self.push_pdl(a1);
        self.push_pdl(a2);

        self.fail = false;

        while !(self.pdl.is_empty() || self.fail) {
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
                    (Ref(_), _) => Self::bind(d1, d2),
                    (_, Ref(_)) => Self::bind(d1, d2),
                    _ => {
                        let f1 = self.get_functor(c1);
                        let f2 = self.get_functor(c2);

                        let Functor(f1_name, f1_arity) = f1;
                        let Functor(f2_name, f2_arity) = f2;

                        if f1_name == f2_name && f1_arity == f2_arity {
                            for i in 1..f1_arity+1 {
                                self.push_pdl(StoreAddress::Heap(v1+i));
                                self.push_pdl(StoreAddress::Heap(v2+i));
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

    fn get_store_cell(&self, address: StoreAddress) -> &Cell {
        match address {
            StoreAddress::Heap(addr) => &self.heap.cells[addr],
            StoreAddress::X(addr) => self.registers.get_x(addr).unwrap()
        }
    }

    #[allow(dead_code)]
    fn bind(_address: StoreAddress, _heap_address: StoreAddress) {
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

    fn get_x(&self, register: RegisterAddress) -> Option<&Cell> {
        self.x.get(&register)
    }

    fn insert_x(&mut self, register: RegisterAddress, cell: Cell) -> Option<Cell> {
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

    println!("init: {:?}", env);
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_m0_1() {
        // p(Z, h(Z, W), f(W))
        let mut env = Env::new();

        // put_structure h/2, x3
        env.put_structure(Functor(String::from("h"), 2), 2);
        // set_variable, x2
        env.set_variable(1);
        // set_variable, x5
        env.set_variable(4);
        // put_structure f/1, x4
        env.put_structure(Functor(String::from("f"), 1), 3);
        // set_value, x5
        env.set_value(4);
        // put_structure p/3, x1
        env.put_structure(Functor(String::from("p"), 3), 0);
        // set_value x2
        env.set_value(1);
        // set_value x3
        env.set_value(2);
        // set_value x4
        env.set_value(3);

        let h = String::from("h");
        let f = String::from("f");
        let p = String::from("p");

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

        let (heap_cells, registers) = (env.heap.cells, env.registers);
        assert_eq!(heap_cells, expected_heap_cells);

        assert_eq!(registers.get_x(0).cloned().unwrap(), Str(8));
        assert_eq!(registers.get_x(1).cloned().unwrap(), Ref(2));
        assert_eq!(registers.get_x(2).cloned().unwrap(), Str(1));
        assert_eq!(registers.get_x(3).cloned().unwrap(), Str(5));
        assert_eq!(registers.get_x(4).cloned().unwrap(), Ref(3));
    }
}
