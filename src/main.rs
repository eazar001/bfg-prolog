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
            StoreAddress::X(addr) => (self.registers.get_x(addr).unwrap(), addr),
        };

        match *cell {
            Ref(_) => {
                let h = self.heap.h;

                self.heap.cells.push(Str(h+1));
                self.heap.cells.push(Func(functor));
                self.bind(StoreAddress::Heap(address), StoreAddress::Heap(h));

                self.heap.h += 2;
                self.heap.mode = Write;
            },
            Str(a) => {
                match &self.heap.cells[a] {
                    Func(s_functor) => {
                        if s_functor == &functor {
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
            },
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
                    (Ref(_), _) => self.bind(d1, d2),
                    (_, Ref(_)) => self.bind(d1, d2),
                    _ => {
                        let f1 = self.get_functor(c1);
                        let f2 = self.get_functor(c2);

                        if &f1 == &f2 {
                            let Functor(_, f1_arity) = f1;
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
    fn bind(&mut self, a1: StoreAddress, a2: StoreAddress) {
        let (c1, a1, c1_heap) = match a1 {
            StoreAddress::Heap(addr) => (&self.heap.cells[addr], addr, true),
            StoreAddress::X(addr) => (self.registers.get_x(addr).unwrap(), addr, false)
        };

        let (c2, a2, c2_heap) = match a2 {
            StoreAddress::Heap(addr) => (&self.heap.cells[addr], addr, true),
            StoreAddress::X(addr) => (self.registers.get_x(addr).unwrap(), addr, false)
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
                self.registers.insert_x(a1, c2);
            }
        } else {
            if c2_heap {
                self.heap.cells[a2] = c1.clone();
                self.trail(a2);
            } else {
                let c1 = c1.clone();
                self.registers.insert_x(a2, c1);
            }
        }
    }

    #[allow(dead_code)]
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
        env.set_variable(2);
        env.set_variable(3);
        env.set_value(2);

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
        register_is(&registers, 2, Ref(2));
        register_is(&registers, 3, Ref(3));
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

    fn register_is(registers: &Registers, register: RegisterAddress, cell: Cell) {
        assert_eq!(registers.get_x(register).cloned().unwrap(), cell);
    }
}
