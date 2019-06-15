use self::CellTag::{STR, REF};
use self::Mode::{Read, Write};
use std::collections::HashMap;


#[derive(Debug, Copy, Clone)]
pub enum CellTag {
    STR,
    REF
}

#[derive(Debug, Copy, Clone)]
pub enum Mode {
    Read,
    Write
}

#[derive(Debug, Clone)]
pub struct Cell {
    cell_tag: Option<CellTag>,
    cell_ref: Option<usize>,
    functor: Option<Functor>
}

#[derive(Debug, Clone)]
pub struct Functor {
    name: String,
    arity: usize
}

#[derive(Debug, Clone)]
pub struct Heap {
    pub pointer: usize,
    pub cells: Vec<Cell>,
    mode: Mode
}

#[derive(Debug, Clone)]
pub struct Register {
    // variable register mapping heap location to cell data (x-register)
    pub variable: HashMap<usize, Cell>,
    pub variable_pointer: usize,
    // subterm register containing heap address of next subterm to be matched (s-register)
    pub subterm: usize
}

#[derive(Debug, Clone)]
pub struct Env {
    pub heap: Heap,
    pub register: Register
}

impl Env {
    pub fn new() -> Env {
        Env {
            heap: Heap::new(Vec::new()),
            register: Register::new()
        }
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn register(&self) -> &Register {
        &self.register
    }

    pub fn heap_ptr(&self) -> &usize {
        &self.heap.pointer
    }

    pub fn set_heap_ptr(&mut self, val: usize) {
        self.heap.pointer = val;
    }

    #[allow(dead_code)]
    pub fn put_structure(&mut self, register: usize, functor_cell: Cell, mut struct_cell: Cell) {
        let h = self.heap.pointer;

        // HEAP[H] <- <STR, H+1>
        &struct_cell.set_ref(h + 1);
        self.heap.cells.push(struct_cell);

        // HEAP[H+1] <- f/n
        self.heap.cells.push(functor_cell);

        // Xi <- HEAP[H]
        self.register.insert(register, self.heap.cells[h].clone());

        // H <- H + 2
        self.heap.pointer += 2;
    }

    #[allow(dead_code)]
    pub fn set_variable(&mut self, register: usize, ref_cell: &Cell) {
        let h = self.heap.pointer;

        // HEAP[H] <- <REF, H>
        self.heap.cells.push(ref_cell.clone());

        // Xi <- HEAP[H]
        self.register.insert(register, self.heap.cells[h].clone());

        // H <- H + 1
        self.heap.pointer += 1;
    }

    #[allow(dead_code)]
    pub fn set_value(&mut self, register: usize) {
        // HEAP[H] <- Xi
        self.heap.cells.push(self.register.get(register).unwrap().clone());

        // H <- H + 1
        self.heap.pointer += 1;
    }

    // TODO: Make this iterative
    #[allow(dead_code)]
    fn deref(address: usize, store: &mut impl Store) -> usize {
        let cell: (CellTag, usize) = match store.get(address) {
            Some(cell) => (cell.cell_tag.unwrap(), cell.cell_ref.unwrap()),
            None => panic!("No cell value")
        };

        match cell {
            (REF, value) => {
                if value != address {
                    Self::deref(value, store)
                } else {
                    address
                }
            },
            (STR, _) => address
        }
    }

    #[allow(dead_code)]
    pub fn get_structure(&mut self, functor: Functor, register: usize, store: &impl Store) -> bool {
        let address = Self::deref(register, &mut self.register);

        let cell = store.get(address).unwrap();

        match (cell.cell_tag.unwrap(), cell.cell_ref) {
            (REF, _) => {
                let h = self.heap.pointer;

                self.heap.cells.push(Cell::new(STR, h + 1));
                self.heap.cells.push(Cell::new_functor(functor));
                Self::bind(address, h);

                self.heap.pointer += 2;
                self.heap.mode = Write;
            },

            (STR, Some(a)) => {
                let name = functor.name;
                let arity = functor.arity;
                let heap_a_cell = self.heap.get(a).cloned().unwrap().clone();
                let heap_a_functor = heap_a_cell.functor.unwrap();
                let f_name = &heap_a_functor.name;
                let f_arity = &heap_a_functor.arity;

                if *f_name == name && *f_arity == arity {
                    self.register.subterm += 1;
                    self.heap.mode = Read;
                } else {
                    return false
                }
            },

            _ => return false
        }

        true
    }

    #[allow(dead_code)]
    pub fn unify_variable(&mut self, cell: Cell, slot: usize, h: usize, mode: Mode) {
        match mode {
            Read => {
                self.register.insert(slot, self.heap.cells[h].clone());
            },
            Write => {
                self.heap.cells[h] = cell;
                self.register.insert(slot, self.heap.cells[h].clone());
                self.heap.pointer += 1;
            }
        }

        self.register.subterm += 1;
    }

    #[allow(dead_code)]
    fn unify_value(&mut self, register: usize, h: usize, s: usize, mode: Mode) {
        match mode {
            Read => self.unify(register, s), //Self::unify(register, s),
            Write => {
                self.heap.cells[h] = self.register.variable.get(&register).unwrap().clone();
                self.heap.pointer += 1;
            }
        }

        self.register.subterm += 1;
    }

    #[allow(dead_code)]
    fn bind(address: usize, heap_address: usize) {
        unimplemented!()
    }

    #[allow(dead_code)]
    fn unify(&mut self, register: usize, s: usize) {
        unimplemented!()
    }
}

impl Cell {
    pub fn new(cell_tag: CellTag, cell_ref: usize) -> Cell {
        Cell {
            cell_tag: Some(cell_tag),
            cell_ref: Some(cell_ref),
            functor: None
        }
    }

    pub fn new_functor(functor: Functor) -> Cell {
        Cell {
            cell_tag: None,
            cell_ref: None,
            functor: Some(functor)
        }
    }

    pub fn set_ref(&mut self, cell_ref: usize) -> &Self {
        self.cell_ref = Some(cell_ref);
        self
    }
}

pub trait Store {
    fn get(&self, slot: usize) -> Option<&Cell>;
}

impl<'a> Register {
    pub fn new() -> Register {
        Register {
            variable: HashMap::new(),
            variable_pointer: 0,
            subterm: 0
        }
    }

    pub fn insert(&mut self, slot: usize, cell: Cell) -> Option<Cell> {
        self.variable.insert(slot, cell)
    }
}

impl<'a> Store for Register {
    fn get(&self, slot: usize) -> Option<&Cell> {
        self.variable.get(&slot).map(|cell| cell)
    }
}

impl<'a> Heap {
    pub fn new(heap: Vec<Cell>) -> Heap {
        Heap {
            pointer: 0,
            cells: heap,
            mode: Read
        }
    }
}

impl<'a> Store for Heap {
    fn get(&self, slot: usize) -> Option<&Cell> {
        Some(&self.cells[slot])
    }
}


fn main() {
    let env = Env::new();

    println!("{:?}", env);
}
