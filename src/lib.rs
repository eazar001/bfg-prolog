#![allow(dead_code)]
#![allow(unused)]
#![allow(clippy::new_without_default)]

pub mod ast;

use self::Cell::*;
use self::Store::*;
use self::Mode::{Read, Write};
use self::ast::*;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter, Debug};
use std::cmp::Ordering;
use env_logger;
use log::{info, warn, error, debug, trace, Level};
use log::Level::*;
use lalrpop_util::lalrpop_mod;
use std::hash::Hash;

lalrpop_mod!(pub parser);


// heap address represented as usize that corresponds to the vector containing cell data
type HeapAddress = usize;
// x-register address which identifies the register that holds the cell data in the corresponding variable
pub type Register = usize;
type FunctorArity = usize;
type FunctorName = String;
// the "global stack"
type Heap = Vec<Cell>;
type TermMap = HashMap<Term, Register>;
type RegisterMap = HashMap<Register, Term>;
type TermSet = HashSet<Term>;
type Instructions = Vec<Instruction>;
type QueryBindings = Vec<String>;
type ProgramBindings = Vec<String>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    PutStructure(Functor, Register),
    GetStructure(Functor, Register),
    SetVariable(Register),
    UnifyVariable(Register),
    SetValue(Register),
    UnifyValue(Register),
    PutVariable(Register, Register),
    PutValue(Register, Register),
    GetValue(Register, Register),
    GetVariable(Register, Register),
    Call(Functor),
    Proceed
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Functor(pub FunctorName, pub FunctorArity);

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Cell {
    Str(HeapAddress),
    Ref(HeapAddress),
    Func(Functor)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Store {
    HeapAddr(HeapAddress),
    XAddr(Register)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Mode {
    Read,
    Write
}

#[derive(Clone, Eq, PartialEq)]
struct Registers {
    // the "h" counter contains the location of the next cell to be pushed onto the heap
    h: HeapAddress,
    // variable register mapping a variable to cell data (x-register)
    x: HashMap<Register, Cell>,
    // subterm register containing heap address of next subterm to be matched (s-register)
    s: Register,
    // program/instruction counter, containing address of the next instruction to be executed
    p: Register,
    // address of the next instruction in the code area to follow up after successful return from a call
    cp: Register
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Machine {
    heap: Heap,
    // the "push-down-list" contains StoreAddresses and serves as a unification stack
    pdl: Vec<Store>,
    code: Instructions,
    code_address: HashMap<Functor, usize>,
    registers: Registers,
    mode: Mode,
    fail: bool,
}

impl Debug for Registers {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self.x.len() {
            0 => Ok(write!(f, "{:?}", self.x.len())?),
            _ => {
                let mut keys: Vec<&usize> = self.x.keys().collect();
                keys.sort();

                write!(f, "[")?;

                for key in &keys[..keys.len()-1] {
                    write!(f, "{}: {:?}, ", key, self.x[key])?;
                }

                Ok(write!(f, "{}: {:?}]", keys.len(), self.x[&keys.len()])?)
            }
        }
    }
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Ref(a) => Ok(write!(f, "{:?}", Ref(*a))?),
            Str(a) => Ok(write!(f, "{:?}", Str(*a))?),
            Func(f1) => Ok(write!(f, "Functor({})", f1)?)
        }
    }
}

impl Display for Functor {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Ok(write!(f, "{}/{}", self.name(), self.arity())?)
    }
}

impl From<&str> for Functor {
    fn from(s: &str) -> Functor {
        let v: Vec<&str> = s.split('/').collect();

        assert_eq!(v.len(), 2);

        Functor(String::from(v[0]), String::from(v[1]).parse().unwrap())
    }
}

impl From<&str> for Cell {
    fn from(s: &str) -> Cell {
        Func(Functor::from(s))
    }
}

impl Functor {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn arity(&self) -> usize {
        self.1
    }
}

impl Machine {
    pub fn new() -> Machine {
        Machine {
            heap: Heap::new(),
            pdl: Vec::new(),
            code: Vec::new(),
            code_address: HashMap::new(),
            registers: Registers::new(),
            mode: Read,
            fail: false
        }
    }

    fn execute(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::PutStructure(f, x) => self.put_structure(f.clone(), *x),
            Instruction::GetStructure(f, x) => self.get_structure(f.clone(), *x),
            Instruction::SetValue(x) => self.set_value(*x),
            Instruction::UnifyValue(x) => self.unify_value(*x),
            Instruction::SetVariable(x) => self.set_variable(*x),
            Instruction::UnifyVariable(x) => self.unify_variable(*x),
            Instruction::PutVariable(x, a) => self.put_variable(*x, *a),
            Instruction::PutValue(x, a) => self.put_value(*x, *a),
            Instruction::GetValue(x, a) => self.get_value(*x, *a),
            Instruction::GetVariable(x, a) => self.get_variable(*x, *a),
            Instruction::Call(f) => self.call(f.clone()),
            Instruction::Proceed => self.proceed()
        }
    }

    fn push_instruction(&mut self, instruction: Instruction) {
        self.code.push(instruction);
        let ia = self.get_p();
        self.set_p(ia + 1);
    }

    pub fn get_code(&self) -> &Instructions {
        &self.code
    }

    fn push_code_address(&mut self, fact: Functor) {
        let a = self.code.len();
        self.code_address.insert(fact, a);
    }

    fn get_code_address(&self, fact: Functor) -> usize {
        *self.code_address.get(&fact).unwrap()
    }

    pub fn get_heap(&self) -> &Heap {
        &self.heap
    }

    pub fn get_x_registers(&self) -> &HashMap<Register, Cell> {
        &self.registers.x
    }

    fn push_heap(&mut self, cell: Cell) {
        trace!("\t\tHEAP[{}] <- {}", self.heap.len(), cell);

        self.heap.push(cell);
    }

    pub fn is_true(&self) -> bool {
        !self.fail
    }

    pub fn is_false(&self) -> bool {
        self.fail
    }

    pub fn get_x(&self, xi: Register) -> Option<&Cell> {
        self.registers.x.get(&xi)
    }

    fn insert_x(&mut self, xi: Register, cell: Cell) -> Option<Cell> {
        trace!("\t\tX{} <- {:?}", xi, cell);

        self.registers.x.insert(xi, cell)
    }

    fn get_s(&self) -> Register {
        self.registers.s
    }

    fn inc_s(&mut self, value: usize) {
        trace!("\t\tS <- S + {}", value);

        self.registers.s += value;
    }

    fn set_s(&mut self, value: usize) {
        trace!("\t\tS <- {}", value);

        self.registers.s = value;
    }

    fn set_fail(&mut self, value: bool) {
        trace!("\t\tFail <- {}", value);

        self.fail = value;
    }

    fn get_h(&self) -> usize {
        self.registers.h
    }

    fn get_p(&self) -> usize {
        self.registers.p
    }

    fn set_p(&mut self, value: usize) {
        self.registers.p = value;
    }

    fn get_cp(&self) -> usize {
        self.registers.cp
    }

    fn set_cp(&mut self, value: usize) {
        self.registers.cp = value;
    }

    fn inc_h(&mut self, value: usize) {
        trace!("\t\tH <- H + {}", value);

        self.registers.h += value;
    }

    fn set_mode(&mut self, mode: Mode) {
        trace!("\t\tMode <- {:?}", mode);

        self.mode = mode;
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

    fn put_structure(&mut self, f: Functor, xi: Register) {
        trace!("put_structure {}, {}:", f, xi);

        let h = self.get_h();

        self.push_heap(Str(h+1));
        self.push_heap(Func(f));
        self.insert_x(xi, Str(h+1));
        self.inc_h(2);
    }

    fn put_variable(&mut self, xn: Register, ai: Register) {
        trace!("put_variable {}, {}", xn, ai);

        let h = self.get_h();

        self.push_heap(Ref(h));
        self.insert_x(xn, Ref(h));
        self.insert_x(ai, Ref(h));
        self.inc_h(1);
    }

    fn put_value(&mut self, xn: Register, ai: Register) {
        trace!("put_value {}, {}", xn, ai);

        self.insert_x(ai, self.get_x(xn).cloned().unwrap());
    }

    fn set_variable(&mut self, xi: Register) {
        let h = self.get_h();

        trace!("set_variable {}:", xi);

        self.push_heap(Ref(h));
        self.insert_x(xi, Ref(h));
        self.inc_h(1);
    }

    fn set_value(&mut self, xi: Register) {
        trace!("set_value {}:", xi);

        self.push_heap(self.get_x(xi).cloned().unwrap());
        self.inc_h(1);
    }

    fn deref(&self, address: Store) -> Store {
        let mut address = address;
        let start_address = address;

        loop {
            let (cell, a) = match address {
                HeapAddr(addr) => (&self.heap[addr], addr),
                XAddr(addr) => {
                    let e = &format!("Illegal access: register {}, does not exist", addr);
                    let c = self.get_x(addr).expect(e);

                    (c, addr)
                }
            };

            match cell {
                Ref(value) => {
                    if *value != a {
                        // keep following the reference chain
                        address = HeapAddr(*value);
                    } else {
                        // ref cell is unbound return the address
                        trace!("\t\tderef: {:?} -> {:?}", start_address, address);
                        return address
                    }
                },
                Str(_) => {
                    trace!("\t\tderef: {:?} -> {:?}", start_address, address);
                    return address
                },
                Func(_) => {
                    trace!("\t\tderef: {:?} -> {:?}", start_address, address);
                    return address
                }
            }
        }
    }

    fn call(&mut self, f: Functor) {
        let a = self.get_code_address(f);
        self.set_p(a);
    }

    // address of procedure in the code area
    fn cp_address(&self, f: Functor) -> usize {
        unimplemented!()
    }

    fn proceed(&mut self) {
        // implemented in L1 as NOP
    }

    fn get_variable(&mut self, xn: Register, ai: Register) {
        trace!("get_variable {}, {}", xn, ai);

        self.insert_x(xn, self.get_x(ai).cloned().unwrap());
    }

    fn get_value(&mut self, xn: Register, ai: Register) {
        trace!("get_value {}, {}", xn, ai);

        self.unify(XAddr(xn), XAddr(ai));
    }

    fn get_structure(&mut self, f: Functor, xi: Register) {
        trace!("get_structure {}, {}:", f, xi);

        let (cell, address) = match self.deref(XAddr(xi)) {
            HeapAddr(addr) => (&self.heap[addr], addr),
            XAddr(addr) => (self.get_x(xi).unwrap(), addr)
        };

        match cell.clone() {
            Ref(_) => {
                let h = self.get_h();

                self.push_heap(Str(h+1));
                self.push_heap(Func(f.clone()));
                self.bind(HeapAddr(address), HeapAddr(h));

                self.inc_h(2);
                self.set_mode(Write);
            },
            Str(a) => {
                match self.heap[a] {
                    Func(ref functor) => {
                        if *functor == f {
                            self.set_s(a+1);
                            self.set_mode(Read);
                        } else {
                            self.set_fail(true);
                        }
                    }
                    _ => panic!()
                }
            },
            Func(_) => {
                self.set_fail(true);
            }
        }
    }

    fn unify_variable(&mut self, xi: Register) {
        trace!("unify_variable {} ({:?}): ", xi, self.mode);

        match self.mode {
            Read => {
                let s = self.get_s();

                self.insert_x(xi, self.heap[s].clone());
            },
            Write => {
                let h = self.get_h();

                self.push_heap(Ref(h));
                self.insert_x(xi, Ref(h));
                self.inc_h(1);
            }
        }

        self.inc_s(1);
    }

    fn unify_value(&mut self, xi: Register) {
        trace!("unify_value {} ({:?}): ", xi, self.mode);

        match self.mode {
            Read => {
                let s = self.get_s();

                self.unify(XAddr(xi), HeapAddr(s))
            },
            Write => {
                self.push_heap(self.get_x(xi).unwrap().clone());
                self.inc_h(1);
            }
        }

        self.inc_s(1);
    }

    pub fn unify(&mut self, a1: Store, a2: Store) {
        trace!("\t\tunify {:?}, {:?}:", a1, a2);

        self.push_pdl(a1);
        self.push_pdl(a2);

        self.set_fail(false);

        while !(self.empty_pdl() || self.fail) {
            let (a1, a2) = (self.pop_pdl().unwrap(), self.pop_pdl().unwrap());

            let d1 = self.deref(a1);
            let d2 = self.deref(a2);

            if d1 != d2 {
                let c1 = self.get_store_cell(d1);
                let c2 = self.get_store_cell(d2);

                if c1.is_ref() || c2.is_ref() {
                    self.bind(d1, d2);
                } else {
                    let (v1, v2) = (c1.address().unwrap(), c2.address().unwrap());
                    let (f1, f2) = (self.get_functor(c1), self.get_functor(c2));

                    if f1 == f2 {
                        let n1 = f1.arity();

                        for i in 1..=n1 {
                            self.push_pdl(HeapAddr(v1+i));
                            self.push_pdl(HeapAddr(v2+i));
                        }
                    } else {
                        self.set_fail(true);
                    }
                }
            }
        }
    }

    // extracts functor only if cell is a structure or a functor cell
    fn get_functor<'a>(&'a self, cell: &'a Cell) -> &'a Functor {
        match cell {
            Str(addr) => {
                if let Func(f) = &self.heap[*addr] {
                    trace!("\t\tget_functor: {:?} -> {}", cell, f);
                    &f
                } else {
                    error!("encountered a structure that doesn't point to a functor");
                    panic!("invalid cell: structure cell pointing to non-functor data")
                }
            },
            Func(f) => {
                warn!("accessing a functor from a functor-cell, but this normally shouldn't happen");
                trace!("\t\tget_functor: {:?} -> {}", cell, f);
                f
            },
            Ref(_) => {
                error!("tried getting a functor from a ref-cell");
                panic!("invalid cell-type for functor retrieval used");
            }
        }
    }

    fn get_store_cell(&self, address: Store) -> &Cell {
        match address {
            HeapAddr(addr) => &self.heap[addr],
            XAddr(addr) => self.get_x(addr).unwrap()
        }
    }

    fn bind(&mut self, a1: Store, a2: Store) {
        let (c1, c2) = (self.get_store_cell(a1), self.get_store_cell(a2));
        let (a1, a2) = (c1.address().unwrap(), c2.address().unwrap());

        if c1.is_ref() && (!c2.is_ref() || a2 < a1) {
            trace!("\t\tbind: HEAP[{1}] <- {:2?} | ({:3?} -> {:2?})", a1, c2.clone(), c1.clone());
            self.heap[a1] = c2.clone();
        } else {
            trace!("\t\tbind: HEAP[{1}] <- {:2?} | ({:3?} -> {:2?})", a2, c1.clone(), c2.clone());
            self.heap[a2] = c1.clone();
        }
    }
}

impl Registers {
    fn new() -> Registers {
        Registers {
            h: 0,
            x: HashMap::new(),
            s: 0,
            p: 0,
            cp: 0
        }
    }
}

impl Cell {
    fn is_ref(&self) -> bool {
        if let Ref(_) = self {
            return true
        }

        false
    }

    fn is_str(&self) -> bool {
        if let Str(_) = self {
            return true
        }

        false
    }

    fn is_func(&self) -> bool {
        if let Func(_) = self {
            return true
        }

        false
    }

    pub fn address(&self) -> Option<HeapAddress> {
        match self {
            Str(addr) => Some(*addr),
            Ref(addr) => Some(*addr),
            Func(_) => None
        }
    }
}

impl Store {
    fn is_heap(&self) -> bool {
        if let HeapAddr(_) = self {
            return true
        }

        false
    }

    fn is_x(&self) -> bool {
        if let XAddr(_) = self {
            return true
        }

        false
    }

    fn address(&self) -> usize {
        match self {
            HeapAddr(addr) => *addr,
            XAddr(addr) => *addr
        }
    }
}

// TODO: make this iterative
fn allocate_query_registers(compound: &Compound, x: &mut usize, m: &mut TermMap, seen: &mut TermSet, instructions: &mut Instructions) {
    let term = Term::Compound(compound.clone());

    if !m.contains_key(&term) {
        m.insert(term, *x);
        *x += 1;
    }

    for t in &compound.args {
        if !m.contains_key(&t) {
            m.insert(t.clone(), *x);
            *x += 1;
        }
    }

    for t in &compound.args {
        if let Term::Compound(ref c) = t {
            allocate_query_registers(c, x, m, seen, instructions);
        }
    }

    let f = Functor(compound.name.clone(), compound.arity);
    let t = Term::Compound(compound.clone());

    instructions.push(Instruction::PutStructure(f, *m.get(&t).unwrap()));
    seen.insert(t);

    for t in &compound.args {
        if !seen.contains(t) {
            instructions.push(Instruction::SetVariable(*m.get(t).unwrap()));
            seen.insert(t.clone());
        } else {
            instructions.push(Instruction::SetValue(*m.get(t).unwrap()));
        }
    }
}

// TODO: make this iterative
fn allocate_program_registers(root: bool, compound: &Compound, x: &mut usize, m: &mut TermMap, seen: &mut TermSet, arg_instructions: &mut Instructions, instructions: &mut Instructions) {
    let term = Term::Compound(compound.clone());

    if !m.contains_key(&term) {
        m.insert(term, *x);
        *x += 1;
    }

    for t in &compound.args {
        if !m.contains_key(&t) {
            m.insert(t.clone(), *x);
            *x += 1;
        }
    }

    let f = Functor(compound.name.clone(), compound.arity);
    let t = Term::Compound(compound.clone());

    if root {
        arg_instructions.push(Instruction::GetStructure(f, *m.get(&t).unwrap()));
    } else {
        instructions.push(Instruction::GetStructure(f, *m.get(&t).unwrap()));
    }

    seen.insert(t);

    for t in &compound.args {
        if !seen.contains(t) {
            if root {
                arg_instructions.push(Instruction::UnifyVariable(*m.get(t).unwrap()));
            } else {
                instructions.push(Instruction::UnifyVariable(*m.get(t).unwrap()));
            }

            seen.insert(t.clone());
        } else {
            if root {
                arg_instructions.push(Instruction::UnifyValue(*m.get(t).unwrap()));
            } else {
                instructions.push(Instruction::UnifyValue(*m.get(t).unwrap()));
            }
        }
    }

    for t in &compound.args {
        if let Term::Compound(ref c) = t {
            allocate_program_registers(false, c, x, m, seen, arg_instructions, instructions);
        }
    }
}

fn compile_query<T: Structuralize>(term: &T) -> (Instructions, TermMap) {
    let mut m = HashMap::new();
    let mut instructions = Vec::new();
    let mut seen = HashSet::new();

    let compound = term.structuralize().unwrap();

    for (i, arg) in compound.args.iter().enumerate() {
        let a = i + 1;
        let mut x = a + compound.arity;

        if let Term::Var(_) = arg {
            if !seen.contains(arg) {
                instructions.push(Instruction::PutVariable(x, a));
                m.insert(arg.clone(), x);
                seen.insert(arg.clone());
            } else {
                instructions.push(Instruction::PutValue(*m.get(arg).unwrap(), a));
            }
        } else {
            m.insert(arg.clone(), a);
            seen.insert(arg.clone());
            allocate_query_registers(&arg.structuralize().unwrap(), &mut x, &mut m, &mut seen, &mut instructions);
        }
    }

    instructions.push(Instruction::Call(Functor(compound.name.clone(), compound.arity)));

    (instructions, m)
}

fn compile_program<T: Structuralize>(term: &T) -> (Instructions, TermMap) {
    let mut m = HashMap::new();
    let mut arg_instructions = Vec::new();
    let mut instructions = Vec::new();
    let mut seen = HashSet::new();

    let compound = term.structuralize().unwrap();

    for (i, arg) in compound.args.iter().enumerate() {
        let a = i + 1;
        let mut x = a + compound.arity;

        if let Term::Var(_) = arg {
            if !seen.contains(arg) {
                arg_instructions.push(Instruction::GetVariable(x, a));
                m.insert(arg.clone(), x);
                seen.insert(arg.clone());
            } else {
                arg_instructions.push(Instruction::GetValue(*m.get(arg).unwrap(), a));
            }
        } else {
            m.insert(arg.clone(), a);
            seen.insert(arg.clone());
            allocate_program_registers(true, &arg.structuralize().unwrap(), &mut x, &mut m, &mut seen, &mut arg_instructions, &mut instructions);
        }
    }

    instructions.push(Instruction::Proceed);
    arg_instructions.extend_from_slice(&instructions);
    let instructions = arg_instructions;

    (instructions, m)
}

pub fn query(m: &mut Machine, q: &str) -> HashMap<Cell, Term> {
    let e = parser::ExpressionParser::new();
    let mut query = e.parse(q).unwrap();

    if let t@Term::Compound(_) | t@Term::Atom(_) = &mut query {
        let (instructions, term_map) = compile_query(t);
        let mut output = HashMap::new();

        {
            let compound = t.structuralize().unwrap();
            let name = compound.name;
            let arity = compound.arity;
            let query_functor = Functor(name, arity);

            m.push_code_address(query_functor);

            for instruction in &instructions {
                m.push_instruction(instruction.clone());
            }
        }

        // TODO: this is very inefficient; make this an internal machine operation
        for instruction in &m.get_code().clone() {
            m.execute(instruction);
        }

        for (term, x) in &term_map {
            output.insert(m.get_x(*x).unwrap().clone(), term.clone());
        }

        output
    } else {
        panic!("not supported yet")
    }
}

// execute a query against the program term and display the results of the bindings (if any)
pub fn run_query(m: &mut Machine, q: &str, p: &str) -> (QueryBindings, ProgramBindings) {
    let query_map = query(m, q);
    let program_map = program(m, p);

    let mut display_vec = Vec::new();
    let mut query_bindings = Vec::new();
    let mut program_bindings = Vec::new();

    display_vec.extend(query_map);

    for (cell, term) in &display_vec {
        match cell {
            Cell::Ref(a) | Cell::Str(a) => {
                if let ast::Term::Var(_) = term {
                    let mut buffer = String::new();
                    resolve_term(&m, *a, &display_vec, &mut buffer);

                    if buffer != term.to_string() {
                        query_bindings.push(format!("{} = {}", term, buffer));
                    }
                }
            },
            _ => ()
        }
    }

    display_vec.extend(program_map);

    for (cell, term) in &display_vec {
        match cell {
            Cell::Ref(a) | Cell::Str(a) => {
                if let ast::Term::Var(_) = term {
                    let mut buffer = String::new();
                    resolve_term(&m, *a, &display_vec, &mut buffer);

                    if buffer != term.to_string() {
                        let program_binding = format!("{} = {}", term, buffer);

                        if !query_bindings.contains(&program_binding) {
                            program_bindings.push(format!("{} = {}", term, buffer));
                        }

                    }
                }
            },
            _ => ()
        }
    }

    query_bindings.sort();
    program_bindings.sort();

    (query_bindings, program_bindings)
}

pub fn program(m: &mut Machine, p: &str) -> HashMap<Cell, Term> {
    let e = parser::ExpressionParser::new();
    let mut program = e.parse(p).unwrap();

    if let t@Term::Compound(_) | t@Term::Atom(_) = &mut program {
        let (instructions, term_map) = compile_program(t);
        let mut output = HashMap::new();

        for instruction in &instructions {
            m.execute(instruction);
        }

        for (term, x) in &term_map {
            output.insert(m.get_x(*x).unwrap().clone(), term.clone());
        }

        output
    } else {
        panic!("not supported yet")
    }
}

pub fn resolve_term(m: &Machine, addr: HeapAddress, display_map: &[(Cell, Term)], term_string: &mut String) {
    let d = m.deref(Store::HeapAddr(addr));
    let cell = m.get_store_cell(d);


    match cell {
        Cell::Func(Functor(name, arity)) => {
            if *arity == 0 {
                term_string.push_str(name);
            } else {
                term_string.push_str(&format!("{}(", name));
            }

            for i in 1..=*arity {
                resolve_term(&m, d.address() + i, display_map, term_string);

                if i != *arity {
                    term_string.push_str(", ");
                }
            }

            if *arity > 0 {
                term_string.push_str(")");
            }
        },
        Cell::Str(a) => {
            resolve_term(&m, *a, display_map, term_string)
        },
        Cell::Ref(r) => {
            if *r == d.address() {
                for (cell, term) in display_map {
                    if let Ref(a) = cell {
                        if *a == *r {
                            if let Term::Var(_) = term {
                                let s = format!("{}", term);

                                term_string.push_str(&s);
                                break;
                            }
                        }
                    }
                }
            } else {
                resolve_term(&m, *r, display_map, term_string);
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn init_test_logger() {
        env_logger::builder()
            .is_test(true)
            .default_format_timestamp(false)
            .try_init()
            .unwrap()
    }

    #[test]
    fn test_set_variable() {
        let mut m = Machine::new();

        m.set_variable(0);

        let expected_heap_cells = vec![Ref(0)];
        let heap_cells = &m.heap;

        assert_eq!(heap_cells, &expected_heap_cells);
        register_is(&m, 0, Ref(0));
    }

    #[test]
    fn test_set_value() {
        let mut m = Machine::new();

        m.set_variable(0);
        m.set_variable(1);

        m.set_value(0);
        m.set_value(1);

        let expected_heap_cells = vec![Ref(0), Ref(1), Ref(0), Ref(1)];
        let heap_cells = &m.heap;

        assert_eq!(heap_cells, &expected_heap_cells);
        register_is(&m, 0, Ref(0));
        register_is(&m, 1, Ref(1));
        assert_eq!(m.registers.x.len(), 2);
    }

    #[test]
    fn test_put_structure() {
        let mut m = Machine::new();

        m.put_structure(Functor(String::from("foo"), 2), 0);
        m.set_variable(1);
        m.set_variable(2);
        m.set_value(1);

        let expected_heap_cells = vec![
            Str(1),
            Cell::from("foo/2"),
            Ref(2),
            Ref(3),
            Ref(2)
        ];

        let heap_cells = &m.heap;

        assert_eq!(heap_cells, &expected_heap_cells);
        register_is(&m, 0, Str(1));
        register_is(&m, 1, Ref(2));
        register_is(&m, 2, Ref(3));
        assert_eq!(m.registers.x.len(), 3);
    }

    #[test]
    fn test_deref() {
        let mut m = Machine::new();

        m.heap = vec![
            Ref(2),
            Ref(3),
            Ref(1),
            Ref(3),
            Str(5),
            Cell::from("f/2"),
            Ref(3)
        ];

        m.insert_x(3, Ref(4));

        assert_eq!(m.deref(HeapAddr(0)), HeapAddr(3));
        assert_eq!(m.deref(HeapAddr(1)), HeapAddr(3));
        assert_eq!(m.deref(HeapAddr(2)), HeapAddr(3));
        assert_eq!(m.deref(HeapAddr(3)), HeapAddr(3));
        assert_eq!(m.deref(HeapAddr(4)), HeapAddr(4));
        assert_eq!(m.deref(HeapAddr(5)), HeapAddr(5));
        assert_eq!(m.deref(HeapAddr(6)), HeapAddr(3));
        assert_eq!(m.deref(XAddr(3)), HeapAddr(4));
    }

    #[test]
    fn test_exercise_2_1() {
        let mut m = Machine::new();

        m.put_structure(Functor::from("h/2"), 3);
        m.set_variable(2);
        m.set_variable(5);
        m.put_structure(Functor::from("f/1"), 4);
        m.set_value(5);
        m.put_structure(Functor::from("p/3"), 1);
        m.set_value(2);
        m.set_value(3);
        m.set_value(4);


        let expected_heap_cells = vec![
            Str(1),
            Cell::from("h/2"),
            Ref(2),
            Ref(3),
            Str(5),
            Cell::from("f/1"),
            Ref(3),
            Str(8),
            Cell::from("p/3"),
            Ref(2),
            Str(1),
            Str(5),
        ];

        let heap_cells = &m.heap;
        assert_eq!(heap_cells, &expected_heap_cells);

        register_is(&m, 1, Str(8));
        register_is(&m, 2, Ref(2));
        register_is(&m, 3, Str(1));
        register_is(&m, 4, Str(5));
        register_is(&m, 5, Ref(3));
    }

    #[test]
    fn test_exercise_2_3() {
        let mut m = Machine::new();

        m.put_structure(Functor::from("h/2"), 3);
        m.set_variable(2);
        m.set_variable(5);
        m.put_structure(Functor::from("f/1"), 4);
        m.set_value(5);
        m.put_structure(Functor::from("p/3"), 1);
        m.set_value(2);
        m.set_value(3);
        m.set_value(4);

        m.get_structure(Functor::from("p/3"), 1);
        m.unify_variable(2);
        m.unify_variable(3);
        m.unify_variable(4);
        m.get_structure(Functor::from("f/1"), 2);
        m.unify_variable(5);
        m.get_structure(Functor::from("h/2"), 3);
        m.unify_value(4);
        m.unify_variable(6);
        m.get_structure(Functor::from("f/1"), 6);
        m.unify_variable(7);
        m.get_structure(Functor::from("a/0"), 7);

        let expected_heap_cells = vec![
            Str(1),
            Cell::from("h/2"),
            Str(13),
            Str(16),
            Str(5),
            Cell::from("f/1"),
            Ref(3),
            Str(8),
            Cell::from("p/3"),
            Ref(2),
            Str(1),
            Str(5),
            Str(13),
            Cell::from("f/1"),
            Ref(3),
            Str(16),
            Cell::from("f/1"),
            Str(19),
            Str(19),
            Cell::from("a/0")
        ];


        let heap_cells = &m.heap;
        assert_eq!(heap_cells, &expected_heap_cells);


        register_is(&m, 1, Str(8));
        register_is(&m, 2, Ref(2));
        register_is(&m, 3, Str(1));
        register_is(&m, 4, Str(5));
        register_is(&m, 5, Ref(14));
        register_is(&m, 6, Ref(3));
        register_is(&m, 7, Ref(17));
    }

    #[ignore]
    #[test]
    fn test_program_instruction_compilation_fig_2_3() {
        let e = parser::ExpressionParser::new();

        let expected_instructions = vec![
            Instruction::PutStructure(Functor::from("h/2"), 3),
            Instruction::SetVariable(2),
            Instruction::SetVariable(5),
            Instruction::PutStructure(Functor::from("f/1"), 4),
            Instruction::SetValue(5),
            Instruction::PutStructure(Functor::from("p/3"), 1),
            Instruction::SetValue(2),
            Instruction::SetValue(3),
            Instruction::SetValue(4)
        ];

        let mut p = e.parse("p(Z, h(Z, W), f(W)).").unwrap();
        let (instructions, _) = compile_query(&p);

        assert_eq!(instructions, expected_instructions);
    }

    #[ignore]
    #[test]
    fn test_program_instruction_compilation_fig_2_4() {
        let e = parser::ExpressionParser::new();

        let expected_instructions = vec![
            Instruction::GetStructure(Functor::from("p/3"), 1),
            Instruction::UnifyVariable(2),
            Instruction::UnifyVariable(3),
            Instruction::UnifyVariable(4),
            Instruction::GetStructure(Functor::from("f/1"), 2),
            Instruction::UnifyVariable(5),
            Instruction::GetStructure(Functor::from("h/2"), 3),
            Instruction::UnifyValue(4),
            Instruction::UnifyVariable(6),
            Instruction::GetStructure(Functor::from("f/1"), 6),
            Instruction::UnifyVariable(7),
            Instruction::GetStructure(Functor::from("a/0"), 7)
        ];

        let mut p = e.parse("p(f(X), h(Y, f(a)), Y).").unwrap();
        let (instructions, _) = compile_program(&p);

        assert_eq!(instructions, expected_instructions);
    }

    #[ignore]
    #[test]
    fn test_instruction_compilation_exercise_2_4() {
        let e = parser::ExpressionParser::new();

        let expected_query_instructions = vec![
            Instruction::PutStructure(Functor::from("f/1"), 2),
            Instruction::SetVariable(5),
            Instruction::PutStructure(Functor::from("a/0"), 7),
            Instruction::PutStructure(Functor::from("f/1"), 6),
            Instruction::SetValue(7),
            Instruction::PutStructure(Functor::from("h/2"), 3),
            Instruction::SetVariable(4),
            Instruction::SetValue(6),
            Instruction::PutStructure(Functor::from("p/3"), 1),
            Instruction::SetValue(2),
            Instruction::SetValue(3),
            Instruction::SetValue(4)
        ];

        let expected_program_instructions = vec![
            Instruction::GetStructure(Functor::from("p/3"), 1),
            Instruction::UnifyVariable(2),
            Instruction::UnifyVariable(3),
            Instruction::UnifyVariable(4),
            Instruction::GetStructure(Functor::from("h/2"), 3),
            Instruction::UnifyValue(2),
            Instruction::UnifyVariable(5),
            Instruction::GetStructure(Functor::from("f/1"), 4),
            Instruction::UnifyValue(5)
        ];

        let mut q = e.parse("p(f(X), h(Y, f(a)), Y).").unwrap();
        let mut p = e.parse("p(Z, h(Z, W), f(W)).").unwrap();

        let (query_instructions, _) = compile_query(&q);
        let (program_instructions, _) = compile_program(&p);

        assert_eq!(expected_query_instructions, query_instructions);
        assert_eq!(expected_program_instructions, program_instructions);
    }

    #[test]
    fn test_query_execution_2_6() {
        let mut m = Machine::new();


        m.put_variable(4, 1);
        m.put_structure(Functor::from("h/2"), 2);
        m.set_value(4);
        m.set_variable(5);
        m.put_structure(Functor::from("f/1"), 3);
        m.set_value(5);
//        m.call(Functor::from("p/3"));

        let expected_heap_cells = vec![
            Ref(0),
            Str(2),
            Cell::from("h/2"),
            Ref(0),
            Ref(4),
            Str(6),
            Cell::from("f/1"),
            Ref(4)
        ];

        let heap_cells = m.get_heap();
        assert_eq!(heap_cells, &expected_heap_cells);
    }

    #[test]
    fn test_exercise_2_7() {
        let mut m = Machine::new();

        m.put_variable(4, 1);
        m.put_structure(Functor::from("h/2"), 2);
        m.set_value(4);
        m.set_variable(5);
        m.put_structure(Functor::from("f/1"), 3);
        m.set_value(5);
//        m.call(Functor::from("p/3"));

        m.get_structure(Functor::from("f/1"), 1);
        m.unify_variable(4);
        m.get_structure(Functor::from("h/2"), 2);
        m.unify_variable(5);
        m.unify_variable(6);
        m.get_value(5, 3);
        m.get_structure(Functor::from("f/1"), 6);
        m.unify_variable(7);
        m.get_structure(Functor::from("a/0"), 7);
        m.proceed();

        let expected_heap_cells = vec![
            Str(9),
            Str(2),
            Cell::from("h/2"),
            Ref(0),
            Str(12),
            Str(6),
            Cell::from("f/1"),
            Ref(4),
            Str(9),
            Cell::from("f/1"),
            Ref(4),
            Str(12),
            Cell::from("f/1"),
            Str(15),
            Str(15),
            Cell::from("a/0")
        ];


        let heap_cells = &m.heap;
        assert_eq!(heap_cells, &expected_heap_cells);


        register_is(&m, 1, Ref(0));
        register_is(&m, 2, Str(2));
        register_is(&m, 3, Str(6));
        register_is(&m, 4, Ref(10));
        register_is(&m, 5, Ref(0));
        register_is(&m, 6, Ref(4));
        register_is(&m, 7, Ref(13));
    }

    #[test]
    fn test_instruction_compilation_exercise_2_8() {
        let q = Compound {
            name: "p".to_string(),
            arity: 3,
            args: vec![
                Term::Compound(Compound { name: "f".to_string(), arity: 1, args: vec![Term::Var(Var("X".to_string()))] }),
                Term::Compound(
                    Compound {
                        name: "h".to_string(),
                        arity: 2,
                        args: vec![
                            Term::Var(Var("Y".to_string())),
                            Term::Compound(Compound {
                                name: "f".to_string(),
                                arity: 1,
                                args: vec![Term::Atom(Atom("a".to_string()))]
                            })
                        ]
                    }
                ),
                Term::Var(Var("Y".to_string()))
            ]
        };

        let p = Compound {
            name: "p".to_string(),
            arity: 3,
            args: vec![
                Term::Var(Var("Z".to_string())),
                Term::Compound(
                    Compound {
                        name: "h".to_string(),
                        arity: 2,
                        args: vec![
                            Term::Var(Var("Z".to_string())),
                            Term::Var(Var("W".to_string()))
                        ]
                    }
                ),
                Term::Compound(
                    Compound {
                        name: "f".to_string(),
                        arity: 1,
                        args: vec![
                            Term::Var(Var("W".to_string()))
                        ]
                    }
                )
            ]
        };

        let expected_query_instructions = vec![
            Instruction::PutStructure(Functor::from("f/1"), 1),
            Instruction::SetVariable(4),
            Instruction::PutStructure(Functor::from("a/0"), 7),
            Instruction::PutStructure(Functor::from("f/1"), 6),
            Instruction::SetValue(7),
            Instruction::PutStructure(Functor::from("h/2"), 2),
            Instruction::SetVariable(5),
            Instruction::SetValue(6),
            Instruction::PutValue(5, 3),
            Instruction::Call(Functor::from("p/3"))
        ];

        let expected_program_instructions = vec![
            Instruction::GetVariable(4, 1),
            Instruction::GetStructure(Functor::from("h/2"), 2),
            Instruction::UnifyValue(4),
            Instruction::UnifyVariable(5),
            Instruction::GetStructure(Functor::from("f/1"), 3),
            Instruction::UnifyValue(5),
            Instruction::Proceed
        ];

        let (query_instructions, _) = compile_query(&q);
        let (program_instructions, _) = compile_program(&p);

        assert_eq!(&expected_query_instructions, &query_instructions);
        assert_eq!(&expected_program_instructions, &program_instructions);
    }

    #[test]
    fn test_instruction_compilation_figure_2_9() {
        let q = Compound {
            name: "p".to_string(),
            arity: 3,
            args: vec![
                Term::Var(Var("Z".to_string())),
                Term::Compound(
                    Compound {
                        name: "h".to_string(),
                        arity: 2,
                        args: vec![
                            Term::Var(Var("Z".to_string())),
                            Term::Var(Var("W".to_string()))
                        ]
                    }
                ),
                Term::Compound(
                    Compound {
                        name: "f".to_string(),
                        arity: 1,
                        args: vec![
                            Term::Var(Var("W".to_string()))
                        ]
                    }
                )
            ]
        };

        let expected_query_instructions = vec![
            Instruction::PutVariable(4, 1),
            Instruction::PutStructure(Functor::from("h/2"), 2),
            Instruction::SetValue(4),
            Instruction::SetVariable(5),
            Instruction::PutStructure(Functor::from("f/1"), 3),
            Instruction::SetValue(5),
            Instruction::Call(Functor::from("p/3"))
        ];

        let (query_instructions, _) = compile_query(&q);

        assert_eq!(&expected_query_instructions, &query_instructions);
    }

    #[test]
    fn test_instruction_compilation_figure_2_10() {
        let q = Compound {
            name: "p".to_string(),
            arity: 3,
            args: vec![
                Term::Compound(
                    Compound {
                        name: "f".to_string(),
                        arity: 1,
                        args: vec![Term::Var(Var("X".to_string()))]
                    }
                ),
                Term::Compound(
                    Compound {
                        name: "h".to_string(),
                        arity: 2,
                        args: vec![
                            Term::Var(Var("Y".to_string())),
                            Term::Compound(
                                Compound {
                                    name: "f".to_string(),
                                    arity: 1,
                                    args: vec![Term::Atom(Atom("a".to_string()))]
                                }
                            ),
                        ]
                    }
                ),
                Term::Var(Var("Y".to_string()))
            ]
        };

        let expected_program_instructions = vec![
            Instruction::GetStructure(Functor::from("f/1"), 1),
            Instruction::UnifyVariable(4),
            Instruction::GetStructure(Functor::from("h/2"), 2),
            Instruction::UnifyVariable(5),
            Instruction::UnifyVariable(6),
            Instruction::GetValue(5, 3),
            Instruction::GetStructure(Functor::from("f/1"), 6),
            Instruction::UnifyVariable(7),
            Instruction::GetStructure(Functor::from("a/0"), 7),
            Instruction::Proceed
        ];

        let (program_instructions, _) = compile_program(&q);

        assert_eq!(&expected_program_instructions, &program_instructions);
    }

    #[test]
    fn test_unify_variable_read_mode() {
        let mut m = Machine::new();

        m.set_mode(Read);
        m.push_heap(Ref(3));
        m.unify_variable(1);

        assert_eq!(m.get_x(1).cloned().unwrap(), Ref(3));
        assert_eq!(m.get_s(), 1);
    }

    #[test]
    fn test_unify_variable_write_mode() {
        let mut m = Machine::new();

        m.set_mode(Write);
        m.unify_variable(1);

        assert_eq!(m.heap[0], Ref(0));
        assert_eq!(m.get_x(1).cloned().unwrap(), Ref(0));
        assert_eq!(m.get_h(), 1);
        assert_eq!(m.get_s(), 1);
    }

    #[test]
    fn test_functor_eq() {
        let f1 = Functor::from("foo/1");
        let f2 = Functor::from("bar/1");

        assert_ne!(f1, f2);

        let f2 = Functor::from("foo/1");
        assert_eq!(f1, f2);

        let f2 = Functor::from("foo/2");
        assert_ne!(f1, f2);
    }

    #[test]
    fn test_compound_structure_rendering() {
        let t = Term::Compound( Compound {
            name: String::from("foo"),
            arity: 2,
            args: vec![Term::Atom(Atom("bar".to_string())), Term::Atom(Atom("baz".to_string()))]});

        assert_eq!(t.to_string(), "foo(bar, baz)");
    }

    #[test]
    fn test_atomic_structure_rendering() {
        let t = Term::Compound( Compound { name: String::from("bar"), arity: 0, args: Vec::new() });

        assert_eq!(t.to_string(), "bar");
    }

    #[test]
    fn test_atom_parser() {
        let atom_parser = parser::AtomParser::new();

        // atoms
        assert!(atom_parser.parse("22").is_err());
        assert!(atom_parser.parse("_Abc").is_err());
        assert!(atom_parser.parse("Abc").is_err());
        assert!(atom_parser.parse("abc").is_ok());
        assert!(atom_parser.parse("'Abc'").is_ok());
        assert!(atom_parser.parse("'Abc").is_err());
        assert!(atom_parser.parse(".q").is_err());
        assert!(atom_parser.parse("snake_case").is_ok());
        assert!(atom_parser.parse("'snake_case'").is_ok());
        assert!(atom_parser.parse("This_Fails").is_err());
        assert!(atom_parser.parse("'This_Succeeds'").is_ok());
    }

    #[test]
    fn test_number_parser() {
        let number_parser = parser::NumberParser::new();

        // numbers
        assert!(number_parser.parse("2").is_ok());
        assert!(number_parser.parse("42").is_ok());
        assert!(number_parser.parse("34345354").is_ok());
//    assert!(number_parser.parse("3.3").is_ok());
//    assert!(number_parser.parse("3.30").is_ok());
//    assert!(number_parser.parse("0.3").is_ok());
        assert!(number_parser.parse("a03").is_err());
        assert!(number_parser.parse("_21").is_err());
        assert!(number_parser.parse("2_12").is_err());
        assert!(number_parser.parse(".3").is_err());
        assert!(number_parser.parse("2.").is_err());
    }

    #[test]
    fn test_compound_parser() {
        let c = parser::CompoundParser::new();

        // compounds
        assert!(c.parse("p(Z, h(Z, W), f(W))").is_ok());
        assert!(c.parse("p (Z, h(Z, W), f(W))").is_err());
        assert!(c.parse("p(Z, h(Z, W), f(W)").is_err());
        assert!(c.parse("p(Z, h(Z,, f(W)").is_err());
        assert!(c.parse("p(Z, f(h(Z, W)), f(W))").is_ok());
    }

    #[test]
    fn test_simple_expressions() {
        let e = parser::ExpressionParser::new();

        //expressions
        assert!(e.parse("A.").is_ok());
        assert!(e.parse("2.").is_err());
        assert!(e.parse("foo(bar).").is_ok());
        assert!(e.parse("foo.").is_ok());
    }

    #[ignore]
    #[test]
    fn test_instruction_compilation_exercise_2_1() {
        let e = parser::ExpressionParser::new();

        let expected_instructions = vec![
            Instruction::PutStructure(Functor::from("h/2"), 3),
            Instruction::SetVariable(2),
            Instruction::SetVariable(5),
            Instruction::PutStructure(Functor::from("f/1"), 4),
            Instruction::SetValue(5),
            Instruction::PutStructure(Functor::from("p/3"), 1),
            Instruction::SetValue(2),
            Instruction::SetValue(3),
            Instruction::SetValue(4)
        ];

        let mut q = e.parse("p(Z, h(Z, W), f(W)).").unwrap();
        let (instructions, _) = compile_query(&q);

        assert_eq!(instructions, expected_instructions);
    }

    fn register_is(machine: &Machine, register: Register, cell: Cell) {
        assert_eq!(machine.get_x(register).cloned().unwrap(), cell);
    }
}
