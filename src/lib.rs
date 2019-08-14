#![allow(dead_code)]
#![allow(unused)]
#![allow(clippy::new_without_default)]

pub mod ast;

use self::ast::*;
use self::Cell::*;
use self::Mode::{Read, Write};
use self::Register::*;
use self::Store::*;
use env_logger;
use lalrpop_util::lalrpop_mod;
use log::Level::*;
use log::{debug, error, info, trace, warn, Level};
use std::cmp::Ordering;
use std::collections::hash_set::Iter;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

lalrpop_mod!(pub parser);

type HeapAddress = usize;
type Address = usize;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Register {
    X(usize),
    Y(usize),
}

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
    Allocate(usize),
    Deallocate,
    Call(Functor),
    Proceed,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Functor(pub FunctorName, pub FunctorArity);

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Cell {
    Str(HeapAddress),
    Ref(HeapAddress),
    Func(Functor),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Store {
    HeapAddr(HeapAddress),
    Register(Register),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Mode {
    Read,
    Write,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum CodeType {
    Query(Functor),
    Fact(Functor),
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Code {
    code_area: Instructions,
    code_address: HashMap<CodeType, usize>,
}

// Environment stack frames
#[derive(Debug, Clone, Eq, PartialEq)]
enum Frame {
    Code(Address),
    Cell(Cell),
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Registers {
    h: HeapAddress,
    x: Vec<Option<Cell>>,
    s: Address,
    p: Address,
    cp: Address,
    e: Address,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Machine {
    heap: Heap,
    pdl: Vec<Store>,
    code: Code,
    stack: Vec<Option<Frame>>,
    registers: Registers,
    mode: Mode,
    fail: bool,
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Ref(a) => Ok(write!(f, "{:?}", Ref(*a))?),
            Str(a) => Ok(write!(f, "{:?}", Str(*a))?),
            Func(f1) => Ok(write!(f, "Functor({})", f1)?),
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
            code: Code::new(),
            stack: Vec::new(),
            registers: Registers::new(),
            mode: Read,
            fail: false,
        }
    }

    fn execute_instruction(&mut self, instruction: &Instruction) {
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
            Instruction::Allocate(n) => self.allocate(*n),
            Instruction::Deallocate => self.deallocate(),
            Instruction::Call(f) => self.call(&CodeType::Fact(f.clone())),
            Instruction::Proceed => self.proceed(),
        }
    }

    fn execute(&mut self, code: &CodeType) {
        match code {
            c @ CodeType::Query(_) => {
                let a = *self.code.code_address.get(c).unwrap();
                let instructions = self.code.code_area.clone();

                for instruction in &instructions[a..] {
                    if let Instruction::Call(_) = instruction {
                        self.execute_instruction(instruction);
                        break;
                    } else {
                        self.execute_instruction(instruction);
                    }
                }
            }
            c @ CodeType::Fact(_) => {
                let a = *self.code.code_address.get(c).unwrap();
                let instructions = self.code.code_area.clone();

                for instruction in &instructions[a..] {
                    if let Instruction::Proceed = instruction {
                        self.execute_instruction(instruction);
                        break;
                    } else {
                        self.execute_instruction(instruction);
                    }
                }
            }
        }
    }

    fn push_instruction(&mut self, instruction: Instruction) {
        self.code.code_area.push(instruction);
    }

    fn push_instructions(&mut self, code: &CodeType, instructions: &[Instruction]) {
        self.push_code_address(code);
        for instruction in instructions {
            self.push_instruction(instruction.clone());
        }
    }

    fn push_code_address(&mut self, code: &CodeType) {
        let a = self.code.code_area.len();
        self.code.code_address.insert(code.clone(), a);
    }

    fn get_code_address(&self, code: &CodeType) -> usize {
        *self.code.code_address.get(code).unwrap()
    }

    pub fn allocate(&mut self, n: usize) {}

    pub fn deallocate(&mut self) {}

    pub fn get_register(&self, register: Register) -> Option<&Cell> {
        match register {
            Register::X(xi) => self.get_x(xi),
            Register::Y(yi) => self.get_y(yi),
        }
    }

    pub fn get_x(&self, xi: usize) -> Option<&Cell> {
        self.registers.x[xi - 1].as_ref()
    }

    pub fn get_y(&self, yi: usize) -> Option<&Cell> {
        let offset = self.registers.e + 2;
        let stack = &self.stack;

        match &stack[offset + yi] {
            Some(Frame::Cell(c)) => Some(c),
            _ => panic!("error retrieving cell-data from permanent register"),
        }
    }

    fn insert_register(&mut self, register: Register, cell: Cell) {
        match register {
            Register::X(xi) => self.insert_x(xi, cell),
            Register::Y(yi) => self.insert_y(yi, cell),
        }
    }

    fn insert_y(&mut self, yi: usize, cell: Cell) {
        let offset = self.registers.e + 2;
        self.stack[offset + yi] = Some(Frame::Cell(cell));
    }

    fn insert_x(&mut self, xi: usize, cell: Cell) {
        if xi > self.registers.x.len() {
            self.registers.x.resize(xi, None);
        }

        self.registers.x[xi - 1] = Some(cell);
    }

    fn put_structure(&mut self, f: Functor, xi: Register) {
        let h = self.registers.h;
        self.heap.push(Str(h + 1));
        self.heap.push(Func(f));
        self.insert_register(xi, Str(h + 1));
        self.registers.h += 2;
    }

    fn put_variable(&mut self, xn: Register, ai: Register) {
        let h = self.registers.h;
        self.heap.push(Ref(h));
        self.insert_register(xn, Ref(h));
        self.insert_register(ai, Ref(h));
        self.registers.h += 1;
    }

    fn put_value(&mut self, xn: Register, ai: Register) {
        self.insert_register(ai, self.get_register(xn).cloned().unwrap());
    }

    fn set_variable(&mut self, xi: Register) {
        let h = self.registers.h;
        self.heap.push(Ref(h));
        self.insert_register(xi, Ref(h));
        self.registers.h += 1;
    }

    fn set_value(&mut self, xi: Register) {
        self.heap.push(self.get_register(xi).cloned().unwrap());
        self.registers.h += 1;
    }

    fn deref(&self, address: Store) -> Store {
        let mut address = address;

        loop {
            let (cell, a) = match address {
                HeapAddr(addr) => (&self.heap[addr], addr),
                Store::Register(r) => {
                    let addr = r.address();
                    let e = &format!("Illegal access: register {:?}, does not exist", addr);
                    let c = self.get_register(r).expect(e);

                    (c, addr)
                }
            };

            match cell {
                // keep following the reference chain
                Ref(value) if *value != a => address = HeapAddr(*value),
                Ref(_) | Str(_) | Func(_) => return address,
            }
        }
    }

    fn call(&mut self, f: &CodeType) {
        let a = self.get_code_address(f);
        let p = self.registers.p;
        self.registers.cp = p + 1;
        self.registers.p = a;
    }

    fn proceed(&mut self) {
        let cp = self.registers.cp;
        self.registers.p = cp;
    }

    fn get_variable(&mut self, xn: Register, ai: Register) {
        self.insert_register(xn, self.get_register(ai).cloned().unwrap());
    }

    fn get_value(&mut self, xn: Register, ai: Register) {
        self.unify(Store::Register(xn), Store::Register(ai));
    }

    fn get_structure(&mut self, f: Functor, xi: Register) {
        let (cell, address) = match self.deref(Store::Register(xi)) {
            HeapAddr(addr) => (&self.heap[addr], addr),
            Store::Register(r) => (self.get_register(xi).unwrap(), r.address()),
        };

        match cell.clone() {
            Ref(_) => {
                let h = self.registers.h;
                self.heap.push(Str(h + 1));
                self.heap.push(Func(f.clone()));
                self.bind(HeapAddr(address), HeapAddr(h));
                self.registers.h += 2;
                self.mode = Write;
            }
            Str(a) => match self.heap[a] {
                Func(ref functor) if *functor == f => {
                    self.registers.s = a + 1;
                    self.mode = Read;
                }
                Func(_) => self.fail = true,
                _ => panic!("Invalid structure cell pointing to non-functor cell"),
            },
            Func(_) => self.fail = true,
        }
    }

    fn unify_variable(&mut self, xi: Register) {
        match self.mode {
            Read => {
                let s = self.registers.s;
                self.insert_register(xi, self.heap[s].clone());
            }
            Write => {
                let h = self.registers.h;
                self.heap.push(Ref(h));
                self.insert_register(xi, Ref(h));
                self.registers.h += 1;
            }
        }

        self.registers.s += 1;
    }

    fn unify_value(&mut self, xi: Register) {
        match self.mode {
            Read => {
                let s = self.registers.s;
                self.unify(Store::Register(xi), Store::HeapAddr(s))
            }
            Write => {
                self.heap.push(self.get_register(xi).cloned().unwrap());
                self.registers.h += 1;
            }
        }

        self.registers.s += 1;
    }

    pub fn unify(&mut self, a1: Store, a2: Store) {
        self.pdl.push(a1);
        self.pdl.push(a2);
        self.fail = false;

        while !(self.pdl.is_empty() || self.fail) {
            let (a1, a2) = (self.pdl.pop().unwrap(), self.pdl.pop().unwrap());

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
                        for i in 1..=f1.arity() {
                            self.pdl.push(HeapAddr(v1 + i));
                            self.pdl.push(HeapAddr(v2 + i));
                        }
                    } else {
                        self.fail = true;
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
                    &f
                } else {
                    panic!("invalid cell: structure cell pointing to non-functor data")
                }
            }
            Func(f) => panic!("accessing a functor from a functor-cell"),
            Ref(_) => panic!("invalid cell-type for functor retrieval used"),
        }
    }

    fn get_store_cell(&self, address: Store) -> &Cell {
        match address {
            HeapAddr(addr) => &self.heap[addr],
            Register(r) => self.get_register(r).unwrap(),
        }
    }

    fn bind(&mut self, a1: Store, a2: Store) {
        let (c1, c2) = (self.get_store_cell(a1), self.get_store_cell(a2));
        let (a1, a2) = (c1.address().unwrap(), c2.address().unwrap());

        if c1.is_ref() && (!c2.is_ref() || a2 < a1) {
            self.heap[a1] = c2.clone();
        } else {
            self.heap[a2] = c1.clone();
        }
    }
}

impl Register {
    fn is_x(&self) -> bool {
        if let Register::X(_) = self {
            return true;
        }

        false
    }

    fn address(&self) -> usize {
        match self {
            Register::X(a) => *a,
            Register::Y(a) => *a,
        }
    }
}

impl Registers {
    fn new() -> Registers {
        Registers {
            h: 0,
            x: Vec::new(),
            s: 0,
            p: 0,
            cp: 0,
            e: 0,
        }
    }
}

impl Cell {
    fn is_ref(&self) -> bool {
        if let Ref(_) = self {
            return true;
        }

        false
    }

    pub fn address(&self) -> Option<HeapAddress> {
        match self {
            Str(addr) => Some(*addr),
            Ref(addr) => Some(*addr),
            Func(_) => None,
        }
    }
}

impl Store {
    fn address(&self) -> usize {
        match self {
            HeapAddr(addr) => *addr,
            Store::Register(r) => r.address(),
        }
    }
}

impl Code {
    fn new() -> Self {
        Code {
            code_area: Vec::new(),
            code_address: HashMap::new(),
        }
    }
}

fn allocate_query_registers(
    compound: &Structure,
    x: &mut usize,
    m: &mut TermMap,
    seen: &mut TermSet,
    instructions: &mut Instructions,
) {
    let term = Term::Structure(compound.clone());

    if !m.contains_key(&term) {
        m.insert(term, X(*x));
        *x += 1;
    }

    for t in &compound.args {
        if !m.contains_key(&t) {
            m.insert(t.clone(), X(*x));
            *x += 1;
        }
    }

    for t in &compound.args {
        if let Term::Structure(ref c) = t {
            allocate_query_registers(c, x, m, seen, instructions);
        }
    }

    let f = Functor(compound.name.clone(), compound.arity);
    let t = Term::Structure(compound.clone());

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

fn allocate_program_registers(
    root: bool,
    compound: &Structure,
    x: &mut usize,
    m: &mut TermMap,
    seen: &mut TermSet,
    arg_instructions: &mut Instructions,
    instructions: &mut Instructions,
) {
    let term = Term::Structure(compound.clone());

    if !m.contains_key(&term) {
        m.insert(term, X(*x));
        *x += 1;
    }

    for t in &compound.args {
        if !m.contains_key(&t) {
            m.insert(t.clone(), X(*x));
            *x += 1;
        }
    }

    let f = Functor(compound.name.clone(), compound.arity);
    let t = Term::Structure(compound.clone());

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
        } else if root {
            arg_instructions.push(Instruction::UnifyValue(*m.get(t).unwrap()));
        } else {
            instructions.push(Instruction::UnifyValue(*m.get(t).unwrap()));
        }
    }

    for t in &compound.args {
        if let Term::Structure(ref c) = t {
            allocate_program_registers(false, c, x, m, seen, arg_instructions, instructions);
        }
    }
}

fn compile_query<T: Structuralize>(term: &T, m: &mut TermMap, seen: &mut TermSet) -> Instructions {
    let mut instructions = Vec::new();

    let compound = term.structuralize().unwrap();

    for (i, arg) in compound.args.iter().enumerate() {
        let a = i + 1;
        let mut x = a + compound.arity;

        if let Term::Var(_) = arg {
            if !seen.contains(arg) {
                if m.contains_key(arg) {
                    instructions.push(Instruction::PutVariable(*m.get(&arg).unwrap(), X(a)));
                } else {
                    instructions.push(Instruction::PutVariable(X(x), X(a)));
                    m.insert(arg.clone(), X(x));
                }

                seen.insert(arg.clone());
            } else {
                instructions.push(Instruction::PutValue(*m.get(arg).unwrap(), X(a)));
            }
        } else {
            m.insert(arg.clone(), X(a));
            seen.insert(arg.clone());
            allocate_query_registers(
                &arg.structuralize().unwrap(),
                &mut x,
                m,
                seen,
                &mut instructions,
            );
        }
    }

    instructions.push(Instruction::Call(Functor(
        compound.name.clone(),
        compound.arity,
    )));

    instructions
}

fn compile_fact<T: Structuralize>(term: &T, m: &mut TermMap, seen: &mut TermSet) -> Instructions {
    let mut arg_instructions = Vec::new();
    let mut instructions = Vec::new();

    let compound = term.structuralize().unwrap();

    for (i, arg) in compound.args.iter().enumerate() {
        let a = i + 1;
        let mut x = a + compound.arity;

        if let Term::Var(_) = arg {
            if !seen.contains(arg) {
                if m.contains_key(arg) {
                    arg_instructions.push(Instruction::GetVariable(*m.get(&arg).unwrap(), X(a)));
                } else {
                    arg_instructions.push(Instruction::GetVariable(X(x), X(a)));
                    m.insert(arg.clone(), X(x));
                }

                seen.insert(arg.clone());
            } else {
                arg_instructions.push(Instruction::GetValue(*m.get(arg).unwrap(), X(a)));
            }
        } else {
            m.insert(arg.clone(), X(a));
            seen.insert(arg.clone());
            allocate_program_registers(
                true,
                &arg.structuralize().unwrap(),
                &mut x,
                m,
                seen,
                &mut arg_instructions,
                &mut instructions,
            );
        }
    }

    instructions.push(Instruction::Proceed);
    arg_instructions.extend_from_slice(&instructions);

    arg_instructions
}

fn find_variables(term: &Term, vars: &mut Vec<Var>) {
    if let Term::Structure(c) = term {
        for arg in &c.args {
            if let Term::Var(v) = arg {
                vars.push(v.clone());
            } else if let Term::Structure(Structure { name, arity, .. }) = arg {
                if *arity > 0 {
                    find_variables(arg, vars);
                }
            }
        }
    }
}

fn find_variable_positions(all_vars: &[Var]) -> Vec<Term> {
    let mut perm_vars = Vec::new();

    for var in all_vars {
        let t = Term::Var(var.clone());
        if !perm_vars.contains(&t) {
            perm_vars.push(t);
        }
    }

    perm_vars
}

fn collect_permanent_variables(rule: &Rule) -> TermMap {
    let Rule { head, body } = rule;
    let mut vars = Vec::new();
    let mut all_vars = Vec::new();
    let head = Term::Structure(head.structuralize().unwrap());
    let mut counts = HashMap::new();

    find_variables(&head, &mut vars);
    find_variables(&Term::Structure(body[0].clone()), &mut vars);

    for head_var in &vars {
        counts.insert(head_var.clone(), 1);
    }

    vars.clear();

    for body_term in &body[1..] {
        find_variables(&Term::Structure(body_term.clone()), &mut vars);
    }

    for body_var in &vars {
        match counts.get(body_var).cloned() {
            Some(c) => counts.insert(body_var.clone(), c + 1),
            None => counts.insert(body_var.clone(), 1),
        };
    }

    let vars: Vec<Term> = counts
        .iter()
        .filter(|(v, c)| **c > 1)
        .map(|(v, c)| Term::Var(v.clone()))
        .collect();

    find_variables(&head, &mut all_vars);

    for body_term in body {
        find_variables(&Term::Structure(body_term.clone()), &mut all_vars);
    }

    let mut perm_vars = find_variable_positions(&all_vars);
    let mut temp = Vec::new();

    for term in &perm_vars {
        if vars.contains(term) && !temp.contains(term) {
            temp.push(term.clone());
        }
    }

    let mut vars = HashMap::new();

    for (i, term) in temp.iter().enumerate() {
        vars.insert(term.clone(), Y(i + 1));
    }

    vars
}

fn compile_rule(rule: &Rule, m: &mut TermMap, seen: &mut TermSet) -> Instructions {
    let mut body_instructions = Vec::new();
    let y_map = collect_permanent_variables(rule);
    let n = y_map.len();

    m.extend(y_map);

    let Rule { head: term, body } = rule;
    let head = Term::Structure(term.clone());
    let head_instructions = compile_fact(&head, m, seen);
    let head_slice = &head_instructions[..head_instructions.len() - 1];

    let mut head_instructions = vec![Instruction::Allocate(n)];
    head_instructions.extend_from_slice(head_slice);

    for body_term in body {
        let body_term_instructions = compile_query(body_term, m, seen);
        body_instructions.extend(body_term_instructions);
    }

    body_instructions.push(Instruction::Deallocate);
    head_instructions.extend(body_instructions);

    head_instructions
}

pub fn compare_terms(solvent_term: &Term, other_term: &Term, mappings: &mut HashMap<Term, Term>) {
    match (solvent_term, other_term) {
        (q @ Term::Var(_), p @ Term::Var(_)) => {
            mappings.insert(q.clone(), q.clone());
        }
        (q @ Term::Var(_), p @ Term::Structure(_)) => {
            mappings.insert(q.clone(), p.clone());
        }
        (q @ Term::Structure(_), p @ Term::Var(_)) => (),
        (Term::Structure(q), Term::Structure(p)) => {
            let q_args = q.args.iter();
            let p_args = p.args.iter();
            let arg_pairs = q_args.zip(p_args);

            for (q_arg, p_arg) in arg_pairs {
                compare_terms(q_arg, p_arg, mappings);
            }
        }
        _ => panic!("unsupported term comparison"),
    };
}

pub fn find_solutions(solvent_args: &[Term], other_args: &[Term]) -> HashMap<Term, Term> {
    let solvent_terms = solvent_args.iter();
    let other_terms = other_args.iter();
    let terms = solvent_terms.zip(other_terms);

    let mut mappings = HashMap::new();

    for (solvent_term, other_term) in terms {
        compare_terms(&solvent_term, &other_term, &mut mappings);
    }

    mappings
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exercise_2_1() {
        let mut machine = Machine::new();

        for instruction in &figure_2_3_instructions() {
            machine.execute_instruction(instruction);
        }

        assert!(!machine.fail);
        assert_eq!(
            "p(_X2, h(_X2, _X3), f(_X3))",
            show_cell(&machine, HeapAddr(7))
        );
    }

    #[test]
    fn test_exercise_2_2() {
        let mut machine = Machine::new();
        machine.heap = exercise_2_2_heap_representation();
        machine.unify(HeapAddr(6), HeapAddr(12));

        assert!(!machine.fail);
        assert_eq!(machine.heap, exercise_2_2_mutated_heap_representation());

        let x = format!("X = {}", show_cell(&machine, HeapAddr(10)));
        let y = format!("Y = {}", show_cell(&machine, HeapAddr(15)));

        assert_eq!("X = b", x);
        assert_eq!("Y = g(b, a)", y);
    }

    #[test]
    fn test_exercise_2_3() {
        let mut machine = Machine::new();

        for instruction in &figure_2_3_instructions() {
            machine.execute_instruction(instruction);
        }

        let w = machine.deref(Register(X(5)));
        let z = machine.deref(Register(X(2)));

        for instruction in &figure_2_4_instructions() {
            machine.execute_instruction(instruction);
        }

        let x = machine.deref(Register(X(5)));
        let y = machine.deref(Register(X(4)));
        let show = |store| show_cell(&machine, store);

        assert!(!machine.fail);
        assert_eq!("W = f(a)", format!("W = {}", show(w)));
        assert_eq!("X = f(a)", format!("X = {}", show(x)));
        assert_eq!("Y = f(f(a))", format!("Y = {}", show(y)));
        assert_eq!("Z = f(f(a))", format!("Z = {}", show(z)));
    }

    #[test]
    fn test_exercise_2_4() {
        let mut machine = Machine::new();
        let parser = parser::ExpressionParser::new();
        let query = parser.parse("p(f(X), h(Y, f(a)), Y).").unwrap();
        let program = parser.parse("p(Z, h(Z, W), f(W)).").unwrap();

        println!("query: {:?}\nprogram: {:?}", query, program);

        let expected_query_instructions = vec![
            Instruction::PutStructure(Functor::from("f/1"), X(2)),
            Instruction::SetVariable(X(5)),
            Instruction::PutStructure(Functor::from("a/0"), X(7)),
            Instruction::PutStructure(Functor::from("f/1"), X(6)),
            Instruction::SetValue(X(7)),
            Instruction::PutStructure(Functor::from("h/2"), X(3)),
            Instruction::SetVariable(X(4)),
            Instruction::SetValue(X(6)),
            Instruction::PutStructure(Functor::from("p/3"), X(1)),
            Instruction::SetValue(X(2)),
            Instruction::SetValue(X(3)),
            Instruction::SetValue(X(4)),
        ];

        let expected_program_instructions = vec![
            Instruction::GetStructure(Functor::from("p/3"), X(1)),
            Instruction::UnifyVariable(X(2)),
            Instruction::UnifyVariable(X(3)),
            Instruction::UnifyVariable(X(4)),
            Instruction::GetStructure(Functor::from("h/2"), X(3)),
            Instruction::UnifyValue(X(2)),
            Instruction::UnifyVariable(X(5)),
            Instruction::GetStructure(Functor::from("f/1"), X(4)),
            Instruction::UnifyValue(X(5)),
        ];
    }

    #[test]
    fn test_exercise_2_5() {
        let mut machine = Machine::new();
        let parser = parser::ExpressionParser::new();
        let query = parser.parse("p(f(X), h(Y, f(a)), Y).").unwrap();
        let program = parser.parse("p(Z, h(Z, W), f(W)).").unwrap();

        let mut query_allocation = TermMap::new();
        let mut query_set = TermSet::new();
        let mut program_allocation = TermMap::new();
        let mut program_set = TermSet::new();

        let query_instructions = compile_query(&query, &mut query_allocation, &mut query_set);
        let program_instructions =
            compile_fact(&program, &mut program_allocation, &mut program_set);

        machine.push_instructions(&CodeType::Query(Functor::from("p/3")), &query_instructions);
        machine.push_instructions(&CodeType::Fact(Functor::from("p/3")), &program_instructions);
        machine.execute(&CodeType::Query(Functor::from("p/3")));
        machine.execute(&CodeType::Fact(Functor::from("p/3")));

        let query_tree = query.structuralize().unwrap();
        let program_tree = program.structuralize().unwrap();
        let query_args = &query_tree.args;
        let program_args = &program_tree.args;
        let program_args_unbound = program_args.clone();

        let program_args: Vec<_> = program_args
            .iter()
            .map(|t| {
                if !program_allocation.contains_key(t) {
                    t.clone()
                } else {
                    let mut s = show_cell(&machine, Register(*program_allocation.get(t).unwrap()));
                    s.push_str(".");
                    Term::Structure(parser.parse(&s).unwrap().structuralize().unwrap())
                }
            })
            .collect();

        let query_bindings = find_solutions(&query_args, &program_args);
        let mut query_bindings: Vec<_> = query_bindings.iter().collect();
        query_bindings.sort();
        let query_bindings: Vec<_> = query_bindings
            .iter()
            .map(|(var, term)| format!("{} = {}", var, term))
            .collect();

        assert!(!machine.fail);

        let expected_query_bindings = vec!["X = f(a)", "Y = f(f(a))"];
        let expected_program_bindings = vec!["W = f(a)", "Z = f(f(a))"];

        assert_eq!(&expected_query_bindings, &query_bindings);

        let program_bindings = find_solutions(&program_args_unbound, &program_args);
        let mut program_bindings: Vec<_> = program_bindings.iter().collect();
        program_bindings.sort();
        let program_bindings: Vec<_> = program_bindings
            .iter()
            .map(|(var, term)| format!("{} = {}", var, term))
            .collect();

        assert_eq!(expected_program_bindings, program_bindings);
    }

    // utility test functions

    // Figure 2.3: Compiled code for L0 query ?- p(Z, h(Z, W), f(W)).
    fn figure_2_3_instructions() -> Instructions {
        vec![
            Instruction::PutStructure(Functor::from("h/2"), X(3)),
            Instruction::SetVariable(X(2)),
            Instruction::SetVariable(X(5)),
            Instruction::PutStructure(Functor::from("f/1"), X(4)),
            Instruction::SetValue(X(5)),
            Instruction::PutStructure(Functor::from("p/3"), X(1)),
            Instruction::SetValue(X(2)),
            Instruction::SetValue(X(3)),
            Instruction::SetValue(X(4)),
        ]
    }

    // Figure 2.4: Compiled code for L0 program p(f(X), h(Y, f(a)), Y).
    fn figure_2_4_instructions() -> Instructions {
        vec![
            Instruction::GetStructure(Functor::from("p/3"), X(1)),
            Instruction::UnifyVariable(X(2)),
            Instruction::UnifyVariable(X(3)),
            Instruction::UnifyVariable(X(4)),
            Instruction::GetStructure(Functor::from("f/1"), X(2)),
            Instruction::UnifyVariable(X(5)),
            Instruction::GetStructure(Functor::from("h/2"), X(3)),
            Instruction::UnifyValue(X(4)),
            Instruction::UnifyVariable(X(6)),
            Instruction::GetStructure(Functor::from("f/1"), X(6)),
            Instruction::UnifyVariable(X(7)),
            Instruction::GetStructure(Functor::from("a/0"), X(7)),
        ]
    }

    // Exercise 2.2: Heap representations for f(X, g(X, a)) and f(b, Y).
    fn exercise_2_2_heap_representation() -> Vec<Cell> {
        vec![
            Str(1),
            Func(Functor::from("a/0")),
            Str(3),
            Func(Functor::from("g/2")),
            Ref(4),
            Str(1),
            Str(7),
            Func(Functor::from("f/2")),
            Ref(4),
            Str(3),
            Str(11),
            Func(Functor::from("b/0")),
            Str(13),
            Func(Functor::from("f/2")),
            Str(11),
            Ref(15),
        ]
    }

    // Exercise 2.2: Heap representations for f(X, g(X, a)) and f(b, Y), post-unification.
    fn exercise_2_2_mutated_heap_representation() -> Vec<Cell> {
        vec![
            Str(1),
            Func(Functor::from("a/0")),
            Str(3),
            Func(Functor::from("g/2")),
            Str(11),
            Str(1),
            Str(7),
            Func(Functor::from("f/2")),
            Ref(4),
            Str(3),
            Str(11),
            Func(Functor::from("b/0")),
            Str(13),
            Func(Functor::from("f/2")),
            Str(11),
            Str(3),
        ]
    }

    fn show_cell(machine: &Machine, address: Store) -> String {
        let d = machine.get_store_cell(machine.deref(address));

        match d.clone() {
            Ref(a) => match &machine.heap[a] {
                Ref(v) if *v == a => format!("_X{}", v),
                r @ Ref(_) => show_cell(machine, HeapAddr(a)),
                _ => show_cell(machine, HeapAddr(a)),
            },
            Str(a) => {
                let cell = Str(a);
                let Functor(name, arity) = machine.get_functor(&cell).clone();

                if arity == 0 {
                    name
                } else {
                    let mut s = format!("{}(", name);

                    for i in 1..arity {
                        let t = show_cell(machine, HeapAddr(a + i));
                        s.push_str(&format!("{}, ", t));
                    }

                    let t = show_cell(machine, HeapAddr(a + arity));
                    s.push_str(&format!("{})", t));

                    s
                }
            }
            Func(_) => panic!("Attempted to render a functor, when a ref or str cell was expected"),
        }
    }
}
