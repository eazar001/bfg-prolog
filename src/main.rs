#![allow(unused)]

pub mod ast;


use lalrpop_util::lalrpop_mod;
use crate::ast::*;
use std::collections::{HashMap};
use std::hash::Hash;
use bfg_prolog::{Functor, Register};


lalrpop_mod!(pub parser);


#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct QueryCompound {
    name: String,
    arity: usize,
    args: Vec<QueryTerm>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum QueryTerm {
    Atom(usize),
    Number(usize),
    Var(usize),
    Compound(QueryCompound)
}

enum Instruction {
    PutStructure(Functor, Register),
    SetVariable(Register),
    SetValue(Register)
}


// TODO: make this iterative
fn assign_registers(compound: &Compound, x: &mut usize, m: &mut HashMap<Term, usize>) {
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
        if let Term::Compound(c) = t {
            assign_registers(c, x, m);
        }
    }
}

fn allocate_registers(compound: &Compound) -> HashMap<QueryTerm, usize> {
    let mut m = HashMap::new();
    let mut x = 1;

    assign_registers(compound, &mut x, &mut m);

    term_to_query_map(&m)
}

fn term_to_query_map(m: &HashMap<Term, usize>) -> HashMap<QueryTerm, usize> {
    let mut q = HashMap::new();

    for (term, x) in m.iter() {
        q.insert(term_to_query_term(m, term), *x);
    }

    q
}

fn term_to_query_term(m: &HashMap<Term, usize>, term: &Term) -> QueryTerm {
    let get = |t| *m.get(t).unwrap();

    match term {
        t@Term::Atom(_) => QueryTerm::Atom(get(t)),
        t@Term::Number(_) => QueryTerm::Number(get(t)),
        t@Term::Var(_) => QueryTerm::Var(get(t)),
        Term::Compound( Compound { name, arity, args }) => {
            let mut args = args
                .iter()
                .map(|t| term_to_query_term(m, t)).collect();

            QueryTerm::Compound(QueryCompound { name: name.clone(), arity: *arity, args })
        }
    }
}

fn compile_query(compound: &Compound) {
    let m = allocate_registers(compound);
    let mut pairs: Vec<_> = m.iter().map(|(term, reg)| (reg, term)).collect();
    pairs.sort();

    for (reg, term) in &pairs[1..] {
        if let c@QueryTerm::Compound(_) = term {
            compile_query_subterm(c, &m)
        }
    }

    match pairs[0] {
        (1, QueryTerm::Compound(c)) => {
            for t in &c.args {
                println!("top-term: {:?}", m.get(t).unwrap())
            }

            println!("root-compound: {:?}", c);
        },
        _ => panic!()
    }


}

fn compile_query_subterm(term: &QueryTerm, m: &HashMap<QueryTerm, usize>) {
    if let QueryTerm::Var(v) = term {
        println!("var: {:?}", v);
    } else if let QueryTerm::Compound(compound) = term {
        for t in &compound.args {
            compile_query_subterm(t, m);
        }

        println!("compound: {:?}", compound)
    }
}


fn main() {
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

    let c = parser::CompoundParser::new();

    // compounds
    let s = c.parse("p(Z, h(Z, W), f(W))").unwrap();

//    println!("{:?}", s);

//    let m = allocate_registers(&s);
//
//    println!("Final Assignments: {:?}", m);

    compile_query(&s);
}
