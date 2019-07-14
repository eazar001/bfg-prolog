#![allow(unused)]

pub mod ast;


use lalrpop_util::lalrpop_mod;
use crate::ast::*;
use std::collections::{HashMap};
use std::hash::Hash;


lalrpop_mod!(pub parser);


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct QueryCompound {
    name: String,
    arity: usize,
    args: Vec<QueryTerm>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum QueryTerm {
    Atom(usize),
    Number(usize),
    Var(usize),
    Compound(QueryCompound)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct QueryVar(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct QueryAtom(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct QueryNumber(usize);


// TODO: make this iterative
fn allocate_registers(compound: &Compound, x: &mut usize, m: &mut HashMap<Term, usize>) {
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
            allocate_registers(c, x, m)
        }
    }
}

fn flattened_term_to_query_term(compound: &Compound, m: &HashMap<Term, usize>) -> Vec<QueryTerm> {
    let l: Vec<_> = compound.args.iter().map(|t| term_to_query_term(m, t)).collect();
    l
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

fn compile_query(compound: &Compound, m: &HashMap<Term, i32>) {

}

fn compile_query_subterms(compound: &Compound, m: &HashMap<Term, i32>) {
    for t in &compound.args {
        if let Term::Compound(c) = t {
            compile_query(c, m);
        } else if let Term::Var(v) = t {

        }
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

    println!("{:?}", s);

    let mut m = HashMap::new();

    allocate_registers(&s, &mut 1, &mut m);
    println!("First pass: {:?}", m);
    println!("Final Assignments: {:?}", flattened_term_to_query_term(&s, &m));
}
