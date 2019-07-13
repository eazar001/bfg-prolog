#![allow(unused)]

pub mod ast;


use lalrpop_util::lalrpop_mod;
use crate::ast::*;
use std::collections::{HashMap};


lalrpop_mod!(pub parser);


fn allocate_registers(compound: &Compound, x: &mut i32, m: &mut HashMap<Term, i32>) {
    let term = Term::CompoundTerm(compound.clone());

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
        match &t {
            Term::CompoundTerm(ref c) => {
                allocate_registers(c, x, m)
            },
            _ => ()
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
    println!("{:?}", m);
}
