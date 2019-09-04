use bfg_prolog::ast::{Assertion, Clause};
use bfg_prolog::*;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;

lalrpop_mod!(pub parser);

fn main() {
    let v = read_source_code("tests\\example_programs\\the_expanse\\the_expanse.pl");

    solve_toplevel(
        &v,
        parse_query("member(X, list(a, list(b, list(c, list(d, list(e, list(nil)))))))."),
    );
}

fn read_source_code(path: &str) -> Vec<Assertion> {
    let s = read_to_string(String::from(path)).unwrap();
    parse_code(&s)
}

fn parse_code(code: &str) -> Vec<Assertion> {
    let code_parser = parser::CodeParser::new();
    let mut c = code_parser.parse(code).unwrap();
    c.reverse();

    c
}

fn parse_query(query: &str) -> Clause {
    let clause_parser = parser::ClauseParser::new();
    let mut c = clause_parser.parse(query).unwrap();
    c.reverse();

    c
}
