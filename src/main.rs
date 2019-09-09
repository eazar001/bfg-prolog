use bfg_prolog::ast::{Assertion, Clause};
use bfg_prolog::*;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;
use std::io::Write;

lalrpop_mod!(pub parser);

fn main() {
    let source = read_source_code("tests\\example_programs\\the_expanse\\the_expanse.pl");

    loop {
        print!("?- ");
        std::io::stdout().flush().expect("Could not flush stdout");

        let mut input_buffer = String::new();
        std::io::stdin()
            .read_line(&mut input_buffer)
            .expect("error reading input");

        let query = parse_query(&input_buffer);

        solve_toplevel(&source, query.to_vec());
    }
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
