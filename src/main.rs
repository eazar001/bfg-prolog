use bfg_prolog::ast;
use bfg_prolog::ast::{Assertion, Atom, Clause, Const, Term};
use bfg_prolog::solve_toplevel;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;
use std::io::Write;

lalrpop_mod!(pub parser);

fn main() {
    let mut source = Vec::new();
    let consult_const = Const::new("consult");

    loop {
        print!("?- ");
        std::io::stdout().flush().expect("Could not flush stdout");

        let mut input_buffer = String::new();
        std::io::stdin()
            .read_line(&mut input_buffer)
            .expect("error reading input");

        let query = parse_query(&input_buffer);

        if query.len() == 1 && query[0].name == consult_const && query[0].arity == 1 {
            if let Term::Atom(Atom { name: Const(p), .. }) = &query[0].args[0] {
                source = read_source_code(p);
                solve_toplevel(true, &source, (&query[1..]).to_vec());
            }
        } else {
            solve_toplevel(true, &source, query);
        }
    }
}

fn read_source_code(path: &str) -> Vec<Assertion> {
    let s = read_to_string(String::from(path)).unwrap();
    let mut source = parse_code(&s);
    source.reverse();

    source
}

fn parse_code(code: &str) -> Vec<Assertion> {
    let code_parser = parser::CodeParser::new();
    code_parser.parse(code).unwrap()
}

fn parse_query(query: &str) -> Clause {
    let clause_parser = parser::ClauseParser::new();
    clause_parser.parse(query).unwrap()
}
