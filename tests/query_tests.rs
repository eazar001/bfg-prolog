use bfg_prolog::ast;
use bfg_prolog::ast::{Assertion, Clause};
use bfg_prolog::solve_toplevel;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;

lalrpop_mod!(pub parser);

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

fn compare_answers(answers: Vec<String>, expected: &[&str]) {
    let mut answers: Vec<&str> = answers.iter().map(|s| s.trim()).collect();
    answers.sort();
    assert_eq!(answers, expected);
}

#[test]
fn test_the_expanse_program_1_succeeds() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("leader(X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(
        results,
        &["X = 'James Holden'", "X = 'McDowell'", "X = 'Naomi Nagata'"],
    )
}

#[test]
fn test_the_expanse_program_1_fails() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("leader('Amos Burton').");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No."])
}

#[test]
fn test_the_expanse_program_2_succeeds() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("captain(S, X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(
        results,
        &[
            "S = 'Canterbury'\nX = 'McDowell'",
            "S = 'Rocinante'\nX = 'James Holden'",
        ],
    )
}

#[test]
fn test_the_expanse_program_2_fails() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("captain(X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No."])
}
