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
    let answers: Vec<&str> = answers.iter().map(|s| s.trim()).collect();
    assert_eq!(answers, expected);
}

#[test]
fn test_basic_1_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(X, X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = X1"])
}

#[test]
fn test_basic_2_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(X, Y).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = X1\nY = X1"])
}

#[test]
fn test_basic_3_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(a, a).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["Yes"])
}

#[test]
fn test_basic_3_fails() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(a, b).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No"])
}

#[test]
fn test_basic_4_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(X, a).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = a"])
}

#[test]
fn test_basic_5_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("member(a, list(a, nil)).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["Yes"])
}

#[test]
fn test_basic_6_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("member(a, list(a, list(b, nil))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["Yes"])
}

#[test]
fn test_basic_7_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("member(a, list(a, list(b, list(a, nil)))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["Yes", "Yes"])
}

#[test]
fn test_basic_7_fails() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("member(c, list(a, list(b, list(a, nil)))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No"])
}

#[test]
fn test_basic_8_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("member(X, list(a, list(b, list(a, nil)))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = a", "X = b", "X = a"]);
}

#[test]
fn test_basic_9_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(X, b), member(X, list(a, list(b, list(a, nil)))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = b"]);
}

#[test]
fn test_basic_10_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(X, b), member(X, list(a, list(b, list(b, nil)))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = b", "X = b"]);
}

#[test]
fn test_basic_11_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("append(X, Y, list(a, list(b, nil))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(
        results,
        &[
            "X = nil\nY = list(a, list(b, nil))",
            "X = list(a, nil)\nY = list(b, nil)",
            "X = list(a, list(b, nil))\nY = nil",
        ],
    );
}

#[test]
fn test_basic_12_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("append(X, list(Y, list(Z, nil)), list(a, list(b, nil))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = nil\nY = a\nZ = b"]);
}

#[test]
fn test_basic_12_fails() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("append(X, list(Y, list(q, nil)), list(a, list(b, nil))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No"]);
}

#[test]
fn test_basic_13_fails() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query(
        "append(list(a, list(b, list(c, nil))), list(Y, list(Z, nil)), list(a, list(b, nil))).",
    );

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No"]);
}

#[test]
fn test_basic_14_fails() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("append(list(a, nil), list(b, nil), list(a, list(b, nil))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["Yes"]);
}

#[test]
fn test_basic_14_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("append(list(a, nil), X, list(a, list(b, nil))).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = list(b, nil)"]);
}

#[test]
fn test_basic_15_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(p(Z, h(Z, W), f(W)), p(f(X), h(Y, f(a)), Y)).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["W = f(a)\nX = f(a)\nY = f(f(a))\nZ = f(f(a))"]);
}

#[test]
fn test_basic_16_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(f(X, g(X, a)), f(b, Y)).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = b\nY = g(b, a)"]);
}

#[test]
fn test_basic_17_succeeds() {
    let source = read_source_code("tests/example_programs/basic/basic.pl");
    let query = parse_query("unify(f(X), X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No"]);
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

    compare_answers(results, &["No"])
}

#[test]
fn test_the_expanse_program_2_succeeds() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("captain(S, X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(
        results,
        &[
            "S = 'Rocinante'\nX = 'James Holden'",
            "S = 'Canterbury'\nX = 'McDowell'",
        ],
    )
}

#[test]
fn test_the_expanse_program_2_fails() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("captain(X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No"])
}

#[test]
fn test_the_expanse_program_3_succeeds() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("mechanic('Rocinante', X).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["X = 'Amos Burton'"])
}

#[test]
fn test_the_expanse_program_3_fails() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("mechanic('Rocinante', 'Alex Kamal').");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["No"])
}

#[test]
fn test_the_expanse_program_4_succeeds() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("mechanic(S, 'Amos Burton').");

    let results = solve_toplevel(false, &source, query);

    compare_answers(results, &["S = 'Canterbury'", "S = 'Rocinante'"])
}

#[test]
fn test_the_expanse_program_5_succeeds() {
    let source = read_source_code("tests/example_programs/the_expanse/the_expanse.pl");
    let query = parse_query("mechanic(S, 'Amos Burton'), pilot(S, Pilot).");

    let results = solve_toplevel(false, &source, query);

    compare_answers(
        results,
        &[
            "Pilot = 'Alex Kamal'\nS = 'Canterbury'",
            "Pilot = 'Alex Kamal'\nS = 'Rocinante'",
        ],
    )
}
