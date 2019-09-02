use bfg_prolog::*;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);

fn main() {
    let clause_parser = parser::ClauseParser::new();
    let assertion_parser = parser::AssertionParser::new();

    let x = clause_parser.parse("p(U, V).").unwrap();

    let a = assertion_parser
        .parse("p(U, v) :- a, x(A). p(U, v) :- a, x(A).")
        .unwrap();

    println!("{:?}", a);

    solve_toplevel(x);
}
