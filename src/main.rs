use ast::*;
use bfg_prolog::*;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);

fn main() {
    solve_toplevel(vec![Atom::new(
        "q",
        vec![
            Term::Var(Var(String::from("X"), 0)),
            Term::Atom(Atom::new("b", vec![])),
        ],
    )])
}
