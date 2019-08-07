use bfg_prolog::{Machine, run_query, ast};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);


fn main() {
    let mut m = Machine::new();
    let mut bindings = Vec::new();

    let (query_bindings, program_bindings) = run_query(&mut m, "p(f(X), h(Y, f(a)), Y).", "p(Z, h(Z, W), f(W)).");

    bindings.extend_from_slice(&query_bindings);
    bindings.extend_from_slice(&program_bindings);

    for answer in bindings {
        println!("{}", answer);
    }
}
