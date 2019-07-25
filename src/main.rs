use bfg_prolog::{Machine, run_query, ast};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);


fn main() {
    let mut m = Machine::new();

    let (bindings, _) = run_query(&mut m, "p(f(X), h(Y, f(a)), Y).", "p(Z, h(Z, W), f(W)).");

    for answer in bindings {
        println!("{}", answer);
    }
}
