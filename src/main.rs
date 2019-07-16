use bfg_prolog::{Machine, query};


fn main() {
    let mut m = Machine::new();

    // test call
    println!("{:?}", query(&mut m, "p(f(X), h(Y, f(a)), Y)."));
}
