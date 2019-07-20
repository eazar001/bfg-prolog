use bfg_prolog::{Machine, query, program};


fn main() {
    let mut m = Machine::new();

    // test call
    println!("{:?}", query(&mut m, "a."));

    m = Machine::new();

    // test call
    println!("{:?}", query(&mut m, "p(f(X), h(Y, f(a)), Y)."));

    //test call
    println!("{:?}", program(&mut m, "p(Z, h(Z, W), f(W))."))
}
