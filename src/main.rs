use bfg_prolog::{Machine, Functor};


fn main() {
    let m = Machine::new();

    let f1 = Functor(String::from("foo"), 2);
    let f2 = f1.clone();

    println!("{0:?}\n{0}\n{1}\n{2:?}", &f1, f2.to_string(), m);
}
