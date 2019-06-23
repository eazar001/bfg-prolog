use wam_prolog::{Env, Functor};


fn main() {
    let env = Env::new();

    let f1 = Functor(String::from("foo"), 2);
    let f2 = f1.clone();

    println!("{0:?}\n{0}\n{1}\n{2:?}", &f1, f2.to_string(), env);
}
