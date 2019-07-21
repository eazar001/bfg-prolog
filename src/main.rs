use bfg_prolog::{Machine, query, program, resolve_term, ast, link_terms};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);


fn main() {
    let mut m = Machine::new();
    let p = parser::ExpressionParser::new();

    // test call
    println!("{:?}", query(&mut m, "a."));

    m = Machine::new();

    // test call
    println!("{:?}", query(&mut m, "p(f(X), h(Y, f(a)), Y)."));

    //test call
    let r_map = program(&mut m, "p(Z, h(Z, W), f(W)).");

    assert!(m.is_true());

    for (x, cell) in m.get_x_registers() {
        let mut s = String::new();

        resolve_term(&m, cell.address().unwrap(), &mut s);
        s.push_str(".");

        let result = r_map.get(x);

        if result.is_some() {
            let r = result.unwrap();

            if let ast::Term::Var(_) = r {
                let parse = p.parse(&s);

                if parse.is_ok() {
                    println!("{} = {}", r, p.parse(&s).unwrap())
                }
            }
        }
    }

    link_terms(&p.parse("p(f(X), h(Y, f(a)), Y).").unwrap(), &p.parse("p(Z, h(Z, W), f(W)).").unwrap());
}
