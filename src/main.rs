use bfg_prolog::{Machine, query, program, resolve_term, ast, Cell};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);


fn main() {
    let mut m = Machine::new();

    let query_map = query(&mut m, "p(f(X), h(Y, f(a)), Y).");
    let program_map = program(&mut m, "p(Z, h(Z, W), f(W)).");

    let mut display_map = Vec::new();

    display_map.extend(query_map);
    display_map.extend(program_map);

    println!("{:?}\n\n", display_map);


    for (cell, term) in &display_map {
        match cell {
            Cell::Ref(a) | Cell::Str(a) => {
                if let ast::Term::Var(_) = term {
                    let mut buffer = String::new();
                    resolve_term(&m, *a, &display_map, &mut buffer);

                    if buffer != term.to_string() {
                        println!("{} = {}", term, buffer);
                    }
                }
            },
            _ => ()
        }
    }
}
