use bfg_prolog::{query, Machine, Cell, Register, run_query};
use bfg_prolog::Store::*;
use bfg_prolog::Cell::{Str, Ref};
use bfg_prolog::Register::*;


#[test]
fn test_query_execution_2_5() {
    let mut m = Machine::new();

    let (query_bindings, program_bindings) = run_query(
        &mut m,
        "p(f(X), h(Y, f(a)), Y).",
        "p(Z, h(Z, W), f(W))."
    );

    let expected_query_bindings = vec![
        "X = f(a)",
        "Y = f(f(a))"
    ];

    let expected_program_bindings = vec![
        "W = f(a)",
        "Z = f(f(a))"
    ];

    assert_eq!(expected_query_bindings, query_bindings);
    assert_eq!(expected_program_bindings, program_bindings);
}

fn register_is(machine: &Machine, register: Register, cell: Cell) {
    assert_eq!(machine.get_register(register).cloned().unwrap(), cell);
}
