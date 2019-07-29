use bfg_prolog::{query, Machine, Cell, Register, run_query};
use bfg_prolog::Store::*;
use bfg_prolog::Cell::{Str, Ref};


#[ignore]
#[test]
fn test_query_execution_exercise_2_1() {
    let mut m = Machine::new();
    query(&mut m, "p(Z, h(Z, W), f(W)).");

    let heap_output = m.get_heap();

    let expected_heap_cells = vec![
        Str(1),
        Cell::from("h/2"),
        Ref(2),
        Ref(3),
        Str(5),
        Cell::from("f/1"),
        Ref(3),
        Str(8),
        Cell::from("p/3"),
        Ref(2),
        Str(1),
        Str(5)
    ];

    assert_eq!(heap_output, &expected_heap_cells)
}

#[ignore]
#[test]
fn test_query_execution_exercise_2_2() {
    let mut m = Machine::new();

    query(&mut m, "f(X, g(X, a)).");
    query(&mut m, "f(b, Y).");

    let expected_heap_cells = vec![
        Str(1),
        Cell::from("a/0"),
        Str(3),
        Cell::from("g/2"),
        Ref(4),
        Str(1),
        Str(7),
        Cell::from("f/2"),
        Ref(4),
        Str(3),
        Str(11),
        Cell::from("b/0"),
        Str(13),
        Cell::from("f/2"),
        Str(11),
        Ref(15)
    ];

    assert_eq!(m.get_heap(), &expected_heap_cells);

    register_is(&m, 1, Str(13));
    register_is(&m, 2, Str(11));
    register_is(&m, 3, Ref(15));
    register_is(&m, 4, Str(1));

    m.unify(HeapAddr(6), HeapAddr(12));

    assert!(m.is_true());

    let expected_heap_cells = vec![
        Str(1),
        Cell::from("a/0"),
        Str(3),
        Cell::from("g/2"),
        Str(11),
        Str(1),
        Str(7),
        Cell::from("f/2"),
        Ref(4),
        Str(3),
        Str(11),
        Cell::from("b/0"),
        Str(13),
        Cell::from("f/2"),
        Str(11),
        Str(3)
    ];

    assert_eq!(m.get_heap(), &expected_heap_cells);
}

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
    assert_eq!(machine.get_x(register).cloned().unwrap(), cell);
}
