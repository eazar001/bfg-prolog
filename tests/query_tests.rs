use bfg_prolog::{query, Machine, Cell, Register};
use bfg_prolog::Store::*;
use bfg_prolog::Cell::{Str, Ref};


#[test]
fn test_query_execution_exercise_2_1() {
    let mut m = Machine::new();
    let heap_output = query(&mut m, "p(Z, h(Z, W), f(W)).");

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

#[test]
fn test_query_execution_2_2() {
    let mut m = Machine::new();

    query(&mut m, "f(X, g(X, a)).");
    let heap_output = query(&mut m, "f(b, Y).").clone();

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

    assert_eq!(&heap_output, &expected_heap_cells);

    let m_clone = m.clone();
    register_is(&m_clone, 1, Str(13));
    register_is(&m_clone, 2, Str(11));
    register_is(&m_clone, 3, Ref(15));
    register_is(&m_clone, 4, Str(1));

    m.unify(HeapAddr(6), HeapAddr(12));

    assert!(m.succeed());

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

fn register_is(machine: &Machine, register: Register, cell: Cell) {
    assert_eq!(machine.get_x(register).cloned().unwrap(), cell);
}
