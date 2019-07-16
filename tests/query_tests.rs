use bfg_prolog::{query, Machine, Cell};
use bfg_prolog::Cell::{Str, Ref};

extern crate bfg_prolog;


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