use bfg_prolog::{query, Instruction, Functor};


#[test]
fn test_query() {
    let expected_instructions = vec![
        Instruction::PutStructure(Functor::from("h/2"), 3),
        Instruction::SetVariable(2),
        Instruction::SetVariable(5),
        Instruction::PutStructure(Functor::from("f/1"), 4),
        Instruction::SetValue(5),
        Instruction::PutStructure(Functor::from("p/3"), 1),
        Instruction::SetValue(2),
        Instruction::SetValue(3),
        Instruction::SetValue(4)
    ];

    assert_eq!(query("p(Z, h(Z, W), f(W))."), expected_instructions);
}