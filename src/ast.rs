type Arity = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Number {
    Integer(i32),
//    Float(f32)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Var(Var),
    Number(Number),
    Atom(Atom),
    Compound(Compound)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Compound {
    pub name: String,
    pub arity: Arity,
    pub args: Vec<Term>
}