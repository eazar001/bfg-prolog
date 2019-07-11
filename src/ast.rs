type Arity = usize;

#[derive(Debug, Clone)]
pub struct Var(pub String);

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Integer(i32),
    Float(f32)
}

#[derive(Debug, Clone)]
pub struct Atom(pub String);

#[derive(Debug, Clone)]
pub enum Term {
    VarTerm(Var),
    NumberTerm(Number),
    AtomTerm(Atom),
    CompoundTerm(Compound)
}

#[derive(Debug, Clone)]
pub struct Compound {
    pub name: String,
    pub arity: Arity,
    pub args: Vec<Term>
}