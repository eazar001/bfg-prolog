use std::fmt::{Display, Formatter};

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

pub trait NamedType {
    fn name(&self) -> &str;
}

impl NamedType for Atom {
    fn name(&self) -> &str {
        &self.0
    }
}

impl NamedType for Compound {
    fn name(&self) -> &str {
        self.name()
    }
}

impl From<Atom> for Compound {
    fn from(Atom(a): Atom) -> Compound {
        Compound { name: a.clone(), arity: 0, args: Vec::new() }
    }
}

impl<T: NamedType + Clone> From<&T> for Compound where Compound: From<T> {
    fn from(t: &T) -> Compound
        where T: From<T> + NamedType
    {
        let t = t.clone();

        Compound::from(t)
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Term::Var(Var(label)) => Ok(write!(f, "{}", label)?),
            Term::Number(Number::Integer(i)) => Ok(write!(f, "{}", i)?),
            Term::Atom(Atom(a)) => Ok(write!(f, "{}", a)?),
            Term::Compound(Compound { name, arity, args }) => {
                match args.len() {
                    0 => Ok(write!(f, "{}", &name)?),
                    _ => {
                        let init = &args[..args.len() - 1];
                        let last = args.last();
                        let mut args = String::new();

                        for arg in init {
                            args.push_str(&format!("{}, ", arg));
                        }

                        args.push_str(&format!("{})", last.unwrap()));

                        Ok(write!(f, "{}({}", &name, args)?)
                    }
                }
            }
        }
    }
}
