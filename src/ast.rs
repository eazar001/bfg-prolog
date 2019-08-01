use std::fmt::{Display, Formatter};

type Arity = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Number {
    Integer(i32),
//    Float(f32)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Atom(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Rule {
    pub head: Compound,
    pub body: Vec<Compound>
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Var(Var),
    Number(Number),
    Atom(Atom),
    Compound(Compound),
    Rule(Rule)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Compound {
    pub name: String,
    pub arity: Arity,
    pub args: Vec<Term>
}

pub trait Structuralize {
    fn structuralize(&self) -> Option<Compound>;
    fn name(&self) -> String;
}

impl Structuralize for Atom {
    fn structuralize(&self) -> Option<Compound> {
        let Atom(a) = self;
        Some(Compound {name: a.clone(), arity: 0, args: Vec::new() })
    }

    fn name(&self) -> String {
        self.0.clone()
    }
}

impl Structuralize for Compound {
    fn structuralize(&self) -> Option<Compound> {
        let mut args = Vec::new();

        for t in &self.args {
            match t {
                Term::Atom(a) => args.push(Term::Compound(a.structuralize().unwrap())),
                Term::Compound(ref c) => {
                    args.push(Term::Compound(c.structuralize().unwrap()));
                },
                _ => args.push(t.clone())
            }
        }

        Some(Compound { name: String::from(&self.name), arity: self.arity, args })
    }

    fn name(&self) -> String {
        let Compound { name, .. } = self;
        name.clone()
    }
}

impl Structuralize for Term {
    fn structuralize(&self) -> Option<Compound> {
        match self {
            Term::Atom(a) => Some(a.structuralize().unwrap()),
            Term::Compound(c) => Some(c.structuralize().unwrap()),
            t => None
        }
    }

    fn name(&self) -> String {
        match self {
            Term::Compound(c) => c.name.clone(),
            Term::Atom(Atom(a)) => a.clone(),
            Term::Number(Number::Integer(i)) => format!("{}", i),
            Term::Var(Var(v)) => v.clone(),
            Term::Rule(Rule {head, ..}) => head.name.clone()
        }
    }
}

impl From<&Atom> for Compound {
    fn from(Atom(a): &Atom) -> Compound {
        Compound { name: a.clone(), arity: 0, args: Vec::new() }
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
            },
            r@Term::Rule(_) => Ok(write!(f, "{:?}", r)?)
        }
    }
}
