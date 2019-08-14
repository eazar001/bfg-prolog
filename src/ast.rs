use std::fmt::{Display, Formatter};

type Arity = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Number {
    Integer(i32),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Atom(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Rule {
    pub head: Structure,
    pub body: Vec<Structure>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Var(Var),
    Atom(Atom),
    Structure(Structure),
    Rule(Rule),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Structure {
    pub name: String,
    pub arity: Arity,
    pub args: Vec<Term>,
}

pub trait Structuralize {
    fn structuralize(&self) -> Option<Structure>;
    fn name(&self) -> String;
}

impl Structuralize for Atom {
    fn structuralize(&self) -> Option<Structure> {
        let Atom(a) = self;
        Some(Structure {
            name: a.clone(),
            arity: 0,
            args: Vec::new(),
        })
    }

    fn name(&self) -> String {
        self.0.clone()
    }
}

impl Structuralize for Structure {
    fn structuralize(&self) -> Option<Structure> {
        let mut args = Vec::new();

        for t in &self.args {
            match t {
                Term::Atom(a) => args.push(Term::Structure(a.structuralize().unwrap())),
                Term::Structure(ref s) => {
                    args.push(Term::Structure(s.structuralize().unwrap()));
                }
                _ => args.push(t.clone()),
            }
        }

        Some(Structure {
            name: String::from(&self.name),
            arity: self.arity,
            args,
        })
    }

    fn name(&self) -> String {
        let Structure { name, .. } = self;
        name.clone()
    }
}

impl Structuralize for Term {
    fn structuralize(&self) -> Option<Structure> {
        match self {
            Term::Atom(a) => Some(a.structuralize().unwrap()),
            Term::Structure(c) => Some(c.structuralize().unwrap()),
            t => None,
        }
    }

    fn name(&self) -> String {
        match self {
            Term::Structure(s) => s.name.clone(),
            Term::Atom(Atom(a)) => a.clone(),
            Term::Var(Var(v)) => v.clone(),
            Term::Rule(Rule { head, .. }) => head.name.clone(),
        }
    }
}

impl From<&Atom> for Structure {
    fn from(Atom(a): &Atom) -> Structure {
        Structure {
            name: a.clone(),
            arity: 0,
            args: Vec::new(),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Term::Var(Var(label)) => Ok(write!(f, "{}", label)?),
            Term::Atom(Atom(a)) => Ok(write!(f, "{}", a)?),
            Term::Structure(Structure { name, arity, args }) => match args.len() {
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
            },
            r @ Term::Rule(_) => Ok(write!(f, "{:?}", r)?),
        }
    }
}
