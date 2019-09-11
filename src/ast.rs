use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Var(Var),
    Const(Const),
    Atom(Atom),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(pub String, pub usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Const(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Atom {
    pub name: Const,
    pub arity: Arity,
    pub args: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Assertion {
    pub head: Atom,
    pub clause: Clause,
}

pub type Arity = usize;
pub type Clause = Vec<Atom>;

impl Assertion {
    pub fn new(head: Atom, clause: Clause) -> Self {
        Assertion { head, clause }
    }
}

impl Atom {
    pub fn new(name: &str, args: Vec<Term>) -> Self {
        Atom {
            name: Const::new(name),
            arity: args.len(),
            args,
        }
    }
}

impl Var {
    pub fn new(name: &str, n: usize) -> Self {
        Var(String::from(name), n)
    }
}

impl Const {
    pub fn new(name: &str) -> Self {
        Const(String::from(name))
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Term::Var(Var(name, n)) if *n == 0 => Ok(write!(f, "{}", name)?),
            Term::Var(Var(name, n)) => Ok(write!(f, "{}{}", name, n)?),
            Term::Const(Const(a)) => Ok(write!(f, "{}", a)?),
            Term::Atom(Atom {
                name: Const(name),
                args,
                ..
            }) => match args.last() {
                None => Ok(write!(f, "{}", &name)?),
                Some(last) => {
                    let init = &args[..args.len() - 1];
                    let mut args = String::new();

                    for arg in init {
                        args.push_str(&format!("{}, ", arg));
                    }

                    args.push_str(&format!("{})", last));

                    Ok(write!(f, "{}({}", &name, args)?)
                }
            },
        }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Ok(write!(f, "{}", Term::Var(self.clone()))?)
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Ok(write!(f, "{}", Term::Const(self.clone()))?)
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Ok(write!(f, "{}", Term::Atom(self.clone()))?)
    }
}
