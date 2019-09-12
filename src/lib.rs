pub mod ast;

use self::ast::{Assertion, Atom, Clause, Const, Term, Var};
use lalrpop_util::lalrpop_mod;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Write;

lalrpop_mod!(pub parser);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment(HashMap<Var, Term>);
pub type Database = Vec<Assertion>;

#[derive(Debug, Copy, Clone)]
enum UnifyErr {
    NoUnify,
}

#[derive(Debug, Copy, Clone)]
enum SolveErr {
    NoSolution,
}

#[derive(Debug, Clone)]
enum Solution {
    Answer(String),
    Continuation(String, (Database, Vec<ChoicePoint>)),
}

#[derive(Debug, Clone)]
struct ChoicePoint {
    database: Database,
    environment: Environment,
    clause: Clause,
    depth: usize,
}

impl Display for Environment {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let env: Vec<_> = self.0.iter().filter(|(Var(_, n), _t)| *n == 0).collect();
        let mut response = String::from("\n");
        let last = env.last().cloned();

        match last {
            None => Ok(write!(f, "Yes")?),
            Some((Var(last_x, _), last_t)) => {
                for (Var(x, _n), t) in &env[..env.len() - 1] {
                    response.push_str(&format!("{} = {}\n", x, self.substitute_term(t)))
                }

                response.push_str(&format!("{} = {} ", last_x, self.substitute_term(last_t)));

                Ok(write!(f, "{}", response)?)
            }
        }
    }
}

impl Environment {
    fn new() -> Self {
        Environment(HashMap::new())
    }

    fn insert(&mut self, x: &Var, t: &Term) {
        self.0.insert(x.clone(), t.clone());
    }

    fn env(mut self, map: HashMap<Var, Term>) -> Self {
        self.0 = map;
        self
    }

    fn lookup(&self, x: &Var) -> Term {
        match self.0.get(x) {
            Some(t) => t.clone(),
            None => Term::Var(x.clone()),
        }
    }

    fn substitute_term(&self, t: &Term) -> Term {
        match t {
            Term::Var(x) => {
                let s = self.lookup(x);

                if Term::Var(x.clone()) == s {
                    return s;
                }

                self.substitute_term(&s)
            }
            t @ Term::Const(_) => t.clone(),
            Term::Atom(Atom {
                name: Const(name),
                args,
                ..
            }) => Term::Atom(Atom::new(
                name,
                args.iter().map(|t| self.substitute_term(t)).collect(),
            )),
        }
    }

    fn unify_terms(&self, t1: &Term, t2: &Term) -> Result<Self, UnifyErr> {
        match (self.substitute_term(t1), self.substitute_term(t2)) {
            (ref t1, ref t2) if t1 == t2 => Ok(self.clone()),
            (Term::Var(ref y), ref t) | (ref t, Term::Var(ref y)) => {
                if occurs(y, t) {
                    return Err(UnifyErr::NoUnify);
                }

                let (v, t) = (y.clone(), t.clone());
                let mut env = Environment::new().env(self.0.clone());

                env.insert(&v, &t);

                Ok(env)
            }
            (
                Term::Atom(Atom {
                    name: ref c1,
                    args: ref ts1,
                    ..
                }),
                Term::Atom(Atom {
                    name: ref c2,
                    args: ref ts2,
                    ..
                }),
            ) if c1 == c2 => self.unify_lists(ts1, ts2),
            _ => Err(UnifyErr::NoUnify),
        }
    }

    fn unify_lists(&self, l1: &[Term], l2: &[Term]) -> Result<Self, UnifyErr> {
        if l1.len() != l2.len() {
            return Err(UnifyErr::NoUnify);
        }

        let terms = l1.iter().zip(l2.iter());
        let mut env = self.clone();

        for (t1, t2) in terms {
            match env.unify_terms(t1, t2) {
                Err(UnifyErr::NoUnify) => {
                    return Err(UnifyErr::NoUnify);
                }
                Ok(e) => env = e,
            }
        }

        Ok(env)
    }

    fn unify_atoms(&self, a1: &Atom, a2: &Atom) -> Result<Self, UnifyErr> {
        let Atom {
            name: c1,
            args: ts1,
            ..
        } = a1;

        let Atom {
            name: c2,
            args: ts2,
            ..
        } = a2;

        if c1 == c2 {
            return self.unify_lists(ts1, ts2);
        }

        Err(UnifyErr::NoUnify)
    }
}

fn occurs(x: &Var, t: &Term) -> bool {
    match t {
        Term::Var(y) => x == y,
        Term::Const(_) => false,
        Term::Atom(Atom { args, .. }) => args.iter().any(|t| occurs(x, t)),
    }
}

fn renumber_term(n: usize, t: &Term) -> Term {
    match t {
        Term::Var(Var(x, _)) => Term::Var(Var(x.clone(), n)),
        c @ Term::Const(_) => c.clone(),
        Term::Atom(Atom {
            name: Const(c),
            args: ts,
            ..
        }) => Term::Atom(Atom::new(
            c,
            ts.iter().map(|t| renumber_term(n, t)).collect(),
        )),
    }
}

fn renumber_atom(n: usize, a: &Atom) -> Atom {
    let Atom {
        name: Const(c),
        args: ts,
        ..
    } = a;

    Atom::new(c, ts.iter().map(|t| renumber_term(n, t)).collect())
}

fn continue_search(kb: &[Assertion], ch: &[ChoicePoint]) -> Result<Solution, SolveErr> {
    match ch.split_first() {
        None => Err(SolveErr::NoSolution),
        Some((
            ChoicePoint {
                database: asrl,
                environment: env,
                clause: gs,
                depth: n,
            },
            cs,
        )) => solve(cs, kb, asrl, env, gs, *n),
    }
}

fn solve(
    ch: &[ChoicePoint],
    kb: &[Assertion],
    asrl: &[Assertion],
    env: &Environment,
    c: &[Atom],
    n: usize,
) -> Result<Solution, SolveErr> {
    match c.split_first() {
        None => Ok(match (&env.to_string()[..], ch) {
            (answer, []) => Solution::Answer(String::from(answer)),
            (answer, ch) => {
                let answer = if answer == "Yes" { "Yes " } else { answer };

                Solution::Continuation(String::from(answer), (kb.to_vec(), ch.to_vec()))
            }
        }),
        Some((
            Atom {
                name: Const(ref n),
                arity,
                ..
            },
            _,
        )) if n == "halt" && *arity == 0 => {
            std::process::exit(0);
        }
        Some((a, next_c)) => match reduce_atom(env, n, a, asrl) {
            None => continue_search(kb, ch),
            Some((next_asrl, next_env, mut d)) => {
                let mut next_ch = ch.to_vec();
                next_ch.push(ChoicePoint {
                    database: next_asrl,
                    environment: env.clone(),
                    clause: c.to_vec(),
                    depth: n,
                });

                d.extend_from_slice(next_c);

                solve(&next_ch, kb, kb, &next_env, &d, n + 1)
            }
        },
    }
}

fn reduce_atom(
    env: &Environment,
    n: usize,
    a: &Atom,
    asrl: &[Assertion],
) -> Option<(Vec<Assertion>, Environment, Vec<Atom>)> {
    match asrl.split_first() {
        None => None,
        Some((
            Assertion {
                head: b,
                clause: lst,
            },
            next_asrl,
        )) => {
            let next_env = env.unify_atoms(a, &renumber_atom(n, b));

            match next_env {
                Ok(next_env) => Some((
                    next_asrl.to_vec(),
                    next_env,
                    lst.iter().map(|a| renumber_atom(n, a)).collect(),
                )),
                Err(UnifyErr::NoUnify) => reduce_atom(env, n, a, next_asrl),
            }
        }
    }
}

pub fn solve_toplevel(interactive: bool, kb: &[Assertion], c: Clause) {
    let env = Environment::new();
    let asrl = kb.to_vec();
    let mut s = solve(&[], kb, &asrl, &env, &c, 1);
    let mut found = false;

    loop {
        match s {
            Err(SolveErr::NoSolution) if found => break,
            Err(SolveErr::NoSolution) => {
                println!("\nNo.");
                break;
            }
            Ok(Solution::Continuation(ref answer, (ref kb, ref ch))) => {
                found = true;

                print!("{}", answer);
                std::io::stdout().flush().expect("Could not flush stdout");

                let mut input_buffer = String::new();
                std::io::stdin()
                    .read_line(&mut input_buffer)
                    .expect("error reading input");

                if interactive {
                    match &input_buffer[..] {
                        ";\r\n" | ";\n" => {
                            s = continue_search(kb, ch);
                        }
                        _ => break,
                    }
                } else {
                    s = continue_search(kb, ch);
                }
            }
            Ok(Solution::Answer(answer)) => {
                println!("\n{}.", answer);
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unification_result(env: &Environment, results: &mut [(Var, Term)]) {
        let mut env: Vec<_> = env.0.iter().map(|(v, t)| (v.clone(), t.clone())).collect();
        env.sort();
        results.sort();
        assert_eq!(env, results);
    }

    #[test]
    fn test_unify_1_succeed() {
        let x = Term::Atom(Atom::new(
            "foo",
            vec![Term::Atom(Atom::new(
                "bar",
                vec![Term::Var(Var::new("X", 0))],
            ))],
        ));
        let f = Term::Atom(Atom::new(
            "foo",
            vec![Term::Atom(Atom::new(
                "bar",
                vec![Term::Const(Const::new("z"))],
            ))],
        ));

        let env = Environment::new().unify_terms(&x, &f);
        unification_result(
            &env.unwrap(),
            &mut [(Var::new("X", 0), Term::Const(Const::new("z")))],
        );
    }

    #[test]
    #[should_panic]
    fn test_unify_1_fail() {
        let x = Term::Atom(Atom::new(
            "foo",
            vec![Term::Atom(Atom::new(
                "baz",
                vec![Term::Var(Var::new("X", 0))],
            ))],
        ));
        let f = Term::Atom(Atom::new(
            "foo",
            vec![Term::Atom(Atom::new(
                "bar",
                vec![Term::Const(Const::new("z"))],
            ))],
        ));

        let env = Environment::new().unify_terms(&x, &f);
        env.unwrap();
    }

    #[test]
    fn test_unify_2_succeed() {
        let x = Term::Var(Var::new("X", 0));
        let f = Term::Atom(Atom::new(
            "foo",
            vec![Term::Atom(Atom::new(
                "bar",
                vec![Term::Const(Const::new("a"))],
            ))],
        ));

        let env = Environment::new().unify_terms(&f, &x);
        unification_result(&env.unwrap(), &mut [(Var::new("X", 0), f)]);
    }

    #[test]
    fn test_unify_3_succeed() {
        let x = Term::Var(Var::new("X", 0));
        let y = Term::Var(Var::new("Y", 0));;

        let env = Environment::new().unify_terms(&x, &y);
        unification_result(&env.unwrap(), &mut [(Var::new("X", 0), y)]);
    }

    #[test]
    fn test_unify_4_succeed() {
        let x1 = Term::Var(Var::new("X", 0));
        let x2 = Term::Var(Var::new("X", 0));;

        let env = Environment::new().unify_terms(&x1, &x2);
        unification_result(&env.unwrap(), &mut []);
    }

    #[test]
    fn test_unify_5_succeed() {
        let a1 = Term::Const(Const::new("a"));
        let a2 = Term::Const(Const::new("a"));

        let env = Environment::new().unify_terms(&a1, &a2);
        unification_result(&env.unwrap(), &mut []);
    }

    #[test]
    #[should_panic]
    fn test_unify_5_fail() {
        let a1 = Term::Const(Const::new("a"));
        let a2 = Term::Const(Const::new("b"));

        let env = Environment::new().unify_terms(&a1, &a2);
        env.unwrap();
    }

    #[test]
    fn test_unify_6_succeed() {
        let x = Term::Atom(Atom::new(
            "foo",
            vec![Term::Atom(Atom::new(
                "bar",
                vec![Term::Var(Var::new("X", 0)), Term::Const(Const::new("q"))],
            ))],
        ));
        let f = Term::Atom(Atom::new(
            "foo",
            vec![Term::Atom(Atom::new(
                "bar",
                vec![Term::Const(Const::new("z")), Term::Var(Var::new("V", 0))],
            ))],
        ));

        let env = Environment::new().unify_terms(&x, &f);
        unification_result(
            &env.unwrap(),
            &mut [
                (Var::new("V", 0), Term::Const(Const::new("q"))),
                (Var::new("X", 0), Term::Const(Const::new("z"))),
            ],
        );
    }

    #[test]
    fn test_unify_7_succeed() {
        let p1 = Term::Atom(Atom::new(
            "p",
            vec![
                Term::Var(Var::new("Z", 0)),
                Term::Atom(Atom::new(
                    "h",
                    vec![Term::Var(Var::new("Z", 0)), Term::Var(Var::new("W", 0))],
                )),
                Term::Atom(Atom::new("f", vec![Term::Var(Var::new("W", 0))])),
            ],
        ));
        let p2 = Term::Atom(Atom::new(
            "p",
            vec![
                Term::Atom(Atom::new("f", vec![Term::Var(Var::new("X", 0))])),
                Term::Atom(Atom::new(
                    "h",
                    vec![
                        Term::Var(Var::new("Y", 0)),
                        Term::Atom(Atom::new("f", vec![Term::Const(Const::new("a"))])),
                    ],
                )),
                Term::Var(Var::new("Y", 0)),
            ],
        ));

        let env = Environment::new().unify_terms(&p1, &p2);
        unification_result(
            &env.unwrap(),
            &mut [
                (
                    Var::new("W", 0),
                    Term::Atom(Atom::new("f", vec![Term::Const(Const::new("a"))])),
                ),
                (
                    Var::new("X", 0),
                    Term::Atom(Atom::new("f", vec![Term::Const(Const::new("a"))])),
                ),
                (
                    Var::new("Y", 0),
                    Term::Atom(Atom::new("f", vec![Term::Var(Var::new("X", 0))])),
                ),
                (
                    Var::new("Z", 0),
                    Term::Atom(Atom::new("f", vec![Term::Var(Var::new("X", 0))])),
                ),
            ],
        )
    }

    #[test]
    #[should_panic]
    fn test_unify_7_fail() {
        let p1 = Term::Atom(Atom::new(
            "p",
            vec![
                Term::Var(Var::new("Z", 0)),
                Term::Atom(Atom::new(
                    "g",
                    vec![Term::Var(Var::new("Z", 0)), Term::Var(Var::new("W", 0))],
                )),
                Term::Atom(Atom::new("f", vec![Term::Var(Var::new("W", 0))])),
            ],
        ));
        let p2 = Term::Atom(Atom::new(
            "p",
            vec![
                Term::Atom(Atom::new("f", vec![Term::Var(Var::new("X", 0))])),
                Term::Atom(Atom::new(
                    "h",
                    vec![
                        Term::Var(Var::new("Y", 0)),
                        Term::Atom(Atom::new("f", vec![Term::Const(Const::new("a"))])),
                    ],
                )),
                Term::Var(Var::new("Y", 0)),
            ],
        ));

        let env = Environment::new().unify_terms(&p1, &p2);
        env.unwrap();
    }
}
