pub mod ast;

use self::ast::{Assertion, Atom, Clause, Const, Term, Var};
use lalrpop_util::lalrpop_mod;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Write;

lalrpop_mod!(pub parser);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment(HashMap<Var, Term>);
pub type KnowledgeBase = Vec<Assertion>;
pub type Assertions = Vec<Assertion>;

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
    Continuation(String, (KnowledgeBase, Vec<ChoicePoint>)),
}

#[derive(Debug, Clone)]
struct ChoicePoint {
    assertions: KnowledgeBase,
    environment: Environment,
    clause: Clause,
    depth: usize,
}

impl Display for Environment {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let mut env: Vec<_> = self.0.iter().filter(|(Var(_, n), _)| *n == 0).collect();
        env.sort();
        let mut response = String::from("\n");
        let last = env.last().cloned();

        match last {
            None => Ok(write!(f, "Yes")?),
            Some((Var(last_x, _), last_t)) => {
                for (Var(x, _), t) in &env[..env.len() - 1] {
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
        if let Term::Const(_) = t {
            return t.clone();
        }

        let mut t = t.clone();
        let mut temp = t;

        loop {
            match temp {
                Term::Var(x) => {
                    t = self.lookup(&x);

                    if Term::Var(x) == t {
                        return t;
                    }

                    temp = t;
                }
                Term::Atom(mut a) => {
                    let mut next_atoms = self.substitute_atom(&mut a);

                    while let Some(a) = next_atoms.pop() {
                        next_atoms.extend(self.substitute_atom(a));
                    }

                    return Term::Atom(a);
                }
                Term::Const(_) => return temp,
            }
        }
    }

    fn substitute_atom<'a>(&self, a: &'a mut Atom) -> Vec<&'a mut Atom> {
        let mut next = Vec::new();

        for arg in &mut a.args {
            match arg {
                ref t @ Term::Var(_) => {
                    *arg = self.substitute_term(*t);
                }
                Term::Atom(ref mut a) => next.push(a),
                _ => (),
            }
        }

        next
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

    fn reduce_atom(
        &self,
        n: usize,
        a: &Atom,
        mut asrl: &[Assertion],
    ) -> Option<(KnowledgeBase, Environment, Clause)> {
        while let Some((
            Assertion {
                head: b,
                clause: lst,
            },
            next_asrl,
        )) = asrl.split_first()
        {
            let next_env = self.unify_atoms(a, &renumber_atom(n, b));

            match next_env {
                Ok(next_env) => {
                    return Some((
                        next_asrl.to_vec(),
                        next_env,
                        lst.iter().map(|a| renumber_atom(n, a)).collect(),
                    ));
                }
                Err(UnifyErr::NoUnify) => {
                    asrl = &next_asrl;
                }
            }
        }

        None
    }

    fn solve(
        &self,
        mut ch: Vec<ChoicePoint>,
        kb: &[Assertion],
        asrl: &[Assertion],
        mut c: Clause,
        mut n: usize,
    ) -> Result<Solution, SolveErr> {
        let mut env = self.clone();
        let mut asrl = asrl;
        let mut next_asrl = Some(asrl.to_vec());
        c.reverse();

        while let Some(a) = c.pop() {
            let Atom {
                name: Const(ref atom_name),
                arity,
                ..
            } = a;

            if atom_name == "halt" && arity == 0 {
                std::process::exit(0);
            }

            asrl = match next_asrl {
                None => kb,
                Some(ref assertions) => assertions,
            };

            match env.reduce_atom(n, &a, &asrl) {
                None => match ch.pop() {
                    None => return Err(SolveErr::NoSolution),
                    Some(ChoicePoint {
                        assertions: ch_asrl,
                        environment: next_env,
                        clause: gs,
                        depth: next_n,
                    }) => {
                        env = next_env;
                        next_asrl = Some(ch_asrl);
                        c = gs;
                        n = next_n;
                    }
                },
                Some((ch_asrl, next_env, mut d)) => {
                    let mut ch_clause = c.clone();
                    ch_clause.push(a);

                    let mut ch_buffer = vec![ChoicePoint {
                        assertions: ch_asrl,
                        environment: env,
                        clause: ch_clause,
                        depth: n,
                    }];

                    ch_buffer.extend_from_slice(&ch);
                    d.extend_from_slice(&c);

                    env = next_env;
                    ch = ch_buffer;
                    next_asrl = None;
                    c = d;
                    n += 1;
                }
            }
        }

        Ok(match (&env.to_string()[..], &ch[..]) {
            (answer, []) => Solution::Answer(String::from(answer)),
            (answer, ch) => {
                let answer = if answer == "Yes" { "Yes " } else { answer };
                Solution::Continuation(String::from(answer), (kb.to_vec(), ch.to_vec()))
            }
        })
    }
}

fn occurs(x: &Var, t: &Term) -> bool {
    match t {
        Term::Var(y) => x == y,
        Term::Const(_) => false,
        Term::Atom(a) => occurs_atom(x, a),
    }
}

fn occurs_atom(x: &Var, a: &Atom) -> bool {
    let mut atom_queue = vec![a];

    while let Some(a) = atom_queue.pop() {
        for t in &a.args {
            match t {
                Term::Var(y) if x == y => return true,
                Term::Atom(ref q) => atom_queue.push(q),
                _ => (),
            }
        }
    }

    false
}

fn renumber_term(n: usize, t: &Term) -> Term {
    match t {
        Term::Var(Var(x, _)) => Term::Var(Var(x.clone(), n)),
        c @ Term::Const(_) => c.clone(),
        Term::Atom(a) => Term::Atom(renumber_atom(n, a)),
    }
}

fn renumber_atom(n: usize, a: &Atom) -> Atom {
    let mut a = a.clone();
    let mut next_atoms = renumber_atom_level(n, &mut a);

    while let Some(a) = next_atoms.pop() {
        next_atoms.extend(renumber_atom_level(n, a));
    }

    a
}

fn renumber_atom_level(n: usize, a: &mut Atom) -> Vec<&mut Atom> {
    let mut next = Vec::new();

    for arg in &mut a.args {
        match arg {
            ref t @ Term::Var(_) => {
                *arg = renumber_term(n, *t);
            }
            Term::Atom(ref mut a) => next.push(a),
            _ => (),
        }
    }

    next
}

fn continue_search(kb: &[Assertion], ch: &[ChoicePoint]) -> Result<Solution, SolveErr> {
    match ch.split_first() {
        None => Err(SolveErr::NoSolution),
        Some((
            ChoicePoint {
                assertions: asrl,
                environment: env,
                clause: gs,
                depth: n,
            },
            cs,
        )) => env.solve(cs.to_vec(), kb, asrl, gs.clone(), *n),
    }
}

pub fn solve_toplevel(interactive: bool, kb: &[Assertion], c: Clause) -> Vec<String> {
    let env = Environment::new();
    let asrl = kb.to_vec();
    let mut s = env.solve(Vec::new(), kb, &asrl, c, 1);
    let mut answers = Vec::new();
    let mut found = false;

    loop {
        match s {
            Err(SolveErr::NoSolution) if found => break,
            Err(SolveErr::NoSolution) => {
                println!("\nNo.");
                if !interactive {
                    answers.push(String::from("No"))
                }
                break;
            }
            Ok(Solution::Continuation(ref answer, (ref kb, ref ch))) => {
                found = true;

                print!("{}", answer);
                if !interactive {
                    answers.push(answer.clone())
                }

                std::io::stdout().flush().expect("Could not flush stdout");

                if interactive {
                    let mut input_buffer = String::new();
                    std::io::stdin()
                        .read_line(&mut input_buffer)
                        .expect("error reading input");

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
                if !interactive {
                    answers.push(answer)
                }
                break;
            }
        }
    }

    answers
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
    fn test_unify_1_succeeds() {
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
    fn test_unify_1_fails() {
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
    fn test_unify_2_succeeds() {
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
    fn test_unify_3_succeeds() {
        let x = Term::Var(Var::new("X", 0));
        let y = Term::Var(Var::new("Y", 0));;

        let env = Environment::new().unify_terms(&x, &y);
        unification_result(&env.unwrap(), &mut [(Var::new("X", 0), y)]);
    }

    #[test]
    fn test_unify_4_succeeds() {
        let x1 = Term::Var(Var::new("X", 0));
        let x2 = Term::Var(Var::new("X", 0));;

        let env = Environment::new().unify_terms(&x1, &x2);
        unification_result(&env.unwrap(), &mut []);
    }

    #[test]
    fn test_unify_5_succeeds() {
        let a1 = Term::Const(Const::new("a"));
        let a2 = Term::Const(Const::new("a"));

        let env = Environment::new().unify_terms(&a1, &a2);
        unification_result(&env.unwrap(), &mut []);
    }

    #[test]
    #[should_panic]
    fn test_unify_5_fails() {
        let a1 = Term::Const(Const::new("a"));
        let a2 = Term::Const(Const::new("b"));

        let env = Environment::new().unify_terms(&a1, &a2);
        env.unwrap();
    }

    #[test]
    fn test_unify_6_succeeds() {
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
    fn test_unify_7_succeeds() {
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
    fn test_unify_7_fails() {
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

    #[test]
    fn test_unify_8_succeeds() {
        let f1 = Term::Atom(Atom::new(
            "f",
            vec![
                Term::Var(Var::new("X", 0)),
                Term::Atom(Atom::new(
                    "g",
                    vec![
                        Term::Var(Var::new("X", 0)),
                        Term::Atom(Atom::new("a", vec![])),
                    ],
                )),
            ],
        ));
        let f2 = Term::Atom(Atom::new(
            "f",
            vec![
                Term::Atom(Atom::new("b", vec![])),
                Term::Var(Var::new("Y", 0)),
            ],
        ));

        let env = Environment::new().unify_terms(&f1, &f2);
        unification_result(
            &env.unwrap(),
            &mut [
                (Var::new("X", 0), Term::Atom(Atom::new("b", vec![]))),
                (
                    Var::new("Y", 0),
                    Term::Atom(Atom::new(
                        "g",
                        vec![
                            Term::Atom(Atom::new("b", vec![])),
                            Term::Atom(Atom::new("a", vec![])),
                        ],
                    )),
                ),
            ],
        )
    }

    #[test]
    fn test_unify_9_succeeds() {
        let f1 = Atom::new(
            "f",
            vec![
                Term::Var(Var::new("X", 0)),
                Term::Atom(Atom::new(
                    "g",
                    vec![
                        Term::Var(Var::new("X", 0)),
                        Term::Atom(Atom::new("a", vec![])),
                    ],
                )),
            ],
        );
        let f2 = Atom::new(
            "f",
            vec![
                Term::Atom(Atom::new("b", vec![])),
                Term::Var(Var::new("Y", 0)),
            ],
        );

        let env = Environment::new().unify_atoms(&f1, &f2);
        unification_result(
            &env.unwrap(),
            &mut [
                (Var::new("X", 0), Term::Atom(Atom::new("b", vec![]))),
                (
                    Var::new("Y", 0),
                    Term::Atom(Atom::new(
                        "g",
                        vec![
                            Term::Atom(Atom::new("b", vec![])),
                            Term::Atom(Atom::new("a", vec![])),
                        ],
                    )),
                ),
            ],
        )
    }

    #[test]
    #[should_panic]
    fn test_unify_9_fails() {
        let f1 = Atom::new(
            "f",
            vec![
                Term::Var(Var::new("X", 0)),
                Term::Atom(Atom::new(
                    "g",
                    vec![
                        Term::Var(Var::new("X", 0)),
                        Term::Atom(Atom::new("a", vec![])),
                    ],
                )),
            ],
        );
        let f2 = Atom::new(
            "f",
            vec![
                Term::Atom(Atom::new("b", vec![])),
                Term::Var(Var::new("X", 0)),
            ],
        );

        let env = Environment::new().unify_atoms(&f1, &f2);
        env.unwrap();
    }

    #[test]
    fn test_unify_10_succeeds() {
        let l1 = vec![Term::Atom(Atom::new("a", vec![]))];
        let l2 = vec![Term::Var(Var::new("X", 1))];
        let env = Environment::new().unify_lists(&l1, &l2);

        unification_result(
            &env.unwrap(),
            &mut [(Var::new("X", 1), Term::Atom(Atom::new("a", vec![])))],
        )
    }

    #[test]
    #[should_panic]
    fn test_unify_10_fails() {
        let l1 = vec![
            Term::Atom(Atom::new("a", vec![])),
            Term::Atom(Atom::new("a", vec![])),
        ];
        let l2 = vec![Term::Var(Var::new("X", 0))];
        let env = Environment::new().unify_lists(&l1, &l2);

        env.unwrap();
    }

    #[test]
    #[should_panic]
    fn test_unify_11_fails() {
        let l1 = vec![Term::Atom(Atom::new("a", vec![]))];
        let l2 = vec![Term::Atom(Atom::new("b", vec![]))];
        let env = Environment::new().unify_lists(&l1, &l2);

        env.unwrap();
    }

    #[test]
    fn test_unify_12_succeeds() {
        let l1 = vec![
            Term::Atom(Atom::new(
                "a",
                vec![Term::Atom(Atom::new(
                    "x",
                    vec![Term::Const(Const::new("c"))],
                ))],
            )),
            Term::Atom(Atom::new("b", vec![])),
        ];
        let l2 = vec![
            Term::Atom(Atom::new("a", vec![Term::Var(Var::new("X", 0))])),
            Term::Atom(Atom::new("b", vec![])),
        ];
        let env = Environment::new().unify_lists(&l1, &l2);

        unification_result(
            &env.unwrap(),
            &mut [(
                Var::new("X", 0),
                Term::Atom(Atom::new("x", vec![Term::Const(Const::new("c"))])),
            )],
        )
    }

    #[test]
    #[should_panic]
    fn test_unify_12_fails() {
        let l1 = vec![
            Term::Atom(Atom::new(
                "a",
                vec![Term::Atom(Atom::new(
                    "x",
                    vec![Term::Const(Const::new("c"))],
                ))],
            )),
            Term::Atom(Atom::new("q", vec![])),
        ];
        let l2 = vec![
            Term::Atom(Atom::new("a", vec![Term::Var(Var::new("X", 0))])),
            Term::Atom(Atom::new("b", vec![])),
        ];
        let env = Environment::new().unify_lists(&l1, &l2);

        env.unwrap();
    }

    #[test]
    fn test_occurs_1_succeeds() {
        let v = Var::new("X", 0);
        let t = Term::Var(Var::new("X", 0));

        assert!(occurs(&v, &t))
    }

    #[test]
    fn test_occurs_1_fails() {
        let v = Var::new("X", 0);
        let t = Term::Var(Var::new("X", 1));

        assert!(!occurs(&v, &t))
    }

    #[test]
    fn test_occurs_2_fails() {
        let v = Var::new("X", 0);
        let t = Term::Var(Var::new("Y", 0));

        assert!(!occurs(&v, &t))
    }

    #[test]
    fn test_occurs_3_succeeds() {
        let v = Var::new("X", 0);
        let t = Term::Atom(Atom::new(
            "x",
            vec![Term::Atom(Atom::new(
                "y",
                vec![Term::Var(Var::new("X", 0))],
            ))],
        ));

        assert!(occurs(&v, &t))
    }

    #[test]
    fn test_occurs_3_fails() {
        let v = Var::new("X", 0);
        let t = Term::Atom(Atom::new(
            "x",
            vec![Term::Atom(Atom::new(
                "y",
                vec![Term::Var(Var::new("Var", 0))],
            ))],
        ));

        assert!(!occurs(&v, &t))
    }
}
