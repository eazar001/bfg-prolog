use crate::{Functor, Register, Store};
use self::Instruction::*;

type InstructionSize = usize;

pub enum Instruction {
    PutStructure(Functor, Register),
    PutVariable(Register, Register),
    PutValue(Register, Register),
    GetVariable(Register, Register),
    GetValue(Register, Register),
    SetVariable(Register),
    SetValue(Register),
    GetStructure(Functor, Register),
    UnifyVariable(Register),
    UnifyValue(Register),
    Unify(Store, Store)
}
