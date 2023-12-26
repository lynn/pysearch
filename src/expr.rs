use std::hash::{Hash, Hasher};
use std::{fmt::Display, ptr::NonNull};

use crate::{
    operator::Operator,
    params::{Num, INPUTS},
    vec::Vector,
};

pub type Mask = u8;

#[derive(Clone, Debug, Eq)]
pub struct Expr {
    pub left: Option<NonNull<Expr>>,
    pub right: Option<NonNull<Expr>>,
    pub op: Operator,
    pub var_mask: Mask,
    pub output: Vector,
}
unsafe impl Send for Expr {}
unsafe impl Sync for Expr {}

impl Expr {
    pub fn prec(&self) -> u8 {
        self.op as u8 >> 4
    }

    pub fn variable(index: usize, output: Vector) -> Self {
        Self {
            left: None,
            right: None,
            op: Operator::Variable,
            var_mask: 1 << index,
            output,
        }
    }

    pub fn literal(value: Num) -> Self {
        Self {
            left: None,
            right: None,
            op: Operator::Literal,
            var_mask: 0,
            output: Vector::constant(value),
        }
    }

    pub fn is_literal(&self) -> bool {
        self.var_mask == 0
    }

    pub fn bin(
        el: NonNull<Expr>,
        er: NonNull<Expr>,
        op: Operator,
        var_mask: Mask,
        output: Vector,
    ) -> Self {
        Self {
            left: Some(el),
            right: Some(er),
            op,
            var_mask,
            output,
        }
    }

    pub fn unary(er: &Expr, op: Operator, output: Vector) -> Self {
        Self {
            left: None,
            right: Some(er.into()),
            op,
            var_mask: er.var_mask,
            output,
        }
    }

    pub fn parens(er: &Expr) -> Self {
        Self {
            left: None,
            right: Some(er.into()),
            op: Operator::Parens,
            var_mask: er.var_mask,
            output: er.output.clone(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(left) = self.left {
            Self::fmt(unsafe { left.as_ref() }, f)?;
        }
        Display::fmt(&self.op, f)?;
        if let Some(right) = self.right {
            Self::fmt(unsafe { right.as_ref() }, f)?;
            if self.op == Operator::Parens {
                write!(f, ")")?;
            }
        } else if self.op == Operator::Variable {
            write!(
                f,
                "{}",
                INPUTS[self.var_mask.trailing_zeros() as usize].name
            )?;
        } else {
            write!(f, "{}", self.output[0])?;
        }
        Ok(())
    }
}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.output.hash(state);
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        return self.output == other.output;
    }
}

// "3or" and ")or" are valid, but "nor" isn't.
pub fn ok_before_keyword(e: &Expr) -> bool {
    match e.right {
        None => e.op == Operator::Literal,
        Some(right) => e.op == Operator::Parens || ok_before_keyword(unsafe { right.as_ref() }),
    }
}

// "or3", "orn" are invalid. Need a unary op or parens.
pub fn ok_after_keyword(e: &Expr) -> bool {
    match e.left {
        None => e.op != Operator::Literal && e.op != Operator::Variable,
        Some(left) => ok_after_keyword(unsafe { left.as_ref() }),
    }
}
