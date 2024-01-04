use std::hash::{Hash, Hasher};
use std::{fmt::Display, ptr::NonNull};

use crate::{
    operator::*,
    params::{Num, INPUTS},
    vec::Vector,
};

pub type Mask = u8;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expr {
    pub left: Option<NonNull<Expr>>,
    pub right: Option<NonNull<Expr>>,
    pub op_idx: OpIndex,
    pub var_mask: Mask,
    pub output: Vector,
}
unsafe impl Send for Expr {}
unsafe impl Sync for Expr {}

impl Expr {
    pub fn prec(&self) -> Prec {
        self.op_idx.prec()
    }

    pub fn variable(index: usize, output: Vector) -> Self {
        Self {
            left: None,
            right: None,
            op_idx: OP_INDEX_VARIABLE,
            var_mask: 1 << index,
            output,
        }
    }

    pub fn literal(value: Num) -> Self {
        Self {
            left: None,
            right: None,
            op_idx: OP_INDEX_LITERAL,
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
        op_idx: OpIndex,
        var_mask: Mask,
        output: Vector,
    ) -> Self {
        Self {
            left: Some(el),
            right: Some(er),
            op_idx,
            var_mask,
            output,
        }
    }

    pub fn unary(er: &Expr, op_idx: OpIndex, output: Vector) -> Self {
        Self {
            left: None,
            right: Some(er.into()),
            op_idx,
            var_mask: er.var_mask,
            output,
        }
    }

    pub fn parens(er: &Expr) -> Self {
        Self {
            left: None,
            right: Some(er.into()),
            op_idx: OP_INDEX_PARENS,
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
        Display::fmt(&self.op_idx, f)?;
        if let Some(right) = self.right {
            Self::fmt(unsafe { right.as_ref() }, f)?;
            if self.op_idx == OP_INDEX_PARENS {
                write!(f, ")")?;
            }
        } else if self.op_idx == OP_INDEX_VARIABLE {
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

#[derive(Eq, Clone, Copy)]
pub struct NonNullExpr(NonNull<Expr>);
unsafe impl Send for NonNullExpr {}
unsafe impl Sync for NonNullExpr {}

impl NonNullExpr {
    pub fn as_ptr(&self) -> *const Expr {
        self.0.as_ptr()
    }

    pub fn as_mut_ptr(&self) -> *mut Expr {
        self.0.as_ptr()
    }
}

impl Into<NonNullExpr> for &Expr {
    fn into(self) -> NonNullExpr {
        NonNullExpr(self.into())
    }
}

impl AsRef<Expr> for NonNullExpr {
    fn as_ref(&self) -> &Expr {
        unsafe { self.0.as_ref() }
    }
}

impl Hash for NonNullExpr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().output.hash(state);
    }
}

impl PartialEq for NonNullExpr {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().output == other.as_ref().output
    }
}

// "3or" and ")or" are valid, but "nor" isn't.
pub fn ok_before_keyword(e: &Expr) -> bool {
    match e.right {
        None => e.op_idx == OP_INDEX_LITERAL,
        Some(right) => e.op_idx == OP_INDEX_PARENS || ok_before_keyword(unsafe { right.as_ref() }),
    }
}

// "or3", "orn" are invalid. Need a unary op or parens.
pub fn ok_after_keyword(e: &Expr) -> bool {
    match e.left {
        None => e.op_idx != OP_INDEX_LITERAL && e.op_idx != OP_INDEX_VARIABLE,
        Some(left) => ok_after_keyword(unsafe { left.as_ref() }),
    }
}
