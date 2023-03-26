use std::{fmt::Display, ptr::NonNull};

use crate::{operator::Operator, params::INPUTS};

pub type Literal = i32;
pub type Mask = u8;

#[derive(Clone, Copy, Debug)]
pub struct Expr {
    pub left: Option<NonNull<Expr>>,
    pub right: Option<NonNull<Expr>>,
    pub literal: Literal,
    pub op: Operator,
    pub var_mask: Mask,
}
unsafe impl Send for Expr {}
unsafe impl Sync for Expr {}

impl Expr {
    pub fn prec(&self) -> u8 {
        self.op as u8 >> 4
    }

    pub fn variable(index: Literal) -> Self {
        Self {
            left: None,
            right: None,
            op: Operator::Literal,
            literal: !index,
            var_mask: 1 << index,
        }
    }

    pub fn literal(value: Literal) -> Self {
        Self {
            left: None,
            right: None,
            op: Operator::Literal,
            literal: value,
            var_mask: 0,
        }
    }

    pub fn is_literal(&self) -> bool {
        self.literal > 0
    }

    pub fn bin(el: NonNull<Expr>, er: NonNull<Expr>, op: Operator, var_mask: Mask) -> Self {
        Self {
            left: Some(el),
            right: Some(er),
            op,
            literal: 0,
            var_mask,
        }
    }

    pub fn unary(er: NonNull<Expr>, op: Operator) -> Self {
        Self {
            left: None,
            right: Some(er),
            op,
            literal: 0,
            var_mask: unsafe { *er.as_ptr() }.var_mask,
        }
    }

    pub fn parens(er: NonNull<Expr>) -> Self {
        Self {
            left: None,
            right: Some(er),
            op: Operator::Parens,
            literal: 0,
            var_mask: unsafe { *er.as_ptr() }.var_mask,
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(left) = self.left {
            Self::fmt(unsafe { &*left.as_ptr() }, f)?;
        }
        Display::fmt(&self.op, f)?;
        if let Some(right) = self.right {
            Self::fmt(unsafe { &*right.as_ptr() }, f)?;
            if self.op == Operator::Parens {
                write!(f, ")")?;
            }
        } else if self.literal < 0 {
            write!(f, "{}", INPUTS[(!self.literal) as usize].name)?;
        } else {
            write!(f, "{}", self.literal)?;
        }
        Ok(())
    }
}

// "3or" and ")or" are valid, but "nor" isn't.
pub fn ok_before_keyword(e: &Expr) -> bool {
    match e.right {
        None => e.literal >= 0,
        Some(right) => e.op == Operator::Parens || ok_before_keyword(unsafe { &*right.as_ptr() }),
    }
}

// "or3", "orn" are invalid. Need a unary op or parens.
pub fn ok_after_keyword(e: &Expr) -> bool {
    match e.left {
        None => e.op != Operator::Literal,
        Some(left) => ok_after_keyword(unsafe { &*left.as_ptr() }),
    }
}
