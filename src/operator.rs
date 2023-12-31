use std::fmt::Display;

use crate::{
    expr::{ok_after_keyword, ok_before_keyword, Expr},
    params::{Num, C_STYLE_BIT_SHIFT, C_STYLE_MOD},
    vec::Vector,
};

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Or = 0x30,
    SpaceOr = 0x31,
    OrSpace = 0x32,
    SpaceOrSpace = 0x33,
    Lt = 0x50,
    Le = 0x51,
    Gt = 0x52,
    Ge = 0x53,
    Eq = 0x54,
    Ne = 0x55,
    BitOr = 0x60,
    BitXor = 0x70,
    BitAnd = 0x80,
    BitShl = 0x90,
    BitShr = 0x91,
    Add = 0xA0,
    Sub = 0xA1,
    Mul = 0xB0,
    Mod = 0xB1,
    Div1 = 0xB2,
    Div2 = 0xB3,
    Gcd = 0xB4,
    Neg = 0xC0,
    BitNeg = 0xC1,
    Exp = 0xD0,
    Parens = 0xE0,
    Literal = 0xF0,
    Variable = 0xF1,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Or => write!(f, "or"),
            Operator::SpaceOr => write!(f, " or"),
            Operator::OrSpace => write!(f, "or "),
            Operator::SpaceOrSpace => write!(f, " or "),
            Operator::Lt => write!(f, "<"),
            Operator::Le => write!(f, "<="),
            Operator::Gt => write!(f, ">"),
            Operator::Ge => write!(f, ">="),
            Operator::Eq => write!(f, "=="),
            Operator::Ne => write!(f, "!="),
            Operator::BitOr => write!(f, "|"),
            Operator::BitXor => write!(f, "^"),
            Operator::BitAnd => write!(f, "&"),
            Operator::BitShl => write!(f, "<<"),
            Operator::BitShr => write!(f, ">>"),
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Mod => write!(f, "%"),
            Operator::Div1 => write!(f, "/"),
            Operator::Div2 => write!(f, "//"),
            Operator::Gcd => write!(f, "∨"),
            Operator::Neg => write!(f, "-"),
            Operator::BitNeg => write!(f, "~"),
            Operator::Exp => write!(f, "**"),
            Operator::Parens => write!(f, "("),
            Operator::Literal | Operator::Variable => write!(f, ""),
        }
    }
}

impl From<UnaryOperator> for Operator {
    #[inline]
    fn from(op: UnaryOperator) -> Self {
        match op {
            UnaryOperator::Neg => Operator::Neg,
            UnaryOperator::BitNeg => Operator::BitNeg,
        }
    }
}

impl From<BinaryOperator> for Operator {
    #[inline]
    fn from(op: BinaryOperator) -> Self {
        use BinaryOperator::*;
        match op {
            Or => Operator::Or,
            SpaceOr => Operator::SpaceOr,
            OrSpace => Operator::OrSpace,
            SpaceOrSpace => Operator::SpaceOrSpace,
            Lt => Operator::Lt,
            Le => Operator::Le,
            Gt => Operator::Gt,
            Ge => Operator::Ge,
            Eq => Operator::Eq,
            Ne => Operator::Ne,
            BitOr => Operator::BitOr,
            BitXor => Operator::BitXor,
            BitAnd => Operator::BitAnd,
            BitShl => Operator::BitShl,
            BitShr => Operator::BitShr,
            Add => Operator::Add,
            Sub => Operator::Sub,
            Mul => Operator::Mul,
            Mod => Operator::Mod,
            Div1 => Operator::Div1,
            Div2 => Operator::Div2,
            Gcd => Operator::Gcd,
            Exp => Operator::Exp,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Neg,
    BitNeg,
}

impl UnaryOperator {
    #[inline]
    pub fn can_apply(&self, er: &Expr) -> bool {
        er.prec() >= 12
    }

    #[inline]
    pub fn apply(&self, x: Num) -> Num {
        match self {
            UnaryOperator::Neg => 0 - x,
            UnaryOperator::BitNeg => !x,
        }
    }

    #[inline]
    pub fn vec_apply(&self, v: Vector) -> Vector {
        v.map(|x| self.apply(x))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Or,
    SpaceOr,
    OrSpace,
    SpaceOrSpace,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    BitOr,
    BitXor,
    BitAnd,
    BitShl,
    BitShr,
    Add,
    Sub,
    Mul,
    Mod,
    Div1,
    Div2,
    Gcd,
    Exp,
}

impl BinaryOperator {
    #[inline]
    pub fn length(&self) -> usize {
        use BinaryOperator::*;
        match self {
            Lt | Gt | BitOr | BitXor | BitAnd | Add | Sub | Mul | Mod | Div1 | Gcd => 1,
            Or | Le | Ge | Eq | Ne | BitShl | BitShr | Div2 | Exp => 2,
            SpaceOr | OrSpace => 3,
            SpaceOrSpace => 4,
        }
    }

    #[inline]
    pub fn can_apply(&self, el: &Expr, er: &Expr) -> bool {
        use BinaryOperator::*;

        // For commutative operators, choose an arbitrary order for the two operands based on memory
        // address.
        let use_commutative_op =
            |prec| el.prec() >= prec && er.prec() >= prec && el as *const Expr <= er as *const Expr;
        match self {
            Or => el.prec() >= 3 && er.prec() > 3 && ok_before_keyword(el) && ok_after_keyword(er),
            SpaceOr => {
                el.prec() >= 3 && er.prec() > 3 && !ok_before_keyword(el) && ok_after_keyword(er)
            }
            OrSpace => {
                el.prec() >= 3 && er.prec() > 3 && ok_before_keyword(el) && !ok_after_keyword(er)
            }
            SpaceOrSpace => {
                el.prec() >= 3 && er.prec() > 3 && !ok_before_keyword(el) && !ok_after_keyword(er)
            }
            Lt | Le | Gt | Ge | Eq | Ne => el.prec() >= 5 && er.prec() > 5,
            BitOr => use_commutative_op(6),
            BitXor => use_commutative_op(7),
            BitAnd => use_commutative_op(8),
            BitShl | BitShr => el.prec() >= 9 && er.prec() > 9,
            Add => use_commutative_op(10),
            Sub => el.prec() >= 10 && er.prec() > 10,
            Mul => {
                er.prec() > 11
                    && (el.prec() > 11 && el as *const Expr <= er as *const Expr || el.prec() == 11)
            }
            Mod | Div1 | Div2 | Gcd => el.prec() >= 11 && er.prec() > 11,
            Exp => el.prec() > 13 && er.prec() >= 13,
        }
    }

    #[inline]
    pub fn apply(&self, x: Num, y: Num) -> Option<Num> {
        use BinaryOperator::*;
        match self {
            Or | SpaceOr | OrSpace | SpaceOrSpace => Some(if x != 0 { x } else { y }),
            Lt => Some((x < y) as Num),
            Le => Some((x <= y) as Num),
            Gt => Some((x > y) as Num),
            Ge => Some((x >= y) as Num),
            Eq => Some((x == y) as Num),
            Ne => Some((x != y) as Num),
            BitOr => Some(x | y),
            BitXor => Some(x ^ y),
            BitAnd => Some(x & y),
            BitShl => {
                if C_STYLE_BIT_SHIFT || y >= 0 && y < Num::BITS as Num {
                    Some(x << y)
                } else {
                    None
                }
            }
            BitShr => {
                if C_STYLE_BIT_SHIFT {
                    Some(x >> y)
                } else if y >= 0 {
                    Some(x >> y.min(Num::BITS as Num - 1))
                } else {
                    None
                }
            }
            Add => Some(x + y),
            Sub => Some(x - y),
            Mul => Some(x * y),
            Mod => {
                if C_STYLE_MOD {
                    x.checked_rem(y)
                } else {
                    if y == 0 || (Num::MIN < 0 && x == Num::MIN && y == !0) {
                        return None;
                    }
                    Some(num_integer::mod_floor(x, y))
                }
            }
            Div1 | Div2 => {
                if C_STYLE_MOD {
                    x.checked_div(y)
                } else {
                    if y == 0 || (Num::MIN < 0 && x == Num::MIN && y == !0) {
                        return None;
                    }
                    Some(num_integer::div_floor(x, y))
                }
            }
            Gcd => Some(num_integer::gcd(x, y)),
            Exp => x.checked_pow(y.try_into().ok()?),
        }
    }

    #[inline]
    pub fn vec_apply(&self, mut left: Vector, right: &Vector) -> Option<Vector> {
        for (x, y) in left.iter_mut().zip(right.iter()) {
            *x = self.apply(*x, *y)?;
        }
        Some(left)
    }
}
