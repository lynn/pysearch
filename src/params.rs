use crate::{
    expr::Expr,
    operator::{BinaryOperator, BinaryOperator::*, UnaryOperator, UnaryOperator::*},
    vec::Vector,
};

pub type Num = i32;

pub struct Input {
    pub name: &'static str,
    pub vec: &'static [Num],
}

pub const INPUTS: &[Input] = &[Input {
    name: "n",
    vec: &['E' as i32, 'W' as i32, 'N' as i32, 'S' as i32],
}];

/// This function gets applied to the output when comparing to GOAL.
///
/// If you only care about e.g. truthiness, change this to
///
///     (n != 0) as Num
///
/// and use only 0/1 in the GOAL.
pub fn mapping(n: Num) -> Num {
    n
}

pub fn match_goal(expr: &Expr) -> bool {
    expr.output.clone().map(mapping) == Vector::from_slice(GOAL)
}

pub const GOAL: &[Num] = &[1, -1, 0, 0];

pub const MAX_LENGTH: usize = 14;
pub const MAX_CACHE_LENGTH: usize = 10;
pub const MIN_MULTITHREAD_LENGTH: usize = MAX_CACHE_LENGTH + 1;
pub const LITERALS: &[Num] = &[
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
];
/// If not 0, include all numbers in 1..=MAX_LITERAL in addition to LITERALS.
pub const MAX_LITERAL: Num = 0;

#[rustfmt::skip]
pub const BINARY_OPERATORS: &[BinaryOperator] = &[
    Or,
    SpaceOr,
    OrSpace,
    // SpaceOrSpace,
    Lt,
    Le,
    // Gt,
    // Ge,
    // Eq,
    // Ne,
    BitOr,
    BitXor,
    BitAnd,
    BitShl,
    BitShr,
    Add,
    Sub,
    Mul,
    Mod,
    // Div1,
    Div2,
    // Gcd,
    Exp,
];

#[rustfmt::skip]
pub const UNARY_OPERATORS: &[UnaryOperator] = &[
    BitNeg,
    Neg
];

/// Use C-style modulo and division (-2 % 10 == -2) rather than Python style (-2 % 10 == 8).
pub const C_STYLE_MOD: bool = false;

/// Use C-style bit shift (1 >> 32 == 1) rather than Python style (1 >> 32 == 0).
pub const C_STYLE_BIT_SHIFT: bool = false;

/// Search expressions that use the same variable twice (like `x*x`).
pub const REUSE_VARS: bool = true;

/// Controls whether all declared variables should be always used.
pub const USE_ALL_VARS: bool = true;
