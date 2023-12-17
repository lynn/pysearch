use crate::{expr::Expr, vec::Vector};

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
pub const LITERALS: &[Num] = &[
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
];
/// If not 0, include all numbers in 1..=MAX_LITERAL in addition to LITERALS.
pub const MAX_LITERAL: Num = 0;

pub const USE_OR: bool = true;
pub const USE_LT: bool = true;
pub const USE_LE: bool = true;
pub const USE_BIT_OR: bool = true;
pub const USE_BIT_XOR: bool = true;
pub const USE_BIT_AND: bool = true;
pub const USE_BIT_SHL: bool = true;
pub const USE_BIT_SHR: bool = true;
pub const USE_BIT_NEG: bool = true;
pub const USE_ADD: bool = true;
pub const USE_SUB: bool = true;
pub const USE_MUL: bool = true;
pub const USE_MOD: bool = true;
pub const USE_DIV1: bool = false; /* / */
pub const USE_DIV2: bool = true; /* // */
pub const USE_GCD: bool = false;
pub const USE_NEG: bool = true;
pub const USE_EXP: bool = true;

/// Use C-style modulo and division (-2 % 10 == -2) rather than Python style (-2 % 10 == 8).
pub const C_STYLE_MOD: bool = false;

/// Use C-style bit shift (1 >> 32 == 1) rather than Python style (1 >> 32 == 0).
pub const C_STYLE_BIT_SHIFT: bool = false;

/// Search expressions that use the same variable twice (like `x*x`).
pub const REUSE_VARS: bool = true;

/// Controls whether all declared variables should be always used.
pub const USE_ALL_VARS: bool = true;
