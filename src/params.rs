pub type Num = i32;
pub type Nums = [Num; 8];

pub struct Input {
    pub name: &'static str,
    pub vec: Nums,
}

pub const INPUTS: [Input; 2] = [
    Input {
        name: "n",
        vec: [100, 100, 100, 100, 53, 53, 53, 53],
    },
    Input {
        name: "x",
        vec: [100, 98, 2, 99, 53, 1, 20, 4],
    },
];

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

pub const GOAL: Nums = [98, 96, 99, 97, 51, 52, 18, 2];

pub const MAX_LENGTH: usize = 14;
pub const LITERALS: [Num; 12] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];

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

pub const C_STYLE_MOD: bool = false;
pub const REUSE_VARS: bool = true;
