use crate::{matcher::Match, operator::*};

pub type Num = i32;

pub struct Input {
    pub name: &'static str,
    pub vec: &'static [Num],
    pub min_uses: u8,
    pub max_uses: u8,
}

// Test input: map bytes of "pysea" to 1,2,3,4,5. Should find 1^7&n*n>>4 at length 10
pub const INPUTS: &[Input] = &[Input {
    name: "n",
    vec: &[
        b'p' as i32,
        b'y' as i32,
        b's' as i32,
        b'e' as i32,
        b'a' as i32,
    ],
    min_uses: 1,
    max_uses: 255,
}];

pub struct Matcher {}

impl Match for Matcher {
    fn new() -> Self {
        Self {}
    }

    // Override the default match_one, other methods in the Match trait can be
    // overridden as well.
    fn match_one(&mut self, index: usize, output: Num) -> bool {
        output == GOAL[index]
    }
}

pub const GOAL: &[Num] = &[1, 2, 3, 4, 5];

pub const MAX_LENGTH: usize = 14;
pub const MAX_CACHE_LENGTH: usize = 9;
pub const MIN_MULTITHREAD_LENGTH: usize = MAX_CACHE_LENGTH + 1;
pub const LITERALS: &[Num] = &[];
/// If not 0, include all numbers in 1..=MAX_LITERAL in addition to LITERALS.
pub const MAX_LITERAL: Num = 40;

#[rustfmt::skip]
pub const BINARY_OPERATORS: &[BinaryOp] = &[
    OP_OR,
    OP_SPACE_OR,
    OP_OR_SPACE,
    // OP_SPACE_OR_SPACE,
    // OP_OR_SYMBOL,
    // OP_OR_LOGICAL,
    // OP_AND,
    // OP_SPACE_AND,
    // OP_AND_SPACE,
    // OP_SPACE_AND_SPACE,
    // OP_AND_SYMBOL,
    // OP_AND_LOGICAL,
    OP_LT,
    OP_LE,
    // OP_GT,
    // OP_GE,
    // OP_EQ,
    // OP_NE,
    OP_BIT_OR,
    OP_BIT_XOR,
    OP_BIT_AND,
    OP_BIT_SHL,
    OP_BIT_SHR,
    // OP_BIT_SHL_WRAP,
    // OP_BIT_SHR_WRAP,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_MOD_FLOOR,
    OP_DIV_FLOOR,
    // OP_MOD_TRUNC,
    // OP_DIV_TRUNC,
    // OP_GCD,
    OP_EXP,
];

#[rustfmt::skip]
pub const UNARY_OPERATORS: &[UnaryOp] = &[
    OP_BIT_NEG,
    OP_NEG,
    // OP_NOT,
];

/// If set, e.g. to `Some(-159236)`, this arbitrary number is chosen to represent errors.
/// That is, pysearch will pretend 1/0 = -159236, and -159236 * 2 = -159236, and so on.
pub const ERROR_VALUE: Option<Num> = None;

/// Apply invertible operators on GOAL and cached expressions to find
/// expressions up to 2*MAX_CACHE_LENGTH+1 length.
/// Does not support custom Matcher.
pub const ENABLE_INVERSE_SEARCH: bool = false;

/// Maximum number of expressions allowed in a group.
/// Only used when `Matcher::GROUP_BY_FIRST_OUTPUT` is enabled.
pub const MAX_GROUP_SIZE: usize = 256;
