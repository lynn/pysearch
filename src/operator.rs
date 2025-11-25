use std::fmt::Display;

use crate::{
    expr::{ok_after_keyword, ok_before_keyword, Expr},
    params::{Num, BINARY_OPERATORS, ERROR_VALUE, UNARY_OPERATORS},
    vec::Vector,
};

pub type Prec = u8;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
pub struct OpIndex(u8);

impl OpIndex {
    #[inline(always)]
    pub const fn new(idx: usize) -> Self {
        Self(idx as u8)
    }

    #[inline(always)]
    pub const fn as_index(&self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    pub const fn prec(&self) -> Prec {
        self.0 >> 4
    }

    #[inline(always)]
    pub const fn name(&self) -> &'static str {
        OP_NAME_TABLE[self.as_index()]
    }
}

impl Display for OpIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Clone, Copy)]
pub struct UnaryOp {
    pub name: &'static str,
    pub prec: Prec,
    pub apply: fn(Num) -> Num,
    pub can_apply: fn(&Expr) -> bool,
}

#[derive(Clone, Copy)]
pub struct BinaryOp {
    pub name: &'static str,
    pub prec: Prec,
    pub apply: fn(Num, Num) -> Option<Num>,
    pub apply_inverse: Option<fn(Num, Num) -> Option<Num>>,
    pub can_apply: fn(&Expr, &Expr) -> bool,
    pub commutative: bool,
    pub right_assoc: bool,
    pub short_circuit: bool,
}

impl UnaryOp {
    pub const PREC: Prec = 12;

    #[inline(always)]
    pub fn apply_(&self, x: Num) -> Num {
        if Some(x) == ERROR_VALUE {
            x
        } else {
            (self.apply)(x)
        }
    }

    #[inline(always)]
    pub fn vec_apply(&self, v: Vector) -> Vector {
        v.map(|x| self.apply_(x))
    }

    #[inline(always)]
    pub fn can_apply(&self, e: &Expr) -> bool {
        e.prec() >= self.prec && (self.can_apply)(&e)
    }
}

impl BinaryOp {
    const EMPTY: BinaryOp = BinaryOp {
        name: "",
        prec: 0,
        apply: apply_add,
        apply_inverse: None,
        can_apply: can_apply_binary_always,
        commutative: false,
        right_assoc: false,
        short_circuit: false,
    };

    #[inline(always)]
    pub fn apply_(&self, l: Num, r: Num) -> Option<Num> {
        if Some(l) == ERROR_VALUE || !self.short_circuit && Some(r) == ERROR_VALUE {
            ERROR_VALUE
        } else {
            (self.apply)(l, r).or(ERROR_VALUE)
        }
    }

    #[inline(always)]
    pub fn vec_apply(&self, mut vl: Vector, vr: &Vector) -> Option<Vector> {
        for (l, &r) in vl.iter_mut().zip(vr.iter()) {
            *l = self.apply_(*l, r)?;
        }
        Some(vl)
    }

    #[inline(always)]
    pub fn vec_apply_inverse(&self, mut vr: Vector, goal: &Vector) -> Option<Vector> {
        let apply_inverse = self.apply_inverse?;
        for (r, &goal) in vr.iter_mut().zip(goal.iter()) {
            *r = apply_inverse(*r, goal)?;
        }
        Some(vr)
    }

    #[inline(always)]
    pub fn can_apply(&self, el: &Expr, er: &Expr) -> bool {
        if !(self.can_apply)(el, er) {
            return false;
        }
        let prec = self.prec;
        if self.commutative {
            if self.right_assoc {
                el.prec() > prec
                    && (er.prec() > prec && el as *const Expr <= er as *const Expr
                        || er.prec() == prec)
            } else {
                er.prec() > prec
                    && (el.prec() > prec && el as *const Expr <= er as *const Expr
                        || el.prec() == prec)
            }
        } else {
            if self.right_assoc {
                el.prec() > prec && er.prec() >= prec
            } else {
                el.prec() >= prec && er.prec() > prec
            }
        }
    }
}

pub fn apply_or(l: Num, r: Num) -> Option<Num> {
    Some(if l != 0 { l } else { r })
}
pub fn apply_or_logical(l: Num, r: Num) -> Option<Num> {
    if l == 0 && Some(r) == ERROR_VALUE {
        ERROR_VALUE
    } else {
        Some((l != 0 || r != 0) as Num)
    }
}
pub fn apply_and(l: Num, r: Num) -> Option<Num> {
    Some(if l != 0 { r } else { l })
}
pub fn apply_and_logical(l: Num, r: Num) -> Option<Num> {
    if l != 0 && Some(r) == ERROR_VALUE {
        ERROR_VALUE
    } else {
        Some((l != 0 && r != 0) as Num)
    }
}
pub fn apply_lt(l: Num, r: Num) -> Option<Num> {
    Some((l < r) as Num)
}
pub fn apply_le(l: Num, r: Num) -> Option<Num> {
    Some((l <= r) as Num)
}
pub fn apply_gt(l: Num, r: Num) -> Option<Num> {
    Some((l > r) as Num)
}
pub fn apply_ge(l: Num, r: Num) -> Option<Num> {
    Some((l >= r) as Num)
}
pub fn apply_eq(l: Num, r: Num) -> Option<Num> {
    Some((l == r) as Num)
}
pub fn apply_ne(l: Num, r: Num) -> Option<Num> {
    Some((l != r) as Num)
}
pub fn apply_bit_or(l: Num, r: Num) -> Option<Num> {
    Some(l | r)
}
pub fn apply_bit_xor(l: Num, r: Num) -> Option<Num> {
    Some(l ^ r)
}
pub fn apply_bit_and(l: Num, r: Num) -> Option<Num> {
    Some(l & r)
}
pub fn apply_bit_shl(l: Num, r: Num) -> Option<Num> {
    #[allow(unused_comparisons)]
    if r >= 0 && r < Num::BITS as Num {
        Some(l << r)
    } else {
        None
    }
}
pub fn apply_bit_shl_wrap(l: Num, r: Num) -> Option<Num> {
    Some(l << r)
}
pub fn apply_bit_shr(l: Num, r: Num) -> Option<Num> {
    #[allow(unused_comparisons)]
    if r >= 0 {
        Some(l >> r.min(Num::BITS as Num - 1))
    } else {
        None
    }
}
pub fn apply_bit_shr_wrap(l: Num, r: Num) -> Option<Num> {
    Some(l >> r)
}
pub fn apply_add(l: Num, r: Num) -> Option<Num> {
    Some(l + r)
}
pub fn apply_inverse_add(r: Num, goal: Num) -> Option<Num> {
    Some(goal - r)
}
pub fn apply_sub(l: Num, r: Num) -> Option<Num> {
    Some(l - r)
}
pub fn apply_mul(l: Num, r: Num) -> Option<Num> {
    Some(l * r)
}
pub fn apply_inverse_mul(r: Num, goal: Num) -> Option<Num> {
    if r != 0 && goal % r == 0 {
        Some(goal / r)
    } else {
        None
    }
}
pub fn apply_mod_floor(l: Num, r: Num) -> Option<Num> {
    #[allow(unused_comparisons)]
    if r == 0 || (Num::MIN < 0 && l == Num::MIN && r == !0) {
        None
    } else {
        Some(num_integer::mod_floor(l, r))
    }
}
pub fn apply_mod_trunc(l: Num, r: Num) -> Option<Num> {
    l.checked_rem(r)
}
pub fn apply_div_floor(l: Num, r: Num) -> Option<Num> {
    #[allow(unused_comparisons)]
    if r == 0 || (Num::MIN < 0 && l == Num::MIN && r == !0) {
        None
    } else {
        Some(num_integer::div_floor(l, r))
    }
}
pub fn apply_div_trunc(l: Num, r: Num) -> Option<Num> {
    l.checked_div(r)
}
pub fn apply_gcd(l: Num, r: Num) -> Option<Num> {
    Some(num_integer::gcd(l, r))
}
pub fn apply_exp(l: Num, r: Num) -> Option<Num> {
    l.checked_pow(r.try_into().ok()?)
}
pub fn apply_bit_neg(x: Num) -> Num {
    !x
}
pub fn apply_neg(x: Num) -> Num {
    0 - x
}
pub fn apply_not(x: Num) -> Num {
    (x == 0) as Num
}

#[inline(always)]
pub fn can_apply_unary_always(_: &Expr) -> bool {
    true
}
#[inline(always)]
pub fn can_apply_binary_always(_: &Expr, _: &Expr) -> bool {
    true
}
#[inline(always)]
pub fn can_apply_keyword(el: &Expr, er: &Expr) -> bool {
    ok_before_keyword(el) && ok_after_keyword(er)
}
#[inline(always)]
pub fn can_apply_space_keyword(el: &Expr, er: &Expr) -> bool {
    !ok_before_keyword(el) && ok_after_keyword(er)
}
#[inline(always)]
pub fn can_apply_keyword_space(el: &Expr, er: &Expr) -> bool {
    ok_before_keyword(el) && !ok_after_keyword(er)
}
#[inline(always)]
pub fn can_apply_space_keyword_space(el: &Expr, er: &Expr) -> bool {
    !ok_before_keyword(el) && !ok_after_keyword(er)
}

pub const OP_OR: BinaryOp = BinaryOp {
    name: "or",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_keyword,
    short_circuit: true,
    ..BinaryOp::EMPTY
};
pub const OP_SPACE_OR: BinaryOp = BinaryOp {
    name: " or",
    can_apply: can_apply_space_keyword,
    ..OP_OR
};
pub const OP_OR_SPACE: BinaryOp = BinaryOp {
    name: "or ",
    can_apply: can_apply_keyword_space,
    ..OP_OR
};
pub const OP_SPACE_OR_SPACE: BinaryOp = BinaryOp {
    name: " or ",
    can_apply: can_apply_space_keyword_space,
    ..OP_OR
};
pub const OP_OR_SYMBOL: BinaryOp = BinaryOp {
    name: "||",
    can_apply: can_apply_binary_always,
    ..OP_OR
};
pub const OP_OR_LOGICAL: BinaryOp = BinaryOp {
    name: "||",
    prec: 3,
    apply: apply_or_logical,
    commutative: true,
    short_circuit: true,
    ..BinaryOp::EMPTY
};
pub const OP_AND: BinaryOp = BinaryOp {
    name: "and",
    prec: 4,
    apply: apply_and,
    can_apply: can_apply_keyword,
    short_circuit: true,
    ..BinaryOp::EMPTY
};
pub const OP_SPACE_AND: BinaryOp = BinaryOp {
    name: " and",
    can_apply: can_apply_space_keyword,
    ..OP_AND
};
pub const OP_AND_SPACE: BinaryOp = BinaryOp {
    name: "and ",
    can_apply: can_apply_keyword_space,
    ..OP_AND
};
pub const OP_SPACE_AND_SPACE: BinaryOp = BinaryOp {
    name: " and ",
    can_apply: can_apply_space_keyword_space,
    ..OP_AND
};
pub const OP_AND_SYMBOL: BinaryOp = BinaryOp {
    name: "&&",
    can_apply: can_apply_binary_always,
    ..OP_AND
};
pub const OP_AND_LOGICAL: BinaryOp = BinaryOp {
    name: "&&",
    prec: 4,
    apply: apply_and_logical,
    commutative: true,
    short_circuit: true,
    ..BinaryOp::EMPTY
};
pub const OP_LT: BinaryOp = BinaryOp {
    name: "<",
    prec: 5,
    apply: apply_lt,
    ..BinaryOp::EMPTY
};
pub const OP_LE: BinaryOp = BinaryOp {
    name: "<=",
    prec: 5,
    apply: apply_le,
    ..BinaryOp::EMPTY
};
pub const OP_GT: BinaryOp = BinaryOp {
    name: ">",
    prec: 5,
    apply: apply_gt,
    ..BinaryOp::EMPTY
};
pub const OP_GE: BinaryOp = BinaryOp {
    name: ">=",
    prec: 5,
    apply: apply_ge,
    ..BinaryOp::EMPTY
};
pub const OP_EQ: BinaryOp = BinaryOp {
    name: "==",
    prec: 5,
    apply: apply_eq,
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_NE: BinaryOp = BinaryOp {
    name: "!=",
    prec: 5,
    apply: apply_ne,
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_BIT_OR: BinaryOp = BinaryOp {
    name: "|",
    prec: 6,
    apply: apply_bit_or,
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_BIT_XOR: BinaryOp = BinaryOp {
    name: "^",
    prec: 7,
    apply: apply_bit_xor,
    apply_inverse: Some(apply_bit_xor),
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_BIT_AND: BinaryOp = BinaryOp {
    name: "&",
    prec: 8,
    apply: apply_bit_and,
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_BIT_SHL: BinaryOp = BinaryOp {
    name: "<<",
    prec: 9,
    apply: apply_bit_shl,
    ..BinaryOp::EMPTY
};
pub const OP_BIT_SHL_WRAP: BinaryOp = BinaryOp {
    apply: apply_bit_shl_wrap,
    ..OP_BIT_SHL
};
pub const OP_BIT_SHR: BinaryOp = BinaryOp {
    name: ">>",
    prec: 9,
    apply: apply_bit_shr,
    ..BinaryOp::EMPTY
};
pub const OP_BIT_SHR_WRAP: BinaryOp = BinaryOp {
    apply: apply_bit_shr_wrap,
    ..OP_BIT_SHR
};
pub const OP_ADD: BinaryOp = BinaryOp {
    name: "+",
    prec: 10,
    apply: apply_add,
    apply_inverse: Some(apply_inverse_add),
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_SUB: BinaryOp = BinaryOp {
    name: "-",
    prec: 10,
    apply: apply_sub,
    apply_inverse: Some(apply_add),
    ..BinaryOp::EMPTY
};
pub const OP_MUL: BinaryOp = BinaryOp {
    name: "*",
    prec: 11,
    apply: apply_mul,
    apply_inverse: Some(apply_inverse_mul),
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_MOD_FLOOR: BinaryOp = BinaryOp {
    name: "%",
    prec: 11,
    apply: apply_mod_floor,
    ..BinaryOp::EMPTY
};
pub const OP_MOD_TRUNC: BinaryOp = BinaryOp {
    name: "%",
    prec: 11,
    apply: apply_mod_trunc,
    ..BinaryOp::EMPTY
};
pub const OP_DIV_FLOOR: BinaryOp = BinaryOp {
    name: "//",
    prec: 11,
    apply: apply_div_floor,
    ..BinaryOp::EMPTY
};
pub const OP_DIV_TRUNC: BinaryOp = BinaryOp {
    name: "/",
    prec: 11,
    apply: apply_div_trunc,
    ..BinaryOp::EMPTY
};
pub const OP_GCD: BinaryOp = BinaryOp {
    name: "V",
    prec: 11,
    apply: apply_gcd,
    commutative: true,
    ..BinaryOp::EMPTY
};
pub const OP_EXP: BinaryOp = BinaryOp {
    name: "**",
    prec: 13,
    apply: apply_exp,
    right_assoc: true,
    ..BinaryOp::EMPTY
};
pub const OP_BIT_NEG: UnaryOp = UnaryOp {
    name: "~",
    prec: 12,
    apply: apply_bit_neg,
    can_apply: can_apply_unary_always,
};
pub const OP_NEG: UnaryOp = UnaryOp {
    name: "-",
    prec: 12,
    apply: apply_neg,
    can_apply: can_apply_unary_always,
};
pub const OP_NOT: UnaryOp = UnaryOp {
    name: "!",
    prec: 12,
    apply: apply_not,
    can_apply: can_apply_unary_always,
};

// All operators: [..Binary, ..Unary, Parens, Literal, Variable]
pub const NUM_OPERATORS: usize = UNARY_OPERATORS.len() + BINARY_OPERATORS.len() + 3;
pub const OP_INDEX_PARENS: OpIndex = OpIndex::new(0xFD);
pub const OP_INDEX_LITERAL: OpIndex = OpIndex::new(0xFE);
pub const OP_INDEX_VARIABLE: OpIndex = OpIndex::new(0xFF);

const fn gen_index_tables() -> (
    [OpIndex; BINARY_OPERATORS.len()],
    [OpIndex; UNARY_OPERATORS.len()],
) {
    let mut binary_table = [OpIndex::new(0); BINARY_OPERATORS.len()];
    let mut unary_table = [OpIndex::new(0); UNARY_OPERATORS.len()];
    let mut cnt = [0; 16];

    let mut i: usize = 0;
    while i < BINARY_OPERATORS.len() {
        let prec = BINARY_OPERATORS[i].prec;
        assert!(prec < 16 && cnt[prec as usize] < 16);
        binary_table[i] = OpIndex(prec << 4 | cnt[prec as usize]);
        cnt[prec as usize] += 1;
        i += 1;
    }

    let mut i: usize = 0;
    while i < UNARY_OPERATORS.len() {
        let prec = UNARY_OPERATORS[i].prec;
        assert!(prec < 16 && cnt[prec as usize] < 16);
        unary_table[i] = OpIndex(prec << 4 | cnt[prec as usize]);
        cnt[prec as usize] += 1;
        i += 1;
    }

    (binary_table, unary_table)
}

pub const OP_BINARY_INDEX_TABLE: [OpIndex; BINARY_OPERATORS.len()] = gen_index_tables().0;
pub const OP_UNARY_INDEX_TABLE: [OpIndex; UNARY_OPERATORS.len()] = gen_index_tables().1;
pub const OP_NAME_TABLE: [&'static str; 256] = {
    let mut table = [""; 256];
    let mut i: usize = 0;
    while i < OP_BINARY_INDEX_TABLE.len() {
        table[OP_BINARY_INDEX_TABLE[i].as_index()] = BINARY_OPERATORS[i].name;
        i += 1;
    }
    let mut i: usize = 0;
    while i < OP_UNARY_INDEX_TABLE.len() {
        table[OP_UNARY_INDEX_TABLE[i].as_index()] = UNARY_OPERATORS[i].name;
        i += 1;
    }
    table[OP_INDEX_PARENS.as_index()] = "(";
    table
};
pub const MIN_BINARY_OP_LEN: usize = {
    let mut min_len = usize::MAX;
    let mut i = 0;
    while i < OP_BINARY_INDEX_TABLE.len() {
        if BINARY_OPERATORS[i].name.len() < min_len {
            min_len = BINARY_OPERATORS[i].name.len();
        }
        i += 1;
    }
    min_len
};
