use std::fmt::Display;

use crate::{
    expr::{ok_after_keyword, ok_before_keyword, Expr},
    params::{Num, BINARY_OPERATORS, UNARY_OPERATORS},
    vec::Vector,
};

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
    pub const fn prec(&self) -> u8 {
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
    pub prec: u8,
    pub apply: fn(Num) -> Num,
    pub can_apply: fn(&Expr) -> bool,
}

#[derive(Clone, Copy)]
pub struct BinaryOp {
    pub name: &'static str,
    pub prec: u8,
    pub apply: fn(Num, Num) -> Option<Num>,
    pub can_apply: fn(&Expr, &Expr) -> bool,
    pub commutative: bool,
    pub right_assoc: bool,
}

impl UnaryOp {
    pub const PREC: u8 = 12;

    #[inline(always)]
    pub fn vec_apply(&self, v: Vector) -> Vector {
        v.map(self.apply)
    }

    #[inline(always)]
    pub fn can_apply(&self, e: &Expr) -> bool {
        e.prec() >= self.prec && (self.can_apply)(&e)
    }
}

impl BinaryOp {
    const OP_EMPTY: BinaryOp = BinaryOp {
        name: "",
        prec: 0,
        apply: apply_add,
        can_apply: can_apply_binary_always,
        commutative: false,
        right_assoc: false,
    };

    #[inline(always)]
    pub fn vec_apply(&self, mut vl: Vector, vr: &Vector) -> Option<Vector> {
        for (x, y) in vl.iter_mut().zip(vr.iter()) {
            *x = (self.apply)(*x, *y)?;
        }
        Some(vl)
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
pub fn apply_sub(l: Num, r: Num) -> Option<Num> {
    Some(l - r)
}
pub fn apply_mul(l: Num, r: Num) -> Option<Num> {
    Some(l * r)
}
pub fn apply_mod_floor(l: Num, r: Num) -> Option<Num> {
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

#[inline(always)]
pub fn can_apply_unary_always(_: &Expr) -> bool {
    true
}
#[inline(always)]
pub fn can_apply_binary_always(_: &Expr, _: &Expr) -> bool {
    true
}
#[inline(always)]
pub fn can_apply_or(el: &Expr, er: &Expr) -> bool {
    ok_before_keyword(el) && ok_after_keyword(er)
}
#[inline(always)]
pub fn can_apply_space_or(el: &Expr, er: &Expr) -> bool {
    !ok_before_keyword(el) && ok_after_keyword(er)
}
#[inline(always)]
pub fn can_apply_or_space(el: &Expr, er: &Expr) -> bool {
    ok_before_keyword(el) && !ok_after_keyword(er)
}
#[inline(always)]
pub fn can_apply_space_or_space(el: &Expr, er: &Expr) -> bool {
    !ok_before_keyword(el) && !ok_after_keyword(er)
}

pub const OP_OR: BinaryOp = BinaryOp {
    name: "or",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_or,
    ..BinaryOp::OP_EMPTY
};
pub const OP_SPACE_OR: BinaryOp = BinaryOp {
    name: " or",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_space_or,
    ..BinaryOp::OP_EMPTY
};
pub const OP_OR_SPACE: BinaryOp = BinaryOp {
    name: "or ",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_or_space,
    ..BinaryOp::OP_EMPTY
};
pub const OP_SPACE_OR_SPACE: BinaryOp = BinaryOp {
    name: " or ",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_space_or_space,
    ..BinaryOp::OP_EMPTY
};
pub const OP_LT: BinaryOp = BinaryOp {
    name: "<",
    prec: 5,
    apply: apply_lt,
    ..BinaryOp::OP_EMPTY
};
pub const OP_LE: BinaryOp = BinaryOp {
    name: "<=",
    prec: 5,
    apply: apply_le,
    ..BinaryOp::OP_EMPTY
};
pub const OP_GT: BinaryOp = BinaryOp {
    name: ">",
    prec: 5,
    apply: apply_gt,
    ..BinaryOp::OP_EMPTY
};
pub const OP_GE: BinaryOp = BinaryOp {
    name: ">=",
    prec: 5,
    apply: apply_ge,
    ..BinaryOp::OP_EMPTY
};
pub const OP_EQ: BinaryOp = BinaryOp {
    name: "==",
    prec: 5,
    apply: apply_eq,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_NE: BinaryOp = BinaryOp {
    name: "!=",
    prec: 5,
    apply: apply_ne,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_BIT_OR: BinaryOp = BinaryOp {
    name: "|",
    prec: 6,
    apply: apply_bit_or,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_BIT_XOR: BinaryOp = BinaryOp {
    name: "^",
    prec: 7,
    apply: apply_bit_xor,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_BIT_AND: BinaryOp = BinaryOp {
    name: "&",
    prec: 8,
    apply: apply_bit_and,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_BIT_SHL: BinaryOp = BinaryOp {
    name: "<<",
    prec: 9,
    apply: apply_bit_shl,
    commutative: false,
    ..BinaryOp::OP_EMPTY
};
pub const OP_BIT_SHL_WRAP: BinaryOp = BinaryOp {
    name: "<<",
    prec: 9,
    apply: apply_bit_shl_wrap,
    ..BinaryOp::OP_EMPTY
};
pub const OP_BIT_SHR: BinaryOp = BinaryOp {
    name: ">>",
    prec: 9,
    apply: apply_bit_shr,
    ..BinaryOp::OP_EMPTY
};
pub const OP_BIT_SHR_WRAP: BinaryOp = BinaryOp {
    name: ">>",
    prec: 9,
    apply: apply_bit_shr_wrap,
    ..BinaryOp::OP_EMPTY
};
pub const OP_ADD: BinaryOp = BinaryOp {
    name: "+",
    prec: 10,
    apply: apply_add,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_SUB: BinaryOp = BinaryOp {
    name: "-",
    prec: 10,
    apply: apply_sub,
    ..BinaryOp::OP_EMPTY
};
pub const OP_MUL: BinaryOp = BinaryOp {
    name: "*",
    prec: 11,
    apply: apply_mul,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_MOD_FLOOR: BinaryOp = BinaryOp {
    name: "%",
    prec: 11,
    apply: apply_mod_floor,
    ..BinaryOp::OP_EMPTY
};
pub const OP_MOD_TRUNC: BinaryOp = BinaryOp {
    name: "%",
    prec: 11,
    apply: apply_mod_trunc,
    ..BinaryOp::OP_EMPTY
};
pub const OP_DIV_FLOOR: BinaryOp = BinaryOp {
    name: "//",
    prec: 11,
    apply: apply_div_floor,
    ..BinaryOp::OP_EMPTY
};
pub const OP_DIV_TRUNC: BinaryOp = BinaryOp {
    name: "/",
    prec: 11,
    apply: apply_div_trunc,
    ..BinaryOp::OP_EMPTY
};
pub const OP_GCD: BinaryOp = BinaryOp {
    name: "V",
    prec: 11,
    apply: apply_gcd,
    commutative: true,
    ..BinaryOp::OP_EMPTY
};
pub const OP_EXP: BinaryOp = BinaryOp {
    name: "**",
    prec: 13,
    apply: apply_exp,
    right_assoc: true,
    ..BinaryOp::OP_EMPTY
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

// All operators: [..Unary, ..Binary, Parens, Literal, Variable]
pub const NUM_OPERATORS: usize = UNARY_OPERATORS.len() + BINARY_OPERATORS.len() + 3;
pub const OP_INDEX_PARENS: OpIndex = OpIndex::new(0xFD);
pub const OP_INDEX_LITERAL: OpIndex = OpIndex::new(0xFE);
pub const OP_INDEX_VARIABLE: OpIndex = OpIndex::new(0xFF);

const fn gen_binary_index_table() -> [OpIndex; BINARY_OPERATORS.len()] {
    let mut table = [OpIndex::new(0); BINARY_OPERATORS.len()];
    let mut cnt = [0; 16];
    let mut i: usize = 0;
    while i < BINARY_OPERATORS.len() {
        let prec = BINARY_OPERATORS[i].prec;
        table[i] = OpIndex(prec << 4 | cnt[prec as usize]);
        cnt[prec as usize] += 1;
        i += 1;
    }
    table
}

const fn gen_unary_index_table() -> [OpIndex; UNARY_OPERATORS.len()] {
    let mut table = [OpIndex::new(0); UNARY_OPERATORS.len()];
    let mut cnt = [0; 16];
    let mut i: usize = 0;
    while i < UNARY_OPERATORS.len() {
        let prec = UNARY_OPERATORS[i].prec;
        table[i] = OpIndex(prec << 4 | cnt[prec as usize]);
        cnt[prec as usize] += 1;
        i += 1;
    }
    table
}

const fn gen_op_name_table() -> [&'static str; 256] {
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
}

pub const OP_BINARY_INDEX_TABLE: [OpIndex; BINARY_OPERATORS.len()] = gen_binary_index_table();
pub const OP_UNARY_INDEX_TABLE: [OpIndex; UNARY_OPERATORS.len()] = gen_unary_index_table();
pub const OP_NAME_TABLE: [&'static str; 256] = gen_op_name_table();
