use std::fmt::Display;

use crate::{
    expr::{ok_after_keyword, ok_before_keyword, Expr},
    params::{Num, BINARY_OPERATORS, C_STYLE_BIT_SHIFT, C_STYLE_MOD, UNARY_OPERATORS},
    vec::Vector,
};

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
pub enum OpKind {
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
    Div,
    Gcd,
    Neg,
    BitNeg,
    Exp,
    Parens,
    Literal,
    Variable,
}

impl OpKind {
    pub fn length(&self) -> usize {
        OP_NAME_TABLE[*self as usize].len()
    }

    pub fn prec(&self) -> u8 {
        OP_PREC_TABLE[*self as usize]
    }
}

impl Display for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", OP_NAME_TABLE[*self as usize])
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum OpFlag {
    Default = 0x00,
    Commutative = 0x01,
    RightAssoc = 0x02,
}

#[derive(Clone, Copy)]
pub struct UnaryOp {
    pub kind: OpKind,
    pub name: &'static str,
    pub prec: u8,
    pub apply: fn(Num) -> Num,
    pub can_apply: fn(&Expr) -> bool,
}

#[derive(Clone, Copy)]
pub struct BinaryOp {
    pub kind: OpKind,
    pub name: &'static str,
    pub prec: u8,
    pub apply: fn(Num, Num) -> Option<Num>,
    pub can_apply: fn(&Expr, &Expr) -> bool,
    pub flags: OpFlag,
}

impl UnaryOp {
    #[inline]
    pub fn vec_apply(&self, v: Vector) -> Vector {
        v.map(|x| (self.apply)(x))
    }

    #[inline]
    pub fn can_apply(&self, e: &Expr) -> bool {
        e.prec() >= self.prec && (self.can_apply)(&e)
    }
}

impl BinaryOp {
    #[inline]
    pub fn vec_apply(&self, mut left: Vector, right: &Vector) -> Option<Vector> {
        for (x, y) in left.iter_mut().zip(right.iter()) {
            *x = (self.apply)(*x, *y)?;
        }
        Some(left)
    }

    #[inline]
    pub fn can_apply(&self, el: &Expr, er: &Expr) -> bool {
        if !(self.can_apply)(el, er) {
            return false;
        }
        let prec = self.prec;
        if self.flags as u8 & OpFlag::Commutative as u8 != 0 {
            if self.flags as u8 & OpFlag::RightAssoc as u8 != 0 {
                el.prec() > prec
                    && (er.prec() > prec && el as *const Expr <= er as *const Expr
                        || er.prec() == prec)
            } else {
                er.prec() > prec
                    && (el.prec() > prec && el as *const Expr <= er as *const Expr
                        || el.prec() == prec)
            }
        } else {
            if self.flags as u8 & OpFlag::RightAssoc as u8 != 0 {
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
pub fn apply_python_bit_shl(l: Num, r: Num) -> Option<Num> {
    if r >= 0 && r < Num::BITS as Num {
        Some(l << r)
    } else {
        None
    }
}
pub fn apply_python_bit_shr(l: Num, r: Num) -> Option<Num> {
    if r >= 0 {
        Some(l >> r.min(Num::BITS as Num - 1))
    } else {
        None
    }
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
pub fn apply_floor_mod(l: Num, r: Num) -> Option<Num> {
    if r == 0 || (Num::MIN < 0 && l == Num::MIN && r == !0) {
        None
    } else {
        Some(num_integer::mod_floor(l, r))
    }
}
pub fn apply_floor_div(l: Num, r: Num) -> Option<Num> {
    if r == 0 || (Num::MIN < 0 && l == Num::MIN && r == !0) {
        None
    } else {
        Some(num_integer::div_floor(l, r))
    }
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
    -x
}

pub fn can_apply_unary_always(_: &Expr) -> bool {
    true
}
pub fn can_apply_binary_always(_: &Expr, _: &Expr) -> bool {
    true
}

pub fn can_apply_or(el: &Expr, er: &Expr) -> bool {
    ok_before_keyword(el) && ok_after_keyword(er)
}
pub fn can_apply_space_or(el: &Expr, er: &Expr) -> bool {
    !ok_before_keyword(el) && ok_after_keyword(er)
}
pub fn can_apply_or_space(el: &Expr, er: &Expr) -> bool {
    ok_before_keyword(el) && !ok_after_keyword(er)
}
pub fn can_apply_space_or_space(el: &Expr, er: &Expr) -> bool {
    !ok_before_keyword(el) && !ok_after_keyword(er)
}

// Binary operators
pub const OP_OR: BinaryOp = BinaryOp {
    kind: OpKind::Or,
    name: "or",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_or,
    flags: OpFlag::Default,
};
pub const OP_SPACE_OR: BinaryOp = BinaryOp {
    kind: OpKind::SpaceOr,
    name: " or",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_space_or,
    flags: OpFlag::Default,
};
pub const OP_OR_SPACE: BinaryOp = BinaryOp {
    kind: OpKind::OrSpace,
    name: "or ",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_or_space,
    flags: OpFlag::Default,
};
pub const OP_SPACE_OR_SPACE: BinaryOp = BinaryOp {
    kind: OpKind::SpaceOrSpace,
    name: " or ",
    prec: 3,
    apply: apply_or,
    can_apply: can_apply_space_or_space,
    flags: OpFlag::Default,
};
pub const OP_LT: BinaryOp = BinaryOp {
    kind: OpKind::Lt,
    name: "<",
    prec: 5,
    apply: apply_lt,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_LE: BinaryOp = BinaryOp {
    kind: OpKind::Le,
    name: "<=",
    prec: 5,
    apply: apply_le,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_GT: BinaryOp = BinaryOp {
    kind: OpKind::Lt,
    name: ">",
    prec: 5,
    apply: apply_gt,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_GE: BinaryOp = BinaryOp {
    kind: OpKind::Le,
    name: ">=",
    prec: 5,
    apply: apply_ge,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_EQ: BinaryOp = BinaryOp {
    kind: OpKind::Eq,
    name: "==",
    prec: 5,
    apply: apply_eq,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_NE: BinaryOp = BinaryOp {
    kind: OpKind::Ne,
    name: "!=",
    prec: 5,
    apply: apply_ne,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_BIT_OR: BinaryOp = BinaryOp {
    kind: OpKind::BitOr,
    name: "|",
    prec: 6,
    apply: apply_bit_or,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Commutative,
};
pub const OP_BIT_XOR: BinaryOp = BinaryOp {
    kind: OpKind::BitXor,
    name: "^",
    prec: 7,
    apply: apply_bit_xor,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Commutative,
};
pub const OP_BIT_AND: BinaryOp = BinaryOp {
    kind: OpKind::BitAnd,
    name: "&",
    prec: 8,
    apply: apply_bit_and,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Commutative,
};
pub const OP_BIT_SHL: BinaryOp = BinaryOp {
    kind: OpKind::BitShl,
    name: "<<",
    prec: 9,
    apply: apply_python_bit_shl,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_BIT_SHR: BinaryOp = BinaryOp {
    kind: OpKind::BitShr,
    name: ">>",
    prec: 9,
    apply: apply_python_bit_shr,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_ADD: BinaryOp = BinaryOp {
    kind: OpKind::Add,
    name: "+",
    prec: 10,
    apply: apply_add,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Commutative,
};
pub const OP_SUB: BinaryOp = BinaryOp {
    kind: OpKind::Sub,
    name: "-",
    prec: 10,
    apply: apply_sub,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_MUL: BinaryOp = BinaryOp {
    kind: OpKind::Mul,
    name: "*",
    prec: 11,
    apply: apply_mul,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Commutative,
};
pub const OP_MOD: BinaryOp = BinaryOp {
    kind: OpKind::Mod,
    name: "%",
    prec: 11,
    apply: apply_floor_mod,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_DIV: BinaryOp = BinaryOp {
    kind: OpKind::Div,
    name: "//",
    prec: 11,
    apply: apply_floor_div,
    can_apply: can_apply_binary_always,
    flags: OpFlag::Default,
};
pub const OP_EXP: BinaryOp = BinaryOp {
    kind: OpKind::Exp,
    name: "**",
    prec: 13,
    apply: apply_exp,
    can_apply: can_apply_binary_always,
    flags: OpFlag::RightAssoc,
};

// Unary operators
pub const OP_BIT_NEG: UnaryOp = UnaryOp {
    kind: OpKind::BitNeg,
    name: "~",
    prec: 12,
    apply: apply_bit_neg,
    can_apply: can_apply_unary_always,
};
pub const OP_NEG: UnaryOp = UnaryOp {
    kind: OpKind::Neg,
    name: "-",
    prec: 12,
    apply: apply_neg,
    can_apply: can_apply_unary_always,
};

const fn gen_op_name_table() -> [&'static str; 64] {
    let mut table = [""; 64];
    let mut idx: usize = 0;
    while idx < UNARY_OPERATORS.len() {
        let op = UNARY_OPERATORS[idx];
        table[op.kind as usize] = op.name;
        idx += 1;
    }
    let mut idx: usize = 0;
    while idx < BINARY_OPERATORS.len() {
        let op = BINARY_OPERATORS[idx];
        table[op.kind as usize] = op.name;
        idx += 1;
    }
    table[OpKind::Parens as usize] = "(";
    table
}

const fn gen_op_prec_table() -> [u8; 64] {
    let mut table = [0; 64];
    let mut idx: usize = 0;
    while idx < UNARY_OPERATORS.len() {
        let op = UNARY_OPERATORS[idx];
        table[op.kind as usize] = op.prec;
        idx += 1;
    }
    let mut idx: usize = 0;
    while idx < BINARY_OPERATORS.len() {
        let op = BINARY_OPERATORS[idx];
        table[op.kind as usize] = op.prec;
        idx += 1;
    }
    table[OpKind::Parens as usize] = 14;
    table[OpKind::Literal as usize] = 15;
    table[OpKind::Variable as usize] = 15;
    table
}

pub const OP_NAME_TABLE: [&'static str; 64] = gen_op_name_table();
pub const OP_PREC_TABLE: [u8; 64] = gen_op_prec_table();
