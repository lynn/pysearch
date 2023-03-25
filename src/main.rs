pub mod gcd;

use crate::gcd::gcd;

use ndarray::prelude::*;
use ndarray::{Array, Ix1};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
use std::num::Wrapping;
use std::sync::{Mutex, RwLock};

type Num = i32;
type Vec = Array<Num, Ix1>;

struct Input {
    name: &'static str,
    vec: [Num; 5],
}

const INPUTS: [Input; 1] = [Input {
    name: "x",
    vec: [0, 3, 5, 7, 9],
}];
const GOAL: [Num; 5] = [1, 1, 3, 2, 1];

const MAX_LENGTH: usize = 14;
const LITERALS: [Num; 12] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];

const USE_OR: bool = false;
const USE_LT: bool = true;
const USE_LEQ: bool = true;
const USE_BIT_OR: bool = true;
const USE_BIT_XOR: bool = true;
const USE_BIT_AND: bool = true;
const USE_BIT_SHL: bool = true;
const USE_BIT_SHR: bool = true;
const USE_BIT_NEG: bool = true;
const USE_ADD: bool = true;
const USE_SUB: bool = true;
const USE_MUL: bool = true;
const USE_MOD: bool = true;
const USE_DIV1: bool = false; /* / */
const USE_DIV2: bool = true; /* // */
const USE_GCD: bool = false;
const USE_NEG: bool = true;
const USE_EXP: bool = true;

const C_STYLE_MOD: bool = false;
const REUSE_VARS: bool = true;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Operator {
    Or = 0x300,
    SpaceOr = 0x301,
    OrSpace = 0x302,
    SpaceOrSpace = 0x303,
    Lt = 0x500,
    Leq = 0x501,
    Gt = 0x502,
    Geq = 0x503,
    Eq = 0x504,
    Neq = 0x505,
    BitOr = 0x600,
    BitXor = 0x700,
    BitAnd = 0x800,
    BitShl = 0x900,
    BitShr = 0x901,
    Add = 0xA00,
    Sub = 0xA01,
    Mul = 0xB00,
    Mod = 0xB01,
    Div1 = 0xB02,
    Div2 = 0xB03,
    Gcd = 0xB04,
    Neg = 0xC00,
    BitNeg = 0xC01,
    Exp = 0xD00,
    Parens = 0xFE00,
    Literal = 0xFF00,
}

#[derive(Clone, Copy, Debug)]
struct Expr {
    left: Option<*const Expr>,
    right: Option<*const Expr>,
    op: Operator,
    literal: Num,
    var_mask: usize,
}

impl Expr {
    fn prec(&self) -> usize {
        self.op as usize >> 8
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Or => write!(f, "or"),
            Operator::SpaceOr => write!(f, " or"),
            Operator::OrSpace => write!(f, "or "),
            Operator::SpaceOrSpace => write!(f, " or "),
            Operator::Lt => write!(f, "<"),
            Operator::Leq => write!(f, "<="),
            Operator::Gt => write!(f, ">"),
            Operator::Geq => write!(f, ">="),
            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
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
            Operator::Literal => write!(f, ""),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(left) = self.left {
            Self::fmt(unsafe { &*left }, f)?;
        }
        Display::fmt(&self.op, f)?;
        if let Some(right) = self.right {
            Self::fmt(unsafe { &*right }, f)?;
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

// cache[length][output] = highest-prec expression of that length yielding that output
type CacheLevel = HashMap<Vec, Expr>;
type Cache = HashMap<usize, RwLock<CacheLevel>>;

// "3or" and ")or" are valid, but "nor" isn't.
unsafe fn ok_before_keyword(e: &Expr) -> bool {
    match e.right {
        None => e.literal >= 0,
        Some(right) => e.op == Operator::Parens || ok_before_keyword(&*right),
    }
}

// "or3", "orn" are invalid. Need a unary op or parens.
unsafe fn ok_after_keyword(e: &Expr) -> bool {
    match e.left {
        None => e.op != Operator::Literal,
        Some(left) => ok_after_keyword(&*left),
    }
}

fn positive_integer_length(mut k: Num) -> usize {
    let mut l = 1;
    while k >= 10 {
        k /= 10;
        l += 1;
    }
    l
}

fn cache_if_better(level: &RwLock<CacheLevel>, output: Vec, expr: Expr) {
    let all_mask = (1 << INPUTS.len()) - 1;
    if !REUSE_VARS && expr.var_mask == all_mask {
        let mut mp: HashMap<Num, Num> = HashMap::new();
        for i in 0..output.len() {
            if let Some(old) = mp.insert(output[i], GOAL[i]) {
                if old != GOAL[i] {
                    return;
                }
            }
        }
    }

    let old_prec = level.read().unwrap().get(&output).map(|x| x.prec());
    match old_prec {
        None => {
            level.try_write().unwrap().insert(output, expr);
        }
        Some(old_prec) => {
            if expr.prec() > old_prec {
                level.try_write().unwrap().insert(output, expr);
            }
        }
    }
}

unsafe fn find_expressions(cache: &Cache, n: usize) {
    let cn = &cache[&n];
    if n == 1 {
        for i in 0..INPUTS.len() {
            let vec: Vec = Array::from_iter(INPUTS[i].vec);
            cn.try_write().unwrap().insert(
                vec,
                Expr {
                    left: None,
                    right: None,
                    op: Operator::Literal,
                    literal: !(i as Num),
                    var_mask: 1 << i,
                },
            );
        }
    }
    for l in LITERALS {
        if positive_integer_length(l) == n {
            let vec: Vec = Array::from_elem(GOAL.len(), l);
            cn.try_write().unwrap().insert(
                vec,
                Expr {
                    left: None,
                    right: None,
                    op: Operator::Literal,
                    literal: l,
                    var_mask: 0,
                },
            );
        }
    }

    let dim = GOAL.len();

    for nR in 1..n {
        for (oR, eR) in &*cache[&nR].read().unwrap() {
            // 1-byte operators
            if n >= nR + 2 {
                for (oL, eL) in &*cache[&(n - nR - 1)].read().unwrap() {
                    if !REUSE_VARS && (eL.var_mask & eR.var_mask != 0) {
                        continue;
                    }
                    let mask = eL.var_mask | eR.var_mask;
                    if USE_LT && eL.prec() >= 5 && eR.prec() > 5 {
                        let z = Array::from_shape_fn(GOAL.len(), |i| (oL[i] < oR[i]) as Num);
                        cache_if_better(
                            cn,
                            z,
                            Expr {
                                left: Some(&*eL),
                                right: Some(&*eR),
                                op: Operator::Lt,
                                literal: 0,
                                var_mask: mask,
                            },
                        );
                    }
                    if USE_BIT_OR && eL.prec() >= 6 && eR.prec() > 6 {
                        cache_if_better(
                            cn,
                            oL | oR,
                            Expr {
                                left: Some(&*eL),
                                right: Some(&*eR),
                                op: Operator::BitOr,
                                literal: 0,
                                var_mask: mask,
                            },
                        );
                    }
                    if USE_BIT_XOR && eL.prec() >= 7 && eR.prec() > 7 {
                        cache_if_better(
                            cn,
                            oL ^ oR,
                            Expr {
                                left: Some(&*eL),
                                right: Some(&*eR),
                                op: Operator::BitXor,
                                literal: 0,
                                var_mask: mask,
                            },
                        );
                    }
                    if USE_BIT_AND && eL.prec() >= 8 && eR.prec() > 8 {
                        cache_if_better(
                            cn,
                            oL & oR,
                            Expr {
                                left: Some(&*eL),
                                right: Some(&*eR),
                                op: Operator::BitAnd,
                                literal: 0,
                                var_mask: mask,
                            },
                        );
                    }
                    if eL.prec() >= 10 && eR.prec() > 10 {
                        if USE_ADD {
                            cache_if_better(
                                cn,
                                oL + oR,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::Add,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                        if USE_SUB {
                            cache_if_better(
                                cn,
                                oL - oR,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::Sub,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                    }
                    if eL.prec() >= 11 && eR.prec() > 11 {
                        if USE_MUL {
                            cache_if_better(
                                cn,
                                oL * oR,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::Mul,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                        if (0..dim).all(|i| oR[i] != 0 && (oL[i] != Num::MIN || oR[i] != -1)) {
                            if C_STYLE_MOD {
                                if (USE_MOD) {
                                    cache_if_better(
                                        cn,
                                        oL % oR,
                                        Expr {
                                            left: Some(&*eL),
                                            right: Some(&*eR),
                                            op: Operator::Mod,
                                            literal: 0,
                                            var_mask: mask,
                                        },
                                    );
                                }
                                if (USE_DIV1) {
                                    cache_if_better(
                                        cn,
                                        oL / oR,
                                        Expr {
                                            left: Some(&*eL),
                                            right: Some(&*eR),
                                            op: Operator::Div1,
                                            literal: 0,
                                            var_mask: mask,
                                        },
                                    );
                                }
                            } else {
                                let modulo = ((oL % oR) + oR) % oR;
                                if (USE_MOD) {
                                    cache_if_better(
                                        cn,
                                        modulo.clone(),
                                        Expr {
                                            left: Some(&*eL),
                                            right: Some(&*eR),
                                            op: Operator::Mod,
                                            literal: 0,
                                            var_mask: mask,
                                        },
                                    );
                                }
                                if (USE_DIV1) {
                                    cache_if_better(
                                        cn,
                                        (oL - modulo) / oR,
                                        Expr {
                                            left: Some(&*eL),
                                            right: Some(&*eR),
                                            op: Operator::Div1,
                                            literal: 0,
                                            var_mask: mask,
                                        },
                                    );
                                }
                            }
                        }
                        if (USE_GCD) {
                            let z = Array::from_shape_fn(GOAL.len(), |i| gcd(oL[i], oR[i]));
                            cache_if_better(
                                cn,
                                z,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::Gcd,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                    }
                }
            }
            // 2-byte operators
            if n >= nR + 3 {
                for (oL, eL) in &*cache[&(n - nR - 2)].read().unwrap() {
                    if !REUSE_VARS && (eL.var_mask & eR.var_mask != 0) {
                        continue;
                    }
                    let mask = eL.var_mask | eR.var_mask;
                    if eL.prec() >= 3 && eR.prec() > 3 {
                        if USE_OR && ok_before_keyword(&*eL) && ok_after_keyword(&*eR) {
                            let z = Array::from_shape_fn(GOAL.len(), |i| {
                                if oL[i] == 0 {
                                    oR[i]
                                } else {
                                    oL[i]
                                }
                            });
                            cache_if_better(
                                cn,
                                z,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::Or,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                    }
                    if (USE_LEQ && eL.prec() >= 5 && eR.prec() > 5) {
                        let z =
                            Array::from_shape_fn(
                                GOAL.len(),
                                |i| if oL[i] <= oR[i] { 1 } else { 0 },
                            );
                        cache_if_better(
                            cn,
                            z,
                            Expr {
                                left: Some(&*eL),
                                right: Some(&*eR),
                                op: Operator::Leq,
                                literal: 0,
                                var_mask: mask,
                            },
                        );
                    }
                    if eL.prec() > 9
                        && eR.prec() >= 9
                        && (0..dim).all(|i| 0 <= oR[i] && oR[i] <= 31)
                    {
                        if (USE_BIT_SHL) {
                            cache_if_better(
                                cn,
                                oL << oR,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::BitShl,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                        if (USE_BIT_SHR) {
                            cache_if_better(
                                cn,
                                oL >> oR,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::BitShr,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                    }
                    if (eL.prec() >= 11 && eR.prec() > 11) {
                        if (0..dim).all(|i| oR[i] != 0 && (oL[i] != Num::MIN || oR[i] != -1)) {
                            if (C_STYLE_MOD) {
                                if (USE_DIV2) {
                                    cache_if_better(
                                        cn,
                                        oL / oR,
                                        Expr {
                                            left: Some(&*eL),
                                            right: Some(&*eR),
                                            op: Operator::Div2,
                                            literal: 0,
                                            var_mask: mask,
                                        },
                                    );
                                }
                            } else {
                                let modulo = ((oL % oR) + oR) % oR;
                                if (USE_DIV2) {
                                    cache_if_better(
                                        cn,
                                        (oL - modulo) / oR,
                                        Expr {
                                            left: Some(&*eL),
                                            right: Some(&*eR),
                                            op: Operator::Div2,
                                            literal: 0,
                                            var_mask: mask,
                                        },
                                    );
                                }
                            }
                        }
                    }
                    if eL.prec() > 13
                        && eR.prec() >= 13
                        && (0..dim).all(|i| 0 <= oR[i] && oR[i] <= 6)
                    {
                        if (USE_EXP) {
                            let z = Array::from_shape_fn(GOAL.len(), |i| oL[i].pow(oR[i] as u32));
                            cache_if_better(
                                cn,
                                z,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::Exp,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                    }
                }
            }
            // 3-byte operators
            if n >= nR + 4 {
                for (oL, eL) in &*cache[&(n - nR - 3)].read().unwrap() {
                    if (!REUSE_VARS && (eL.var_mask & eR.var_mask != 0)) {
                        continue;
                    }
                    let mask = eL.var_mask | eR.var_mask;
                    if (eL.prec() >= 3 && eR.prec() > 3) {
                        let z = Array::from_shape_fn(GOAL.len(), |i| {
                            if oL[i] == 0 {
                                oR[i]
                            } else {
                                oL[i]
                            }
                        });
                        if (USE_OR && !ok_before_keyword(&*eL) && ok_after_keyword(&*eR)) {
                            cache_if_better(
                                cn,
                                z.clone(),
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::SpaceOr,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                        if (USE_OR && ok_before_keyword(&*eL) && !ok_after_keyword(&*eR)) {
                            cache_if_better(
                                cn,
                                z,
                                Expr {
                                    left: Some(&*eL),
                                    right: Some(&*eR),
                                    op: Operator::OrSpace,
                                    literal: 0,
                                    var_mask: mask,
                                },
                            );
                        }
                    }
                }
            }
        }
    }
    if n >= 3 {
        for (oR, eR) in &*cache[&(n - 2)].read().unwrap() {
            if (eR.op >= Operator::Parens) {
                continue;
            }
            cn.write().unwrap().insert(
                oR.clone(),
                Expr {
                    left: None,
                    right: Some(&*eR),
                    op: Operator::Parens,
                    literal: 0,
                    var_mask: eR.var_mask,
                },
            );
        }
    }
    if n >= 2 {
        for (oR, eR) in &*cache[&(n - 1)].read().unwrap() {
            if (eR.prec() >= 12) {
                if (USE_BIT_NEG) {
                    cache_if_better(
                        cn,
                        !oR,
                        Expr {
                            left: None,
                            right: Some(&*eR),
                            op: Operator::BitNeg,
                            literal: 0,
                            var_mask: eR.var_mask,
                        },
                    );
                }
                if (USE_NEG) {
                    cache_if_better(
                        cn,
                        -oR,
                        Expr {
                            left: None,
                            right: Some(&*eR),
                            op: Operator::Neg,
                            literal: 0,
                            var_mask: eR.var_mask,
                        },
                    );
                }
            }
        }
    }
}

fn main() {
    let mut cache: Cache = HashMap::new();
    println!("sizeof(Expr) = {}", std::mem::size_of::<Expr>());
    let mut no_results: bool = true;
    for n in 1..=MAX_LENGTH {
        cache.insert(n, Default::default());
        println!("Finding length {n}...");
        unsafe {
            find_expressions(&cache, n);
        }
        println!("Found {} expressions.", cache[&n].read().unwrap().len());
        let mut first: bool = true;
        for (oR, eR) in &*cache[&n].read().unwrap() {
            if GOAL.iter().enumerate().all(|(i, x)| *x == oR[i]) {
                if first {
                    println!("\n--- Length {n} ---");
                    first = false;
                    no_results = false;
                }
                println!("{}", eR);
            }
        }
    }
    if no_results {
        println!("\nNo results found.");
    }
    println!();
}
