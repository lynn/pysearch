pub mod expr;
pub mod gcd;
pub mod operator;
pub mod params;
pub mod vec;

use expr::{ok_after_keyword, ok_before_keyword, Expr, Mask};
use operator::Operator;
use params::*;
use vec::{divmod, vec_gcd, vec_in, vec_le, vec_lt, vec_or, vec_pow, Vec};

use ndarray::Array;

use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ptr::NonNull;
use std::time::Instant;

// cache[length][output] = highest-prec expression of that length yielding that output
type CacheLevel = HashMap<Vec, Expr>;
type Cache = HashMap<usize, RefCell<CacheLevel>>;

fn positive_integer_length(mut k: Num) -> usize {
    let mut l = 1;
    while k >= 10 {
        k /= 10;
        l += 1;
    }
    l
}

fn save(level: &RefCell<CacheLevel>, output: Vec, expr: Expr) {
    let all_mask: Mask = (1 << INPUTS.len()) - 1;
    if !REUSE_VARS && expr.var_mask == all_mask {
        let mut mp: HashMap<Num, Num> = HashMap::new();
        for (i, o) in output.iter().enumerate() {
            if let Some(old) = mp.insert(*o, GOAL[i]) {
                if old != GOAL[i] {
                    return;
                }
            }
        }
    }

    match level.borrow_mut().entry(output) {
        Entry::Occupied(mut e) => {
            if expr.prec() > e.get().prec() {
                e.insert(expr);
            }
        }
        Entry::Vacant(e) => {
            e.insert(expr);
        }
    }
}

fn find_expressions(cache: &Cache, n: usize) {
    let cn = &cache[&n];
    if n == 1 {
        for (i, input) in INPUTS.iter().enumerate() {
            let vec: Vec = Array::from_iter(input.vec);
            cn.borrow_mut().insert(vec, Expr::variable(i));
        }
    }
    for lit in LITERALS {
        if positive_integer_length(lit) == n {
            let vec: Vec = Array::from_elem(GOAL.len(), lit);
            cn.borrow_mut().insert(vec, Expr::literal(lit));
        }
    }

    for k in 1..n {
        for (or, er) in cache[&k].borrow().iter() {
            // 1-byte operators
            if n >= k + 2 {
                for (ol, el) in cache[&(n - k - 1)].borrow().iter() {
                    let elp: NonNull<Expr> = el.into();
                    let erp: NonNull<Expr> = er.into();
                    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
                        continue;
                    }
                    let mask = el.var_mask | er.var_mask;
                    if USE_LT && el.prec() >= 5 && er.prec() > 5 {
                        save(cn, vec_lt(ol, or), Expr::bin(elp, erp, Operator::Lt, mask));
                    }
                    if USE_BIT_OR && el.prec() >= 6 && er.prec() > 6 {
                        save(cn, ol | or, Expr::bin(elp, erp, Operator::BitOr, mask));
                    }
                    if USE_BIT_XOR && el.prec() >= 7 && er.prec() > 7 {
                        save(cn, ol ^ or, Expr::bin(elp, erp, Operator::BitXor, mask));
                    }
                    if USE_BIT_AND && el.prec() >= 8 && er.prec() > 8 {
                        save(cn, ol & or, Expr::bin(elp, erp, Operator::BitAnd, mask));
                    }
                    if el.prec() >= 10 && er.prec() > 10 {
                        if USE_ADD {
                            save(cn, ol + or, Expr::bin(elp, erp, Operator::Add, mask));
                        }
                        if USE_SUB {
                            save(cn, ol - or, Expr::bin(elp, erp, Operator::Sub, mask));
                        }
                    }
                    if el.prec() >= 11 && er.prec() > 11 {
                        if USE_MUL {
                            save(cn, ol * or, Expr::bin(elp, erp, Operator::Mul, mask));
                        }
                        if let Some((div, modulo)) = divmod(ol, or) {
                            if USE_MOD {
                                save(cn, modulo, Expr::bin(elp, erp, Operator::Mod, mask));
                            }
                            if USE_DIV1 {
                                save(cn, div, Expr::bin(elp, erp, Operator::Div1, mask));
                            }
                        }
                        if USE_GCD {
                            save(
                                cn,
                                vec_gcd(ol, or),
                                Expr::bin(elp, erp, Operator::Gcd, mask),
                            );
                        }
                    }
                }
            }
            // 2-byte operators
            if n >= k + 3 {
                for (ol, el) in cache[&(n - k - 2)].borrow().iter() {
                    let elp: NonNull<Expr> = el.into();
                    let erp: NonNull<Expr> = er.into();
                    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
                        continue;
                    }
                    let mask = el.var_mask | er.var_mask;
                    if USE_OR
                        && el.prec() >= 3
                        && er.prec() > 3
                        && ok_before_keyword(el)
                        && ok_after_keyword(er)
                    {
                        save(cn, vec_or(ol, or), Expr::bin(elp, erp, Operator::Or, mask));
                    }
                    if USE_LE && el.prec() >= 5 && er.prec() > 5 {
                        save(cn, vec_le(ol, or), Expr::bin(elp, erp, Operator::Le, mask));
                    }
                    if el.prec() > 9 && er.prec() >= 9 && vec_in(or, 0..=31) {
                        if USE_BIT_SHL {
                            save(cn, ol << or, Expr::bin(elp, erp, Operator::BitShl, mask));
                        }
                        if USE_BIT_SHR {
                            save(cn, ol >> or, Expr::bin(elp, erp, Operator::BitShr, mask));
                        }
                    }
                    if el.prec() >= 11 && er.prec() > 11 {
                        if let Some((div, _)) = divmod(ol, or) {
                            if USE_DIV2 {
                                save(cn, div, Expr::bin(elp, erp, Operator::Div2, mask));
                            }
                        }
                    }
                    if USE_EXP && el.prec() > 13 && er.prec() >= 13 && vec_in(or, 0..=6) {
                        save(
                            cn,
                            vec_pow(ol, or),
                            Expr::bin(elp, erp, Operator::Exp, mask),
                        );
                    }
                }
            }
            // 3-byte operators
            if n >= k + 4 {
                for (ol, el) in cache[&(n - k - 3)].borrow().iter() {
                    let elp: NonNull<Expr> = el.into();
                    let erp: NonNull<Expr> = er.into();
                    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
                        continue;
                    }
                    let mask = el.var_mask | er.var_mask;
                    if el.prec() >= 3 && er.prec() > 3 {
                        let z = vec_or(ol, or);
                        if USE_OR && !ok_before_keyword(el) && ok_after_keyword(er) {
                            save(cn, z.clone(), Expr::bin(elp, erp, Operator::SpaceOr, mask));
                        }
                        if USE_OR && ok_before_keyword(el) && !ok_after_keyword(er) {
                            save(cn, z, Expr::bin(elp, erp, Operator::OrSpace, mask));
                        }
                    }
                }
            }
        }
    }
    if n >= 3 {
        for (or, er) in cache[&(n - 2)].borrow().iter() {
            if er.op < Operator::Parens {
                let erp: NonNull<Expr> = er.into();
                cn.borrow_mut().insert(or.clone(), Expr::parens(erp));
            }
        }
    }
    if n >= 2 {
        for (or, er) in cache[&(n - 1)].borrow().iter() {
            let erp: NonNull<Expr> = er.into();
            if er.prec() >= 12 {
                if USE_BIT_NEG {
                    save(cn, !or, Expr::unary(erp, Operator::BitNeg));
                }
                if USE_NEG {
                    save(cn, -or, Expr::unary(erp, Operator::Neg));
                }
            }
        }
    }
}

fn main() {
    let mut cache: Cache = HashMap::new();
    println!("sizeof(Expr) = {}", std::mem::size_of::<Expr>());
    let mut no_results: bool = true;
    let start = Instant::now();
    for n in 1..=MAX_LENGTH {
        cache.insert(n, Default::default());
        println!("Finding length {n}...");
        find_expressions(&cache, n);
        let count = cache[&n].borrow().len();
        let time = start.elapsed();
        println!("Found {count} expressions in {time:?}.");
        let mut first: bool = true;
        for (out, expr) in cache[&n].borrow().iter() {
            if GOAL.iter().enumerate().all(|(i, x)| mapping(*x) == out[i]) {
                if first {
                    println!("\n--- Length {n} ---");
                    first = false;
                    no_results = false;
                }
                println!("{}", expr);
            }
        }
    }
    if no_results {
        println!("\nNo results found.");
    }
    println!();
}
