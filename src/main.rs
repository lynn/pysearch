#![feature(portable_simd)]
pub mod expr;
pub mod gcd;
pub mod operator;
pub mod params;

#[cfg_attr(feature = "simd", path = "vec_simd.rs")]
#[cfg_attr(not(feature = "simd"), path = "vec.rs")]
pub mod vec;

use expr::{ok_after_keyword, ok_before_keyword, Expr, Literal, Mask};
use operator::Operator;
use params::*;

use vec::{divmod, vec_gcd, vec_in, vec_le, vec_lt, vec_or, vec_pow, Vector};

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ptr::NonNull;
use std::time::Instant;

// cache[length][output] = highest-prec expression of that length yielding that output
type CacheLevel = HashMap<Vector, Expr>;
type Cache = Vec<Vec<(Vector, Expr)>>;

fn positive_integer_length(mut k: Num) -> usize {
    let mut l = 1;
    while k >= 10 {
        k /= 10;
        l += 1;
    }
    l
}

fn save(level: &mut CacheLevel, output: Vector, expr: Expr) {
    let all_mask: Mask = (1 << INPUTS.len()) - 1;
    if !REUSE_VARS && expr.var_mask == all_mask {
        let mut mp: HashMap<Num, Num> = HashMap::new();
        for i in 0..GOAL.len() {
            if let Some(old) = mp.insert(output[i], GOAL[i]) {
                if old != GOAL[i] {
                    return;
                }
            }
        }
    }

    if output.clone().map(mapping) == Vector::from_slice(GOAL) {
        println!("{expr}");
    }

    match level.entry(output) {
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

fn find_expressions(cache: &mut Cache, n: usize) {
    let mut cn = CacheLevel::new();
    if n == 1 {
        for (i, input) in INPUTS.iter().enumerate() {
            let vec: Vector = Vector::from_slice(input.vec);
            cn.insert(vec, Expr::variable(i as Literal));
        }
    }
    for &lit in LITERALS {
        if positive_integer_length(lit) == n {
            let vec: Vector = Vector::constant(lit);
            cn.insert(vec, Expr::literal(lit as Literal));
        }
    }

    for k in 1..n {
        for (or, er) in cache[k].iter() {
            // 1-byte operators
            if n >= k + 2 {
                for (ol, el) in cache[n - k - 1].iter() {
                    if er.is_literal() && el.is_literal() {
                        continue;
                    }
                    let elp: NonNull<Expr> = el.into();
                    let erp: NonNull<Expr> = er.into();
                    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
                        continue;
                    }
                    let mask = el.var_mask | er.var_mask;
                    if USE_LT && el.prec() >= 5 && er.prec() > 5 {
                        save(
                            &mut cn,
                            vec_lt(ol, or),
                            Expr::bin(elp, erp, Operator::Lt, mask),
                        );
                    }
                    if USE_BIT_OR && el.prec() >= 6 && er.prec() > 6 {
                        save(
                            &mut cn,
                            ol.clone() | or,
                            Expr::bin(elp, erp, Operator::BitOr, mask),
                        );
                    }
                    if USE_BIT_XOR && el.prec() >= 7 && er.prec() > 7 {
                        save(
                            &mut cn,
                            ol.clone() ^ or,
                            Expr::bin(elp, erp, Operator::BitXor, mask),
                        );
                    }
                    if USE_BIT_AND && el.prec() >= 8 && er.prec() > 8 {
                        save(
                            &mut cn,
                            ol.clone() & or,
                            Expr::bin(elp, erp, Operator::BitAnd, mask),
                        );
                    }
                    if el.prec() >= 10 && er.prec() > 10 {
                        if USE_ADD {
                            save(
                                &mut cn,
                                ol.clone() + or,
                                Expr::bin(elp, erp, Operator::Add, mask),
                            );
                        }
                        if USE_SUB {
                            save(
                                &mut cn,
                                ol.clone() - or,
                                Expr::bin(elp, erp, Operator::Sub, mask),
                            );
                        }
                    }
                    if el.prec() >= 11 && er.prec() > 11 {
                        if USE_MUL {
                            save(
                                &mut cn,
                                ol.clone() * or,
                                Expr::bin(elp, erp, Operator::Mul, mask),
                            );
                        }
                        if let Some((div, modulo)) = divmod(ol, or) {
                            if USE_MOD {
                                save(&mut cn, modulo, Expr::bin(elp, erp, Operator::Mod, mask));
                            }
                            if USE_DIV1 {
                                save(&mut cn, div, Expr::bin(elp, erp, Operator::Div1, mask));
                            }
                        }
                        if USE_GCD {
                            save(
                                &mut cn,
                                vec_gcd(ol, or),
                                Expr::bin(elp, erp, Operator::Gcd, mask),
                            );
                        }
                    }
                }
            }
            // 2-byte operators
            if n >= k + 3 {
                for (ol, el) in cache[n - k - 2].iter() {
                    if er.is_literal() && el.is_literal() {
                        continue;
                    }
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
                        save(
                            &mut cn,
                            vec_or(ol, or),
                            Expr::bin(elp, erp, Operator::Or, mask),
                        );
                    }
                    if USE_LE && el.prec() >= 5 && er.prec() > 5 {
                        save(
                            &mut cn,
                            vec_le(ol, or),
                            Expr::bin(elp, erp, Operator::Le, mask),
                        );
                    }
                    if el.prec() > 9 && er.prec() >= 9 && vec_in(or, 0..=31) {
                        if USE_BIT_SHL {
                            save(
                                &mut cn,
                                ol.clone() << or,
                                Expr::bin(elp, erp, Operator::BitShl, mask),
                            );
                        }
                        if USE_BIT_SHR {
                            save(
                                &mut cn,
                                ol.clone() >> or,
                                Expr::bin(elp, erp, Operator::BitShr, mask),
                            );
                        }
                    }
                    if el.prec() >= 11 && er.prec() > 11 {
                        if let Some((div, _)) = divmod(ol, or) {
                            if USE_DIV2 {
                                save(&mut cn, div, Expr::bin(elp, erp, Operator::Div2, mask));
                            }
                        }
                    }
                    if USE_EXP && el.prec() > 13 && er.prec() >= 13 && vec_in(or, 0..=6) {
                        save(
                            &mut cn,
                            vec_pow(ol, or),
                            Expr::bin(elp, erp, Operator::Exp, mask),
                        );
                    }
                }
            }
            // 3-byte operators
            if n >= k + 4 {
                for (ol, el) in cache[n - k - 3].iter() {
                    if er.is_literal() && el.is_literal() {
                        continue;
                    }
                    let elp: NonNull<Expr> = el.into();
                    let erp: NonNull<Expr> = er.into();
                    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
                        continue;
                    }
                    let mask = el.var_mask | er.var_mask;
                    if el.prec() >= 3 && er.prec() > 3 {
                        let z = vec_or(ol, or);
                        if USE_OR && !ok_before_keyword(el) && ok_after_keyword(er) {
                            save(
                                &mut cn,
                                z.clone(),
                                Expr::bin(elp, erp, Operator::SpaceOr, mask),
                            );
                        }
                        if USE_OR && ok_before_keyword(el) && !ok_after_keyword(er) {
                            save(&mut cn, z, Expr::bin(elp, erp, Operator::OrSpace, mask));
                        }
                    }
                }
            }
        }
    }
    if n >= 3 {
        for (or, er) in cache[n - 2].iter() {
            if er.op < Operator::Parens {
                let erp: NonNull<Expr> = er.into();
                cn.insert(or.clone(), Expr::parens(erp));
            }
        }
    }
    if n >= 2 {
        for (or, er) in cache[n - 1].iter() {
            let erp: NonNull<Expr> = er.into();
            if er.prec() >= 12 {
                if USE_BIT_NEG {
                    save(&mut cn, !or.clone(), Expr::unary(erp, Operator::BitNeg));
                }
                if USE_NEG {
                    save(&mut cn, -or.clone(), Expr::unary(erp, Operator::Neg));
                }
            }
        }
    }
    cache.push(cn.into_iter().collect());
}

fn main() {
    for i in INPUTS {
        assert_eq!(
            i.vec.len(),
            GOAL.len(),
            "INPUTS and GOAL must have equal length"
        );
    }
    let mut cache: Cache = vec![vec![]];
    let mut total_count = 0;
    println!("sizeof(Expr) = {}", std::mem::size_of::<Expr>());
    let start = Instant::now();
    for n in 1..=MAX_LENGTH {
        println!("Finding length {n}...");
        let layer_start = Instant::now();
        find_expressions(&mut cache, n);
        let count = cache[n].len();
        total_count += count;
        let time = layer_start.elapsed();
        println!("Explored {count} expressions in {time:?}");
        let total_time = start.elapsed();
        println!("Total: {total_count} expressions in {total_time:?}\n");
    }
    println!();
}
