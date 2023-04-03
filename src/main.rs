#![cfg_attr(feature = "simd", feature(portable_simd))]

pub mod expr;
pub mod gcd;
pub mod operator;

#[path = "params.rs"]
pub mod params;

#[cfg_attr(all(feature = "simd", feature = "portable_simd"), path = "vec_simd.rs")]
#[cfg_attr(not(feature = "simd"), path = "vec.rs")]
pub mod vec;

use expr::{ok_after_keyword, ok_before_keyword, Expr, Literal, Mask};
use operator::Operator;
use params::*;

use vec::{divmod, vec_gcd, vec_in, vec_le, vec_lt, vec_or, vec_pow, Vector};

use hashbrown::{hash_map::Entry, HashMap};
use std::ptr::NonNull;
use std::time::Instant;

// cache[length][output] = highest-prec expression of that length yielding that output
type CacheLevel = HashMap<Vector, Expr>;
type Cache = Vec<CacheLevel>;

fn positive_integer_length(mut k: Num) -> usize {
    let mut l = 1;
    while k >= 10 {
        k /= 10;
        l += 1;
    }
    l
}

#[cfg(feature = "rayon")]
fn unit_if(b: bool) -> Option<()> {
    if b {
        Some(())
    } else {
        None
    }
}

fn save(level: &mut CacheLevel, output: Vector, expr: Expr, n: usize, cache: &Cache) {
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

    if match_goal(&output) {
        println!("{expr}");
        return;
    }

    if n == MAX_LENGTH || n == MAX_LENGTH - 1 && expr.prec() < 12 {
        return;
    }

    for l in cache {
        if let Some(e) = l.get(&output) {
            if e.prec() >= expr.prec() {
                return;
            }
        }
    }

    insert_to_level(level, output, expr);
}

fn insert_to_level(level: &mut CacheLevel, output: Vector, expr: Expr) {
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

fn find_binary_expressions(
    cn: &mut CacheLevel,
    cache: &Cache,
    n: usize,
    k: usize,
    (or, er): (&Vector, &Expr),
) {
    // 1-byte operators
    for (ol, el) in &cache[n - k - 1] {
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
                cn,
                vec_lt(ol, or),
                Expr::bin(elp, erp, Operator::Lt, mask),
                n,
                cache,
            );
        }
        if USE_BIT_OR && el.prec() >= 6 && er.prec() > 6 {
            save(
                cn,
                ol.clone() | or,
                Expr::bin(elp, erp, Operator::BitOr, mask),
                n,
                cache,
            );
        }
        if USE_BIT_XOR && el.prec() >= 7 && er.prec() > 7 {
            save(
                cn,
                ol.clone() ^ or,
                Expr::bin(elp, erp, Operator::BitXor, mask),
                n,
                cache,
            );
        }
        if USE_BIT_AND && el.prec() >= 8 && er.prec() > 8 {
            save(
                cn,
                ol.clone() & or,
                Expr::bin(elp, erp, Operator::BitAnd, mask),
                n,
                cache,
            );
        }
        if el.prec() >= 10 && er.prec() > 10 {
            if USE_ADD {
                save(
                    cn,
                    ol.clone() + or,
                    Expr::bin(elp, erp, Operator::Add, mask),
                    n,
                    cache,
                );
            }
            if USE_SUB {
                save(
                    cn,
                    ol.clone() - or,
                    Expr::bin(elp, erp, Operator::Sub, mask),
                    n,
                    cache,
                );
            }
        }
        if el.prec() >= 11 && er.prec() > 11 {
            if USE_MUL {
                save(
                    cn,
                    ol.clone() * or,
                    Expr::bin(elp, erp, Operator::Mul, mask),
                    n,
                    cache,
                );
            }
            if let Some((div, modulo)) = divmod(ol, or) {
                if USE_MOD {
                    save(
                        cn,
                        modulo,
                        Expr::bin(elp, erp, Operator::Mod, mask),
                        n,
                        cache,
                    );
                }
                if USE_DIV1 {
                    save(cn, div, Expr::bin(elp, erp, Operator::Div1, mask), n, cache);
                }
            }
            if USE_GCD {
                save(
                    cn,
                    vec_gcd(ol, or),
                    Expr::bin(elp, erp, Operator::Gcd, mask),
                    n,
                    cache,
                );
            }
        }
    }
    // 2-byte operators
    if n < k + 3 {
        return;
    }
    for (ol, el) in &cache[n - k - 2] {
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
                cn,
                vec_or(ol, or),
                Expr::bin(elp, erp, Operator::Or, mask),
                n,
                cache,
            );
        }
        if USE_LE && el.prec() >= 5 && er.prec() > 5 {
            save(
                cn,
                vec_le(ol, or),
                Expr::bin(elp, erp, Operator::Le, mask),
                n,
                cache,
            );
        }
        if el.prec() > 9 && er.prec() >= 9 && vec_in(or, 0..=31) {
            if USE_BIT_SHL {
                save(
                    cn,
                    ol.clone() << or,
                    Expr::bin(elp, erp, Operator::BitShl, mask),
                    n,
                    cache,
                );
            }
            if USE_BIT_SHR {
                save(
                    cn,
                    ol.clone() >> or,
                    Expr::bin(elp, erp, Operator::BitShr, mask),
                    n,
                    cache,
                );
            }
        }
        if USE_DIV2 && el.prec() >= 11 && er.prec() > 11 {
            if let Some((div, _)) = divmod(ol, or) {
                save(cn, div, Expr::bin(elp, erp, Operator::Div2, mask), n, cache);
            }
        }
        if USE_EXP && el.prec() > 13 && er.prec() >= 13 && vec_in(or, 0..=6) {
            save(
                cn,
                vec_pow(ol, or),
                Expr::bin(elp, erp, Operator::Exp, mask),
                n,
                cache,
            );
        }
    }
    // 3-byte operators
    if n < k + 4 {
        return;
    }
    for (ol, el) in &cache[n - k - 3] {
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
                    cn,
                    z.clone(),
                    Expr::bin(elp, erp, Operator::SpaceOr, mask),
                    n,
                    cache,
                );
            }
            if USE_OR && ok_before_keyword(el) && !ok_after_keyword(er) {
                save(
                    cn,
                    z,
                    Expr::bin(elp, erp, Operator::OrSpace, mask),
                    n,
                    cache,
                );
            }
        }
    }
}

fn find_unary_expressions(cn: &mut CacheLevel, cache: &Cache, n: usize) {
    for (or, er) in cache[n - 1].iter() {
        let erp: NonNull<Expr> = er.into();
        if er.prec() >= 12 {
            if USE_BIT_NEG {
                save(
                    cn,
                    !or.clone(),
                    Expr::unary(erp, Operator::BitNeg),
                    n,
                    cache,
                );
            }
            if USE_NEG {
                save(cn, -or.clone(), Expr::unary(erp, Operator::Neg), n, cache);
            }
        }
    }
}

fn find_parens_expressions(cn: &mut CacheLevel, cache: &Cache, n: usize) {
    for (or, er) in cache[n - 2].iter() {
        if er.op < Operator::Parens {
            let erp: NonNull<Expr> = er.into();
            cn.insert(or.clone(), Expr::parens(erp));
        }
    }
}

fn find_variables_and_literals(cn: &mut CacheLevel, n: usize) {
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
    if MAX_LITERAL > 0 {
        let m = (10 as Num).pow(n as u32 - 1);
        for lit in m..=(m * 10 - 1).min(MAX_LITERAL) {
            let vec: Vector = Vector::constant(lit);
            cn.insert(vec, Expr::literal(lit as Literal));
        }
    }
}

#[cfg(feature = "rayon")]
fn find_expressions(mut_cache: &mut Cache, n: usize) {
    use rayon::{iter::Either, prelude::*};

    let cache = &mut_cache;
    let mut cn = (1..n - 1)
        .into_par_iter()
        .flat_map(|k| {
            // Use `par_bridge` for more parallelism when there're not enough
            // items in the map.
            // https://github.com/rust-lang/hashbrown/issues/383
            if k <= 2 {
                Either::Left(cache[k].iter().par_bridge())
            } else {
                Either::Right(cache[k].par_iter())
            }
            .map(move |r| {
                let mut cn = CacheLevel::new();
                find_binary_expressions(&mut cn, cache, n, k, r);
                cn
            })
        })
        .chain(unit_if(n >= 3 && n < MAX_LENGTH).into_par_iter().map(|()| {
            let mut cn = CacheLevel::new();
            find_parens_expressions(&mut cn, cache, n);
            cn
        }))
        .chain(unit_if(n >= 2).into_par_iter().map(|()| {
            let mut cn = CacheLevel::new();
            find_unary_expressions(&mut cn, cache, n);
            cn
        }))
        .reduce(
            || CacheLevel::new(),
            |mut level, mut level2| {
                if level.len() < level2.len() {
                    std::mem::swap(&mut level, &mut level2);
                }
                for (output, expr) in level2 {
                    insert_to_level(&mut level, output, expr);
                }
                level
            },
        );

    find_variables_and_literals(&mut cn, n);

    cn.shrink_to_fit();
    mut_cache.push(cn);
}

#[cfg(not(feature = "rayon"))]
fn find_expressions(cache: &mut Cache, n: usize) {
    let mut cn = CacheLevel::new();
    find_variables_and_literals(&mut cn, n);
    if n >= 3 && n < MAX_LENGTH {
        find_parens_expressions(&mut cn, cache, n);
    }
    if n >= 2 {
        find_unary_expressions(&mut cn, cache, n);
    }
    for k in 1..n - 1 {
        for r in &cache[k] {
            find_binary_expressions(&mut cn, cache, n, k, r);
        }
    }

    cn.shrink_to_fit();
    cache.push(cn);
}

fn main() {
    for i in INPUTS {
        assert_eq!(
            i.vec.len(),
            GOAL.len(),
            "INPUTS and GOAL must have equal length"
        );
    }
    let mut cache: Cache = vec![CacheLevel::new()];
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
