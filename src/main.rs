#![cfg_attr(feature = "simd", feature(portable_simd))]

pub mod expr;
pub mod gcd;
pub mod operator;

#[path = "params.rs"]
pub mod params;

#[cfg_attr(all(feature = "simd", feature = "portable_simd"), path = "vec_simd.rs")]
#[cfg_attr(not(feature = "simd"), path = "vec.rs")]
pub mod vec;

use expr::{ok_after_keyword, ok_before_keyword, Expr, Mask};
use operator::Operator;
use params::*;

use vec::{divmod, vec_gcd, vec_in, vec_le, vec_lt, vec_or, vec_pow, Vector};

use hashbrown::{hash_set::Entry, HashMap, HashSet};
use std::ptr::NonNull;
use std::time::Instant;

// cache[length][output] = highest-prec expression of that length yielding that output
type CacheLevel = HashSet<Expr>;
type Cache = Vec<CacheLevel>;

fn positive_integer_length(mut k: Num) -> usize {
    let mut l = 1;
    while k >= 10 {
        k /= 10;
        l += 1;
    }
    l
}

fn can_use_required_vars(mask: u8, length: usize) -> bool {
    !USE_ALL_VARS || length + (INPUTS.len() - (mask.count_ones() as usize)) * 2 <= MAX_LENGTH
}

#[cfg(feature = "rayon")]
fn unit_if(b: bool) -> Option<()> {
    if b {
        Some(())
    } else {
        None
    }
}

fn save(level: &mut CacheLevel, expr: Expr, n: usize, cache: &Cache) {
    const ALL_MASK: Mask = (1 << INPUTS.len()) - 1;

    if (!USE_ALL_VARS || expr.var_mask == ALL_MASK) && match_goal(&expr) {
        println!("{expr}");
        return;
    }

    if n == MAX_LENGTH || n == MAX_LENGTH - 1 && expr.prec() < 12 {
        return;
    }

    if !REUSE_VARS && expr.var_mask == ALL_MASK {
        let mut mp: HashMap<Num, Num> = HashMap::new();
        for i in 0..GOAL.len() {
            if let Some(old) = mp.insert(expr.output[i], GOAL[i]) {
                if old != GOAL[i] {
                    return;
                }
            }
        }
    }

    if n <= MAX_LENGTH - 2 {
        for l in cache {
            if let Some(e) = l.get(&expr) {
                if e.prec() >= expr.prec() {
                    return;
                }
            }
        }
    }

    if n > MAX_CACHE_LENGTH {
        for dfs_len in n + 2..=MAX_LENGTH {
            find_binary_expressions_left(level, cache, dfs_len, n, &expr);
            find_binary_expressions_right(level, cache, dfs_len, n, &expr);
        }
        if n + 1 <= MAX_LENGTH {
            find_unary_expression(level, cache, n + 1, &expr);
        }
        if n + 2 < MAX_LENGTH && expr.op < Operator::Parens {
            save(level, Expr::parens((&expr).into()), n + 2, cache);
        }
        return;
    }

    insert_to_level(level, expr);
}

fn insert_to_level(level: &mut CacheLevel, expr: Expr) {
    let prec = expr.prec();
    match level.entry(expr) {
        Entry::Occupied(e) => {
            if prec > e.get().prec() {
                e.replace();
            }
        }
        Entry::Vacant(e) => {
            e.insert();
        }
    }
}

fn find_1_byte_operators(cn: &mut CacheLevel, cache: &Cache, n: usize, el: &Expr, er: &Expr) {
    if er.is_literal() && el.is_literal() {
        return;
    }
    let elp: NonNull<Expr> = el.into();
    let erp: NonNull<Expr> = er.into();
    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
        return;
    }
    let mask = el.var_mask | er.var_mask;
    if !can_use_required_vars(mask, n) {
        return;
    }
    let ol = &el.output;
    let or = &er.output;
    // For commutative operators, choose an arbitrary order for the two operands based on memory
    // address.
    let use_commutative_op = |prec| el.prec() >= prec && er.prec() >= prec && elp <= erp;
    if USE_LT && el.prec() >= 5 && er.prec() > 5 {
        save(
            cn,
            Expr::bin(elp, erp, Operator::Lt, mask, vec_lt(ol, or)),
            n,
            cache,
        );
    }
    if USE_BIT_OR && use_commutative_op(6) {
        save(
            cn,
            Expr::bin(elp, erp, Operator::BitOr, mask, ol.clone() | or),
            n,
            cache,
        );
    }
    if USE_BIT_XOR && use_commutative_op(7) {
        save(
            cn,
            Expr::bin(elp, erp, Operator::BitXor, mask, ol.clone() ^ or),
            n,
            cache,
        );
    }
    if USE_BIT_AND && use_commutative_op(8) {
        save(
            cn,
            Expr::bin(elp, erp, Operator::BitAnd, mask, ol.clone() & or),
            n,
            cache,
        );
    }
    if USE_ADD && use_commutative_op(10) {
        save(
            cn,
            Expr::bin(elp, erp, Operator::Add, mask, ol.clone() + or),
            n,
            cache,
        );
    }
    if USE_SUB && el.prec() >= 10 && er.prec() > 10 {
        save(
            cn,
            Expr::bin(elp, erp, Operator::Sub, mask, ol.clone() - or),
            n,
            cache,
        );
    }
    // No `use_commutative_op` here because there are other operators with precedence 11 that could
    // break commutativity.
    if USE_MUL && er.prec() > 11 && (el.prec() > 11 && elp <= erp || el.prec() == 11) {
        save(
            cn,
            Expr::bin(elp, erp, Operator::Mul, mask, ol.clone() * or),
            n,
            cache,
        );
    }
    if el.prec() >= 11 && er.prec() > 11 {
        if let Some((div, modulo)) = divmod(ol, or) {
            if USE_MOD {
                save(
                    cn,
                    Expr::bin(elp, erp, Operator::Mod, mask, modulo),
                    n,
                    cache,
                );
            }
            if USE_DIV1 {
                save(cn, Expr::bin(elp, erp, Operator::Div1, mask, div), n, cache);
            }
        }
        if USE_GCD {
            save(
                cn,
                Expr::bin(elp, erp, Operator::Gcd, mask, vec_gcd(ol, or)),
                n,
                cache,
            );
        }
    }
}

fn find_2_byte_operators(cn: &mut CacheLevel, cache: &Cache, n: usize, el: &Expr, er: &Expr) {
    if er.is_literal() && el.is_literal() {
        return;
    }
    let elp: NonNull<Expr> = el.into();
    let erp: NonNull<Expr> = er.into();
    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
        return;
    }
    let mask = el.var_mask | er.var_mask;
    if !can_use_required_vars(mask, n) {
        return;
    }
    let ol = &el.output;
    let or = &er.output;
    if USE_OR && el.prec() >= 3 && er.prec() > 3 && ok_before_keyword(el) && ok_after_keyword(er) {
        save(
            cn,
            Expr::bin(elp, erp, Operator::Or, mask, vec_or(ol, or)),
            n,
            cache,
        );
    }
    if USE_LE && el.prec() >= 5 && er.prec() > 5 {
        save(
            cn,
            Expr::bin(elp, erp, Operator::Le, mask, vec_le(ol, or)),
            n,
            cache,
        );
    }
    if el.prec() >= 9 && er.prec() > 9 && vec_in(or, 0..Num::BITS as Num) {
        if USE_BIT_SHL {
            save(
                cn,
                Expr::bin(elp, erp, Operator::BitShl, mask, ol.clone() << or),
                n,
                cache,
            );
        }
        if USE_BIT_SHR {
            save(
                cn,
                Expr::bin(elp, erp, Operator::BitShr, mask, ol.clone() >> or),
                n,
                cache,
            );
        }
    }
    if USE_DIV2 && el.prec() >= 11 && er.prec() > 11 {
        if let Some((div, _)) = divmod(ol, or) {
            save(cn, Expr::bin(elp, erp, Operator::Div2, mask, div), n, cache);
        }
    }
    if USE_EXP && el.prec() > 13 && er.prec() >= 13 {
        if let Some(output) = vec_pow(ol, or) {
            save(
                cn,
                Expr::bin(elp, erp, Operator::Exp, mask, output),
                n,
                cache,
            );
        }
    }
}

fn find_3_byte_operators(cn: &mut CacheLevel, cache: &Cache, n: usize, el: &Expr, er: &Expr) {
    if er.is_literal() && el.is_literal() {
        return;
    }
    let elp: NonNull<Expr> = el.into();
    let erp: NonNull<Expr> = er.into();
    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
        return;
    }
    let mask = el.var_mask | er.var_mask;
    if !can_use_required_vars(mask, n) {
        return;
    }
    let ol = &el.output;
    let or = &er.output;
    if USE_OR && el.prec() >= 3 && er.prec() > 3 {
        let z = vec_or(ol, or);
        match (ok_before_keyword(el), ok_after_keyword(er)) {
            (true, false) => save(
                cn,
                Expr::bin(elp, erp, Operator::OrSpace, mask, z),
                n,
                cache,
            ),
            (false, true) => save(
                cn,
                Expr::bin(elp, erp, Operator::SpaceOr, mask, z),
                n,
                cache,
            ),
            _ => {}
        }
    }
}

fn find_binary_expressions_left(cn: &mut CacheLevel, cache: &Cache, n: usize, k: usize, r: &Expr) {
    for l in &cache[n - k - 1] {
        find_1_byte_operators(cn, cache, n, l, r);
    }
    if n < k + 3 {
        return;
    }
    for l in &cache[n - k - 2] {
        find_2_byte_operators(cn, cache, n, l, r);
    }
    if n < k + 4 {
        return;
    }
    for l in &cache[n - k - 3] {
        find_3_byte_operators(cn, cache, n, l, r);
    }
}

fn find_binary_expressions_right(cn: &mut CacheLevel, cache: &Cache, n: usize, k: usize, l: &Expr) {
    for r in &cache[n - k - 1] {
        find_1_byte_operators(cn, cache, n, l, r);
    }
    if n < k + 3 {
        return;
    }
    for r in &cache[n - k - 2] {
        find_2_byte_operators(cn, cache, n, l, r);
    }
    if n < k + 4 {
        return;
    }
    for r in &cache[n - k - 3] {
        find_3_byte_operators(cn, cache, n, l, r);
    }
}

fn find_unary_expression(cn: &mut CacheLevel, cache: &Cache, n: usize, er: &Expr) {
    if !can_use_required_vars(er.var_mask, n) {
        return;
    }
    if er.prec() < 12 {
        return;
    }
    let erp: NonNull<Expr> = er.into();
    let or = &er.output;
    if USE_BIT_NEG {
        save(
            cn,
            Expr::unary(erp, Operator::BitNeg, !or.clone()),
            n,
            cache,
        );
    }
    if USE_NEG {
        save(cn, Expr::unary(erp, Operator::Neg, -or.clone()), n, cache);
    }
}

fn find_unary_expressions(cn: &mut CacheLevel, cache: &Cache, n: usize) {
    for r in &cache[n - 1] {
        find_unary_expression(cn, cache, n, r);
    }
}

fn find_parens_expressions(cn: &mut CacheLevel, cache: &Cache, n: usize) {
    for er in &cache[n - 2] {
        if !can_use_required_vars(er.var_mask, n) {
            return;
        }
        if er.op < Operator::Parens {
            let erp: NonNull<Expr> = er.into();
            if n <= MAX_CACHE_LENGTH {
                cn.insert(Expr::parens(erp));
            } else {
                save(cn, Expr::parens(erp), n, cache);
            }
        }
    }
}

fn find_variables_and_literals(cn: &mut CacheLevel, n: usize) {
    if n == 1 {
        for (i, input) in INPUTS.iter().enumerate() {
            cn.insert(Expr::variable(i, Vector::from_slice(input.vec)));
        }
    }
    for &lit in LITERALS {
        if positive_integer_length(lit) == n {
            cn.insert(Expr::literal(lit));
        }
    }
    if MAX_LITERAL > 0 {
        if let Some(m) = (10 as Num).checked_pow(n as u32 - 1) {
            for lit in m..=m.saturating_mul(9).saturating_add(m - 1).min(MAX_LITERAL) {
                cn.insert(Expr::literal(lit));
            }
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
                find_binary_expressions_left(&mut cn, cache, n, k, r);
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
                for expr in level2 {
                    insert_to_level(&mut level, expr);
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
            find_binary_expressions_left(&mut cn, cache, n, k, r);
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
    let mut layer_start = Instant::now();
    for n in 1..=MAX_LENGTH {
        match n {
            0..=MAX_CACHE_LENGTH => println!("Finding length {n}..."),
            n if n == MAX_CACHE_LENGTH + 1 => println!("Finding length {n}-{MAX_LENGTH}..."),
            _ => {}
        }
        find_expressions(&mut cache, n);
        let count = cache[n].len();
        total_count += count;
        if n <= MAX_CACHE_LENGTH || n == MAX_LENGTH {
            let time = layer_start.elapsed();
            println!("Explored {count} expressions in {time:?}");
            let total_time = start.elapsed();
            println!("Total: {total_count} expressions in {total_time:?}\n");
            layer_start = Instant::now()
        }
    }
    println!();
}
