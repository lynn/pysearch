#![cfg_attr(feature = "simd", feature(portable_simd))]

pub mod expr;
pub mod operator;

#[path = "params.rs"]
pub mod params;

#[cfg_attr(all(feature = "simd", feature = "portable_simd"), path = "vec_simd.rs")]
#[cfg_attr(not(feature = "simd"), path = "vec.rs")]
pub mod vec;

use expr::{Expr, Mask, NonNullExpr};
use operator::{BinaryOperator, Operator};
use params::*;

use vec::Vector;

use hashbrown::{hash_set::Entry, HashMap, HashSet};
use std::time::Instant;

// cache[length][output] = highest-prec expression of that length yielding that output
type CacheLevel = Vec<Expr>;
type Cache = Vec<CacheLevel>;

type HashSetCache = HashSet<NonNullExpr>;

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

fn unit_if(b: bool) -> Option<()> {
    if b {
        Some(())
    } else {
        None
    }
}

fn save(level: &mut CacheLevel, expr: Expr, n: usize, cache: &Cache, hashset_cache: &HashSetCache) {
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
        let expr_ptr: NonNullExpr = (&expr).into();
        if let Some(e) = hashset_cache.get(&expr_ptr) {
            if e.as_ref().prec() >= expr.prec() {
                return;
            }
        }
    }

    if n > MAX_CACHE_LENGTH {
        for dfs_len in n + 2..=MAX_LENGTH {
            find_binary_expressions_left(level, cache, hashset_cache, dfs_len, n, &expr);
            find_binary_expressions_right(level, cache, hashset_cache, dfs_len, n, &expr);
        }
        if n + 1 <= MAX_LENGTH {
            find_unary_expression(level, cache, hashset_cache, n + 1, &expr);
        }
        if n + 2 < MAX_LENGTH && expr.op < Operator::Parens {
            save(
                level,
                Expr::parens((&expr).into()),
                n + 2,
                cache,
                hashset_cache,
            );
        }
        return;
    }

    level.push(expr);
}

fn apply_binary_operator(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    el: &Expr,
    er: &Expr,
    op: BinaryOperator,
) {
    if er.is_literal() && el.is_literal() {
        return;
    }
    if !REUSE_VARS && (el.var_mask & er.var_mask != 0) {
        return;
    }
    let mask = el.var_mask | er.var_mask;
    if !can_use_required_vars(mask, n) {
        return;
    }
    if op.can_apply(el, er) {
        if let Some(output) = op.vec_apply(el.output.clone(), &er.output) {
            save(
                cn,
                Expr::bin(el.into(), er.into(), op.into(), mask, output),
                n,
                cache,
                hashset_cache,
            );
        }
    }
}

fn find_binary_expressions_left(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    er: &Expr,
) {
    for &op in BINARY_OPERATORS {
        if k + op.length() >= n {
            continue;
        }
        if !op.can_apply_right(er) {
            continue;
        }
        for el in &cache[n - k - op.length()] {
            if op.can_apply_left(el) {
                apply_binary_operator(cn, cache, hashset_cache, n, el, er, op)
            }
        }
    }
}

fn find_binary_expressions_right(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    el: &Expr,
) {
    for &op in BINARY_OPERATORS {
        if k + op.length() >= n {
            continue;
        }
        if !op.can_apply_left(el) {
            continue;
        }
        for er in &cache[n - k - op.length()] {
            if op.can_apply_right(er) {
                apply_binary_operator(cn, cache, hashset_cache, n, el, er, op)
            }
        }
    }
}

fn find_unary_expression(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    er: &Expr,
) {
    if !can_use_required_vars(er.var_mask, n) {
        return;
    }
    for &op in UNARY_OPERATORS {
        if op.can_apply(er) {
            save(
                cn,
                Expr::unary(er, op.into(), op.vec_apply(er.output.clone())),
                n,
                cache,
                hashset_cache,
            );
        }
    }
}

fn find_unary_expressions(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
) {
    for r in &cache[n - 1] {
        find_unary_expression(cn, cache, hashset_cache, n, r);
    }
}

fn find_parens_expressions(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
) {
    for er in &cache[n - 2] {
        if !can_use_required_vars(er.var_mask, n) {
            return;
        }
        if er.op < Operator::Parens {
            if n <= MAX_CACHE_LENGTH {
                cn.push(Expr::parens(er));
            } else {
                save(cn, Expr::parens(er), n, cache, hashset_cache);
            }
        }
    }
}

fn find_variables_and_literals(cn: &mut CacheLevel, n: usize) {
    if n == 1 {
        for (i, input) in INPUTS.iter().enumerate() {
            cn.push(Expr::variable(i, Vector::from_slice(input.vec)));
        }
    }
    for &lit in LITERALS {
        if positive_integer_length(lit) == n {
            cn.push(Expr::literal(lit));
        }
    }
    if MAX_LITERAL > 0 {
        if let Some(m) = (10 as Num).checked_pow(n as u32 - 1) {
            for lit in m..=m.saturating_mul(9).saturating_add(m - 1).min(MAX_LITERAL) {
                cn.push(Expr::literal(lit));
            }
        }
    }
}

fn add_to_cache(mut cn: CacheLevel, cache: &mut Cache, hashset_cache: &mut HashSetCache, n: usize) {
    let mut idx = 0;
    let start_ptr = cn.as_ptr();
    while idx < cn.len() {
        let expr = &cn[idx];
        match hashset_cache.entry(expr.into()) {
            Entry::Occupied(e) => {
                let oe = e.get();
                if expr.prec() > oe.as_ref().prec() {
                    if oe.as_ptr() >= start_ptr && oe.as_ptr() < unsafe { start_ptr.add(idx) } {
                        unsafe {
                            *oe.as_mut_ptr() = cn.swap_remove(idx);
                        }
                    } else {
                        e.replace();
                        idx += 1;
                    }
                } else {
                    cn.swap_remove(idx);
                }
            }
            Entry::Vacant(e) => {
                e.insert();
                if hashset_cache.len() == hashset_cache.capacity() {
                    cn.shrink_to_fit();
                }
                idx += 1;
            }
        }
    }
    cn.shrink_to_fit();
    cache.push(cn);
    if n == std::cmp::min(MAX_CACHE_LENGTH, MAX_LENGTH - 1) {
        hashset_cache.shrink_to_fit();
    }
}

fn find_expressions_multithread(
    mut_cache: &mut Cache,
    mut_hashset_cache: &mut HashSetCache,
    n: usize,
) {
    use rayon::prelude::*;

    let cache = &mut_cache;
    let hashset_cache = &mut_hashset_cache;

    let mut cn = (1..n - 1)
        .into_par_iter()
        .flat_map(|k| {
            cache[k].par_iter().map(move |r| {
                let mut cn = CacheLevel::new();
                find_binary_expressions_left(&mut cn, cache, hashset_cache, n, k, r);
                cn
            })
        })
        .chain(unit_if(n >= 3 && n < MAX_LENGTH).into_par_iter().map(|()| {
            let mut cn = CacheLevel::new();
            find_parens_expressions(&mut cn, cache, hashset_cache, n);
            cn
        }))
        .chain(unit_if(n >= 2).into_par_iter().map(|()| {
            let mut cn = CacheLevel::new();
            find_unary_expressions(&mut cn, cache, hashset_cache, n);
            cn
        }))
        .flatten_iter()
        .collect();

    find_variables_and_literals(&mut cn, n);

    add_to_cache(cn, mut_cache, mut_hashset_cache, n);
}

fn find_expressions(cache: &mut Cache, hashset_cache: &mut HashSetCache, n: usize) {
    let mut cn = CacheLevel::new();
    find_variables_and_literals(&mut cn, n);
    if n >= 3 && n < MAX_LENGTH {
        find_parens_expressions(&mut cn, cache, hashset_cache, n);
    }
    if n >= 2 {
        find_unary_expressions(&mut cn, cache, hashset_cache, n);
    }
    for k in 1..n - 1 {
        for r in &cache[k] {
            find_binary_expressions_left(&mut cn, cache, hashset_cache, n, k, r);
        }
    }
    add_to_cache(cn, cache, hashset_cache, n);
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
    let mut hashset_cache: HashSetCache = HashSetCache::new();
    let mut total_count = 0;
    println!("sizeof(Expr) = {}", std::mem::size_of::<Expr>());
    let start = Instant::now();
    for n in 1..=MAX_LENGTH {
        match n {
            0..=MAX_CACHE_LENGTH | MAX_LENGTH => println!("Finding length {n}..."),
            _ => println!("Finding length {n}-{MAX_LENGTH}..."),
        }
        let layer_start = Instant::now();
        if n >= MIN_MULTITHREAD_LENGTH {
            find_expressions_multithread(&mut cache, &mut hashset_cache, n);
        } else {
            find_expressions(&mut cache, &mut hashset_cache, n);
        }
        let count = cache[n].len();
        total_count += count;
        let time = layer_start.elapsed();
        println!("Explored {count} expressions in {time:?}");
        let total_time = start.elapsed();
        println!("Total: {total_count} expressions in {total_time:?}\n");
    }
    println!();
}
