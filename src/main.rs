#![cfg_attr(feature = "simd", feature(portable_simd))]

pub mod expr;
pub mod operator;

#[path = "params.rs"]
pub mod params;

#[cfg_attr(all(feature = "simd", feature = "portable_simd"), path = "vec_simd.rs")]
#[cfg_attr(not(feature = "simd"), path = "vec.rs")]
pub mod vec;

use expr::{Expr, Mask, NonNullExpr};
use operator::*;
use params::*;

use vec::Vector;

use hashbrown::{hash_set::Entry, HashMap, HashSet};
use seq_macro::seq;
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

fn is_leaf_expr(op_idx: OpIndex, length: usize) -> bool {
    length == MAX_LENGTH
        || length == MAX_LENGTH - 1 && (UNARY_OPERATORS.len() == 0 || op_idx.prec() < UnaryOp::PREC)
}

fn save(level: &mut CacheLevel, expr: Expr, n: usize, cache: &Cache, hashset_cache: &HashSetCache) {
    const ALL_MASK: Mask = (1 << INPUTS.len()) - 1;

    if (!USE_ALL_VARS || expr.var_mask == ALL_MASK) && Matcher::new().match_all(&expr) {
        println!("{expr}");
        return;
    }

    if !MATCH_1BY1 && is_leaf_expr(expr.op_idx, n) {
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
            find_unary_operators(level, cache, hashset_cache, n + 1, &expr);
        }
        if !is_leaf_expr(OP_INDEX_PARENS, n + 2) && expr.op_idx < OP_INDEX_PARENS {
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

#[inline(always)]
fn find_binary_operators(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    el: &Expr,
    er: &Expr,
    op_len: usize,
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
    seq!(idx in 0..100 {
        if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
            if op.name.len() == op_len && op.can_apply(el, er) {
                if MATCH_1BY1 && is_leaf_expr(op_idx, n) {
                    let mut matcher = Matcher::new();
                    if el
                        .output
                        .iter()
                        .zip(er.output.iter())
                        .enumerate()
                        .all(|(i, (&ol, &or))| match (op.apply)(ol, or) {
                            Some(o) => matcher.match_one(i, o),
                            None => false,
                        })
                        && matcher.match_final(Some(el), er, op_idx)
                    {
                        println!("{el}{op_idx}{er}");
                    }
                } else if let Some(output) = op.vec_apply(el.output.clone(), &er.output) {
                    save(
                        cn,
                        Expr::bin(el.into(), er.into(), op_idx, mask, output),
                        n,
                        cache,
                        hashset_cache,
                    );
                }
            }
        }
    });
}

fn find_binary_expressions_left(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    er: &Expr,
) {
    seq!(op_len in 1..=5 {
        if n <= k + op_len {
            return;
        };
        for el in &cache[n - k - op_len] {
            find_binary_operators(cn, cache, hashset_cache, n, el, er, op_len);
        }
    });
}

fn find_binary_expressions_right(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    el: &Expr,
) {
    seq!(op_len in 1..=5 {
        if n <= k + op_len {
            return;
        };
        for er in &cache[n - k - op_len] {
            find_binary_operators(cn, cache, hashset_cache, n, el, er, op_len);
        }
    });
}

fn find_unary_operators(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    er: &Expr,
) {
    if !can_use_required_vars(er.var_mask, n) {
        return;
    }
    seq!(idx in 0..10 {
        if let (Some(&op_idx), Some(op)) = (OP_UNARY_INDEX_TABLE.get(idx), UNARY_OPERATORS.get(idx)) {
            if op.can_apply(er) {
                if MATCH_1BY1 && is_leaf_expr(op_idx, n) {
                    let mut matcher = Matcher::new();
                    if er
                        .output
                        .iter()
                        .enumerate()
                        .all(|(i, &or)| matcher.match_one(i, (op.apply)(or)))
                        && matcher.match_final(None, er, op_idx)
                    {
                        println!("{op_idx}{er}");
                    }
                } else {
                    save(
                        cn,
                        Expr::unary(er, op_idx, op.vec_apply(er.output.clone())),
                        n,
                        cache,
                        hashset_cache,
                    );
                }
            }
        }
    });
}

fn find_unary_expressions(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
) {
    if n < 2 {
        return;
    }
    for r in &cache[n - 1] {
        find_unary_operators(cn, cache, hashset_cache, n, r);
    }
}

fn find_parens_expressions(
    cn: &mut CacheLevel,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
) {
    if n < 4 || is_leaf_expr(OP_INDEX_PARENS, n) {
        return;
    }
    for er in &cache[n - 2] {
        if !can_use_required_vars(er.var_mask, n) {
            continue;
        }
        if er.op_idx < OP_INDEX_PARENS {
            save(cn, Expr::parens(er), n, cache, hashset_cache);
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

fn add_to_cache(mut cn: CacheLevel, cache: &mut Cache, hashset_cache: &mut HashSetCache) {
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
    hashset_cache.shrink_to_fit();
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
        .chain(
            std::iter::once_with(|| {
                let mut cn = CacheLevel::new();
                find_parens_expressions(&mut cn, cache, hashset_cache, n);
                cn
            })
            .par_bridge(),
        )
        .chain(
            std::iter::once_with(|| {
                let mut cn = CacheLevel::new();
                find_unary_expressions(&mut cn, cache, hashset_cache, n);
                cn
            })
            .par_bridge(),
        )
        .flatten_iter()
        .collect();

    find_variables_and_literals(&mut cn, n);

    add_to_cache(cn, mut_cache, mut_hashset_cache);
}

fn find_expressions(cache: &mut Cache, hashset_cache: &mut HashSetCache, n: usize) {
    let mut cn = CacheLevel::new();
    find_variables_and_literals(&mut cn, n);
    find_parens_expressions(&mut cn, cache, hashset_cache, n);
    find_unary_expressions(&mut cn, cache, hashset_cache, n);
    for k in 1..n - 1 {
        for r in &cache[k] {
            find_binary_expressions_left(&mut cn, cache, hashset_cache, n, k, r);
        }
    }
    add_to_cache(cn, cache, hashset_cache);
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
