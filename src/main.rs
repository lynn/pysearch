mod expr;
mod matcher;
mod operator;
mod vec;

#[path = "params.rs"]
mod params;

use expr::{Expr, NonNullExpr, VarCount};
use matcher::Match as _;
use operator::*;
use params::*;
use vec::Vector;

use hashbrown::{hash_set::Entry, HashSet};
use rayon::prelude::*;
use seq_macro::seq;
use std::time::Instant;

struct CacheLevel {
    exprs: Vec<Expr>,
    sorted_indices: Vec<u32>,
    out0_groups: Vec<(Num, u32, u32)>,
}

type Cache = Vec<CacheLevel>;

type HashSetCache = HashSet<NonNullExpr>;

const HAS_UNLIMITED_VAR: bool = has_unlimited_var();

const fn has_unlimited_var() -> bool {
    let mut i = 0;
    while i < INPUTS.len() {
        if INPUTS[i].max_uses == u8::MAX {
            return true;
        }
        i += 1;
    }
    false
}

fn positive_integer_length(mut k: Num) -> usize {
    let mut l = 1;
    while k >= 10 {
        k /= 10;
        l += 1;
    }
    l
}

fn can_use_required_vars(var_count: VarCount, length: usize) -> bool {
    let missing_uses: u8 = var_count
        .iter()
        .zip(INPUTS.iter())
        .map(|(&c, i)| i.min_uses.saturating_sub(c))
        .sum();
    length + missing_uses as usize * (1 + MIN_BINARY_OP_LEN) <= MAX_LENGTH
}

fn is_leaf_expr(op_idx: OpIndex, length: usize) -> bool {
    length == MAX_LENGTH
        || length == MAX_LENGTH - 1 && (UNARY_OPERATORS.len() == 0 || op_idx.prec() < UnaryOp::PREC)
}

fn save(level: &mut Vec<Expr>, expr: Expr, n: usize, cache: &Cache, hashset_cache: &HashSetCache) {
    if Matcher::new().match_one(0, expr.output[0]) {
        let uses_required_vars = expr
            .var_count
            .iter()
            .zip(INPUTS.iter())
            .all(|(&c, i)| c >= i.min_uses);

        if uses_required_vars && Matcher::match_all(&expr) {
            println!("{expr}");
            return;
        }
    }

    if !Matcher::MATCH_1BY1 && is_leaf_expr(expr.op_idx, n) {
        return;
    }

    let cant_use_more_vars = !HAS_UNLIMITED_VAR
        && expr
            .var_count
            .iter()
            .zip(INPUTS.iter())
            .all(|(&c, i)| c == i.max_uses);

    if cant_use_more_vars && Matcher::output_has_conflict(&expr.output) {
        return;
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
        for dfs_len in n + 1 + MIN_BINARY_OP_LEN..=MAX_LENGTH {
            find_binary_expressions(level, cache, hashset_cache, dfs_len, n, &expr);
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
fn find_binary_operators<const OP_LEN: usize, const BI_DIRECTIONAL: bool>(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    e1: &Expr,
    e2: &Expr,
) {
    if e1.is_literal() && e2.is_literal() { return; }
    
    let mut var_count = e2.var_count;
    let mut valid_vars = true;
    for ((l, &r), input) in var_count.iter_mut().zip(e1.var_count.iter()).zip(INPUTS.iter()) {
        *l += r;
        if *l > input.max_uses { valid_vars = false; break; }
    }
    if !valid_vars || !can_use_required_vars(var_count, n) { return; }

    let e1_out0 = e1.output[0];
    let e2_out0 = e2.output[0];

    seq!(idx in 0..32 {
        if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
            if op.name.len() == OP_LEN {
                if Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n) {
                    let mut matcher = Matcher::new();
                    if EARLY_FIRST_ELEMENT_MATCH {
                        if let Some(o0) = op.apply_(e1_out0, e2_out0) {
                            if matcher.match_one(0, o0) && op.can_apply(e1, e2) {
                                if e1.output.iter().zip(e2.output.iter()).enumerate().skip(1).all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                                    Some(o) => matcher.match_one(i, o),
                                    None => false,
                                }) && matcher.match_final(Some(e1), e2, op_idx) {
                                    println!("{e1}{op_idx}{e2}");
                                }
                            }
                        }
                    } else {
                        if op.can_apply(e1, e2) {
                            if e1.output.iter().zip(e2.output.iter()).enumerate().all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                                Some(o) => matcher.match_one(i, o),
                                None => false,
                            }) && matcher.match_final(Some(e1), e2, op_idx) {
                                println!("{e1}{op_idx}{e2}");
                            }
                        }
                    }
                } else if op.can_apply(e1, e2) {
                    if let Some(output) = op.vec_apply(e1.output.clone(), &e2.output) {
                        save(cn, Expr::bin(e1.into(), e2.into(), op_idx, var_count, output), n, cache, hashset_cache);
                    }
                }      
                if BI_DIRECTIONAL {
                    if Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n) {
                        let mut matcher = Matcher::new();
                        if EARLY_FIRST_ELEMENT_MATCH {
                            if let Some(o0) = op.apply_(e2_out0, e1_out0) {
                                if matcher.match_one(0, o0) && op.can_apply(e2, e1) {
                                    if e2.output.iter().zip(e1.output.iter()).enumerate().skip(1).all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                                        Some(o) => matcher.match_one(i, o),
                                        None => false,
                                    }) && matcher.match_final(Some(e2), e1, op_idx) {
                                        println!("{e2}{op_idx}{e1}");
                                    }
                                }
                            }
                        } else {
                            if op.can_apply(e2, e1) {
                                if e2.output.iter().zip(e1.output.iter()).enumerate().all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                                    Some(o) => matcher.match_one(i, o),
                                    None => false,
                                }) && matcher.match_final(Some(e2), e1, op_idx) {
                                    println!("{e2}{op_idx}{e1}");
                                }
                            }
                        }
                    } else if op.can_apply(e2, e1) {
                        if let Some(output) = op.vec_apply(e2.output.clone(), &e1.output) {
                            save(cn, Expr::bin(e2.into(), e1.into(), op_idx, var_count, output), n, cache, hashset_cache);
                        }
                    }      
                }
            }
        }
    });
}

#[inline(always)]
fn find_binary_operators_chunked_left<const OP_LEN: usize>(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    left_level: &CacheLevel,
    er: &Expr,
) {
    let left_cache = &left_level.exprs;
    let sorted_indices = &left_level.sorted_indices;
    let er_is_literal = er.is_literal();
    let er_var_count = er.var_count;
    let er_out0 = er.output[0];

    for &(ol0, start, len) in &left_level.out0_groups {
        let mut valid_ops = 0u32;
        
        seq!(idx in 0..32 {
            if let (Some(&_op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                if op.name.len() == OP_LEN {
                    if let Some(o0) = op.apply_(ol0, er_out0) {
                        let mut matcher = Matcher::new();
                        if matcher.match_one(0, o0) {
                            valid_ops |= 1u32 << idx;
                        }
                    }
                }
            }
        });
        
        if valid_ops == 0 { continue; }
        
        for j in 0..len {
            let el = &left_cache[sorted_indices[(start + j) as usize] as usize];
            if er_is_literal && el.is_literal() { continue; }
            
            let mut var_count = el.var_count;
            let mut valid_vars = true;
            for ((l, &r), input) in var_count.iter_mut().zip(er_var_count.iter()).zip(INPUTS.iter()) {
                *l += r;
                if *l > input.max_uses { valid_vars = false; break; }
            }
            if !valid_vars || !can_use_required_vars(var_count, n) { continue; }
            
            seq!(idx in 0..32 {
                if (valid_ops & (1u32 << idx)) != 0 {
                    if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                        if op.can_apply(el, er) {
                            if Matcher::MATCH_1BY1 {
                                let mut matcher = Matcher::new();
                                if el.output.iter().zip(er.output.iter()).enumerate().all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                                    Some(o) => matcher.match_one(i, o),
                                    None => false,
                                }) && matcher.match_final(Some(el), er, op_idx) {
                                    println!("{el}{op_idx}{er}");
                                }
                            } else  {
                                if let Some(output) = op.vec_apply(el.output.clone(), &er.output) {
                                    save(cn, Expr::bin(el.into(), er.into(), op_idx, var_count, output), n, cache, hashset_cache);
                                }
                            }
                        }
                    }
                }
            });
        }
    }
}

#[inline(always)]
fn find_binary_operators_chunked<const OP_LEN: usize>(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    left_level: &CacheLevel,
    e1: &Expr,
) {
    let left_cache = &left_level.exprs;
    let sorted_indices = &left_level.sorted_indices;
    let e1_is_literal = e1.is_literal();
    let e1_var_count = e1.var_count;
    let e1_out0 = e1.output[0];

    for &(e2_out0, start, len) in &left_level.out0_groups {
        let mut valid_ops_e1_e2 = 0u32;
        let mut valid_ops_e2_e1 = 0u32;

        seq!(idx in 0..32 {
            if let (Some(&_op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                if op.name.len() == OP_LEN {
                    if let Some(o0) = op.apply_(e1_out0, e2_out0) {
                        let mut matcher = Matcher::new();
                        if matcher.match_one(0, o0) { valid_ops_e1_e2 |= 1u32 << idx; }
                    }
                    if let Some(o0) = op.apply_(e2_out0, e1_out0) {
                        let mut matcher = Matcher::new();
                        if matcher.match_one(0, o0) { valid_ops_e2_e1 |= 1u32 << idx; }
                    }
                }
            }
        });

        if valid_ops_e1_e2 == 0 && valid_ops_e2_e1 == 0 { continue; }

        for j in 0..len {
            let e2 = &left_cache[sorted_indices[(start + j) as usize] as usize];
            if e1_is_literal && e2.is_literal() { continue; }

            let mut var_count = e2.var_count;
            let mut valid_vars = true;
            for ((l, &r), input) in var_count.iter_mut().zip(e1_var_count.iter()).zip(INPUTS.iter()) {
                *l += r;
                if *l > input.max_uses { valid_vars = false; break; }
            }
            if !valid_vars || !can_use_required_vars(var_count, n) { continue; }

            seq!(idx in 0..32 {
                if (valid_ops_e1_e2 & (1u32 << idx)) != 0 {
                    if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                        if op.can_apply(e1, e2) {
                            let mut matcher = Matcher::new();
                            if Matcher::MATCH_1BY1 {
                                if e1.output.iter().zip(e2.output.iter()).enumerate().all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                                    Some(o) => matcher.match_one(i, o),
                                    None => false,
                                }) && matcher.match_final(Some(e1), e2, op_idx) {
                                    println!("{e1}{op_idx}{e2}");
                                }
                            } else  {
                                if let Some(output) = op.vec_apply(e1.output.clone(), &e2.output) {
                                    save(cn, Expr::bin(e1.into(), e2.into(), op_idx, var_count, output), n, cache, hashset_cache);
                                }
                            }
                        }
                    }
                }
                if (valid_ops_e2_e1 & (1u32 << idx)) != 0 {
                    if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                        if op.can_apply(e2, e1) {
                            if Matcher::MATCH_1BY1 {
                                let mut matcher = Matcher::new();
                                if e2.output.iter().zip(e1.output.iter()).enumerate().all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                                    Some(o) => matcher.match_one(i, o),
                                    None => false,
                                }) && matcher.match_final(Some(e2), e1, op_idx) {
                                    println!("{e2}{op_idx}{e1}");
                                }
                            } else  {
                                if let Some(output) = op.vec_apply(e2.output.clone(), &e1.output) {
                                    save(cn, Expr::bin(e2.into(), e1.into(), op_idx, var_count, output), n, cache, hashset_cache);
                                }
                            }
                        }
                    }
                }
            });
        }
    }
}

fn find_binary_expressions_left(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    er: &Expr,
) {
    seq!(op_len in 0..=5 {
        if n > k + op_len {
            let left_level = &cache[n - k - op_len];
            if EARLY_FIRST_ELEMENT_MATCH && n == MAX_LENGTH {
                find_binary_operators_chunked_left::<op_len>(cn, cache, hashset_cache, n, left_level, er);
            } else {
                for el in &left_level.exprs {
                    find_binary_operators::<op_len, false>(cn, cache, hashset_cache, n, el, er);
                }
            } 
        }
    });
}

fn find_binary_expressions(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    e1: &Expr,
) {
    seq!(op_len in 0..=5 {
        if n > k + op_len {
            let other_level = &cache[n - k - op_len];
            if EARLY_FIRST_ELEMENT_MATCH && n == MAX_LENGTH {
                find_binary_operators_chunked::<op_len>(cn, cache, hashset_cache, n, other_level, e1);
            } else {
                for e2 in &other_level.exprs {
                    find_binary_operators::<op_len, true>(cn, cache, hashset_cache, n, e1, e2);
                }
            }
        }
    });
}

fn find_unary_operators(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    er: &Expr,
) {
    if !can_use_required_vars(er.var_count, n) {
        return;
    }
    
    let er_out0 = er.output[0];

    seq!(idx in 0..10 {
        if let (Some(&op_idx), Some(op)) = (OP_UNARY_INDEX_TABLE.get(idx), UNARY_OPERATORS.get(idx)) {
            if Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n) {
                let o0 = op.apply_(er_out0);
                let mut matcher = Matcher::new();
                if matcher.match_one(0, o0) && op.can_apply(er) {
                    if er.output.iter().enumerate().skip(1).all(|(i, &or)| matcher.match_one(i, op.apply_(or)))
                        && matcher.match_final(None, er, op_idx)
                    {
                        println!("{op_idx}{er}");
                    }
                }
            } else {
                if op.can_apply(er) {
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
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
) {
    if n < 2 {
        return;
    }
    for r in &cache[n - 1].exprs {
        find_unary_operators(cn, cache, hashset_cache, n, r);
    }
}

fn find_parens_expressions(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
) {
    if n < 4 || is_leaf_expr(OP_INDEX_PARENS, n) {
        return;
    }
    for er in &cache[n - 2].exprs {
        if !can_use_required_vars(er.var_count, n) {
            continue;
        }
        if er.op_idx < OP_INDEX_PARENS {
            save(cn, Expr::parens(er), n, cache, hashset_cache);
        }
    }
}

fn find_variables_and_literals(cn: &mut Vec<Expr>, n: usize) {
    for (i, input) in INPUTS.iter().enumerate() {
        if n == input.name.len() {
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

fn add_to_cache(mut cn: Vec<Expr>, cache: &mut Cache, hashset_cache: &mut HashSetCache) {
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
                        hashset_cache.replace(expr.into());
                        idx += 1;
                    }
                } else {
                    cn.swap_remove(idx);
                }
            }
            Entry::Vacant(e) => {
                e.insert();
                idx += 1;
            }
        }
    }

    let mut sorted_indices: Vec<u32> = (0..cn.len() as u32).collect();
    sorted_indices.sort_unstable_by(|&a, &b| cn[a as usize].output[0].cmp(&cn[b as usize].output[0]));

    let mut out0_groups = Vec::new();
    if !sorted_indices.is_empty() {
        let mut start = 0;
        let mut last_val = cn[sorted_indices[0] as usize].output[0];
        for i in 1..sorted_indices.len() {
            let val = cn[sorted_indices[i] as usize].output[0];
            if val != last_val {
                out0_groups.push((last_val, start as u32, (i - start) as u32));
                start = i;
                last_val = val;
            }
        }
        out0_groups.push((last_val, start as u32, (sorted_indices.len() - start) as u32));
    }

    cache.push(CacheLevel {
        exprs: cn,
        sorted_indices,
        out0_groups,
    });
}

fn find_expressions_multithread(
    mut_cache: &mut Cache,
    mut_hashset_cache: &mut HashSetCache,
    n: usize,
) {
    let cache = &mut_cache;
    let hashset_cache = &mut_hashset_cache;

    let mut cn: Vec<Expr> = (1..n.saturating_sub(MIN_BINARY_OP_LEN))
        .into_par_iter()
        .flat_map(|k| {
            cache[k].exprs.par_iter().fold(
                || Vec::new(),
                move |mut cn, er| {
                    find_binary_expressions_left(&mut cn, cache, hashset_cache, n, k, er);
                    cn
                },
            )
        })
        .chain(
            (if n >= 4 && !is_leaf_expr(OP_INDEX_PARENS, n) {
                &cache[n - 2].exprs[..]
            } else {
                &[]
            })
            .par_iter()
            .fold(
                || Vec::new(),
                move |mut cn, er| {
                    if can_use_required_vars(er.var_count, n) && er.op_idx < OP_INDEX_PARENS {
                        save(&mut cn, Expr::parens(er), n, cache, hashset_cache);
                    }
                    cn
                },
            ),
        )
        .chain(
            (if n >= 2 { &cache[n - 1].exprs[..] } else { &[] })
                .par_iter()
                .fold(
                    || Vec::new(),
                    move |mut cn, r| {
                        find_unary_operators(&mut cn, cache, hashset_cache, n, r);
                        cn
                    },
                ),
        )
        .flatten_iter()
        .collect();

    find_variables_and_literals(&mut cn, n);

    add_to_cache(cn, mut_cache, mut_hashset_cache);
}

fn find_expressions(cache: &mut Cache, hashset_cache: &mut HashSetCache, n: usize) {
    let mut cn = Vec::new();
    find_variables_and_literals(&mut cn, n);
    find_parens_expressions(&mut cn, cache, hashset_cache, n);
    find_unary_expressions(&mut cn, cache, hashset_cache, n);
    for k in 1..n.saturating_sub(MIN_BINARY_OP_LEN) {
        for r in &cache[k].exprs {
            find_binary_expressions_left(&mut cn, cache, hashset_cache, n, k, r);
        }
    }
    add_to_cache(cn, cache, hashset_cache);
}

fn find_expressions_inverse(cache: &Cache, hashset_cache: &HashSetCache) {
    let goal_vec = Vector::from_slice(GOAL);
    cache
        .par_iter()
        .for_each(|level| {
            level.exprs.par_iter().for_each(|er| {
                seq!(idx in 0..32 {
                    if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                        if let Some(output) = op.vec_apply_inverse(er.output.clone(), &goal_vec) {
                            if let Some(el) = hashset_cache.get(&output) {
                                let el = el.as_ref();
                                if op.can_apply(el, er)
                                    && el
                                        .var_count
                                        .iter()
                                        .zip(er.var_count.iter())
                                        .zip(INPUTS.iter())
                                        .all(|((&l, &r), i)| l + r >= i.min_uses && l + r <= i.max_uses) {
                                    println!("{el}{op_idx}{er}");
                                }
                            }
                        }
                    }
                });
            });
        });
}

fn validate_input() {
    for i in INPUTS {
        assert_eq!(
            i.vec.len(),
            GOAL.len(),
            "INPUTS and GOAL must have equal length"
        );

        assert_ne!(i.max_uses, 0, "INPUTS maximum uses must be non-zero");
    }

    assert!(
        INPUTS.iter().map(|i| i.min_uses as usize).sum::<usize>() * (1 + MIN_BINARY_OP_LEN)
            <= MAX_LENGTH + 1,
        "The minimum uses requirement will never be met"
    );

    let mut input_set = HashSet::new();
    for i in 0..INPUTS[0].vec.len() {
        let mut input = [0; INPUTS.len()];
        for j in 0..INPUTS.len() {
            input[j] = INPUTS[j].vec[i];
        }
        assert!(input_set.insert(input), "duplicated input {:?}", input);
    }
}

fn main() {
    validate_input();

    let mut cache: Cache = vec![CacheLevel { exprs: Vec::new(), sorted_indices: Vec::new(), out0_groups: Vec::new() }];
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
        let count = cache[n].exprs.len();
        total_count += count;
        let time = layer_start.elapsed();
        println!("Cached {count} expressions in {time:?}");
        let total_time = start.elapsed();
        println!("Total: {total_count} expressions in {total_time:?}\n");
        if ENABLE_INVERSE_SEARCH && n == MAX_CACHE_LENGTH {
            println!(
                "Finding length {n}-{} with invertible operators...",
                2 * n + 1
            );
            let inverse_start = Instant::now();
            find_expressions_inverse(&cache, &hashset_cache);
            let time = inverse_start.elapsed();
            println!("Explored expressions with invertible operators in {time:?}\n");
        }
    }
    println!();
}