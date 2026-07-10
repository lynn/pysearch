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
    out0_groups: Vec<(Num, u32)>,
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

#[inline(always)]
fn iter_groups<'a>(cache_level: &'a CacheLevel) -> impl Iterator<Item = (&'a [Expr], Num)> {
    let mut start = 0;
    cache_level.out0_groups.iter().map(move |&(out0, len)| {
        let group = &cache_level.exprs[start..start + len as usize];
        start += len as usize;
        (group, out0)
    })
}

#[inline(always)]
fn par_iter_groups<'a>(
    cache_level: &'a CacheLevel,
) -> impl ParallelIterator<Item = (&'a [Expr], Num)> {
    let mut starts = Vec::with_capacity(cache_level.out0_groups.len());
    let mut current = 0;
    for &(_, len) in &cache_level.out0_groups {
        starts.push(current);
        current += len as usize;
    }
    cache_level
        .out0_groups
        .par_iter()
        .zip(starts.into_par_iter())
        .map(move |(&(out0, len), start)| (&cache_level.exprs[start..start + len as usize], out0))
}

// -----------------------------------------------------------------------------
// SINGLE EXPR LOGIC (Used when !Matcher::GROUP_BY_FIRST_OUTPUT)
// -----------------------------------------------------------------------------

fn save(cn: &mut Vec<Expr>, expr: Expr, n: usize, cache: &Cache, hashset_cache: &HashSetCache) {
    let uses_required_vars = expr
        .var_count
        .iter()
        .zip(INPUTS.iter())
        .all(|(&c, i)| c >= i.min_uses);

    if uses_required_vars && Matcher::match_all(&expr) {
        println!("{expr}");
        return;
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
            find_binary_expressions_single::<true>(cn, cache, hashset_cache, dfs_len, n, &expr);
        }
        if n + 1 <= MAX_LENGTH {
            find_unary_operators_single(cn, cache, hashset_cache, n + 1, &expr);
        }
        if !is_leaf_expr(OP_INDEX_PARENS, n + 2) && expr.op_idx < OP_INDEX_PARENS {
            save(
                cn,
                Expr::parens((&expr).into()),
                n + 2,
                cache,
                hashset_cache,
            );
        }
        return;
    }

    cn.push(expr);
}

#[inline(always)]
fn find_binary_operators_single(
    cn: &mut Vec<Expr>,
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
    let mut var_count = el.var_count;
    let mut exceeds = false;
    for ((l, &r), i) in var_count
        .iter_mut()
        .zip(er.var_count.iter())
        .zip(INPUTS.iter())
    {
        *l += r;
        if *l > i.max_uses {
            exceeds = true;
            break;
        }
    }
    if exceeds || !can_use_required_vars(var_count, n) {
        return;
    }
    seq!(idx in 0..100 {
        if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
            if op.name.len() == op_len && op.can_apply(el, er) {
                if Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n) {
                    let mut matcher = Matcher::new();
                    if el
                        .output
                        .iter()
                        .zip(er.output.iter())
                        .enumerate()
                        .all(|(i, (&ol, &or))| match op.apply_(ol, or) {
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
                        Expr::bin(el.into(), er.into(), op_idx, var_count, output),
                        n,
                        cache,
                        hashset_cache,
                    );
                }
            }
        }
    });
}

fn find_binary_expressions_single<const BI_DIRECTIONAL: bool>(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    e_fixed: &Expr,
) {
    seq!(op_len in 0..=5 {
        if n > k + op_len {
            for e_other in &cache[n - k - op_len].exprs {
                if BI_DIRECTIONAL {
                    find_binary_operators_single(cn, cache, hashset_cache, n, e_fixed, e_other, op_len);
                    find_binary_operators_single(cn, cache, hashset_cache, n, e_other, e_fixed, op_len);
                } else {
                    find_binary_operators_single(cn, cache, hashset_cache, n, e_other, e_fixed, op_len);
                }
            }
        }
    });
}

#[inline(always)]
fn find_unary_operators_single(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    er: &Expr,
) {
    if !can_use_required_vars(er.var_count, n) {
        return;
    }
    seq!(idx in 0..10 {
        if let (Some(&op_idx), Some(op)) = (OP_UNARY_INDEX_TABLE.get(idx), UNARY_OPERATORS.get(idx)) {
            if op.can_apply(er) {
                if Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n) {
                    let mut matcher = Matcher::new();
                    if er
                        .output
                        .iter()
                        .enumerate()
                        .all(|(i, &or)| matcher.match_one(i, op.apply_(or)))
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

fn find_unary_expressions_single(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
) {
    if n < 2 {
        return;
    }
    for r in &cache[n - 1].exprs {
        find_unary_operators_single(cn, cache, hashset_cache, n, r);
    }
}

fn find_parens_expressions_single(
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

// -----------------------------------------------------------------------------
// GROUPED EXPR LOGIC (Used when Matcher::GROUP_BY_FIRST_OUTPUT)
// -----------------------------------------------------------------------------

fn save_group(
    cn: &mut Vec<Expr>,
    group: Vec<Expr>,
    out0: Num,
    n: usize,
    cache: &Cache,
    hashset_cache: &HashSetCache,
) {
    if group.is_empty() {
        return;
    }

    let mut valid_exprs = Vec::with_capacity(group.len());
    let mut matcher = Matcher::new();
    let is_out0_match = matcher.match_one(0, out0);

    for expr in &group {
        if is_out0_match {
            let uses_required_vars = expr
                .var_count
                .iter()
                .zip(INPUTS.iter())
                .all(|(&c, i)| c >= i.min_uses);

            if uses_required_vars && Matcher::match_all(expr) {
                println!("{expr}");
                continue;
            }
        }
    }

    if is_leaf_expr(group[0].op_idx, n) {
        return;
    }

    for expr in group {
        let cant_use_more_vars = !HAS_UNLIMITED_VAR
            && expr
                .var_count
                .iter()
                .zip(INPUTS.iter())
                .all(|(&c, i)| c == i.max_uses);

        if cant_use_more_vars && Matcher::output_has_conflict(&expr.output) {
            continue;
        }

        if n <= MAX_LENGTH - 3 {
            let expr_ptr: NonNullExpr = (&expr).into();
            if let Some(e) = hashset_cache.get(&expr_ptr) {
                if e.as_ref().prec() >= expr.prec() {
                    continue;
                }
            }
        }

        valid_exprs.push(expr);
    }

    if valid_exprs.is_empty() {
        return;
    }

    if n > MAX_CACHE_LENGTH {
        for dfs_len in n + 1 + MIN_BINARY_OP_LEN..=MAX_LENGTH {
            find_binary_expressions_grouped::<true>(
                cn,
                cache,
                hashset_cache,
                dfs_len,
                n,
                &valid_exprs,
                out0,
            );
        }
        if n + 1 <= MAX_LENGTH {
            find_unary_expressions_grouped(cn, cache, hashset_cache, n + 1, &valid_exprs, out0);
        }
        if !is_leaf_expr(OP_INDEX_PARENS, n + 2) {
            find_parens_expressions_grouped(cn, cache, hashset_cache, n + 2, &valid_exprs, out0);
        }
    } else {
        cn.extend(valid_exprs);
    }
}

#[inline(always)]
fn find_binary_operators_grouped(
    cn: &mut Vec<Expr>,
    group: &[Expr],
    cache_level: &CacheLevel,
    start: usize,
    len: usize,
    op: &BinaryOp,
    op_idx: OpIndex,
    n: usize,
    forward_out0: Option<Num>,
    backward_out0: Option<Num>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
) {
    let mut new_group_forward = Vec::new();
    let mut new_group_backward = Vec::new();
    let check_match = Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n);

    for e2 in &cache_level.exprs[start..start + len] {
        for e1 in group {
            if e1.is_literal() && e2.is_literal() {
                continue;
            }

            let mut var_count = e1.var_count;
            let mut exceeds = false;
            for ((c1, c2), input) in var_count
                .iter_mut()
                .zip(e2.var_count.iter())
                .zip(INPUTS.iter())
            {
                *c1 += c2;
                if *c1 > input.max_uses {
                    exceeds = true;
                    break;
                }
            }
            if exceeds || !can_use_required_vars(var_count, n) {
                continue;
            }

            if forward_out0.is_some() && op.can_apply(e1, e2) {
                if check_match {
                    let mut matcher = Matcher::new();
                    if e1
                        .output
                        .iter()
                        .zip(e2.output.iter())
                        .enumerate()
                        .all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                            Some(o) => matcher.match_one(i, o),
                            None => false,
                        })
                        && matcher.match_final(Some(e1), e2, op_idx)
                    {
                        println!("{e1}{op_idx}{e2}");
                    }
                } else if let Some(output) = op.vec_apply(e1.output.clone(), &e2.output) {
                    new_group_forward.push(Expr::bin(
                        e1.into(),
                        e2.into(),
                        op_idx,
                        var_count,
                        output,
                    ));
                }
            }

            if backward_out0.is_some() && op.can_apply(e2, e1) {
                if check_match {
                    let mut matcher = Matcher::new();
                    if e2
                        .output
                        .iter()
                        .zip(e1.output.iter())
                        .enumerate()
                        .all(|(i, (&ol, &or))| match op.apply_(ol, or) {
                            Some(o) => matcher.match_one(i, o),
                            None => false,
                        })
                        && matcher.match_final(Some(e2), e1, op_idx)
                    {
                        println!("{e2}{op_idx}{e1}");
                    }
                } else if let Some(output) = op.vec_apply(e2.output.clone(), &e1.output) {
                    new_group_backward.push(Expr::bin(
                        e2.into(),
                        e1.into(),
                        op_idx,
                        var_count,
                        output,
                    ));
                }
            }
        }
    }

    if let Some(out0) = forward_out0 {
        save_group(cn, new_group_forward, out0, n, cache, hashset_cache);
    }
    if let Some(out0) = backward_out0 {
        save_group(cn, new_group_backward, out0, n, cache, hashset_cache);
    }
}

fn find_binary_expressions_grouped<const BI_DIRECTIONAL: bool>(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    k: usize,
    group: &[Expr],
    out0: Num,
) {
    seq!(op_len in 0..=5 {
        if n > k + op_len {
            let cache_level = &cache[n - k - op_len];
            let mut start = 0_usize;
            for &(e2_out0, len) in &cache_level.out0_groups {
                let len = len as usize;
                seq!(idx in 0..100 {
                    if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                        if op.name.len() == op_len {
                            let mut forward_out0 = None;
                            if let Some(new_out0) = op.apply_(out0, e2_out0) {
                                let check_match = Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n);
                                if !check_match || Matcher::new().match_one(0, new_out0) {
                                    forward_out0 = Some(new_out0);
                                }
                            }

                            let mut backward_out0 = None;
                            if BI_DIRECTIONAL {
                                if let Some(new_out0) = op.apply_(e2_out0, out0) {
                                    let check_match = Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n);
                                    if !check_match || Matcher::new().match_one(0, new_out0) {
                                        backward_out0 = Some(new_out0);
                                    }
                                }
                            }

                            if forward_out0.is_some() || backward_out0.is_some() {
                                find_binary_operators_grouped(
                                    cn, group, cache_level, start, len, op, op_idx, n,
                                    forward_out0, backward_out0, cache, hashset_cache
                                );
                            }
                        }
                    }
                });
                start += len;
            }
        }
    });
}

fn find_unary_expressions_grouped(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    group: &[Expr],
    out0: Num,
) {
    seq!(idx in 0..10 {
        if let (Some(&op_idx), Some(op)) = (OP_UNARY_INDEX_TABLE.get(idx), UNARY_OPERATORS.get(idx)) {
            let new_out0 = op.apply_(out0);
            let check_match = Matcher::MATCH_1BY1 && is_leaf_expr(op_idx, n);

            if !check_match || Matcher::new().match_one(0, new_out0) {
                let mut new_group = Vec::new();
                for er in group {
                    if !can_use_required_vars(er.var_count, n) { continue; }
                    if op.can_apply(er) {
                        if check_match {
                            let mut matcher = Matcher::new();
                            if er.output.iter().enumerate().all(|(i, &or)| matcher.match_one(i, op.apply_(or)))
                                && matcher.match_final(None, er, op_idx)
                            {
                                println!("{op_idx}{er}");
                            }
                        } else {
                            new_group.push(Expr::unary(er, op_idx, op.vec_apply(er.output.clone())));
                        }
                    }
                }
                save_group(cn, new_group, new_out0, n, cache, hashset_cache);
            }
        }
    });
}

fn find_parens_expressions_grouped(
    cn: &mut Vec<Expr>,
    cache: &Cache,
    hashset_cache: &HashSetCache,
    n: usize,
    group: &[Expr],
    out0: Num,
) {
    if n < 4 || is_leaf_expr(OP_INDEX_PARENS, n) {
        return;
    }

    let mut new_group = Vec::with_capacity(group.len());
    for er in group {
        if can_use_required_vars(er.var_count, n) && er.op_idx < OP_INDEX_PARENS {
            new_group.push(Expr::parens(er));
        }
    }
    save_group(cn, new_group, out0, n, cache, hashset_cache);
}

// -----------------------------------------------------------------------------
// MAIN SEARCH LOGIC
// -----------------------------------------------------------------------------

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
                if hashset_cache.len() == hashset_cache.capacity() {
                    cn.shrink_to_fit();
                }
                idx += 1;
            }
        }
    }

    if Matcher::GROUP_BY_FIRST_OUTPUT {
        for expr in &cn {
            let expr_key: NonNullExpr = expr.into();
            let expr_ptr = expr_key.as_ptr();
            if let Entry::Occupied(e) = hashset_cache.entry(expr_key) {
                if e.get().as_ptr() == expr_ptr {
                    e.remove();
                }
            }
        }
        cn.sort_by_key(|e| e.output[0]);
        for expr in &cn {
            hashset_cache.insert(expr.into());
        }
    }

    cn.shrink_to_fit();
    hashset_cache.shrink_to_fit();

    let mut out0_groups = Vec::new();
    if Matcher::GROUP_BY_FIRST_OUTPUT && !cn.is_empty() {
        let mut start = 0;
        let mut last_val = cn[0].output[0];
        for i in 1..cn.len() {
            let val = cn[i].output[0];
            if val != last_val {
                out0_groups.push((last_val, (i - start) as u32));
                start = i;
                last_val = val;
            }
        }
        out0_groups.push((last_val, (cn.len() - start) as u32));
    }
    out0_groups.shrink_to_fit();

    cache.push(CacheLevel {
        exprs: cn,
        out0_groups,
    });
}

fn find_expressions_multithread(
    mut_cache: &mut Cache,
    mut_hashset_cache: &mut HashSetCache,
    n: usize,
) {
    let cache = &*mut_cache;
    let hashset_cache = &*mut_hashset_cache;

    let mut cn: Vec<Expr>;

    if Matcher::GROUP_BY_FIRST_OUTPUT {
        cn = (1..n.saturating_sub(MIN_BINARY_OP_LEN))
            .into_par_iter()
            .flat_map(|k| {
                par_iter_groups(&cache[k]).map(move |(group, out0)| {
                    let mut cn = Vec::new();
                    find_binary_expressions_grouped::<false>(
                        &mut cn,
                        cache,
                        hashset_cache,
                        n,
                        k,
                        group,
                        out0,
                    );
                    cn
                })
            })
            .chain(
                (if n >= 4 && !is_leaf_expr(OP_INDEX_PARENS, n) {
                    &cache[n - 2..n - 1]
                } else {
                    &[]
                })
                .par_iter()
                .flat_map(|level| par_iter_groups(level))
                .map(move |(group, out0)| {
                    let mut cn = Vec::new();
                    find_parens_expressions_grouped(&mut cn, cache, hashset_cache, n, group, out0);
                    cn
                }),
            )
            .chain(
                (if n >= 2 { &cache[n - 1..n] } else { &[] })
                    .par_iter()
                    .flat_map(|level| par_iter_groups(level))
                    .map(move |(group, out0)| {
                        let mut cn = Vec::new();
                        find_unary_expressions_grouped(
                            &mut cn,
                            cache,
                            hashset_cache,
                            n,
                            group,
                            out0,
                        );
                        cn
                    }),
            )
            .flatten_iter()
            .collect();
    } else {
        cn = (1..n.saturating_sub(MIN_BINARY_OP_LEN))
            .into_par_iter()
            .flat_map(|k| {
                cache[k].exprs.par_iter().map(move |r| {
                    let mut cn = Vec::new();
                    find_binary_expressions_single::<false>(&mut cn, cache, hashset_cache, n, k, r);
                    cn
                })
            })
            .chain(
                std::iter::once_with(|| {
                    let mut cn = Vec::new();
                    find_parens_expressions_single(&mut cn, cache, hashset_cache, n);
                    cn
                })
                .par_bridge(),
            )
            .chain(
                std::iter::once_with(|| {
                    let mut cn = Vec::new();
                    find_unary_expressions_single(&mut cn, cache, hashset_cache, n);
                    cn
                })
                .par_bridge(),
            )
            .flatten_iter()
            .collect();
    }

    find_variables_and_literals(&mut cn, n);
    add_to_cache(cn, mut_cache, mut_hashset_cache);
}

fn find_expressions(cache: &mut Cache, hashset_cache: &mut HashSetCache, n: usize) {
    let mut cn = Vec::new();

    find_variables_and_literals(&mut cn, n);

    if Matcher::GROUP_BY_FIRST_OUTPUT {
        for k in 1..n.saturating_sub(MIN_BINARY_OP_LEN) {
            for (group, out0) in iter_groups(&cache[k]) {
                find_binary_expressions_grouped::<false>(
                    &mut cn,
                    cache,
                    hashset_cache,
                    n,
                    k,
                    group,
                    out0,
                );
            }
        }

        if n >= 4 && !is_leaf_expr(OP_INDEX_PARENS, n) {
            for (group, out0) in iter_groups(&cache[n - 2]) {
                find_parens_expressions_grouped(&mut cn, cache, hashset_cache, n, group, out0);
            }
        }

        if n >= 2 {
            for (group, out0) in iter_groups(&cache[n - 1]) {
                find_unary_expressions_grouped(&mut cn, cache, hashset_cache, n, group, out0);
            }
        }
    } else {
        find_parens_expressions_single(&mut cn, cache, hashset_cache, n);
        find_unary_expressions_single(&mut cn, cache, hashset_cache, n);
        for k in 1..n.saturating_sub(MIN_BINARY_OP_LEN) {
            for r in &cache[k].exprs {
                find_binary_expressions_single::<false>(&mut cn, cache, hashset_cache, n, k, r);
            }
        }
    }

    add_to_cache(cn, cache, hashset_cache);
}

fn find_expressions_inverse(cache: &Cache, hashset_cache: &HashSetCache) {
    let goal_vec = Vector::from_slice(GOAL);
    cache.par_iter().for_each(|level| {
        level.exprs.par_iter().for_each(|er| {
            seq!(idx in 0..100 {
                if let (Some(&op_idx), Some(op)) = (OP_BINARY_INDEX_TABLE.get(idx), BINARY_OPERATORS.get(idx)) {
                    if let Some(output) = op.vec_apply_inverse(er.output.clone(), &goal_vec) {
                        if let Some(el) = hashset_cache.get(&output) {
                            let el = el.as_ref();
                            if op.can_apply(el, er)
                                && el.var_count.iter().zip(er.var_count.iter()).zip(INPUTS.iter())
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

    let mut cache: Cache = vec![CacheLevel {
        exprs: Vec::new(),
        out0_groups: Vec::new(),
    }];
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
