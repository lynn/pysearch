use std::ops::RangeBounds;

use ndarray::{Array, Ix1};

use crate::{
    gcd::gcd,
    params::{Num, C_STYLE_MOD, GOAL},
};

pub type Vec = Array<Num, Ix1>;

pub fn divmod(left: &Vec, right: &Vec) -> Option<(Vec, Vec)> {
    if left
        .iter()
        .zip(right)
        .any(|(&x, &y)| y == 0 || (x, y) == (Num::MIN, -1))
    {
        None
    } else if C_STYLE_MOD {
        Some((left / right, left % right))
    } else {
        let modulo = (left % right + right) % right;
        let div = (left - modulo.clone()) / right;
        Some((div, modulo))
    }
}

pub fn vec_or(left: &Vec, right: &Vec) -> Vec {
    Array::from_shape_fn(
        GOAL.len(),
        |i| if left[i] == 0 { right[i] } else { left[i] },
    )
}

pub fn vec_eq(left: &Vec, right: &Vec) -> Vec {
    Array::from_shape_fn(GOAL.len(), |i| (left[i] == right[i]) as Num)
}

pub fn vec_lt(left: &Vec, right: &Vec) -> Vec {
    Array::from_shape_fn(GOAL.len(), |i| (left[i] < right[i]) as Num)
}

pub fn vec_le(left: &Vec, right: &Vec) -> Vec {
    Array::from_shape_fn(GOAL.len(), |i| (left[i] <= right[i]) as Num)
}

pub fn vec_gcd(left: &Vec, right: &Vec) -> Vec {
    Array::from_shape_fn(GOAL.len(), |i| gcd(left[i], right[i]))
}

pub fn vec_pow(left: &Vec, right: &Vec) -> Vec {
    let mut k = left.clone();
    k.zip_mut_with(right, |x, y| *x = x.pow(*y as u32));
    k
}

pub fn vec_in<R: RangeBounds<Num>>(vec: &Vec, bounds: R) -> bool {
    (0..GOAL.len()).all(|i| bounds.contains(&vec[i]))
}
