use std::ops::RangeBounds;

use ndarray::{Array, Ix1};

use crate::{
    gcd::gcd,
    params::{Num, C_STYLE_MOD, GOAL},
};

pub type Vector = Array<Num, Ix1>;

pub fn divmod(left: &Vector, right: &Vector) -> Option<(Vector, Vector)> {
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

pub fn vec_or(left: &Vector, right: &Vector) -> Vector {
    Array::from_shape_fn(
        GOAL.len(),
        |i| if left[i] == 0 { right[i] } else { left[i] },
    )
}

pub fn vec_eq(left: &Vector, right: &Vector) -> Vector {
    Array::from_shape_fn(GOAL.len(), |i| (left[i] == right[i]) as Num)
}

pub fn vec_lt(left: &Vector, right: &Vector) -> Vector {
    Array::from_shape_fn(GOAL.len(), |i| (left[i] < right[i]) as Num)
}

pub fn vec_le(left: &Vector, right: &Vector) -> Vector {
    Array::from_shape_fn(GOAL.len(), |i| (left[i] <= right[i]) as Num)
}

pub fn vec_gcd(left: &Vector, right: &Vector) -> Vector {
    Array::from_shape_fn(GOAL.len(), |i| gcd(left[i], right[i]))
}

pub fn vec_pow(left: &Vector, right: &Vector) -> Vector {
    let mut k = left.clone();
    k.zip_mut_with(right, |x, y| *x = x.pow(*y as u32));
    k
}

pub fn vec_in<R: RangeBounds<Num>>(vec: &Vector, bounds: R) -> bool {
    (0..GOAL.len()).all(|i| bounds.contains(&vec[i]))
}
