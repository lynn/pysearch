use crate::params::{Num, GOAL};
use crate::{expr::Expr, operator::OpIndex, vec::Vector};

/// The Match trait provides default implementation for a simple matcher.
/// A custom matcher in params.rs can override any constants or methods.
pub trait Match: Sized {
    /// Match leaf expressions 1 output at a time to avoid unnecessary
    /// precalculations.
    const MATCH_1BY1: bool = true;

    fn new() -> Self;

    fn match_one(&mut self, index: usize, output: Num) -> bool {
        output == GOAL[index]
    }

    // Will be called after match_one returns true for all outputs
    fn match_final(self, _el: Option<&Expr>, _er: &Expr, _op: OpIndex) -> bool {
        true
    }

    fn match_all(expr: &Expr) -> bool {
        let mut matcher = Self::new();
        expr.output
            .iter()
            .enumerate()
            .all(|(i, &o)| matcher.match_one(i, o))
            && matcher.match_final(
                expr.left.map(|e| unsafe { e.as_ref() }),
                unsafe { expr.right.unwrap().as_ref() },
                expr.op_idx,
            )
    }

    fn output_has_conflict(output: &Vector) -> bool {
        let mut mp = hashbrown::HashMap::new();
        for i in 0..GOAL.len() {
            if let Some(old) = mp.insert(output[i], GOAL[i]) {
                if old != GOAL[i] {
                    return true;
                }
            }
        }
        false
    }
}
