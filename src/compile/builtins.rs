use std::num::NonZeroUsize;

use simplicity::node::CoreConstructible;

use super::ProgNode;
use crate::named::CoreExt;

/// Fold an array of size `size` elements using function `f`.
///
/// Function `f: E × A → A`
/// takes an array element of type `E` and an accumulator of type `A`,
/// and it produces an updated accumulator of type `A`.
///
/// The fold `(fold f)_n : E^n × A → A`
/// takes the array of type `E^n` and an initial accumulator of type `A`,
/// and it produces the final accumulator of type `A`.
pub fn array_fold(size: NonZeroUsize, f: &ProgNode) -> Result<ProgNode, simplicity::types::Error> {
    /// Recursively fold the array using the precomputed folding functions.
    fn tree_fold(
        n: usize,
        f_powers_of_two: &Vec<ProgNode>,
    ) -> Result<ProgNode, simplicity::types::Error> {
        if n == 1 {
            return Ok(f_powers_of_two[0].clone());
        }
        // For n > 1 the next largest power is always >= 0
        let max_pow2 = n.ilog2() as usize;
        let size_right = 1 << max_pow2;
        // Array is a left-balanced (right-associative) binary tree.
        let f_right = f_powers_of_two.get(max_pow2).expect("max_pow2 OOB");
        let f_left = tree_fold(n - size_right, f_powers_of_two)?;
        f_array_fold(&f_left, f_right)
    }

    /// Fold the two arrays applying the folding function sequentially left -> right.
    fn f_array_fold(
        f_left: &ProgNode,
        f_right: &ProgNode,
    ) -> Result<ProgNode, simplicity::types::Error> {
        // The input is a tuple ((L, R), acc): ([E; n], A) where:
        // - L and R are arrays of varying size E^x and E^y respectively (x + y = n).
        // - acc is an accumulator of type A.
        let ctx = f_left.inference_context();
        let left_arr = ProgNode::o().o().h(ctx);
        let right_arr = ProgNode::o().i().h(ctx);
        let acc = ProgNode::i().h(ctx);
        let left_res = left_arr.pair(acc).comp(f_left)?;
        let right_res = right_arr.pair(left_res).comp(f_right)?;
        Ok(right_res.build())
    }

    // Precompute the folding functions for arrays of size 2^i where i < n.
    let n = size.get();
    let mut f_powers_of_two: Vec<ProgNode> = Vec::with_capacity(n.ilog2() as usize);

    // An array of size 1 is just the element itself, so f_array_fold_1 is the same as the folding function.
    let mut f_prev = f.clone();
    f_powers_of_two.push(f_prev.clone());

    let mut i = 1;
    while i < n {
        f_prev = f_array_fold(&f_prev, &f_prev)?;
        f_powers_of_two.push(f_prev.clone());
        i *= 2;
    }

    tree_fold(n, &f_powers_of_two)
}

#[cfg(test)]
mod tests {
    use crate::{tests::TestCase, WitnessValues};

    #[test]
    fn array_fold() {
        TestCase::program_file("./examples/array_fold.simf")
            .with_witness_values(WitnessValues::default())
            .assert_run_success();
    }
}
