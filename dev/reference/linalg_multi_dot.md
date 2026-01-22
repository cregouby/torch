# Efficiently multiplies two or more matrices

Efficiently multiplies two or more matrices by reordering the
multiplications so that the fewest arithmetic operations are performed.

## Usage

``` r
linalg_multi_dot(tensors)
```

## Arguments

- tensors:

  (`Sequence[Tensor]`): two or more tensors to multiply. The first and
  last tensors may be 1D or 2D. Every other tensor must be 2D.

## Details

Supports inputs of `float`, `double`, `cfloat` and `cdouble` dtypes.
This function does not support batched inputs.

Every tensor in `tensors` must be 2D, except for the first and last
which may be 1D. If the first tensor is a 1D vector of shape `(n,)` it
is treated as a row vector of shape `(1, n)`, similarly if the last
tensor is a 1D vector of shape `(n,)` it is treated as a column vector
of shape `(n, 1)`.

If the first and last tensors are matrices, the output will be a matrix.
However, if either is a 1D vector, then the output will be a 1D vector.

## Note

This function is implemented by chaining
[`torch_mm()`](https://cregouby.github.io/torch/dev/reference/torch_mm.md)
calls after computing the optimal matrix multiplication order.

The cost of multiplying two matrices with shapes `(a, b)` and `(b, c)`
is `a * b * c`. Given matrices `A`, `B`, `C` with shapes `(10, 100)`,
`(100, 5)`, `(5, 50)` respectively, we can calculate the cost of
different multiplication orders as follows:

Equation not displayed. Install 'katex' then re-install 'torch'.

In this case, multiplying `A` and `B` first followed by `C` is 10 times
faster.

## See also

Other linalg:
[`linalg_cholesky()`](https://cregouby.github.io/torch/dev/reference/linalg_cholesky.md),
[`linalg_cholesky_ex()`](https://cregouby.github.io/torch/dev/reference/linalg_cholesky_ex.md),
[`linalg_det()`](https://cregouby.github.io/torch/dev/reference/linalg_det.md),
[`linalg_eig()`](https://cregouby.github.io/torch/dev/reference/linalg_eig.md),
[`linalg_eigh()`](https://cregouby.github.io/torch/dev/reference/linalg_eigh.md),
[`linalg_eigvals()`](https://cregouby.github.io/torch/dev/reference/linalg_eigvals.md),
[`linalg_eigvalsh()`](https://cregouby.github.io/torch/dev/reference/linalg_eigvalsh.md),
[`linalg_householder_product()`](https://cregouby.github.io/torch/dev/reference/linalg_householder_product.md),
[`linalg_inv()`](https://cregouby.github.io/torch/dev/reference/linalg_inv.md),
[`linalg_inv_ex()`](https://cregouby.github.io/torch/dev/reference/linalg_inv_ex.md),
[`linalg_lstsq()`](https://cregouby.github.io/torch/dev/reference/linalg_lstsq.md),
[`linalg_matrix_norm()`](https://cregouby.github.io/torch/dev/reference/linalg_matrix_norm.md),
[`linalg_matrix_power()`](https://cregouby.github.io/torch/dev/reference/linalg_matrix_power.md),
[`linalg_matrix_rank()`](https://cregouby.github.io/torch/dev/reference/linalg_matrix_rank.md),
[`linalg_norm()`](https://cregouby.github.io/torch/dev/reference/linalg_norm.md),
[`linalg_pinv()`](https://cregouby.github.io/torch/dev/reference/linalg_pinv.md),
[`linalg_qr()`](https://cregouby.github.io/torch/dev/reference/linalg_qr.md),
[`linalg_slogdet()`](https://cregouby.github.io/torch/dev/reference/linalg_slogdet.md),
[`linalg_solve()`](https://cregouby.github.io/torch/dev/reference/linalg_solve.md),
[`linalg_solve_triangular()`](https://cregouby.github.io/torch/dev/reference/linalg_solve_triangular.md),
[`linalg_svd()`](https://cregouby.github.io/torch/dev/reference/linalg_svd.md),
[`linalg_svdvals()`](https://cregouby.github.io/torch/dev/reference/linalg_svdvals.md),
[`linalg_tensorinv()`](https://cregouby.github.io/torch/dev/reference/linalg_tensorinv.md),
[`linalg_tensorsolve()`](https://cregouby.github.io/torch/dev/reference/linalg_tensorsolve.md),
[`linalg_vector_norm()`](https://cregouby.github.io/torch/dev/reference/linalg_vector_norm.md)

## Examples

``` r
if (torch_is_installed()) {

linalg_multi_dot(list(torch_tensor(c(1, 2)), torch_tensor(c(2, 3))))
}
#> torch_tensor
#> 8
#> [ CPUFloatType{} ]
```
