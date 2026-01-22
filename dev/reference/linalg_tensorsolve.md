# Computes the solution `X` to the system `torch_tensordot(A, X) = B`.

If `m` is the product of the first `B`\\ `.ndim` dimensions of `A` and
`n` is the product of the rest of the dimensions, this function expects
`m` and `n` to be equal. The returned tensor `x` satisfies
`tensordot(A, x, dims=x$ndim) == B`.

## Usage

``` r
linalg_tensorsolve(A, B, dims = NULL)
```

## Arguments

- A:

  (Tensor): tensor to solve for.

- B:

  (Tensor): the solution

- dims:

  (`Tuple[int]`, optional): dimensions of `A` to be moved. If `NULL`, no
  dimensions are moved. Default: `NULL`.

## Details

If `dims` is specified, `A` will be reshaped as
`A = movedim(A, dims, seq(len(dims) - A$ndim + 1, 0))`

Supports inputs of float, double, cfloat and cdouble dtypes.

## See also

- [`linalg_tensorinv()`](https://cregouby.github.io/torch/dev/reference/linalg_tensorinv.md)
  computes the multiplicative inverse of
  [`torch_tensordot()`](https://cregouby.github.io/torch/dev/reference/torch_tensordot.md).

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
[`linalg_multi_dot()`](https://cregouby.github.io/torch/dev/reference/linalg_multi_dot.md),
[`linalg_norm()`](https://cregouby.github.io/torch/dev/reference/linalg_norm.md),
[`linalg_pinv()`](https://cregouby.github.io/torch/dev/reference/linalg_pinv.md),
[`linalg_qr()`](https://cregouby.github.io/torch/dev/reference/linalg_qr.md),
[`linalg_slogdet()`](https://cregouby.github.io/torch/dev/reference/linalg_slogdet.md),
[`linalg_solve()`](https://cregouby.github.io/torch/dev/reference/linalg_solve.md),
[`linalg_solve_triangular()`](https://cregouby.github.io/torch/dev/reference/linalg_solve_triangular.md),
[`linalg_svd()`](https://cregouby.github.io/torch/dev/reference/linalg_svd.md),
[`linalg_svdvals()`](https://cregouby.github.io/torch/dev/reference/linalg_svdvals.md),
[`linalg_tensorinv()`](https://cregouby.github.io/torch/dev/reference/linalg_tensorinv.md),
[`linalg_vector_norm()`](https://cregouby.github.io/torch/dev/reference/linalg_vector_norm.md)

## Examples

``` r
if (torch_is_installed()) {
A <- torch_eye(2 * 3 * 4)$reshape(c(2 * 3, 4, 2, 3, 4))
B <- torch_randn(2 * 3, 4)
X <- linalg_tensorsolve(A, B)
X$shape
torch_allclose(torch_tensordot(A, X, dims = X$ndim), B)

A <- torch_randn(6, 4, 4, 3, 2)
B <- torch_randn(4, 3, 2)
X <- linalg_tensorsolve(A, B, dims = c(1, 3))
A <- A$permute(c(2, 4, 5, 1, 3))
torch_allclose(torch_tensordot(A, X, dims = X$ndim), B, atol = 1e-6)
}
#> [1] TRUE
```
