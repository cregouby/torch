# Computes the singular values of a matrix.

Supports input of float, double, cfloat and cdouble dtypes. Also
supports batches of matrices, and if `A` is a batch of matrices then the
output has the same batch dimensions. The singular values are returned
in descending order.

## Usage

``` r
linalg_svdvals(A)
```

## Arguments

- A:

  (Tensor): tensor of shape `(*, m, n)` where `*` is zero or more batch
  dimensions.

## Value

A real-valued tensor, even when `A` is complex.

## See also

[`linalg_svd()`](https://cregouby.github.io/torch/dev/reference/linalg_svd.md)
computes the full singular value decomposition.

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
[`linalg_tensorinv()`](https://cregouby.github.io/torch/dev/reference/linalg_tensorinv.md),
[`linalg_tensorsolve()`](https://cregouby.github.io/torch/dev/reference/linalg_tensorsolve.md),
[`linalg_vector_norm()`](https://cregouby.github.io/torch/dev/reference/linalg_vector_norm.md)

## Examples

``` r
if (torch_is_installed()) {
A <- torch_randn(5, 3)
S <- linalg_svdvals(A)
S
}
#> torch_tensor
#>  5.7100
#>  2.4039
#>  0.9428
#> [ CPUFloatType{3} ]
```
