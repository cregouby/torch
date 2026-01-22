# Computes the QR decomposition of a matrix.

Letting be or , the **full QR decomposition** of a matrix is defined as

## Usage

``` r
linalg_qr(A, mode = "reduced")
```

## Arguments

- A:

  (Tensor): tensor of shape `(*, m, n)` where `*` is zero or more batch
  dimensions.

- mode:

  (str, optional): one of `'reduced'`, `'complete'`, `'r'`. Controls the
  shape of the returned tensors. Default: `'reduced'`.

## Value

A list `(Q, R)`.

## Details

Equation not displayed. Install 'katex' then re-install 'torch'.

where is orthogonal in the real case and unitary in the complex case,
and is upper triangular. When `m > n` (tall matrix), as `R` is upper
triangular, its last `m - n` rows are zero. In this case, we can drop
the last `m - n` columns of `Q` to form the **reduced QR
decomposition**:

Equation not displayed. Install 'katex' then re-install 'torch'.

The reduced QR decomposition agrees with the full QR decomposition when
`n >= m` (wide matrix). Supports input of float, double, cfloat and
cdouble dtypes. Also supports batches of matrices, and if `A` is a batch
of matrices then the output has the same batch dimensions. The parameter
`mode` chooses between the full and reduced QR decomposition.

If `A` has shape `(*, m, n)`, denoting `k = min(m, n)`

- `mode = 'reduced'` (default): Returns `(Q, R)` of shapes `(*, m, k)`,
  `(*, k, n)` respectively.

- `mode = 'complete'`: Returns `(Q, R)` of shapes `(*, m, m)`,
  `(*, m, n)` respectively.

- `mode = 'r'`: Computes only the reduced `R`. Returns `(Q, R)` with `Q`
  empty and `R` of shape `(*, k, n)`.

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
[`linalg_multi_dot()`](https://cregouby.github.io/torch/dev/reference/linalg_multi_dot.md),
[`linalg_norm()`](https://cregouby.github.io/torch/dev/reference/linalg_norm.md),
[`linalg_pinv()`](https://cregouby.github.io/torch/dev/reference/linalg_pinv.md),
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
a <- torch_tensor(rbind(c(12., -51, 4), c(6, 167, -68), c(-4, 24, -41)))
qr <- linalg_qr(a)

torch_mm(qr[[1]], qr[[2]])$round()
torch_mm(qr[[1]]$t(), qr[[1]])$round()
}
#> torch_tensor
#>  1 -0  0
#> -0  1  0
#>  0  0  1
#> [ CPUFloatType{3,3} ]
```
