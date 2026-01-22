# Baddbmm

Baddbmm

## Usage

``` r
torch_baddbmm(self, batch1, batch2, beta = 1L, alpha = 1L)
```

## Arguments

- self:

  (Tensor) the tensor to be added

- batch1:

  (Tensor) the first batch of matrices to be multiplied

- batch2:

  (Tensor) the second batch of matrices to be multiplied

- beta:

  (Number, optional) multiplier for `input` (\\\beta\\)

- alpha:

  (Number, optional) multiplier for \\\mbox{batch1} \mathbin{@}
  \mbox{batch2}\\ (\\\alpha\\)

## baddbmm(input, batch1, batch2, \*, beta=1, alpha=1, out=NULL) -\> Tensor

Performs a batch matrix-matrix product of matrices in `batch1` and
`batch2`. `input` is added to the final result.

`batch1` and `batch2` must be 3-D tensors each containing the same
number of matrices.

If `batch1` is a \\(b \times n \times m)\\ tensor, `batch2` is a \\(b
\times m \times p)\\ tensor, then `input` must be broadcastable with a
\\(b \times n \times p)\\ tensor and `out` will be a \\(b \times n
\times p)\\ tensor. Both `alpha` and `beta` mean the same as the scaling
factors used in `torch_addbmm`.

\$\$ \mbox{out}\_i = \beta\\ \mbox{input}\_i + \alpha\\
(\mbox{batch1}\_i \mathbin{@} \mbox{batch2}\_i) \$\$ For inputs of type
`FloatTensor` or `DoubleTensor`, arguments `beta` and `alpha` must be
real numbers, otherwise they should be integers.

## Examples

``` r
if (torch_is_installed()) {

M = torch_randn(c(10, 3, 5))
batch1 = torch_randn(c(10, 3, 4))
batch2 = torch_randn(c(10, 4, 5))
torch_baddbmm(M, batch1, batch2)
}
#> torch_tensor
#> (1,.,.) = 
#>   2.7364  2.8371  0.2508 -8.3834  3.0192
#>   2.0202  2.9163 -0.3238 -3.1291  1.5977
#>   1.7723  4.6291 -0.0829 -5.3840  7.1647
#> 
#> (2,.,.) = 
#>  -1.0480 -1.4790 -0.4415 -0.4889  1.7070
#>  -3.1974  1.5755 -1.0913  2.8153 -1.4848
#>   0.6270 -3.4767 -0.9815  1.9012  3.8158
#> 
#> (3,.,.) = 
#>   1.5835  0.4489 -3.5600 -0.8893 -0.3087
#>  -0.2668 -0.2130  1.7169  0.3505 -0.3589
#>  -1.1629 -2.1194  3.9897  3.1600  0.7083
#> 
#> (4,.,.) = 
#>  -0.4905  0.0385 -3.8941 -3.1261  1.4626
#>   5.7657  2.2196  4.9869  3.1349 -0.4674
#>   0.0116 -0.3995  3.2481 -0.2624  0.8896
#> 
#> (5,.,.) = 
#>  -1.7363  2.2905 -0.0364 -0.1453 -3.5759
#>  -1.0487 -1.3490 -3.7982 -0.5948 -4.2886
#>   2.1482  5.6600  3.7796  0.2785 -4.4826
#> 
#> (6,.,.) = 
#>  -0.1925 -1.6898 -0.3234 -4.4103 -1.5411
#>   3.3524  1.1370 -6.3860  2.0914 -0.4908
#>  -0.5389 -1.7397  1.1211 -2.6232 -1.5042
#> 
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{10,3,5} ]
```
