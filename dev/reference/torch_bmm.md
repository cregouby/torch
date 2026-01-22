# Bmm

Bmm

## Usage

``` r
torch_bmm(self, mat2)
```

## Arguments

- self:

  (Tensor) the first batch of matrices to be multiplied

- mat2:

  (Tensor) the second batch of matrices to be multiplied

## Note

This function does not broadcast . For broadcasting matrix products, see
[`torch_matmul`](https://cregouby.github.io/torch/dev/reference/torch_matmul.md).

## bmm(input, mat2, out=NULL) -\> Tensor

Performs a batch matrix-matrix product of matrices stored in `input` and
`mat2`.

`input` and `mat2` must be 3-D tensors each containing the same number
of matrices.

If `input` is a \\(b \times n \times m)\\ tensor, `mat2` is a \\(b
\times m \times p)\\ tensor, `out` will be a \\(b \times n \times p)\\
tensor.

\$\$ \mbox{out}\_i = \mbox{input}\_i \mathbin{@} \mbox{mat2}\_i \$\$

## Examples

``` r
if (torch_is_installed()) {

input = torch_randn(c(10, 3, 4))
mat2 = torch_randn(c(10, 4, 5))
res = torch_bmm(input, mat2)
res
}
#> torch_tensor
#> (1,.,.) = 
#>   2.1812 -1.5808  2.8638  1.7387 -1.3121
#>  -1.0848 -4.4464 -4.9711 -1.9544 -0.1024
#>  -0.1478  3.5107  2.1781  0.6777  0.5422
#> 
#> (2,.,.) = 
#>  -1.8422  0.0134  0.2337  0.9960 -0.6883
#>   0.3511 -0.4587 -0.1397  0.6448 -0.9997
#>  -0.1518 -1.9713  0.5466  0.7427  0.6922
#> 
#> (3,.,.) = 
#>  -0.4698  2.2796 -3.1508  2.9401  1.6232
#>  -2.1174  1.4913 -3.1132  1.0020 -0.1556
#>   1.2492 -2.0754  5.5102 -2.2153 -1.7911
#> 
#> (4,.,.) = 
#>   0.5186 -0.4245 -0.6697  3.1912 -1.1212
#>  -1.0121  2.3120 -1.2606  1.2685  0.1612
#>  -1.1546  1.1345 -0.5148  0.6169  0.1122
#> 
#> (5,.,.) = 
#>  -0.0625 -2.0549  2.3297 -0.4301 -0.7883
#>   0.2435 -1.8234 -3.5054  2.1952 -0.4549
#>   1.9820 -2.4007  1.3932  0.8522 -1.3181
#> 
#> (6,.,.) = 
#>  -1.1538 -0.2177  0.7648 -1.9871  0.4090
#>   0.1369  0.3652 -1.3935 -2.3436  0.7055
#>   2.2262  1.4786  2.0509  5.0400  1.5360
#> 
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{10,3,5} ]
```
