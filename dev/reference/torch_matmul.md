# Matmul

Matmul

## Usage

``` r
torch_matmul(self, other)
```

## Arguments

- self:

  (Tensor) the first tensor to be multiplied

- other:

  (Tensor) the second tensor to be multiplied

## Note

    The 1-dimensional dot product version of this function does not support an `out` parameter.

## matmul(input, other, out=NULL) -\> Tensor

Matrix product of two tensors.

The behavior depends on the dimensionality of the tensors as follows:

- If both tensors are 1-dimensional, the dot product (scalar) is
  returned.

- If both arguments are 2-dimensional, the matrix-matrix product is
  returned.

- If the first argument is 1-dimensional and the second argument is
  2-dimensional, a 1 is prepended to its dimension for the purpose of
  the matrix multiply. After the matrix multiply, the prepended
  dimension is removed.

- If the first argument is 2-dimensional and the second argument is
  1-dimensional, the matrix-vector product is returned.

- If both arguments are at least 1-dimensional and at least one argument
  is N-dimensional (where N \> 2), then a batched matrix multiply is
  returned. If the first argument is 1-dimensional, a 1 is prepended to
  its dimension for the purpose of the batched matrix multiply and
  removed after. If the second argument is 1-dimensional, a 1 is
  appended to its dimension for the purpose of the batched matrix
  multiple and removed after. The non-matrix (i.e. batch) dimensions are
  broadcasted (and thus must be broadcastable). For example, if `input`
  is a \\(j \times 1 \times n \times m)\\ tensor and `other` is a \\(k
  \times m \times p)\\ tensor, `out` will be an \\(j \times k \times n
  \times p)\\ tensor.

## Examples

``` r
if (torch_is_installed()) {

# vector x vector
tensor1 = torch_randn(c(3))
tensor2 = torch_randn(c(3))
torch_matmul(tensor1, tensor2)
# matrix x vector
tensor1 = torch_randn(c(3, 4))
tensor2 = torch_randn(c(4))
torch_matmul(tensor1, tensor2)
# batched matrix x broadcasted vector
tensor1 = torch_randn(c(10, 3, 4))
tensor2 = torch_randn(c(4))
torch_matmul(tensor1, tensor2)
# batched matrix x batched matrix
tensor1 = torch_randn(c(10, 3, 4))
tensor2 = torch_randn(c(10, 4, 5))
torch_matmul(tensor1, tensor2)
# batched matrix x broadcasted matrix
tensor1 = torch_randn(c(10, 3, 4))
tensor2 = torch_randn(c(4, 5))
torch_matmul(tensor1, tensor2)
}
#> torch_tensor
#> (1,.,.) = 
#>  -3.5365 -1.8727 -0.2462 -2.1914  1.6448
#>   1.0572  1.4390  1.0769  1.4004  0.0145
#>   0.3110  2.3201  2.7979  1.0997  1.0476
#> 
#> (2,.,.) = 
#>   0.6228  3.1554  3.4763  0.9272  0.1650
#>   3.7172  3.3896  2.5928  2.6954  0.1282
#>  -0.4307 -2.6196 -2.8153 -1.9591 -1.0113
#> 
#> (3,.,.) = 
#>  -1.5669 -0.7535  0.3055 -1.2742  1.2559
#>   0.9075  2.4672  3.2986  1.0941  2.0910
#>  -2.4457 -3.2790 -2.6619 -1.6708  0.8201
#> 
#> (4,.,.) = 
#>   0.3313  2.6220  2.3432  2.7483  0.5031
#>   1.7697  3.7316  3.8064  2.6361  1.2644
#>  -1.7316 -3.0962 -3.3210 -2.8758 -2.1930
#> 
#> (5,.,.) = 
#>  -2.4235 -4.0759 -5.0381 -2.8712 -3.7774
#>   2.5074  5.3678  4.4729  3.1385 -1.3010
#>   2.2464  3.7321  3.1475  1.3516 -1.3623
#> 
#> (6,.,.) = 
#>  -3.7421 -3.8820 -2.3186 -3.1164  1.6659
#>   0.3176  2.1106  2.8938  1.9092  2.6691
#>   0.3271 -0.9282 -1.5563  2.5838  1.4359
#> 
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{10,3,5} ]
```
