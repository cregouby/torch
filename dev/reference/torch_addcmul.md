# Addcmul

Addcmul

## Usage

``` r
torch_addcmul(self, tensor1, tensor2, value = 1L)
```

## Arguments

- self:

  (Tensor) the tensor to be added

- tensor1:

  (Tensor) the tensor to be multiplied

- tensor2:

  (Tensor) the tensor to be multiplied

- value:

  (Number, optional) multiplier for \\tensor1 .\* tensor2\\

## addcmul(input, tensor1, tensor2, \*, value=1, out=NULL) -\> Tensor

Performs the element-wise multiplication of `tensor1` by `tensor2`,
multiply the result by the scalar `value` and add it to `input`.

\$\$ \mbox{out}\_i = \mbox{input}\_i + \mbox{value} \times
\mbox{tensor1}\_i \times \mbox{tensor2}\_i \$\$ The shapes of `tensor`,
`tensor1`, and `tensor2` must be broadcastable .

For inputs of type `FloatTensor` or `DoubleTensor`, `value` must be a
real number, otherwise an integer.

## Examples

``` r
if (torch_is_installed()) {

t = torch_randn(c(1, 3))
t1 = torch_randn(c(3, 1))
t2 = torch_randn(c(1, 3))
torch_addcmul(t, t1, t2, 0.1)
}
#> torch_tensor
#>  1.4098 -0.8883  0.5658
#>  1.3663 -0.7669  0.6084
#>  1.3409 -0.6959  0.6332
#> [ CPUFloatType{3,3} ]
```
