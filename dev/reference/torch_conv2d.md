# Conv2d

Conv2d

## Usage

``` r
torch_conv2d(
  input,
  weight,
  bias = list(),
  stride = 1L,
  padding = 0L,
  dilation = 1L,
  groups = 1L
)
```

## Arguments

- input:

  input tensor of shape \\(\mbox{minibatch} , \mbox{in\\channels} , iH ,
  iW)\\

- weight:

  filters of shape \\(\mbox{out\\channels} ,
  \frac{\mbox{in\\channels}}{\mbox{groups}} , kH , kW)\\

- bias:

  optional bias tensor of shape \\(\mbox{out\\channels})\\. Default:
  `NULL`

- stride:

  the stride of the convolving kernel. Can be a single number or a tuple
  `(sH, sW)`. Default: 1

- padding:

  implicit paddings on both sides of the input. Can be a single number
  or a tuple `(padH, padW)`. Default: 0

- dilation:

  the spacing between kernel elements. Can be a single number or a tuple
  `(dH, dW)`. Default: 1

- groups:

  split input into groups, \\\mbox{in\\channels}\\ should be divisible
  by the number of groups. Default: 1

## conv2d(input, weight, bias=NULL, stride=1, padding=0, dilation=1, groups=1) -\> Tensor

Applies a 2D convolution over an input image composed of several input
planes.

See
[`nn_conv2d()`](https://cregouby.github.io/torch/dev/reference/nn_conv2d.md)
for details and output shape.

## Examples

``` r
if (torch_is_installed()) {

# With square kernels and equal stride
filters = torch_randn(c(8,4,3,3))
inputs = torch_randn(c(1,4,5,5))
nnf_conv2d(inputs, filters, padding=1)
}
#> torch_tensor
#> (1,1,.,.) = 
#>  -1.5230  1.7668 -0.0654 -0.4138  2.2065
#>   2.4970 -3.5101 -1.5984  3.4416 -4.1708
#>   6.6242  0.4741  2.5819  1.9278  4.2386
#>  -0.1396 -2.0355 -7.1703 -3.0617  1.0763
#>  -2.2207 -3.7271  1.1662  5.5941 -7.0022
#> 
#> (1,2,.,.) = 
#>  -4.4357  5.4379 -0.8905  2.2181  1.2637
#>  -8.7417  3.2319 -6.8466  5.4765  3.4941
#>   4.0105  2.6637  7.8180 -11.3100 -0.1577
#>  -6.6622 -2.7099  2.7364  1.6380  0.0999
#>  -0.8134  0.8670 -4.8620 -7.1084  4.4242
#> 
#> (1,3,.,.) = 
#>  -1.8119 -1.8179  4.6699  2.9159  3.2446
#>  -2.5676  3.6727  2.8945  4.1250 -2.3794
#>  -2.5390 -14.1635  8.1876  2.8294 -0.1165
#>  -0.0231 -4.5889 -1.7700  2.1208 -1.2626
#>   1.8193  0.7797  4.7317  1.8067 -2.0702
#> 
#> (1,4,.,.) = 
#>   0.5386 -5.8446  4.2200  1.2964 -5.5175
#>   7.6187  4.3093  5.0252 -5.3392 -0.7105
#>  -9.1418 -3.3400  1.4862  8.6663 -0.5981
#>  -1.8401 -5.9986  3.4926  1.3314  0.7587
#>  -1.5064 -5.4209 -4.0403  2.5362 -3.5284
#> 
#> (1,5,.,.) = 
#>   -1.5872  -5.8144   1.0798   4.5583  -0.0277
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{1,8,5,5} ]
```
