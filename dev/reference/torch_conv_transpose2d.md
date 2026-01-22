# Conv_transpose2d

Conv_transpose2d

## Usage

``` r
torch_conv_transpose2d(
  input,
  weight,
  bias = list(),
  stride = 1L,
  padding = 0L,
  output_padding = 0L,
  groups = 1L,
  dilation = 1L
)
```

## Arguments

- input:

  input tensor of shape \\(\mbox{minibatch} , \mbox{in\\channels} , iH ,
  iW)\\

- weight:

  filters of shape \\(\mbox{in\\channels} ,
  \frac{\mbox{out\\channels}}{\mbox{groups}} , kH , kW)\\

- bias:

  optional bias of shape \\(\mbox{out\\channels})\\. Default: NULL

- stride:

  the stride of the convolving kernel. Can be a single number or a tuple
  `(sH, sW)`. Default: 1

- padding:

  `dilation * (kernel_size - 1) - padding` zero-padding will be added to
  both sides of each dimension in the input. Can be a single number or a
  tuple `(padH, padW)`. Default: 0

- output_padding:

  additional size added to one side of each dimension in the output
  shape. Can be a single number or a tuple `(out_padH, out_padW)`.
  Default: 0

- groups:

  split input into groups, \\\mbox{in\\channels}\\ should be divisible
  by the number of groups. Default: 1

- dilation:

  the spacing between kernel elements. Can be a single number or a tuple
  `(dH, dW)`. Default: 1

## conv_transpose2d(input, weight, bias=NULL, stride=1, padding=0, output_padding=0, groups=1, dilation=1) -\> Tensor

Applies a 2D transposed convolution operator over an input image
composed of several input planes, sometimes also called "deconvolution".

See
[`nn_conv_transpose2d()`](https://cregouby.github.io/torch/dev/reference/nn_conv_transpose2d.md)
for details and output shape.

## Examples

``` r
if (torch_is_installed()) {

# With square kernels and equal stride
inputs = torch_randn(c(1, 4, 5, 5))
weights = torch_randn(c(4, 8, 3, 3))
nnf_conv_transpose2d(inputs, weights, padding=1)
}
#> torch_tensor
#> (1,1,.,.) = 
#>    3.4254   2.2774  -6.5747   1.1062   1.9991
#>   -0.0150  15.4708  -9.4999  -8.2066   6.0233
#>  -10.3216   3.0975   6.5459  -5.6879  -4.7293
#>  -10.7171  -2.0623  -5.0262   1.6488   1.9998
#>   -1.4499   0.3116  -4.2691  -1.4121   0.4517
#> 
#> (1,2,.,.) = 
#>    0.2699  12.1088  -0.1838   0.5935   2.2500
#>    1.2761   2.4576  -1.6288   9.1189  -1.9689
#>    5.1371   6.7329   1.0677  -2.1740   0.8493
#>  -11.2528 -10.6877   9.0617   9.7789  -8.7050
#>   -8.4418 -10.0726  -3.8667   3.6241   7.4990
#> 
#> (1,3,.,.) = 
#>    0.4422  -0.2761   5.2912  -0.7918   0.1837
#>    4.3272  -4.1234 -11.9677   1.7883   2.7535
#>    1.6875  12.0331   5.4122  -2.0173   1.0685
#>   -6.5783   7.7451   3.8101  -6.6928  -1.2671
#>   -2.3280   3.6617   0.7048   1.7590  -3.3586
#> 
#> (1,4,.,.) = 
#>   -4.2832   3.1436  -1.0443  -0.6713   1.8924
#>    3.1942  10.2712   3.5938 -10.1210   1.2684
#>   -1.0268  -3.7637  -3.6578   0.3285   1.3568
#>    4.7761   1.2153  19.0026  -5.0947  -5.3441
#>   -6.2463  -8.0360  10.5003   0.0714  -5.5605
#> 
#> (1,5,.,.) = 
#>  -5.3428  5.0610  5.0429  6.2285 -3.2735
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{1,8,5,5} ]
```
