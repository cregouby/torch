# Conv1d

Conv1d

## Usage

``` r
torch_conv1d(
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

  input tensor of shape \\(\mbox{minibatch} , \mbox{in\\channels} ,
  iW)\\

- weight:

  filters of shape \\(\mbox{out\\channels} ,
  \frac{\mbox{in\\channels}}{\mbox{groups}} , kW)\\

- bias:

  optional bias of shape \\(\mbox{out\\channels})\\. Default: `NULL`

- stride:

  the stride of the convolving kernel. Can be a single number or a
  one-element tuple `(sW,)`. Default: 1

- padding:

  implicit paddings on both sides of the input. Can be a single number
  or a one-element tuple `(padW,)`. Default: 0

- dilation:

  the spacing between kernel elements. Can be a single number or a
  one-element tuple `(dW,)`. Default: 1

- groups:

  split input into groups, \\\mbox{in\\channels}\\ should be divisible
  by the number of groups. Default: 1

## conv1d(input, weight, bias=NULL, stride=1, padding=0, dilation=1, groups=1) -\> Tensor

Applies a 1D convolution over an input signal composed of several input
planes.

See
[`nn_conv1d()`](https://cregouby.github.io/torch/dev/reference/nn_conv1d.md)
for details and output shape.

## Examples

``` r
if (torch_is_installed()) {

filters = torch_randn(c(33, 16, 3))
inputs = torch_randn(c(20, 16, 50))
nnf_conv1d(inputs, filters)
}
#> torch_tensor
#> (1,.,.) = 
#>  Columns 1 to 6  2.1982e+00  2.7440e+00  1.3536e+00  1.1962e+01 -4.8202e+00 -1.5131e+01
#>   7.9756e+00 -6.2728e-01 -6.9085e+00  1.0184e+01  1.2748e+00 -8.0004e+00
#>   2.9388e+00 -9.6058e-01  2.2950e+00  1.3961e+01 -8.3971e+00  4.9530e+00
#>  -1.9820e+00  2.1771e+00 -3.7570e+00 -3.6007e+00 -1.2778e+01 -6.0813e+00
#>  -2.0419e+01  3.1149e+00 -6.0490e+00 -5.0154e+00  1.5569e+01 -4.7844e+00
#>   3.3044e+00  1.2368e+01  5.0741e+00  1.6903e+00 -1.9781e+00 -1.6369e+00
#>   5.8970e-01 -1.9930e+00  2.6515e+00  4.6733e+00 -2.0266e+01 -2.7047e+00
#>  -7.9739e+00 -6.1592e+00 -6.8854e+00  3.4756e-01  9.7303e+00 -2.9541e+00
#>  -1.3016e+01 -1.5615e+00  2.6022e+00  1.0719e+00  4.7899e+00 -1.7861e+00
#>  -1.4370e+01  2.4513e+00 -1.1900e+01 -5.5326e+00 -3.9159e+00  8.7506e+00
#>   2.9989e+00  1.7004e+01  5.5604e+00 -5.1921e-01 -1.2008e+01 -5.3257e+00
#>   4.7444e+00 -9.4822e+00  5.4486e+00 -2.8194e+00 -6.9295e+00 -3.6551e+00
#>  -9.5339e-02 -8.4736e+00 -5.8785e+00 -1.1007e+01 -4.9013e-01 -4.6383e+00
#>  -8.5230e+00  1.4373e+01  4.3376e+00 -4.8879e+00 -5.5394e+00  3.2283e+00
#>   3.2312e+00  5.4811e+00  1.4939e+01 -5.9470e+00 -4.8641e+00 -4.8325e+00
#>  -2.3971e+00 -7.5532e+00 -2.1825e+00 -5.7073e+00  5.1662e+00  2.3887e+00
#>   8.6159e+00 -7.2407e+00 -4.3988e+00  5.5438e+00  1.0977e-01  6.2644e+00
#>   2.5571e+00  2.3851e+00  3.9986e-01 -7.7355e+00 -2.1969e+00  5.5187e+00
#>  -1.0097e+01 -1.2153e+01 -3.4119e+00  2.4375e+00 -3.6611e+00  1.0369e+00
#>   5.8033e-02 -5.8682e+00  7.2110e-04 -2.4398e+00  1.9455e+00  1.2592e+01
#>   3.9000e+00  8.2486e+00  5.0256e+00 -1.0168e+01  3.2120e+00 -1.9753e+00
#>  -1.0589e+01  5.7383e+00  1.8396e+00 -4.9883e+00  9.2911e-01  1.8230e+00
#>  -3.6458e-01 -3.8584e+00  6.6872e+00  2.4930e+00 -6.4309e+00  5.3107e+00
#>   6.1033e+00 -1.5013e+01  1.2205e-01 -5.8112e-01 -4.0099e+00 -5.0259e-02
#>  -2.1827e+00  1.4603e+01  8.8962e-01 -9.5394e+00 -3.0421e+00 -9.1882e+00
#>   1.0039e+01 -1.6171e-01 -1.4462e+00  7.3602e-01 -2.5916e-01  5.3570e+00
#>   5.8794e+00  1.1771e+01  1.5518e+00 -2.8564e+00  1.1686e+01 -5.8354e+00
#>   2.9593e+00  4.4943e+00  8.5047e+00  2.3992e+00  7.0555e+00  6.2074e+00
#>  -4.3158e+00 -5.4156e+00 -1.3689e+01 -4.6016e+00 -3.3076e+00 -3.5237e+00
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{20,33,48} ]
```
