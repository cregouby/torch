# Conv_transpose1d

Conv_transpose1d

## Usage

``` r
torch_conv_transpose1d(
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

  input tensor of shape \\(\mbox{minibatch} , \mbox{in\\channels} ,
  iW)\\

- weight:

  filters of shape \\(\mbox{in\\channels} ,
  \frac{\mbox{out\\channels}}{\mbox{groups}} , kW)\\

- bias:

  optional bias of shape \\(\mbox{out\\channels})\\. Default: NULL

- stride:

  the stride of the convolving kernel. Can be a single number or a tuple
  `(sW,)`. Default: 1

- padding:

  `dilation * (kernel_size - 1) - padding` zero-padding will be added to
  both sides of each dimension in the input. Can be a single number or a
  tuple `(padW,)`. Default: 0

- output_padding:

  additional size added to one side of each dimension in the output
  shape. Can be a single number or a tuple `(out_padW)`. Default: 0

- groups:

  split input into groups, \\\mbox{in\\channels}\\ should be divisible
  by the number of groups. Default: 1

- dilation:

  the spacing between kernel elements. Can be a single number or a tuple
  `(dW,)`. Default: 1

## conv_transpose1d(input, weight, bias=NULL, stride=1, padding=0, output_padding=0, groups=1, dilation=1) -\> Tensor

Applies a 1D transposed convolution operator over an input signal
composed of several input planes, sometimes also called "deconvolution".

See
[`nn_conv_transpose1d()`](https://cregouby.github.io/torch/dev/reference/nn_conv_transpose1d.md)
for details and output shape.

## Examples

``` r
if (torch_is_installed()) {

inputs = torch_randn(c(20, 16, 50))
weights = torch_randn(c(16, 33, 5))
nnf_conv_transpose1d(inputs, weights)
}
#> torch_tensor
#> (1,.,.) = 
#>  Columns 1 to 8   1.2059  -7.7484  -7.5242 -17.7030  -3.1969   5.4189   1.3866 -10.3283
#>   -4.2174  -1.8283  -2.6671   4.9302   6.0166 -14.1320  23.7773  11.8100
#>   -2.8136   6.8659   3.6887  -0.0996   5.8889  12.5411  -5.6196 -10.9772
#>   -0.7567  -7.1764  -3.6188  18.7027  -4.1573   1.9051  -0.3479  12.8615
#>   -1.5184  -7.2339 -14.2617  -3.2746   5.6981  -5.6743   1.4289  18.3446
#>    5.9250  -1.8822  -6.4963  -7.4816   1.0012   2.7906  -0.8270  -9.7955
#>   -5.8367   0.2075   6.0531  15.8971  20.3515   2.7462   7.2076   8.6226
#>   -6.1234   2.2005   3.0830   9.4854  10.2756  13.6038 -10.2975   1.1707
#>    0.1245 -10.3326  -2.3601 -11.8095  -2.8488  21.1974 -15.3602   4.6763
#>   -1.2950  -3.3010   3.1487  -7.6877  -7.4431   5.4054   3.3810  11.5664
#>   -3.5132  -2.5779   5.0581  -1.0742   0.0564   6.6162   1.5024  -3.1026
#>   -1.2996  -8.7453  12.9066  -5.5706 -16.5850   8.3478  -8.7794  -7.3567
#>   -3.7675   1.6891   9.4248  -8.9404   2.4445  -7.0486 -13.3829  -8.8899
#>   -0.2128  -0.0579 -10.1651  -9.1487  10.1101  -5.1701   5.9767   3.1601
#>   -1.5972   2.2379  -4.6768  -5.3037  -8.2226  10.0714  -4.1928   1.6209
#>   -1.0280   5.3935   4.7014  -2.0472  -8.3870  -8.8970   2.9155   6.7654
#>   -5.6576  -2.6845   6.2118   4.4288  -1.6935  22.2476  -3.4270 -10.6488
#>    1.6288  -8.7038  -5.2851  -8.1049  -5.0325   4.1468   0.9965  19.7496
#>    3.1183   1.9531   2.3660   2.1067   2.9428  -1.9359   2.5892   9.3653
#>    2.5178  -4.9936  -2.7006  -3.0967 -11.7151  -1.5369   4.1439  -9.5027
#>    4.6742  -8.5392  -5.0137  -0.5770  -1.5957  11.6885   2.2825   2.1665
#>    2.0798  -3.1016  -0.6724  15.1464   5.5322   2.5350   0.2466  -2.4218
#>   -0.0722   3.1590  -1.7924 -11.0390   0.6802   0.6588  -5.5387  12.4483
#>    1.6145   0.8778  -2.4752 -14.1355   5.4285   1.9172  -4.8445   0.6049
#>   -1.3606 -12.3516   4.4189   3.4160   0.5684  18.3558   8.8678  -1.4484
#>   -0.4815  -4.7067   5.5157   2.3989   2.8459   8.3810  -9.5141  15.4903
#>   -0.4703  -3.6627  10.9260 -14.9033  -1.0552  11.3941   9.8190 -21.0820
#>   -4.4731   4.8518  -4.4172 -17.1152  -2.1071   6.1264  -4.4763 -24.6514
#>   -0.1251   4.9313   4.2759  -1.7051  -5.1651  -9.1573   5.4568  -5.1681
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{20,33,54} ]
```
