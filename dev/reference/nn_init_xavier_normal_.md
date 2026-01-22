# Xavier normal initialization

Fills the input `Tensor` with values according to the method described
in
`Understanding the difficulty of training deep feedforward neural networks` -
Glorot, X. & Bengio, Y. (2010), using a normal distribution.

## Usage

``` r
nn_init_xavier_normal_(tensor, gain = 1)
```

## Arguments

- tensor:

  an n-dimensional `Tensor`

- gain:

  an optional scaling factor

## Examples

``` r
if (torch_is_installed()) {
w <- torch_empty(3, 5)
nn_init_xavier_normal_(w)
}
#> torch_tensor
#>  0.0426  0.4278 -0.5795 -0.8625  0.2233
#>  0.0307  0.3594  0.2841  0.2825 -0.9693
#> -0.3535 -0.2369  0.6070 -0.1858 -0.6629
#> [ CPUFloatType{3,5} ]
```
