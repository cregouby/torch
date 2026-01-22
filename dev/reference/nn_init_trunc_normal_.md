# Truncated normal initialization

Fills the input Tensor with values drawn from a truncated normal
distribution.

## Usage

``` r
nn_init_trunc_normal_(tensor, mean = 0, std = 1, a = -2, b = 2)
```

## Arguments

- tensor:

  an n-dimensional Tensor

- mean:

  the mean of the normal distribution

- std:

  the standard deviation of the normal distribution

- a:

  the minimum cutoff value

- b:

  the maximum cutoff value

## Examples

``` r
if (torch_is_installed()) {
w <- torch_empty(3, 5)
nn_init_trunc_normal_(w)
}
#> torch_tensor
#> -1.1443  0.9454 -0.0305 -1.6211  0.3748
#>  0.4009  0.6359  0.2332  1.1480  0.7229
#>  1.9432  1.8347 -1.2006 -1.3518 -1.2122
#> [ CPUFloatType{3,5} ]
```
