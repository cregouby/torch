# Normal initialization

Fills the input Tensor with values drawn from the normal distribution

## Usage

``` r
nn_init_normal_(tensor, mean = 0, std = 1)
```

## Arguments

- tensor:

  an n-dimensional Tensor

- mean:

  the mean of the normal distribution

- std:

  the standard deviation of the normal distribution

## Examples

``` r
if (torch_is_installed()) {
w <- torch_empty(3, 5)
nn_init_normal_(w)
}
#> torch_tensor
#>  0.8635  0.9955 -0.3135  1.8547  0.5342
#>  1.4781 -0.9697 -0.3508 -0.9785  1.2885
#> -0.1718  2.3967  0.0834 -0.7305 -0.3255
#> [ CPUFloatType{3,5} ]
```
