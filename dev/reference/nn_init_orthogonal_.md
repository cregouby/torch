# Orthogonal initialization

Fills the input `Tensor` with a (semi) orthogonal matrix, as described
in
`Exact solutions to the nonlinear dynamics of learning in deep linear neural networks` -
Saxe, A. et al. (2013). The input tensor must have at least 2
dimensions, and for tensors with more than 2 dimensions the trailing
dimensions are flattened.

## Usage

``` r
nn_init_orthogonal_(tensor, gain = 1)
```

## Arguments

- tensor:

  an n-dimensional `Tensor`

- gain:

  optional scaling factor

## Examples

``` r
if (torch_is_installed()) {
w <- torch_empty(3, 5)
nn_init_orthogonal_(w)
}
#> torch_tensor
#> -0.3798  0.6302 -0.6410 -0.2140  0.0429
#> -0.3736  0.5028  0.6343  0.1586 -0.4244
#> -0.1088  0.2393  0.2515  0.3199  0.8748
#> [ CPUFloatType{3,5} ]
```
