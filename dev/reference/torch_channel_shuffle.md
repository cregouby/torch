# Channel_shuffle

Channel_shuffle

## Usage

``` r
torch_channel_shuffle(self, groups)
```

## Arguments

- self:

  (Tensor) the input tensor

- groups:

  (int) number of groups to divide channels in and rearrange.

## Divide the channels in a tensor of shape

math:`(*, C , H, W)` :

Divide the channels in a tensor of shape \\(\*, C , H, W)\\ into g
groups and rearrange them as \\(\*, C \frac g, g, H, W)\\, while keeping
the original tensor shape.

## Examples

``` r
if (torch_is_installed()) {

input <- torch_randn(c(1, 4, 2, 2))
print(input)
output <- torch_channel_shuffle(input, 2)
print(output)
}
#> torch_tensor
#> (1,1,.,.) = 
#>   1.0428  0.3784
#>  -0.1136 -0.4347
#> 
#> (1,2,.,.) = 
#>  -0.6438  1.0234
#>  -0.9056 -0.0598
#> 
#> (1,3,.,.) = 
#>   1.1235 -1.5232
#>   0.2899 -0.1961
#> 
#> (1,4,.,.) = 
#>  -2.0979 -0.8875
#>  -0.9219  1.4589
#> [ CPUFloatType{1,4,2,2} ]
#> torch_tensor
#> (1,1,.,.) = 
#>   1.0428  0.3784
#>  -0.1136 -0.4347
#> 
#> (1,2,.,.) = 
#>   1.1235 -1.5232
#>   0.2899 -0.1961
#> 
#> (1,3,.,.) = 
#>  -0.6438  1.0234
#>  -0.9056 -0.0598
#> 
#> (1,4,.,.) = 
#>  -2.0979 -0.8875
#>  -0.9219  1.4589
#> [ CPUFloatType{1,4,2,2} ]
```
