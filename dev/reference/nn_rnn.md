# RNN module

Applies a multi-layer Elman RNN with \\\tanh\\ or \\\mbox{ReLU}\\
non-linearity to an input sequence.

## Usage

``` r
nn_rnn(
  input_size,
  hidden_size,
  num_layers = 1,
  nonlinearity = NULL,
  bias = TRUE,
  batch_first = FALSE,
  dropout = 0,
  bidirectional = FALSE,
  ...
)
```

## Arguments

- input_size:

  The number of expected features in the input `x`

- hidden_size:

  The number of features in the hidden state `h`

- num_layers:

  Number of recurrent layers. E.g., setting `num_layers=2` would mean
  stacking two RNNs together to form a `stacked RNN`, with the second
  RNN taking in outputs of the first RNN and computing the final
  results. Default: 1

- nonlinearity:

  The non-linearity to use. Can be either `'tanh'` or `'relu'`. Default:
  `'tanh'`

- bias:

  If `FALSE`, then the layer does not use bias weights `b_ih` and
  `b_hh`. Default: `TRUE`

- batch_first:

  If `TRUE`, then the input and output tensors are provided as
  `(batch, seq, feature)`. Default: `FALSE`

- dropout:

  If non-zero, introduces a `Dropout` layer on the outputs of each RNN
  layer except the last layer, with dropout probability equal to
  `dropout`. Default: 0

- bidirectional:

  If `TRUE`, becomes a bidirectional RNN. Default: `FALSE`

- ...:

  other arguments that can be passed to the super class.

## Details

For each element in the input sequence, each layer computes the
following function:

\$\$ h_t = \tanh(W\_{ih} x_t + b\_{ih} + W\_{hh} h\_{(t-1)} + b\_{hh})
\$\$

where \\h_t\\ is the hidden state at time `t`, \\x_t\\ is the input at
time `t`, and \\h\_{(t-1)}\\ is the hidden state of the previous layer
at time `t-1` or the initial hidden state at time `0`. If `nonlinearity`
is `'relu'`, then \\\mbox{ReLU}\\ is used instead of \\\tanh\\.

## Inputs

- **input** of shape `(seq_len, batch, input_size)`: tensor containing
  the features of the input sequence. The input can also be a packed
  variable length sequence.

- **h_0** of shape `(num_layers * num_directions, batch, hidden_size)`:
  tensor containing the initial hidden state for each element in the
  batch. Defaults to zero if not provided. If the RNN is bidirectional,
  num_directions should be 2, else it should be 1.

## Outputs

- **output** of shape `(seq_len, batch, num_directions * hidden_size)`:
  tensor containing the output features (`h_t`) from the last layer of
  the RNN, for each `t`. If a :class:`nn_packed_sequence` has been given
  as the input, the output will also be a packed sequence. For the
  unpacked case, the directions can be separated using
  `output$view(seq_len, batch, num_directions, hidden_size)`, with
  forward and backward being direction `0` and `1` respectively.
  Similarly, the directions can be separated in the packed case.

- **h_n** of shape `(num_layers * num_directions, batch, hidden_size)`:
  tensor containing the hidden state for `t = seq_len`. Like *output*,
  the layers can be separated using
  `h_n$view(num_layers, num_directions, batch, hidden_size)`.

## Shape

- Input1: \\(L, N, H\_{in})\\ tensor containing input features where
  \\H\_{in}=\mbox{input\\size}\\ and `L` represents a sequence length.

- Input2: \\(S, N, H\_{out})\\ tensor containing the initial hidden
  state for each element in the batch. \\H\_{out}=\mbox{hidden\\size}\\
  Defaults to zero if not provided. where \\S=\mbox{num\\layers} \*
  \mbox{num\\directions}\\ If the RNN is bidirectional, num_directions
  should be 2, else it should be 1.

- Output1: \\(L, N, H\_{all})\\ where \\H\_{all}=\mbox{num\\directions}
  \* \mbox{hidden\\size}\\

- Output2: \\(S, N, H\_{out})\\ tensor containing the next hidden state
  for each element in the batch

## Attributes

- `weight_ih_l[k]`: the learnable input-hidden weights of the k-th
  layer, of shape `(hidden_size, input_size)` for `k = 0`. Otherwise,
  the shape is `(hidden_size, num_directions * hidden_size)`

- `weight_hh_l[k]`: the learnable hidden-hidden weights of the k-th
  layer, of shape `(hidden_size, hidden_size)`

- `bias_ih_l[k]`: the learnable input-hidden bias of the k-th layer, of
  shape `(hidden_size)`

- `bias_hh_l[k]`: the learnable hidden-hidden bias of the k-th layer, of
  shape `(hidden_size)`

## Note

All the weights and biases are initialized from \\\mathcal{U}(-\sqrt{k},
\sqrt{k})\\ where \\k = \frac{1}{\mbox{hidden\\size}}\\

## Examples

``` r
if (torch_is_installed()) {
rnn <- nn_rnn(10, 20, 2)
input <- torch_randn(5, 3, 10)
h0 <- torch_randn(2, 3, 20)
rnn(input, h0)
}
#> [[1]]
#> torch_tensor
#> (1,.,.) = 
#>  Columns 1 to 9 -0.3555  0.4771  0.5386 -0.8385  0.1889 -0.9473 -0.5210 -0.2487 -0.0541
#>   0.6720 -0.4605 -0.6424 -0.0946  0.2838  0.1854  0.5000 -0.2369 -0.1453
#>  -0.4803  0.6478 -0.6261  0.7281 -0.5697 -0.6865 -0.6475  0.2744  0.3995
#> 
#> Columns 10 to 18 -0.6705  0.7790  0.2014  0.5670 -0.7282 -0.8572  0.0746 -0.5719 -0.9550
#>   0.8720 -0.4986 -0.4406 -0.6093 -0.5630  0.7949  0.4174  0.3876  0.0644
#>  -0.5379 -0.2426 -0.2864  0.7425 -0.7099  0.9216  0.3988 -0.9431 -0.7903
#> 
#> Columns 19 to 20  0.7738  0.8120
#>   0.5393  0.1757
#>  -0.8209 -0.5569
#> 
#> (2,.,.) = 
#>  Columns 1 to 9 -0.8100  0.2648  0.1347 -0.5475  0.4430 -0.7235  0.0642  0.3342  0.3395
#>   0.4810  0.0667 -0.4766  0.2240  0.1321  0.5023 -0.2687  0.4448  0.2498
#>   0.5679 -0.4243 -0.3638 -0.2184  0.0258 -0.2699 -0.6062  0.6306  0.6763
#> 
#> Columns 10 to 18 -0.3531  0.1642  0.0209 -0.0045 -0.4190  0.2801 -0.3458 -0.2200 -0.7888
#>   0.1108 -0.0995 -0.5460  0.2452  0.1027  0.4329 -0.0707  0.4283 -0.1246
#>  -0.6000  0.1805 -0.3699  0.0236  0.0649 -0.5009  0.4191  0.4116 -0.6687
#> 
#> Columns 19 to 20  0.5327  0.2762
#>  -0.3875  0.3574
#>   0.4085  0.3700
#> 
#> (3,.,.) = 
#>  Columns 1 to 9 -0.0978  0.2738 -0.1937 -0.3794  0.0470  0.0162  0.2237  0.1356  0.6034
#>   0.6275 -0.4022 -0.5518 -0.4923  0.2450  0.0173 -0.3343 -0.0337  0.2312
#>   0.5478 -0.1462 -0.2840 -0.2487  0.2812 -0.0500 -0.1356  0.3778  0.1267
#> ... [the output was truncated (use n=-1 to disable)]
#> [ CPUFloatType{5,3,20} ][ grad_fn = <StackBackward0> ]
#> 
#> [[2]]
#> torch_tensor
#> (1,.,.) = 
#>  Columns 1 to 9 -0.0449  0.1774  0.3724  0.3238  0.4089 -0.5059  0.6239  0.1749 -0.0797
#>   0.0186 -0.3631  0.6086  0.1217  0.7192  0.5171 -0.2047  0.5783  0.1110
#>  -0.2392 -0.4074  0.0202  0.2061 -0.4127  0.5556 -0.4723  0.2793  0.1819
#> 
#> Columns 10 to 18  0.0544  0.2464  0.6899  0.0362 -0.4529  0.2044 -0.2719 -0.6412 -0.3890
#>   0.4794  0.4954  0.3746 -0.1756  0.4053 -0.2816  0.6101 -0.3240  0.1384
#>   0.0821 -0.2334 -0.1222 -0.6252  0.5533 -0.6741  0.6525  0.2861  0.2166
#> 
#> Columns 19 to 20 -0.4922 -0.4114
#>   0.0036  0.4678
#>   0.2711 -0.0437
#> 
#> (2,.,.) = 
#>  Columns 1 to 9  0.1340  0.4347 -0.2065 -0.1951 -0.1651 -0.4457 -0.0022  0.1669  0.3705
#>   0.7716 -0.1028 -0.5738 -0.1247  0.0899  0.3646 -0.2133  0.6109  0.4648
#>   0.6311 -0.2154 -0.6607 -0.0246  0.2288  0.2142  0.2315  0.1493  0.2687
#> 
#> Columns 10 to 18  0.2221  0.0995  0.2621 -0.0648 -0.5427  0.4495 -0.5507  0.0754 -0.2450
#>  -0.4223 -0.2238  0.3828 -0.1186 -0.6003  0.0223  0.1919  0.2521 -0.4470
#>  -0.4620 -0.5110  0.1724 -0.2528 -0.1546 -0.3212  0.0720  0.4126 -0.3411
#> 
#> Columns 19 to 20  0.4179  0.6934
#>   0.1363  0.3721
#>   0.1117  0.0924
#> [ CPUFloatType{2,3,20} ][ grad_fn = <StackBackward0> ]
#> 
```
