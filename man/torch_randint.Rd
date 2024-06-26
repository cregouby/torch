% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creation-ops.R, R/gen-namespace-docs.R,
%   R/gen-namespace-examples.R
\name{torch_randint}
\alias{torch_randint}
\title{Randint}
\usage{
torch_randint(
  low,
  high,
  size,
  generator = NULL,
  dtype = NULL,
  layout = NULL,
  device = NULL,
  requires_grad = FALSE,
  memory_format = torch_preserve_format()
)
}
\arguments{
\item{low}{(int, optional) Lowest integer to be drawn from the distribution. Default: 0.}

\item{high}{(int) One above the highest integer to be drawn from the distribution.}

\item{size}{(tuple) a tuple defining the shape of the output tensor.}

\item{generator}{(\code{torch.Generator}, optional) a pseudorandom number generator for sampling}

\item{dtype}{(\code{torch.dtype}, optional) the desired data type of returned tensor.        Default: if \code{NULL}, uses a global default (see \code{torch_set_default_tensor_type}).}

\item{layout}{(\code{torch.layout}, optional) the desired layout of returned Tensor.        Default: \code{torch_strided}.}

\item{device}{(\code{torch.device}, optional) the desired device of returned tensor.        Default: if \code{NULL}, uses the current device for the default tensor type        (see \code{torch_set_default_tensor_type}). \code{device} will be the CPU        for CPU tensor types and the current CUDA device for CUDA tensor types.}

\item{requires_grad}{(bool, optional) If autograd should record operations on the        returned tensor. Default: \code{FALSE}.}

\item{memory_format}{memory format for the resulting tensor.}
}
\description{
Randint
}
\section{randint(low=0, high, size, *, generator=NULL, out=NULL, \ }{


dtype=NULL, layout=torch.strided, device=NULL, requires_grad=False) -> Tensor

Returns a tensor filled with random integers generated uniformly
between \code{low} (inclusive) and \code{high} (exclusive).

The shape of the tensor is defined by the variable argument \code{size}.

.. note:
With the global dtype default (\code{torch_float32}), this function returns
a tensor with dtype \code{torch_int64}.
}

\examples{
if (torch_is_installed()) {

torch_randint(3, 5, list(3))
torch_randint(0, 10, size = list(2, 2))
torch_randint(3, 10, list(2, 2))
}
}
