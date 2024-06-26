% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creation-ops.R, R/gen-namespace-docs.R,
%   R/gen-namespace-examples.R
\name{torch_range}
\alias{torch_range}
\title{Range}
\usage{
torch_range(
  start,
  end,
  step = 1,
  dtype = NULL,
  layout = NULL,
  device = NULL,
  requires_grad = FALSE
)
}
\arguments{
\item{start}{(float) the starting value for the set of points. Default: \code{0}.}

\item{end}{(float) the ending value for the set of points}

\item{step}{(float) the gap between each pair of adjacent points. Default: \code{1}.}

\item{dtype}{(\code{torch.dtype}, optional) the desired data type of returned tensor.        Default: if \code{NULL}, uses a global default (see \code{torch_set_default_tensor_type}). If \code{dtype} is not given, infer the data type from the other input        arguments. If any of \code{start}, \code{end}, or \code{stop} are floating-point, the        \code{dtype} is inferred to be the default dtype, see        \code{~torch.get_default_dtype}. Otherwise, the \code{dtype} is inferred to        be \code{torch.int64}.}

\item{layout}{(\code{torch.layout}, optional) the desired layout of returned Tensor.        Default: \code{torch_strided}.}

\item{device}{(\code{torch.device}, optional) the desired device of returned tensor.        Default: if \code{NULL}, uses the current device for the default tensor type        (see \code{torch_set_default_tensor_type}). \code{device} will be the CPU        for CPU tensor types and the current CUDA device for CUDA tensor types.}

\item{requires_grad}{(bool, optional) If autograd should record operations on the        returned tensor. Default: \code{FALSE}.}
}
\description{
Range
}
\section{range(start=0, end, step=1, out=NULL, dtype=NULL, layout=torch.strided, device=NULL, requires_grad=False) -> Tensor }{


Returns a 1-D tensor of size \eqn{\left\lfloor \frac{\mbox{end} - \mbox{start}}{\mbox{step}} \right\rfloor + 1}
with values from \code{start} to \code{end} with step \code{step}. Step is
the gap between two values in the tensor.

\deqn{
    \mbox{out}_{i+1} = \mbox{out}_i + \mbox{step}.
}
}

\section{Warning}{

This function is deprecated in favor of \code{\link{torch_arange}}.
}

\examples{
if (torch_is_installed()) {

torch_range(1, 4)
torch_range(1, 4, 0.5)
}
}
