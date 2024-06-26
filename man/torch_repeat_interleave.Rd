% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen-namespace-docs.R,
%   R/gen-namespace-examples.R, R/gen-namespace.R
\name{torch_repeat_interleave}
\alias{torch_repeat_interleave}
\title{Repeat_interleave}
\usage{
torch_repeat_interleave(self, repeats, dim = NULL, output_size = NULL)
}
\arguments{
\item{self}{(Tensor) the input tensor.}

\item{repeats}{(Tensor or int) The number of repetitions for each element.        repeats is broadcasted to fit the shape of the given axis.}

\item{dim}{(int, optional) The dimension along which to repeat values.        By default, use the flattened input array, and return a flat output        array.}

\item{output_size}{(int, optional) – Total output size for the given axis
( e.g. sum of repeats). If given, it will avoid stream syncronization needed
to calculate output shape of the tensor.}
}
\description{
Repeat_interleave
}
\section{repeat_interleave(input, repeats, dim=NULL) -> Tensor }{


Repeat elements of a tensor.
}

\section{Warning}{


\if{html}{\out{<div class="sourceCode">}}\preformatted{This is different from `torch_Tensor.repeat` but similar to `numpy.repeat`.
}\if{html}{\out{</div>}}
}

\section{repeat_interleave(repeats) -> Tensor }{


If the \code{repeats} is \verb{tensor([n1, n2, n3, ...])}, then the output will be
\verb{tensor([0, 0, ..., 1, 1, ..., 2, 2, ..., ...])} where \code{0} appears \code{n1} times,
\code{1} appears \code{n2} times, \code{2} appears \code{n3} times, etc.
}

\examples{
if (torch_is_installed()) {
\dontrun{
x = torch_tensor(c(1, 2, 3))
x$repeat_interleave(2)
y = torch_tensor(matrix(c(1, 2, 3, 4), ncol = 2, byrow=TRUE))
torch_repeat_interleave(y, 2)
torch_repeat_interleave(y, 3, dim=1)
torch_repeat_interleave(y, torch_tensor(c(1, 2)), dim=1)
}
}
}
