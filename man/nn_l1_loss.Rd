% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn-loss.R
\name{nn_l1_loss}
\alias{nn_l1_loss}
\title{L1 loss}
\usage{
nn_l1_loss(reduction = "mean")
}
\arguments{
\item{reduction}{(string, optional): Specifies the reduction to apply to the output:
\code{'none'} | \code{'mean'} | \code{'sum'}. \code{'none'}: no reduction will be applied,
\code{'mean'}: the sum of the output will be divided by the number of
elements in the output, \code{'sum'}: the output will be summed.}
}
\description{
Creates a criterion that measures the mean absolute error (MAE) between each
element in the input \eqn{x} and target \eqn{y}.
}
\details{
The unreduced (i.e. with \code{reduction} set to \code{'none'}) loss can be described
as:

\deqn{
\ell(x, y) = L = \{l_1,\dots,l_N\}^\top, \quad
l_n = \left| x_n - y_n \right|,
}

where \eqn{N} is the batch size. If \code{reduction} is not \code{'none'}
(default \code{'mean'}), then:

\deqn{
\ell(x, y) =
\begin{array}{ll}
\mbox{mean}(L), & \mbox{if reduction} = \mbox{'mean';}\\
\mbox{sum}(L),  & \mbox{if reduction} = \mbox{'sum'.}
\end{array}
}

\eqn{x} and \eqn{y} are tensors of arbitrary shapes with a total
of \eqn{n} elements each.

The sum operation still operates over all the elements, and divides by \eqn{n}.
The division by \eqn{n} can be avoided if one sets \code{reduction = 'sum'}.
}
\section{Shape}{

\itemize{
\item Input: \eqn{(N, *)} where \eqn{*} means, any number of additional
dimensions
\item Target: \eqn{(N, *)}, same shape as the input
\item Output: scalar. If \code{reduction} is \code{'none'}, then
\eqn{(N, *)}, same shape as the input
}
}

\examples{
if (torch_is_installed()) {
loss <- nn_l1_loss()
input <- torch_randn(3, 5, requires_grad = TRUE)
target <- torch_randn(3, 5)
output <- loss(input, target)
output$backward()
}
}
