% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn-activation.R
\name{nn_threshold}
\alias{nn_threshold}
\title{Threshold module}
\usage{
nn_threshold(threshold, value, inplace = FALSE)
}
\arguments{
\item{threshold}{The value to threshold at}

\item{value}{The value to replace with}

\item{inplace}{can optionally do the operation in-place. Default: \code{FALSE}}
}
\description{
Thresholds each element of the input Tensor.
}
\details{
Threshold is defined as:
\deqn{
  y =
  \left\{ \begin{array}{ll}
  x, &\mbox{ if } x > \mbox{threshold} \\
  \mbox{value}, &\mbox{ otherwise }
  \end{array}
  \right.
}
}
\section{Shape}{

\itemize{
\item Input: \eqn{(N, *)} where \code{*} means, any number of additional
dimensions
\item Output: \eqn{(N, *)}, same shape as the input
}
}

\examples{
if (torch_is_installed()) {
m <- nn_threshold(0.1, 20)
input <- torch_randn(2)
output <- m(input)
}
}
