% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen-namespace-docs.R
\name{torch_lstsq}
\alias{torch_lstsq}
\title{Lstsq}
\arguments{
\item{self}{(Tensor) the matrix \eqn{B}}

\item{A}{(Tensor) the \eqn{m} by \eqn{n} matrix \eqn{A}}
}
\description{
Lstsq
}
\note{
\if{html}{\out{<div class="sourceCode">}}\preformatted{The case when \eqn{m < n} is not supported on the GPU.
}\if{html}{\out{</div>}}
}
\section{lstsq(input, A, out=NULL) -> Tensor }{


Computes the solution to the least squares and least norm problems for a full
rank matrix \eqn{A} of size \eqn{(m \times n)} and a matrix \eqn{B} of
size \eqn{(m \times k)}.

If \eqn{m \geq n}, \code{\link[=torch_lstsq]{torch_lstsq()}} solves the least-squares problem:

\deqn{
   \begin{array}{ll}
   \min_X & \|AX-B\|_2.
   \end{array}
}
If \eqn{m < n}, \code{\link[=torch_lstsq]{torch_lstsq()}} solves the least-norm problem:

\deqn{
   \begin{array}{llll}
   \min_X & \|X\|_2 & \mbox{subject to} & AX = B.
   \end{array}
}
Returned tensor \eqn{X} has shape \eqn{(\mbox{max}(m, n) \times k)}. The first \eqn{n}
rows of \eqn{X} contains the solution. If \eqn{m \geq n}, the residual sum of squares
for the solution in each column is given by the sum of squares of elements in the
remaining \eqn{m - n} rows of that column.
}

