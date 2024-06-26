% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn-activation.R
\name{nn_rrelu}
\alias{nn_rrelu}
\title{RReLU module}
\usage{
nn_rrelu(lower = 1/8, upper = 1/3, inplace = FALSE)
}
\arguments{
\item{lower}{lower bound of the uniform distribution. Default: \eqn{\frac{1}{8}}}

\item{upper}{upper bound of the uniform distribution. Default: \eqn{\frac{1}{3}}}

\item{inplace}{can optionally do the operation in-place. Default: \code{FALSE}}
}
\description{
Applies the randomized leaky rectified liner unit function, element-wise,
as described in the paper:
}
\details{
\verb{Empirical Evaluation of Rectified Activations in Convolutional Network}.

The function is defined as:

\deqn{
\mbox{RReLU}(x) =
\left\{ \begin{array}{ll}
x & \mbox{if } x \geq 0 \\
ax & \mbox{ otherwise }
\end{array}
\right.
}

where \eqn{a} is randomly sampled from uniform distribution
\eqn{\mathcal{U}(\mbox{lower}, \mbox{upper})}.
See: https://arxiv.org/pdf/1505.00853.pdf
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
m <- nn_rrelu(0.1, 0.3)
input <- torch_randn(2)
m(input)
}
}
