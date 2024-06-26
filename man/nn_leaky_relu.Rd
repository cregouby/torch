% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn-activation.R
\name{nn_leaky_relu}
\alias{nn_leaky_relu}
\title{LeakyReLU module}
\usage{
nn_leaky_relu(negative_slope = 0.01, inplace = FALSE)
}
\arguments{
\item{negative_slope}{Controls the angle of the negative slope. Default: 1e-2}

\item{inplace}{can optionally do the operation in-place. Default: \code{FALSE}}
}
\description{
Applies the element-wise function:
}
\details{
\deqn{
  \mbox{LeakyReLU}(x) = \max(0, x) + \mbox{negative\_slope} * \min(0, x)
}
or

\deqn{
  \mbox{LeakyRELU}(x) =
  \left\{ \begin{array}{ll}
x, & \mbox{ if } x \geq 0 \\
\mbox{negative\_slope} \times x, & \mbox{ otherwise }
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
m <- nn_leaky_relu(0.1)
input <- torch_randn(2)
output <- m(input)
}
}
