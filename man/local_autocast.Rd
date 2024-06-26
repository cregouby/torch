% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autocast.R
\name{local_autocast}
\alias{local_autocast}
\alias{with_autocast}
\alias{set_autocast}
\alias{unset_autocast}
\title{Autocast context manager}
\usage{
local_autocast(
  device_type,
  dtype = NULL,
  enabled = TRUE,
  cache_enabled = NULL,
  ...,
  .env = parent.frame()
)

with_autocast(
  code,
  ...,
  device_type,
  dtype = NULL,
  enabled = TRUE,
  cache_enabled = NULL
)

set_autocast(device_type, dtype = NULL, enabled = TRUE, cache_enabled = NULL)

unset_autocast(context)
}
\arguments{
\item{device_type}{a character string indicating whether to use 'cuda' or 'cpu' device}

\item{dtype}{a torch data type indicating whether to use \code{torch_float16()} or \code{torch_bfloat16()}.}

\item{enabled}{a logical value indicating whether autocasting should be enabled in the region. Default: TRUE}

\item{cache_enabled}{a logical value indicating whether the weight cache inside autocast should be enabled.}

\item{...}{currently unused.}

\item{.env}{The environment to use for scoping.}

\item{code}{code to be executed with no gradient recording.}

\item{context}{Returned by \code{set_autocast} and should be passed when unsetting it.}
}
\description{
Allow regions of your code to run in mixed precision.
In these regions, ops run in an op-specific dtype chosen by autocast
to improve performance while maintaining accuracy.
}
\details{
When entering an autocast-enabled region, Tensors may be any type.
You should not call \code{half()} or \code{bfloat16()} on your model(s) or inputs
when using autocasting.

\code{autocast} should only be enabled during the forward pass(es) of your network,
including the loss computation(s).  Backward passes under autocast are not
recommended. Backward ops run in the same type that autocast used for
corresponding forward ops.
}
\section{Functions}{
\itemize{
\item \code{with_autocast()}: A with context for automatic mixed precision.

\item \code{set_autocast()}: Set the autocast context. For advanced users only.

\item \code{unset_autocast()}: Unset the autocast context.

}}
\examples{
if (torch_is_installed()) {
x <- torch_randn(5, 5, dtype = torch_float32())
y <- torch_randn(5, 5, dtype = torch_float32())

foo <- function(x, y) {
  local_autocast(device = "cpu")
  z <- torch_mm(x, y)
  w <- torch_mm(z, x)
  w
}

out <- foo(x, y)
}
}
\seealso{
\code{\link[=cuda_amp_grad_scaler]{cuda_amp_grad_scaler()}} to perform dynamic gradient scaling.
}
