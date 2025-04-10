% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linalg.R
\name{linalg_solve_triangular}
\alias{linalg_solve_triangular}
\title{Triangular solve}
\usage{
linalg_solve_triangular(A, B, ..., upper, left = TRUE, unitriangular = FALSE)
}
\arguments{
\item{A}{tensor of shape \verb{(*, n, n)} or \verb{(*, k, k)} if \code{left=TRUE}) where \code{*} is zero or more batch dimensions.}

\item{B}{right-hand side tensor of shape \verb{(*, n, k)}}

\item{...}{Currently ignored.}

\item{upper}{whether A is an upper or lower triangular matrix.}

\item{left}{wheter to solve the system AX=B or XA=B}

\item{unitriangular}{if \code{TRUE}, the diagonal elements of A are assumed to be all equal to 1.}
}
\description{
Triangular solve
}
\seealso{
Other linalg: 
\code{\link{linalg_cholesky}()},
\code{\link{linalg_cholesky_ex}()},
\code{\link{linalg_det}()},
\code{\link{linalg_eig}()},
\code{\link{linalg_eigh}()},
\code{\link{linalg_eigvals}()},
\code{\link{linalg_eigvalsh}()},
\code{\link{linalg_householder_product}()},
\code{\link{linalg_inv}()},
\code{\link{linalg_inv_ex}()},
\code{\link{linalg_lstsq}()},
\code{\link{linalg_matrix_norm}()},
\code{\link{linalg_matrix_power}()},
\code{\link{linalg_matrix_rank}()},
\code{\link{linalg_multi_dot}()},
\code{\link{linalg_norm}()},
\code{\link{linalg_pinv}()},
\code{\link{linalg_qr}()},
\code{\link{linalg_slogdet}()},
\code{\link{linalg_solve}()},
\code{\link{linalg_svd}()},
\code{\link{linalg_svdvals}()},
\code{\link{linalg_tensorinv}()},
\code{\link{linalg_tensorsolve}()},
\code{\link{linalg_vector_norm}()}
}
\concept{linalg}
