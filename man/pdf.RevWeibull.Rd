% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReversedWeibull.R
\name{pdf.RevWeibull}
\alias{pdf.RevWeibull}
\alias{log_pdf.RevWeibull}
\title{Evaluate the probability mass function of an RevWeibull distribution}
\usage{
\method{pdf}{RevWeibull}(d, x, drop = TRUE, ...)

\method{log_pdf}{RevWeibull}(d, x, drop = TRUE, ...)
}
\arguments{
\item{d}{A \code{RevWeibull} object created by a call to \code{\link[=RevWeibull]{RevWeibull()}}.}

\item{x}{A vector of elements whose probabilities you would like to
determine given the distribution \code{d}.}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{...}{Arguments to be passed to \code{\link[revdbayes]{dgev}}.
Unevaluated arguments will generate a warning to catch mispellings or other
possible errors.}
}
\value{
In case of a single distribution object, either a numeric
vector of length \code{probs} (if \code{drop = TRUE}, default) or a \code{matrix} with
\code{length(x)} columns (if \code{drop = FALSE}). In case of a vectorized distribution
object, a matrix with \code{length(x)} columns containing all possible combinations.
}
\description{
Evaluate the probability mass function of an RevWeibull distribution
}
\examples{

set.seed(27)

X <- RevWeibull(1, 2)
X

random(X, 10)

pdf(X, 0.7)
log_pdf(X, 0.7)

cdf(X, 0.7)
quantile(X, 0.7)

cdf(X, quantile(X, 0.7))
quantile(X, cdf(X, 0.7))
}
