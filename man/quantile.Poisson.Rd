% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Poisson.R
\name{quantile.Poisson}
\alias{quantile.Poisson}
\title{Determine quantiles of a Poisson distribution}
\usage{
\method{quantile}{Poisson}(x, probs, drop = TRUE, ...)
}
\arguments{
\item{x}{A \code{Poisson} object created by a call to \code{\link[=Poisson]{Poisson()}}.}

\item{probs}{A vector of probabilities.}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{...}{Arguments to be passed to \code{\link[stats]{qpois}}.
Unevaluated arguments will generate a warning to catch mispellings or other
possible errors.}
}
\value{
In case of a single distribution object, either a numeric
vector of length \code{probs} (if \code{drop = TRUE}, default) or a \code{matrix} with
\code{length(probs)} columns (if \code{drop = FALSE}). In case of a vectorized
distribution object, a matrix with \code{length(probs)} columns containing all
possible combinations.
}
\description{
\code{quantile()} is the inverse of \code{cdf()}.
}
\examples{

set.seed(27)

X <- Poisson(2)
X

random(X, 10)

pdf(X, 2)
log_pdf(X, 2)

cdf(X, 4)
quantile(X, 0.7)

cdf(X, quantile(X, 0.7))
quantile(X, cdf(X, 7))
}
