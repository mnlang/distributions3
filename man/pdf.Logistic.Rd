% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Logistic.R
\name{pdf.Logistic}
\alias{pdf.Logistic}
\alias{log_pdf.Logistic}
\title{Evaluate the probability mass function of a Logistic distribution}
\usage{
\method{pdf}{Logistic}(d, x, drop = TRUE, ...)

\method{log_pdf}{Logistic}(d, x, drop = TRUE, ...)
}
\arguments{
\item{d}{A \code{Logistic} object created by a call to \code{\link[=Logistic]{Logistic()}}.}

\item{x}{A vector of elements whose probabilities you would like to
determine given the distribution \code{d}.}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{...}{Arguments to be passed to \code{\link[stats]{dlogis}}.
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
Please see the documentation of \code{\link[=Logistic]{Logistic()}} for some properties
of the Logistic distribution, as well as extensive examples
showing to how calculate p-values and confidence intervals.
}
\examples{

set.seed(27)

X <- Logistic(2, 4)
X

random(X, 10)

pdf(X, 2)
log_pdf(X, 2)

cdf(X, 4)
quantile(X, 0.7)
}
\seealso{
Other Logistic distribution: 
\code{\link{cdf.Logistic}()},
\code{\link{quantile.Logistic}()},
\code{\link{random.Logistic}()}
}
\concept{Logistic distribution}
