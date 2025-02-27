% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NegativeBinomial.R
\name{quantile.NegativeBinomial}
\alias{quantile.NegativeBinomial}
\title{Determine quantiles of a NegativeBinomial distribution}
\usage{
\method{quantile}{NegativeBinomial}(x, probs, drop = TRUE, ...)
}
\arguments{
\item{x}{A \code{NegativeBinomial} object created by a call to
\code{\link[=NegativeBinomial]{NegativeBinomial()}}.}

\item{probs}{A vector of probabilities.}

\item{drop}{logical. Should the result be simplified to a vector if possible?}

\item{...}{Arguments to be passed to \code{\link[stats]{qnbinom}}.
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
Determine quantiles of a NegativeBinomial distribution
}
\examples{

set.seed(27)

X <- NegativeBinomial(10, 0.3)
X

random(X, 10)

pdf(X, 2)
log_pdf(X, 2)

cdf(X, 4)
quantile(X, 0.7)
}
\seealso{
Other NegativeBinomial distribution: 
\code{\link{cdf.NegativeBinomial}()},
\code{\link{pdf.NegativeBinomial}()},
\code{\link{random.NegativeBinomial}()}
}
\concept{NegativeBinomial distribution}
