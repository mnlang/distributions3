#' Create a Gumbel distribution
#'
#' The Gumbel distribution is a special case of the `\link{GEV}` distribution,
#' obtained when the GEV shape parameter \eqn{\xi} is equal to 0.
#' It may be referred to as a type I extreme value distribution.
#'
#' @param mu The location parameter, written \eqn{\mu} in textbooks.
#'   `mu` can be any real number.  Defaults to `0`.
#' @param sigma The scale parameter, written \eqn{\sigma} in textbooks.
#'   `sigma` can be any positive number.  Defaults to `1`.
#'
#' @return A `Gumbel` object.
#' @export
#'
#' @family continuous distributions
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://alexpghayes.github.io/distributions3/>, where the math
#'   will render with additional detail and much greater clarity.
#'
#'   In the following, let \eqn{X} be a Gumbel random variable with location
#'   parameter  `mu` = \eqn{\mu}, scale parameter `sigma` = \eqn{\sigma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers.
#'
#'   **Mean**: \eqn{\mu + \sigma\gamma}, where \eqn{\gamma} is Euler's
#'   constant, approximately equal to 0.57722.
#'
#'   **Median**: \eqn{\mu - \sigma\ln(\ln 2)}{\mu - \sigma ln(ln 2)}.
#'
#'   **Variance**: \eqn{\sigma^2 \pi^2 / 6}.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{f(x) = \sigma ^ {-1} \exp[-(x - \mu) / \sigma]%
#'         \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
#'        f(x) = (1 / \sigma) exp[-(x - \mu) / \sigma]%
#'         exp{-exp[-(x - \mu) / \sigma]}}
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   In the \eqn{\xi = 0} (Gumbel) special case
#'   \deqn{F(x) = \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
#'         F(x) = exp{ - exp[-(x - \mu) / \sigma]} }
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#' @examples
#'
#' set.seed(27)
#'
#' X <- Gumbel(1, 2)
#' X
#'
#' random(X, 10)
#'
#' pdf(X, 0.7)
#' log_pdf(X, 0.7)
#'
#' cdf(X, 0.7)
#' quantile(X, 0.7)
#'
#' cdf(X, quantile(X, 0.7))
#' quantile(X, cdf(X, 0.7))
Gumbel <- function(mu = 0, sigma = 1) {
  if (any(sigma <= 0)) {
    stop("sigma must be positive")
  }
  stopifnot(
    "parameter lengths do not match (only scalars are allowed to be recycled)" =
      length(mu) == length(sigma) | length(mu) == 1 | length(sigma) == 1
  )

  d <- data.frame(mu = mu, sigma = sigma)
  class(d) <- c("Gumbel", "distribution")
  d
}

#' @export
mean.Gumbel <- function(x, ...) {
  ellipsis::check_dots_used()
  rval <- x$mu + x$sigma * -digamma(1)
  setNames(rval, names(x))
}

#' @export
variance.Gumbel <- function(x, ...) {
  rval <- pi^(2 / 6) * x$sigma^2
  setNames(rval, names(x))
}

#' @export
skewness.Gumbel <- function(x, ...) {
  zeta3 <- 1.20205690315959401459612
  rval <- rep((12 * sqrt(6) * zeta3) / pi^3, length(x))
  setNames(rval, names(x))
}

#' @export
kurtosis.Gumbel <- function(x, ...) {
  rval <- rep(12 / 5, length(x))
  setNames(rval, names(x))
}

#' Draw a random sample from a Gumbel distribution
#'
#' @inherit Gumbel examples
#'
#' @param x A `Gumbel` object created by a call to [Gumbel()].
#' @param n The number of samples to draw. Defaults to `1L`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Unused. Unevaluated arguments will generate a warning to
#'   catch mispellings or other possible errors.
#'
#' @return In case of a single distribution object or `n = 1`, either a numeric
#'   vector of length `n` (if `drop = TRUE`, default) or a `matrix` with `n` columns
#'   (if `drop = FALSE`).
#' @export
#'
random.Gumbel <- function(x, n = 1L, drop = TRUE, ...) {
  n <- make_positive_integer(n)
  if (n == 0L) {
    return(numeric(0L))
  }
  FUN <- function(at, d) revdbayes::rgev(n = length(d), loc = d$mu, scale = d$sigma, shape = 0)
  apply_dpqr(d = x, FUN = FUN, at = matrix(1, ncol = n), type = "random", drop = drop)
}

#' Evaluate the probability mass function of a Gumbel distribution
#'
#' @inherit Gumbel examples
#'
#' @param d A `Gumbel` object created by a call to [Gumbel()].
#' @param x A vector of elements whose probabilities you would like to
#'   determine given the distribution `d`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Arguments to be passed to \code{\link[revdbayes]{dgev}}.
#'   Unevaluated arguments will generate a warning to catch mispellings or other
#'   possible errors.
#'
#' @return In case of a single distribution object, either a numeric
#'   vector of length `probs` (if `drop = TRUE`, default) or a `matrix` with
#'   `length(x)` columns (if `drop = FALSE`). In case of a vectorized distribution
#'   object, a matrix with `length(x)` columns containing all possible combinations.
#' @export
#'
pdf.Gumbel <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) revdbayes::dgev(x = at, loc = d$mu, scale = d$sigma, shape = 0, ...)
  apply_dpqr(d = d, FUN = FUN, at = x, type = "density", drop = drop)
}

#' @rdname pdf.Gumbel
#' @export
#'
log_pdf.Gumbel <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) revdbayes::dgev(x = at, loc = d$mu, scale = d$sigma, shape = 0, log = TRUE)
  apply_dpqr(d = d, FUN = FUN, at = x, type = "logLik", drop = drop)
}

#' Evaluate the cumulative distribution function of a Gumbel distribution
#'
#' @inherit Gumbel examples
#'
#' @param d A `Gumbel` object created by a call to [Gumbel()].
#' @param x A vector of elements whose cumulative probabilities you would
#'   like to determine given the distribution `d`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Arguments to be passed to \code{\link[revdbayes]{pgev}}.
#'   Unevaluated arguments will generate a warning to catch mispellings or other
#'   possible errors.
#'
#' @return In case of a single distribution object, either a numeric
#'   vector of length `probs` (if `drop = TRUE`, default) or a `matrix` with
#'   `length(x)` columns (if `drop = FALSE`). In case of a vectorized distribution
#'   object, a matrix with `length(x)` columns containing all possible combinations.
#' @export
#'
cdf.Gumbel <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) revdbayes::pgev(q = at, loc = d$mu, scale = d$sigma, shape = 0, ...)
  apply_dpqr(d = d, FUN = FUN, at = x, type = "probability", drop = drop)
}

#' Determine quantiles of a Gumbel distribution
#'
#' `quantile()` is the inverse of `cdf()`.
#'
#' @inherit Gumbel examples
#' @inheritParams random.Gumbel
#'
#' @param probs A vector of probabilities.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Arguments to be passed to \code{\link[revdbayes]{qgev}}.
#'   Unevaluated arguments will generate a warning to catch mispellings or other
#'   possible errors.
#'
#' @return In case of a single distribution object, either a numeric
#'   vector of length `probs` (if `drop = TRUE`, default) or a `matrix` with
#'   `length(probs)` columns (if `drop = FALSE`). In case of a vectorized
#'   distribution object, a matrix with `length(probs)` columns containing all
#'   possible combinations.
#' @export
#'
quantile.Gumbel <- function(x, probs, drop = TRUE, ...) {
  ellipsis::check_dots_used()
  FUN <- function(at, d) revdbayes::qgev(p = at, loc = d$mu, scale = d$sigma, shape = 0, ...)
  apply_dpqr(d = x, FUN = FUN, at = probs, type = "quantile", drop = drop)
}
