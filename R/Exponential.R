#' Create an Exponential distribution
#'
#' Exponential distributions are frequently used for modeling the amount
#' of time that passes until a specific event occurs. For example, exponential
#' distributions could be used to model the time between two earthquakes,
#' the amount of delay between internet packets, or the amount of time a piece
#' of machinery can run before needing repair.
#'
#'
#' @param rate The rate parameter, written \eqn{\lambda} in textbooks.
#'   Can be any positive number. Defaults to `1`.
#'
#' @return An `Exponential` object.
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
#'   In the following, let \eqn{X} be an Exponential random variable with
#'   rate parameter `rate` = \eqn{\lambda}.
#'
#'   **Support**: {x in [0, \eqn{\infty})}
#'
#'   **Mean**: 1 / \eqn{\lambda}
#'
#'   **Variance**: 1 / \eqn{\lambda^2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \lambda e^{-\lambda x}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 1 - e^{-\lambda x}
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     \frac{\lambda}{\lambda - t}, for t < \lambda
#'   }
#'
#' @examples
#'
#' set.seed(27)
#'
#' X <- Exponential(5)
#' X
#'
#' mean(X)
#' variance(X)
#' skewness(X)
#' kurtosis(X)
#'
#' random(X, 10)
#'
#' pdf(X, 2)
#' log_pdf(X, 2)
#'
#' cdf(X, 4)
#' quantile(X, 0.7)
#'
#' cdf(X, quantile(X, 0.7))
#' quantile(X, cdf(X, 7))
Exponential <- function(rate = 1) {
  d <- data.frame(rate = rate)
  class(d) <- c("Exponential", "distribution")
  d
}

#' @export
mean.Exponential <- function(x, ...) {
  ellipsis::check_dots_used()
  rval <- x$rate^-1
  setNames(rval, names(x))
}

#' @export
variance.Exponential <- function(x, ...) {
  rval <- x$rate^2
  setNames(rval, names(x))
}

#' @export
skewness.Exponential <- function(x, ...) {
  rval <- rep.int(2, length(x))
  setNames(rval, names(x))
}

#' @export
kurtosis.Exponential <- function(x, ...) {
  rval <- rep.int(6, length(x))
  setNames(rval, names(x))
}

#' Draw a random sample from an Exponential distribution
#'
#' @inherit Exponential examples
#'
#' @param x An `Exponential` object created by a call to [Exponential()].
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
random.Exponential <- function(x, n = 1L, drop = TRUE, ...) {
  n <- make_positive_integer(n)
  if (n == 0L) {
    return(numeric(0L))
  }
  FUN <- function(at, d) rexp(n = length(d), rate = x$rate)
  apply_dpqr(d = x, FUN = FUN, at = matrix(1, ncol = n), type = "random", drop = drop)
}

#' Evaluate the probability density function of an Exponential distribution
#'
#' @inherit Exponential examples
#'
#' @param d An `Exponential` object created by a call to [Exponential()].
#' @param x A vector of elements whose probabilities you would like to
#'   determine given the distribution `d`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Arguments to be passed to \code{\link[stats]{dexp}}.
#'   Unevaluated arguments will generate a warning to catch mispellings or other
#'   possible errors.
#'
#' @return In case of a single distribution object, either a numeric
#'   vector of length `probs` (if `drop = TRUE`, default) or a `matrix` with
#'   `length(x)` columns (if `drop = FALSE`). In case of a vectorized distribution
#'   object, a matrix with `length(x)` columns containing all possible combinations.
#' @export
#'
pdf.Exponential <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) dexp(x = at, rate = d$rate, ...)
  apply_dpqr(d = d, FUN = FUN, at = x, type = "density", drop = drop)
}

#' @rdname pdf.Exponential
#' @export
#'
log_pdf.Exponential <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) dexp(x = at, rate = d$rate, log = TRUE)
  apply_dpqr(d = d, FUN = FUN, at = x, type = "logLik", drop = drop)
}

#' Evaluate the cumulative distribution function of an Exponential distribution
#'
#' @inherit Exponential examples
#'
#' @param d An `Exponential` object created by a call to [Exponential()].
#' @param x A vector of elements whose cumulative probabilities you would
#'   like to determine given the distribution `d`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Arguments to be passed to \code{\link[stats]{pexp}}.
#'   Unevaluated arguments will generate a warning to catch mispellings or other
#'   possible errors.
#'
#' @return In case of a single distribution object, either a numeric
#'   vector of length `probs` (if `drop = TRUE`, default) or a `matrix` with
#'   `length(x)` columns (if `drop = FALSE`). In case of a vectorized distribution
#'   object, a matrix with `length(x)` columns containing all possible combinations.
#' @export
#'
cdf.Exponential <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) pexp(q = at, rate = d$rate, ...)
  apply_dpqr(d = d, FUN = FUN, at = x, type = "probability", drop = drop)
}

#' Determine quantiles of an Exponential distribution
#'
#' `quantile()` is the inverse of `cdf()`.
#'
#' @inherit Exponential examples
#' @inheritParams random.Exponential
#'
#' @param probs A vector of probabilities.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Arguments to be passed to \code{\link[stats]{qexp}}.
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
quantile.Exponential <- function(x, probs, drop = TRUE, ...) {
  ellipsis::check_dots_used()
  FUN <- function(at, d) qexp(at, rate = x$rate, ...)
  apply_dpqr(d = x, FUN = FUN, at = probs, type = "quantile", drop = drop)
}

#' Fit an Exponential distribution to data
#'
#' @param d An `Exponential` object created by a call to [Exponential()].
#' @param x A vector of data.
#' @param ... Unused.
#'
#' @family Exponential distribution
#'
#' @return An `Exponential` object.
#' @export
fit_mle.Exponential <- function(d, x, ...) {
  ss <- suff_stat(d, x, ...)
  Exponential(ss$sum / ss$samples)
}


#' Compute the sufficient statistics of an Exponential distribution from data
#'
#' @inheritParams fit_mle.Exponential
#'
#' @return A named list of the sufficient statistics of the exponential
#'   distribution:
#'
#'   - `sum`: The sum of the observations.
#'   - `samples`: The number of observations.
#'
#' @export
suff_stat.Exponential <- function(d, x, ...) {
  valid_x <- (x > 0)
  if (any(!valid_x)) stop("`x` must only contain positive real numbers")
  list(sum = sum(x), samples = length(x))
}

#' Return the support of the Exponential distribution
#'
#' @param d An `Exponential` object created by a call to [Exponential()].
#' @param drop logical. Should the result be simplified to a vector if possible?
#'
#' @return A vector of length 2 with the minimum and maximum value of the support.
#'
#' @export
support.Exponential <- function(d, drop = TRUE) {
  stopifnot("d must be a supported distribution object" = is_distribution(d))
  stopifnot(is.logical(drop))

  min <- rep(0, length(d))
  max <- rep(Inf, length(d))

  make_support(min, max, d, drop = drop)
}
