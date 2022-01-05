#' Create an Erlang distribution
#'
#' The Erlang distribution is a two-parameter family of continuous probability
#' distributions with support \eqn{x \in [0,\infty)}.
#' The two parameters are a positive integer shape parameter \eqn{k} and a
#' positive real rate parameter \eqn{\lambda}.
#' The Erlang distribution with shape parameter \eqn{k = 1} simplifies to the
#' exponential distribution, and it is a special case of the gamma distribution.
#' It corresponds to a sum of \eqn{k} independent exponential variables with mean
#' \eqn{1 / \lambda} each.
#'
#' @param k The shape parameter. Can be any positive integer number.
#' @param lambda The rate parameter. Can be any positive number.
#' @return An `Erlang` object.
#' @export
#' @family continuous distributions
#'
#' @examples
#'
#' set.seed(27)
#'
#' X <- Erlang(5, 2)
#' X
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
Erlang <- function(k, lambda) {
  stopifnot(
    "parameter lengths do not match (only scalars are allowed to be recycled)" =
    length(k) == length(lambda) | length(k) == 1 | length(lambda) == 1
  )
  d <- data.frame(k = k, lambda = lambda)
  class(d) <- c("Erlang", "distribution")
  d
}

#' Draw a random sample from an Erlang distribution
#'
#' @inherit Erlang examples
#'
#' @param x An `Erlang` object created by a call to [Erlang()].
#' @param n The number of samples to draw. Defaults to `1L`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Unused. Unevaluated arguments will generate a warning to
#'   catch mispellings or other possible errors.
#'
#' @return A numeric vector of length `n`.
#' @export
#'
random.Erlang <- function(x, n = 1L, drop = TRUE, ...) {
  FUN <- function(at, d) rerlang(n = length(d), k = d$k, lambda = d$lambda)
  apply_dpqr(d = x, FUN = FUN, at = rep.int(1, n), type_prefix = "r", drop = drop)
}

#' Evaluate the probability mass function of an Erlang distribution
#'
#' @inherit Erlang examples
#'
#' @param d An `Erlang` object created by a call to [Erlang()].
#' @param x A vector of elements whose probabilities you would like to
#'   determine given the distribution `d`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Unused. Unevaluated arguments will generate a warning to
#'   catch mispellings or other possible errors.
#'
#' @return A vector of probabilities, one for each element of `x`.
#' @export
#'
pdf.Erlang <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) derlang(x = at, k = d$k, lambda = d$lambda, ...)
  apply_dpqr(d = d, FUN = FUN, at = x, type_prefix = "d", drop = drop)
}

#' @rdname pdf.Erlang
#' @export
#'
log_pdf.Erlang <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) derlang(x = at, k = d$k, lambda = d$lambda, log = TRUE, ...)
  apply_dpqr(d = d, FUN = FUN, at = x, type_prefix = "l", drop = drop)
}

#' Evaluate the cumulative distribution function of an Erlang distribution
#'
#' @inherit Erlang examples
#'
#' @param d An `Erlang` object created by a call to [Erlang()].
#' @param x A vector of elements whose cumulative probabilities you would
#'   like to determine given the distribution `d`.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Unused. Unevaluated arguments will generate a warning to
#'   catch mispellings or other possible errors.
#'
#' @return A vector of probabilities, one for each element of `x`.
#' @export
#'
cdf.Erlang <- function(d, x, drop = TRUE, ...) {
  FUN <- function(at, d) perlang(q = at, k = d$k, lambda = d$lambda, ...)
  apply_dpqr(d = d, FUN = FUN, at = x, type_prefix = "p", drop = drop)
}

#' Determine quantiles of an Erlang distribution
#'
#' `quantile()` is the inverse of `cdf()`.
#'
#' @inherit Erlang examples
#' @inheritParams random.Erlang
#'
#' @param probs A vector of probabilites.
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param ... Unused. Unevaluated arguments will generate a warning to
#'   catch mispellings or other possible errors.
#'
#' @return A vector of quantiles, one for each element of `probs`.
#' @export
#'
quantile.Erlang <- function(x, probs, drop = TRUE, ...) {
  ellipsis::check_dots_used()

  FUN <- function(at, d) qerlang(at, k = d$k, lambda = d$lambda)
  apply_dpqr(d = x, FUN = FUN, at = probs, type_prefix = "q", drop = drop)
}

#' Return the support of the Erlang distribution
#'
#' @param d An `Erlang` object created by a call to [Erlang()].
#' @param drop logical. Should the result be simplified to a vector if possible?
#' @param drop logical. Should the result be simplified to a vector if possible?
#'
#' @return A vector of length 2 with the minimum and maximum value of the support.
#'
#' @export
support.Erlang <- function(d, drop = TRUE) {

  stopifnot("d must be a supported distribution object" = is_distribution(d))
  stopifnot(is.logical(drop))

  min <- rep(0, length(d))
  max <- rep(Inf, length(d))

  make_support(min, max, drop = drop)
}

rerlang <- function(n, k, lambda) {
  
  stopifnot(is.numeric(n) && is.null(dim(n)))

  par <- data.frame(k, lambda)

  -1 / par$lambda * colSums(log(matrix(runif(n = n * par$k), ncol = n)))
}

derlang <- function(x, k, lambda, log = FALSE) {

  stopifnot(all(x >= 0))

  par <- data.frame(k, lambda)

  rval <- par$k * log(par$lambda) + (par$k - 1) * log(x) - par$lambda * x - lgamma(par$k)

  if (isTRUE(log)) {
    rval
  } else {
    exp(rval)
  }
}

perlang <- function(q, k, lambda, lower.tail = TRUE, log.p = FALSE) {

  stopifnot(all(q >= 0))

  # FIXME: lower.tail, log.p currently not supported
  # FIXME: Simplify function

  par <- data.frame(k, lambda)

  internal <- Vectorize(FUN = function(par, q) {
    summation <- vector(mode = "numeric", length = par$k)
    n <- 0:(par$k - 1)
    summation <- 1 / factorial(n) * exp(-par$lambda * q) * (par$lambda * q)^n
    return(1 - sum(summation))
  }, vectorize.args = "q")
  internal(par = par, q = q)
}

qerlang <- function(p, k, lambda, lower.tail = TRUE, log.p = FALSE, 
                    interval = c(0, 1e6), tol = .Machine$double.eps) {
# interval Interval being used to search for the quantile using numerical root finding. Defaults to (0, 1e6)
# tol Tolerance of the root finding algorithm. Defaults to `.Machine$double.eps`

  stopifnot(all(p >= 0 & p <=1))

  # FIXME: lower.tail, log.p currently not supported
  # FIXME: Simplify function (remove interval and tol)

  par <- data.frame(k, lambda)

  p[p == 1] <- (1 - .Machine$double.eps^0.25)
  internal <- Vectorize(FUN = function(par, p, interval, tol) {
    qf <- function(x) perlang(q = x, k = par$k, lambda = par$lambda) - p
    root <- stats::uniroot(qf, interval = interval, tol = tol, check.conv = TRUE)
    return(root$root)
  }, vectorize.args = "p")
  internal(par = par, p = p, interval = interval, tol = tol)
}

