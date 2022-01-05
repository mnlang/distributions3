context("test-Erlang")
e <- Erlang(lambda = 0.5, k = 3)

test_that("print.Erlang works", {
  expect_output(print(e), regexp = "Erlang distribution")
})

test_that("random.Erlang works correctly", {
  expect_length(random(e), 1)
  expect_length(random(e, 100), 100)
  expect_length(random(e, 0), 0)
  expect_error(random(e, -2))
})

test_that("pdf.Erlang works correctly", {
  expect_length(pdf(e, seq_len(0)), 0)
  expect_length(pdf(e, seq_len(1)), 1)
  expect_length(pdf(e, seq_len(10)), 10)
  expect_error(pdf(e, -42))
})

test_that("log_pdf.Erlang works correctly", {
  expect_length(log_pdf(e, seq_len(0)), 0)
  expect_length(log_pdf(e, seq_len(1)), 1)
  expect_length(log_pdf(e, seq_len(10)), 10)
  expect_error(log_pdf(e, -42))
})

test_that("cdf.Erlang works correctly", {
  expect_equal(cdf(e, 0), 0)
  expect_length(cdf(e, seq_len(0)), 0)
  expect_length(cdf(e, seq_len(1)), 1)
  expect_length(cdf(e, seq_len(10)), 10)
  expect_error(cdf(e, -42))
})

test_that("quantile.Erlang works correctly", {
  expect_equal(quantile(e, 0), 0)
  expect_length(quantile(e, seq_len(1)), 1)
  expect_length(quantile(e, seq(0.1, 0.9, by = 0.1)), 9)
  expect_error(quantile(e, p = -42))
  expect_error(quantile(e, p = 42))
  expect_error(quantile(e, p = 0.5, interval = 42))
  expect_error(quantile(e, p = 0.5, interval = "Hi"))
  expect_error(quantile(e, p = 0.5, tol = "Hi"))
})

test_that("support.Erlang works correctly", {
  expect_equal(support(e), c(0, Inf))
})

test_that("vectorization of a Erlang distribution work correctly", {
  d <- Erlang(c(0.5, 0.8), 3)
  d1 <- d[1]
  d2 <- d[2]

  set.seed(123); r1 <- random(d)
  set.seed(123); r2 <- c(random(d1), random(d2))
  expect_equal(r1, r2)

  expect_equal(pdf(d, 0), c(pdf(d1, 0), pdf(d2, 0)))
  expect_equal(log_pdf(d, 0), c(log_pdf(d1, 0), log_pdf(d2, 0)))
  expect_equal(cdf(d, 0.5), c(cdf(d1, 0.5), cdf(d2, 0.5)))

  expect_equal(quantile(d, 0.5), c(quantile(d1, 0.5), quantile(d2, 0.5)))
  expect_equal(quantile(d, c(0.5, 0.5)), c(quantile(d1, 0.5), quantile(d2, 0.5)))
  expect_equal(
    quantile(d, c(0.1, 0.5, 0.9)),
    matrix(
      c(quantile(d1, c(0.1, 0.5, 0.9)), quantile(d2, c(0.1, 0.5, 0.9))),
      nrow = 2,
      ncol = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("q_0.1", "q_0.5", "q_0.9"))
    )
  )

  expect_equal(
    support(d),
    matrix(
      c(support(d1), support(d2)),
      nrow = 2,
      ncol = 2,
      byrow = TRUE,
      dimnames = list(NULL, c("min", "max"))
    )
  )
})

