context("test-Geometric")

test_that("print.Geometric works", {
  expect_output(print(Geometric()), regexp = "Geometric distribution")
})

test_that("likelihood.Geometric and log_likelihood.Geometric work correctly", {
  cau <- Geometric()
  x <- c(1, 1, 0)

  expect_equal(likelihood(cau, 1), dgeom(1, 0.5))
  expect_equal(likelihood(cau, x), dgeom(1, 0.5) * dgeom(1, 0.5) * dgeom(0, 0.5))

  expect_equal(log_likelihood(cau, 1), log(dgeom(1, 0.5)))
  expect_equal(log_likelihood(cau, x), log(dgeom(1, 0.5) * dgeom(1, 0.5) * dgeom(0, 0.5)))
})

test_that("random.Geometric work correctly", {
  cau <- Geometric()

  expect_length(random(cau), 1)
  expect_length(random(cau, 100), 100)
  expect_length(random(cau, 0), 0)
  expect_error(random(cau, -2))
})

test_that("pdf.Geometric work correctly", {
  cau <- Geometric()

  expect_equal(pdf(cau, 0), dgeom(0, 0.5))
  expect_equal(pdf(cau, 1), dgeom(1, 0.5))

  expect_length(pdf(cau, seq_len(0)), 0)
  expect_length(pdf(cau, seq_len(1)), 1)
  expect_length(pdf(cau, seq_len(10)), 10)
})

test_that("log_pdf.Geometric work correctly", {
  cau <- Geometric()

  expect_equal(log_pdf(cau, 0), log(dgeom(0, 0.5)))
  expect_equal(log_pdf(cau, 1), log(dgeom(1, 0.5)))

  expect_length(log_pdf(cau, seq_len(0)), 0)
  expect_length(log_pdf(cau, seq_len(1)), 1)
  expect_length(log_pdf(cau, seq_len(10)), 10)
})

test_that("cdf.Geometric work correctly", {
  cau <- Geometric()

  expect_equal(cdf(cau, 0), pgeom(0, 0.5))
  expect_equal(cdf(cau, 1), pgeom(1, 0.5))


  expect_length(cdf(cau, seq_len(0)), 0)
  expect_length(cdf(cau, seq_len(1)), 1)
  expect_length(cdf(cau, seq_len(10)), 10)
})

test_that("quantile.Geometric work correctly", {
  cau <- Geometric()

  expect_equal(quantile(cau, 0), qgeom(0, 0.5))
  expect_equal(quantile(cau, 1), qgeom(1, 0.5))


  expect_length(quantile(cau, seq_len(0)), 0)
  expect_length(quantile(cau, c(0, 1)), 2)
})

test_that("vectorization of a Geometric distribution work correctly", {
  d <- Geometric(c(0.1, 0.5))
  d1 <- d[1]
  d2 <- d[2]

  ## moments
  expect_equal(mean(d), c(mean(d1), mean(d2)))
  expect_equal(variance(d), c(variance(d1), variance(d2)))
  expect_equal(skewness(d), c(skewness(d1), skewness(d2)))
  expect_equal(kurtosis(d), c(kurtosis(d1), kurtosis(d2)))

  ## random
  set.seed(123)
  r1 <- random(d)
  set.seed(123)
  r2 <- c(random(d1), random(d2))
  expect_equal(r1, r2)

  ## pdf, log_pdf, cdf
  expect_equal(pdf(d, 0), c(pdf(d1, 0), pdf(d2, 0)))
  expect_equal(log_pdf(d, 0), c(log_pdf(d1, 0), log_pdf(d2, 0)))
  expect_equal(cdf(d, 0.5), c(cdf(d1, 0.5), cdf(d2, 0.5)))

  ## quantile
  expect_equal(quantile(d, 0.5), c(quantile(d1, 0.5), quantile(d2, 0.5)))
  expect_equal(quantile(d, c(0.5, 0.5)), c(quantile(d1, 0.5), quantile(d2, 0.5)))
  expect_equal(
    quantile(d, c(0.1, 0.5, 0.9)),
    rbind(quantile(d1, c(0.1, 0.5, 0.9)), quantile(d2, c(0.1, 0.5, 0.9)))
  )

  ## support
  expect_equal(
    support(d),
    matrix(
      c(support(d1)[1], support(d2)[1], support(d1)[2], support(d2)[2]),
      ncol = 2, dimnames = list(names(d), c("min", "max"))
    )
  )
  expect_true(is.numeric(support(d1)))
  expect_true(is.numeric(support(d1, drop = FALSE)))
  expect_null(dim(support(d1)))
  expect_equal(dim(support(d1, drop = FALSE)), c(1L, 2L))
})

test_that("named return values for Normal distribution work correctly", {
  d <- Normal(c(0, 10), c(1, 1))
  names(d) <- LETTERS[1:length(d)]

  expect_equal(names(mean(d)), LETTERS[1:length(d)])
  expect_equal(names(variance(d)), LETTERS[1:length(d)])
  expect_equal(names(skewness(d)), LETTERS[1:length(d)])
  expect_equal(names(kurtosis(d)), LETTERS[1:length(d)])
  expect_equal(names(random(d, 1)), LETTERS[1:length(d)])
  expect_equal(rownames(random(d, 3)), LETTERS[1:length(d)])
  expect_equal(names(pdf(d, 0.5)), LETTERS[1:length(d)])
  expect_equal(names(pdf(d, c(0.5, 0.7))), LETTERS[1:length(d)])
  expect_equal(rownames(pdf(d, c(0.5, 0.7, 0.9))), LETTERS[1:length(d)])
  expect_equal(names(log_pdf(d, 0.5)), LETTERS[1:length(d)])
  expect_equal(names(log_pdf(d, c(0.5, 0.7))), LETTERS[1:length(d)])
  expect_equal(rownames(log_pdf(d, c(0.5, 0.7, 0.9))), LETTERS[1:length(d)])
  expect_equal(names(cdf(d, 0.5)), LETTERS[1:length(d)])
  expect_equal(names(cdf(d, c(0.5, 0.7))), LETTERS[1:length(d)])
  expect_equal(rownames(cdf(d, c(0.5, 0.7, 0.9))), LETTERS[1:length(d)])
  expect_equal(names(quantile(d, 0.5)), LETTERS[1:length(d)])
  expect_equal(names(quantile(d, c(0.5, 0.7))), LETTERS[1:length(d)])
  expect_equal(rownames(quantile(d, c(0.5, 0.7, 0.9))), LETTERS[1:length(d)])
  expect_equal(names(support(d[1])), c("min", "max"))
  expect_equal(colnames(support(d)), c("min", "max"))
  expect_equal(rownames(support(d)), LETTERS[1:length(d)])
})
