context("test-Tukey")

test_that("print.Tukey works", {
  expect_output(print(Tukey(1, 2, 2)), regexp = "Tukey distribution")
})

test_that("vectorization of a Normal distribution work correctly", {
  d <- Tukey(c(3L, 4L), 16L, 2L)
  d1 <- d[1]
  d2 <- d[2]

  ## cdf
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
