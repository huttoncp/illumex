# The isolation forest's driver: each column replaced by the value the other
# columns predict for it (dev/studies/driver_redesign.R chose this over the
# median or commonest level).

test_that("a category is predicted from its nearest rows, in its own class", {
  set.seed(2)
  n <- 120
  z <- stats::rnorm(n)
  others <- data.frame(a = z + stats::rnorm(n, 0, 0.1), b = -z + stats::rnorm(n, 0, 0.1))
  f <- factor(ifelse(z > 0, "hi", "lo"), levels = c("hi", "lo", "unused"))
  f[1] <- if (z[1] > 0) "lo" else "hi"             # one row contradicting its numbers
  pr <- ilm_iforest_predict_cat(f, others, pool = seq_len(n))
  expect_s3_class(pr, "factor")
  expect_identical(levels(pr), levels(f))
  ## the contradicting row is given the level its neighbours have
  expect_identical(as.character(pr[1]), if (z[1] > 0) "hi" else "lo")
  expect_gt(mean(pr[-1] == f[-1]), 0.9)
  ## a character column stays character, and a gap in the other columns is
  ## averaged over the columns a pair has
  others$a[5] <- NA
  pc <- ilm_iforest_predict_cat(as.character(f), others, pool = seq_len(n))
  expect_type(pc, "character")
  expect_false(anyNA(pc))
})

test_that("a number is predicted by regression, keeping its value where it cannot be", {
  set.seed(3)
  n <- 80
  others <- data.frame(a = stats::rnorm(n), g = factor(sample(c("u", "v"), n, TRUE)))
  x <- 2 * others$a + stats::rnorm(n, 0, 0.1)
  others$a[7] <- NA
  pr <- ilm_iforest_predict_num(x, others)
  expect_equal(pr[7], x[7])
  expect_lt(max(abs(pr[-7] - x[-7])), 0.5)
})

test_that("above the pool size the neighbours come from a seeded draw of rows", {
  skip_if_not_installed("isotree")
  set.seed(4)
  n <- 300
  d <- data.frame(a = stats::rnorm(n), b = stats::rnorm(n),
                  g = factor(sample(c("x", "y", "z"), n, TRUE)))
  fit <- isotree::isolation.forest(d, ntrees = 50L, ndim = 1L, seed = 1L, nthreads = 1L)
  sc <- as.numeric(stats::predict(fit, d))
  m1 <- ilm_iforest_drivers(fit, d, sc, seed = 1L, pool_max = 60L)
  m2 <- ilm_iforest_drivers(fit, d, sc, seed = 1L, pool_max = 60L)
  expect_identical(m1, m2)
  expect_identical(colnames(m1), names(d))
  expect_equal(dim(m1), c(n, 3))
})

test_that("the forest's scan leaves the user's stream as it was, and repeats itself", {
  skip_if_not_installed("isotree")
  set.seed(5)
  n <- 150
  d <- data.frame(a = stats::rnorm(n), b = stats::rnorm(n),
                  g = factor(sample(c("x", "y"), n, TRUE)))
  set.seed(6); before <- .Random.seed
  r1 <- suppressMessages(ilm_anomaly(d, method = "iforest", seed = 1, progress = FALSE))
  expect_identical(.Random.seed, before)
  r2 <- suppressMessages(ilm_anomaly(d, method = "iforest", seed = 1, progress = FALSE))
  expect_identical(r1$driver, r2$driver)
})
