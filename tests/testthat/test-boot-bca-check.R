# BCa says when it cannot be relied on (as settled for Craig):
# below 8 rows, or when its bias correction or acceleration is undefined,
# naming the groups and the remedy.

test_that("a group below 8 rows is named, with its rows and the remedy", {
  d <- ilm_sim()
  expect_warning(ilm_boot_ci(d, "income", by = "grp", stat = "median",
                             ci_type = "bca", R = 200, seed = 1),
                 "BCa is not reliable below 8 rows, and group delta has 3.*ci_type = \"percentile\"")
  expect_warning(ilm_boot_ci(c(3, 9, 4, 1), ci_type = "bca", R = 200, seed = 1),
                 "the data have 4")
  ## eight rows or more, and every other interval type, say nothing
  expect_silent(ilm_boot_ci(stats::rexp(8), ci_type = "bca", R = 200, seed = 1))
  expect_silent(ilm_boot_ci(d, "income", by = "grp", stat = "median",
                            ci_type = "percentile", R = 200, seed = 1))
})

test_that("an undefined bias correction is said, and percentile given", {
  y <- c(rep(0, 9), 1)
  expect_warning(r <- ilm_boot_ci(y, stat = "median", ci_type = "bca", R = 200, seed = 1),
                 "one side of the observed value.*More data would help")
  p <- ilm_boot_ci(y, stat = "median", ci_type = "percentile", R = 200, seed = 1)
  expect_identical(c(r$lower, r$upper), c(p$lower, p$upper))
})

test_that("an undefined acceleration is said", {
  rg <- function(v) diff(range(v))
  expect_warning(ilm_boot_ci(c(0, 0, 1, 2, 3, 4, 5, 5, 5, 5), stat = rg,
                             ci_type = "bca", R = 200, seed = 1),
                 "every leave-one-out statistic is the same")
})

test_that("the intervals themselves are unchanged by the check", {
  y <- stats::rexp(5)
  a <- suppressWarnings(ilm_boot_ci(y, ci_type = "bca", R = 300, seed = 2))
  set.seed(2)
  th <- vapply(1:300, function(r) mean(y[sample.int(5, 5, TRUE)]), 1)
  b <- ilm_bca(y, th, mean(y), mean, 0.95, "mean")
  expect_equal(c(a$lower, a$upper), as.numeric(b))
})
