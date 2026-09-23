mar_data <- function(n = 500L, seed = 11L, on = c("outcome", "covariate", "none")) {
  on <- match.arg(on)
  set.seed(seed)
  z <- rnorm(n); x <- 0.6 * z + rnorm(n); y <- 0.5 * x + 0.3 * z + rnorm(n)
  d <- data.frame(x = x, z = z, y = y)
  p <- switch(on, outcome = plogis(1.0 * y - 0.4),
              covariate = plogis(1.2 * z - 0.4), none = rep(0.3, n))
  d$x[stats::runif(n) < p] <- NA
  d
}

test_that("check_missing reports how much is missing and where", {
  d <- mar_data()
  r <- ilm_check_missing(d, y = "y", verbose = FALSE)
  expect_s3_class(r, "ilm_missing")
  expect_equal(r$n, nrow(d))
  expect_equal(r$n_complete, sum(stats::complete.cases(d)))
  expect_equal(r$variables$n_missing[r$variables$variable == "x"],
               sum(is.na(d$x)))
  expect_equal(r$variables$n_missing[r$variables$variable == "y"], 0L)
  expect_true(nrow(r$patterns) >= 1L)

  ## nothing missing is said plainly rather than analysed
  clean <- ilm_check_missing(d[stats::complete.cases(d), ], verbose = FALSE)
  expect_equal(clean$verdict, "NONE")
  expect_null(clean$associations)
  expect_output(print(clean), "no missing values")
})

test_that("check_missing separates missingness on a covariate from on the outcome", {
  ## This is the distinction the whole function exists for: complete cases are
  ## unbiased in the first case and biased in the second, so the advice differs.
  cov <- ilm_check_missing(mar_data(on = "covariate"), y = "y", verbose = FALSE)
  expect_equal(cov$verdict, "RELATED_TO_COVARIATES")
  fl <- cov$associations[cov$associations$flag, ]
  expect_true("z" %in% fl$related_to)
  ## The MARGINAL table does flag the outcome, and correctly: missingness
  ## depends on z, the outcome depends on z, so the two are associated. That is
  ## exactly why the verdict cannot be read off this table -- the conditional
  ## test is what decides, and it clears the outcome here.
  expect_true(any(fl$is_outcome))
  expect_false(any(cov$outcome_test$flag))
  expect_lt(cov$outcome_test$effect[1], 0.1)
  expect_output(print(cov), "complete cases stay unbiased")

  out <- ilm_check_missing(mar_data(on = "outcome"), y = "y", verbose = FALSE)
  expect_equal(out$verdict, "RELATED_TO_OUTCOME")
  expect_true(any(out$outcome_test$flag))
  expect_gt(out$outcome_test$effect[1], 0.2)
  expect_output(print(out), "complete cases are biased")

  ## and missingness unrelated to anything is not talked up
  expect_equal(ilm_check_missing(mar_data(on = "none"), y = "y",
                                 verbose = FALSE)$verdict, "MCAR_NOT_REJECTED")
})

test_that("check_missing needs an effect as well as a p-value", {
  ## a trivial association at a large n is significant and not worth acting on
  set.seed(5); n <- 6000
  d <- data.frame(z = rnorm(n), y = rnorm(n))
  d$x <- rnorm(n)
  d$x[plogis(0.06 * d$z - 0.8) > runif(n)] <- NA
  loose <- ilm_check_missing(d, y = "y", min_effect = 0.001, verbose = FALSE)
  strict <- ilm_check_missing(d, y = "y", min_effect = 0.3, verbose = FALSE)
  expect_gte(sum(loose$associations$flag), sum(strict$associations$flag))
  expect_equal(strict$verdict, "MCAR_NOT_REJECTED")
})

test_that("check_missing detects a monotone pattern", {
  set.seed(3); n <- 200
  d <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  drop <- sample(n, 60)
  d$b[drop] <- NA
  d$c[drop] <- NA; d$c[sample(setdiff(seq_len(n), drop), 30)] <- NA
  expect_true(ilm_check_missing(d, verbose = FALSE)$monotone)

  d2 <- d; d2$b[sample(n, 40)] <- NA      # now they cross
  expect_false(ilm_check_missing(d2, verbose = FALSE)$monotone)
})

## --- the signature three vignettes assumed -----------------------------------
## `y ~ x + z` reads as exactly what this function asks, and three vignettes
## reached for it independently while the signature took a column name. Rather
## than correct three call sites, the function now takes both.

test_that("a formula and the string form give the same answer", {
  set.seed(2)
  d <- data.frame(x = rnorm(200), z = rnorm(200))
  d$y <- 0.4 * d$x + rnorm(200)
  d$y[d$x > 1] <- NA
  a <- ilm_check_missing(d, y ~ x + z, verbose = FALSE)
  b <- ilm_check_missing(d, y = "y", covariates = c("x", "z"), verbose = FALSE)
  expect_equal(a, b)
})

test_that("a malformed formula is refused with the shape it wanted", {
  set.seed(2)
  d <- data.frame(x = rnorm(50), z = rnorm(50))
  d$y <- rnorm(50); d$y[1:5] <- NA
  expect_error(ilm_check_missing(d, ~ x + z, verbose = FALSE), "needs a response")
  expect_error(ilm_check_missing(d, y + z ~ x, verbose = FALSE), "one outcome")
  expect_error(ilm_check_missing(d, y = 42, verbose = FALSE), "single string")
  expect_error(ilm_check_missing(d, y ~ nope, verbose = FALSE), "not in `data`")
})
