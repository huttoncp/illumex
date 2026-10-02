# The tail check (Craig's items 88, 144 and 282): ilm_anomaly() compares its
# residuals' tails with the reference's and warns at p <= 0.05, since on
# heavy-tailed data the scan flags rows that are not anomalous. The check
# draws nothing of its own, so the scores and p-values are as before.

rank2 <- function(seed, noise) {
  set.seed(seed)
  n <- 400; p <- 8
  W <- matrix(stats::rnorm(2 * p), 2, p)
  X <- matrix(stats::rnorm(n * 2), n, 2) %*% W + matrix(noise(n * p), n, p)
  as.data.frame(X)
}

test_that("heavy-tailed residuals warn, normal ones seldom do", {
  warns <- function(noise) vapply(1:10, function(seed) {
    w <- FALSE
    a <- withCallingHandlers(ilm_anomaly(rank2(seed, noise), seed = 1),
                             warning = function(c) {
                               if (grepl("heavier tails", conditionMessage(c))) w <<- TRUE
                               invokeRestart("muffleWarning")
                             })
    expect_identical(w, attr(a, "tail_p") <= 0.05)
    w
  }, TRUE)
  ## the study's false-warning rate on clean normal noise is up to about 0.10
  expect_lte(sum(warns(function(m) stats::rnorm(m, sd = 0.5))), 2L)
  ## and on t with 3 degrees of freedom, at 400 rows and 8 columns, 0.99 to 1.00
  expect_gte(sum(warns(function(m) stats::rt(m, 3) * 0.5)), 8L)
})

test_that("the tail statistic is the 99th over the 75th percentile, on the rows kept", {
  set.seed(3)
  Z <- scale(matrix(stats::rnorm(200 * 5), 200, 5))
  f <- ilm_anom_score(Z, 1L, 0.25)
  keep <- order(f$score)[seq_len(150)]
  R <- f$residual[keep, ]
  a <- abs(sweep(sweep(R, 2, apply(R, 2, stats::median)), 2, apply(R, 2, stats::mad), "/"))
  expect_equal(ilm_anom_tail(f, 1L, 0.25),
               unname(stats::quantile(a, 0.99) / stats::quantile(a, 0.75)))
})
