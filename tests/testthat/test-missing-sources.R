## Worked examples from the published sources, as tests of ilm_check_missing().

test_that("the screen's chi-squared on Little and Rubin's Table 13.1 is Pearson's, uncorrected", {
  ## Little and Rubin (2019), Example 13.2, Table 13.1 (pp. 305-306): Y1 is
  ## recorded for every unit and Y2 is missing for 190 of the 410. Y1 = 1 for
  ## 90 of the 220 complete units and 100 of the 190 incomplete ones. The
  ## 2 x 2 table of Y1 by "Y2 missing" is [[90, 100], [130, 90]], with
  ## expected counts 101.95, 88.05 / 118.05, 101.95, and Pearson's statistic
  ## without a continuity correction is 5.634 (Yates' would be 5.173, G^2
  ## 5.643). The book prints 5.23 with p < 0.01, which none of these
  ## reproduces (5.23 on 1 df is p = 0.022): it is an outside comparison
  ## only, and this test asserts the Pearson value.
  y1 <- factor(c(rep(1, 90), rep(2, 130), rep(1, 100), rep(2, 90)))
  y2 <- c(rep(1, 220), rep(NA, 190))
  d <- data.frame(y1 = y1, y2 = y2)
  r <- ilm_check_missing(d, verbose = FALSE)
  a <- r$associations[r$associations$missing_in == "y2" &
                        r$associations$related_to == "y1", ]
  expect_equal(nrow(a), 1L)
  x2 <- stats::qchisq(a$p_value, 1, lower.tail = FALSE)
  expect_equal(x2, 5.634086, tolerance = 1e-6)
  ## Cramer's V, sqrt(X^2 / (n min(I - 1, J - 1))) (Agresti 2013, p. 110)
  expect_equal(a$effect, sqrt(5.634086 / 410), tolerance = 1e-6)
})

test_that("the verdict follows Hughes et al.'s Table 1 in the linear case", {
  ## Hughes et al. (2019), Table 1 (p. 1297): for a linear regression of the
  ## outcome y on the exposure x (and the confounder z), complete cases are
  ## unbiased when completeness depends on nothing or on the exposure, and
  ## biased in general when it depends on the outcome. One large sample per
  ## mechanism.
  sim <- function(on, seed, n = 3000) {
    set.seed(seed)
    z <- stats::rnorm(n); x <- 0.5 * z + stats::rnorm(n)
    y <- 0.4 * x + 0.3 * z + stats::rnorm(n)
    p <- switch(on, none = rep(0.3, n), x_self = stats::plogis(1.2 * x - 0.6),
                z_by_x = stats::plogis(1.2 * x - 0.6),
                outcome = stats::plogis(1.2 * y - 0.6))
    miss <- stats::runif(n) < p
    if (on == "z_by_x") z[miss] <- NA else x[miss] <- NA
    data.frame(x = x, z = z, y = y)
  }
  v <- function(on, seed) ilm_check_missing(sim(on, seed), y ~ x + z, verbose = FALSE)$verdict
  ## none: MCAR
  expect_identical(v("none", 1), "MCAR_NOT_REJECTED")
  ## the confounder goes missing according to the observed exposure: the
  ## "Exposure" row, which the outcome test clears
  expect_identical(v("z_by_x", 2), "RELATED_TO_COVARIATES")
  ## missingness in the exposure driven by the outcome: the "Outcome" row
  expect_identical(v("outcome", 3), "RELATED_TO_OUTCOME")
  ## A KNOWN LIMIT, documented in the help: the exposure goes missing by its
  ## own unseen value. Complete cases are unbiased here (the "Exposure" row;
  ## Little and Rubin 2019, Example 3.3, p. 49), but the test cannot hold the
  ## unseen x fixed, the outcome carries information about it, and the
  ## outcome is flagged. Hence "may be biased".
  expect_identical(v("x_self", 2), "RELATED_TO_OUTCOME")
})
