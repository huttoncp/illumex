# ilm_glrm() chooses its penalty on the grid 0.1 to 100,
# ten folds with three held out, a warm path down the grid with a cold
# restart after a collapse: arm path3_10_fixed of dev/studies/glrm_penalty.R.
# Held to the choices that study recorded for that arm, replicate by
# replicate, on the study's own data generators and seeds.

## dev/studies/glrm_penalty.R's generators and seeds, unchanged
penalty_numeric <- function(s, n = 150L, p = 4L, miss = 0.2) {
  set.seed(1000L * (n %/% 50L) + s)
  X <- matrix(stats::rnorm(n * 2), n, 2) %*% matrix(stats::rnorm(2 * p), 2, p) +
    matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  M <- matrix(stats::runif(n * p) < miss, n, p)
  M[rowSums(M) == p, 1] <- FALSE
  Xm <- X; Xm[M] <- NA
  colnames(Xm) <- paste0("x", seq_len(p))
  as.data.frame(Xm)
}
penalty_mixed <- function(s, n = 200L) {
  set.seed(400000L + 10L * (n %/% 100L) + 1000L * s)
  g <- rep(1:3, length.out = n)
  f1 <- c(-2, 0, 2)[g] + stats::rnorm(n, 0, 0.6); f2 <- stats::rnorm(n)
  d <- data.frame(a = f1 + stats::rnorm(n, 0, 0.4), b = -f1 + stats::rnorm(n, 0, 0.4),
                  c = f2 + stats::rnorm(n, 0, 0.4))
  d$y <- factor(ifelse(stats::plogis(2.5 * f1) > stats::runif(n), "hi", "lo"))
  noisef <- function(L) factor(sample(paste0("l", seq_len(L)), n, TRUE))
  h <- f2 + stats::rnorm(n, 0, 0.4)
  d$w <- factor(cut(h, stats::quantile(h, 0:6 / 6), include.lowest = TRUE, labels = FALSE))
  d$z1 <- noisef(3)
  M <- matrix(stats::runif(n * ncol(d)) < 0.10, n, ncol(d))
  M[rowSums(M) == ncol(d), 1] <- FALSE
  for (j in seq_len(ncol(d))) d[[j]][M[, j]] <- NA
  d
}
chosen <- function(d) suppressWarnings(ilm_glrm(d, lambda = NULL, progress = FALSE))$lambda

test_that("the choice is the study's, replicate by replicate", {
  ## the study's choices for arm path3_10_fixed, replicates 1 to 5 (its
  ## per-replicate CSVs are not kept; dev/studies/results/glrm_penalty.txt
  ## gives the counts over all 50)
  expect_identical(vapply(1:5, function(s) chosen(penalty_numeric(s)), 0),
                   c(2, 10, 5, 5, 10))
  skip_on_cran()
  expect_identical(chosen(penalty_mixed(1)), 5)
})

test_that("the grid runs from 0.1 to 100, and a warm start is never left set", {
  expect_identical(ILM_GLRM_GRID, c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100))
  invisible(chosen(penalty_numeric(1)))
  expect_null(.ilm_glrm_start$fit)
  ## a fit given its penalty is not affected by the selection's machinery
  d <- penalty_numeric(2)
  a <- ilm_glrm(d, lambda = 5, progress = FALSE)
  .ilm_glrm_start$fit <- NULL
  b <- ilm_glrm(d, lambda = 5, progress = FALSE)
  expect_identical(a$fitted, b$fitted)
})
