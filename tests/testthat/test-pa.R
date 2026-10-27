## Permutation parallel analysis (R/ilm_pa.R). The engine permutes rows of the
## encoded matrix, one row order per original column; dev/studies/ndim_choice.R
## permuted the data and reduced it again. On the same seed the two must agree,
## since sample(x) draws sample.int(n) once per column as the engine does.

pa_old <- function(d, B, q = 0.95, seed) {
  set.seed(seed)
  obs <- ilm_reduce(d, ndim = 1L)$eig$eigenvalue
  perm <- vapply(seq_len(B), function(b) {
    e <- ilm_reduce(as.data.frame(lapply(d, sample)), ndim = 1L)$eig$eigenvalue
    e[seq_along(obs)]
  }, numeric(length(obs)))
  list(eigenvalue = obs,
       threshold = apply(perm, 1L, stats::quantile, q, na.rm = TRUE, names = FALSE))
}

pa_new <- function(d, B, q = 0.95, seed) {
  set.seed(seed)
  ilm_pa_inputs(ilm_reduce_inputs(d, names(d), "cycles"), B = B, q = q)
}

## largest relative difference over the eigenvalues that are not zero up to
## rounding: the cross-product route squares the condition number, so an
## absolute tolerance would not hold for the smallest
rel_diff <- function(a, b) {
  k <- min(length(a), length(b))
  a <- a[seq_len(k)]; b <- b[seq_len(k)]
  big <- abs(b) > 1e-8 * max(abs(b))
  max(abs(a[big] - b[big]) / abs(b[big]))
}

pa_data <- function(n = 200L) {
  set.seed(11)
  z <- stats::rnorm(n)
  d <- data.frame(a = z + stats::rnorm(n, 0, 0.5), b = -z + stats::rnorm(n, 0, 0.5),
                  c = stats::rnorm(n),
                  g = factor(ifelse(z > 0.3, "hi", ifelse(z < -0.3, "lo", "mid"))),
                  h = factor(sample(c("p", "q", "r", "s"), n, TRUE)))
  d
}

test_that("the engine's thresholds equal re-reducing permuted data, mixed columns", {
  d <- pa_data()
  old <- pa_old(d, B = 25L, seed = 3)
  new <- pa_new(d, B = 25L, seed = 3)
  expect_lt(rel_diff(new$eigenvalue, old$eigenvalue), 1e-8)
  expect_lt(rel_diff(new$threshold, old$threshold), 1e-8)
})

test_that("... numeric columns only, and categorical only", {
  d <- pa_data()
  num <- d[c("a", "b", "c")]
  cat <- d[c("g", "h")]
  for (x in list(num, cat)) {
    old <- pa_old(x, B = 20L, seed = 5)
    new <- pa_new(x, B = 20L, seed = 5)
    expect_lt(rel_diff(new$eigenvalue, old$eigenvalue), 1e-8)
    expect_lt(rel_diff(new$threshold, old$threshold), 1e-8)
  }
})

test_that("... with missing numbers and missing categories", {
  d <- pa_data()
  d$a[c(3, 17, 40)] <- NA
  d$g[c(5, 9)] <- NA
  d$h[c(1, 60, 61)] <- NA
  old <- pa_old(d, B = 20L, seed = 7)
  new <- pa_new(d, B = 20L, seed = 7)
  expect_lt(rel_diff(new$eigenvalue, old$eigenvalue), 1e-8)
  expect_lt(rel_diff(new$threshold, old$threshold), 1e-8)
})

test_that("the cross-product route gives the SVD's eigenvalues", {
  d <- pa_data()
  Z <- ilm_famd_z(d[c("a", "b", "c")], d[c("g", "h")])$Z
  sv <- svd(Z / sqrt(nrow(Z)), nu = 0L, nv = 0L)$d^2
  expect_lt(rel_diff(ilm_pa_eig(Z), sv), 1e-8)
  ## and with more columns than rows, by the SVD itself, padded with zeros
  W <- Z[1:4, ]
  e <- ilm_pa_eig(W)
  expect_length(e, ncol(W))
  expect_lt(rel_diff(e, svd(W / 2, nu = 0L, nv = 0L)$d^2), 1e-8)
})

test_that("a known rank is found, and noise gives one dimension", {
  set.seed(21)
  n <- 400L
  L <- matrix(stats::rnorm(2 * n), n, 2)
  X <- L %*% matrix(stats::rnorm(2 * 8), 2, 8) + matrix(stats::rnorm(8 * n, 0, 0.5), n, 8)
  d <- as.data.frame(X)
  set.seed(1)
  expect_equal(ilm_pa_inputs(ilm_reduce_inputs(d, names(d), "cycles"))$ndim, 2L)
  noise <- as.data.frame(matrix(stats::rnorm(8 * n), n, 8))
  set.seed(1)
  expect_equal(ilm_pa_inputs(ilm_reduce_inputs(noise, names(noise), "cycles"))$ndim, 1L)
})

test_that("a date's encoded columns move together, as one column", {
  set.seed(4)
  n <- 300L
  day <- as.Date("2020-01-01") + sample(0:1500, n, TRUE)
  d <- data.frame(day = day, x = stats::rnorm(n),
                  y = 3 * sin(2 * pi * as.numeric(format(day, "%j")) / 365) +
                    stats::rnorm(n))
  inp <- suppressMessages(ilm_reduce_inputs(d, names(d), "cycles"))
  enc <- ilm_famd_z(inp$quanti, inp$quali)
  ## its elapsed time and the time-of-year sine and cosine
  expect_equal(sum(enc$owner %in% names(inp$tmap$day)), 3L)
  ## one row order for the date, so its sine and cosine stay a point on the
  ## circle; the orders are drawn in column order, day, x, y
  set.seed(2)
  res <- ilm_pa_inputs(inp, B = 1L)
  expect_length(res$threshold, inp$max_dim)
  set.seed(2)
  ord <- list(sample.int(n), sample.int(n), sample.int(n))
  grp <- ifelse(enc$owner %in% names(inp$tmap$day), "day", enc$owner)
  Zp <- enc$Z
  for (i in 1:3) {
    j <- grp == c("day", "x", "y")[i]
    Zp[, j] <- enc$Z[ord[[i]], j, drop = FALSE]
  }
  ## with one permutation its percentile is itself
  expect_lt(rel_diff(res$threshold, ilm_pa_eig(Zp)), 1e-8)
})
