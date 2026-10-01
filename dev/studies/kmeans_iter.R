## How many k-means iterations ilm_cluster() should allow. At stats::kmeans()'s
## default of 10, the census study (dev/studies/census) saw 128 starts warn
## "did not converge in 10 iterations"; on its data's own coordinates no
## start fails, so they come from the gap statistic's uniform reference sets.
## Measured here on simulated data of the census study's shape -- 5,000 rows
## by 5 correlated dimensions, a continuum with no clusters -- so the census
## data are not reanalysed: the gap statistic exactly as ilm_cluster() runs
## it (cluster::clusGap(), k = 1 to 10, 25 starts, B reference sets), at each
## iter.max, counting the starts that stop before converging, with the gap
## curve and chosen k beside those at 1,000 iterations (taken as converged).
## Usage: Rscript dev/studies/kmeans_iter.R [B]
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
B <- if (length(commandArgs(TRUE))) as.integer(commandArgs(TRUE)[1]) else 100L
set.seed(2026)
S <- 0.6 ^ abs(outer(1:5, 1:5, "-"))
X <- matrix(stats::rnorm(5000 * 5), 5000, 5) %*% chol(S)
gap_at <- function(it) {
  n_warn <- 0L; n_start <- 0L
  fk <- function(x, k) {
    n_start <<- n_start + 25L
    list(cluster = withCallingHandlers(
      stats::kmeans(x, centers = k, nstart = 25, iter.max = it)$cluster,
      warning = function(w) {
        if (grepl("did not converge", conditionMessage(w))) n_warn <<- n_warn + 1L
        invokeRestart("muffleWarning") }))
  }
  set.seed(1)
  t0 <- proc.time()[["elapsed"]]
  g <- cluster::clusGap(X, FUNcluster = fk, K.max = 10, B = B, verbose = FALSE)
  list(gap = g$Tab[, "gap"], se = g$Tab[, "SE.sim"], warn = n_warn, starts = n_start,
       k = cluster::maxSE(g$Tab[, "gap"], g$Tab[, "SE.sim"], method = "firstSEmax"),
       secs = proc.time()[["elapsed"]] - t0)
}
its <- c(10L, 30L, 100L, 1000L)
fits <- lapply(its, gap_at)
ref <- fits[[length(fits)]]
res <- data.frame(iter_max = its,
                  starts_not_converged = vapply(fits, `[[`, 0L, "warn"),
                  starts = vapply(fits, `[[`, 0L, "starts"),
                  k_chosen = vapply(fits, `[[`, 0, "k"),
                  max_gap_shift_in_se = vapply(fits, function(f) max(abs(f$gap - ref$gap) / ref$se), 0),
                  seconds = round(vapply(fits, `[[`, 0, "secs"), 1))
cat("simulated continuum, 5,000 x 5; gap statistic with B =", B, "\n")
print(res, row.names = FALSE)
