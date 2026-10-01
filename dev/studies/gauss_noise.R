## How far normal data sit from the best-fitting normal, by sampling noise
## alone: the null distribution of the distance ilm_gauss_check() measures.
##
## ilm_gauss_check() subtracts 0.9/sqrt(n) from the distance before scoring,
## and its documentation says noise typically gives about 0.6/sqrt(n) and 95%
## of the time no more than about 0.9/sqrt(n). This measures both: for each n,
## `reps` normal samples, the distance scaled by sqrt(n), and its mean, median
## and upper percentiles, with the share of samples at or below 0.6 and 0.9.
## Lilliefors (1967) gives 0.886/sqrt(n) as the large-sample 5% critical value.
##
## Run from the package root:
##   Rscript dev/studies/gauss_noise.R [reps] [outfile.csv]
pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 20000L
out <- if (length(args) >= 2L) args[2] else NA_character_

set.seed(20260925)
ns <- c(20L, 50L, 100L, 500L, 2000L)
tab <- do.call(rbind, lapply(ns, function(n) {
  d <- vapply(seq_len(reps), function(r) ilm_gauss_d(stats::rnorm(n)), 0) *
    sqrt(n)
  q <- stats::quantile(d, c(0.5, 0.8, 0.9, 0.95, 0.99), names = FALSE)
  data.frame(n = n, mean = mean(d), median = q[1], q80 = q[2], q90 = q[3],
             q95 = q[4], q99 = q[5], share_le_0.6 = mean(d <= 0.6),
             share_le_0.9 = mean(d <= 0.9))
}))
print(format(tab, digits = 3), row.names = FALSE)
if (!is.na(out)) utils::write.csv(tab, out, row.names = FALSE)
