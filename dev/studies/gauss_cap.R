## Where clear departures from normality land on ilm_gauss_check()'s scale.
##
## Backs the calibration of the `cap` default (0.12) in R/ilm_describe.R and
## ?ilm_gauss_check: the excess distance -- the Kolmogorov distance to the
## best-fitting normal less the 95th percentile of noise, 0.9/sqrt(n) -- for a
## lognormal, a t(3), a two-component mixture and a Poisson, at the size the
## examples use (n = 500), and at n = 100,000 where the excess is close to its
## population value.
##
## Distributions: lognormal(0, 1); t with 3 df; an equal mixture of N(0, 1)
## and N(5, 1), as in the examples; Poisson with mean 4. A normal is included
## as the reference, whose excess should sit at or below 0. Fixed before any
## full run: 200 replicates at n = 500 and 10 at n = 100,000; every mean is
## printed with its Monte Carlo standard error.
##
## Run from the package root:
##   Rscript dev/studies/gauss_cap.R [reps] [outfile.csv]
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 200L
out <- if (length(args) >= 2L) args[2] else NA_character_

draw <- list(
  normal     = function(n) stats::rnorm(n),
  lognormal  = function(n) stats::rlnorm(n),
  t3         = function(n) stats::rt(n, 3),
  bimodal    = function(n) c(stats::rnorm(n %/% 2), stats::rnorm(n - n %/% 2, 5)),
  poisson    = function(n) stats::rpois(n, 4))

set.seed(20260925)
rows <- list()
for (nm in names(draw)) for (n in c(500L, 100000L)) {
  r <- if (n > 10000L) max(10L, reps %/% 20L) else reps
  ex <- vapply(seq_len(r), function(i) {
    x <- draw[[nm]](n)
    ilm_gauss_d(x) - 0.9 / sqrt(n)
  }, 0)
  g <- vapply(seq_len(min(r, 50L)), function(i)
    ilm_gauss_check(draw[[nm]](n))$gauss, 0)
  rows[[length(rows) + 1L]] <- data.frame(distribution = nm, n = n, reps = r,
    excess = mean(ex), excess_mc_se = stats::sd(ex) / sqrt(r),
    gauss_at_cap_0.12 = mean(g), gauss_mc_se = stats::sd(g) / sqrt(length(g)))
}
res <- do.call(rbind, rows)
print(format(res, digits = 3), row.names = FALSE)
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
