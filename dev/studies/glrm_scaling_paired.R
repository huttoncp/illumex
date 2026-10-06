## S1, read in pairs. The arms of dev/studies/glrm_scaling.R fit the same
## simulated data in each replicate, so two arms are compared replicate by
## replicate:
##   s2z - current, over the replicates s2z ran (1 to 10): the check of the
##     argument in the design that centring the multinomial blocks is a
##     no-op. Reported as the largest absolute difference on each outcome.
##   scaled - current, over all 50: the mean paired difference and its
##     standard error, the reading that ranking two arms asks for.
##   How often each arm's lambda sat at the top of the cross-validation grid.
##
## Rscript dev/studies/glrm_scaling_paired.R part1.csv [part2.csv ...]
## with the CSVs glrm_scaling.R wrote.
files <- commandArgs(TRUE)
d <- do.call(rbind, lapply(files, utils::read.csv))
out <- c("noise_share", "latent_r2", "ari", "err_numeric", "err_binary",
         "err_info", "lambda")
key <- c("scenario", "n", "rep")
lv <- c("noise_L3", "noise_L10", "noise3_L3", "info_L6")
options(width = 200)

pairs <- function(arm) {
  m <- merge(d[d$arm == "current", c(key, out)], d[d$arm == arm, c(key, out)],
             by = key, suffixes = c(".cur", ".arm"))
  m[order(match(m$scenario, lv), m$n, m$rep), ]
}
by_cell <- function(m, f) {
  r <- do.call(rbind, lapply(split(m, list(m$scenario, m$n), drop = TRUE), function(g)
    data.frame(scenario = g$scenario[1], n = g$n[1], pairs = nrow(g),
               t(vapply(out, function(o) {
                 dd <- g[[paste0(o, ".arm")]] - g[[paste0(o, ".cur")]]
                 dd <- dd[!is.na(dd)]
                 if (length(dd)) f(dd) else ""
               }, "")), check.names = FALSE)))
  r[order(match(r$scenario, lv), r$n), ]
}

m <- pairs("s2z")
cat("s2z - current: largest absolute paired difference\n")
print(by_cell(m, function(dd) format(max(abs(dd)), digits = 2)),
      row.names = FALSE, right = TRUE)

m <- pairs("scaled")
cat("\nscaled - current: mean paired difference (standard error)\n")
print(by_cell(m, function(dd) sprintf("%+.3f (%.3f)", mean(dd),
                                      stats::sd(dd) / sqrt(length(dd)))),
      row.names = FALSE, right = TRUE)

cat("\nshare of fits whose lambda is the top of the grid (10)\n")
top <- stats::aggregate(cbind(current = lambda.cur == 10, scaled = lambda.arm == 10) ~
                          scenario + n, data = m, FUN = mean)
print(top[order(match(top$scenario, lv), top$n), ], row.names = FALSE)
