## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## S3's results taken apart for the default question: how much of the
## ten-fold arms' gain each change buys, and at what cost in time. Written
## after the run, on its CSVs; descriptive only, it adds no rule.
##   grid     wide - grid6                   the wider grid, three folds, cold
##   folds    cold3_10 - wide                the ten-fold split alone: the same
##                                           grid, cold, three held-out folds
##                                           either way, so each fit trains on
##                                           nine tenths of the observed cells
##                                           rather than two thirds
##   warm     path3_10_fixed - cold3_10      the warm path on that split
##   total    path3_10_fixed - grid6         the arm R3 adopts, against today's
## Errors pair by replicate; wide and grid6 are the registered run's, which
## the check below finds equal to addendum 2's own fits. Seconds are
## addendum 2's, all arms timed in one session.
## Usage: Rscript dev/studies/glrm_penalty_contrasts.R registered.csv... addendum2.csv...
options(width = 200)
res <- do.call(rbind, lapply(commandArgs(TRUE), utils::read.csv))
ALL <- c("numeric_150", "numeric_400", "numeric_1000", "mixed_200", "mixed_1000")
conds <- intersect(ALL, unique(res$cond))
err <- function(cd, a) { r <- res[res$cond == cd & res$arm == a, ]; r$error[order(r$rep)] }
se <- function(z) stats::sd(z) / sqrt(length(z))
med <- function(cd, a) stats::median(res$seconds[res$cond == cd & res$arm == a])

same <- vapply(conds, function(cd) identical(err(cd, "grid6"), err(cd, "grid6_a2")) &&
                 identical(err(cd, "wide"), err(cd, "wide_a2")), TRUE)
cat("check: the registered run's grid6 and wide errors equal addendum 2's, replicate by replicate:",
    all(same), "\n")

parts <- list(grid = c("wide", "grid6"), folds = c("cold3_10", "wide"),
              warm = c("path3_10_fixed", "cold3_10"), total = c("path3_10_fixed", "grid6"))
cat("\nerror differences, mean (paired SE), and as a share of grid6's mean error\n")
tab <- do.call(rbind, lapply(conds, function(cd) {
  g <- mean(err(cd, "grid6"))
  data.frame(cond = cd, grid6_error = sprintf("%.3f", g),
             t(vapply(parts, function(ab) {
               z <- err(cd, ab[1]) - err(cd, ab[2])
               sprintf("%+.4f (%.4f) %+.1f%%", mean(z), se(z), 100 * mean(z) / g)
             }, "")), check.names = FALSE)
}))
print(tab, row.names = FALSE)

cat("\nmedian seconds per selection (addendum 2's session) and over grid6_a2's\n")
arms <- c("grid6_a2", "wide_a2", "cold3_10", "path3_10_fixed", "path1_fixed")
tm <- do.call(rbind, lapply(conds, function(cd) {
  g <- med(cd, "grid6_a2")
  data.frame(cond = cd, t(vapply(arms, function(a)
    sprintf("%.2f (x%.2f)", med(cd, a), med(cd, a) / g), "")), check.names = FALSE)
}))
print(tm, row.names = FALSE)
