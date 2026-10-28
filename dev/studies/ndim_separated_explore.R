## EXPLORATORY, not a registered study, and it changes nothing by itself.
## Why, in ndim_choice.R's full run, the oracle (3 dimensions) did worst on
## the cluster-options study's separated design: k = 1 in 13 of 20, where
## five dimensions found k = 4 in 13 of 20. The lead: the gap statistic's
## first-SE-max rule stops at the first k with gap(k) >= gap(k+1) - SE(k+1);
## with four clusters at the corners of a 3-dimensional simplex a split in
## two may not beat the reference, so it stops at 1 though the curve peaks
## at 4. Here the same replicates are re-run exactly (same data, same seeds,
## same ilm_reduce() and ilm_cluster() calls), the gap curves kept, and every
## rule cluster::maxSE() offers applied to the same curves.
##
## Run from the package root at the study's commit:
##   Rscript dev/studies/ndim_separated_explore.R <out folder>
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
out <- commandArgs(TRUE)[1]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
q <- function(e) suppressWarnings(suppressMessages(e))

## ndim_choice.R's options_design("separated"), seeded as it seeds condition 7
separated <- function(n = 1000L) {
  p <- 5L
  centres <- rbind(c(0, 0, 0, 0, 0), c(6, 0, 0, 0, 0), c(0, 6, 0, 0, 0), c(0, 0, 6, 0, 0))
  g <- sample.int(4L, n, replace = TRUE)
  list(g = g, d = as.data.frame(centres[g, ] + matrix(stats::rnorm(n * p), n, p)))
}
RULES <- c("firstSEmax", "Tibs2001SEmax", "globalSEmax", "firstmax", "globalmax")
rows <- list(); curves <- list()
for (rep in 1:20) {
  seed <- 1000L * 7L + rep
  set.seed(seed)
  x <- separated()
  for (nd in c(3L, 5L)) {
    r <- q(ilm_reduce(x$d, ndim = nd))
    cl <- q(ilm_cluster(r, k_max = 10L, B = 50L, seed = seed))
    tab <- cl$gap$Tab
    ks <- vapply(RULES, function(m) cluster::maxSE(tab[, "gap"], tab[, "SE.sim"], method = m), 1L)
    rows[[length(rows) + 1L]] <- data.frame(rep = rep, ndim = nd, k_ilm = cl$k, t(ks))
    curves[[length(curves) + 1L]] <- data.frame(rep = rep, ndim = nd, k = seq_len(nrow(tab)),
                                                gap = tab[, "gap"], se = tab[, "SE.sim"])
    cat(sprintf("rep %2d ndim %d: k %d  %s\n", rep, nd, cl$k, paste(RULES, ks, sep = "=", collapse = " ")))
  }
}
res <- do.call(rbind, rows); cv <- do.call(rbind, curves)
utils::write.csv(res, file.path(out, "ndim_separated_rules.csv"), row.names = FALSE)
utils::write.csv(cv, file.path(out, "ndim_separated_curves.csv"), row.names = FALSE)
cat("\nk = 4 in 20 replicates, by rule and ndim\n")
print(aggregate(cbind(firstSEmax, Tibs2001SEmax, globalSEmax, firstmax, globalmax) ~ ndim,
                data = res, FUN = function(k) sum(k == 4L)))
cat("\nk chosen by firstSEmax, by ndim\n")
print(table(res$ndim, res$firstSEmax))

## the curves: gap +- SE by k, the oracle's k = 1 replicates and fixed5's k = 4 ones
grDevices::png(file.path(out, "ndim_separated_gap_curves.png"), width = 1000, height = 500, res = 100)
graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
for (sel in list(list(nd = 3L, k = 1L, main = "3 dimensions (oracle), replicates choosing k = 1"),
                 list(nd = 5L, k = 4L, main = "5 dimensions (fixed5), replicates choosing k = 4"))) {
  reps <- res$rep[res$ndim == sel$nd & res$k_ilm == sel$k]
  sub <- cv[cv$ndim == sel$nd & cv$rep %in% reps, ]
  graphics::plot(NA, xlim = c(1, 10), ylim = range(c(sub$gap - sub$se, sub$gap + sub$se)),
                 xlab = "k", ylab = "gap (+- SE)", main = sel$main, cex.main = 0.9)
  for (rp in reps) {
    s <- sub[sub$rep == rp, ]
    graphics::lines(s$k, s$gap, col = "grey40")
    graphics::segments(s$k, s$gap - s$se, s$k, s$gap + s$se, col = "grey70")
  }
  graphics::abline(v = 4, lty = 3)
}
grDevices::dev.off()
