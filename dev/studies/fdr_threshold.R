## How the false-discovery level behind ilm_anomaly()'s flags shapes how many
## anomalies are flagged, and at what cost.
##
## The trim grid found the reconstruction ranking planted rows well long before
## it flags them: at 4 noise sd along random directions it flagged 0.16 of
## them with an AUC near 0.94. The question was whether the threshold is the reason.
##
## What the flag rule is now (R/ilm_anomaly.R): each row's p-value comes from
## a simulated reference, not a formula. B = 39 datasets are simulated from
## the fitted structure with no anomalies and scored exactly as the data
## were, and their n x B scores are pooled; a row's p is (1 + the number of
## reference scores at least as large) / (n B + 1), so the smallest p it can
## have is 1 / (n B + 1), 0.000064 at n = 400. The p-values are then adjusted
## by Benjamini-Hochberg across the n rows, and a row is flagged when its
## adjusted p is below `alpha`, 0.05 by default. The rule is therefore already
## false-discovery-rate control at q = 0.05; after the adjustment a lone row
## cannot fall much below n / (n B + 1), about 1 / B = 0.026, which is why the
## function warns when B * alpha <= 1.
##
## Design, fixed before any full run.
##   Data: trim_grid.R's cells and datasets -- n = 400, p = 8, rank 2; shared
##   10%, shared 5% and random 5%; shifts of 3 to 9 noise sd; 50 replicates --
##   with the seeds its run used (base 100000 for shared10 and for shared5,
##   200000 for random5, since it ran the conditions in two parts), at
##   trim = 0.25 (the default) and trim = 0. Clean data for the false flags:
##   anomaly_null.R's design (400 x 8, rank 2, no anomalies), 100 scans at
##   each trim.
##   Rules, all on the same fit's p-values: Benjamini-Hochberg at q = 0.05
##   (the current rule, checked to reproduce ilm_anomaly()'s own `flag`
##   exactly) and at q = 0.10; and, added before the run on request,
##   Storey's adaptive Benjamini-Hochberg at q = 0.05 and 0.10, which scales
##   the adjusted p-values by an estimate of the share of rows that are not
##   anomalous, pi0 = #{p > 0.5} / (0.5 m), capped at 1 (Storey 2002, with the
##   usual lambda = 0.5; Storey, Taylor and Siegmund 2004). The mean pi0 is
##   reported per cell.
##   Reported per cell, each with its Monte Carlo standard error: the share of
##   planted rows flagged; the realised false-discovery proportion among the
##   flags (0 when nothing is flagged; its mean is the false-discovery rate);
##   the share of other rows flagged; and the AUC, which no rule changes, as
##   a check. On clean data: the share of rows flagged and the share of scans
##   flagging anything (with no anomalies, every flag is false, so that share
##   is the false-discovery rate).
##   Every fit's per-row p-values and labels are saved (outfile.rds), so any
##   other rule asked about later can be computed from this run without
##   refitting.
##   Whether the reference's size B is what limits the flags (added before
##   the run, as a count that changes no fit): per cell, with Monte Carlo
##   standard errors, the share of planted rows whose raw p is at the floor
##   1 / (n B + 1), and the share whose adjusted p is within a factor of 2 of
##   the smallest adjusted p any row can reach in that fit -- floor x n / m
##   when m rows sit at the raw floor, floor x n when none does. Many planted
##   rows at or near the floor where few are flagged would mean a larger B
##   could flag more; few would mean the scores themselves sit inside the
##   reference.
##
## Run from the package root:
##   Rscript dev/studies/fdr_threshold.R [reps] [outfile.csv] [parts]
## `parts` (optional) is a comma-separated subset of shared10, shared5,
## random5, clean; the design is the same.
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 50L
out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else NA_character_
parts <- if (length(args) >= 3L) strsplit(args[3], ",")[[1]] else
  c("shared10", "shared5", "random5", "clean")

auc <- function(score, lab) {
  r <- rank(score)
  (sum(r[lab]) - sum(lab) * (sum(lab) + 1) / 2) / (sum(lab) * sum(!lab))
}
## trim_grid.R's generator, unchanged
make_data <- function(seed, share, how, shift, n = 400L, p = 8L) {
  set.seed(seed)
  W <- matrix(stats::rnorm(2 * p), 2, p)
  X <- matrix(stats::rnorm(n * 2), n, 2) %*% W +
    matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  i <- sample(n, round(share * n))
  P <- diag(p) - t(W) %*% solve(W %*% t(W)) %*% W
  unit <- function() { v <- P %*% stats::rnorm(p); as.vector(v / sqrt(sum(v^2))) }
  u0 <- unit()
  for (r in i) X[r, ] <- X[r, ] + shift * 0.5 * (if (how == "shared") u0 else unit())
  colnames(X) <- paste0("x", seq_len(p))
  list(d = as.data.frame(X), lab = seq_len(n) %in% i)
}
## anomaly_null.R's clean design
make_clean <- function(seed, n = 400L, p = 8L) {
  set.seed(seed)
  W <- matrix(stats::rnorm(2 * p), 2, p)
  X <- matrix(stats::rnorm(n * 2), n, 2) %*% W + 0.5 * matrix(stats::rnorm(n * p), n, p)
  colnames(X) <- paste0("x", seq_len(p))
  list(d = as.data.frame(X), lab = rep(FALSE, n))
}
rules <- expand.grid(q = c(0.05, 0.10), rule = c("bh", "storey"),
                     stringsAsFactors = FALSE)
conds <- list(shared10 = list(share = 0.10, how = "shared", base = 100000L),
              shared5 = list(share = 0.05, how = "shared", base = 100000L),
              random5 = list(share = 0.05, how = "random", base = 200000L))

rows <- list(); saved <- list(); mismatch <- 0L
fit_one <- function(x, tr, label) {
  a <- suppressMessages(suppressWarnings(
    ilm_anomaly(x$d, trim = tr, seed = 1L, progress = FALSE)))
  a <- a[order(a$row), ]
  mismatch <<- mismatch + sum((stats::p.adjust(a$p, "BH") < 0.05) != a$flag)
  saved[[length(saved) + 1L]] <<- list(label = label, trim = tr, p = a$p, lab = x$lab)
  pa <- stats::p.adjust(a$p, "BH")
  pi0 <- min(1, sum(a$p > 0.5) / (0.5 * length(a$p)))
  adj <- list(bh = pa, storey = pmin(1, pi0 * pa))
  floor_p <- 1 / (attr(a, "n_null") + 1)
  at_floor <- abs(a$p - floor_p) < 1e-12
  min_adj <- floor_p * nrow(a) / max(1L, sum(at_floor))
  raw_floor <- if (any(x$lab)) mean(at_floor[x$lab]) else NA_real_
  near_floor <- if (any(x$lab)) mean(pa[x$lab] <= 2 * min_adj) else NA_real_
  lapply(seq_len(nrow(rules)), function(k) {
    q <- rules$q[k]; f <- adj[[rules$rule[k]]] < q
    data.frame(rule = rules$rule[k], q = q, pi0 = pi0, flagged = if (any(x$lab)) mean(f[x$lab]) else NA_real_,
               fdp = if (any(f)) mean(!x$lab[f]) else 0,
               false_flag = mean(f[!x$lab]), any_flag = any(f),
               auc = if (any(x$lab)) auc(a$score, x$lab) else NA_real_,
               raw_floor = raw_floor, near_floor = near_floor)
  })
}
for (cn in intersect(names(conds), parts)) for (SH in 3:9) for (s in seq_len(reps)) {
  cd <- conds[[cn]]
  x <- make_data(cd$base + 1000L * SH + s, cd$share, cd$how, SH)
  for (tr in c(0.25, 0)) {
    r <- do.call(rbind, fit_one(x, tr, paste(cn, SH, s)))
    rows[[length(rows) + 1L]] <- cbind(condition = cn, shift = SH, rep = s, trim = tr, r)
  }
}
if ("clean" %in% parts) for (s in seq_len(2L * reps)) {
  x <- make_clean(900000L + s)
  for (tr in c(0.25, 0)) {
    r <- do.call(rbind, fit_one(x, tr, paste("clean", s)))
    rows[[length(rows) + 1L]] <- cbind(condition = "clean", shift = NA, rep = s, trim = tr, r)
  }
}
res <- do.call(rbind, rows)
se <- function(z) { z <- z[is.finite(z)]; if (length(z) < 2L) NA else stats::sd(z) / sqrt(length(z)) }
key <- paste(res$condition, res$shift, res$trim, res$rule, res$q)
tab <- do.call(rbind, lapply(unique(key), function(k) {
  r <- res[key == k, ]
  data.frame(condition = r$condition[1], shift = r$shift[1], trim = r$trim[1],
             rule = r$rule[1], q = r$q[1], pi0 = mean(r$pi0),
             flagged = mean(r$flagged), flagged_se = se(r$flagged),
             fdp = mean(r$fdp), fdp_se = se(r$fdp),
             false_flag = mean(r$false_flag), false_flag_se = se(r$false_flag),
             any_flag = mean(r$any_flag), auc = mean(r$auc), auc_se = se(r$auc),
             raw_floor = mean(r$raw_floor), raw_floor_se = se(r$raw_floor),
             near_floor = mean(r$near_floor), near_floor_se = se(r$near_floor))
}))
cat(sprintf("n 400 x 8, rank 2, B = 39; %d replicates per cell (%d clean scans)\n",
            reps, 2L * reps))
cat(sprintf("q = 0.05 reproduces ilm_anomaly()'s own flag: %s (%d rows differ)\n\n",
            if (mismatch == 0L) "yes" else "NO", mismatch))
print(format(tab, digits = 3), row.names = FALSE)
if (!is.na(out)) {
  utils::write.csv(res, out, row.names = FALSE)
  saveRDS(saved, sub("\\.csv$", ".rds", out))
}
