## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## A reference for ilm_anomaly() that holds its false-flag rate when the noise
## is not normal, and what it costs in power.
##
## Stage 2 of the calibration study (H4, part C). Stage 1
## (anomaly_calibration.R, results beside it) found the unchanged reference
## slightly hot on normal noise -- worst with few residual dimensions, p - k
## = 3 flagging some row in 0.13 to 0.19 of clean datasets at alpha = 0.05 --
## and far off with heavier tails: with t5 noise 0.97 of clean datasets had a
## row flagged. It was decided that a flag produced by heavy-tailed noise is a
## false flag, so the reference must hold under such noise, not only warn.
##
## Why the arms are all simulated references, and conformal calibration is
## not one of them. A split-conformal p-value is (1 + the number of
## calibration scores at least the row's) / (c + 1), for c calibration rows
## held out of the fit (Bates, Candes, Lei, Romano and Sesia 2023, Annals of
## Statistics 51, 149-178), so its floor is 1 / (c + 1). Its guarantee, and
## the validity of Benjamini-Hochberg on it, assume a clean calibration set
## beside the rows tested. The flag rule is Benjamini-Hochberg at q = 0.05
## over the m rows tested (Benjamini and Hochberg 1995), which flags k rows
## only if their p-values are at most k q / m. With every row tested and the
## calibration rows drawn from the same data (c < m), k rows that each beat
## every calibration score are flagged only if k >= m / (q (c + 1)), about
## 20 m / c rows: about 60 with a three-way split (fit, calibrate, test,
## rotated so every row is tested), at any n. Cross-conformal (Vovk 2015;
## Barber, Candes, Ramdas and Tibshirani 2021 for jackknife+) is worse: each
## row is ranked against the other rows, planted rows included, so the j-th
## smallest p-value is at least j / (n + 1) and never meets j q / n; it
## cannot flag at all. Anomalies in the calibration rows, the ordinary case
## here, raise the planted rows' p-values further. The simulated reference
## escapes the floor: n B pooled simulated scores give a smallest p of
## 1 / (n B + 1), 0.000064 at n = 400 and B = 39 (R/ilm_anomaly.R;
## fdr_threshold.R). So every arm keeps the simulated reference and changes
## only where its noise comes from and how it is scaled, and the bound
## settles the conformal arms without a run.
##
## Design, fixed before any full run.
##   Data. X = L W + E: L standard normal (n x k), W standard normal (k x p),
##   E drawn from one of four noises, each scaled to sd 0.5:
##     normal
##     t3          t with 3 df, divided by sqrt(3)
##     t5          t with 5 df, times sqrt(3/5) (stage 1's)
##     lognormal   exp(N(0, 0.5^2)), centred and scaled (skewness 1.75)
##   Part "clean": no anomalies. n = 150, 400, 1,000; p = 5, 8, 15; k = 2, so
##   p - k = 3 is the p = 5 row; the four noises; 200 datasets per cell. The
##   normal-noise datasets are stage 1's grid datasets (same generator and
##   seeds), so the unchanged arm reproduces stage 1's k = 2 grid.
##   Part "power": fdr_threshold.R's anomalous cells with its generator and
##   seeds -- n = 400, p = 8, rank 2, normal noise; 10% of rows shifted along
##   one shared direction off the structure, 5% shared, 5% along random
##   directions; shifts of 3 to 9 noise sd; 50 datasets per cell -- plus 2%
##   along random directions (seed base 300000), so contamination runs at 2,
##   5 and 10%. The planted rows sit in the data every arm's noise is learned
##   from, which is the contamination each arm has to survive.
##   Arms, each through ilm_anomaly()'s pipeline (standardise, rank by
##   parallel analysis, trimmed rank-k fit with trim 0.25, B = 39 simulated
##   datasets pooled, raw p = (1 + #null >= s) / (n B + 1), BH at 0.05);
##   they differ only in the simulated noise and in the scale matching:
##     default     normal noise per column at the residual MAD level, matched
##                 to the data's median score (the package now; checked to
##                 reproduce ilm_anomaly()'s p-values exactly)
##     q90         the same noise, matched at the 90th percentile of the
##                 scores instead of the median (stage 1's best on normal
##                 noise)
##     resid_q90   each column's noise resampled from that column's in-sample
##                 residuals, inflated by the degrees-of-freedom factor, then
##                 matched at the 90th percentile (stage 1's resid_noise)
##     row_q90     whole rows of the in-sample residual matrix resampled, so
##                 the noise keeps its dependence across columns, inflated
##                 and matched as resid_q90
##     oos         whole rows of out-of-sample residuals resampled, with no
##                 matching: the rows are split at random into halves, each
##                 half's residuals are taken off the other half's trimmed
##                 rank-k fit, so no residual is shrunk by a fit it helped
##                 make
##     oos_q90     oos, then matched at the 90th percentile
##   Every arm draws its reference with seed 1, as ilm_anomaly() does.
##   The heavy-tail diagnostic, computed from the default arm's own reference
##   (no extra fits): T is the ratio of the 99th to the 75th percentile of
##   the absolute residuals, each column centred at its median and divided by
##   its MAD, pooled over cells; T_kept on the rows the trimmed fit keeps,
##   T_all on every row. Its p-value is (1 + #{reference datasets with T at
##   least the data's}) / (B + 1), and it alarms at p <= 0.05.
##   Reported, each with its Monte Carlo standard error:
##     clean, per cell and arm: the share of rows with raw p at or below
##       0.0002, 0.001, 0.01, 0.05 and 0.10 beside the exact nominal share
##       floor((n B + 1) c) / (n B + 1); the share of datasets with any row
##       flagged (nominal at most 0.05); and for the diagnostic, the share of
##       datasets it alarms on -- false alarms on normal noise, detection on
##       t3, t5 and lognormal.
##     power, per condition, shift and arm: the share of planted rows
##       flagged; the false-discovery proportion (0 with no flags; its mean
##       is the false-discovery rate); the share of other rows flagged; and
##       the diagnostic's alarm rate, whose alarms here are false (the noise
##       is normal). Also averaged over shifts.
##   Decision rule, fixed now: an arm holds in a clean cell when its share of
##   datasets flagging anything is within two Monte Carlo standard errors of
##   0.05 or below it. Among the arms that hold in all 36 clean cells, the one
##   flagging the most planted rows averaged over the power cells is
##   recommended; if none holds everywhere, the report says where each
##   fails, and nothing changes in the package until the results are reviewed.
##
## Addendum, 2026-09-28, committed while the six-arm run (0b1bca4) was under
## way and before any of its results existed:
##   a seventh arm,
##     row_kept_q90  whole rows of the in-sample residual matrix resampled
##                   from the rows the trimmed fit keeps only (the best-fitting
##                   (1 - trim) share), inflated by the degrees-of-freedom
##                   factor and matched at the 90th percentile of the scores
##   Why: in the smoke run (2 datasets per cell, 0b1bca4), the arms that
##   resample residuals from every row flagged almost none of the planted
##   rows on the power cells, since the planted rows' own residuals are in
##   the pool; resampling only the kept rows leaves most of them out. It runs
##   on the same datasets and seeds as the other six, after their run, as its
##   own process (`arms = row_kept_q90`), and falls under the same decision
##   rule, fixed above, as one of seven arms.
##
## Run from the package root:
##   Rscript dev/studies/anomaly_calibration2.R [reps] [outfile.csv] [parts] [arms]
## `parts` (optional) is a comma-separated subset of clean_normal, clean_t3,
## clean_t5, clean_lognormal and power, so the work can be split across
## processes; `reps` is the clean part's datasets per cell (200), and the
## power part keeps fdr_threshold.R's 50 (or `reps`, if smaller). `arms`
## (optional) is a comma-separated subset of the arms below; the default
## arm's check against ilm_anomaly() and the diagnostic run only when the
## default arm does.
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
options(width = 220)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 200L
out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else NA_character_
noises <- c("normal", "t3", "t5", "lognormal")
parts <- if (length(args) >= 3L && nzchar(args[3])) strsplit(args[3], ",")[[1]] else
  c(paste0("clean_", noises), "power")
power_reps <- min(reps, 50L)

SD <- 0.5; SDLOG <- 0.5
draw_noise <- function(m, noise) switch(noise,
  normal = stats::rnorm(m, sd = SD),
  t3 = stats::rt(m, 3) / sqrt(3) * SD,
  t5 = stats::rt(m, 5) * sqrt(3 / 5) * SD,
  lognormal = (exp(stats::rnorm(m, 0, SDLOG)) - exp(SDLOG^2 / 2)) /
    sqrt((exp(SDLOG^2) - 1) * exp(SDLOG^2)) * SD)

## stage 1's generator and seeds for normal noise (its "grid" part), and the
## same layout offset per noise for the others
make_clean <- function(s, n, p, k, noise) {
  base <- if (noise == "normal") 1e6 else 1e6 * (4 + match(noise, noises))
  set.seed(base + 1e4 * p + 100L * k + (n %/% 50L) * 1e3 + s)
  E <- matrix(draw_noise(n * p, noise), n, p)
  X <- matrix(stats::rnorm(n * k), n, k) %*% matrix(stats::rnorm(k * p), k, p) + E
  colnames(X) <- paste0("x", seq_len(p))
  X
}
## fdr_threshold.R's generator (trim_grid.R's), unchanged
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

## the diagnostic's statistic on one fit: absolute residuals over their
## column's MAD, 99th over 75th percentile, on the kept rows or on all
tail_stat <- function(f, k, trim, kept = TRUE) {
  R <- f$residual
  if (kept) {
    nkeep <- max(k + 2L, floor(nrow(R) * (1 - trim)))
    R <- R[order(f$score)[seq_len(nkeep)], , drop = FALSE]
  }
  s <- apply(R, 2L, stats::mad); s[!is.finite(s) | s <= 0] <- 1
  a <- abs(sweep(sweep(R, 2L, apply(R, 2L, stats::median), "-"), 2L, s, "/"))
  unname(stats::quantile(a, 0.99) / stats::quantile(a, 0.75))
}

## residual rows off a fit the row took no part in: halves at random, each
## scored against the other half's trimmed rank-k basis
oos_resid <- function(Z, k, trim) {
  n <- nrow(Z); h <- sample(n) <= n %/% 2L
  R <- Z
  for (side in c(TRUE, FALSE)) {
    V <- ilm_anom_score(Z[h == side, , drop = FALSE], k, trim)$V
    Zo <- Z[h != side, , drop = FALSE]
    R[h != side, ] <- Zo - Zo %*% V %*% t(V)
  }
  R
}

arms <- list(default = c("normal", "median"), q90 = c("normal", "q90"),
             resid_q90 = c("resid", "q90"), row_q90 = c("row", "q90"),
             oos = c("oos", "none"), oos_q90 = c("oos", "q90"),
             row_kept_q90 = c("row_kept", "q90"))        # the addendum's arm
run_arms <- if (length(args) >= 4L && nzchar(args[4])) strsplit(args[4], ",")[[1]] else
  setdiff(names(arms), "row_kept_q90")                # the registered six by default
stopifnot(all(run_arms %in% names(arms)))

## ilm_anomaly()'s reconstruction route with the arm's noise and matching;
## the default arm follows it call for call, random draws included
ref_p <- function(X, arm, trim = 0.25, B = 39L, seed = 1L, diag = FALSE) {
  noise <- arms[[arm]][1]; how <- arms[[arm]][2]
  n <- nrow(X); p <- ncol(X)
  ctr <- colMeans(X); scl <- apply(X, 2L, stats::sd); scl[!is.finite(scl) | scl <= 0] <- 1
  Z <- sweep(sweep(X, 2L, ctr, "-"), 2L, scl, "/")
  set.seed(seed)
  k <- max(1L, min(ilm_anom_rank(Z), p - 1L))
  obs <- ilm_anom_score(Z, k, trim)
  sv <- svd(Z, nu = k, nv = k)
  V <- sv$v[, seq_len(k), drop = FALSE]; d <- sv$d[seq_len(k)]
  Rin <- Z - Z %*% V %*% t(V)
  dfc <- (n * p) / max(n * p - (n + p) * k, 1)
  mad2 <- function(M, mult = 1) {
    v <- apply(M, 2L, function(z) stats::mad(z)^2) * mult
    v[!is.finite(v) | v <= 0] <- .Machine$double.eps
    v
  }
  kept <- order(obs$score)[seq_len(max(k + 2L, floor(n * (1 - trim))))]
  pool <- switch(noise, resid = , row = Rin, row_kept = Rin[kept, , drop = FALSE],
                 oos = oos_resid(Z, k, trim), NULL)
  infl <- if (noise %in% c("resid", "row", "row_kept")) dfc else 1
  rv <- if (is.null(pool)) mad2(Rin, dfc) else mad2(pool)
  pv0 <- rv
  sim <- function(rv, reps, stat = FALSE) {
    sc <- vector("list", reps); Tk <- Ta <- numeric(reps)
    for (b in seq_len(reps)) {
      U <- matrix(stats::rnorm(n * k), n, k)
      E <- switch(noise,
        normal = vapply(seq_len(p), function(j) stats::rnorm(n, 0, sqrt(rv[j])), numeric(n)),
        resid = vapply(seq_len(p), function(j)
          sample(pool[, j], n, replace = TRUE) * sqrt(infl * rv[j] / pv0[j]), numeric(n)),
        sweep(pool[sample.int(nrow(pool), n, replace = TRUE), , drop = FALSE], 2L,
              sqrt(infl * rv / pv0), "*"))
      f <- ilm_anom_score(scale(U %*% diag(d / sqrt(n), k, k) %*% t(V) + E), k, trim)
      sc[[b]] <- f$score
      if (stat) { Tk[b] <- tail_stat(f, k, trim); Ta[b] <- tail_stat(f, k, trim, FALSE) }
    }
    list(score = unlist(sc), Tk = Tk, Ta = Ta)
  }
  if (how != "none") {
    cal <- sim(rv, 3L)$score
    pr <- if (how == "q90") 0.9 else 0.5
    mo <- stats::quantile(obs$score, pr, names = FALSE)
    mc <- stats::quantile(cal, pr, names = FALSE)
    if (is.finite(mc) && mc > 0) rv <- rv * (mo / mc)
  }
  nl <- sim(rv, B, stat = diag)
  pv <- (1 + vapply(obs$score, function(s) sum(nl$score >= s), 0L)) / (length(nl$score) + 1)
  res <- list(p = pv)
  if (diag) {
    res$diag_kept <- (1 + sum(nl$Tk >= tail_stat(obs, k, trim))) / (B + 1)
    res$diag_all <- (1 + sum(nl$Ta >= tail_stat(obs, k, trim, FALSE))) / (B + 1)
  }
  res
}

cuts <- c(0.0002, 0.001, 0.01, 0.05, 0.10)
cut_names <- paste0("p_le_", c("0.0002", "0.001", "0.01", "0.05", "0.10"))
rows <- list(); checked <- 0L; mismatch <- 0L
check <- function(X, pv, trim = 0.25) {
  a <- suppressMessages(suppressWarnings(
    ilm_anomaly(as.data.frame(X), trim = trim, seed = 1L, progress = FALSE)))
  checked <<- checked + 1L
  mismatch <<- mismatch + as.integer(!isTRUE(all.equal(a$p[order(a$row)], pv)))
}
diag_cols <- function(r) c(alarm_kept = if (is.null(r$diag_kept)) NA else as.numeric(r$diag_kept <= 0.05),
                           alarm_all = if (is.null(r$diag_all)) NA else as.numeric(r$diag_all <= 0.05))

for (noise in noises) if (paste0("clean_", noise) %in% parts)
  for (n in c(150L, 400L, 1000L)) for (p in c(5L, 8L, 15L)) for (s in seq_len(reps)) {
    X <- make_clean(s, n, p, 2L, noise)
    for (arm in run_arms) {
      r <- ref_p(X, arm, diag = arm == "default")
      if (arm == "default" && s <= 20L) check(X, r$p)
      rows[[length(rows) + 1L]] <- data.frame(
        part = "clean", noise = noise, n = n, p = p, arm = arm, cond = NA, shift = NA, rep = s,
        t(stats::setNames(vapply(cuts, function(c) mean(r$p <= c), 0), cut_names)),
        any_flag = as.numeric(any(stats::p.adjust(r$p, "BH") < 0.05)),
        flagged = NA, fdp = NA, false_flag = NA, t(diag_cols(r)))
    }
  }

conds <- list(shared10 = list(share = 0.10, how = "shared", base = 100000L),
              shared5 = list(share = 0.05, how = "shared", base = 100000L),
              random5 = list(share = 0.05, how = "random", base = 200000L),
              random2 = list(share = 0.02, how = "random", base = 300000L))
if ("power" %in% parts)
  for (cn in names(conds)) for (SH in 3:9) for (s in seq_len(power_reps)) {
    cd <- conds[[cn]]
    x <- make_data(cd$base + 1000L * SH + s, cd$share, cd$how, SH)
    X <- as.matrix(x$d)
    for (arm in run_arms) {
      r <- ref_p(X, arm, diag = arm == "default")
      if (arm == "default") check(X, r$p)
      f <- stats::p.adjust(r$p, "BH") < 0.05
      rows[[length(rows) + 1L]] <- data.frame(
        part = "power", noise = "normal", n = 400L, p = 8L, arm = arm, cond = cn, shift = SH,
        rep = s, t(stats::setNames(rep(NA_real_, length(cuts)), cut_names)),
        any_flag = as.numeric(any(f)), flagged = mean(f[x$lab]),
        fdp = if (any(f)) mean(!x$lab[f]) else 0, false_flag = mean(f[!x$lab]),
        t(diag_cols(r)))
    }
  }

res <- do.call(rbind, rows)
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
se <- function(z) { z <- z[is.finite(z)]; if (length(z) < 2L) NA else stats::sd(z) / sqrt(length(z)) }
ms <- function(z) { z <- z[is.finite(z)]; if (!length(z)) "" else sprintf("%.4f (%.4f)", mean(z), se(z)) }
cat(sprintf("the default arm reproduces ilm_anomaly()'s p exactly: %d of %d datasets\n\n",
            checked - mismatch, checked))
armlv <- names(arms)

cl <- res[res$part == "clean", ]
if (nrow(cl)) {
  cat("clean data, B = 39, k = 2; each share is mean (Monte Carlo SE) [exact nominal]\n")
  g <- split(cl, list(factor(cl$arm, armlv), cl$p, cl$n, factor(cl$noise, noises)), drop = TRUE)
  tab <- do.call(rbind, lapply(g, function(d) {
    N <- d$n[1] * 39 + 1
    nom <- floor(N * cuts) / N
    data.frame(noise = d$noise[1], n = d$n[1], p = d$p[1], arm = d$arm[1],
               t(stats::setNames(vapply(seq_along(cuts), function(i)
                 sprintf("%s [%.4f]", ms(d[[cut_names[i]]]), nom[i]), ""), cut_names)),
               any_flag = ms(d$any_flag), alarm_kept = ms(d$alarm_kept),
               alarm_all = ms(d$alarm_all), check.names = FALSE)
  }))
  print(tab, row.names = FALSE, right = TRUE)
  cat("\n")
}
pw <- res[res$part == "power", ]
if (nrow(pw)) {
  cat("planted anomalies, n = 400, p = 8, rank 2, normal noise; mean (Monte Carlo SE)\n")
  g <- split(pw, list(factor(pw$arm, armlv), pw$shift, factor(pw$cond, names(conds))), drop = TRUE)
  tab <- do.call(rbind, lapply(g, function(d)
    data.frame(cond = d$cond[1], shift = d$shift[1], arm = d$arm[1], flagged = ms(d$flagged),
               fdp = ms(d$fdp), false_flag = ms(d$false_flag), any_flag = ms(d$any_flag),
               alarm_kept = ms(d$alarm_kept), alarm_all = ms(d$alarm_all))))
  print(tab, row.names = FALSE, right = TRUE)
  cat("\naveraged over shifts 3 to 9\n")
  g <- split(pw, list(factor(pw$arm, armlv), factor(pw$cond, names(conds))), drop = TRUE)
  tab <- do.call(rbind, lapply(g, function(d)
    data.frame(cond = d$cond[1], arm = d$arm[1], flagged = ms(d$flagged), fdp = ms(d$fdp),
               false_flag = ms(d$false_flag), alarm_kept = ms(d$alarm_kept),
               alarm_all = ms(d$alarm_all))))
  print(tab, row.names = FALSE, right = TRUE)
}
