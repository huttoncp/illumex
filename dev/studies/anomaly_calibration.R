## Why ilm_anomaly()'s reference runs slightly hot on clean data, and where.
##
## Stage 1 of the calibration study (H4, part C): locate the cause before
## trying a fix. On clean rank-2 data, 0.061 of rows fall below a raw p of
## 0.05 (anomaly_null.R), and 5 and 8 of 100 datasets had some row flagged at
## alpha = 0.05 where the false-discovery rate allows 5 (anomaly_null.R,
## fdr_threshold.R). Stage 2 is a separate registered run, designed and
## committed after this one: if a fix is found, it tests the fix across the
## grid and with heavier-tailed noise, and checks that it costs no power on
## fdr_threshold.R's anomalous cells.
##
## The reference (R/ilm_anomaly.R): the data are standardised; the rank k is
## chosen by parallel analysis; each row's score is its residual off a
## rank-k fit (trimmed); the reference simulates datasets from the fitted
## structure -- k latent scores times the fitted directions and singular
## values, plus independent normal noise per column at the column's residual
## level (MAD squared, times a degrees-of-freedom factor) -- and scores them
## through the same pipeline. The noise level is then rescaled so that the
## median score of 3 calibration datasets matches the data's median, and
## B = 39 more datasets form the reference.
##
## Design, fixed before any full run. Clean data, X = L W + E: L standard
## normal (n x k), W standard normal (k x p), E normal with sd 0.5; no
## anomalies. The reference is rebuilt here with switches, and the
## unchanged version is checked to reproduce ilm_anomaly()'s own p-values
## exactly on every dataset where it runs.
##   Grid (the unchanged reference, trim 0.25): n = 150, 400, 1,000; p = 5,
##   8, 15; true rank k = 1, 2; 200 datasets per cell.
##   Ablations at n = 400, p = 8, k = 2, trim 0.25 and 0, 200 datasets each,
##   one change at a time:
##     true_k       the true rank instead of parallel analysis
##     k_in_null    parallel analysis rerun on every simulated dataset, so
##                  the reference carries the rank's own uncertainty
##     no_match     no rescaling to the data's median
##     q90_match    rescaling to the 90th percentile instead of the median
##     cal_20       20 calibration datasets instead of 3
##     resid_noise  noise resampled from the fit's own residuals, column by
##                  column and inflated by the degrees-of-freedom factor,
##                  instead of drawn normal
##   Heavier tails than the reference assumes (added at review, before any
##   run): at the same setting, trim 0.25, the noise E
##   drawn from t with 5 df scaled to sd 0.5, for the unchanged reference,
##   resid_noise, q90_match and k_in_null, 200 datasets each. Which fix
##   survives a tail heavier than the normal noise it simulates is the
##   question stage 1 answers; a full t5 grid belongs to stage 2.
##   Reported per cell, each with its Monte Carlo standard error: the share
##   of rows with raw p at or below 0.0002, 0.001, 0.01, 0.05 and 0.10,
##   beside the exact share a perfectly calibrated pooled reference allows,
##   floor((n B + 1) c) / (n B + 1) -- at 0.0002 with n = 400 and B = 39 that
##   is 3 reference points, so the smallest cut is coarse -- and the share of
##   datasets with any row flagged at alpha = 0.05 after Benjamini-Hochberg
##   (nominal: at most 0.05).
##
## Run from the package root:
##   Rscript dev/studies/anomaly_calibration.R [reps] [outfile.csv] [parts]
## `parts` (optional) is a comma-separated subset of grid, ablation, t5.
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 200L
out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else NA_character_
parts <- if (length(args) >= 3L) strsplit(args[3], ",")[[1]] else c("grid", "ablation", "t5")

make_clean <- function(seed, n, p, k, noise = "normal") {
  set.seed(seed)
  E <- if (noise == "t5") matrix(stats::rt(n * p, 5) * sqrt(3 / 5) * 0.5, n, p)
       else matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  X <- matrix(stats::rnorm(n * k), n, k) %*% matrix(stats::rnorm(k * p), k, p) + E
  colnames(X) <- paste0("x", seq_len(p))
  X
}

## ilm_anomaly()'s reconstruction route, with switches; variant "default"
## follows it call for call, random draws included
ref_p <- function(X, trim = 0.25, B = 39L, seed = 1L, variant = "default",
                  true_k = NULL) {
  n <- nrow(X); p <- ncol(X)
  ctr <- colMeans(X); scl <- apply(X, 2L, stats::sd); scl[!is.finite(scl) | scl <= 0] <- 1
  Z <- sweep(sweep(X, 2L, ctr, "-"), 2L, scl, "/")
  set.seed(seed)
  k <- if (variant == "true_k") as.integer(true_k) else ilm_anom_rank(Z)
  k <- max(1L, min(k, p - 1L))
  obs <- ilm_anom_score(Z, k, trim)
  sv <- svd(Z, nu = k, nv = k)
  V <- sv$v[, seq_len(k), drop = FALSE]; d <- sv$d[seq_len(k)]
  Rin <- Z - Z %*% V %*% t(V)
  dfc <- (n * p) / max(n * p - (n + p) * k, 1)
  rvar <- apply(Rin, 2L, function(z) stats::mad(z)^2) * dfc
  rvar[!is.finite(rvar) | rvar <= 0] <- .Machine$double.eps
  sim <- function(rv, reps) unlist(lapply(seq_len(reps), function(b) {
    U <- matrix(stats::rnorm(n * k), n, k)
    E <- if (variant == "resid_noise")
      vapply(seq_len(p), function(j)
        sample(Rin[, j], n, replace = TRUE) * sqrt(dfc * rv[j] / rvar0[j]), numeric(n))
    else vapply(seq_len(p), function(j) stats::rnorm(n, 0, sqrt(rv[j])), numeric(n))
    S <- scale(U %*% diag(d / sqrt(n), k, k) %*% t(V) + E)
    ks <- if (variant == "k_in_null") max(1L, min(ilm_anom_rank(S), p - 1L)) else k
    ilm_anom_score(S, ks, trim)$score
  }))
  rvar0 <- rvar
  ncal <- if (variant == "cal_20") 20L else 3L
  cal <- sim(rvar, ncal)
  if (variant != "no_match") {
    pr <- if (variant == "q90_match") 0.9 else 0.5
    mo <- stats::quantile(obs$score, pr, names = FALSE)
    mc <- stats::quantile(cal, pr, names = FALSE)
    if (is.finite(mc) && mc > 0) rvar <- rvar * (mo / mc)
  }
  null <- unlist(lapply(seq_len(B), function(b) sim(rvar, 1L)))
  (1 + vapply(obs$score, function(s) sum(null >= s), 0L)) / (length(null) + 1)
}

cuts <- c(0.0002, 0.001, 0.01, 0.05, 0.10)
## names that survive data.frame() unchanged ("2e-04" would not)
cut_names <- paste0("p_le_", c("0.0002", "0.001", "0.01", "0.05", "0.10"))
summarise_p <- function(p) c(stats::setNames(vapply(cuts, function(c) mean(p <= c), 0),
                                             cut_names),
                             any_flag = as.numeric(any(stats::p.adjust(p, "BH") < 0.05)))
rows <- list(); checked <- 0L; mismatch <- 0L
one <- function(part, n, p, k, trim, variant, s) {
  X <- make_clean(1e6 * match(part, c("grid", "ablation", "t5")) + 1e4 * p + 100L * k +
                    (n %/% 50L) * 1e3 + s, n, p, k,
                  noise = if (part == "t5") "t5" else "normal")
  pv <- ref_p(X, trim = trim, variant = variant, true_k = k)
  if (variant == "default") {
    a <- suppressMessages(suppressWarnings(
      ilm_anomaly(as.data.frame(X), trim = trim, seed = 1L, progress = FALSE)))
    checked <<- checked + 1L
    mismatch <<- mismatch + as.integer(!isTRUE(all.equal(a$p[order(a$row)], pv)))
  }
  rows[[length(rows) + 1L]] <<- data.frame(part = part, n = n, p = p, k = k,
                                           trim = trim, variant = variant, rep = s,
                                           t(summarise_p(pv)))
}
if ("grid" %in% parts)
  for (n in c(150L, 400L, 1000L)) for (p in c(5L, 8L, 15L)) for (k in 1:2)
    for (s in seq_len(reps)) one("grid", n, p, k, 0.25, "default", s)
if ("ablation" %in% parts)
  for (v in c("default", "true_k", "k_in_null", "no_match", "q90_match", "cal_20",
              "resid_noise")) for (tr in c(0.25, 0)) for (s in seq_len(reps))
    one("ablation", 400L, 8L, 2L, tr, v, s)

if ("t5" %in% parts)
  for (v in c("default", "resid_noise", "q90_match", "k_in_null"))
    for (s in seq_len(reps)) one("t5", 400L, 8L, 2L, 0.25, v, s)

res <- do.call(rbind, rows)
se <- function(z) stats::sd(z) / sqrt(length(z))
vals <- c(cut_names, "any_flag")
key <- paste(res$part, res$n, res$p, res$k, res$trim, res$variant)
tab <- do.call(rbind, lapply(unique(key), function(kk) {
  r <- res[key == kk, ]
  N1 <- r$n[1] * 39 + 1
  nominal <- sprintf("%.4f", c(floor(N1 * cuts) / N1, 0.05))
  cbind(r[1, c("part", "n", "p", "k", "trim", "variant")],
        t(stats::setNames(vapply(seq_along(vals), function(i)
          sprintf("%.4f (%.4f) [%s]", mean(r[[vals[i]]]), se(r[[vals[i]]]), nominal[i]), ""),
          vals)))
}))
cat(sprintf(paste0("clean data, B = 39; %d datasets per cell; each share is shown as",
                   " mean (Monte Carlo SE) [exact nominal]\n"), reps))
cat(sprintf("the unchanged reference reproduces ilm_anomaly()'s p exactly: %d of %d datasets\n\n",
            checked - mismatch, checked))
print(tab, row.names = FALSE)
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
