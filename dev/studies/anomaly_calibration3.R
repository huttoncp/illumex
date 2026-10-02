## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## Stage 3 of the calibration study for ilm_anomaly(): a heavy-tailed
## reference, a mixture-density score, the five-column excess, the warning on
## clustered data, and the tail warning's threshold.
##
## Why. Stage 2 (anomaly_calibration2.R at 8cdd87b, results beside it)
## found no arm both calibrated and able to find anomalies: the two arms
## that held their false-flag rate in all 36 clean cells found 0.15% and
## 0.10% of planted rows, against 47% for the default, which held in 6 of 36
## (on t3 noise it flagged something in 94-100% of clean datasets, and on
## normal noise with 5 columns in 12-19%). Decisions (items 88, 143 and 144):
##   88   keep the default; add a run-time warning driven by the tail
##        diagnostic (wording and threshold decided separately); say in the help that
##        the scan over-flags on heavy-tailed or clustered data and with few
##        columns; then this registered stage 3: a heavy-tailed reference
##        fitted to the kept rows, the cause of the five-column excess,
##        whether the warning also fires on clustered data, and whether a
##        mixture-density score fixes both failures.
##   143  an arm qualifies only if it holds the false-flag ceiling in the
##        clean cells and finds planted anomalies in normal noise at least
##        75% as often as today's default (about 47%, so a planted-row
##        share of about 0.35). Among the qualifying arms, the one with the
##        most power, heavy-tailed cells included, is recommended. If none
##        qualifies, nothing changes until the results are reviewed.
##   144  the warning's threshold by a rule fixed now: the most sensitive
##        threshold whose false-warning rate on clean normal noise is at most
##        10%, allowing for Monte Carlo error; the check's p-value recorded
##        in every dataset, with false-warning and detection curves for
##        thresholds 0.01 to 0.20, cell by cell. The curves are reviewed, and
##        the threshold can be overruled with a stated reason.
##
## Design, fixed before any run.
##   Build: this branch's package code (main's, ff5e579; the branch adds only
##   dev/), installed from a clone of the commit named in the run's log into
##   a pinned library whose manifest records that commit,
##   and loaded from there; the script runs from that clone, which nothing
##   edits, so a resumed run cannot pick up changed code. mclust 6.1 from the
##   same library (the mixture arm only).
##
##   Parts, with stage 2's generators and seeds wherever a part repeats one,
##   so the default arm reproduces stage 2's numbers:
##     clean       stage 2's 36 clean cells: X = L W + E, L standard normal
##                 (n x 2), W standard normal (2 x p), E one of four noises
##                 scaled to sd 0.5 (normal; t3; t5; lognormal, skewness
##                 1.75); n = 150, 400, 1,000; p = 5, 8, 15; 200 datasets a
##                 cell; stage 2's make_clean() and seeds.
##     clustered   clean normal noise on a clustered latent: L drawn around
##                 three centres (0, 0), (4, 0) and (2, 3.46) with sd 1, equal
##                 shares, so the rows form three well-separated groups on the
##                 two-dimensional structure; n = 400, 1,000; p = 5, 8, 15;
##                 200 datasets a cell; seed base 9e6.
##     power       stage 2's power cells: n = 400, p = 8, rank 2, normal
##                 noise; 10% of rows shifted along one shared direction off
##                 the structure, 5% shared, 5% and 2% along random
##                 directions; shifts of 3 to 9 noise sd; 50 datasets a cell;
##                 fdr_threshold.R's generator and seeds.
##     power_heavy the same planting in heavy-tailed noise, t5 and t3 (drawn
##                 and scaled as in the clean part), shifts 3, 5, 7 and 9,
##                 the four contamination conditions, 50 datasets a cell;
##                 seed base 5e6 (t5) and 6e6 (t3).
##     p5_cause    the clean normal cells at p = 5 and, as the control, p =
##                 8 (the clean part's own datasets): the default arm and
##                 three variants that each change one thing --
##                   rank2    the rank fixed at the truth, 2, instead of
##                            chosen by parallel analysis
##                   nodf     no degrees-of-freedom inflation of the noise
##                   nomatch  no matching of the reference to the data's
##                            median score
##                 -- and the rank the default chose, per dataset.
##   Arms, each through ilm_anomaly()'s pipeline (standardise, rank by
##   parallel analysis, trimmed rank-k fit with trim 0.25, B = 39 simulated
##   datasets pooled, raw p = (1 + #null >= s) / (n B + 1), BH at 0.05); the
##   first three differ only in the simulated noise and its matching:
##     default         normal noise per column at the residual MAD level,
##                     matched at the median score (the package; checked to
##                     reproduce ilm_anomaly()'s p-values exactly on the first
##                     20 datasets of every clean cell and every power dataset)
##     heavy_kept      each column's noise a scaled t whose scale and degrees
##                     of freedom are fitted by maximum likelihood to that
##                     column's residuals on the rows the trimmed fit keeps
##                     (df between 2.1 and 200), inflated by the default's
##                     degrees-of-freedom factor, matched at the median
##     heavy_kept_q90  the same, matched at the 90th percentile of the scores
##     mixture         a mixture-density score in place of the reconstruction
##                     score: a Gaussian mixture (mclust, 1 to 4 components,
##                     models EII, VII, EEI, VVI, EEE and VVV, by BIC) fitted
##                     to the standardised data, refitted to the rows whose
##                     density is highest (the best-fitting 1 - trim share),
##                     and each row scored as its negative log-density under
##                     the refit; the reference is B = 39 datasets simulated
##                     from that fitted mixture, each standardised, fitted and
##                     scored the same way with its component count and model
##                     held at the data's
##   Recorded in every dataset of every part: the default arm's tail check,
##   stage 2's diagnostic on the kept rows (T, the 99th over the 75th
##   percentile of the absolute residuals over their column's MAD), as its
##   p-value (1 + #{reference datasets with T at least the data's}) / 40,
##   and the same on all rows.
##   Outcomes, each with its Monte Carlo standard error:
##     clean and clustered, per cell and arm: the share of datasets with any
##       row flagged (nominal at most 0.05), and the share of rows with raw p
##       at or below 0.001, 0.01 and 0.05.
##     power and power_heavy, per condition, shift and arm: the share of
##       planted rows flagged, the false-discovery proportion, and the share
##       of other rows flagged.
##     p5_cause, per cell and variant: the share of datasets with any row
##       flagged, and the distribution of the chosen rank.
##     the warning, per cell of every part: the share of datasets whose tail
##       check has p <= t, for t = 0.01, 0.02, ..., 0.20 (false warnings on
##       clean normal and on clustered data, detection on clean
##       heavy-tailed data, and warnings on normal data with planted
##       anomalies).
##   The check's p-value takes only the values k / 40 (k = 1 to 40), so no
##   threshold below 0.025 can warn, and thresholds between two of those
##   values decide alike; the curves are read with that in mind.
##
##   Decision rules, fixed now.
##   R1 (item 143, the arms). An arm holds in a clean cell when its share of
##      datasets flagging anything is at most 0.05 plus two Monte Carlo
##      standard errors. An arm qualifies when it holds in all 36 clean
##      cells and the share of planted rows it flags, averaged over the 28
##      power cells (normal noise), is at least 0.75 times the default arm's
##      average over the same cells. Among the qualifying arms, the one
##      flagging the largest share of planted rows averaged over the 28
##      power cells and the 32 power_heavy cells together is recommended. If
##      none qualifies, the report says where each arm fails, and nothing
##      changes in the package until the results are reviewed. The default
##      arm is judged by
##      the same rule and cannot qualify on stage 2's evidence unless this
##      run differs from it.
##   R2 (item 144, the warning's threshold). For a threshold t among 0.01,
##      0.02, ..., 0.20 and each of the nine clean normal-noise cells, let
##      F(t) be the share of the cell's 200 datasets whose tail check has
##      p <= t, and SE(t) = sqrt(F(t) (1 - F(t)) / 200), its Monte Carlo
##      standard error (taken as sqrt(0.1 x 0.9 / 200) = 0.0212 where F(t) is
##      0). A threshold is admissible when F(t) <= 0.10 + 2 SE(t) in all nine
##      cells. The rule's threshold is the largest admissible t, which is the
##      most sensitive, since a larger threshold warns on every dataset a
##      smaller one does. It is reported with the largest attainable value
##      k / 40 at or below it, since the two decide alike. If no t in the
##      grid is admissible, the rule gives no threshold and the curves are
##      reviewed. They are reviewed either way, and the threshold can be
##      overruled with a stated reason.
##   R3 (item 88, the five-column excess). A variant explains the excess
##      when the default fails to hold (R1's sense) in a p = 5 clean normal
##      cell and the variant holds in every p = 5 cell, while it still holds
##      in the p = 8 control cells wherever the default does. Every variant
##      that does is reported; if none does, the report says the excess is
##      not explained by the rank, the degrees-of-freedom factor or the
##      matching alone.
##   R4 (item 88, the mixture score). The mixture score fixes both failures
##      when it holds (R1's sense) in every clean heavy-tailed cell (t3, t5,
##      lognormal) and in every clustered cell. Reported whether or not it
##      qualifies under R1.
##   Clustered data: the default arm's share of datasets flagging anything,
##   and the warning's rate at the R2 threshold, are reported per cell; no
##   rule, since no remedy is proposed for it here.
##
##   Cost: from a smoke run of 1 dataset a cell, stated in an addendum below
##   before the full run.
##
## Addendum, the cost, from the smoke run (1 dataset a cell, every part and
## arm, 8664ac6, 1,414 s; its output was not kept). The
## default arm reproduced ilm_anomaly()'s p-values exactly in 64 of 64
## datasets checked. Seconds per dataset, scaled to the full run:
##     part          datasets    default + heavy arms    mixture
##     clean         200 a cell        4.6 core-hours      32.2
##     clustered     200 a cell        0.7                 11.4
##     power         50 a cell         0.5                  2.4
##     power_heavy   50 a cell         0.5                  3.9
##     p5_cause      200 a cell        0.4                    -
##   About 57 core-hours in all, 50 of them the mixture arm, whose refits
##   take about 45 s a dataset at 1,000 rows. The parts split across
##   processes, so on four cores the run takes about 15 hours. A cheaper
##   alternative, to be chosen before the run: the mixture
##   arm at 100 datasets a clean and clustered cell, about 35 core-hours in
##   all. Its two-standard-error allowance under R1 would then be about 1.4
##   times the other arms', which the report would state. The smoke run's
##   timings came while a package check shared the core, so they are, if
##   anything, high.
##
## Addendum 2, chosen before the run: the full run as
## registered, at about 57 core-hours (about 15 hours on four cores), the
## mixture arm measured to the same precision as the others, since the
## comparison between arms is what the decision rules rest on. Each dataset
## and arm is checkpointed as one appended row, and a restart skips what is
## already written, so the run splits across cores (by part, and by an
## optional replicate range) and resumes.
##
## Run from the package root:
##   Rscript dev/studies/anomaly_calibration3.R [reps] [outfile.csv] [parts] [arms] [from:to or k/m]
##   Rscript dev/studies/anomaly_calibration3.R summarise out1.csv [out2.csv ...]
## `parts`: a comma-separated subset of clean_normal, clean_t3, clean_t5,
## clean_lognormal, clustered, power, power_heavy and p5_cause, so the work
## can be split across processes; `reps` is the clean and clustered parts'
## datasets per cell (200), and the power parts keep 50 (or `reps`, if
## smaller). `arms`: a subset of default, heavy_kept, heavy_kept_q90 and
## mixture (all four by default; p5_cause runs the default and its variants
## whatever is given).
## the installed build, never the working tree (see Build above); the two
## internals the arms reuse are taken from its namespace
suppressPackageStartupMessages(library(illumex))
ilm_anom_rank <- illumex:::ilm_anom_rank
ilm_anom_score <- illumex:::ilm_anom_score
options(width = 220)
args <- commandArgs(TRUE)
noises <- c("normal", "t3", "t5", "lognormal")
ARMS <- c("default", "heavy_kept", "heavy_kept_q90", "mixture")
THRESH <- seq(0.01, 0.20, by = 0.01)
se <- function(z) { z <- z[is.finite(z)]; if (length(z) < 2L) NA else stats::sd(z) / sqrt(length(z)) }

SD <- 0.5; SDLOG <- 0.5
draw_noise <- function(m, noise) switch(noise,
  normal = stats::rnorm(m, sd = SD),
  t3 = stats::rt(m, 3) / sqrt(3) * SD,
  t5 = stats::rt(m, 5) * sqrt(3 / 5) * SD,
  lognormal = (exp(stats::rnorm(m, 0, SDLOG)) - exp(SDLOG^2 / 2)) /
    sqrt((exp(SDLOG^2) - 1) * exp(SDLOG^2)) * SD)

## stage 2's make_clean(), unchanged
make_clean <- function(s, n, p, k, noise) {
  base <- if (noise == "normal") 1e6 else 1e6 * (4 + match(noise, noises))
  set.seed(base + 1e4 * p + 100L * k + (n %/% 50L) * 1e3 + s)
  E <- matrix(draw_noise(n * p, noise), n, p)
  X <- matrix(stats::rnorm(n * k), n, k) %*% matrix(stats::rnorm(k * p), k, p) + E
  colnames(X) <- paste0("x", seq_len(p))
  X
}
## three groups on the two-dimensional structure, normal noise
make_clustered <- function(s, n, p) {
  set.seed(9e6 + 1e4 * p + (n %/% 50L) * 1e3 + s)
  g <- sample(rep_len(1:3, n))
  L <- rbind(c(0, 0), c(4, 0), c(2, 3.46))[g, ] + matrix(stats::rnorm(n * 2), n, 2)
  X <- L %*% matrix(stats::rnorm(2 * p), 2, p) + matrix(stats::rnorm(n * p, sd = SD), n, p)
  colnames(X) <- paste0("x", seq_len(p))
  X
}
## fdr_threshold.R's generator (stage 2's make_data), with the noise as an
## option; for normal noise it draws exactly what stage 2 drew
make_data <- function(seed, share, how, shift, noise = "normal", n = 400L, p = 8L) {
  set.seed(seed)
  W <- matrix(stats::rnorm(2 * p), 2, p)
  X <- matrix(stats::rnorm(n * 2), n, 2) %*% W +
    (if (noise == "normal") matrix(stats::rnorm(n * p, sd = 0.5), n, p)
     else matrix(draw_noise(n * p, noise), n, p))
  i <- sample(n, round(share * n))
  P <- diag(p) - t(W) %*% solve(W %*% t(W)) %*% W
  unit <- function() { v <- P %*% stats::rnorm(p); as.vector(v / sqrt(sum(v^2))) }
  u0 <- unit()
  for (r in i) X[r, ] <- X[r, ] + shift * 0.5 * (if (how == "shared") u0 else unit())
  colnames(X) <- paste0("x", seq_len(p))
  list(X = X, lab = seq_len(n) %in% i)
}

## stage 2's diagnostic statistic, unchanged
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

## a scaled t fitted by maximum likelihood to one column's residuals:
## scale and degrees of freedom, df kept between 2.1 and 200
fit_t <- function(r) {
  r <- r - stats::median(r)
  s0 <- stats::mad(r); if (!is.finite(s0) || s0 <= 0) s0 <- stats::sd(r)
  if (!is.finite(s0) || s0 <= 0) return(c(scale = 1e-8, df = 200))
  nll <- function(th) {
    s <- exp(th[1]); nu <- 2.1 + (200 - 2.1) * stats::plogis(th[2])
    -sum(stats::dt(r / s, nu, log = TRUE) - log(s))
  }
  o <- tryCatch(stats::optim(c(log(s0), 0), nll), error = function(e) NULL)
  if (is.null(o)) return(c(scale = s0, df = 200))
  c(scale = exp(o$par[1]), df = 2.1 + (200 - 2.1) * stats::plogis(o$par[2]))
}

## the reconstruction arms and the p5_cause variants: ilm_anomaly()'s route,
## the default arm call for call, random draws included
ref_p <- function(X, arm, trim = 0.25, B = 39L, seed = 1L, diag = FALSE) {
  t_noise <- arm %in% c("heavy_kept", "heavy_kept_q90")
  how <- if (arm == "heavy_kept_q90") "q90" else if (arm == "nomatch") "none" else "median"
  n <- nrow(X); p <- ncol(X)
  ctr <- colMeans(X); scl <- apply(X, 2L, stats::sd); scl[!is.finite(scl) | scl <= 0] <- 1
  Z <- sweep(sweep(X, 2L, ctr, "-"), 2L, scl, "/")
  set.seed(seed)
  k <- if (arm == "rank2") 2L else max(1L, min(ilm_anom_rank(Z), p - 1L))
  obs <- ilm_anom_score(Z, k, trim)
  sv <- svd(Z, nu = k, nv = k)
  V <- sv$v[, seq_len(k), drop = FALSE]; d <- sv$d[seq_len(k)]
  Rin <- Z - Z %*% V %*% t(V)
  dfc <- if (arm == "nodf") 1 else (n * p) / max(n * p - (n + p) * k, 1)
  ## the noise's variance per column, in the package's order of operations
  ## (MAD squared, times the degrees-of-freedom factor, then times the
  ## matching ratio), so the default arm draws exactly what the package does
  if (t_noise) {
    kept <- order(obs$score)[seq_len(max(k + 2L, floor(n * (1 - trim))))]
    tp <- vapply(seq_len(p), function(j) fit_t(Rin[kept, j]), c(scale = 0, df = 0))
    rvar <- tp["scale", ]^2 * dfc
    draw <- function(rv) vapply(seq_len(p), function(j)
      stats::rt(n, tp["df", j]) * sqrt(rv[j]), numeric(n))
  } else {
    rvar <- apply(Rin, 2L, function(z) stats::mad(z)^2) * dfc
    draw <- function(rv) vapply(seq_len(p), function(j)
      stats::rnorm(n, 0, sqrt(rv[j])), numeric(n))
  }
  rvar[!is.finite(rvar) | rvar <= 0] <- .Machine$double.eps
  sim <- function(rv, reps, stat = FALSE) {
    sc <- vector("list", reps); Tk <- Ta <- numeric(reps)
    for (b in seq_len(reps)) {
      U <- matrix(stats::rnorm(n * k), n, k)
      f <- ilm_anom_score(scale(U %*% diag(d / sqrt(n), k, k) %*% t(V) + draw(rv)), k, trim)
      sc[[b]] <- f$score
      if (stat) { Tk[b] <- tail_stat(f, k, trim); Ta[b] <- tail_stat(f, k, trim, FALSE) }
    }
    list(score = unlist(sc), Tk = Tk, Ta = Ta)
  }
  if (how != "none") {
    cal <- sim(rvar, 3L)$score
    if (how == "q90") {
      mo <- stats::quantile(obs$score, 0.9, names = FALSE)
      mc <- stats::quantile(cal, 0.9, names = FALSE)
    } else { mo <- stats::median(obs$score); mc <- stats::median(cal) }
    if (is.finite(mc) && mc > 0) rvar <- rvar * (mo / mc)
  }
  nl <- sim(rvar, B, stat = diag)
  pv <- (1 + vapply(obs$score, function(s) sum(nl$score >= s), 0L)) / (length(nl$score) + 1)
  res <- list(p = pv, k = k)
  if (diag) {
    res$tail_kept <- (1 + sum(nl$Tk >= tail_stat(obs, k, trim))) / (B + 1)
    res$tail_all <- (1 + sum(nl$Ta >= tail_stat(obs, k, trim, FALSE))) / (B + 1)
  }
  res
}

## the mixture-density arm
MODELS <- c("EII", "VII", "EEI", "VVI", "EEE", "VVV")
mix_fit_score <- function(Z, trim, G = 1:4, models = MODELS) {
  q <- function(e) suppressMessages(suppressWarnings(e))
  m0 <- q(tryCatch(mclust::Mclust(Z, G = G, modelNames = models, verbose = FALSE),
                   error = function(e) NULL))
  if (is.null(m0)) m0 <- q(mclust::Mclust(Z, G = 1L, modelNames = "EEE", verbose = FALSE))
  ld <- function(m, W) mclust::dens(W, modelName = m$modelName, parameters = m$parameters,
                                    logarithm = TRUE)
  keep <- order(-ld(m0, Z))[seq_len(floor(nrow(Z) * (1 - trim)))]
  m <- q(tryCatch(mclust::Mclust(Z[keep, , drop = FALSE], G = m0$G, modelNames = m0$modelName,
                                 verbose = FALSE), error = function(e) NULL))
  if (is.null(m)) m <- m0
  list(score = -ld(m, Z), model = m)
}
mix_p <- function(X, trim = 0.25, B = 39L, seed = 1L) {
  Z <- scale(X); n <- nrow(Z)
  set.seed(seed)
  f <- mix_fit_score(Z, trim)
  mdl <- f$model
  null <- unlist(lapply(seq_len(B), function(b) {
    Zb <- scale(mclust::sim(mdl$modelName, mdl$parameters, n)[, -1L, drop = FALSE])
    mix_fit_score(Zb, trim, G = mdl$G, models = mdl$modelName)$score
  }))
  list(p = (1 + vapply(f$score, function(s) sum(null >= s), 0L)) / (length(null) + 1),
       k = mdl$G)
}

one_arm <- function(X, arm, diag = FALSE) if (arm == "mixture") mix_p(X) else ref_p(X, arm, diag = diag)

## ---- summary -----------------------------------------------------------------
if (length(args) && args[1] == "summarise") {
  r <- do.call(rbind, lapply(args[-1], utils::read.csv, stringsAsFactors = FALSE))
  ms <- function(z) { z <- z[is.finite(z)]; if (!length(z)) "" else sprintf("%.4f (%.4f)", mean(z), se(z)) }
  holds <- function(z) { z <- z[is.finite(z)]; mean(z) <= 0.05 + 2 * se(z) }
  cat(sprintf("the default arm reproduces ilm_anomaly()'s p exactly: %d of %d datasets checked\n\n",
              sum(r$check_ok, na.rm = TRUE), sum(!is.na(r$check_ok))))
  cl <- r[r$part %in% c("clean", "clustered"), ]
  cat("clean and clustered cells: share of datasets flagging anything, mean (MC SE); holds?\n")
  g <- split(cl, list(cl$arm, cl$p, cl$n, cl$noise, cl$part), drop = TRUE)
  print(do.call(rbind, lapply(g, function(d) data.frame(part = d$part[1], noise = d$noise[1],
        n = d$n[1], p = d$p[1], arm = d$arm[1], any_flag = ms(d$any_flag),
        holds = holds(d$any_flag), p_le_0.01 = ms(d$p_le_0.01)))), row.names = FALSE)
  pw <- r[r$part %in% c("power", "power_heavy"), ]
  cat("\nplanted rows flagged, averaged over shifts, mean (MC SE)\n")
  g <- split(pw, list(pw$arm, pw$cond, pw$noise), drop = TRUE)
  print(do.call(rbind, lapply(g, function(d) data.frame(noise = d$noise[1], cond = d$cond[1],
        arm = d$arm[1], flagged = ms(d$flagged), fdp = ms(d$fdp), false_flag = ms(d$false_flag)))),
        row.names = FALSE)
  cat("\nR1 (item 143):\n")
  ccl <- r[r$part == "clean", ]
  pow_n <- r[r$part == "power", ]; pow_all <- pw
  def_pow <- mean(pow_n$flagged[pow_n$arm == "default"])
  best <- NULL; bestp <- -Inf
  for (a in unique(ccl$arm)) {
    cells <- split(ccl$any_flag[ccl$arm == a],
                   interaction(ccl$noise, ccl$n, ccl$p)[ccl$arm == a], drop = TRUE)
    nh <- sum(vapply(cells, holds, TRUE))
    pn <- mean(pow_n$flagged[pow_n$arm == a]); pa <- mean(pow_all$flagged[pow_all$arm == a])
    ok <- nh == 36L && is.finite(pn) && pn >= 0.75 * def_pow
    cat(sprintf("  %-15s holds in %2d of %d clean cells; planted rows flagged %.4f (normal), %.4f (all); floor %.4f -> %s\n",
                a, nh, length(cells), pn, pa, 0.75 * def_pow, if (ok) "qualifies" else "does not"))
    if (ok && pa > bestp) { best <- a; bestp <- pa }
  }
  cat("  recommended:", if (is.null(best)) "none qualifies; nothing changes until the results are reviewed" else best, "\n")
  cat("\nR2 (item 144): share of datasets whose tail check has p <= t\n")
  tp <- r[r$arm == "default" & !is.na(r$tail_kept), ]
  tp$cell <- paste(tp$part, tp$noise, tp$n, tp$p, tp$cond, sep = "/")
  curve <- do.call(rbind, lapply(split(tp, tp$cell), function(d) data.frame(cell = d$cell[1],
            t(stats::setNames(vapply(THRESH, function(t) mean(d$tail_kept <= t), 0),
                              sprintf("t%.2f", THRESH))), check.names = FALSE)))
  print(curve, row.names = FALSE, digits = 3)
  cn <- tp[tp$part == "clean" & tp$noise == "normal", ]
  adm <- vapply(THRESH, function(t) all(vapply(split(cn$tail_kept, paste(cn$n, cn$p)), function(z) {
    F <- mean(z <= t); s <- if (F > 0) sqrt(F * (1 - F) / length(z)) else sqrt(0.09 / length(z))
    F <= 0.10 + 2 * s }, TRUE)), TRUE)
  thr <- if (any(adm)) max(THRESH[adm]) else NA
  cat(sprintf("  admissible thresholds: %s\n  the rule's threshold: %s\n",
              paste(sprintf("%.2f", THRESH[adm]), collapse = " "),
              if (is.na(thr)) "none (the curves are reviewed)" else
                sprintf("%.2f, attainable as %d/40 = %.3f", thr, floor(thr * 40 + 1e-9),
                        floor(thr * 40 + 1e-9) / 40)))
  cat("\nR3 (item 88, the five-column excess):\n")
  pc <- r[r$part == "p5_cause", ]
  hold_by <- function(v, pp) vapply(split(pc$any_flag[pc$arm == v & pc$p == pp],
                         pc$n[pc$arm == v & pc$p == pp]), holds, TRUE)
  d5 <- hold_by("default", 5); d8 <- hold_by("default", 8)
  for (v in c("rank2", "nodf", "nomatch")) {
    v5 <- hold_by(v, 5); v8 <- hold_by(v, 8)
    ok <- any(!d5) && all(v5) && all(v8[d8])
    cat(sprintf("  %-8s p=5 holds in %d of 3 (default %d); p=8 control %d of 3 -> %s\n", v,
                sum(v5), sum(d5), sum(v8), if (ok) "explains the excess" else "does not"))
  }
  cat("  rank chosen by the default at p = 5:", paste(names(table(pc$k[pc$arm == "default" & pc$p == 5])),
      table(pc$k[pc$arm == "default" & pc$p == 5]), sep = " x", collapse = ", "), "\n")
  cat("\nR4 (item 88, the mixture score fixes both failures):\n")
  mx <- r[r$arm == "mixture" & ((r$part == "clean" & r$noise != "normal") | r$part == "clustered"), ]
  if (nrow(mx)) {
    h <- vapply(split(mx$any_flag, paste(mx$part, mx$noise, mx$n, mx$p)), holds, TRUE)
    cat(sprintf("  holds in %d of %d heavy-tailed and clustered cells -> %s\n", sum(h), length(h),
                if (all(h)) "fixes both" else "does not"))
  }
  quit(save = "no")
}

## ---- the run -----------------------------------------------------------------
reps <- if (length(args) >= 1L) as.integer(args[1]) else 200L
out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else "anomaly_calibration3.csv"
parts <- if (length(args) >= 3L && nzchar(args[3])) strsplit(args[3], ",")[[1]] else
  c(paste0("clean_", noises), "clustered", "power", "power_heavy", "p5_cause")
run_arms <- if (length(args) >= 4L && nzchar(args[4])) strsplit(args[4], ",")[[1]] else ARMS
stopifnot(all(run_arms %in% ARMS))
## mclust::Mclust() calls mclustBIC() unqualified, so the package is attached
if ("mixture" %in% run_arms) suppressPackageStartupMessages(library(mclust))
power_reps <- min(reps, 50L)
## an optional fifth argument "from:to" runs only those replicates, so one
## part can be split across processes
## "k/m" instead takes every m-th replicate starting at the k-th, so m
## processes each take a like share of every cell and finish together
rsel <- if (length(args) >= 5L && nzchar(args[5])) args[5] else ""
rr <- if (grepl(":", rsel)) as.integer(strsplit(rsel, ":")[[1]]) else NULL
km <- if (grepl("/", rsel)) as.integer(strsplit(rsel, "/")[[1]]) else NULL
in_range <- function(s) (is.null(rr) || (s >= rr[1] && s <= rr[2])) &&
  (is.null(km) || (s - 1L) %% km[2] == km[1] - 1L)

## Checkpointing: every dataset and arm is one row, appended as it finishes;
## on a restart the rows already in the output file are skipped, so a run
## stopped at any point resumes where it left off.
key_of <- function(part, noise, n, p, arm, cond, shift, s)
  paste(part, noise, n, p, arm, cond, shift, s, sep = "|")
done <- if (file.exists(out)) {
  d0 <- utils::read.csv(out, stringsAsFactors = FALSE)
  key_of(d0$part, d0$noise, d0$n, d0$p, d0$arm, d0$cond, d0$shift, d0$rep)
} else character(0)
todo <- function(part, noise, n, p, arm, cond, shift, s)
  in_range(s) && !(key_of(part, noise, n, p, arm, cond, shift, s) %in% done)

write_row <- function(row) utils::write.table(row, out, sep = ",", append = file.exists(out),
                                              col.names = !file.exists(out), row.names = FALSE)
emit <- function(part, noise, n, p, arm, cond, shift, s, r, lab = NULL, check_ok = NA, t0) {
  f <- stats::p.adjust(r$p, "BH") < 0.05
  write_row(data.frame(part = part, noise = noise, n = n, p = p, arm = arm, cond = cond,
    shift = shift, rep = s, k = r$k,
    p_le_0.001 = mean(r$p <= 0.001), p_le_0.01 = mean(r$p <= 0.01), p_le_0.05 = mean(r$p <= 0.05),
    any_flag = as.numeric(any(f)),
    flagged = if (is.null(lab)) NA else mean(f[lab]),
    fdp = if (is.null(lab)) NA else if (any(f)) mean(!lab[f]) else 0,
    false_flag = if (is.null(lab)) NA else mean(f[!lab]),
    tail_kept = if (is.null(r$tail_kept)) NA else r$tail_kept,
    tail_all = if (is.null(r$tail_all)) NA else r$tail_all,
    check_ok = check_ok, seconds = round(proc.time()[["elapsed"]] - t0, 2)))
}
check <- function(X, pv) {
  a <- suppressMessages(suppressWarnings(
    ilm_anomaly(as.data.frame(X), trim = 0.25, seed = 1L, progress = FALSE)))
  isTRUE(all.equal(a$p[order(a$row)], pv))
}
run_cell <- function(part, noise, n, p, X, s, lab = NULL, cond = NA, shift = NA, do_check = FALSE,
                     arms = run_arms) {
  for (arm in arms) {
    if (!todo(part, noise, n, p, arm, cond, shift, s)) next
    t0 <- proc.time()[["elapsed"]]
    r <- one_arm(X, arm, diag = arm == "default")
    ck <- if (arm == "default" && do_check) check(X, r$p) else NA
    emit(part, noise, n, p, arm, cond, shift, s, r, lab, ck, t0)
  }
}

for (noise in noises) if (paste0("clean_", noise) %in% parts)
  for (n in c(150L, 400L, 1000L)) for (p in c(5L, 8L, 15L)) for (s in seq_len(reps))
    run_cell("clean", noise, n, p, make_clean(s, n, p, 2L, noise), s, do_check = s <= 20L)
if ("clustered" %in% parts)
  for (n in c(400L, 1000L)) for (p in c(5L, 8L, 15L)) for (s in seq_len(reps))
    run_cell("clustered", "normal", n, p, make_clustered(s, n, p), s)
conds <- list(shared10 = list(share = 0.10, how = "shared", base = 100000L),
              shared5 = list(share = 0.05, how = "shared", base = 100000L),
              random5 = list(share = 0.05, how = "random", base = 200000L),
              random2 = list(share = 0.02, how = "random", base = 300000L))
if ("power" %in% parts)
  for (cn in names(conds)) for (SH in 3:9) for (s in seq_len(power_reps)) {
    cd <- conds[[cn]]
    x <- make_data(cd$base + 1000L * SH + s, cd$share, cd$how, SH)
    run_cell("power", "normal", 400L, 8L, x$X, s, x$lab, cn, SH, do_check = TRUE)
  }
if ("power_heavy" %in% parts)
  for (nz in c("t5", "t3")) for (cn in names(conds)) for (SH in c(3L, 5L, 7L, 9L))
    for (s in seq_len(power_reps)) {
      cd <- conds[[cn]]
      x <- make_data((if (nz == "t5") 5e6 else 6e6) + cd$base + 1000L * SH + s,
                     cd$share, cd$how, SH, noise = nz)
      run_cell("power_heavy", nz, 400L, 8L, x$X, s, x$lab, cn, SH)
    }
if ("p5_cause" %in% parts)
  for (n in c(150L, 400L, 1000L)) for (p in c(5L, 8L)) for (s in seq_len(reps)) {
    X <- make_clean(s, n, p, 2L, "normal")
    for (v in c("default", "rank2", "nodf", "nomatch")) {
      if (!todo("p5_cause", "normal", n, p, v, NA, NA, s)) next
      t0 <- proc.time()[["elapsed"]]
      emit("p5_cause", "normal", n, p, v, NA, NA, s, ref_p(X, v), NULL, NA, t0)
    }
  }
cat("done:", paste(parts, collapse = ", "), "\n")
