## Which column made the row odd: a redesign of the isolation forest's
## driver, measured before it is built.
##
## The fault. ilm_anomaly(method = "iforest") names as a row's driver the
## column whose replacement by its median (a category: its commonest level)
## lowers the row's score the most. Against planted anomalies
## (anomaly_methods.R, lof_option.R) that names the changed column for 0.83
## to 0.98 of rows pushed out of range and 0.84 of rare category pairs, but
## for only 0.16 to 0.25 of rows whose category contradicts their numbers:
## the commonest level usually contradicts the numbers too, so replacing the
## category barely lowers the score, while pulling a number to its median
## often does.
##
## Design, fixed before any run (approved 2026-09-28).
##   Data: anomaly_methods.R's generator, conditions and seeds, unchanged --
##   n = 150 or 400 rows; four numeric columns driven by two latents; f1 set
##   by the first latent's tercile and f2 independent except that the pair
##   f1 = p, f2 = high never occurs; 4% of rows planted as
##     marginal     n1 raised by 2 or 4 of its standard deviations
##     cross_type   f1 replaced by a level its latent tercile never gives
##     rare_combo   f1 = p and f2 = high
##   (combination, which has no single driver, is not scored and not run).
##   50 replicates per condition.
##   One forest per dataset, fitted exactly as ilm_anomaly(method =
##   "iforest", seed = 1) fits it; the arms differ only in how a column's
##   share of the score is measured, each by predictions from that forest:
##     current           the column set to its median or commonest level, as
##                       the package does now (checked to reproduce
##                       ilm_anomaly()'s drivers exactly on every dataset)
##     best_alternative  the largest fall in score over the column's
##                       plausible values: a number's deciles, a category's
##                       levels (its 20 commonest at most)
##     conditional       the column replaced by its value predicted from the
##                       other columns: a number by a linear regression on
##                       them, a category by the commonest level among the 10
##                       nearest other rows in Gower distance on them
##   The driver is the column whose measure is largest.
##   Outcomes, each with its Monte Carlo standard error: the share of
##   planted rows whose driver is right -- n1 for marginal, f1 for
##   cross_type, f1 or f2 for rare_combo -- and the seconds each arm adds
##   to a scan.
##   Decision rule, fixed now: the arm that most raises cross_type's share
##   is adopted, provided its marginal and rare_combo shares each stay
##   within two Monte Carlo standard errors of current's or above them; if
##   no arm does, the package keeps current and its help states the rates.
##
## Run from the package root:
##   Rscript dev/studies/driver_redesign.R [reps] [outfile.csv]
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
for (p in c("isotree", "cluster"))
  if (!requireNamespace(p, quietly = TRUE)) stop("this study needs ", p)
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 50L
out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else NA_character_

## anomaly_methods.R's generator, unchanged
make_data <- function(kind, seed, n, shift) {
  set.seed(seed)
  m <- round(0.04 * n)
  L <- matrix(stats::rnorm(n * 2), n, 2)
  W <- rbind(c(0.8, 0.8, 0, 0), c(0, 0, 0.8, 0.8))
  X <- L %*% W + matrix(stats::rnorm(n * 4, sd = 0.5), n, 4)
  colnames(X) <- paste0("n", 1:4)
  d <- as.data.frame(X)
  terc <- cut(L[, 1], stats::quantile(L[, 1], 0:3 / 3), include.lowest = TRUE,
              labels = c("p", "q", "r"))
  d$f1 <- factor(as.character(terc), levels = c("p", "q", "r"))
  d$f2 <- factor(sample(c("high", "low"), n, TRUE), levels = c("high", "low"))
  d$f2[d$f1 == "p"] <- "low"
  i <- sample(n, m)
  if (kind == "marginal") {
    d$n1[i] <- d$n1[i] + shift * stats::sd(d$n1)
  } else if (kind == "combination") {
    u <- c(1, -1, 1, -1) / 2
    Y <- as.matrix(d[paste0("n", 1:4)])
    lo <- apply(Y, 2, min); hi <- apply(Y, 2, max)
    for (r in i) Y[r, ] <- pmin(pmax(Y[r, ] + shift * 0.5 * u, lo), hi)
    d[paste0("n", 1:4)] <- as.data.frame(Y)
  } else if (kind == "cross_type") {
    wrong <- c(p = "r", q = "p", r = "p")
    d$f1[i] <- wrong[as.character(d$f1[i])]
    d$f2[i][d$f1[i] == "p"] <- "low"
  } else if (kind == "rare_combo") {
    d$f1[i] <- "p"; d$f2[i] <- "high"
  }
  list(d = d, lab = seq_len(n) %in% i, rows = i)
}

## each arm's measure of every column's share of every row's score
arm_current <- function(fit, d, score) {
  typ <- lapply(d, function(v) if (is.numeric(v))
    stats::median(v, na.rm = TRUE) else
      factor(names(sort(table(v), decreasing = TRUE))[1], levels = levels(factor(v))))
  vapply(names(d), function(v) {
    dd <- d; dd[[v]] <- typ[[v]]
    score - as.numeric(stats::predict(fit, dd))
  }, numeric(nrow(d)))
}
arm_best <- function(fit, d, score) {
  vapply(names(d), function(v) {
    x <- d[[v]]
    alts <- if (is.numeric(x)) unique(stats::quantile(x, 1:9 / 10, na.rm = TRUE, names = FALSE))
            else utils::head(names(sort(table(x), decreasing = TRUE)), 20L)
    low <- rep(Inf, nrow(d))
    for (a in alts) {
      dd <- d
      dd[[v]] <- if (is.numeric(x)) a else factor(a, levels = levels(x))
      low <- pmin(low, as.numeric(stats::predict(fit, dd)))
    }
    score - low
  }, numeric(nrow(d)))
}
arm_conditional <- function(fit, d, score, k = 10L) {
  vapply(names(d), function(v) {
    others <- d[setdiff(names(d), v)]
    x <- d[[v]]
    pred <- if (is.numeric(x)) {
      mf <- data.frame(y = x, others)
      stats::predict(stats::lm(y ~ ., data = mf), newdata = mf)
    } else {
      D <- as.matrix(cluster::daisy(others, metric = "gower"))
      diag(D) <- Inf
      nb <- apply(D, 1L, function(r) order(r)[seq_len(k)])
      lv <- apply(nb, 2L, function(j) names(which.max(table(x[j]))))
      factor(lv, levels = levels(x))
    }
    dd <- d; dd[[v]] <- pred
    score - as.numeric(stats::predict(fit, dd))
  }, numeric(nrow(d)))
}
driver <- function(M) colnames(M)[max.col(M, ties.method = "first")]

## anomaly_methods.R's conditions in its order, so its seeds give the same
## datasets; combination is skipped
conds <- rbind(
  expand.grid(kind = c("marginal", "combination"), shift = c(2, 4),
              n = c(150L, 400L), stringsAsFactors = FALSE),
  expand.grid(kind = c("cross_type", "rare_combo"), shift = NA,
              n = c(150L, 400L), stringsAsFactors = FALSE))
right <- function(kind, drv) switch(kind, marginal = drv == "n1",
                                    cross_type = drv == "f1",
                                    rare_combo = drv %in% c("f1", "f2"))
rows <- list(); checked <- 0L; mismatch <- 0L
for (ci in seq_len(nrow(conds))) {
  cd <- conds[ci, ]
  if (cd$kind == "combination") next
  for (s in seq_len(reps)) {
    x <- make_data(cd$kind, 10000L * ci + s, cd$n,
                   if (is.na(cd$shift)) 0 else cd$shift)
    d <- x$d
    fit <- isotree::isolation.forest(d, ntrees = 500L, ndim = 1L, seed = 1L,
                                     nthreads = 1L)
    score <- as.numeric(stats::predict(fit, d))
    t_cur <- system.time(M_cur <- arm_current(fit, d, score))[["elapsed"]]
    t_best <- system.time(M_best <- arm_best(fit, d, score))[["elapsed"]]
    t_cond <- system.time(M_cond <- arm_conditional(fit, d, score))[["elapsed"]]
    a <- suppressMessages(suppressWarnings(
      ilm_anomaly(d, method = "iforest", seed = 1L, progress = FALSE)))
    checked <- checked + 1L
    mismatch <- mismatch + as.integer(!identical(a$driver[order(a$row)], driver(M_cur)))
    for (arm in c("current", "best_alternative", "conditional")) {
      M <- switch(arm, current = M_cur, best_alternative = M_best, conditional = M_cond)
      rows[[length(rows) + 1L]] <- data.frame(
        kind = cd$kind, shift = cd$shift, n = cd$n, rep = s, arm = arm,
        right = mean(right(cd$kind, driver(M)[x$rows])),
        seconds = switch(arm, current = t_cur, best_alternative = t_best,
                         conditional = t_cond))
    }
  }
}
res <- do.call(rbind, rows)
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
cat(sprintf("the current arm reproduces ilm_anomaly()'s drivers exactly: %d of %d datasets\n\n",
            checked - mismatch, checked))
fmt <- function(z) sprintf("%.3f (%.3f)", mean(z), stats::sd(z) / sqrt(length(z)))
key <- paste(res$kind, res$shift, res$n, res$arm)
tab <- do.call(rbind, lapply(unique(key), function(k) {
  r <- res[key == k, ]
  data.frame(kind = r$kind[1], shift = r$shift[1], n = r$n[1], arm = r$arm[1],
             right = fmt(r$right), seconds = fmt(r$seconds))
}))
cat(sprintf(paste0("share of planted rows whose driver is right, and seconds per scan;",
                   " mean (Monte Carlo SE) over %d replicates\n"), reps))
print(tab, row.names = FALSE, right = TRUE)
