## How many rows the conditional driver needs to search for a category's
## neighbours, before it is built.
##
## driver_redesign.R adopted the conditional driver: a column's share of a
## row's isolation-forest score is how far the score falls when the column is
## replaced by its value predicted from the other columns -- a number by a
## linear regression on them, a category by the commonest level among the
## row's 10 nearest rows in Gower distance on them. Every studied case (up to
## 400 rows) searched all rows, which is n by n and impossible at 100,000.
## It was settled (2026-09-28) to search a pool of at most 2,000
## rows, on condition that the pool is measured first: this is that
## measurement.
##
## Design, fixed before any run.
##   Data: anomaly_methods.R's generator at n = 10,000 rows, 4% planted, for
##   the kinds whose driver is a category or a number: cross_type,
##   rare_combo and marginal (shift 2); 20 replicates each, seeds 900000 +
##   1000 x kind + replicate.
##   One forest per dataset, fitted as ilm_anomaly(method = "iforest",
##   seed = 1) fits it. The rows studied are 200 drawn at random from the
##   flagged ones (the top 4% by score, as many as were planted), where a
##   driver is read.
##   For each such row the conditional driver is computed with the
##   category's neighbours searched among all 10,000 rows (the reference),
##   and among pools of 500, 1,000, 2,000 and 5,000 rows drawn at random
##   (without the row itself); the numbers' regressions use every row in all
##   of them, since they scale.
##   Outcomes, each with its Monte Carlo standard error over replicates: the
##   share of studied rows whose driver from the pool equals the reference's,
##   and seconds per 200 rows for each pool and for the reference.
##   Decision rule, fixed now: keep 2,000 if its agreement
##   is at least 0.95 on every kind; otherwise the smallest pool that
##   reaches 0.95 on every kind, and the help says which.
##
## Run from the package root:
##   Rscript dev/studies/driver_pool.R [reps] [outfile.csv]
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
if (!requireNamespace("isotree", quietly = TRUE)) stop("this study needs isotree")
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 20L
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
  } else if (kind == "cross_type") {
    wrong <- c(p = "r", q = "p", r = "p")
    d$f1[i] <- wrong[as.character(d$f1[i])]
    d$f2[i][d$f1[i] == "p"] <- "low"
  } else if (kind == "rare_combo") {
    d$f1[i] <- "p"; d$f2[i] <- "high"
  }
  list(d = d, lab = seq_len(n) %in% i, rows = i)
}

## Gower distance from each of the rows `at` to each of the rows `pool`, on
## the columns `cols`: a number's absolute difference over its range, a
## category's mismatch, averaged over the columns (as cluster::daisy()'s
## "gower" does)
gower_to <- function(d, cols, at, pool) {
  D <- matrix(0, length(at), length(pool))
  for (v in cols) {
    x <- d[[v]]
    D <- D + if (is.numeric(x)) {
      r <- diff(range(x)); if (!is.finite(r) || r == 0) r <- 1
      abs(outer(x[at], x[pool], "-")) / r
    } else outer(as.character(x[at]), as.character(x[pool]), "!=") * 1
  }
  D / length(cols)
}

## the conditional driver of the rows `at`, a category's neighbours searched
## among `pool` (NULL: every row)
driver_at <- function(fit, d, at, pool = NULL, k = 10L) {
  score <- as.numeric(stats::predict(fit, d[at, , drop = FALSE]))
  M <- vapply(names(d), function(v) {
    others <- setdiff(names(d), v)
    x <- d[[v]]
    pred <- if (is.numeric(x)) {
      mf <- data.frame(y = x, d[others])
      stats::predict(stats::lm(y ~ ., data = mf), newdata = mf[at, , drop = FALSE])
    } else {
      pl <- if (is.null(pool)) seq_len(nrow(d)) else pool
      D <- gower_to(d, others, at, pl)
      D[outer(at, pl, "==")] <- Inf                   # never the row itself
      nb <- apply(D, 1L, function(r) pl[order(r)[seq_len(k)]])
      lv <- apply(nb, 2L, function(j) names(which.max(table(x[j]))))
      factor(lv, levels = levels(x))
    }
    dd <- d[at, , drop = FALSE]; dd[[v]] <- pred
    score - as.numeric(stats::predict(fit, dd))
  }, numeric(length(at)))
  colnames(M)[max.col(M, ties.method = "first")]
}

n <- 10000L
kinds <- c(cross_type = 1L, rare_combo = 2L, marginal = 3L)
pools <- c(500L, 1000L, 2000L, 5000L)
rows <- list()
for (kind in names(kinds)) for (s in seq_len(reps)) {
  x <- make_data(kind, 900000L + 1000L * kinds[[kind]] + s, n, 2)
  d <- x$d
  fit <- isotree::isolation.forest(d, ntrees = 500L, ndim = 1L, seed = 1L, nthreads = 1L)
  sc <- as.numeric(stats::predict(fit, d))
  set.seed(s)
  flagged <- order(-sc)[seq_len(round(0.04 * n))]
  at <- sort(sample(flagged, 200L))
  t_ref <- system.time(ref <- driver_at(fit, d, at))[["elapsed"]]
  for (P in pools) {
    pool <- sort(sample(n, P))
    t_p <- system.time(drv <- driver_at(fit, d, at, pool))[["elapsed"]]
    rows[[length(rows) + 1L]] <- data.frame(kind = kind, rep = s, pool = P,
                                            agree = mean(drv == ref), seconds = t_p,
                                            ref_seconds = t_ref)
  }
}
res <- do.call(rbind, rows)
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
fmt <- function(z) sprintf("%.3f (%.3f)", mean(z), stats::sd(z) / sqrt(length(z)))
tab <- do.call(rbind, lapply(split(res, list(res$pool, res$kind)), function(r)
  data.frame(kind = r$kind[1], pool = r$pool[1], agree = fmt(r$agree),
             seconds = fmt(r$seconds), all_rows_seconds = fmt(r$ref_seconds))))
cat(sprintf(paste0("agreement of the pooled conditional driver with the all-rows one, 200 flagged rows",
                   " of %d; mean (Monte Carlo SE) over %d replicates\n"), n, reps))
print(tab[order(match(tab$kind, names(kinds)), tab$pool), ], row.names = FALSE, right = TRUE)
