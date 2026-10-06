## Which penalty suits a GLRM reduction that feeds clustering: the one chosen
## for imputation, the same capped at 10, or one chosen to keep structure.
##
## Why: item 140's selector chooses the penalty by held-out imputation error,
## which on weakly structured data rightly shrinks the fit toward the column
## means. ilm_reduce(method = "glrm") and ilm_profile() on that route use the
## same selector, and there the shrinkage empties the dimensions: on the
## recorded print case the penalty went from 10 (the old grid's cap) to 20
## and the first three dimensions from 25.0% of the numeric variance to 1.4%.
## Approved as option (3), a short registered study, before the choice
## between one selector for both and a different rule for reductions.
##
## Design, fixed before any run:
##   Build: this branch (scaled losses, W1, with item 140's selector), run
##   with pkgload from this commit.
##   Data, n = 300 rows, three clusters of equal expected size (truth k = 3),
##   10% of cells missing completely at random (never a whole row), 20
##   replicates per condition, seeds fixed:
##     numeric_strong  six numeric columns from two latent dimensions; the
##                     clusters' centres 3 apart on the latents (sd 1)
##     numeric_weak    the same, centres 1.2 apart
##     mixed_strong    four numeric columns as above, a four-level factor
##                     taking its cluster's own level with probability 0.7
##                     (else uniform over the other three), and a
##                     three-level factor of pure noise; centres 3 apart
##     mixed_weak      the same, centres 1.2 apart and the factor's
##                     probability 0.4
##   Rules, each choosing the penalty for ilm_reduce(ndim = 5, method =
##   "glrm"), which is then fitted at that penalty:
##     ruled    the default: item 140's selector as ilm_glrm() runs it
##              (lambda = NULL)
##     cap10    the same selector, path and folds, on the grid cut at 10
##              (0.1, 0.5, 1, 2, 5, 10)
##     onese    the least penalty whose held-out error is within one
##              standard error of the best: cold fits at every value of the
##              grid 0.1 to 100 on the same three held-out folds of ten, the
##              standard error over held-out cells. Named in advance, with
##              its reason: among the penalties the held-out cells cannot
##              tell apart, it keeps the most structure, where the default
##              selector takes the most shrinkage the data allow.
##   Outcomes per replicate and rule:
##     lambda    the penalty chosen
##     var3      the share of the numeric columns' variance the first three
##               dimensions explain (the reduction's cum_pct_var[3])
##     ari       adjusted Rand index of ilm_cluster(k = 3) on the reduction
##               against the true clusters
##     jaccard   the mean of its clusters' bootstrap Jaccard stability
##               (ilm_cluster's own, B = 50, seed 1)
##   Each reported as the mean over replicates with its Monte Carlo standard
##   error, and against ruled as the paired difference with its paired
##   standard error.
##   Decision rule, fixed now. cap10 or onese is preferred over ruled for
##   reductions if (i) its mean ari exceeds ruled's by at least 0.05, with the
##   paired difference more than two paired standard errors, in at least two
##   of the four conditions, and (ii) in no condition is its mean ari below
##   ruled's by more than 0.02. If both qualify, the one with the higher mean
##   ari over the four conditions; within 0.01 of each other, cap10 (the
##   simpler). If neither qualifies, one selector for both is recommended.
##   var3 and jaccard are reported beside it, and decide nothing.
##   Check, before the results: in the first 3 replicates of each condition,
##   the study's own call of the selector on the full grid gives
##   ilm_glrm(lambda = NULL)'s choice.
##   Cost, from a smoke run of 2 replicates: about 1 hour for the four
##   conditions above (350 s for 2 replicates each).
##
## Addendum, before any run of the study: two conditions added. A smoke
## run of 2 replicates, not kept, showed the default selector choosing 1 to
## 5 in all four conditions, never the heavy shrinkage that raised the
## question: every condition's columns share strong low-rank structure, even
## where the clusters are weak. A probe (glrm_reduce_probe_noisy.R, 2 draws a
## setting) found the selector at 10 and 20 once the columns' own noise has
## sd 2.5. So:
##     numeric_noisy   as numeric_strong, the columns' noise sd 2.5
##     mixed_noisy     as mixed_strong, the numeric columns' noise sd 2.5
## The decision rule stands as written, over the six conditions.
##
## Run from the package root:
##   Rscript dev/studies/glrm_reduce_penalty.R [reps] [outfile.csv]
##   Rscript dev/studies/glrm_reduce_penalty.R summarise outfile.csv
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
options(width = 200)
args <- commandArgs(TRUE)
CONDS <- c("numeric_strong", "numeric_weak", "mixed_strong", "mixed_weak",
           "numeric_noisy", "mixed_noisy")
N <- 300L; K <- 3L; NDIM <- 5L; SEED <- 1L

make_cond <- function(cond, s) {
  set.seed(700000L + 1000L * match(cond, CONDS) + s)
  noisy <- endsWith(cond, "noisy")
  strong <- endsWith(cond, "strong") || noisy
  gap <- if (strong) 3 else 1.2
  noise_sd <- if (noisy) 2.5 else 0.5
  g <- sample(rep_len(seq_len(K), N))
  centres <- rbind(c(0, 0), c(gap, 0), c(0, gap))
  z <- centres[g, ] + matrix(stats::rnorm(N * 2), N, 2)
  mixed <- startsWith(cond, "mixed")
  p <- if (mixed) 4L else 6L
  X <- z %*% matrix(stats::rnorm(2 * p), 2, p) + matrix(stats::rnorm(N * p, sd = noise_sd), N, p)
  d <- as.data.frame(X); names(d) <- paste0("x", seq_len(p))
  if (mixed) {
    pr <- if (strong) 0.7 else 0.4
    own <- stats::runif(N) < pr
    other <- vapply(g, function(k) sample(setdiff(1:4, k), 1L), 1L)
    d$f <- factor(paste0("l", ifelse(own, g, other)), levels = paste0("l", 1:4))
    d$noise <- factor(sample(paste0("n", 1:3), N, TRUE))
  }
  M <- matrix(stats::runif(N * ncol(d)) < 0.10, N, ncol(d))
  M[rowSums(M) == ncol(d), 1] <- FALSE
  for (j in seq_len(ncol(d))) d[[j]][M[, j]] <- NA
  list(d = d, g = g)
}

ari <- function(a, b) {
  t <- table(a, b); n <- sum(t)
  s <- sum(choose(t, 2)); sa <- sum(choose(rowSums(t), 2)); sb <- sum(choose(colSums(t), 2))
  e <- sa * sb / choose(n, 2); m <- (sa + sb) / 2
  if (m == e) return(1)
  (s - e) / (m - e)
}
losses <- function(d) as.list(vapply(d, ilm_glrm_loss_of, ""))
q <- function(e) suppressMessages(suppressWarnings(e))

## the selector as ilm_glrm() calls it, on a given grid
select_path <- function(d, grid)
  q(ilm_glrm_lambda(d, losses(d), NDIM, 300L, 1e-7, SEED, grid = grid))

## onese: cold fits on every value, folds 1 to 3 of ilm_glrm_lambda()'s ten,
## the least value within one standard error (over held-out cells) of the best
select_onese <- function(d, grid = ILM_GLRM_GRID) {
  n <- nrow(d)
  obs <- which(!is.na(as.matrix(data.frame(lapply(d, function(z) as.numeric(as.factor(z)))))))
  set.seed(SEED)
  fold <- sample(rep_len(seq_len(10L), length(obs)))
  cells <- lapply(grid, function(lam) {
    unlist(lapply(1:3, function(f) {
      hold <- obs[fold == f]
      rr <- ((hold - 1L) %% n) + 1L; cc <- ((hold - 1L) %/% n) + 1L
      sh <- d
      for (j in unique(cc)) sh[rr[cc == j], j] <- NA
      fit <- q(tryCatch(ilm_glrm(sh, rank = NDIM, lambda = lam, maxit = 200L,
                                 seed = SEED, progress = FALSE), error = function(e) NULL))
      if (is.null(fit)) return(NULL)
      unlist(lapply(seq_along(d), function(j) {
        i <- rr[cc == j]
        if (!length(i)) return(NULL)
        tv <- d[[j]][i]; pv <- fit$fitted[[j]][i]
        if (is.numeric(tv)) {
          sc <- stats::sd(d[[j]], na.rm = TRUE); if (!is.finite(sc) || sc <= 0) sc <- 1
          ((as.numeric(pv) - tv) / sc)^2
        } else as.numeric(as.character(pv) != as.character(tv))
      }))
    }))
  })
  err <- vapply(cells, function(e) if (length(e)) mean(e) else Inf, 0)
  se <- vapply(cells, function(e) if (length(e) > 1L) stats::sd(e) / sqrt(length(e)) else Inf, 0)
  b <- which.min(err)
  grid[which(err <= err[b] + se[b])[1L]]
}

outcomes <- function(d, g, lam) {
  red <- q(ilm_reduce(d, ndim = NDIM, method = "glrm", lambda = lam, progress = FALSE))
  cl <- q(ilm_cluster(red, k = K, B = 50L, seed = SEED, progress = FALSE))
  c(var3 = red$eig$cum_pct_var[3L], ari = ari(cl$ind_cluster$cluster, g),
    jaccard = mean(cl$clusters$jaccard))
}

if (length(args) && args[1] == "summarise") {
  r <- utils::read.csv(args[2], stringsAsFactors = FALSE)
  mse <- function(z) sprintf("%.3f (%.3f)", mean(z), stats::sd(z) / sqrt(length(z)))
  cat("mean (Monte Carlo SE) over replicates\n")
  tab <- do.call(rbind, lapply(split(r, list(r$cond, r$rule), drop = TRUE), function(x)
    data.frame(cond = x$cond[1], rule = x$rule[1], ari = mse(x$ari), var3 = mse(x$var3),
               jaccard = mse(x$jaccard), lambda = paste(names(table(x$lambda)), table(x$lambda),
                                                        sep = " x", collapse = ", "))))
  print(tab[order(match(tab$cond, CONDS), tab$rule), ], row.names = FALSE)
  cat("\nthe check: the study's selector call equals ilm_glrm(lambda = NULL)'s\n")
  ck <- r[r$rule == "ruled" & !is.na(r$check), ]
  cat(sprintf("  %d of %d agree\n", sum(ck$check), nrow(ck)))
  cat("\npaired against ruled, ari: mean difference (paired SE)\n")
  qual <- list()
  for (rl in c("cap10", "onese")) {
    up <- 0L; bad <- FALSE; means <- numeric(0)
    for (cd in CONDS) {
      a <- r[r$cond == cd & r$rule == rl, ]; b <- r[r$cond == cd & r$rule == "ruled", ]
      m <- merge(a, b, by = "rep"); dd <- m$ari.x - m$ari.y
      se <- stats::sd(dd) / sqrt(length(dd))
      cat(sprintf("  %-6s %-15s %+.3f (%.3f)\n", rl, cd, mean(dd), se))
      if (mean(dd) >= 0.05 && mean(dd) > 2 * se) up <- up + 1L
      if (mean(dd) < -0.02) bad <- TRUE
      means <- c(means, mean(a$ari))
    }
    qual[[rl]] <- list(ok = up >= 2L && !bad, mean = mean(means))
    cat(sprintf("  %s: better in %d condition(s), worse by more than 0.02 in %s -> %s\n", rl, up,
                if (bad) "at least one" else "none", if (qual[[rl]]$ok) "qualifies" else "does not"))
  }
  ok <- Filter(function(z) z$ok, qual)
  verdict <- if (!length(ok)) "one selector for both (neither qualifies)" else if (length(ok) == 1L)
    paste(names(ok), "for reductions") else {
      d <- qual$onese$mean - qual$cap10$mean
      if (abs(d) <= 0.01) "cap10 for reductions (both qualify, within 0.01)" else
        paste(if (d > 0) "onese" else "cap10", "for reductions (both qualify)")
    }
  cat("\ndecision, by the rule fixed before the run:", verdict, "\n")
  quit(save = "no")
}

reps <- if (length(args) >= 1L) as.integer(args[1]) else 20L
out <- if (length(args) >= 2L) args[2] else "glrm_reduce_penalty.csv"
for (cond in CONDS) for (s in seq_len(reps)) {
  x <- make_cond(cond, s)
  t0 <- proc.time()[["elapsed"]]
  lam_r <- q(ilm_glrm(x$d, rank = NDIM, lambda = NULL, progress = FALSE))$lambda
  check <- if (s <= 3L) identical(select_path(x$d, ILM_GLRM_GRID), lam_r) else NA
  lams <- c(ruled = lam_r, cap10 = select_path(x$d, ILM_GLRM_GRID[ILM_GLRM_GRID <= 10]),
            onese = select_onese(x$d))
  for (rl in names(lams)) {
    o <- outcomes(x$d, x$g, lams[[rl]])
    utils::write.table(data.frame(cond = cond, rep = s, rule = rl, lambda = lams[[rl]],
                                  var3 = o[["var3"]], ari = o[["ari"]], jaccard = o[["jaccard"]],
                                  check = if (rl == "ruled") check else NA,
                                  seconds = round(proc.time()[["elapsed"]] - t0, 1)),
                       out, sep = ",", append = file.exists(out), col.names = !file.exists(out),
                       row.names = FALSE)
  }
  cat(cond, s, "done\n")
}
