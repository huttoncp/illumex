## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## S3 of the X7 plan (W3): how ilm_glrm() should choose its ridge penalty --
## over a wider grid, and more cheaply.
##
## The finding behind it: in S1 (dev/studies/glrm_scaling.R) the penalty
## chosen at n = 1,000 was the grid's top value, 10, in nearly every
## replicate, under the current and the scaled losses alike, so there the
## grid rather than the data set the penalty. And the choice costs 18 fits
## (six values by three folds, each from a cold start), more than the fit it
## is for. Scaled losses were made the default (W1), and scaling changes
## what a given penalty means, even on all-numeric data, so the study ran on
## that code: the scaled-losses branch plus the study. This design was
## approved on 2026-09-27.
##
## Design, fixed before any run.
##   Data, rank 2 throughout, 50 replicates per condition (the claim ranks
##   the arms):
##     numeric  glrm_lambda.R's design and seeds: a rank-2 signal in 4 columns
##              plus noise of sd 0.5, 20% of cells missing completely at
##              random (never a whole row); n = 150, 400 and 1,000
##     mixed    glrm_scaling.R's info_L6 design and seeds: numbers a, b and c
##              from two latents, a binary y, an informative 6-level factor w
##              and a pure-noise 3-level factor z1, 10% of cells missing;
##              n = 200 and 1,000
##   Arms. Every arm uses ilm_glrm()'s own selection settings: the observed
##   cells split into 3 folds by one draw under the seed, a held-out fit of
##   at most 200 passes, and a held-out cell scored as its squared error over
##   the column's observed sd for a number and as 1 for a misclassified
##   category, pooled over the folds. The folds are the same in every arm.
##     grid6   the package's choice: 0.1, 0.5, 1, 2, 5 and 10, each fitted
##             from a cold start (18 fits). Its fits are a subset of wide's,
##             so it is read off wide's; a check below confirms that it
##             reproduces ilm_glrm()'s own choice.
##     wide    the same with 20, 50 and 100 added (27 fits)
##     path    wide's grid walked from 100 down, each fold's fit started from
##             that fold's fit at the value before (a warm start, through an
##             option this branch alone reads); the walk stops once the pooled
##             held-out error has failed to improve on its best at two
##             consecutive values
##     path1   path on fold 1 alone
##     oracle  the grid value whose full-data fit has the smallest error at
##             the truly missing cells, in that replicate: a floor, not an arm
##   Every arm's final fit is the full-data fit at its chosen value, as
##   ilm_glrm() makes it (at most 300 passes, cold start), so the nine
##   full-data fits serve every arm; their errors are also printed by value,
##   which re-measures the numbers in ilm_glrm()'s help under scaled losses.
##   Outcomes, per condition, each mean with its Monte Carlo standard error,
##   and between arms with the paired standard error:
##     error    at the truly missing cells, scored as the selection scores
##              held-out cells (so for the mixed data numbers and categories
##              pooled), of the final fit at the chosen value
##     regret   error minus the oracle's
##     lambda   the chosen value, as counts
##     seconds  per selection, fitting and scoring, timed in the same
##              session; grid6's as the sum of its 18 fits' times within
##              wide's run
##     scaled   the chosen value over sqrt(n) + sqrt(d), d the encoded
##              columns: descriptive only, whether a grid on that scale would
##              suit
##   Check, before the results: in the first 3 replicates of each condition,
##   grid6's choice equals ilm_glrm(lambda = NULL)'s.
##   Decision rules, fixed now:
##     R1 the grid is widened to wide's if wide's error is worse than
##        grid6's by no more than two paired standard errors in every
##        condition and better by more than two in at least one;
##     R2 path1, then path, becomes the way of choosing if its error is worse
##        than both grid6's and wide's by no more than two paired standard
##        errors or 1%, whichever is larger, in every condition, and its
##        median seconds per selection are at most half of grid6's in every
##        condition; otherwise the cold three-fold search stays.
##
## Addendum, fixed before the full run (approved 2026-09-27).
## A one-replicate smoke run of the design above (at 746d6af)
## motivated it: at mixed_1000 every arm chose 10, while the error at the
## truly missing cells was flat at 0.359 to 0.368 from 0.1 to 5 and 0.405 at
## 10 (the oracle 0.1, a regret of 13%); at mixed_200 the arms chose 5 and
## the oracle 2. The likely cause is the split: holding out a third of the
## observed cells trains each fold on data far sparser than the real fit
## sees (about 40% missing against 10%), and sparser data wants more
## penalty. Two arms added, on a second split of the observed cells into ten
## folds, drawn the same way under the same seed:
##     path1_10  the warm path on fold 1 of the ten
##     path3_10  the warm path on folds 1 to 3 of the ten, pooled
##   R3 a ten-fold arm replaces the three-fold choice if its error is better
##      than grid6's by more than two paired standard errors in at least one
##      condition and worse by no more than two in any, and its median
##      seconds per selection are at most grid6's in every condition;
##      path1_10 is taken if it passes, path3_10 only if path1_10 does not.
##   R1 and R2 are reported as fixed above.
##
## Addendum 2, fixed after four of the five conditions' results and before
## any run of what it adds (approved 2026-09-27). Those
## results showed every warm path stuck: on the numeric data the path starts
## at 100, where the fit collapses to near-zero factors, and a warm start
## from there cannot leave, since with both factors near zero both gradients
## are too; the walk then stops at once (the path chose 100 in 21 of 50
## replicates at numeric_150). A direct check on one numeric_150 fold: the
## warm fits from 100 down to 5 kept |X| near 1e-4 and the objective at 328,
## the column means', where cold fits reached 317, 231 and 139; the cold
## start's own |X| was 4.7. The registered arms stand as run and are reported
## as they are. Added, on the same datasets and seeds, as a separate run:
##     path_fixed, path1_fixed, path1_10_fixed, path3_10_fixed
##               the four warm paths, except that a fold's fit starts cold
##               whenever its fit at the value before has collapsed: its
##               factors' size sqrt(|X|^2 + |Y|^2) below 1e-3 of the cold
##               start's (ilm_glrm() with no passes). The stuck fits sat
##               near 1e-4 of it, and a fit that truly collapses, at a
##               penalty too large for any factor, starts cold as well.
##     cold3_10  the wide grid, cold starts, on folds 1 to 3 of the ten,
##               pooled: the ten-fold split without the warm start, so the
##               two can be told apart
## The run also times grid6 and wide again (grid6_a2, wide_a2), so that the
## new arms' seconds are compared within one session. R2 is applied to
## path1_fixed, then path_fixed, and R3 to path1_10_fixed, then
## path3_10_fixed, their wording unchanged; cold3_10 is descriptive, under no
## rule.
##
## Run from the package root:
##   Rscript dev/studies/glrm_penalty.R [reps] [outfile.csv] [conditions] [set]
## `set` is "registered" (the default) or "addendum2".
## `conditions` (optional) is a comma-separated subset of numeric_150,
## numeric_400, numeric_1000, mixed_200 and mixed_1000, so the work can be
## split. The parts' CSVs are read together, with the rules applied, by
##   Rscript dev/studies/glrm_penalty.R summarise part1.csv part2.csv ...
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
if (!exists("ilm_glrm_best_constant_loss"))
  stop("this study runs on the scaled-losses tree (W1)")
options(width = 200)
args <- commandArgs(TRUE)

GRID <- c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100)  # ascending: a tie goes to the smaller value, as in ilm_glrm_lambda()
G6 <- GRID <= 10
RANK <- 2L; FOLDS <- 3L; SEED <- 1L; TOL <- 1e-7
ALL <- c("numeric_150", "numeric_400", "numeric_1000", "mixed_200", "mixed_1000")
ARMS <- c("grid6", "wide", "path", "path1", "path1_10", "path3_10", "oracle")
ARMS2 <- c("path_fixed", "path1_fixed", "path1_10_fixed", "path3_10_fixed", "cold3_10")
COLLAPSE <- 1e-3   # addendum 2: a fit whose factors fall below this share of the cold start's

## glrm_lambda.R's generator, unchanged
make_numeric <- function(seed, n = 400L, p = 4L, miss = 0.2) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * 2), n, 2) %*% matrix(stats::rnorm(2 * p), 2, p) +
    matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  M <- matrix(stats::runif(n * p) < miss, n, p)
  M[rowSums(M) == p, 1] <- FALSE
  Xm <- X; Xm[M] <- NA
  colnames(Xm) <- colnames(X) <- paste0("x", seq_len(p))
  list(d = as.data.frame(Xm), full = as.data.frame(X), miss = M)
}

## glrm_scaling.R's generator for info_L6, unchanged
make_mixed <- function(seed, n) {
  set.seed(seed)
  g <- rep(1:3, length.out = n)
  f1 <- c(-2, 0, 2)[g] + stats::rnorm(n, 0, 0.6); f2 <- stats::rnorm(n)
  d <- data.frame(a = f1 + stats::rnorm(n, 0, 0.4), b = -f1 + stats::rnorm(n, 0, 0.4),
                  c = f2 + stats::rnorm(n, 0, 0.4))
  d$y <- factor(ifelse(stats::plogis(2.5 * f1) > stats::runif(n), "hi", "lo"))
  noisef <- function(L) factor(sample(paste0("l", seq_len(L)), n, TRUE))
  h <- f2 + stats::rnorm(n, 0, 0.4)
  d$w <- factor(cut(h, stats::quantile(h, 0:6 / 6), include.lowest = TRUE, labels = FALSE))
  d$z1 <- noisef(3)
  full <- d
  M <- matrix(stats::runif(n * ncol(d)) < 0.10, n, ncol(d))
  M[rowSums(M) == ncol(d), 1] <- FALSE
  for (j in seq_len(ncol(d))) d[[j]][M[, j]] <- NA
  list(d = d, full = full, miss = M)
}

## the two studies' seeds: glrm_lambda.R's for the numeric design, and
## glrm_scaling.R's for info_L6 (its fourth scenario)
make_cond <- function(cond, s) {
  n <- as.integer(sub(".*_", "", cond))
  if (startsWith(cond, "numeric")) make_numeric(1000L * (n %/% 50L) + s, n)
  else make_mixed(400000L + 10L * (n %/% 100L) + 1000L * s, n)
}

## ilm_glrm_lambda()'s scoring: squared error over the column's observed sd
## for a number, 1 for a misclassified category
score_cells <- function(fitted, truth, obs, rows) {
  e <- 0; m <- 0L
  for (j in seq_along(obs)) {
    i <- rows[[j]]
    if (!length(i)) next
    tv <- truth[[j]][i]; pv <- fitted[[j]][i]
    if (is.numeric(tv)) {
      sc <- stats::sd(obs[[j]], na.rm = TRUE)
      if (!is.finite(sc) || sc <= 0) sc <- 1
      e <- e + sum(((as.numeric(pv) - tv) / sc)^2, na.rm = TRUE)
    } else {
      e <- e + sum(as.character(pv) != as.character(tv), na.rm = TRUE)
    }
    m <- m + length(i)
  }
  c(e = e, m = m)
}

## ilm_glrm_lambda()'s folds, drawn the same way under the same seed; with
## k = 10, the addendum's ten-fold split
folds_of <- function(sub, k = FOLDS) {
  n <- nrow(sub)
  obs <- which(!is.na(as.matrix(
    data.frame(lapply(sub, function(z) as.numeric(as.factor(z)))))))
  set.seed(SEED)
  fold <- sample(rep_len(seq_len(k), length(obs)))
  lapply(seq_len(k), function(f) {
    hold <- obs[fold == f]
    rr <- ((hold - 1L) %% n) + 1L
    cc <- ((hold - 1L) %/% n) + 1L
    sh <- sub
    for (j in unique(cc)) sh[rr[cc == j], j] <- NA
    list(sh = sh, rows = lapply(seq_along(sub), function(j) rr[cc == j]))
  })
}

fit_one <- function(dat, lam, start = NULL, maxit = 200L) {
  old <- options(illumex.glrm_start = start)
  on.exit(options(old))
  tryCatch(suppressMessages(suppressWarnings(
    ilm_glrm(dat, rank = RANK, lambda = lam, maxit = maxit, tol = TOL,
             seed = SEED, progress = FALSE))),
    error = function(e) NULL)
}
elapsed <- function() proc.time()[["elapsed"]]
pick <- function(vals, err) {
  err[is.na(err)] <- Inf
  if (all(!is.finite(err))) 1 else vals[which.min(err)]
}

## wide: every value in every fold, cold; grid6 is its first six rows
select_wide <- function(fl, sub) {
  E <- M <- S <- matrix(0, length(GRID), FOLDS)
  for (k in seq_along(GRID)) for (f in seq_len(FOLDS)) {
    t0 <- elapsed()
    fit <- fit_one(fl[[f]]$sh, GRID[k])
    if (!is.null(fit)) {
      sc <- score_cells(fit$fitted, sub, sub, fl[[f]]$rows)
      E[k, f] <- sc[["e"]]; M[k, f] <- sc[["m"]]
    }
    S[k, f] <- elapsed() - t0
  }
  cv <- ifelse(rowSums(M) > 0, rowSums(E) / rowSums(M), Inf)
  list(wide = pick(GRID, cv), grid6 = pick(GRID[G6], cv[G6]),
       sec_wide = sum(S), sec_grid6 = sum(S[G6, ]))
}

## path: down the grid, each fold warm-started from its fit at the value
## above; with `fixed` (addendum 2), started cold instead when that fit has
## collapsed
size_of <- function(X, Y) sqrt(sum(X^2) + sum(Y^2))
select_path <- function(fl, sub, folds, fixed = FALSE) {
  t0 <- elapsed()
  st <- vector("list", length(fl))
  cold <- rep(NA_real_, length(fl))
  if (fixed) for (f in folds) {
    c0 <- fit_one(fl[[f]]$sh, GRID[1L], maxit = 0L)
    if (!is.null(c0)) cold[f] <- size_of(as.matrix(c0$scores), c0$archetypes)
  }
  err <- rep(NA_real_, length(GRID)); best <- Inf; since <- 0L; nfit <- 0L
  for (k in rev(seq_along(GRID))) {
    e <- 0; m <- 0
    for (f in folds) {
      if (fixed && !is.null(st[[f]]) && is.finite(cold[f]) &&
          size_of(st[[f]]$X, st[[f]]$Y) < COLLAPSE * cold[f]) st[f] <- list(NULL)
      fit <- fit_one(fl[[f]]$sh, GRID[k], start = st[[f]]); nfit <- nfit + 1L
      if (is.null(fit)) { st[f] <- list(NULL); next }
      st[[f]] <- list(X = as.matrix(fit$scores), Y = fit$archetypes, mu = fit$offset)
      sc <- score_cells(fit$fitted, sub, sub, fl[[f]]$rows)
      e <- e + sc[["e"]]; m <- m + sc[["m"]]
    }
    err[k] <- if (m) e / m else Inf
    if (err[k] < best) { best <- err[k]; since <- 0L } else since <- since + 1L
    if (since >= 2L) break
  }
  list(lambda = pick(GRID, err), sec = elapsed() - t0, fits = nfit)
}

## cold3_10 (addendum 2): the wide grid, cold, on the given folds, pooled
select_cold <- function(fl, sub, folds) {
  t0 <- elapsed()
  err <- vapply(GRID, function(lam) {
    e <- 0; m <- 0
    for (f in folds) {
      fit <- fit_one(fl[[f]]$sh, lam)
      if (is.null(fit)) next
      sc <- score_cells(fit$fitted, sub, sub, fl[[f]]$rows)
      e <- e + sc[["e"]]; m <- m + sc[["m"]]
    }
    if (m) e / m else Inf
  }, 0)
  list(lambda = pick(GRID, err), sec = elapsed() - t0, fits = length(GRID) * length(folds))
}

run <- function(reps, conds, set = "registered") {
  rows <- list(); checks <- list()
  for (cond in conds) for (s in seq_len(reps)) {
    x <- make_cond(cond, s)
    n <- nrow(x$d)
    miss_rows <- lapply(seq_along(x$d), function(j) which(x$miss[, j]))
    ## the full-data fit at every value: the final fit of every arm, and the oracle
    dd <- NA_integer_
    curve <- vapply(GRID, function(lam) {
      fit <- fit_one(x$d, lam, maxit = 300L)
      if (is.null(fit)) return(NA_real_)
      dd <<- fit$encoding$d
      sc <- score_cells(fit$fitted, x$full, x$d, miss_rows)
      sc[["e"]] / sc[["m"]]
    }, 0)
    fl <- folds_of(x$d)
    w <- select_wide(fl, x$d)
    fl10 <- folds_of(x$d, 10L)
    orc <- pick(GRID, curve)
    at <- function(lam) curve[match(lam, GRID)]
    add <- function(arm, lam, sec, fits)
      rows[[length(rows) + 1L]] <<- data.frame(
        cond = cond, n = n, d = dd, rep = s, arm = arm, lambda = lam,
        error = at(lam), regret = at(lam) - at(orc), seconds = sec, fits = fits)
    if (set == "addendum2") {
      add("grid6_a2", w$grid6, w$sec_grid6, sum(G6) * FOLDS)
      add("wide_a2", w$wide, w$sec_wide, length(GRID) * FOLDS)
      for (a in list(list("path_fixed", fl, seq_len(FOLDS)), list("path1_fixed", fl, 1L),
                     list("path1_10_fixed", fl10, 1L), list("path3_10_fixed", fl10, 1:3))) {
        r <- select_path(a[[2]], x$d, a[[3]], fixed = TRUE)
        add(a[[1]], r$lambda, r$sec, r$fits)
      }
      c3 <- select_cold(fl10, x$d, 1:3)
      add("cold3_10", c3$lambda, c3$sec, c3$fits)
      next
    }
    p3 <- select_path(fl, x$d, seq_len(FOLDS))
    p1 <- select_path(fl, x$d, 1L)
    p1_10 <- select_path(fl10, x$d, 1L)
    p3_10 <- select_path(fl10, x$d, 1:3)
    add("grid6", w$grid6, w$sec_grid6, sum(G6) * FOLDS)
    add("wide", w$wide, w$sec_wide, length(GRID) * FOLDS)
    add("path", p3$lambda, p3$sec, p3$fits)
    add("path1", p1$lambda, p1$sec, p1$fits)
    add("path1_10", p1_10$lambda, p1_10$sec, p1_10$fits)
    add("path3_10", p3_10$lambda, p3_10$sec, p3_10$fits)
    add("oracle", orc, NA_real_, NA_integer_)
    for (k in seq_along(GRID)) add(sprintf("fixed %g", GRID[k]), GRID[k], NA_real_, NA_integer_)
    if (s <= 3L) {
      own <- suppressMessages(suppressWarnings(
        ilm_glrm(x$d, rank = RANK, seed = SEED, progress = FALSE)))$lambda
      checks[[length(checks) + 1L]] <- data.frame(cond = cond, rep = s,
        grid6 = w$grid6, ilm_glrm = own, same = isTRUE(all.equal(own, w$grid6)))
    }
  }
  list(res = do.call(rbind, rows), checks = do.call(rbind, checks))
}

se <- function(z) { z <- z[is.finite(z)]; if (length(z) < 2L) NA else stats::sd(z) / sqrt(length(z)) }
fmt <- function(z, dig = 3) { z <- z[is.finite(z)]
  if (!length(z)) "" else sprintf(paste0("%.", dig, "f (%.", dig, "f)"), mean(z), se(z)) }

report <- function(res, final) {
  conds <- intersect(ALL, unique(res$cond))
  reps <- max(res$rep)
  cat(sprintf("\nerror at the truly missing cells of the final fit, mean (Monte Carlo SE) over %d replicates\n", reps))
  tab <- do.call(rbind, lapply(conds, function(cd) do.call(rbind, lapply(ARMS, function(a) {
    r <- res[res$cond == cd & res$arm == a, ]
    data.frame(cond = cd, arm = a, error = fmt(r$error), regret = fmt(r$regret),
               median_seconds = if (a == "oracle") "" else sprintf("%.2f", stats::median(r$seconds)),
               mean_fits = if (a == "oracle") "" else sprintf("%.1f", mean(r$fits)),
               scaled_lambda = sprintf("%.3f", stats::median(r$lambda / (sqrt(r$n) + sqrt(r$d)))))
  }))))
  print(tab, row.names = FALSE)

  cat("\nlambda chosen, counts\n")
  for (cd in conds) for (a in ARMS) {
    ch <- res$lambda[res$cond == cd & res$arm == a]
    tb <- table(factor(ch, levels = GRID))
    tb <- tb[tb > 0]
    cat(sprintf("  %-12s %-6s ", cd, a), paste(names(tb), tb, sep = " x", collapse = ", "), "\n")
  }

  cat(sprintf("\nerror at fixed values of lambda (every arm's final fit is one of these), mean (Monte Carlo SE)\n"))
  fx <- do.call(rbind, lapply(conds, function(cd) {
    r <- res[res$cond == cd & startsWith(res$arm, "fixed"), ]
    v <- vapply(GRID, function(l) fmt(r$error[r$lambda == l]), "")
    data.frame(cond = cd, t(stats::setNames(v, paste0("lambda_", GRID))), check.names = FALSE)
  }))
  print(fx, row.names = FALSE)

  ## paired differences, arm minus reference, over the same replicates
  pair <- function(cd, a, b) {
    ra <- res[res$cond == cd & res$arm == a, ]; rb <- res[res$cond == cd & res$arm == b, ]
    z <- ra$error[order(ra$rep)] - rb$error[order(rb$rep)]
    c(diff = mean(z), pse = se(z), ref = mean(rb$error))
  }
  cmp <- list(c("wide", "grid6"), c("path", "grid6"), c("path", "wide"),
              c("path1", "grid6"), c("path1", "wide"),
              c("path1_10", "grid6"), c("path3_10", "grid6"))
  cat("\npaired differences in error, arm minus reference: mean (paired SE)\n")
  pd <- do.call(rbind, lapply(conds, function(cd) {
    data.frame(cond = cd, t(vapply(cmp, function(ab) {
      p <- pair(cd, ab[1], ab[2]); sprintf("%+.4f (%.4f)", p[["diff"]], p[["pse"]])
    }, "")), check.names = FALSE)
  }))
  names(pd)[-1] <- vapply(cmp, paste, "", collapse = " - ")
  print(pd, row.names = FALSE)

  cat(if (final) "\ndecision rules, fixed before the run\n" else
        "\ndecision rules on this part's conditions only (the rules apply to all five)\n")
  ## isTRUE(): with one replicate (a smoke run) there is no standard error,
  ## and a rule that cannot be read is reported as not met
  r1_all <- all(vapply(conds, function(cd) { p <- pair(cd, "wide", "grid6"); isTRUE(p[["diff"]] <= 2 * p[["pse"]]) }, TRUE))
  r1_any <- any(vapply(conds, function(cd) { p <- pair(cd, "wide", "grid6"); isTRUE(p[["diff"]] < -2 * p[["pse"]]) }, TRUE))
  cat(sprintf("  R1 widen the grid: no worse than grid6 in every condition %s; better in at least one %s -> %s\n",
              r1_all, r1_any, if (r1_all && r1_any) "WIDEN" else "keep grid6"))
  for (a in c("path1", "path")) {
    ok_err <- all(vapply(conds, function(cd) all(vapply(c("grid6", "wide"), function(b) {
      p <- pair(cd, a, b); isTRUE(p[["diff"]] <= max(2 * p[["pse"]], 0.01 * p[["ref"]]))
    }, TRUE)), TRUE))
    ok_sec <- all(vapply(conds, function(cd) {
      stats::median(res$seconds[res$cond == cd & res$arm == a]) <=
        0.5 * stats::median(res$seconds[res$cond == cd & res$arm == "grid6"])
    }, TRUE))
    cat(sprintf("  R2 %-5s error within tolerance of grid6 and wide everywhere %s; at most half grid6's median time everywhere %s -> %s\n",
                a, ok_err, ok_sec, if (ok_err && ok_sec) "ADOPT" else "no"))
  }
  r3 <- NULL
  for (a in c("path1_10", "path3_10")) {
    no_worse <- all(vapply(conds, function(cd) { p <- pair(cd, a, "grid6"); isTRUE(p[["diff"]] <= 2 * p[["pse"]]) }, TRUE))
    better <- any(vapply(conds, function(cd) { p <- pair(cd, a, "grid6"); isTRUE(p[["diff"]] < -2 * p[["pse"]]) }, TRUE))
    ok_sec <- all(vapply(conds, function(cd) {
      stats::median(res$seconds[res$cond == cd & res$arm == a]) <=
        stats::median(res$seconds[res$cond == cd & res$arm == "grid6"])
    }, TRUE))
    pass <- no_worse && better && ok_sec
    cat(sprintf("  R3 %-8s no worse than grid6 anywhere %s; better in at least one %s; median time at most grid6's everywhere %s -> %s
",
                a, no_worse, better, ok_sec,
                if (pass && is.null(r3)) "ADOPT" else if (pass) "passes, but path1_10 is taken first" else "no"))
    if (pass && is.null(r3)) r3 <- a
  }
}

## addendum 2's arms: the same tables, errors paired with the registered
## run's grid6 and wide where it is given (the same fits on the same data),
## otherwise with this run's; seconds against this run's grid6_a2, timed in
## the same session. R2 and R3 are applied to the corrected arms as worded.
report2 <- function(res, final) {
  conds <- intersect(ALL, unique(res$cond[res$arm %in% ARMS2]))
  reps <- max(res$rep[res$arm %in% ARMS2])
  arms <- c("grid6_a2", "wide_a2", ARMS2)
  cat(sprintf("
addendum 2, a separate run on the same datasets and seeds: error at the truly missing cells, mean (Monte Carlo SE) over %d replicates
", reps))
  tab <- do.call(rbind, lapply(conds, function(cd) do.call(rbind, lapply(arms, function(a) {
    r <- res[res$cond == cd & res$arm == a, ]
    data.frame(cond = cd, arm = a, error = fmt(r$error), regret = fmt(r$regret),
               median_seconds = sprintf("%.2f", stats::median(r$seconds)),
               mean_fits = sprintf("%.1f", mean(r$fits)))
  }))))
  print(tab, row.names = FALSE)
  cat("
lambda chosen, counts (addendum 2)
")
  for (cd in conds) for (a in ARMS2) {
    tb <- table(factor(res$lambda[res$cond == cd & res$arm == a], levels = GRID))
    tb <- tb[tb > 0]
    cat(sprintf("  %-12s %-14s ", cd, a), paste(names(tb), tb, sep = " x", collapse = ", "), "
")
  }
  g6 <- if (any(res$arm == "grid6")) "grid6" else "grid6_a2"
  wd <- if (any(res$arm == "wide")) "wide" else "wide_a2"
  pair <- function(cd, a, b) {
    ra <- res[res$cond == cd & res$arm == a, ]; rb <- res[res$cond == cd & res$arm == b, ]
    z <- ra$error[order(ra$rep)] - rb$error[order(rb$rep)]
    c(diff = mean(z), pse = se(z), ref = mean(rb$error))
  }
  med <- function(cd, a) stats::median(res$seconds[res$cond == cd & res$arm == a])
  cmp <- c(lapply(ARMS2, function(a) c(a, g6)), lapply(ARMS2[1:2], function(a) c(a, wd)))
  cat(sprintf("
paired differences in error, arm minus reference (%s, %s): mean (paired SE)
", g6, wd))
  pd <- do.call(rbind, lapply(conds, function(cd)
    data.frame(cond = cd, t(vapply(cmp, function(ab) {
      p <- pair(cd, ab[1], ab[2]); sprintf("%+.4f (%.4f)", p[["diff"]], p[["pse"]])
    }, "")), check.names = FALSE)))
  names(pd)[-1] <- vapply(cmp, paste, "", collapse = " - ")
  print(pd, row.names = FALSE)
  cat(if (final) "
R2 and R3 on the corrected arms, as worded
" else
        "
R2 and R3 on the corrected arms, on this part's conditions only
")
  for (a in c("path1_fixed", "path_fixed")) {
    ok_err <- all(vapply(conds, function(cd) all(vapply(c(g6, wd), function(b) {
      p <- pair(cd, a, b); isTRUE(p[["diff"]] <= max(2 * p[["pse"]], 0.01 * p[["ref"]]))
    }, TRUE)), TRUE))
    ok_sec <- all(vapply(conds, function(cd) med(cd, a) <= 0.5 * med(cd, "grid6_a2"), TRUE))
    cat(sprintf("  R2 %-14s error within tolerance of grid6 and wide everywhere %s; at most half grid6's median time everywhere %s -> %s
",
                a, ok_err, ok_sec, if (ok_err && ok_sec) "ADOPT" else "no"))
  }
  r3 <- NULL
  for (a in c("path1_10_fixed", "path3_10_fixed")) {
    no_worse <- all(vapply(conds, function(cd) { p <- pair(cd, a, g6); isTRUE(p[["diff"]] <= 2 * p[["pse"]]) }, TRUE))
    better <- any(vapply(conds, function(cd) { p <- pair(cd, a, g6); isTRUE(p[["diff"]] < -2 * p[["pse"]]) }, TRUE))
    ok_sec <- all(vapply(conds, function(cd) med(cd, a) <= med(cd, "grid6_a2"), TRUE))
    pass <- no_worse && better && ok_sec
    cat(sprintf("  R3 %-14s no worse than grid6 anywhere %s; better in at least one %s; median time at most grid6's everywhere %s -> %s
",
                a, no_worse, better, ok_sec,
                if (pass && is.null(r3)) "ADOPT" else if (pass) "passes, but path1_10_fixed is taken first" else "no"))
    if (pass && is.null(r3)) r3 <- a
  }
  cat("  cold3_10 is descriptive, under no rule
")
}

if (length(args) && args[1] == "summarise") {
  res <- do.call(rbind, lapply(args[-1], utils::read.csv))
  if (any(res$arm == "grid6")) report(res[!res$arm %in% c(ARMS2, "grid6_a2", "wide_a2"), ],
                                      final = setequal(unique(res$cond), ALL))
  if (any(res$arm %in% ARMS2)) report2(res, final = setequal(unique(res$cond[res$arm %in% ARMS2]), ALL))
} else {
  reps <- if (length(args) >= 1L) as.integer(args[1]) else 50L
  out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else NA_character_
  conds <- if (length(args) >= 3L && nzchar(args[3])) intersect(ALL, strsplit(args[3], ",")[[1]]) else ALL
  set <- if (length(args) >= 4L && nzchar(args[4])) args[4] else "registered"
  stopifnot(set %in% c("registered", "addendum2"))
  r <- run(reps, conds, set)
  if (set == "registered") {
    cat("check: grid6's choice against ilm_glrm(lambda = NULL)'s own, first 3 replicates of each condition
")
    print(r$checks, row.names = FALSE)
    report(r$res, final = setequal(conds, ALL))
  } else report2(r$res, final = setequal(conds, ALL))
  if (!is.na(out)) utils::write.csv(r$res, out, row.names = FALSE)
}
