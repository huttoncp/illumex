## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## When is a pile at a bound a floor: a confirmation run of one rule. A
## pre-registration, then the study.
##
## Why: study 2 (dev/studies/floor_rule2.R, results beside it) found no arm
## meeting both criteria. The nearest, G1_C001 -- the pile checked on every
## numeric variable with at least 5 distinct values, before describe's other
## gates, with a one-sided exact count test of the bound against the next
## value at 0.001 -- false-fired only on a count whose probabilities fall
## gently from zero (P(0) 0.111, P(1) 0.099), in 9.5% of replicates at 5,000
## rows, where the test finds a real but small difference. An effect-size
## floor beside the test was proposed after seeing those data: the bound must
## also hold at least 1.5 times the next value's count. Because 1.5 was
## chosen from study 2's data, this run is a confirmation, not a tuning step
## (a condition set on 30 September 2026):
##   - The rule is fixed now: gate G1, R0's minimum (c1 >= 5 and
##     c1 / n >= 0.02), the exact test at alpha = 0.001, and c1 >= 1.5 * c2.
##     Neither the ratio nor alpha changes after this run, whatever it shows;
##     the results are reported as they are.
##   - Fresh data seeds throughout: seed = 30,000 + replicate (study 1 and 2
##     used 1 to 200).
##   - Two holdout kinds, never used before, fixed here.
##   - A sensitivity line at ratios 1.3 and 2.0 (same gate, test and alpha),
##     reported beside the rule to show how much hangs on 1.5; they are
##     not candidates.
## The 90% detection criterion for 5% piles at 300 rows is known from study 2
## to be beyond any count-based rule (about 18 rows against 8 at the next
## value on a 0-10 rating): a power limit. It is kept as registered and
## reported as missed if missed; nothing is tuned toward it.
##
## Design, fixed before any run:
##   Arms (all gate G1; c1 the count at the bound, c2 at the next distinct
##   value inward; p the one-sided exact binomial P(X >= c1), X ~
##   Binomial(c1 + c2, 1/2)):
##     RULE   c1 >= 5, c1 / n >= 0.02, p <= 0.001, c1 >= 1.5 * c2
##     S13    the same with c1 >= 1.3 * c2 (sensitivity, reported only)
##     S20    the same with c1 >= 2.0 * c2 (sensitivity, reported only)
##   Data: study 2's kinds, the same sizes (n = 100, 300, 1,000, 5,000), 200
##   replicates, fresh seeds; scored and reported as in study 2 (panel ids
##   are fixed data and repeat study 2's), and two holdout kinds:
##     nb_ratio13   no floor, scored for false floors: negative binomial
##                  counts, size 1, mean 10/3 (geometric), so P(0) = 0.231
##                  and P(1) = 0.178, a ratio of 1.3 -- a moderate real
##                  difference that is the distribution's own shape
##     bb_rating    a pile, scored for detection: a 0-10 rating drawn
##                  beta-binomial (10 trials, Beta(4, 4) probability; P(0)
##                  about 0.005), then 10% of rows set to 0
##   Criteria (study 2's, applied to RULE only):
##     False floors: on each kind with no floor, at each n, at most 5% of
##       replicates fire at either bound; on panel_id, never.
##     Detection, over all replicates: on each kind with a pile, at n >= 300
##       and a pile >= 5%, at least 90% fire at the right bound.
##     The note states, for each criterion, whether RULE meets it and, where
##     not, which kinds and sizes miss and by how much.
##   Code: illumex from a pinned library (RemoteSha c3f3e93), as
##   before.
##   Pilot per size first (20 replicates of every kind at each n, timed, seeds
##   30,001 to 30,020); the estimate is ten times the sum. Estimate before
##   the pilot, from study 2's per-size timings with 44 kinds against 42:
##   about 3.8 minutes on one process (core D). Stop point: twice the pilot's
##   estimate.
##   Results: dev/studies/results/floor_rule3_summary.md; the table of every
##   data set is kept outside the repository (the seeds rebuild it).
##
## Pilot:      Rscript dev/studies/floor_rule3.R pilot <lib> <out_dir>
## Full run:   Rscript dev/studies/floor_rule3.R run <lib> <out.csv> <stop_seconds>
## Summarise:  Rscript dev/studies/floor_rule3.R summarise <out.csv> <summary.md>

args <- commandArgs(TRUE)
mode <- args[1]
SEED0 <- 30000L

ARMS <- c("RULE", "S13", "S20")
arm_fires <- function(c1, c2, n) {
  base <- c1 >= 5 && c1 / n >= 0.02 &&
    stats::pbinom(c1 - 1, c1 + c2, 0.5, lower.tail = FALSE) <= 0.001
  c(RULE = base && c1 >= 1.5 * c2, S13 = base && c1 >= 1.3 * c2, S20 = base && c1 >= 2.0 * c2)
}

held <- function(x, lo, hi) pmin(pmax(x, lo), hi)
pile <- function(x, s, at) { x[stats::runif(length(x)) < s] <- at; x }
likert <- function(p) function(n) sample.int(5, n, TRUE, prob = p) + 0
KINDS <- c(
  lapply(c(21, 30, 50, 100), function(k) list(kind = "unif_int", par = k, bound = NA, score = TRUE,
    make = function(n) sample.int(k, n, TRUE) + 0)),
  list(list(kind = "round_norm_0", par = NA, bound = NA, score = TRUE,
            make = function(n) round(stats::rnorm(n, 50, 10))),
       list(kind = "round_norm_1", par = NA, bound = NA, score = TRUE,
            make = function(n) round(stats::rnorm(n, 50, 10), 1)),
       list(kind = "pois30", par = NA, bound = NA, score = TRUE,
            make = function(n) stats::rpois(n, 30) + 0),
       list(kind = "nb_decreasing", par = NA, bound = NA, score = TRUE,
            make = function(n) stats::rnbinom(n, size = 1, mu = 8) + 0),
       list(kind = "normal", par = NA, bound = NA, score = TRUE,
            make = function(n) stats::rnorm(n)),
       list(kind = "rating_plain", par = NA, bound = NA, score = TRUE,
            make = function(n) held(round(stats::rnorm(n, 5, 2)), 0, 10)),
       list(kind = "pois3", par = NA, bound = NA, score = TRUE,
            make = function(n) stats::rpois(n, 3) + 0),
       list(kind = "pois1", par = NA, bound = NA, score = TRUE,
            make = function(n) stats::rpois(n, 1) + 0),
       list(kind = "likert_mid", par = NA, bound = NA, score = TRUE,
            make = likert(c(0.10, 0.20, 0.30, 0.25, 0.15))),
       ## holdout: a count whose zero holds 1.3 times the share at one
       list(kind = "nb_ratio13", par = NA, bound = NA, score = TRUE,
            make = function(n) stats::rnbinom(n, size = 1, mu = 10 / 3) + 0)),
  unlist(lapply(c(0.02, 0.05, 0.10, 0.20), function(s) list(
    list(kind = "cens_floor", par = s, bound = "min", score = TRUE,
         make = function(n) pmax(stats::rnorm(n), stats::qnorm(s))),
    list(kind = "cens_ceiling", par = s, bound = "max", score = TRUE,
         make = function(n) pmin(stats::rnorm(n), stats::qnorm(1 - s))),
    list(kind = "lod_floor", par = s, bound = "min", score = TRUE,
         make = function(n) pmax(stats::rlnorm(n), stats::qlnorm(s))),
    list(kind = "vas_floor", par = s, bound = "min", score = TRUE,
         make = function(n) pile(held(round(stats::rnorm(n, 45, 20)), 0, 100), s, 0)),
    list(kind = "rating_0_10", par = s, bound = "min", score = TRUE,
         make = function(n) pile(held(round(stats::rnorm(n, 5, 2)), 0, 10), s, 0)),
    list(kind = "rating_1_10", par = s, bound = "min", score = TRUE,
         make = function(n) pile(held(round(stats::rnorm(n, 5, 2)), 1, 10), s, 1)))),
    recursive = FALSE),
  lapply(c(0.05, 0.10, 0.20), function(s) list(kind = "zi_count", par = s, bound = "min", score = TRUE,
    make = function(n) ifelse(stats::runif(n) < s, 0, stats::rnbinom(n, size = 3, mu = 15) + 0))),
  ## holdout: a beta-binomial 0-10 rating with 10% of rows set to 0
  list(list(kind = "bb_rating", par = 0.10, bound = "min", score = TRUE,
            make = function(n) pile(stats::rbinom(n, 10, stats::rbeta(n, 4, 4)) + 0, 0.10, 0)),
       list(kind = "claims_like", par = NA, bound = NA, score = FALSE,
            make = function(n) illumex::ilm_sim(n_id = ceiling(n / 6), n_period = 6)$claims[seq_len(n)] + 0),
       list(kind = "likert_top", par = NA, bound = NA, score = FALSE,
            make = likert(c(0.02, 0.03, 0.10, 0.25, 0.60)))))
PANEL <- expand.grid(k = c(30, 100), m = c(3, 6, 10))

## one data set through gate G1 and every arm, at both bounds
assess <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  u <- sort(unique(x)); nu <- length(u)
  reach <- nu >= 5L
  cnt <- tabulate(match(x, u), nu)
  none <- stats::setNames(rep(FALSE, length(ARMS)), ARMS)
  lo <- if (reach) arm_fires(cnt[1], cnt[2], n) else none
  hi <- if (reach) arm_fires(cnt[nu], cnt[nu - 1L], n) else none
  c(list(n = n, nu = nu, reach = reach,
         c_min = cnt[1], c_next_min = if (nu >= 2L) cnt[2] else NA,
         c_max = cnt[nu], c_next_max = if (nu >= 2L) cnt[nu - 1L] else NA),
    stats::setNames(as.list(lo), paste0("min_", ARMS)), stats::setNames(as.list(hi), paste0("max_", ARMS)))
}

run_cells <- function(sizes, reps, out, panel = TRUE, stop_at = Inf) {
  rows <- list(); t0 <- proc.time()[["elapsed"]]
  for (kd in KINDS) for (n in sizes) for (r in reps) {
    if (proc.time()[["elapsed"]] - t0 > stop_at)
      stop(sprintf("stopped at the stop point, %.0f s, at %s n = %d", stop_at, kd$kind, n), call. = FALSE)
    set.seed(SEED0 + r)
    a <- assess(kd$make(n))
    rows[[length(rows) + 1L]] <- data.frame(kind = kd$kind, par = kd$par, bound = kd$bound,
      scored = kd$score, size = n, rep = r, a, stringsAsFactors = FALSE)
  }
  if (panel) for (i in seq_len(nrow(PANEL))) {
    k <- PANEL$k[i]; m <- PANEL$m[i]
    a <- assess(rep(seq_len(k), each = m) + 0)
    rows[[length(rows) + 1L]] <- data.frame(kind = "panel_id", par = k * 1000 + m, bound = NA,
      scored = TRUE, size = k * m, rep = 1L, a, stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, rows)
  utils::write.csv(res, out, row.names = FALSE)
  res
}

if (mode %in% c("pilot", "run")) {
  .libPaths(c(args[2], .libPaths()))
  suppressMessages(library(illumex))
  cat("illumex", format(utils::packageVersion("illumex")), "from", find.package("illumex"),
      "RemoteSha", utils::packageDescription("illumex")$RemoteSha %||% "none", "\n")
  if (mode == "pilot") {
    tot <- 0
    for (n in c(100, 300, 1000, 5000)) {
      t0 <- proc.time()[["elapsed"]]
      run_cells(n, 1:20, file.path(args[3], sprintf("pilot_%d.csv", n)), panel = FALSE)
      el <- proc.time()[["elapsed"]] - t0; tot <- tot + 10 * el
      cat(sprintf("pilot n = %5d: 20 replicates of %d kinds in %.1f s\n", n, length(KINDS), el))
    }
    cat(sprintf("full-run estimate %.1f min; stop point %.1f min (%.0f s)\n", tot / 60, 2 * tot / 60, 2 * tot))
  } else {
    t0 <- proc.time()[["elapsed"]]
    res <- run_cells(c(100, 300, 1000, 5000), 1:200, args[3], stop_at = as.numeric(args[4]))
    cat(sprintf("run: %d data sets in %.1f min\n", nrow(res), (proc.time()[["elapsed"]] - t0) / 60))
  }
}

if (mode == "summarise") {
  res <- utils::read.csv(args[2], stringsAsFactors = FALSE)
  res$label <- ifelse(is.na(res$par), res$kind, paste0(res$kind, " ", res$par))
  either <- function(d, a) d[[paste0("min_", a)]] | d[[paste0("max_", a)]]
  right <- function(d, a) ifelse(d$bound == "min", d[[paste0("min_", a)]], d[[paste0("max_", a)]])
  fmt <- function(t) c(paste("|", paste(names(t), collapse = " | "), "|"),
                       paste("|", paste(rep("---", ncol(t)), collapse = " | "), "|"),
                       apply(t, 1, function(r) paste("|", paste(r, collapse = " | "), "|")))
  num <- function(t) { for (j in seq_along(t)) if (is.numeric(t[[j]]) && names(t)[j] != "n")
                         t[[j]] <- sprintf("%.3f", t[[j]]); t }
  rate_tab <- function(d, f, bound_col) {
    tb <- do.call(rbind, lapply(split(d, list(d$label, d$size), drop = TRUE), function(s) {
      c1 <- if (bound_col) ifelse(s$bound == "min", s$c_min, s$c_max) else s$c_min
      c2 <- if (bound_col) ifelse(s$bound == "min", s$c_next_min, s$c_next_max) else s$c_next_min
      data.frame(data = s$label[1], n = s$size[1], at_bound = mean(c1 / s$n), next_value = mean(c2 / s$n),
                 t(vapply(ARMS, function(a) mean(f(s, a)), 0)), check.names = FALSE) }))
    tb[order(tb$data, tb$n), ]
  }
  out <- c("# Floor rule study 3 (confirmation): summary", "",
           "RULE is fixed (gate G1, exact test at 0.001, ratio >= 1.5); S13 and S20 are the sensitivity lines at ratios 1.3 and 2.0. Columns at_bound and next_value are mean shares at the minimum (false-floor tables) or at the pile's bound (detection).", "",
           "## False floors: the arm fires at either bound", "")
  null <- res[is.na(res$bound) & res$scored & res$kind != "panel_id", ]
  ft <- rate_tab(null, either, FALSE)
  out <- c(out, fmt(num(ft)), "", "## Panel identifiers (k ids, m rows each)", "")
  pid <- res[res$kind == "panel_id", ]
  pt <- data.frame(k = pid$par %/% 1000, m = pid$par %% 1000,
                   t(vapply(seq_len(nrow(pid)), function(i) vapply(ARMS, function(a) either(pid[i, ], a), TRUE),
                            logical(length(ARMS)))), check.names = FALSE)
  names(pt)[-(1:2)] <- ARMS
  out <- c(out, fmt(pt), "", "## Detection over all replicates: the arm fires at the right bound", "")
  tru <- res[!is.na(res$bound), ]
  dt <- rate_tab(tru, right, TRUE)
  dt$scored <- vapply(seq_len(nrow(dt)), function(i) {
    s <- tru[tru$label == dt$data[i], ][1, ]; s$scored && dt$n[i] >= 300 && s$par >= 0.05 }, TRUE)
  out <- c(out, fmt(num(dt)), "", "## Reported, not scored: either bound", "")
  rt <- rate_tab(res[is.na(res$bound) & !res$scored, ], either, FALSE)
  out <- c(out, fmt(num(rt)), "", "## Against the criteria", "")
  for (a in ARMS) {
    over <- ft[ft[[a]] > 0.05, ]; pid_hit <- any(pt[[a]])
    sc <- dt[dt$scored, ]; miss <- sc[sc[[a]] < 0.90, ]
    out <- c(out, sprintf("- %s%s: false floors above 5%%: %s; fires on a panel id: %s; scored detection below 90%%: %s.",
      a, if (a == "RULE") "" else " (sensitivity)",
      if (nrow(over)) paste(sprintf("%s at n = %d (%.3f)", over$data, over$n, over[[a]]), collapse = ", ") else "none",
      if (pid_hit) "yes" else "no",
      if (nrow(miss)) paste(sprintf("%s at n = %d (%.3f)", miss$data, miss$n, miss[[a]]), collapse = ", ") else "none"))
  }
  writeLines(out, args[3])
  cat(out, sep = "\n")
}
