## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## When is a pile at a bound a floor: a count test, and the pile checked
## before describe's other gates. A pre-registration, then the study.
##
## Why: the first study (dev/studies/floor_rule.R, results beside it) found
## no fixed-ratio rule that met both criteria -- A3, a pile at least three
## times the next value's share, came closest (8.5% false floors, 83%
## detection) -- and found that the gates before the test hide real piles: a
## whole-number variable with at most max(20, n / 50) values is never tested
## (a 0-10 rating with 22% at 0 gets "discrete; bounded at zero"; counts with
## 10% excess zeros lose their floor note between 300 and 5,000 rows), and the
## shape score hides a 5% floor in continuous data at 300 rows. This study
## measures (a) a one-sided count test of the pile against the next value
## and (b) the pile check run before the discrete and score gates. Approved
## as a measurement, 30 September 2026; nothing about
## describe changes until it is decided.
##
## Design, fixed before any run:
##   Gates (which variables reach the test at a bound):
##     G0  as now: shape score below 0.95, not discrete (whole numbers with
##         at most max(20, n / 50) distinct values), more than 20 distinct
##         values
##     G1  every numeric variable with at least 5 distinct values, before
##         the discrete and score gates
##   Tests, each at the minimum (a floor) and the maximum (a ceiling); c1 is
##   the count at the bound, c2 the count at the next distinct value inward:
##     R0     c1 >= 5 and c1 / n >= 0.02 (now)
##     A3     R0 and c1 >= 3 * c2 (the first study's nearest)
##     C01    R0 and a one-sided exact binomial test that the bound holds
##            more than the next value, given c1 + c2 values at the two
##            (P(X >= c1) with X ~ Binomial(c1 + c2, 1/2)), at or below 0.01
##     C001   the same at or below 0.001
##   Arms: every gate with every test, eight in all.
##   Data. Sizes n = 100, 300, 1,000 and 5,000; 200 replicates each, data
##   seed = replicate number, so every arm sees the same data.
##   With no floor or ceiling (scored for false floors), the first study's:
##     unif_int (k = 21, 30, 50, 100), round_norm_0, round_norm_1, pois30,
##     nb_decreasing (a count whose probabilities fall from zero), normal,
##     panel_id (k = 30, 100 ids; m = 3, 6, 10 rows each; one replicate)
##   and, since G1 tests variables with few values:
##     rating_plain  round(N(5, 2)) held to 0-10, no pile added (0 holds
##                   1.2%, 1 holds 3.2%)
##     pois3         Poisson counts, mean 3 (probabilities rise from zero)
##     pois1         Poisson counts, mean 1 (P(0) = P(1) = 0.37)
##     likert_mid    a 1-5 item, probabilities 0.10, 0.20, 0.30, 0.25, 0.15
##   With a pile at a bound (scored for detection), share s added at the
##   bound = 0.02, 0.05, 0.10, 0.20, the first study's:
##     cens_floor, cens_ceiling, lod_floor, vas_floor (0-100), zi_count
##     (s = 0.05, 0.10, 0.20)
##   and, now scored:
##     rating_0_10   round(N(5, 2)) held to 0-10, a share s set to 0
##     rating_1_10   round(N(5, 2)) held to 1-10, a share s set to 1
##   Reported, not scored:
##     claims_like   ilm_sim()'s claims, as before
##     likert_top    a 1-5 item, probabilities 0.02, 0.03, 0.10, 0.25, 0.60:
##                   60% at the top is a ceiling effect by the usual
##                   psychometric reading (more than 15% at the maximum),
##                   but it is also the item's mode; shown, not scored
##
##   Criteria, fixed now:
##     False floors: on each kind with no floor, at each n, an arm fires at
##       either bound in at most 5% of replicates; on panel_id, never.
##     Detection, now scored over all replicates (the gates are under test,
##       so a pile the gate never lets through counts as missed): on each
##       kind with a pile, at n >= 300 and s >= 0.05, the arm fires at the
##       right bound in at least 90% of replicates. s = 0.02 and n = 100
##       are reported, not scored.
##     An arm that meets both is a candidate. Among candidates, the note
##     recommends the one with the lowest worst-case false-floor rate, then
##     the highest detection at s = 0.02 and n >= 300. If none meets both,
##     the note says which criterion each misses, where and by how much.
##   Code: illumex from a pinned library (RemoteSha c3f3e93, whose
##   ilm_gauss_assess() is the same as main's at bdc6262), through its
##   internals, so gate G0 is the package's own.
##
##   The pilot, per size, before the estimate stands: 20 replicates of every
##   kind at each n, timed; the full-run estimate is the sum over sizes of
##   ten times each size's pilot. Estimate before the pilot, from the first
##   study's per-size timings, 42 kinds against 33: about 3.7 minutes on
##   one process (core D). Stop point: twice the pilot-based estimate.
##   Results: dev/studies/results/floor_rule2_summary.md; the table of every
##   data set is kept outside the repository (the seeds rebuild it).
##
## Pilot:      Rscript dev/studies/floor_rule2.R pilot <lib> <out_dir>
## Full run:   Rscript dev/studies/floor_rule2.R run <lib> <out.csv> <stop_seconds>
## Summarise:  Rscript dev/studies/floor_rule2.R summarise <out.csv> <summary.md>

args <- commandArgs(TRUE)
mode <- args[1]

TESTS <- c("R0", "A3", "C01", "C001")
GATES <- c("G0", "G1")
ARMS <- as.vector(outer(GATES, TESTS, paste, sep = "_"))
test_fires <- function(c1, c2, n) {
  r0 <- c1 >= 5 && c1 / n >= 0.02
  pv <- stats::pbinom(c1 - 1, c1 + c2, 0.5, lower.tail = FALSE)
  c(R0 = r0, A3 = r0 && c1 >= 3 * c2, C01 = r0 && pv <= 0.01, C001 = r0 && pv <= 0.001)
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
            make = likert(c(0.10, 0.20, 0.30, 0.25, 0.15)))),
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
  list(list(kind = "claims_like", par = NA, bound = NA, score = FALSE,
            make = function(n) illumex::ilm_sim(n_id = ceiling(n / 6), n_period = 6)$claims[seq_len(n)] + 0),
       list(kind = "likert_top", par = NA, bound = NA, score = FALSE,
            make = likert(c(0.02, 0.03, 0.10, 0.25, 0.60)))))
PANEL <- expand.grid(k = c(30, 100), m = c(3, 6, 10))

## one data set through both gates and every test, at both bounds
assess <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  g <- illumex:::ilm_gauss_assess(x)
  u <- sort(unique(x)); nu <- length(u)
  discrete <- all(abs(x - round(x)) < 1e-8) && nu <= max(20L, n / 50)
  reach <- c(G0 = is.finite(g$gauss) && g$gauss < 0.95 && !discrete && nu > 20L,
             G1 = nu >= 5L)
  cnt <- tabulate(match(x, u), nu)
  lo <- if (nu >= 2L) test_fires(cnt[1], cnt[2], n) else stats::setNames(rep(FALSE, 4), TESTS)
  hi <- if (nu >= 2L) test_fires(cnt[nu], cnt[nu - 1L], n) else stats::setNames(rep(FALSE, 4), TESTS)
  at <- function(f) stats::setNames(as.list(as.vector(outer(reach, f, `&`))), ARMS)
  c(list(n = n, nu = nu, reach_G0 = reach[["G0"]], reach_G1 = reach[["G1"]], kind_now = g$kind,
         c_min = cnt[1], c_next_min = if (nu >= 2L) cnt[2] else NA,
         c_max = cnt[nu], c_next_max = if (nu >= 2L) cnt[nu - 1L] else NA),
    stats::setNames(at(lo), paste0("min_", ARMS)), stats::setNames(at(hi), paste0("max_", ARMS)))
}

run_cells <- function(sizes, reps, out, panel = TRUE, stop_at = Inf) {
  rows <- list(); t0 <- proc.time()[["elapsed"]]
  for (kd in KINDS) for (n in sizes) for (r in reps) {
    if (proc.time()[["elapsed"]] - t0 > stop_at)
      stop(sprintf("stopped at the stop point, %.0f s, at %s n = %d", stop_at, kd$kind, n), call. = FALSE)
    set.seed(r)
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
  rate_tab <- function(d, f) {
    tb <- do.call(rbind, lapply(split(d, list(d$label, d$size), drop = TRUE), function(s)
      data.frame(data = s$label[1], n = s$size[1], G0 = mean(s$reach_G0), G1 = mean(s$reach_G1),
                 t(vapply(ARMS, function(a) mean(f(s, a)), 0)), check.names = FALSE)))
    tb[order(tb$data, tb$n), ]
  }
  out <- c("# Floor rule study 2: summary", "",
           "Columns G0 and G1: share of replicates each gate lets through. Arm columns: share of replicates where the arm fires.", "",
           "## False floors: the arm fires at either bound", "")
  null <- res[is.na(res$bound) & res$scored & res$kind != "panel_id", ]
  ft <- rate_tab(null, either)
  out <- c(out, fmt(num(ft)), "", "## Panel identifiers (k ids, m rows each)", "")
  pid <- res[res$kind == "panel_id", ]
  pt <- data.frame(k = pid$par %/% 1000, m = pid$par %% 1000,
                   t(vapply(seq_len(nrow(pid)), function(i) vapply(ARMS, function(a) either(pid[i, ], a), TRUE),
                            logical(length(ARMS)))), check.names = FALSE)
  names(pt)[-(1:2)] <- ARMS
  out <- c(out, fmt(pt), "", "## Detection over all replicates: the arm fires at the right bound", "")
  tru <- res[!is.na(res$bound), ]
  dt <- rate_tab(tru, right)
  dt$scored <- vapply(seq_len(nrow(dt)), function(i) {
    s <- tru[tru$label == dt$data[i], ][1, ]; s$scored && dt$n[i] >= 300 && s$par >= 0.05 }, TRUE)
  out <- c(out, fmt(num(dt)), "", "## Reported, not scored: either bound", "")
  rep_ <- res[is.na(res$bound) & !res$scored, ]
  rt <- rate_tab(rep_, either)
  out <- c(out, fmt(num(rt)), "", "## Against the criteria", "")
  for (a in ARMS) {
    fp <- max(ft[[a]]); pid_hit <- any(pt[[a]])
    sc <- dt[dt$scored, ]; det <- min(sc[[a]])
    d2 <- dt[grepl(" 0.02$", dt$data) & dt$n >= 300, ]
    ok <- fp <= 0.05 && !pid_hit && det >= 0.90
    worst_fp <- ft[which.max(ft[[a]]), ]; worst_det <- sc[which.min(sc[[a]]), ]
    out <- c(out, sprintf("- %s: worst false-floor rate %.3f (%s, n = %d); fires on a panel id: %s; worst scored detection %.3f (%s, n = %d); mean detection at 2%%, n >= 300: %.3f. %s",
      a, fp, worst_fp$data, worst_fp$n, if (pid_hit) "yes" else "no", det, worst_det$data, worst_det$n,
      mean(d2[[a]]), if (ok) "Meets both criteria." else "Misses."))
  }
  writeLines(out, args[3])
  cat(out, sep = "\n")
}
