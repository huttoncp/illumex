## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## When is a pile at the minimum a floor? A pre-registration, then the study.
##
## Why: ilm_describe() names a floor when at least 2% of a variable's values,
## and at least 5 rows, sit exactly at its minimum (a ceiling the same at its
## maximum), once the variable has more than 20 distinct values and is not
## called discrete. It never compares the stack at the bound with the values
## beside it. On 100 columns of evenly spread integers over 21 to 50 values
## (300 rows) it named a floor or a ceiling in 96: with so few values every
## value, the minimum included, clears 2%. A panel identifier (30 ids, 6 rows
## each) is called a floor at 1 and a ceiling at 30. Measured 30 September
## 2026 before this registration; the study replaces those figures.
##
## Scope: only the test at the bound changes between the rules. The gates
## before it stay as they are -- the score must fall below 0.95, the variable
## must not be discrete (whole numbers with at most max(20, n / 50) distinct
## values), and it must have more than 20 distinct values -- and are
## recorded, so a floor the gates never let the test see is reported as
## such, not as the rule's miss.
##
## Design, fixed before any run:
##   Rules, each applied to the minimum (a floor) and the maximum (a ceiling);
##   p is the share of values at the bound, p_next the share at the next
##   distinct value inward, nu the number of distinct values:
##     R0      p >= 0.02 and n * p >= 5 (now)
##     A2, A3, A5   R0 and p >= r * p_next, r = 2, 3, 5  (stands out from its
##                  neighbour)
##     B2, B3, B5   R0 and p >= r / nu, r = 2, 3, 5  (stands out from the
##                  average share of a distinct value)
##   Data. Sizes n = 100, 300, 1,000 and 5,000; 200 replicates each, data
##   seed = replicate number, so every rule sees the same data.
##   With no floor or ceiling (scored for false floors):
##     unif_int_k      integers spread evenly over k = 21, 30, 50, 100 values
##     round_norm_0    round(N(50, 10)), whole numbers
##     round_norm_1    round(N(50, 10), 1), one decimal
##     pois30          Poisson counts, mean 30
##     nb_decreasing   negative binomial counts, size 1, mean 8: the pmf falls
##                     from zero (P(0) = 0.111, P(1) = 0.099), so zero is the
##                     support's edge, not a pile; ilm_censor() would be
##                     wrong for it
##     normal          N(0, 1), no ties (a check)
##     panel_id        1 to k, each m times, k = 30 and 100, m = 3, 6 and 10
##                     (n = k * m; the data are fixed, so one replicate each)
##   With a floor or a ceiling (scored for detection), share s at the bound
##   = 0.02, 0.05, 0.10 and 0.20:
##     cens_floor      N(0, 1) censored below at its s quantile
##     cens_ceiling    N(0, 1) censored above at its 1 - s quantile
##     lod_floor       lognormal(0, 1) censored below at its s quantile (a
##                     detection limit)
##     vas_floor       a 0-100 rating: round(N(45, 20)) held to 0-100, then a
##                     share s of rows set to 0 (the pile is s plus the
##                     1.2% the bound itself adds)
##     zi_count        zero-inflated counts: 0 with probability s, otherwise
##                     negative binomial size 3, mean 15 (P(0) = 0.005);
##                     s = 0.05, 0.10, 0.20 (not 0.02)
##   Reported, not scored:
##     rating_0_10     a 0-10 rating, round(N(5, 2)) held to 0-10, share s
##                     set to 0: 11 values, so called discrete and never
##                     tested under any rule. Shown to say what the gates
##                     leave unseen.
##     claims_like     ilm_sim()'s claims (negative binomial, size 1.2, mean
##                     varying by row): its zero is the distribution's own
##                     mode, about 1.5 times the share at 1. Whether a
##                     count's own zero mode should be called a floor is the
##                     question to be decided, so it is shown under every rule,
##                     not scored.
##   Recorded for every data set: n, nu, whether the gates let the test run,
##   the kind ilm_gauss_assess() gives now, p and p_next at each bound, and
##   whether each rule fires at each bound.
##
##   Criteria, fixed now:
##     False floors: on each kind with no floor, at each n, a rule fires at
##       either bound in at most 5% of replicates; on panel_id, never.
##     Detection: on each kind with a floor or ceiling, at n >= 300 and
##       s >= 0.05, of the replicates the gates let through, a rule fires at
##       the right bound in at least 90%. s = 0.02 and n = 100 are reported,
##       not scored (at n = 100, 2% is 2 rows, below the 5-row minimum).
##     A rule that meets both is a candidate. Among candidates, the note
##     recommends the one with the lowest worst-case false-floor rate, then
##     the highest detection at s = 0.02 and n >= 300. If none meets both,
##     the note says which criterion each misses and by how much. Nothing
##     about the rule changes until it is decided.
##   Code: illumex from a pinned library (RemoteSha c3f3e93, whose
##   ilm_gauss_assess() is the same as main's at bdc6262), through its
##   internals, so the gates are the package's own.
##
##   The pilot, before the estimate below stands: 20 replicates of every
##   kind at n = 5,000, timed; the full estimate is rebuilt from it with cost
##   in proportion to n (sizes sum to 1.28 times 5,000) and 200 replicates.
##   Estimate before the pilot (untested): under 15 minutes on one process.
##   Stop point: at twice the pilot-based estimate the run pauses and reports.
##   Results: dev/studies/results/floor_rule.csv, and the summary beside it.
##   Addendum, 30 September, before the rerun: the full run stopped at 2.8
##   minutes, twice the pilot's estimate, because cost is mostly fixed per call
##   rather than in proportion to n (20 replicates of every kind: 3.0 s at 100
##   rows, 3.8 s at 300, 4.1 s at 1,000, 6.6 s at 5,000); rebuilt per size the
##   estimate is 2.9 minutes, and the stop point 5.8. Nothing else changes.
##
## Pilot:      Rscript dev/studies/floor_rule.R pilot <lib> <out.csv>
## Full run:   Rscript dev/studies/floor_rule.R run <lib> <out.csv>
## Summarise:  Rscript dev/studies/floor_rule.R summarise <out.csv> <summary.md>

args <- commandArgs(TRUE)
mode <- args[1]

RULES <- c("R0", "A2", "A3", "A5", "B2", "B3", "B5")
fires <- function(p, p_next, nu, n) {
  r0 <- p >= 0.02 && n * p >= 5
  c(R0 = r0,
    A2 = r0 && p >= 2 * p_next, A3 = r0 && p >= 3 * p_next, A5 = r0 && p >= 5 * p_next,
    B2 = r0 && p >= 2 / nu, B3 = r0 && p >= 3 / nu, B5 = r0 && p >= 5 / nu)
}

## the data sets: kind, parameter, the bound a floor sits at (NA for none),
## whether it is scored, and a maker of n values from a seed
held <- function(x, lo, hi) pmin(pmax(x, lo), hi)
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
            make = function(n) stats::rnorm(n))),
  unlist(lapply(c(0.02, 0.05, 0.10, 0.20), function(s) list(
    list(kind = "cens_floor", par = s, bound = "min", score = TRUE,
         make = function(n) pmax(stats::rnorm(n), stats::qnorm(s))),
    list(kind = "cens_ceiling", par = s, bound = "max", score = TRUE,
         make = function(n) pmin(stats::rnorm(n), stats::qnorm(1 - s))),
    list(kind = "lod_floor", par = s, bound = "min", score = TRUE,
         make = function(n) pmax(stats::rlnorm(n), stats::qlnorm(s))),
    list(kind = "vas_floor", par = s, bound = "min", score = TRUE,
         make = function(n) { x <- held(round(stats::rnorm(n, 45, 20)), 0, 100)
                              x[stats::runif(n) < s] <- 0; x }),
    list(kind = "rating_0_10", par = s, bound = "min", score = FALSE,
         make = function(n) { x <- held(round(stats::rnorm(n, 5, 2)), 0, 10)
                              x[stats::runif(n) < s] <- 0; x }))), recursive = FALSE),
  lapply(c(0.05, 0.10, 0.20), function(s) list(kind = "zi_count", par = s, bound = "min", score = TRUE,
    make = function(n) ifelse(stats::runif(n) < s, 0, stats::rnbinom(n, size = 3, mu = 15) + 0))),
  list(list(kind = "claims_like", par = NA, bound = NA, score = FALSE,
            make = function(n) illumex::ilm_sim(n_id = ceiling(n / 6), n_period = 6)$claims[seq_len(n)] + 0)))
PANEL <- expand.grid(k = c(30, 100), m = c(3, 6, 10))

## one data set through the package's gates and every rule, at both bounds
assess <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  g <- illumex:::ilm_gauss_assess(x)
  u <- sort(unique(x)); nu <- length(u)
  discrete <- all(abs(x - round(x)) < 1e-8) && nu <= max(20L, n / 50)
  reached <- is.finite(g$gauss) && g$gauss < 0.95 && !discrete && nu > 20L
  tb <- tabulate(match(x, u), nu) / n
  f_min <- fires(tb[1], tb[2], nu, n); f_max <- fires(tb[nu], tb[nu - 1L], nu, n)
  c(list(n = n, nu = nu, reached = reached, kind_now = g$kind,
         p_min = tb[1], p_next_min = tb[2], p_max = tb[nu], p_next_max = tb[nu - 1L]),
    as.list(stats::setNames(reached & f_min, paste0("min_", RULES))),
    as.list(stats::setNames(reached & f_max, paste0("max_", RULES))))
}

run_cells <- function(sizes, reps, out) {
  rows <- list()
  for (kd in KINDS) for (n in sizes) for (r in reps) {
    set.seed(r)
    a <- assess(kd$make(n))
    rows[[length(rows) + 1L]] <- data.frame(kind = kd$kind, par = kd$par, bound = kd$bound,
      scored = kd$score, size = n, rep = r, a, stringsAsFactors = FALSE)
  }
  if (identical(sizes, c(100, 300, 1000, 5000)))
    for (i in seq_len(nrow(PANEL))) {
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
  t0 <- proc.time()[["elapsed"]]
  if (mode == "pilot") {
    res <- run_cells(5000, 1:20, args[3])
    el <- proc.time()[["elapsed"]] - t0
    est <- el / 20 * 200 * 1.28
    cat(sprintf("pilot: %d data sets at n = 5,000 in %.1f s; full-run estimate %.1f min, stop point %.1f min\n",
                nrow(res), el, est / 60, 2 * est / 60))
  } else {
    res <- run_cells(c(100, 300, 1000, 5000), 1:200, args[3])
    cat(sprintf("run: %d data sets in %.1f min\n", nrow(res),
                (proc.time()[["elapsed"]] - t0) / 60))
  }
}

if (mode == "summarise") {
  res <- utils::read.csv(args[2], stringsAsFactors = FALSE)
  lab <- function(d) ifelse(is.na(d$par), d$kind, paste0(d$kind, " ", d$par))
  res$label <- lab(res)
  either <- function(d, rule) d[[paste0("min_", rule)]] | d[[paste0("max_", rule)]]
  right <- function(d, rule) ifelse(d$bound == "min", d[[paste0("min_", rule)]], d[[paste0("max_", rule)]])
  out <- c("# Floor rule study: summary", "",
           "## False floors: share of replicates where the rule fires at either bound", "")
  null <- res[is.na(res$bound) & res$scored & res$kind != "panel_id", ]
  tab <- do.call(rbind, lapply(split(null, list(null$label, null$size), drop = TRUE), function(d)
    data.frame(data = d$label[1], n = d$size[1], reached = mean(d$reached),
               t(vapply(RULES, function(r) mean(either(d, r)), 0)), check.names = FALSE)))
  tab <- tab[order(tab$data, tab$n), ]
  fmt <- function(t) c(paste("|", paste(names(t), collapse = " | "), "|"),
                       paste("|", paste(rep("---", ncol(t)), collapse = " | "), "|"),
                       apply(t, 1, function(r) paste("|", paste(r, collapse = " | "), "|")))
  num <- function(t) { for (j in seq_along(t)) if (is.numeric(t[[j]]) && names(t)[j] != "n")
                         t[[j]] <- sprintf("%.3f", t[[j]]); t }
  out <- c(out, fmt(num(tab)), "", "## Panel identifiers (k ids, m rows each): does the rule fire?", "")
  pid <- res[res$kind == "panel_id", ]
  pt <- data.frame(k = pid$par %/% 1000, m = pid$par %% 1000, reached = pid$reached,
                   t(vapply(seq_len(nrow(pid)), function(i) vapply(RULES, function(r)
                     either(pid[i, ], r), TRUE), logical(length(RULES)))), check.names = FALSE)
  names(pt)[-(1:3)] <- RULES
  out <- c(out, fmt(pt), "", "## Detection: share of replicates the gates let through where the rule fires at the right bound", "")
  tru <- res[!is.na(res$bound), ]
  dt <- do.call(rbind, lapply(split(tru, list(tru$label, tru$size), drop = TRUE), function(d) {
    k <- d$reached
    data.frame(data = d$label[1], n = d$size[1], scored = d$scored[1] && d$size[1] >= 300 && d$par[1] >= 0.05,
               reached = mean(k),
               t(vapply(RULES, function(r) if (any(k)) mean(right(d[k, ], r)) else NA_real_, 0)),
               check.names = FALSE)
  }))
  dt <- dt[order(dt$data, dt$n), ]
  out <- c(out, fmt(num(dt)), "", "## Claims-like counts (reported, not scored)", "")
  cl <- res[res$kind == "claims_like", ]
  ct <- do.call(rbind, lapply(split(cl, cl$size), function(d)
    data.frame(n = d$size[1], reached = mean(d$reached), p_zero = mean(d$p_min),
               p_one = mean(d$p_next_min), t(vapply(RULES, function(r) mean(d[[paste0("min_", r)]]), 0)),
               check.names = FALSE)))
  out <- c(out, fmt(num(ct)), "", "## Against the criteria", "")
  for (r in RULES) {
    fp <- max(tab[[r]]); pid_hit <- any(pt[[r]])
    sc <- dt[dt$scored, ]; det <- min(sc[[r]], na.rm = TRUE)
    d2 <- dt[grepl(" 0.02$", dt$data) & dt$n >= 300 & !grepl("^rating", dt$data), ]
    ok <- fp <= 0.05 && !pid_hit && det >= 0.90
    out <- c(out, sprintf("- %s: worst false-floor rate %.3f (%s), fires on a panel id: %s; worst scored detection %.3f (%s); mean detection at 2%%, n >= 300: %.3f. %s",
      r, fp, tab$data[which.max(tab[[r]])], if (pid_hit) "yes" else "no", det,
      sc$data[which.min(sc[[r]])], mean(d2[[r]], na.rm = TRUE),
      if (ok) "Meets both criteria." else "Misses."))
  }
  writeLines(out, args[3])
  cat(out, sep = "\n")
}
