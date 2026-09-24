## How a date should enter a clustering, measured.
##
## Two known clusters of 200 rows each. x1 and x2 separate them by `delta`
## standard deviations; the date's relation to the clusters is the scenario.
## Every encoding goes through the same reduction (PCAmix, at most five
## dimensions) and k-means with k = 2, as ilm_reduce() and ilm_cluster() do,
## and is scored by the adjusted Rand index against the truth, averaged over
## the replicates. The numbers quoted in R/ilm_time.R and NEWS.md come from
## this script.
##
## Run from the package root:
##   Rscript dev/studies/time_encoding.R [reps] [outfile.csv]

pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
args <- commandArgs(TRUE)
R <- if (length(args) >= 1L) as.integer(args[1]) else 20L
out_csv <- if (length(args) >= 2L) args[2] else NULL

ari <- function(a, b) {
  tab <- table(a, b); n <- sum(tab)
  s <- sum(choose(tab, 2)); sa <- sum(choose(rowSums(tab), 2))
  sb <- sum(choose(colSums(tab), 2)); e <- sa * sb / choose(n, 2)
  (s - e) / ((sa + sb) / 2 - e)
}

## three years of days, and the calendar facts the scenarios are made of
cal <- as.Date("2022-01-01") + 0:(3 * 365 - 1)
clt <- as.POSIXlt(cal)
iso_wday <- ifelse(clt$wday == 0, 7, clt$wday)
mdays <- ilm_month_days(clt)
last3 <- clt$mday > mdays - 3
turn <- clt$mday > mdays - 2 | clt$mday <= 2

## The scenarios. In each, cluster 1 is the first half of the rows.
##   none          the date is unrelated to anything
##   mild          busier on weekdays and at month-end, the same for both
##                 clusters: a rhythm in WHEN rows occur that nothing follows
##   none_many     `none`, with eight more columns that follow nothing
##   era           cluster 1 in the first two years, cluster 2 in the last two
##   season        cluster 1 in November-February, cluster 2 in May-August
##   weekend       cluster 1 on weekends, cluster 2 on weekdays
##   weekend_even  dates spread evenly; the weekend rows are cluster 1 -- a
##                 rhythm in the values with none in the volume
##   monthend      cluster 1 in the last three days of the month
##   monthend_many `monthend`, with eight more columns that follow nothing
##   turnofmonth   cluster 1 in the last two and first two days of the month
##   none_ts       `none` with timestamps, so the time of day is there too
##   night         timestamps; cluster 1 between 22:00 and 04:00, cluster 2
##                 between 08:00 and 18:00
gen <- function(scen, delta, seed, n = 400) {
  set.seed(seed)
  cl <- rep(1:2, each = n / 2)
  pick <- function(ok, m) cal[ok][sample.int(sum(ok), m, replace = TRUE)]
  all <- rep(TRUE, length(cal))
  when <- switch(scen,
    none = , none_many = , none_ts = , night = pick(all, n),
    mild = cal[sample.int(length(cal), n, replace = TRUE,
                          prob = ifelse(iso_wday >= 6, 0.6, 1) * ifelse(last3, 1.5, 1))],
    era = c(pick(seq_along(cal) <= 2 * 365, n / 2), pick(seq_along(cal) > 365, n / 2)),
    season = c(pick((clt$mon + 1) %in% c(11, 12, 1, 2), n / 2),
               pick((clt$mon + 1) %in% 5:8, n / 2)),
    weekend = c(pick(iso_wday >= 6, n / 2), pick(iso_wday <= 5, n / 2)),
    weekend_even = pick(all, n),
    monthend = , monthend_many = c(pick(last3, n / 2), pick(!last3, n / 2)),
    turnofmonth = c(pick(turn, n / 2), pick(!turn, n / 2)))
  if (scen == "weekend_even")
    cl <- ifelse(as.POSIXlt(when)$wday %in% c(0, 6), 1L, 2L)
  if (scen %in% c("none_ts", "night")) {
    hr <- if (scen == "night") c((22 + runif(n / 2, 0, 6)) %% 24, runif(n / 2, 8, 18))
          else runif(n, 0, 24)
    when <- as.POSIXct(when, tz = "UTC") + hr * 3600
  }
  d <- data.frame(x1 = rnorm(n, delta * (cl == 2)), x2 = rnorm(n, delta * (cl == 2)))
  if (grepl("_many$", scen)) for (j in 1:8) d[[paste0("n", j)]] <- rnorm(n)
  list(X = d, when = when, cl = cl)
}

## Cycles whose positions are lumpy against the calendar between the first
## and last date: the gate this package does NOT use, measured for contrast.
lumpy <- function(t, cand) {
  lt <- as.POSIXlt(t)
  days <- seq(as.Date(min(t)), as.Date(max(t)), by = "day"); dl <- as.POSIXlt(days)
  keep <- character(0)
  for (a in cand) {
    lv <- sort(unique(c(ilm_time_bins(dl, a), ilm_time_bins(lt, a))))
    o <- table(factor(ilm_time_bins(lt, a), levels = lv))
    e <- if (a == "hour") rep(1 / 24, length(o))
         else as.numeric(table(factor(ilm_time_bins(dl, a), levels = lv))) / length(days)
    ok <- e > 0
    if (sum(ok) < 2) next
    chi <- sum((as.numeric(o)[ok] - sum(o) * e[ok])^2 / (sum(o) * e[ok]))
    if (stats::pchisq(chi, sum(ok) - 1, lower.tail = FALSE) < 1e-3 &&
        sqrt(chi / sum(o)) >= 0.1) keep <- c(keep, a)
  }
  keep
}

## The encodings.
##   drop         the date left out
##   elapsed      days since the earliest date: R's own number for it
##   parts        year, month, day of the month, weekday (Mon 1 .. Sun 7),
##                the hour for timestamps, and the date as a number
##   parts_gated  the date as a number, plus the plain number of each part
##                whose cycle the package's test lets in (no year)
##   cycles_all   every cycle the data cover, as sine-cosine pairs, untested
##   cycles_lumpy the cycles whose positions are lumpy against the calendar
##   cycles       what ilm_reduce(time = "cycles") does: the cycles some
##                other column varies with
encode <- function(g, how) {
  X <- g$X; t <- g$when
  if (how == "drop") return(X)
  if (how %in% c("elapsed", "cycles"))
    return(suppressMessages(ilm_time_encode(data.frame(X, when = t), how, "study")))
  f <- ilm_time_features(t, "when", cycles = TRUE)
  cand <- unique(sub("_(sin|cos)$", "", substring(
    names(f)[grepl("_(sin|cos)$", names(f))], nchar("when") + 2L)))
  if (how %in% c("cycles_all", "cycles_lumpy")) {
    k <- if (how == "cycles_all") cand else lumpy(t, cand)
    keep <- !grepl("_(sin|cos)$", names(f)) |
      sub("_(sin|cos)$", "", substring(names(f), 6L)) %in% k
    return(data.frame(X, f[keep], check.names = FALSE))
  }
  lt <- as.POSIXlt(t)
  P <- data.frame(year = lt$year + 1900, month = lt$mon + 1, mday = lt$mday,
                  wday = ifelse(lt$wday == 0, 7, lt$wday))
  if (inherits(t, "POSIXct")) P$hour <- lt$hour + lt$min / 60
  P$date <- ilm_time_number(t) / if (inherits(t, "POSIXct")) 86400 else 1
  if (how == "parts_gated") {
    k <- ilm_time_rhythms(t, X, cand)
    P <- P[intersect(names(P), c("date", c(hour = "hour", wday = "wday",
                                           mday = "mday", yday = "month")[k]))]
  }
  data.frame(X, P)
}

clus <- function(X) {
  num <- vapply(X, is.numeric, TRUE)
  X <- X[!num | vapply(X, function(v) !is.numeric(v) || stats::sd(v) > 0, TRUE)]
  fit <- PCAmixdata::PCAmix(X.quanti = X, ndim = max(2L, min(5L, ncol(X))),
                            rename.level = TRUE, graph = FALSE)
  stats::kmeans(fit$ind$coord, 2, nstart = 25)$cluster
}

scens <- c("none", "mild", "none_many", "era", "season", "weekend", "weekend_even",
           "monthend", "monthend_many", "turnofmonth", "none_ts", "night")
hows <- c("drop", "elapsed", "parts", "parts_gated", "cycles_all", "cycles_lumpy", "cycles")
res <- list()
for (delta in c(1.2, 0)) for (sc in scens) for (r in seq_len(R)) {
  g <- gen(sc, delta, seed = 1000 * r + 7)
  for (h in hows) {
    X <- encode(g, h)
    res[[length(res) + 1L]] <- data.frame(
      delta = delta, scenario = sc, rep = r, encoding = h,
      date_cols = ncol(X) - ncol(g$X), ari = ari(clus(X), g$cl))
  }
}
res <- do.call(rbind, res)
if (!is.null(out_csv)) utils::write.csv(res, out_csv, row.names = FALSE)
for (delta in c(1.2, 0)) {
  cat("\n== delta =", delta,
      if (delta == 0) "(only the date carries the clusters)" else "(x1, x2 carry them too)", "\n")
  s <- res[res$delta == delta, ]
  print(round(tapply(s$ari, list(s$scenario, s$encoding), mean)[scens, hows], 2))
}
cat("\nlargest standard error of a mean above:",
    round(max(tapply(res$ari, list(res$delta, res$scenario, res$encoding),
                     function(a) stats::sd(a) / sqrt(length(a)))), 3), "\n")
cat("\ncolumns the date became (most in any replicate), delta = 1.2:\n")
s <- res[res$delta == 1.2, ]
print(tapply(s$date_cols, list(s$scenario, s$encoding), max)[scens, hows])
