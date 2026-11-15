## Request 46's three-way timing: ilm_describe_all() at its defaults, with
## gauss = "none" and dispersion = FALSE, and with fast = TRUE, on one data
## set and one version of the package, the three alternating within each
## repeat so that drift falls on all alike.
##
##   Rscript dev/describe_speed/time_fast.R <package tree> <rows> <reps> <out.csv>
##
## The data are time_describe.R's: 20 described columns (8 numeric, 4
## counts, 4 factors, 2 logical, a date and a date-time) and a two-level
## grouping variable. Time is elapsed seconds; memory is what R allocated
## during the call (bench::bench_memory), not its peak.
args <- commandArgs(TRUE)
tree <- args[1]; n <- as.integer(args[2]); reps <- as.integer(args[3]); out <- args[4]
suppressMessages(pkgload::load_all(tree, quiet = TRUE))
set.seed(4402)
d <- data.frame(g = sample(c("a", "b"), n, TRUE))
for (k in 1:8) d[[paste0("num", k)]] <- round(stats::rnorm(n, 50, 10), 2)
for (k in 1:4) d[[paste0("cnt", k)]] <- stats::rpois(n, 3)
for (k in 1:4) d[[paste0("cat", k)]] <- factor(sample(letters[1:6], n, TRUE))
for (k in 1:2) d[[paste0("lgl", k)]] <- stats::runif(n) < 0.3
d$day <- as.Date("2020-01-01") + sample(0:999, n, TRUE)
d$when <- as.POSIXct("2020-01-01", tz = "UTC") + sample(0:1e7, n, TRUE)
for (v in c("num1", "cnt1", "cat1")) d[[v]][sample(n, n %/% 20)] <- NA
settings <- list(
  default  = list(),
  switches = list(gauss = "none", dispersion = FALSE),
  fast     = list(fast = TRUE))
rows <- list()
for (by in list(NULL, "g")) {
  dd <- if (is.null(by)) d[setdiff(names(d), "g")] else d
  for (s in names(settings)) invisible(suppressMessages(
    do.call(ilm_describe_all, c(list(dd, by = by), settings[[s]]))))       # warm up
  for (i in seq_len(reps)) for (s in names(settings)) {
    f <- function() suppressMessages(do.call(ilm_describe_all, c(list(dd, by = by), settings[[s]])))
    gc(FALSE)
    m <- as.numeric(bench::bench_memory(f())$mem_alloc)
    t <- as.numeric(bench::bench_time(f())[["real"]])
    rows[[length(rows) + 1L]] <- data.frame(by = if (is.null(by)) "none" else by, setting = s,
                                            rep = i, rows = n, seconds = t, mb = m / 2^20)
  }
}
res <- do.call(rbind, rows)
utils::write.csv(res, out, row.names = FALSE)
cat(sprintf("rows %d, %d repeats of each setting\n", n, reps))
for (b in unique(res$by)) {
  base <- res[res$by == b & res$setting == "default", ]
  for (s in names(settings)) {
    r <- res[res$by == b & res$setting == s, ]
    cat(sprintf("by %-4s %-8s median %6.3f s %8.1f MB | against the default: time %+6.1f%%  memory %+6.1f%%\n",
                b, s, median(r$seconds), median(r$mb),
                100 * (median(r$seconds) / median(base$seconds) - 1),
                100 * (median(r$mb) / median(base$mb) - 1)))
  }
}
