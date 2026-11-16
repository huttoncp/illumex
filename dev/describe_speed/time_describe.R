## One timing of the describe functions, for comparing two versions of the
## package: run it once per version and repeat, alternating, so that drift
## falls on both alike (dev/describe_speed/time_runs.sh does that).
##
##   Rscript dev/describe_speed/time_describe.R <package tree> <label> <rows> <out.csv>
##
## Made-up data: 20 described columns (8 numeric, 4 counts, 4 factors, 2
## logical, a date and a date-time), some missing values, and a two-level
## and a three-level grouping variable. Each setting below is timed once,
## after one warm-up, as elapsed seconds; memory is what R allocated during
## the call (bench::bench_memory), not its peak. One row per setting is
## appended to <out.csv>.
args <- commandArgs(TRUE)
tree <- args[1]; label <- args[2]; n <- as.integer(args[3]); out <- args[4]
suppressMessages(pkgload::load_all(tree, quiet = TRUE))
set.seed(4402)
d <- data.frame(g = sample(c("a", "b"), n, TRUE), h = sample(c("x", "y", "z"), n, TRUE))
for (k in 1:8) d[[paste0("num", k)]] <- round(stats::rnorm(n, 50, 10), 2)
for (k in 1:4) d[[paste0("cnt", k)]] <- stats::rpois(n, 3)
for (k in 1:4) d[[paste0("cat", k)]] <- factor(sample(letters[1:6], n, TRUE))
for (k in 1:2) d[[paste0("lgl", k)]] <- stats::runif(n) < 0.3
d$day <- as.Date("2020-01-01") + sample(0:999, n, TRUE)
d$when <- as.POSIXct("2020-01-01", tz = "UTC") + sample(0:1e7, n, TRUE)
for (v in c("num1", "cnt1", "cat1")) d[[v]][sample(n, n %/% 20)] <- NA
settings <- list(
  all_by1       = function() ilm_describe_all(d[setdiff(names(d), "h")], by = "g"),
  all_by2       = function() ilm_describe_all(d, by = c("g", "h")),
  all_by1_none  = function() ilm_describe_all(d[setdiff(names(d), "h")], by = "g",
                                              gauss = "none", dispersion = FALSE),
  all_noby      = function() ilm_describe_all(d[setdiff(names(d), c("g", "h"))]),
  all_noby_none = function() ilm_describe_all(d[setdiff(names(d), c("g", "h"))],
                                              gauss = "none", dispersion = FALSE),
  na_all_by1    = function() ilm_describe_na_all(d[setdiff(names(d), "h")], by = "g"),
  na_all_by2    = function() ilm_describe_na_all(d, by = c("g", "h")))
rows <- lapply(names(settings), function(s) {
  f <- function() suppressMessages(settings[[s]]())
  invisible(f())
  gc(FALSE)
  m <- as.numeric(bench::bench_memory(f())$mem_alloc)
  t <- as.numeric(bench::bench_time(f())[["real"]])
  data.frame(label = label, setting = s, rows = n, seconds = t, mb = m / 2^20)
})
res <- do.call(rbind, rows)
utils::write.table(res, out, sep = ",", row.names = FALSE, col.names = !file.exists(out),
                   append = file.exists(out))
