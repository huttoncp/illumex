## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## How illumex's functions scale in rows: a measurement before the first
## release, so the documentation and the census-geography article say what
## runs at what size. Measurement only; nothing is changed here, and work on
## scale beyond it is decided separately (item 188).
##
## Design, fixed before any run (approved 2026-09-28):
##   Data: ilm_sim(n_id = n / 10, n_period = 10) -- 13 mixed columns (numbers,
##   categories, a date, logicals, missing values) -- at n = 10,000, 50,000,
##   100,000 and 250,000 rows, seed 1. The profile columns are score, income,
##   visits, claims and grp.
##   Functions, each at its defaults but for what the call needs (a column,
##   progress = FALSE); the arguments are in CELLS below. Where a function
##   needs another's result (a reduction, a clustering), that is made before
##   the clock starts. ilm_var_contrib() is given a clustering from
##   stats::kmeans() on the reduction, wrapped as an ilm_cluster result, since
##   ilm_cluster() itself is one of the functions measured.
##   Each (function, n) cell runs in a fresh R process, one core
##   (OMP_NUM_THREADS = 1), with illumex from a pinned library
##   (lib_scale, main ff5e579 installed). Recorded: elapsed
##   seconds; R's maximum heap during the call (gc()'s "max used", reset just
##   before it, so it includes the data already built, reported beside the
##   data's own size) -- R vectors such as a distance matrix count, memory C
##   code allocates for itself does not; and whether the call finished, with
##   its error if not.
##   A smoke run at n = 1,000 before this commit ran every cell; it needed
##   income made missing on every seventh row for ilm_profile_na(), which
##   wants two columns with missing values, and it found the default
##   ilm_cluster() (a gap statistic over 100 reference sets) and
##   ilm_profile() already take about 65 seconds at that size.
##   A cell has 15 minutes. A function that times out or fails at one size is
##   not run at larger ones, and the table says so.
##   Expected before running, from the code: ilm_cluster() computes every
##   row's silhouette from a full distance matrix (R/ilm_cluster.R), n^2 / 2
##   doubles -- 0.4 GB at 10,000 rows, 10 GB at 50,000, 40 GB at 100,000 --
##   so it, ilm_cluster_na(), ilm_profile() and ilm_profile_na() cannot run
##   above about 40,000 rows on this machine (16 GB); method = "hclust" builds
##   the same matrix. The run tests that, and finds anything else.
##
## Run one cell: Rscript dev/studies/scale_check.R <cell> <n> <out.csv>
## (A shell loop over the cells runs them all, in order. Two cells that
## timed an export feature since removed are left out.)
## Summarise: Rscript dev/studies/scale_check.R summarise <out.csv>
args <- commandArgs(TRUE)
options(width = 200)

CELLS <- list(
  describe_all     = function(d, x) ilm_describe_all(d),
  describe_na_all  = function(d, x) ilm_describe_na_all(d),
  counts_all       = function(d, x) ilm_counts_all(d),
  frame_issues     = function(d, x) ilm_frame_issues(d),
  dupes            = function(d, x) ilm_dupes(d),
  outliers_all     = function(d, x) ilm_outliers_all(d),
  check_missing    = function(d, x) ilm_check_missing(d, y = "score", verbose = FALSE),
  wash_df          = function(d, x) ilm_wash_df(d),
  recode_errors    = function(d, x) ilm_recode_errors(d, c("", "North ")),
  gauss_check      = function(d, x) ilm_gauss_check(d$income),
  boot_ci          = function(d, x) ilm_boot_ci(d, "score", seed = 1, progress = FALSE),
  anomaly          = function(d, x) ilm_anomaly(d, progress = FALSE),
  anomaly_iforest  = function(d, x) ilm_anomaly(d, method = "iforest", progress = FALSE),
  reduce           = function(d, x) ilm_reduce(x$dd, progress = FALSE),
  reduce_glrm      = function(d, x) ilm_reduce(x$dd, method = "glrm", progress = FALSE),
  cluster          = function(d, x) ilm_cluster(x$r, progress = FALSE),
  cluster_k3       = function(d, x) ilm_cluster(x$r, k = 3, progress = FALSE),
  cluster_hclust   = function(d, x) ilm_cluster(x$r, k = 3, method = "hclust", progress = FALSE),
  var_contrib      = function(d, x) ilm_var_contrib(x$km, x$dd),
  profile          = function(d, x) ilm_profile(x$dd, progress = FALSE),
  ## ilm_sim() has one column with missing values, and a pattern needs two:
  ## income is missing on every seventh row
  profile_na       = function(d, x) { d$income[seq(1L, nrow(d), by = 7L)] <- NA
                                      ilm_profile_na(d, progress = FALSE) },
  plot_all         = function(d, x) { grDevices::pdf(NULL); on.exit(grDevices::dev.off()); ilm_plot_all(x$dd) })
## what each cell needs made before its clock starts
NEEDS <- list(reduce = "dd", reduce_glrm = "dd", cluster = "r", cluster_k3 = "r",
              cluster_hclust = "r", var_contrib = "km", profile = "dd",
              plot_all = "dd")

if (length(args) && args[1] == "summarise") {
  res <- utils::read.csv(args[2], stringsAsFactors = FALSE)
  res$result <- ifelse(res$status == "ok",
                       sprintf("%.1f s, %.0f MB", res$seconds, res$max_mb),
                       res$status)
  w <- stats::reshape(res[c("cell", "n", "result")], idvar = "cell", timevar = "n",
                      direction = "wide")
  names(w) <- sub("^result\\.", "n=", names(w))
  w <- w[match(names(CELLS), w$cell), , drop = FALSE]
  w <- w[!is.na(w$cell), ]
  cat("seconds and R's maximum heap during the call, one core; data sizes:",
      paste(unique(sprintf("n=%d %.0f MB", res$n, res$data_mb)), collapse = ", "), "\n")
  print(w, row.names = FALSE)
  quit(save = "no")
}

cell <- args[1]; n <- as.integer(args[2]); out <- args[3]
suppressPackageStartupMessages(library(illumex))
d <- ilm_sim(n_id = n %/% 10L, n_period = 10L, seed = 1L)
x <- list()
need <- NEEDS[[cell]] %||% character(0)
if (length(need)) {
  x$dd <- d[c("score", "income", "visits", "claims", "grp")]
  if (need %in% c("r", "km")) x$r <- suppressMessages(ilm_reduce(x$dd, progress = FALSE))
  if (need == "km") {
    set.seed(1)
    km <- stats::kmeans(x$r$ind_coord %||% x$r$coords %||% x$r[[1]], 3, nstart = 5)
    x$km <- structure(list(ind_cluster = data.frame(cluster = km$cluster), time = NULL),
                      class = "ilm_cluster")
  }
}
data_mb <- as.numeric(utils::object.size(d)) / 2^20
invisible(gc(reset = TRUE))
t0 <- proc.time()[["elapsed"]]
err <- tryCatch({ suppressWarnings(suppressMessages(CELLS[[cell]](d, x))); "ok" },
                error = function(e) paste("error:", gsub("[\r\n,]+", " ", conditionMessage(e))))
secs <- proc.time()[["elapsed"]] - t0
g <- gc()
max_mb <- sum(g[, ncol(g)])
row <- data.frame(cell = cell, n = n, status = substr(err, 1, 160), seconds = round(secs, 2),
                  max_mb = round(max_mb, 1), data_mb = round(data_mb, 1),
                  illumex = format(utils::packageVersion("illumex")),
                  when = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
utils::write.table(row, out, sep = ",", append = file.exists(out), col.names = !file.exists(out),
                   row.names = FALSE)
cat(sprintf("%-16s n=%-7d %-10s %8.1f s %8.0f MB\n", cell, n, substr(err, 1, 10), secs, max_mb))
