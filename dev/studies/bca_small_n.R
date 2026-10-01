## Coverage of illumex's BCa interval (95%) by n, exponential(1) data, for the
## mean (true 1) and the median (true log 2). 400 samples per cell, R = 1000.
## For choosing the row count below which BCa is flagged (ILM_BCA_MIN_N).
## Rscript dev/studies/bca_small_n.R .   Results: results/bca_small_n.txt
tools::psnice(value = 19L)
suppressMessages(pkgload::load_all(commandArgs(TRUE)[1], quiet = TRUE))
set.seed(2029)
for (st in c("mean", "median")) {
  f <- ilm_stat_fun(st); truth <- if (st == "mean") 1 else log(2)
  for (n in c(3, 5, 8, 10, 15, 20, 30)) {
    hit <- replicate(400, { y <- rexp(n)
      r <- suppressWarnings(ilm_boot_stat(y, f, 1000, 0.95, "bca", st, progress = FALSE))
      r$lower <= truth && truth <= r$upper })
    pc <- replicate(400, { y <- rexp(n)
      r <- ilm_boot_stat(y, f, 1000, 0.95, "percentile", st, progress = FALSE)
      r$lower <= truth && truth <= r$upper })
    cat(sprintf("%-6s n=%2d  BCa %.3f  percentile %.3f  (MC se %.3f)\n", st, n, mean(hit, na.rm = TRUE),
                mean(pc, na.rm = TRUE), sqrt(0.95 * 0.05 / 400)))
  }
}
