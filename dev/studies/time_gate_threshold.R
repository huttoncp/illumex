## Where to set the bar for letting a date's cycle into a reduction.
##
## ilm_time_rhythms() lets a cycle in when some other column's separation
## across the cycle's positions stands well above what shuffled positions
## give. This measures that statistic -- as an excess over the shuffled mean,
## as a p-value, and in standard deviations of the shuffled values -- on the
## scenarios of time_encoding.R, where it is known which cycle, if any, is
## real, and counts what each rule would have let in:
##   * the study's scenarios at 400 rows, the rhythm at full strength;
##   * the rhythm scenarios with the effect halved (delta 0.6);
##   * scenarios with no rhythm at 2,000 rows, where chance separation is
##     smaller and a fixed floor would behave differently.
##
## Run from the package root:  Rscript dev/studies/time_gate_threshold.R

pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
src <- readLines("dev/studies/time_encoding.R")
eval(parse(text = src[grep("^ari <- ", src):(grep("^## The encodings", src) - 1L)]))

stats_of <- function(x, others, cand, B = 199L) {
  pr <- ilm_sep_prep(others)
  set.seed(1L)
  lt <- as.POSIXlt(x)
  do.call(rbind, lapply(cand, function(a) {
    b <- ilm_time_bins(lt, a)
    obs <- max(ilm_sep_all(pr, b))
    nul <- vapply(seq_len(B), function(i) max(ilm_sep_all(pr, sample(b))), 0)
    data.frame(aspect = a, excess = obs - mean(nul),
               z = (obs - mean(nul)) / stats::sd(nul),
               p = (1 + sum(nul >= obs)) / (B + 1))
  }))
}
truth <- c(season = "yday", weekend = "wday", weekend_even = "wday",
           monthend = "mday", monthend_many = "mday", turnofmonth = "mday",
           night = "hour")
run <- function(scs, delta, reps, n = 400) {
  do.call(rbind, lapply(scs, function(sc) do.call(rbind, lapply(seq_len(reps), function(r) {
    g <- gen(sc, delta, seed = 1000 * r + 7, n = n)
    f <- ilm_time_features(g$when, "when", cycles = TRUE)
    cand <- unique(sub("_(sin|cos)$", "", substring(
      names(f)[grepl("_(sin|cos)$", names(f))], 6L)))
    s <- stats_of(g$when, g$X, cand)
    s$scenario <- sc; s$rep <- r; s$delta <- delta; s$n <- n
    s$real <- !is.na(truth[sc]) & s$aspect == truth[sc]
    s
  }))))
}
rules <- list(
  "p < 0.01, excess >= 0.01" = function(s) s$p < 0.01 & s$excess >= 0.01,
  "z >= 4, excess >= 0.01"   = function(s) s$z >= 4 & s$excess >= 0.01,
  "z >= 5, excess >= 0.01"   = function(s) s$z >= 5 & s$excess >= 0.01,
  "excess >= 0.1"            = function(s) s$excess >= 0.1)
report <- function(s, label) {
  cat("\n==", label, "\n")
  none <- !s$scenario %in% names(truth)
  for (nm in names(rules)) {
    ok <- rules[[nm]](s)
    ## a data set with no rhythm is wrongly served if ANY of its cycles gets in
    fa <- tapply(ok[none], paste(s$scenario, s$rep)[none], any)
    cat(sprintf("  %-26s real rhythms let in: %s   no-rhythm data sets given a cycle: %s\n",
                nm, if (any(s$real)) sprintf("%3.0f%%", 100 * mean(ok[s$real])) else "  -",
                if (length(fa)) sprintf("%d of %d", sum(fa), length(fa)) else "-"))
  }
  if (any(s$real)) cat("  real rhythms, z: min", round(min(s$z[s$real]), 1),
                       " median", round(stats::median(s$z[s$real]), 1), "\n")
  if (any(!s$real)) cat("  chance, z: max", round(max(s$z[!s$real]), 2),
                        "; excess: max", round(max(s$excess[!s$real]), 3), "\n")
}
all_scens <- c("none", "mild", "none_many", "era", names(truth), "none_ts")
report(run(all_scens, 1.2, 20), "400 rows, rhythms at full strength (delta 1.2)")
report(run(names(truth), 0.6, 10), "400 rows, rhythms at half strength (delta 0.6)")
report(run(c("none", "mild", "none_ts"), 1.2, 20, n = 2000), "2,000 rows, no rhythm")
