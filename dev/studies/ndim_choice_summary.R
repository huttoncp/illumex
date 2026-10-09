## Summarise ndim_choice.R's full run and apply its registered decision rule,
## as written there. Committed before the run's results were read.
##   Rscript dev/studies/ndim_choice_summary.R <ndim_choice.csv> [out.txt]
a <- commandArgs(TRUE)
d <- utils::read.csv(a[1], stringsAsFactors = FALSE)
if (length(a) >= 2L) sink(a[2], split = TRUE)
MIXED <- c("mixed_few", "mixed_few_noise", "mixed_counts", "mixed_many")
se <- function(x) if (sum(!is.na(x)) > 1L) stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))) else NA_real_
f3 <- function(x) formatC(x, format = "f", digits = 3)

cat("ndim_choice: how many dimensions to keep before clustering\n")
cat(nrow(d), "rows,", length(unique(paste(d$cond, d$rep))), "replicates\n\n")

cat("mean (Monte Carlo SE) over replicates, per condition and arm\n")
by <- split(d, list(d$cond, d$arm), drop = TRUE)
tab <- do.call(rbind, lapply(by, function(x) data.frame(
  cond = x$cond[1], arm = x$arm[1], n = nrow(x),
  ari = if (all(is.na(x$ari))) "" else sprintf("%s (%s)", f3(mean(x$ari)), f3(se(x$ari))),
  k_ok = sprintf("%s (%s)", f3(mean(x$k_ok)), f3(se(as.numeric(x$k_ok)))),
  ndim = paste(names(table(x$ndim)), "x", table(x$ndim), sep = "", collapse = " "),
  k = paste(names(table(x$k)), "x", table(x$k), sep = "", collapse = " "),
  seconds = round(mean(x$seconds), 1))))
tab <- tab[order(match(tab$cond, unique(d$cond)), match(tab$arm, c("fixed5", "pa", "oracle"))), ]
print(tab, row.names = FALSE)

cat("\npa against fixed5, paired by replicate\n")
pw <- merge(d[d$arm == "pa", c("cond", "rep", "ari", "k_ok")],
            d[d$arm == "fixed5", c("cond", "rep", "ari", "k_ok")],
            by = c("cond", "rep"), suffixes = c("_pa", "_f5"))
pr <- do.call(rbind, lapply(split(pw, pw$cond), function(x) {
  dif <- x$ari_pa - x$ari_f5
  data.frame(cond = x$cond[1], n = nrow(x),
             ari_diff = mean(dif), ari_diff_se = se(dif),
             k_ok_diff = mean(x$k_ok_pa) - mean(x$k_ok_f5))
}))
pr <- pr[order(match(pr$cond, unique(d$cond))), ]
print(within(pr, { ari_diff <- f3(ari_diff); ari_diff_se <- f3(ari_diff_se); k_ok_diff <- f3(k_ok_diff) }),
      row.names = FALSE)

cat("\nThe registered rule\n")
mx <- pr[pr$cond %in% MIXED & !is.na(pr$ari_diff), ]
wins <- mx$cond[mx$ari_diff >= 0.05 & mx$ari_diff > 2 * mx$ari_diff_se]
c1 <- length(wins) >= 2L
cat(sprintf("  (i)   mean ARI ahead by >= 0.05 and > 2 paired SE in >= 2 of the 4 mixed conditions: %s (%s)\n",
            c1, if (length(wins)) paste(wins, collapse = ", ") else "none"))
judged <- pr[!is.na(pr$ari_diff), ]
behind <- judged$cond[judged$ari_diff < -0.02]
c2 <- length(behind) == 0L
cat(sprintf("  (ii)  mean ARI behind by more than 0.02 in no condition: %s (%s)\n",
            c2, if (length(behind)) paste(behind, collapse = ", ") else "none behind"))
ct <- d[d$cond == "continuum", ]
k1 <- if (nrow(ct)) tapply(ct$k == 1L, ct$arm, mean) else c(pa = NA_real_, fixed5 = NA_real_)
c3 <- isTRUE(k1[["pa"]] >= k1[["fixed5"]] - 0.05)   # FALSE when the continuum has not run
cat(sprintf("  (iii) continuum share of k = 1, pa %s against fixed5 %s, not lower by more than 0.05: %s\n",
            f3(k1[["pa"]]), f3(k1[["fixed5"]]), c3))
cat(sprintf("\nUnder the rule: %s\n",
            if (c1 && c2 && c3) "pa replaces fixed5 as the default"
            else "ndim = 5 stays the default; the help and the profiling vignette say how to choose it"))
if (length(a) >= 2L) sink()
