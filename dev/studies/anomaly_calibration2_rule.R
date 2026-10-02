## Stage 2's decision rule, as fixed in the header of
## dev/studies/anomaly_calibration2.R, applied to the parts' CSVs together.
## Written after the run to apply that rule mechanically; it adds no rule of
## its own.
##
##   Rscript dev/studies/anomaly_calibration2_rule.R part1.csv part2.csv ...
##
## An arm holds in a clean cell when its share of datasets flagging anything
## is within two Monte Carlo standard errors of 0.05 or below it. Among the
## arms that hold in all 36 clean cells, the one flagging the most planted
## rows averaged over the power cells is recommended; if none holds
## everywhere, the report says where each fails.
options(width = 200)
res <- do.call(rbind, lapply(commandArgs(TRUE), utils::read.csv))
se <- function(z) stats::sd(z) / sqrt(length(z))

cl <- res[res$part == "clean", ]
cells <- unique(cl[c("noise", "n", "p")])
arms <- unique(res$arm)
cat(sprintf("%d clean cells, %d power cells, arms: %s\n\n", nrow(cells),
            nrow(unique(res[res$part == "power", c("cond", "shift")])),
            paste(arms, collapse = ", ")))

hold <- do.call(rbind, lapply(arms, function(a) {
  do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    z <- cl$any_flag[cl$arm == a & cl$noise == cells$noise[i] &
                       cl$n == cells$n[i] & cl$p == cells$p[i]]
    m <- mean(z); s <- se(z)
    data.frame(arm = a, noise = cells$noise[i], n = cells$n[i], p = cells$p[i],
               reps = length(z), any_flag = m, mc_se = s,
               holds = m <= 0.05 + 2 * s)
  }))
}))

cat("clean cells held, of", nrow(cells), "(any_flag within two MC SE of 0.05 or below)\n")
tab <- do.call(rbind, lapply(arms, function(a) {
  h <- hold[hold$arm == a, ]
  data.frame(arm = a, held = sum(h$holds), worst_any_flag = max(h$any_flag),
             worst_cell = with(h[which.max(h$any_flag), ], paste(noise, n, p)))
}))
print(tab, row.names = FALSE)

cat("\ncells where each arm fails\n")
for (a in arms) {
  h <- hold[hold$arm == a & !hold$holds, ]
  if (!nrow(h)) { cat(sprintf("  %-12s none\n", a)); next }
  cat(sprintf("  %-12s %s\n", a, paste(sprintf("%s/%d/%d %.3f (%.3f)", h$noise, h$n,
                                                 h$p, h$any_flag, h$mc_se), collapse = "; ")))
}

pw <- res[res$part == "power", ]
pw_tab <- do.call(rbind, lapply(arms, function(a) {
  z <- pw[pw$arm == a, ]
  cellmeans <- tapply(z$flagged, paste(z$cond, z$shift), mean)
  data.frame(arm = a, power_cells = length(cellmeans),
             flagged_avg = mean(cellmeans), false_flag_avg = mean(tapply(
               z$false_flag, paste(z$cond, z$shift), mean)))
}))
cat("\nplanted rows flagged, averaged over the power cells\n")
print(pw_tab[order(-pw_tab$flagged_avg), ], row.names = FALSE)

ok <- tab$arm[tab$held == nrow(cells)]
cat("\nrule: ")
if (!length(ok)) {
  cat("no arm holds in every clean cell; nothing changes until the results are reviewed\n")
} else {
  best <- pw_tab[pw_tab$arm %in% ok, ]
  best <- best$arm[which.max(best$flagged_avg)]
  cat(sprintf("arms holding everywhere: %s; recommended: %s\n",
              paste(ok, collapse = ", "), best))
}

## The run-time warning's diagnostic, which the header has the default arm
## record (alarm_kept on the rows the trimmed fit keeps, alarm_all on every
## row): how often it would warn, on clean and on contaminated data.
d <- res[res$arm == "default", ]
f <- function(z) sprintf("%.2f", mean(z, na.rm = TRUE))
cat("\nthe warning's diagnostic, default arm: share of datasets alarmed\n")
cat("clean data, by noise (averaged over n and p):\n")
dc <- d[d$part == "clean", ]
for (nz in unique(dc$noise)) {
  z <- dc[dc$noise == nz, ]
  cat(sprintf("  %-9s kept rows %s, all rows %s; the default arm flags anything in %s\n",
              nz, f(z$alarm_kept), f(z$alarm_all), f(z$any_flag)))
}
dp <- d[d$part == "power", ]
cat(sprintf("planted anomalies, normal noise: kept rows %s, all rows %s\n",
            f(dp$alarm_kept), f(dp$alarm_all)))
