## POST HOC, descriptive only (not under the registered rules): why did R3's
## detection score zero for both statistics? For each left-out dimension in
## the three detection designs, its share of the between-group SS and its BC
## and dip p-value; and on the mixed designs, what the screen kept.
## Usage, from the package root:
##   Rscript dev/studies/ndim_pa_variants_posthoc.R dev/studies/results/ndim_pa_variants_full
x <- read.csv(paste0(commandArgs(TRUE)[1], "_dims.csv"), stringsAsFactors = FALSE)
s <- x[x$cond %in% c("separated", "overlapping", "numeric_many") & x$left_out, ]
cat("largest between-group share of any LEFT-OUT dimension, per replicate:\n")
print(aggregate(between_share ~ cond, s, function(v) round(stats::quantile(v, c(0, .5, 1)), 3)))
top <- do.call(rbind, lapply(split(s, paste(s$cond, s$rep)), function(r) r[which.max(r$between_share), ]))
cat("\nthat dimension's BC and dip p (median over replicates), and how often each would keep it:\n")
print(aggregate(cbind(between_share, bc, dip_p) ~ cond, top, stats::median))
print(aggregate(cbind(bc_keeps = bc > 5 / 9, dip_keeps = dip_p < 0.05) ~ cond, top, mean))
m <- x[x$cond %in% c("mixed_few_noise", "mixed_counts", "mixed_many") & x$left_out, ]
cat("\nmixed designs: share of left-out dimensions BC keeps, and their between-group share when kept:\n")
print(aggregate(cbind(kept = bc > 5 / 9) ~ cond, m, mean))
print(aggregate(between_share ~ cond, m[m$bc > 5 / 9, ], function(v) round(stats::median(v), 4)))
