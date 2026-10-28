## Study B's findings under its registered rules (dev/studies/ndim_pa_variants.R
## with addendum 1): R3 first (it picks the screen's statistic),
## then R2 for that screen. Paired by replicate; SE from the per-replicate
## differences. Everything else is descriptive.
## Usage, from the package root:
##   Rscript dev/studies/ndim_pa_variants_summary.R dev/studies/results/ndim_pa_variants_full
a <- commandArgs(TRUE)
arms <- read.csv(paste0(a[1], "_arms.csv"), stringsAsFactors = FALSE)
dims <- read.csv(paste0(a[1], "_dims.csv"), stringsAsFactors = FALSE)
pse <- function(d) c(mean = mean(d), se = stats::sd(d) / sqrt(length(d)), n = length(d))

## ---- R3: dip test against Sarle's BC ---------------------------------------------
## a design with one group has no between-group structure: every dimension
## carries none of it
dims$between_share[is.na(dims$between_share)] <- 0
dims$keep_bc <- dims$left_out & dims$bc > 5 / 9
dims$keep_dip <- dims$left_out & dims$dip_p < 0.05
per_rep <- function(conds, f) {
  s <- dims[dims$cond %in% conds, ]
  do.call(rbind, lapply(split(s, paste(s$cond, s$rep)), function(r)
    data.frame(cond = r$cond[1], rep = r$rep[1], bc = f(r, r$keep_bc), dip = f(r, r$keep_dip))))
}
det <- per_rep(c("separated", "overlapping", "numeric_many"),
               function(r, k) as.numeric(any(k & r$between_share >= 0.25)))
fal <- per_rep(c("continuum", "unimodal_heavy", "bimodal_null"),
               function(r, k) as.numeric(any(k & r$between_share < 0.05)))
d_det <- pse(det$dip - det$bc); d_fal <- pse(fal$dip - fal$bc)
cat("R3: detection (keeps a left-out dimension with >= 25% of the between-group SS),",
    "pooled over separated, overlapping, numeric_many\n")
cat(sprintf("  BC %.3f, dip %.3f; dip - BC %.3f (SE %.3f, n %d)\n", mean(det$bc), mean(det$dip),
            d_det["mean"], d_det["se"], d_det["n"]))
print(aggregate(cbind(bc, dip) ~ cond, det, mean), row.names = FALSE)
cat("R3: false alarms (keeps a dimension with < 5%), pooled over continuum, unimodal_heavy, bimodal_null\n")
cat(sprintf("  BC %.3f, dip %.3f; dip - BC %.3f (SE %.3f, n %d)\n", mean(fal$bc), mean(fal$dip),
            d_fal["mean"], d_fal["se"], d_fal["n"]))
print(aggregate(cbind(bc, dip) ~ cond, fal, mean), row.names = FALSE)
r3 <- d_det["mean"] >= 0.10 && d_det["mean"] > 2 * d_det["se"] && d_fal["mean"] <= 0.02
stat <- if (r3) "dip" else "bc"
cat(sprintf("R3 verdict: the dip test is %sclearly better; the screen uses %s\n\n",
            if (r3) "" else "NOT ", if (r3) "the dip test (diptest into Suggests)" else "Sarle's BC (no dependency)"))

## ---- R2: the chosen screen against perm95 -------------------------------------------
scr <- paste0("screen_", stat)
w <- reshape(arms[arms$arm %in% c("perm95", scr, "fixed5", "oracle"), c("cond", "rep", "arm", "ari", "k_ok", "ndim")],
             idvar = c("cond", "rep"), timevar = "arm", direction = "wide")
cond_order <- unique(arms$cond)
tab <- do.call(rbind, lapply(cond_order, function(cc) {
  s <- w[w$cond == cc, ]
  dd <- s[[paste0("ari.", scr)]] - s$ari.perm95
  e <- if (all(is.na(dd))) c(mean = NA, se = NA, n = 0) else pse(dd[!is.na(dd)])
  data.frame(cond = cc, ari_fixed5 = mean(s$ari.fixed5), ari_perm95 = mean(s$ari.perm95),
             ari_screen = mean(s[[paste0("ari.", scr)]]), ari_oracle = mean(s$ari.oracle),
             diff = unname(e["mean"]), se = unname(e["se"]),
             kok_perm95 = mean(s$k_ok.perm95), kok_screen = mean(s[[paste0("k_ok.", scr)]]),
             ndim_perm95 = mean(s$ndim.perm95), ndim_screen = mean(s[[paste0("ndim.", scr)]]))
}))
tab[, -1] <- round(tab[, -1], 3)
print(tab, row.names = FALSE)
sep <- tab[tab$cond == "separated", ]
behind <- tab$cond[!is.na(tab$diff) & tab$diff < -0.02]
r2a <- sep$diff >= 0.10 && sep$diff > 2 * sep$se
cat(sprintf("\nR2: separated, %s - perm95 ARI %.3f (SE %.3f): %s; behind by > 0.02 in: %s\n",
            scr, sep$diff, sep$se, if (r2a) "met" else "NOT met",
            if (length(behind)) paste(behind, collapse = ", ") else "none"))
cat(sprintf("R2 verdict: the screen %s the display\n", if (r2a && !length(behind)) "JOINS" else "does NOT join"))
cat("(continuum and unimodal_heavy have one group, so ARI is undefined there; k_ok is the truth k = 1)\n")
