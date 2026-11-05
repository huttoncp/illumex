## Study B: which parallel analysis chooses ndim, and does a bimodality screen
## of the dimensions it leaves out recover cluster structure that variance
## ordering puts last? Design approved 2026-10-10 (drafts/studyB_ndim_pa_
## variants_design.md in the work folder); registered, with its cost from the
## pilots, at the commit that adds the registration below.
##
## Arms. Each reduces with ilm_reduce() (FAMD), chooses ndim by its rule, and
## clusters with ilm_cluster() at today's settings (gap statistic,
## firstSEmax, k_max = 10, B = 50, seed = the replicate's seed):
##   fixed5      5, capped at the dimensions available
##   perm95      permutation parallel analysis, 95th percentile, B = 30: each
##               original column permuted (a category's indicators together),
##               ilm_pa_inputs() in R/ilm_pa.R
##   screen_bc   perm95's k, then every left-out dimension up to min(10,
##               available) whose scores have Sarle's bimodality coefficient
##               above 5/9 is kept; ndim = the last kept
##   screen_dip  the same, keeping a dimension when Hartigan's dip test gives
##               p < 0.05 (diptest::dip.test)
##   oracle      the smallest ndim whose dimensions hold 99% of the true
##               groups' between-group sum of squares in the reduced scores
## Secondary, outside the rules: screen_bc_select and screen_dip_select
## cluster on the kept dimensions only (1..k and the bimodal ones), passed to
## ilm_cluster() as a matrix of coordinates.
##
## Data: ndim_choice.R's nine designs (its generators, read from its file),
## plus bimodal_null and unimodal_heavy (below);
## 20 replicates each, seed = 70000 + 1000 * design + replicate.
##
## Rules, fixed before any run (design note, R2 and R3; R1 withdrawn by
## addendum 1):
##   R2 a screen joins phase C when its mean ARI on separated exceeds
##      perm95's by >= 0.10 and > 2 paired SE, and is behind by > 0.02 in no
##      design.
##   R3 the dip test is used, and diptest goes into Suggests, only when its
##      pooled detection (keeping a left-out dimension with >= 25% of the
##      between-group SS; separated, overlapping, numeric_many) exceeds BC's by >= 0.10 and > 2 paired SE, and its pooled
##      false-alarm share (keeping one with < 5%; continuum, unimodal_heavy,
##      bimodal_null) is not above BC's by more than 0.02. Otherwise BC.
##
## Addendum 1, made before any pilot (2026-10-11), after the smoke run and a
## diagnostic of the deflated arm.
##   The deflated arm is withdrawn, and with it rule R1. As specified, the
##   value it tests at step j is the residual's largest eigenvalue, which is
##   never below the residual's largest column variance; the permutation null
##   of that residual sits near the same largest column variance; and a
##   rank-(j - 1) PCA residual is not independent across columns even when no
##   structure is left, because the fit removes variance unevenly. So the arm
##   asks whether any correlation is left, which in standardised data is
##   nearly always so: on one replicate it kept every dimension of the
##   correlated continuum (no clusters; 0.264 against 0.099 at j = 5) and kept
##   going on shadow_numeric past its three latent dimensions (0.293 against
##   0.235 at j = 4, 0.095 against 0.070 at j = 5). A sound deflated null
##   (Dobriban and Owen 2019) is a study of its own, recorded
##   as a possible follow-up study after illumex's first CRAN release.
##   shadow_numeric and shadow_mixed are withdrawn with it: their only purpose
##   was R1, and the smoke's oracle on shadow_numeric was 2, which plain PA
##   already chose, so they did not shadow.
##   Unchanged: the arms fixed5, perm95, screen_bc, screen_dip and oracle,
##   the secondary _select arms, the other eleven designs, and rules R2 and
##   R3, whose detection pool is now separated, overlapping and numeric_many.
##
## References for the addendum:
##   Dobriban, E. and Owen, A. B. (2019). Deterministic parallel analysis: an
##     improved method for selecting factors and principal components.
##     Journal of the Royal Statistical Society B 81(1), 163-183.
##
## Registered cost (2026-10-11), from the pilot (replicates 101 and 102 of
## every design, every arm, one core; results/ndim_pa_variants_
## pilot*): 19.1 minutes in all. Per replicate it took 11 to 18 s on the
## 300-row designs and 57 to 89 s on the 1,000-row ones. The full run (20
## replicates of 11 designs) is estimated at 3.2 hours on one core, or about
## 1.7 hours split over two by design; it stops and reports at twice that,
## 6.4 hours (3.4 on two).
##
## Run from the package root of a clean clone at the registration commit,
## with the study library first:
##   R_LIBS_USER=<a pinned library holding diptest> \
##     Rscript dev/studies/ndim_pa_variants.R smoke|pilot|full <out-prefix>
## "smoke" runs every design once with B = 5 everywhere; its output is not a
## result. "pilot" runs replicates 101 and 102 of every design, for the cost
## estimate. "full" runs 1 to 20. Two files are written as each replicate
## finishes, <out-prefix>_arms.csv and <out-prefix>_dims.csv.

suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
for (p in c("cluster", "diptest"))
  if (!requireNamespace(p, quietly = TRUE)) stop("this study needs ", p)
a <- commandArgs(TRUE)
MODE <- if (length(a)) a[1] else "smoke"
PREFIX <- if (length(a) >= 2L) a[2] else file.path(tempdir(), "ndim_pa_variants")
SMOKE <- MODE == "smoke"
B_GAP <- if (SMOKE) 5L else 50L; B_PA <- if (SMOKE) 5L else 30L
K_MAX <- 10L; J_MAX <- 10L; REPS <- 20L
OUT_ARMS <- paste0(PREFIX, "_arms.csv"); OUT_DIMS <- paste0(PREFIX, "_dims.csv")
if (!SMOKE && (file.exists(OUT_ARMS) || file.exists(OUT_DIMS)))
  stop("refusing to overwrite ", OUT_ARMS, " or ", OUT_DIMS)
q <- function(e) suppressWarnings(suppressMessages(e))

## ---- data -------------------------------------------------------------------

## ndim_choice.R's generators, exactly as that study ran them
src <- readLines(file.path("dev", "studies", "ndim_choice.R"))
eval(parse(text = src[grep("^## ---- data", src):(grep("^## ---- arms", src) - 1L)]))

## the separated design plus two noise columns, each a 50/50 mixture of two
## normals 4 sd apart, independent of the clusters
gen$bimodal_null <- function(n = 1000L) {
  x <- options_design("separated", n)
  x$d$b1 <- stats::rnorm(n, sample(c(-2, 2), n, TRUE), 1)
  x$d$b2 <- stats::rnorm(n, sample(c(-2, 2), n, TRUE), 1)
  x
}
## the continuum with t(3) margins: heavy tails, no clusters
gen$unimodal_heavy <- function(n = 1000L) {
  p <- 5L
  R <- 0.6 ^ abs(outer(1:p, 1:p, "-"))
  X <- matrix(stats::rt(n * p, 3), n, p) %*% chol(R)
  list(g = rep(1L, n), oracle = NA_integer_, d = as.data.frame(X))
}
CONDS <- names(gen)
TRUTH <- c(TRUTH, bimodal_null = 4L, unimodal_heavy = 1L)

## ---- the rules ----------------------------------------------------------------

## Sarle's bimodality coefficient with SAS's bias-corrected skewness and
## excess kurtosis
sarle_bc <- function(x) {
  n <- length(x); m <- mean(x); d <- x - m
  m2 <- mean(d^2); m3 <- mean(d^3); m4 <- mean(d^4)
  g1 <- m3 / m2^1.5; g2 <- m4 / m2^2 - 3
  G1 <- g1 * sqrt(n * (n - 1)) / (n - 2)
  G2 <- ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
  (G1^2 + 1) / (G2 + 3 * (n - 1)^2 / ((n - 2) * (n - 3)))
}

## each dimension's share of the true groups' between-group sum of squares
between_share <- function(S, g) {
  if (length(unique(g)) < 2L) return(rep(NA_real_, ncol(S)))
  b <- apply(S, 2L, function(s) {
    m <- tapply(s, g, mean); nc <- tabulate(factor(g))
    sum(nc * (m - mean(s))^2)
  })
  b / sum(b)
}

ari <- function(a, b) {
  t <- table(a, b); n <- sum(t)
  s <- sum(choose(t, 2)); sa <- sum(choose(rowSums(t), 2)); sb <- sum(choose(colSums(t), 2))
  e <- sa * sb / choose(n, 2); m <- (sa + sb) / 2
  if (m == e) return(1)
  (s - e) / (m - e)
}

cluster_on <- function(x, g, seed) {
  t <- system.time(cl <- q(ilm_cluster(x, k_max = K_MAX, B = B_GAP, seed = seed)))[["elapsed"]]
  list(k = cl$k, seconds = t,
       ari = if (length(unique(g)) > 1L) ari(g, cl$ind_cluster$cluster) else NA_real_)
}

write_csv <- function(row, file) utils::write.table(row, file, sep = ",", append = file.exists(file),
                                                    col.names = !file.exists(file), row.names = FALSE)

## ---- run ----------------------------------------------------------------------

plan <- switch(MODE,
  smoke = expand.grid(rep = 1L, cond = CONDS, stringsAsFactors = FALSE),
  pilot = expand.grid(rep = 101:102, cond = CONDS, stringsAsFactors = FALSE),
  full = expand.grid(rep = seq_len(REPS), cond = CONDS, stringsAsFactors = FALSE),
  stop("mode is smoke, pilot or full", call. = FALSE))
cat(sprintf("ndim_pa_variants %s: %d replicates to %s; diptest %s from %s; started %s\n",
            MODE, nrow(plan), PREFIX, format(utils::packageVersion("diptest")),
            dirname(system.file(package = "diptest")), format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
## every package actually loaded, and where from: the study library sits in
## front of the user library, so its manifest alone does not say what ran
loaded <- function() {
  p <- sort(loadedNamespaces())
  data.frame(package = p, version = vapply(p, function(x) format(utils::packageVersion(x)), ""),
             library = vapply(p, function(x) dirname(find.package(x)), ""),
             row.names = NULL)
}
cat(".libPaths():", paste(.libPaths(), collapse = " | "), "\n")
print(utils::sessionInfo())
for (i in seq_len(nrow(plan))) {
  cond <- plan$cond[i]; rep <- plan$rep[i]
  seed <- 70000L + 1000L * match(cond, CONDS) + rep
  set.seed(seed)
  x <- gen[[cond]]()
  inp <- q(ilm_reduce_inputs(x$d, names(x$d), "cycles"))
  J <- min(J_MAX, inp$max_dim)
  full <- q(ilm_reduce(x$d, ndim = J))
  S <- as.matrix(full$ind_coord[, paste0("dim", seq_len(ncol(full$ind_coord) - 1L)), drop = FALSE])
  share <- between_share(S, x$g)
  oracle <- if (anyNA(share)) NA_integer_ else which(cumsum(share) >= 0.99)[1L]

  set.seed(seed + 1L)
  t_pa <- system.time(k_pa <- ilm_pa_inputs(inp, B = B_PA)$ndim)[["elapsed"]]
  left <- if (k_pa < ncol(S)) (k_pa + 1L):ncol(S) else integer(0)
  bc <- vapply(seq_len(ncol(S)), function(j) sarle_bc(S[, j]), 0)
  dipp <- vapply(seq_len(ncol(S)), function(j) diptest::dip.test(S[, j])$p.value, 0)
  keep_bc <- left[bc[left] > 5 / 9]; keep_dip <- left[dipp[left] < 0.05]
  write_csv(data.frame(cond = cond, rep = rep, dim = seq_len(ncol(S)), left_out = seq_len(ncol(S)) %in% left,
                       between_share = round(share, 6), bc = round(bc, 6), dip_p = round(dipp, 6)),
            OUT_DIMS)

  arms <- list(fixed5 = min(5L, inp$max_dim), perm95 = k_pa,
               screen_bc = max(c(k_pa, keep_bc)), screen_dip = max(c(k_pa, keep_dip)),
               oracle = oracle)
  for (arm in names(arms)) {
    nd <- arms[[arm]]
    if (is.na(nd)) next
    r <- cluster_on(q(ilm_reduce(x$d, ndim = max(2L, nd))), x$g, seed)
    write_csv(data.frame(cond = cond, rep = rep, arm = arm, ndim = nd,
                         dims = paste(seq_len(nd), collapse = " "), k = r$k, truth = TRUTH[[cond]],
                         k_ok = r$k == TRUTH[[cond]], ari = r$ari, oracle = oracle,
                         seconds = round(r$seconds + if (arm == "perm95") t_pa else 0, 2)),
              OUT_ARMS)
  }
  for (sel in c("bc", "dip")) {
    dims <- sort(unique(c(seq_len(k_pa), if (sel == "bc") keep_bc else keep_dip)))
    if (length(dims) < 2L) dims <- 1:2
    r <- cluster_on(S[, dims, drop = FALSE], x$g, seed)
    write_csv(data.frame(cond = cond, rep = rep, arm = paste0("screen_", sel, "_select"),
                         ndim = length(dims), dims = paste(dims, collapse = " "), k = r$k,
                         truth = TRUTH[[cond]], k_ok = r$k == TRUTH[[cond]], ari = r$ari,
                         oracle = oracle, seconds = round(r$seconds, 2)),
              OUT_ARMS)
  }
  cat(sprintf("%s %-16s rep %3d  pa %d bc %d dip %d oracle %s\n",
              format(Sys.time(), "%H:%M:%S"), cond, rep, k_pa, arms$screen_bc,
              arms$screen_dip, format(oracle)))
}
cat("\npackages loaded by the end of the run:\n")
print(loaded(), row.names = FALSE)
cat("finished", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
