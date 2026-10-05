## S1 of the X7 plan: does scaling each column's loss, or identifying the
## multinomial blocks, stop a noise factor from taking a dimension of
## ilm_reduce(method = "glrm") -- and what does it do to the fit?
##
## The finding behind it: on two-latent mixed data plus one pure-noise
## three-level factor, the rotated GLRM reduction gave the noise factor a
## whole dimension (squared loading 0.82). The losses are unscaled -- at the
## best constant a standardised number costs about 0.5 per row, a factor with
## L levels log L per row over L columns -- and a factor's L logit columns are
## identified only through the ridge penalty. Udell, Horn, Zadeh and Boyd
## (2016, section 5) scale each column's loss by its loss at the best
## constant. The decision on making that the default rested on this study;
## the package defaults were unchanged while it ran, and the arms were
## switched by options read inside ilm_glrm() on the study's branch only.
## Scaled losses are now ilm_glrm()'s default and those options are gone, so
## on the released package every arm runs the default.
##
## Design, fixed before any full run.
##   Data: n rows (n = 200 or 1,000) in three equal clusters g. Latents
##   f1 = (-2, 0, 2)[g] + N(0, 0.6^2) and f2 ~ N(0, 1). Numeric a = f1 + e,
##   b = -f1 + e, c = f2 + e with e ~ N(0, 0.4^2); a binary y, "hi" with
##   probability plogis(2.5 f1). Scenarios add:
##     noise_L3     one pure-noise factor, 3 equally likely levels
##     noise_L10    one pure-noise factor, 10 levels
##     noise3_L3    three pure-noise factors, 3 levels each
##     info_L6      an informative factor, f2 + N(0, 0.4^2) cut at its
##                  sextiles, plus one pure-noise 3-level factor
##   10% of cells removed completely at random (never a whole row) and kept
##   aside for the imputation outcome.
##   Arms, each run through ilm_reduce(method = "glrm", ndim = 2) with lambda
##   chosen by the function's own cross-validation:
##     current    the package as it is (50 replicates)
##     scaled     each column's loss divided by its loss at the best constant
##                (50 replicates)
##     s2z        multinomial blocks centred (sum-to-zero) after every step
##                (10 replicates, as a check of the argument below)
##   Why s2z should change nothing, and why there is no "both" arm: for a
##   multinomial block the gradient with respect to the linear predictor,
##   P - onehot, sums to zero across the block's columns in every row. So the
##   update of the archetypes, X'G, sums to zero across the block; the ridge
##   term lambda Y shrinks toward zero and keeps any centring; and the offsets'
##   update, colSums(G), sums to zero across the block too. ilm_glrm() starts
##   from the SVD of centred indicator columns, which is centred already, so
##   the fit never leaves the sum-to-zero subspace and centring it is a no-op.
##   The same holds with the losses scaled, so "both" equals "scaled". A smoke
##   run matched current and s2z to three decimals on every outcome.
##   Outcomes per fit, each reported as a mean with its Monte Carlo standard
##   error:
##     noise_share    the pure-noise factors' summed squared loadings,
##                    averaged over the two dimensions (0 is ideal)
##     latent_r2      R^2 of each true latent on the two coordinates,
##                    averaged over f1 and f2 (1 is ideal)
##     ari            adjusted Rand index of k-means (k = 3, nstart = 25) on
##                    the coordinates against g
##     err_numeric    mean squared error at the removed numeric cells, each
##                    column standardised by its observed sd
##     err_binary     misclassification at the removed cells of y
##     err_info       misclassification at the removed cells of the
##                    informative factor (info_L6 only)
##     lambda         the lambda each arm chose
##   Named in advance as an outcome to watch: err_binary at n = 200, which one
##   smoke replicate showed rising under scaling (0.062 to 0.125); it is
##   reported whichever way it falls.
##   50 replicates per condition for current and scaled, since the claim ranks
##   them; 10 for s2z.
##
## Run from the package root:
##   Rscript dev/studies/glrm_scaling.R [reps] [outfile.csv] [scenarios]
## `scenarios` (optional) is a comma-separated subset of noise_L3,
## noise_L10, noise3_L3, info_L6, so the work can be split across processes.
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 50L
out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else NA_character_
scen <- if (length(args) >= 3L) strsplit(args[3], ",")[[1]] else
  c("noise_L3", "noise_L10", "noise3_L3", "info_L6")

ari <- function(a, b) {
  tab <- table(a, b); n <- sum(tab)
  s <- sum(choose(tab, 2)); sa <- sum(choose(rowSums(tab), 2))
  sb <- sum(choose(colSums(tab), 2)); e <- sa * sb / choose(n, 2)
  if ((sa + sb) / 2 == e) return(0)
  (s - e) / ((sa + sb) / 2 - e)
}

make_data <- function(scenario, seed, n) {
  set.seed(seed)
  g <- rep(1:3, length.out = n)
  f1 <- c(-2, 0, 2)[g] + stats::rnorm(n, 0, 0.6); f2 <- stats::rnorm(n)
  d <- data.frame(a = f1 + stats::rnorm(n, 0, 0.4), b = -f1 + stats::rnorm(n, 0, 0.4),
                  c = f2 + stats::rnorm(n, 0, 0.4))
  d$y <- factor(ifelse(stats::plogis(2.5 * f1) > stats::runif(n), "hi", "lo"))
  noisef <- function(L) factor(sample(paste0("l", seq_len(L)), n, TRUE))
  noise <- character(0); info <- character(0)
  if (scenario == "noise_L3") { d$z1 <- noisef(3); noise <- "z1" }
  if (scenario == "noise_L10") { d$z1 <- noisef(10); noise <- "z1" }
  if (scenario == "noise3_L3") { d$z1 <- noisef(3); d$z2 <- noisef(3); d$z3 <- noisef(3)
    noise <- c("z1", "z2", "z3") }
  if (scenario == "info_L6") {
    h <- f2 + stats::rnorm(n, 0, 0.4)
    d$w <- factor(cut(h, stats::quantile(h, 0:6 / 6), include.lowest = TRUE, labels = FALSE))
    d$z1 <- noisef(3); noise <- "z1"; info <- "w"
  }
  full <- d
  M <- matrix(stats::runif(n * ncol(d)) < 0.10, n, ncol(d))
  M[rowSums(M) == ncol(d), 1] <- FALSE
  for (j in seq_len(ncol(d))) d[[j]][M[, j]] <- NA
  list(d = d, full = full, miss = M, g = g, f1 = f1, f2 = f2, noise = noise, info = info)
}

arms <- list(current = c(FALSE, FALSE), scaled = c(TRUE, FALSE),
             s2z = c(FALSE, TRUE))
s2z_reps <- min(reps, 10L)
rows <- list()
for (sc in scen) for (N in c(200L, 1000L)) for (s in seq_len(reps)) {
  x <- make_data(sc, 100000L * match(sc, c("noise_L3", "noise_L10", "noise3_L3", "info_L6")) +
                   10L * (N %/% 100L) + 1000L * s, N)
  for (arm in names(arms)) {
    if (arm == "s2z" && s > s2z_reps) next
    old <- options(illumex.glrm_scale = arms[[arm]][1], illumex.glrm_s2z = arms[[arm]][2])
    r <- tryCatch(suppressMessages(suppressWarnings(
      ilm_reduce(x$d, ndim = 2L, method = "glrm", progress = FALSE))),
      error = function(e) NULL)
    options(old)
    if (is.null(r)) {
      rows[[length(rows) + 1L]] <- data.frame(scenario = sc, n = N, rep = s, arm = arm,
        failed = TRUE, noise_share = NA, latent_r2 = NA, ari = NA, err_numeric = NA,
        err_binary = NA, err_info = NA, lambda = NA)
      next
    }
    vc <- r$var_contrib
    ns <- if (length(x$noise)) mean(tapply(vc$sqload[vc$variable %in% x$noise],
                                           vc$dim[vc$variable %in% x$noise], sum)) else NA
    co <- as.matrix(r$ind_coord)
    r2 <- mean(c(summary(stats::lm(x$f1 ~ co))$r.squared,
                 summary(stats::lm(x$f2 ~ co))$r.squared))
    set.seed(1)
    km <- stats::kmeans(co, 3L, nstart = 25L)$cluster
    fit <- r$fit$fitted
    m <- x$miss; cn <- names(x$d)
    en <- mean(unlist(lapply(c("a", "b", "c"), function(v) {
      i <- m[, match(v, cn)]; if (!any(i)) return(NULL)
      ((as.numeric(fit[[v]][i]) - x$full[[v]][i]) / stats::sd(x$d[[v]], na.rm = TRUE))^2
    })))
    miscl <- function(v) { i <- m[, match(v, cn)]
      if (!any(i)) NA else mean(as.character(fit[[v]][i]) != as.character(x$full[[v]][i])) }
    rows[[length(rows) + 1L]] <- data.frame(scenario = sc, n = N, rep = s, arm = arm,
      failed = FALSE, noise_share = ns, latent_r2 = r2, ari = ari(km, x$g),
      err_numeric = en, err_binary = miscl("y"),
      err_info = if (length(x$info)) miscl(x$info) else NA, lambda = r$fit$lambda)
  }
}
res <- do.call(rbind, rows)
se <- function(z) { z <- z[is.finite(z)]; if (length(z) < 2L) NA else stats::sd(z) / sqrt(length(z)) }
fmt <- function(z) { z <- z[is.finite(z)]
  if (!length(z)) "" else sprintf("%.3f (%.3f)", mean(z), se(z)) }
vals <- c("noise_share", "latent_r2", "ari", "err_numeric", "err_binary", "err_info", "lambda")
key <- paste(res$scenario, res$n, res$arm)
tab <- do.call(rbind, lapply(unique(key), function(k) {
  r <- res[key == k, ]
  cbind(r[1, c("scenario", "n", "arm")], failed = sum(r$failed),
        t(stats::setNames(vapply(vals, function(v) fmt(r[[v]]), ""), vals)))
}))
cat(sprintf("mean (Monte Carlo standard error) over %d replicates\n", reps))
print(tab, row.names = FALSE)
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
