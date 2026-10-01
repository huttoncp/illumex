## A FAMD written out in illumex (ilm_famd(), R/ilm_famd.R) against the two
## established implementations: does it agree, and is it faster?
##
## ilm_reduce() calls PCAmixdata::PCAmix(), a Suggests dependency that
## ilm_reduce(), ilm_cluster() and ilm_profile() need for any mixed data.
## Craig asked whether a hand-rolled FAMD, with no dependency at all, gains
## speed; agreement comes first, since a faster wrong answer is no use.
##
## Design, fixed before any full run.
##   Agreement, against PCAmix (eigenvalues, individual coordinates up to
##   sign, squared loadings) and FactoMineR::FAMD() (eigenvalues and
##   coordinates), on four kinds of data from ilm_sim(): mixed (four numeric
##   columns, grp and site), numeric only, categorical only (grp, site,
##   flag), and mixed with gaps (3 missing numbers, 2 missing categories;
##   FactoMineR is not compared there, since it imputes differently).
##   Reported as the largest absolute difference.
##   Speed: reduce_vs_famd.R's generator (rank-3 structure, half the columns
##   numeric, half four-level factors) at 200 x 12, 2,000 x 12, 2,000 x 120,
##   20,000 x 12, 20,000 x 120 and 100,000 x 20; 5 dimensions; each method
##   timed on the same data in the same session, 20 replicates (5 at
##   100,000 rows), with the order of the three methods rotated between
##   replicates. Reported as the median time relative to PCAmix, with the
##   Monte Carlo standard error of the mean ratio, and the agreement on
##   each dataset as a check.
##
## Run from the package root:
##   Rscript dev/studies/famd_own.R [reps] [outfile.csv]
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
for (p in c("FactoMineR", "PCAmixdata"))
  if (!requireNamespace(p, quietly = TRUE)) stop("this study needs ", p)
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 20L
out <- if (length(args) >= 2L && nzchar(args[2])) args[2] else NA_character_

own <- function(q, c, k) ilm_famd(q, c, k)
pcamix <- function(q, c, k) PCAmixdata::PCAmix(X.quanti = q, X.quali = c, ndim = k,
                                               rename.level = TRUE, graph = FALSE)
famd <- function(q, c, k) {
  d <- if (is.null(q)) c else if (is.null(c)) q else cbind(q, c)
  if (is.null(c)) FactoMineR::PCA(d, ncp = k, graph = FALSE)
  else if (is.null(q)) FactoMineR::MCA(d, ncp = k, graph = FALSE)
  else FactoMineR::FAMD(d, ncp = k, graph = FALSE)
}
agree <- function(a, b, k, sq = TRUE) {
  ne <- min(k, nrow(a$eig), nrow(b$eig))
  kc <- min(k, ncol(a$ind$coord), ncol(b$ind$coord))
  c(eig = max(abs(a$eig[seq_len(ne), 1] - b$eig[seq_len(ne), 1])),
    coord = max(abs(abs(as.matrix(a$ind$coord)[, seq_len(kc)]) -
                    abs(as.matrix(b$ind$coord)[, seq_len(kc)]))),
    sqload = if (sq) max(abs(a$sqload[rownames(b$sqload), seq_len(kc)] -
                             b$sqload[, seq_len(kc)])) else NA_real_)
}

## ---- agreement --------------------------------------------------------------
d <- ilm_sim()
q <- d[c("score", "income", "visits", "claims")]
ql <- data.frame(grp = d$grp, site = factor(d$site))
qn <- q; qn$income[c(3, 50, 99)] <- NA
qln <- ql; qln$grp[c(5, 60)] <- NA
cases <- list(
  mixed = list(q, ql, 5L), numeric_only = list(q, NULL, 3L),
  categorical_only = list(NULL, data.frame(ql, flag = factor(d$flag)), 5L),
  mixed_with_gaps = list(qn, qln, 5L))
ag <- do.call(rbind, lapply(names(cases), function(nm) {
  cs <- cases[[nm]]
  a <- own(cs[[1]], cs[[2]], cs[[3]])
  vp <- agree(a, pcamix(cs[[1]], cs[[2]], cs[[3]]), cs[[3]])
  vf <- if (nm == "mixed_with_gaps") c(eig = NA, coord = NA) else {
    f <- famd(cs[[1]], cs[[2]], cs[[3]])
    ## FactoMineR's MCA divides the eigenvalues by the number of variables Q,
    ## and so the coordinates by sqrt(Q); put them back on FAMD's scale
    fe <- f$eig; fc <- f$ind$coord
    if (is.null(cs[[1]])) {
      fe[, 1] <- fe[, 1] * ncol(cs[[2]]); fc <- fc * sqrt(ncol(cs[[2]]))
    }
    agree(a, list(eig = fe, ind = list(coord = fc)), cs[[3]], sq = FALSE)[1:2]
  }
  data.frame(data = nm, pcamix_eig = vp[["eig"]], pcamix_coord = vp[["coord"]],
             pcamix_sqload = vp[["sqload"]], factominer_eig = vf[["eig"]],
             factominer_coord = vf[["coord"]])
}))
cat("agreement of ilm_famd() with PCAmix and FactoMineR, largest absolute difference\n")
print(format(ag, digits = 3), row.names = FALSE)

## ---- speed -------------------------------------------------------------------
make_data <- function(n, p, seed) {
  set.seed(seed)
  L <- matrix(stats::rnorm(n * 3), n, 3)
  cols <- lapply(seq_len(p), function(j) {
    z <- 0.8 * L[, (j - 1L) %% 3L + 1L] + stats::rnorm(n, sd = 0.6)
    if (j %% 2L) z
    else factor(cut(z, stats::quantile(z, 0:4 / 4), include.lowest = TRUE,
                    labels = c("q1", "q2", "q3", "q4")))
  })
  names(cols) <- sprintf("v%03d", seq_len(p))
  as.data.frame(cols)
}
sizes <- list(c(200, 12), c(2000, 12), c(2000, 120), c(20000, 12),
              c(20000, 120), c(100000, 20))
rows <- list()
for (sz in sizes) for (s in seq_len(if (sz[1] >= 1e5) max(2L, reps %/% 4L) else reps)) {
  dd <- make_data(sz[1], sz[2], 1000L * s + sz[2])
  isn <- vapply(dd, is.numeric, TRUE)
  qq <- dd[isn]; cc <- dd[!isn]
  fns <- list(own = own, pcamix = pcamix, famd = function(q, c, k) famd(q, c, k))
  ord <- names(fns)[(seq_along(fns) + s - 2L) %% 3L + 1L]
  tm <- setNames(numeric(3), names(fns)); res <- list()
  for (m in ord) tm[[m]] <- system.time(res[[m]] <- fns[[m]](qq, cc, 5L))[["elapsed"]]
  chk <- agree(res$own, res$pcamix, 5L)
  rows[[length(rows) + 1L]] <- data.frame(
    n = sz[1], p = sz[2], rep = s, own = tm[["own"]], pcamix = tm[["pcamix"]],
    factominer = tm[["famd"]], own_vs_pcamix = tm[["own"]] / tm[["pcamix"]],
    factominer_vs_pcamix = tm[["famd"]] / tm[["pcamix"]],
    max_diff_vs_pcamix = max(chk, na.rm = TRUE))
}
res <- do.call(rbind, rows)
se <- function(z) stats::sd(z) / sqrt(length(z))
key <- paste(res$n, res$p)
tab <- do.call(rbind, lapply(unique(key), function(k) {
  r <- res[key == k, ]
  data.frame(n = r$n[1], p = r$p[1], reps = nrow(r),
             pcamix_seconds = stats::median(r$pcamix),
             own_vs_pcamix = stats::median(r$own_vs_pcamix),
             own_vs_pcamix_mc_se = se(r$own_vs_pcamix),
             factominer_vs_pcamix = stats::median(r$factominer_vs_pcamix),
             factominer_vs_pcamix_mc_se = se(r$factominer_vs_pcamix),
             worst_diff_vs_pcamix = max(r$max_diff_vs_pcamix))
}))
cat("\ntime relative to PCAmix (median ratio, below 1 is faster; one session,",
    "one core), and the worst disagreement with PCAmix\n")
print(format(tab, digits = 3), row.names = FALSE)
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
