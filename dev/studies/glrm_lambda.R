## How much ridge penalty ilm_glrm() needs, measured by held-out error.
##
## Backs the numbers in the `lambda` documentation of R/ilm_glrm.R: on a 400
## by 4 matrix with 20% of cells missing, the error of the reconstruction at
## the missing cells for small, middling and large lambda, for the value
## ilm_glrm() chooses itself (lambda = NULL), and for three baselines: an
## iterative rank-2 SVD with no penalty (fill, decompose, refill until the
## filled cells stop moving), the regularised iterative PCA of
## missMDA::imputePCA() at rank 2 (added, on request, as the fairer established
## baseline after the first run showed the unpenalised SVD blowing up on some
## datasets: median error 1.10, mean 1.45, largest 8.9), and the column means.
##
## Design, fixed before any full run. X = L W + E: L standard normal
## (n x 2, n = 150 or 400), W standard normal (2 x 4) drawn per replicate, E
## normal with sd 0.5. 20% of cells are removed completely at random, never a
## whole row. Error is the mean squared error at the removed cells, each
## column standardised by its observed sd, as ilm_glrm()'s own lambda
## selection scores it. Rank 2 throughout. 50 replicates per size, since the
## claim ranks settings and methods; every mean is printed with its Monte
## Carlo standard error.
##
## Rerun for item 140 on the scaled losses (W1) with the penalty
## path, to remeasure the numbers in the `lambda` documentation: the fixed
## values extended to the new grid's 20, 50 and 100 in place of 25; nothing
## else changed.
##
## Run from the package root:
##   Rscript dev/studies/glrm_lambda.R [reps] [outfile.csv]
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
if (!requireNamespace("missMDA", quietly = TRUE)) stop("this study needs missMDA")
options(width = 200)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 50L
out <- if (length(args) >= 2L) args[2] else NA_character_

make_data <- function(seed, n = 400L, p = 4L, miss = 0.2) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * 2), n, 2) %*% matrix(stats::rnorm(2 * p), 2, p) +
    matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  M <- matrix(stats::runif(n * p) < miss, n, p)
  M[rowSums(M) == p, 1] <- FALSE
  Xm <- X; Xm[M] <- NA
  colnames(Xm) <- colnames(X) <- paste0("x", seq_len(p))
  list(truth = X, d = as.data.frame(Xm), miss = M)
}

err <- function(fill, x) {
  sc <- apply(as.matrix(x$d), 2, stats::sd, na.rm = TRUE)
  mean((sweep(fill - x$truth, 2, sc, "/")[x$miss])^2)
}

svd_impute <- function(Xm, k = 2L, maxit = 500L, tol = 1e-7) {
  M <- is.na(Xm); X <- Xm
  mu <- colMeans(Xm, na.rm = TRUE)
  X[M] <- mu[col(X)[M]]
  for (it in seq_len(maxit)) {
    m <- colMeans(X)
    s <- svd(sweep(X, 2, m), nu = k, nv = k)
    fit <- sweep(s$u %*% diag(s$d[1:k], k) %*% t(s$v), 2, m, "+")
    delta <- sum((X[M] - fit[M])^2) / max(sum(X[M]^2), 1e-12)
    X[M] <- fit[M]
    if (delta < tol) break
  }
  X
}

## the fixed values: ilm_glrm()'s own grid since item 140, 0.1 to 100
grid <- c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100)
rows <- list()
for (N in c(150L, 400L)) for (s in seq_len(reps)) {
  x <- make_data(1000L * (N %/% 50L) + s, n = N)
  add <- function(method, lambda, error)
    rows[[length(rows) + 1L]] <<- data.frame(n = N, rep = s, method = method,
                                             lambda = lambda, error = error)
  for (lam in c(grid, NA)) {
    g <- ilm_glrm(x$d, rank = 2L, lambda = if (is.na(lam)) NULL else lam,
                  seed = 1L, progress = FALSE)
    f <- as.matrix(as.data.frame(lapply(g$fitted, as.numeric)))
    add(if (is.na(lam)) "ilm_glrm, lambda chosen" else sprintf("ilm_glrm, lambda = %g", lam),
        if (is.na(lam)) g$lambda else lam, err(f, x))
  }
  add("iterative SVD, rank 2, no penalty", NA, err(svd_impute(as.matrix(x$d)), x))
  ip <- missMDA::imputePCA(as.matrix(x$d), ncp = 2L, scale = TRUE,
                           method = "Regularized")$completeObs
  add("missMDA::imputePCA, rank 2, regularised", NA, err(as.matrix(ip), x))
  cm <- as.matrix(x$d); mu <- colMeans(cm, na.rm = TRUE)
  cm[is.na(cm)] <- mu[col(cm)[is.na(cm)]]
  add("column means", NA, err(cm, x))
}
res <- do.call(rbind, rows)
lv <- unique(res$method)
tab <- do.call(rbind, lapply(c(150L, 400L), function(N) {
  r <- res[res$n == N, ]
  data.frame(n = N, method = lv,
             error = tapply(r$error, r$method, mean)[lv],
             mc_se = tapply(r$error, r$method,
                            function(z) stats::sd(z) / sqrt(length(z)))[lv])
}))
cat(sprintf(paste0("held-out error at the missing cells, mean and Monte Carlo",
                   " standard error over %d replicates\n"), reps))
print(format(tab, digits = 3), row.names = FALSE)
for (N in c(150L, 400L)) {
  ch <- res$lambda[res$method == "ilm_glrm, lambda chosen" & res$n == N]
  cat(sprintf("\nlambda chosen at n = %d: ", N),
      paste(names(table(ch)), table(ch), sep = " x", collapse = ", "), "\n")
}
if (!is.na(out)) utils::write.csv(res, out, row.names = FALSE)
