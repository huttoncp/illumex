## ---------------------------------------------------------------------------
## Factor analysis of mixed data, written out rather than borrowed.
##
## What ilm_reduce() computes, since Craig's ruling of 2026-09-28: it replaced
## the PCAmixdata::PCAmix() call after dev/studies/famd_own.R measured it
## against PCAmix and FactoMineR::FAMD() (largest difference 4e-12 on every
## quantity; 1.6 to 5 times faster than PCAmix from 2,000 rows up). Their
## stored outputs stay in the tests as a permanent check.
##
## The method is Pages' FAMD, which PCAmix reproduces: every numeric column
## centred and divided by its standard deviation (the 1/n form); every level
## of a categorical column an indicator, centred and divided by the square
## root of the level's share, so a rare level weighs as much as its rarity
## deserves and no more; then one SVD of the lot with each row weighted 1/n.
## With numeric columns alone this is PCA on the correlation matrix, and with
## categorical columns alone it is MCA up to a constant on the eigenvalues.
##
## A missing number is set to its column's mean and a missing category to a
## row of zeros in that column's indicators -- which is to say, each sits at
## the centre of its column and pulls on nothing.
##
## References:
##   Pages, J. (2004). Analyse factorielle de donnees mixtes. Revue de
##     Statistique Appliquee 52(4), 93-111.
##   Chavent, M., Kuentz-Simonet, V., Labenne, A. and Saracco, J. (2014).
##     Multivariate analysis of mixed data: the PCAmixdata R package.
##     arXiv:1411.4911.
## ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
ilm_famd <- function(quanti = NULL, quali = NULL, ndim = 5L) {
  n <- if (!is.null(quanti)) nrow(quanti) else nrow(quali)
  Z <- NULL; owner <- character(0); num <- NULL
  if (!is.null(quanti)) {
    X <- as.matrix(quanti)
    mu <- colMeans(X, na.rm = TRUE)
    X[is.na(X)] <- mu[col(X)[is.na(X)]]
    s <- sqrt(colMeans(sweep(X, 2L, mu)^2))
    s[!is.finite(s) | s == 0] <- 1
    Z <- sweep(sweep(X, 2L, mu), 2L, s, "/")
    owner <- colnames(X)
    num <- X
  }
  if (!is.null(quali)) for (v in names(quali)) {
    f <- droplevels(as.factor(quali[[v]]))
    G <- vapply(levels(f), function(l) as.numeric(!is.na(f) & f == l),
                numeric(n))
    if (n == 1L) G <- matrix(G, 1L)
    p <- colMeans(G)
    keep <- p > 0
    Zc <- sweep(sweep(G[, keep, drop = FALSE], 2L, p[keep]), 2L,
                sqrt(p[keep]), "/")
    Z <- cbind(Z, Zc)
    owner <- c(owner, rep(v, sum(keep)))
  }
  sv <- svd(Z / sqrt(n), nu = 0L, nv = min(ndim, ncol(Z)))
  eig <- sv$d^2
  eig <- eig[eig > 1e-10 * max(eig)]
  k <- min(ndim, length(eig))
  V <- sv$v[, seq_len(k), drop = FALSE]
  ## A dimension's sign is arbitrary -- the SVD may return either -- so it is
  ## fixed by a rule, the same whatever computes it: the column that loads
  ## most on the dimension loads positively (as scikit-learn's svd_flip)
  big <- apply(abs(V), 2L, which.max)
  flip <- sign(V[cbind(big, seq_len(k))])
  flip[flip == 0] <- 1
  V <- sweep(V, 2L, flip, "*")
  coord <- Z %*% V
  colnames(coord) <- paste0("dim", seq_len(k))

  ## how strongly each original variable relates to each dimension, on a 0 to
  ## 1 scale: the squared correlation for a number, the correlation ratio for
  ## a category (the share of the dimension's variance its levels explain)
  vars <- unique(owner)
  sq <- t(vapply(vars, function(v) {
    vapply(seq_len(k), function(j) {
      y <- coord[, j]; tot <- sum((y - mean(y))^2)
      if (tot <= 0) return(0)
      if (!is.null(num) && v %in% colnames(num)) {
        stats::cor(num[, v], y)^2
      } else {
        f <- as.factor(quali[[v]])
        m <- tapply(y, f, mean); nl <- tabulate(f, nlevels(f))
        ok <- nl > 0
        sum(nl[ok] * (m[ok] - mean(y))^2) / tot
      }
    }, 0)
  }, numeric(k)))
  if (k == 1L) sq <- matrix(sq, ncol = 1L, dimnames = list(vars, NULL))
  colnames(sq) <- paste0("dim", seq_len(k))

  pct <- 100 * eig / sum(eig)
  list(eig = cbind(eigenvalue = eig, pct = pct, cum_pct = cumsum(pct)),
       ind = list(coord = coord), sqload = sq, loadings = V)
}
