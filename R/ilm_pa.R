## ---------------------------------------------------------------------------
## Permutation parallel analysis on the reduction ilm_reduce() computes.
##
## Each original column is permuted independently, which destroys what the
## columns share and keeps each one's own distribution; the reduction's
## eigenvalues are set against the 95th percentile of the permuted data's
## (Buja and Eyuboglu 1992; the percentile is Glorfeld's 1995 rule, in place
## of Horn's 1965 mean of normal data). A dimension is kept while it beats its
## percentile, counting from the first.
##
## A categorical column is permuted BEFORE it is encoded: its levels move
## together, so the null keeps the one-hot structure and the level shares.
## That is the same as permuting the rows of its block of encoded columns
## together, since the shares, and so the weights, do not change -- and nor
## does the fill for a missing value (a column mean, or a row of zeros), which
## depends on no other row. So the permutations are made on ilm_famd_z()'s
## matrix, with no re-encoding: one row order per original column, drawn in
## the data's column order, applied to every encoded column it owns. A date's
## encoded columns (its elapsed time and the cycles kept for it) move together
## as one column, and keep the encoding chosen on the real data.
##
## References:
##   Buja, A. and Eyuboglu, N. (1992). Remarks on parallel analysis.
##     Multivariate Behavioral Research 27(4), 509-540.
##   Glorfeld, L. W. (1995). An improvement on Horn's parallel analysis
##     methodology for selecting the correct number of factors to retain.
##     Educational and Psychological Measurement 55(3), 377-393.
##   Horn, J. L. (1965). A rationale and test for the number of factors in
##     factor analysis. Psychometrika 30, 179-185.
## ---------------------------------------------------------------------------

## The eigenvalues of Z / sqrt(n): from the p x p cross-product when the rows
## outnumber the columns, which costs n p^2 rather than an SVD of the whole
## matrix, and from the SVD otherwise.
#' @keywords internal
#' @noRd
ilm_pa_eig <- function(Z) {
  n <- nrow(Z)
  if (n >= ncol(Z)) {
    e <- eigen(crossprod(Z) / n, symmetric = TRUE, only.values = TRUE)$values
    pmax(e, 0)
  } else {
    c(svd(Z / sqrt(n), nu = 0L, nv = 0L)$d^2, numeric(ncol(Z) - n))
  }
}

## Z and `group`, the original column each of Z's columns came from; `order`,
## the original columns in the data's order, which is the order the row
## permutations are drawn in. Returns the observed eigenvalues, each one's
## permutation percentile, and how many dimensions beat theirs from the first
## (at least 1, at most `max_dim`).
#' @keywords internal
#' @noRd
ilm_pa_z <- function(Z, group, order = unique(group), B = 30L, q = 0.95,
                     max_dim = ncol(Z)) {
  n <- nrow(Z)
  stopifnot(length(group) == ncol(Z), setequal(order, unique(group)))
  idx <- lapply(order, function(g) which(group == g))
  obs <- ilm_pa_eig(Z)
  perm <- vapply(seq_len(B), function(b) {
    Zp <- Z
    for (j in idx) Zp[, j] <- Z[sample.int(n), j, drop = FALSE]
    ilm_pa_eig(Zp)
  }, numeric(length(obs)))
  if (length(obs) == 1L) perm <- matrix(perm, 1L)
  thr <- apply(perm, 1L, stats::quantile, q, names = FALSE)
  k <- min(max_dim, length(obs))
  beat <- obs[seq_len(k)] > thr[seq_len(k)]
  ndim <- if (all(beat)) k else which(!beat)[1L] - 1L
  list(eigenvalue = obs[seq_len(k)], threshold = thr[seq_len(k)],
       beats = beat, ndim = max(1L, ndim), B = B, q = q)
}

## The same, from ilm_reduce()'s prepared inputs (ilm_reduce_inputs()).
#' @keywords internal
#' @noRd
ilm_pa_inputs <- function(inp, B = 30L, q = 0.95) {
  enc <- ilm_famd_z(inp$quanti, inp$quali)
  ## an encoded date column belongs to its date
  owner <- enc$owner
  for (v in names(inp$tmap)) owner[owner %in% names(inp$tmap[[v]])] <- v
  order <- intersect(inp$cols, owner)
  ilm_pa_z(enc$Z, owner, order, B = B, q = q, max_dim = inp$max_dim)
}
