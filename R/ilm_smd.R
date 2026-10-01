## ---------------------------------------------------------------------------
## Standardised mean differences between groups, as a descriptive table
## reports them: each group against a reference group, one number per
## variable.
##
## Every kind divides by the average of the two groups' variances (Austin
## 2009), the convention tableone follows:
##   a number     the difference in means over sqrt((s1^2 + s0^2) / 2); a
##                date as a number
##   a binary     the difference in proportions over
##                sqrt((p1 (1 - p1) + p0 (1 - p0)) / 2), for the second of
##                its two levels (TRUE for a logical)
##   a category   of more than two levels, Yang and Dalton's (2012)
##                multivariate difference sqrt(T' S^-1 T), T the difference in
##                the proportions of every level but the first and S the
##                average of the two groups' multinomial covariance matrices
##                (p_i (1 - p_i) on the diagonal, -p_i p_j off it); with two
##                levels it is the binary one, unsigned
## A number's and a binary's difference keep their sign (group minus
## reference); a category's has none. Levels seen in neither group are left
## out, and a singular S is inverted through its singular value
## decomposition. Missing values are left out group by group.
##
## Austin, P. C. (2009). Balance diagnostics for comparing the distribution of
## baseline covariates between treatment groups in propensity-score matched
## samples. Statistics in Medicine, 28(25), 3083-3107.
## Yang, D. and Dalton, J. E. (2012). A unified approach to measuring the
## effect size between two groups using SAS. SAS Global Forum 2012, paper
## 335-2012.
## ---------------------------------------------------------------------------

## one variable's differences, for every group against `ref`; NA for the
## reference itself and for a group with nothing to compare
#' @keywords internal
#' @noRd
ilm_smd <- function(v, g, ref) {
  g <- as.character(g)
  groups <- unique(g[!is.na(g)])
  out <- stats::setNames(rep(NA_real_, length(groups)), groups)
  if (ref %in% groups) {
    r <- v[g %in% ref & !is.na(v)]
    for (k in setdiff(groups, ref)) {
      out[[k]] <- ilm_smd_pair(v[g %in% k & !is.na(v)], r)
    }
  }
  out
}

## one group (x1) against the reference (x0)
#' @keywords internal
#' @noRd
ilm_smd_pair <- function(x1, x0) {
  if (!length(x1) || !length(x0)) return(NA_real_)
  if (is.numeric(x1) && !is.logical(x1)) {
    s <- sqrt((stats::var(x1) + stats::var(x0)) / 2)
    if (!is.finite(s) || s <= 0) return(NA_real_)
    return((mean(x1) - mean(x0)) / s)
  }
  lev <- if (is.factor(x1)) levels(x1) else if (is.logical(x1)) c("FALSE", "TRUE")
         else sort(unique(c(as.character(x1), as.character(x0))), method = "radix")
  p1 <- tabulate(match(as.character(x1), lev), length(lev)) / length(x1)
  p0 <- tabulate(match(as.character(x0), lev), length(lev)) / length(x0)
  ilm_smd_props(p1, p0, lev)
}

## the same from two groups' shares of each level, in the levels' order
#' @keywords internal
#' @noRd
ilm_smd_props <- function(p1, p0, lev) {
  seen <- p1 > 0 | p0 > 0
  p1 <- p1[seen]; p0 <- p0[seen]; lev <- lev[seen]
  if (length(p1) < 2L) return(NA_real_)
  if (length(p1) == 2L) {
    ## the second level's share, as a binary's TRUE
    s <- sqrt((p1[2] * (1 - p1[2]) + p0[2] * (1 - p0[2])) / 2)
    if (s <= 0) return(NA_real_)
    return((p1[2] - p0[2]) / s)
  }
  d <- (p1 - p0)[-1L]
  cv <- function(p) { p <- p[-1L]; m <- -tcrossprod(p); diag(m) <- p * (1 - p); m }
  S <- (cv(p1) + cv(p0)) / 2
  sv <- svd(S)
  keep <- sv$d > max(sv$d) * 1e-12
  Sinv <- sv$v[, keep, drop = FALSE] %*% (t(sv$u[, keep, drop = FALSE]) / sv$d[keep])
  sqrt(max(0, drop(t(d) %*% Sinv %*% d)))
}
