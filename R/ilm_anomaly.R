## ---------------------------------------------------------------------------
## Multivariate anomaly detection from low-rank reconstruction error.
##
## ilm_outliers() asks whether a value is extreme for its own column. This asks
## a different question: whether a ROW is a plausible combination. Someone 150
## cm tall is unremarkable, someone weighing 110 kg is unremarkable, and
## someone who is both is not. Nothing in either column's distribution says so.
##
## The method: most of the variation in a set of correlated columns lies in a
## few directions. Fit those directions, project each row onto them, and
## measure what is left over. A row that respects the correlations is
## reconstructed from k numbers; one that does not is not.
##
## Three things had to be measured rather than assumed, and two of them
## overturned the first design.
##
## THE RANK IS NOT THE IMPUTATION RANK. ilm_impute() chooses it by
## cross-validating held-out cells, which is the right question for filling a
## value in and the wrong one here. On a rank-2 structure in eight columns that
## chose 6 or 7 -- on clean data as well as contaminated -- and the extra
## components spanned the very directions the anomalies departed along.
## Detection fell from 0.975 to 0.560. Parallel analysis chose 2 every time.
##
## THE FIT IS TRIMMED, NOT HELD OUT. The first design scored every row against
## a fit built from other folds of rows, on the reasoning that an anomaly
## inside the fit bends the directions towards itself. The reasoning is sound
## and that remedy did nothing: measured at four contamination levels the two
## agreed to three decimals, because four fifths of the anomalies are still in
## each training fold. Fitting on the rows with the smallest scores and
## iterating does work, because it removes them from the fit rather than a
## fifth of them.
##
## THE REFERENCE IS SIMULATED AND CALIBRATED. The score is not chi-squared: the
## noise scale is estimated, the rank was chosen from the same data, and the
## residual is taken against an estimated subspace. Simulating from the fitted
## structure needs the noise PER COLUMN -- standardising gives every column
## unit variance, so one that loads weakly on the shared directions keeps
## proportionally more of its variance off them -- and needs its level matched
## to the observed median rather than estimated from shrunken in-sample
## residuals. Getting either wrong flagged 38.9% of the rows of data containing
## no anomalies at all.
##
## References:
##   Hawkins, D. M. (1974). The detection of errors in multivariate data using
##     principal components. JASA 69, 340-344.
##   Horn, J. L. (1965). A rationale and test for the number of factors in
##     factor analysis. Psychometrika 30, 179-185.
##   Hubert, M., Rousseeuw, P. J. and Vanden Branden, K. (2005). ROBPCA: a new
##     approach to robust principal component analysis. Technometrics 47, 64-79.
## ---------------------------------------------------------------------------

#' How many directions are real structure?
#'
#' Horn's parallel analysis. Each column is permuted independently, which
#' destroys everything the columns share while leaving each one's own
#' distribution alone, and a component is kept when it beats what the permuted
#' data produces.
#'
#' @keywords internal
#' @noRd
ilm_anom_rank <- function(Z, B = 30L, q = 0.95) {
  dobs <- svd(Z, nu = 0L, nv = 0L)$d
  perm <- vapply(seq_len(B), function(b)
    svd(apply(Z, 2L, sample), nu = 0L, nv = 0L)$d, numeric(length(dobs)))
  thr <- apply(perm, 1L, stats::quantile, q)
  max(1L, min(sum(dobs > thr), ncol(Z) - 1L))
}


## Which column made each row odd, for the isolation forest. A column's share
## of a row's score is how far the score falls when the column is replaced by
## the value the other columns predict for it: a number by a linear
## regression on them, a category by the commonest level among the row's 10
## nearest rows in Gower distance on them. The driver is the column whose
## replacement lowers the score most.
##
## dev/studies/driver_redesign.R measured it against replacing each column
## by its median or commonest level, the earlier rule, on planted anomalies
## (50 replicates): a row whose category contradicts its numbers was
## attributed to that category for 0.34 to 0.38 of such rows, against 0.17
## to 0.25; rows pushed out of a column's range and rare pairings of
## categories were attributed as well or better (0.85 to 0.99, 0.90 to
## 0.95); at about half as much time again. Replacing a category by its
## commonest level rarely made such a row look normal, since the commonest
## level usually contradicts the numbers too.
##
## The neighbours are searched among at most `pool_max` rows -- all of them
## below that, a fixed draw under the scan's seed above it -- in blocks, so
## the cost grows with the rows rather than their square. At 10,000 rows a
## pool of 2,000 gave the same driver as searching every row for 0.998 to
## 1.000 of flagged rows, and even 500 for 0.997 to 0.999
## (dev/studies/driver_pool.R, 20 replicates); 2,000 took 40% of the time.
#' @keywords internal
#' @noRd
ilm_iforest_drivers <- function(fit, d, score, seed, pool_max = 2000L, k = 10L) {
  n <- nrow(d)
  if (!is.null(seed)) set.seed(seed)
  pool <- if (n <= pool_max) seq_len(n) else sort(sample.int(n, pool_max))
  M <- vapply(names(d), function(v) {
    others <- setdiff(names(d), v)
    x <- d[[v]]
    rep_col <- if (is.numeric(x)) ilm_iforest_predict_num(x, d[others])
               else ilm_iforest_predict_cat(x, d[others], pool, k)
    dd <- d; dd[[v]] <- rep_col
    score - as.numeric(stats::predict(fit, dd))
  }, numeric(n))
  if (n == 1L) M <- matrix(M, 1L, dimnames = list(NULL, names(d)))
  colnames(M) <- names(d)
  M
}

## a number predicted from the other columns by a linear regression; a row
## the regression cannot predict (a gap among its predictors) keeps its value
#' @keywords internal
#' @noRd
ilm_iforest_predict_num <- function(x, others) {
  ## a text column with a different value in every row is an identifier, and
  ## would give the regression one level per row
  keep <- vapply(others, function(z)
    !(is.character(z) || is.factor(z)) || length(unique(z)) < length(z), TRUE)
  mf <- data.frame(y = x, others[keep])
  pr <- tryCatch(stats::predict(stats::lm(y ~ ., data = mf, na.action = stats::na.exclude),
                                newdata = mf),
                 error = function(e) rep(NA_real_, length(x)))
  pr <- as.numeric(pr)
  pr[is.na(pr)] <- x[is.na(pr)]
  pr
}

## a category predicted as the commonest level among the k nearest rows of
## the pool, in Gower distance on the other columns (a number's absolute
## difference over its range, a category's mismatch, averaged over the
## columns both rows have); returned in the column's own class
#' @keywords internal
#' @noRd
ilm_iforest_predict_cat <- function(x, others, pool, k = 10L, block = 1000L) {
  n <- length(x)
  xs <- as.character(x)
  cols <- lapply(others, function(z) {
    if (is.numeric(z)) {
      r <- diff(range(z, na.rm = TRUE)); if (!is.finite(r) || r == 0) r <- 1
      list(num = TRUE, v = z / r)
    } else list(num = FALSE, v = as.character(z))
  })
  lv <- character(n)
  for (b0 in seq(1L, n, by = block)) {
    at <- b0:min(n, b0 + block - 1L)
    S <- matrix(0, length(at), length(pool)); C <- S
    for (cl in cols) {
      a <- cl$v[at]; p <- cl$v[pool]
      dd <- if (cl$num) abs(outer(a, p, "-")) else outer(a, p, "!=") * 1
      ok <- !is.na(dd)
      S[ok] <- S[ok] + dd[ok]; C <- C + ok
    }
    D <- S / pmax(C, 1); D[C == 0] <- Inf
    D[outer(at, pool, "==")] <- Inf                       # never the row itself
    lv[at] <- vapply(seq_along(at), function(i) {
      nb <- pool[order(D[i, ])[seq_len(min(k, length(pool)))]]
      tb <- table(xs[nb])
      if (!length(tb)) xs[at[i]] else names(tb)[which.max(tb)]
    }, "")
  }
  ## the same class as the column: an existing element of it with that level
  out <- x[match(lv, xs)]
  out[is.na(out)] <- x[is.na(out)]
  out
}

#' Score rows against a trimmed low-rank fit
#'
#' The directions are estimated from the rows that fit them best, so a row far
#' off the structure is scored against a structure it did not help define.
#' Every row is then scored, including the trimmed ones.
#'
#' @param Z Standardised numeric matrix with no missing values.
#' @param k Rank.
#' @param trim Share of rows excluded from the fit at each pass; `0` fits all.
#' @param iter Refitting passes.
#' @return A list with the per-row score, the per-cell residuals and the basis.
#' @keywords internal
#' @noRd
ilm_anom_score <- function(Z, k, trim = 0.25, iter = 3L) {
  n <- nrow(Z)
  keep <- seq_len(n)
  nkeep <- max(k + 2L, floor(n * (1 - trim)))
  reps <- if (trim > 0 && nkeep < n) iter else 1L
  sc <- numeric(n); R <- Z; V <- NULL
  for (it in seq_len(reps)) {
    sv <- svd(Z[keep, , drop = FALSE], nu = 0L,
              nv = min(k, length(keep) - 1L))
    V <- sv$v[, seq_len(min(k, ncol(sv$v))), drop = FALSE]
    R <- Z - Z %*% V %*% t(V)
    sc <- rowSums(R^2)
    keep <- order(sc)[seq_len(nkeep)]
  }
  list(score = sc, residual = R, V = V)
}

## Isolation forest: the route for anomalies that live in the CATEGORIES.
##
## WHY IT EXISTS: the reconstruction is a projection, so a category has no
## residual along a direction and the columns are dropped. Against planted
## anomalies that costs nothing on the cases it is built for (AUC 1.000 for an
## extreme value, 0.999 for a jointly-implausible numeric combination) and
## everything on the cases it is not: 0.769 when a category contradicts the
## numbers, 0.505 -- a coin toss -- for a category pairing that never
## otherwise occurs. An isolation forest splits on factors directly, needs no
## embedding and no distance matrix, and scored 0.964 and 0.886 on those two
## while giving up little elsewhere (0.975, 0.950).
##
## `ndim = 1` deliberately. The extended form with oblique splits is better on
## numeric combinations (0.965 against 0.950) and WORSE on rare category
## pairings (0.796 against 0.886) -- and the categories are the reason this is
## here at all.
##
## What it cannot do is the calibrated null. The reconstruction simulates its
## own reference and reports FDR-adjusted p-values; a forest returns a score
## with no null attached, so `p` is NA here and `flag` comes from the
## contamination rate the user is prepared to assume. That is a real
## difference in kind, not a detail, and the print method says so.
#' @keywords internal
#' @noRd
ilm_anomaly_iforest <- function(data, sel, ntrees, alpha, seed,
                                keep_data = NULL) {
  if (!requireNamespace("isotree", quietly = TRUE))
    stop("method = \"iforest\" needs the isotree package. Install it with ",
         'install.packages("isotree"), or use the default ',
         'method = "reconstruction", which needs nothing beyond illumex but ',
         "uses the numeric columns only.", call. = FALSE)
  ## the forest takes every column, by its name
  ilm_stop_not_utf8_names(sel, "ilm_anomaly")
  d <- data[sel]
  if (ncol(d) < 2L)
    stop("at least 2 columns are needed to isolate a row against.", call. = FALSE)
  keep <- rep(TRUE, nrow(d))                  # a forest tolerates gaps natively
  fit <- isotree::isolation.forest(d, ntrees = as.integer(ntrees), ndim = 1L,
                                   seed = seed, nthreads = 1L)
  score <- as.numeric(stats::predict(fit, d))

  ## WHICH column made the row odd: each column replaced by the value the
  ## other columns predict for it, and the column whose replacement lowers
  ## the row's score the most is the driver (ilm_iforest_drivers()).
  drop_mat <- ilm_iforest_drivers(fit, d, score, seed)
  drv <- colnames(drop_mat)[max.col(drop_mat, ties.method = "first")]

  ## No null, so no p-value. `alpha` is read as the share of rows the user is
  ## willing to call anomalous, which is what a forest's score can support.
  cut <- stats::quantile(score, 1 - alpha, na.rm = TRUE)
  out <- data.frame(row = which(keep), score = score, p = NA_real_,
                    p_adj = NA_real_, flag = score > cut, driver = drv,
                    stringsAsFactors = FALSE, row.names = NULL)
  ## most anomalous first, as the reconstruction returns them -- the print
  ## method shows the head, so an unsorted result would show whichever rows
  ## happened to come first and none of the ones that matter
  out <- out[order(-out$score), , drop = FALSE]
  row.names(out) <- NULL
  structure(out, class = c("ilm_anomaly", "data.frame"), method = "iforest",
            data = keep_data,
            ntrees = as.integer(ntrees), alpha = alpha, columns = sel,
            rank = NA_integer_, trim = NA_real_, n_null = 0L,
            dropped = character(0), calibrated = FALSE)
}

#' Rows that do not fit the pattern the other rows make
#'
#' Finds observations that are implausible as a **combination** of values, even
#' when no single value is extreme. [ilm_outliers()] asks whether a number is
#' far out in its own column; this asks whether a row is far from the structure
#' the columns share.
#'
#' A set of correlated columns puts most of its variation in a few directions.
#' Those directions are estimated, each row is projected onto them, and the
#' score is what is left over.
#'
#' @section How many directions:
#'
#' By parallel analysis, and deliberately not by the cross-validation
#' `illume::ilm_impute()` uses on the same decomposition, because the two answer
#' different questions. Imputation wants the rank that best predicts a missing
#' cell. This wants the number of directions that are real shared structure, so
#' that what is left over is residual rather than signal it failed to fit.
#'
#' The difference is large. On a rank-2 structure in eight columns,
#' cross-validation chose 6 or 7 -- on clean data as well as contaminated --
#' and those extra components span the directions the anomalies depart along.
#' Detection fell from 0.975 to 0.560. Parallel analysis chose 2 every time.
#'
#' @section Why the fit is trimmed:
#'
#' The anomalies sit in the same data the directions are estimated from, so
#' they pull the directions towards themselves and are then reconstructed well.
#' `trim` excludes the worst-fitting rows from the fit and refits, so a row is
#' scored against a structure it did not help define. Every row is still
#' scored, the trimmed ones included.
#'
#' It earns its place only where the anomalies share a direction, which is
#' where they can form a component between them. Detection with `trim = 0`
#' against the default 0.25, over 12 datasets each:
#'
#' ```
#'   anomalies along random directions      along ONE shared direction
#'    2%   0.925  vs  0.917                  2%   0.950  vs  0.942
#'    5%   0.967  vs  0.977                  5%   0.797  vs  0.887
#'   10%   0.970  vs  0.978                 10%   0.587  vs  0.670
#' ```
#'
#' Holding rows out in folds instead, which was the first design here, does not
#' work and is not offered: measured at four contamination levels it matched
#' in-sample scoring to three decimals, because most of the anomalies remain in
#' every training fold.
#'
#' @section What it cannot do:
#'
#' When a large share of rows depart along the **same** direction they are not
#' anomalies, they are a subpopulation, and a rank-k fit of the whole data
#' legitimately includes their direction. Detection degrades accordingly: with
#' anomalies sharing one direction, 0.89 of them were found at 5%
#' contamination, 0.67 at 10% and 0.41 at 20%. That is the method reaching its
#' limit rather than failing quietly, and [ilm_cluster()] is the tool for a
#' second group, since finding one is what it is for.
#'
#' @section What the score is compared with:
#'
#' Not a chi-squared distribution. Datasets with the same structure and no
#' anomalies are simulated and scored the same way. `B` can be modest because
#' each simulated dataset contributes `n` null scores rather than one.
#'
#' Measured on 100 matrices of 400 by 8 with a rank-2 structure and **no
#' anomalies at all**: 11 rows of 40,000 were flagged, a rate of 0.00028, with
#' 95 of the 100 datasets producing none. With anomalies present, the rows that
#' were not planted were flagged at 0.0015 to 0.0030 across every design tried
#' above.
#'
#' `flag` uses `p_adj` and is what those numbers describe. The raw `p` runs a
#' little hot -- about 0.064 of clean rows fall below 0.05 rather than 0.05 of
#' them -- so read it as a ranking rather than as a test.
#'
#' @section Size:
#' On one core of a 16 GB Windows machine (`dev/studies/scale_check.R` in the source) the
#' default method took 32 seconds at 10,000 rows and 13 minutes at 50,000 --
#' the time grows about as the square of the rows -- and over 15 minutes at
#' 100,000. `method = "iforest"` took 3.5 minutes at 10,000 rows and over 15
#' at 50,000.
#'
#' @param data A data frame or matrix.
#' @inheritParams ilm_reduce
#' @param cols Columns to use; see [ilm_selection]. Under the default method,
#'   numeric columns only -- the reconstruction is a projection, and a category
#'   has no residual along a direction. Anything else is dropped with a note.
#'   `method = "iforest"` uses every column.
#' @param method `"reconstruction"` (the default) or `"iforest"`. The
#'   reconstruction is unbeaten at what it is for -- against planted anomalies
#'   it scores 1.000 for an extreme value and 0.999 for a jointly-implausible
#'   numeric combination -- and it is the only one here with a calibrated null,
#'   so it reports FDR-adjusted p-values. It is also blind to the categories:
#'   0.771 when a category contradicts the numbers, and 0.505, a coin toss, for
#'   a category pairing that never otherwise occurs. An isolation forest splits
#'   on factors directly and reaches 0.964 and 0.886 on those two, giving up
#'   little elsewhere (0.975, 0.950) -- but it returns a score with no null
#'   behind it, so `p` and `p_adj` are `NA` and `alpha` becomes the share of
#'   rows you are calling anomalous rather than an error rate being controlled.
#'   Needs the isotree package.
#' @param ntrees Trees in the isolation forest. Ignored by the default method.
#' @param rank Number of directions; `NULL` uses parallel analysis.
#' @param trim Share of the worst-fitting rows held out of the fit. `0` fits
#'   every row, which lets the anomalies define the structure they are scored
#'   against.
#' @param B Simulated null datasets for the reference. A Benjamini-Hochberg
#'   adjusted p-value cannot fall much below `1 / B`, so a single anomaly among
#'   many rows needs `B` comfortably above `1 / alpha`.
#' @param alpha Flagging level, after a Benjamini-Hochberg adjustment across
#'   rows -- one row in twenty at 0.05 would be 50 rows in a thousand, which is
#'   a list nobody reads.
#' @param seed Random seed.
#' @param keep_data Keep the scanned data on the result, so that
#'   [ilm_anomalous()], [ilm_profile()], [ilm_cluster()] and the describe
#'   functions can take the object directly. Set `FALSE` if the frame is
#'   large and you only want the scores.
#' @param progress Show a progress bar; see [ilm_progress_arg].
#' @return A data frame with one row per observation: `row`, `score`, `p`,
#'   `p_adj`, `flag`, and `driver`, the column contributing most to the score.
#'   The rank and the residual matrix are attributes. Under the default
#'   method, `driver` is the column with the largest residual off the fitted
#'   structure, and so the largest share of the score; how often that names
#'   the column that made a row odd has not been measured against planted
#'   anomalies. Under `method = "iforest"`, a column's share of a row's score
#'   is how far the score falls when the column is replaced by the value the
#'   other columns predict for it, and `driver` is the column whose
#'   replacement lowers it most. Against planted anomalies the forest's
#'   driver named the changed column for 0.85 to 0.99 of rows pushed out of a
#'   column's range and 0.90 to 0.95 of rows pairing categories that never
#'   otherwise occur, but for only 0.34 to 0.38 of rows whose category
#'   contradicts their numbers (`dev/studies/driver_redesign.R` in the
#'   source). Above 2,000 rows a category's nearest rows are searched among
#'   2,000 drawn under `seed`, which gave the same driver as searching every
#'   row for 0.998 to 1.000 of flagged rows at 10,000 rows
#'   (`dev/studies/driver_pool.R`). Under either method, read `driver` as
#'   where to look first, not as the cause.
#' @seealso [ilm_outliers()] for the one-column-at-a-time question,
#'   [ilm_cluster()] when the unusual rows turn out to be a group,
#'   `illume::ilm_impute()` which fits the same decomposition to fill values in.
#' @references Hawkins, D. M. (1974). The detection of errors in multivariate
#'   data using principal components. *Journal of the American Statistical
#'   Association* 69, 340-344.
#'
#'   Horn, J. L. (1965). A rationale and test for the number of factors in
#'   factor analysis. *Psychometrika* 30, 179-185.
#' @examples
#' set.seed(1)
#' n <- 300
#' f <- rnorm(n)
#' d <- data.frame(a = f + rnorm(n, 0, .3), b = 2 * f + rnorm(n, 0, .3),
#'                 c = -f + rnorm(n, 0, .3))
#' ## a row that is ordinary in every column but breaks the pattern
#' d[1, ] <- c(1.2, -2.4, 1.2)
#' head(ilm_anomaly(d), 3)
#' @export
ilm_anomaly <- function(data, cols = NULL, method = c("reconstruction", "iforest"),
                        rank = NULL, trim = 0.25, ntrees = 500L,
                        B = 39L, alpha = 0.05, seed = 1L, keep_data = TRUE,
                        progress = NULL, cols_negate = FALSE) {
  ilm_rng_restore(seed)                  # the user's random stream, put back on exit
  method <- match.arg(method)
  ## The scan says WHICH rows are odd; the next question is always whether
  ## they are alike, and answering it means taking them back to the
  ## exploration tools. Keeping the frame here is what lets ilm_profile() and
  ## friends accept this object directly instead of the user reconstructing
  ## the subset from `row` and risking lining the wrong rows up.
  .keep <- if (isTRUE(keep_data)) data else NULL
  if (is.matrix(data)) data <- as.data.frame(data)
  if (!is.data.frame(data))
    stop("`data` must be a data frame or matrix, not ", class(data)[1],
         call. = FALSE)
  if (!is.numeric(trim) || length(trim) != 1L || trim < 0 || trim >= 0.5)
    stop("`trim` must be a single number in [0, 0.5): it is the share of rows ",
         "held out of the FIT, and trimming half of them leaves the structure ",
         "defined by whichever half happened to fit first.", call. = FALSE)
  sel <- ilm_resolve_cols(data, cols, negate = cols_negate)
  if (method == "iforest")
    return(ilm_seed_mark(ilm_anomaly_iforest(data, sel, ntrees, alpha, seed, .keep), seed))
  num <- sel[vapply(sel, function(v) is.numeric(data[[v]]), TRUE)]
  drop <- setdiff(sel, num)
  if (length(drop))
    ## Measured, so the numbers are the package's own: against planted
    ## anomalies the reconstruction scores 1.000 and 0.999 on extreme values
    ## and on jointly-implausible numeric combinations -- what it is for -- and
    ## 0.769 and 0.505 when the anomaly is a category contradicting the
    ## numbers, or a category pairing that never otherwise occurs. 0.505 is a
    ## coin toss. method = "iforest" reaches 0.964 and 0.886 on those two.
    message("ilm_anomaly(): using the numeric columns only; a reconstruction ",
            "is a projection and a category has no residual along a ",
            "direction. Dropped: ", paste(drop, collapse = ", "),
            ". For anomalies that LIVE in the categories, method = \"iforest\" ",
            "uses every column; ilm_reduce() is the other route.")
  ilm_cluster_na_note(data[sel], "ilm_anomaly")
  if (length(num) < 3L)
    stop("at least 3 numeric columns are needed: with two there is one ",
         "direction to fit and one left over, which is a scatterplot rather ",
         "than a multivariate question.", call. = FALSE)

  X <- as.matrix(data[num])
  keep <- stats::complete.cases(X)
  if (sum(keep) < 20L)
    stop("only ", sum(keep), " complete rows across these columns. Fill them ",
         "in with ilm_impute() first, or choose columns with fewer gaps -- ",
         "scoring a row against a structure fitted to a fifth of the data is ",
         "not worth doing.", call. = FALSE)
  Xc <- X[keep, , drop = FALSE]
  n <- nrow(Xc); p <- ncol(Xc)
  if (B * alpha <= 1)
    warning("B = ", B, " puts the smallest achievable adjusted p-value at ",
            "about ", signif(1 / B, 2), ", which is not below alpha = ", alpha,
            ", so a lone anomaly among many rows cannot be flagged however ",
            "extreme it is. Use B >= ", ceiling(2 / alpha), ".", call. = FALSE)

  ctr <- colMeans(Xc)
  scl <- apply(Xc, 2L, stats::sd)
  scl[!is.finite(scl) | scl <= 0] <- 1
  Z <- sweep(sweep(Xc, 2L, ctr, "-"), 2L, scl, "/")

  if (!is.null(seed)) set.seed(seed)
  k <- if (is.null(rank)) ilm_anom_rank(Z) else as.integer(rank)
  k <- max(1L, min(k, p - 1L))
  obs <- ilm_anom_score(Z, k, trim)
  ref <- ilm_anom_null(Z, k, trim, obs$score, B, progress)
  null <- ref$null

  pv <- (1 + vapply(obs$score, function(s) sum(null >= s), 0L)) /
    (length(null) + 1)
  padj <- stats::p.adjust(pv, "BH")
  drv <- colnames(Z)[max.col(abs(obs$residual), ties.method = "first")]

  out <- data.frame(row = which(keep), score = obs$score, p = pv,
                    p_adj = padj, flag = padj < alpha, driver = drv,
                    stringsAsFactors = FALSE, row.names = NULL)
  out <- out[order(-out$score), , drop = FALSE]
  ilm_seed_mark(structure(out, class = c("ilm_anomaly", "data.frame"), rank = k,
            data = .keep,
            method = "reconstruction", calibrated = TRUE,
            null_curve = ref$curve,
            residual = obs$residual, columns = num, trim = trim,
            n_null = length(null), alpha = alpha, dropped = drop), seed)
}

## The null: the same structure, the same noise, no anomalies. Returns the
## pooled null scores and the pointwise envelope of the sorted score curve.
#' @keywords internal
#' @noRd
ilm_anom_null <- function(Z, k, trim, score, B, progress) {
  n <- nrow(Z); p <- ncol(Z)
  sv <- svd(Z, nu = k, nv = k)
  V <- sv$v[, seq_len(k), drop = FALSE]
  d <- sv$d[seq_len(k)]
  Rin <- Z - Z %*% V %*% t(V)
  dfc <- (n * p) / max(n * p - (n + p) * k, 1)
  rvar <- apply(Rin, 2L, function(z) stats::mad(z)^2) * dfc
  rvar[!is.finite(rvar) | rvar <= 0] <- .Machine$double.eps

  sim <- function(rv, reps) {
    unlist(lapply(seq_len(reps), function(b) {
      U <- matrix(stats::rnorm(n * k), n, k)
      E <- vapply(seq_len(p),
                  function(j) stats::rnorm(n, 0, sqrt(rv[j])), numeric(n))
      ## standardised and scored exactly as the observed matrix was, so the
      ## two are the same pipeline and not merely similar ones
      ilm_anom_score(scale(U %*% diag(d / sqrt(n), k, k) %*% t(V) + E),
                     k, trim)$score
    }))
  }
  pb <- ilm_progress(B + 3L, progress, "simulating the reference")
  cal <- sim(rvar, 3L); pb$tick(3L)
  mo <- stats::median(score); mc <- stats::median(cal)
  if (is.finite(mc) && mc > 0) rvar <- rvar * (mo / mc)
  ## Kept per simulated dataset rather than pooled, because the plot needs
  ## to know what score to EXPECT at each rank, not just the pooled null.
  nullm <- matrix(NA_real_, n, B)
  for (b in seq_len(B)) { nullm[, b] <- sim(rvar, 1L); pb$tick(3L + b) }
  pb$done()
  null <- as.vector(nullm)

  ## Each null dataset sorted descending is the score curve you would see at
  ## each rank if nothing were anomalous; the spread across datasets is a
  ## pointwise envelope. Three quantiles of it cost n x 3 to store instead
  ## of n x B, and are what ilm_plot_anomaly() draws the reference band from.
  srt <- apply(nullm, 2L, sort, decreasing = TRUE)
  if (!is.matrix(srt)) srt <- matrix(srt, n, B)
  nullq <- t(apply(srt, 1L, stats::quantile, probs = c(0.025, 0.5, 0.975),
                   names = FALSE, na.rm = TRUE))
  colnames(nullq) <- c("lo", "mid", "hi")
  list(null = null, curve = nullq)
}

#' @export
print.ilm_anomaly <- function(x, n = 10L, ...) {
  d <- as.data.frame(x)
  class(d) <- "data.frame"
  nf <- sum(d$flag)
  iforest <- identical(attr(x, "method"), "iforest")
  cat(sprintf("Multivariate anomalies: %d of %d rows flagged at %.3g\n",
              nf, nrow(d), attr(x, "alpha")))
  if (iforest) {
    cat(sprintf("  isolation forest, %d trees over %d columns, categories included\n",
                attr(x, "ntrees"), length(attr(x, "columns"))))
    ## Said plainly, because the two methods differ in KIND and the columns
    ## look the same either way: a forest returns a score with no null behind
    ## it, so there is no p-value to adjust and `alpha` is the share of rows
    ## being called anomalous rather than an error rate being controlled.
    cat("  No null distribution, so `p` and `p_adj` are NA: a forest scores\n",
        "  rows but does not say how surprising a score is. `alpha` is the\n",
        "  share of rows you are calling anomalous, NOT a false-discovery\n",
        "  rate. For calibrated p-values use the default reconstruction,\n",
        "  which simulates its own reference -- at the cost of the\n",
        "  categorical columns.\n", sep = "")
  } else {
    cat(sprintf("  rank %d over %d columns, %s, %d null scores\n",
                attr(x, "rank"), length(attr(x, "columns")),
                if (attr(x, "trim") > 0)
                  sprintf("fitted on the best %.0f%% of rows",
                          100 * (1 - attr(x, "trim")))
                else "fitted on every row",
                attr(x, "n_null")))
    if (attr(x, "trim") <= 0)
      cat("  With trim = 0 the anomalous rows help define the structure they\n",
          "  are then scored against, and hide themselves in it.\n", sep = "")
  }
  show <- utils::head(d, max(n, nf))
  show$score <- round(show$score, 3)
  show$p <- signif(show$p, 3); show$p_adj <- signif(show$p_adj, 3)
  print(show, row.names = FALSE)
  if (nrow(d) > nrow(show))
    cat(sprintf("  ... %d more rows\n", nrow(d) - nrow(show)))
  if (nf)
    cat("\n  `driver` is the column contributing most to each row's score.\n",
        "  A flagged row is a combination the other rows do not make. It is\n",
        "  not necessarily an error, and deleting it because a method said so\n",
        "  is how real effects get removed.\n", sep = "")
  invisible(x)
}
