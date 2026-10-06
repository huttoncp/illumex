## ---------------------------------------------------------------------------
## Dimension reduction over mixed column types.
##
## PCA when every column is numeric, MCA when every column is categorical, and
## a mixed method when both are present -- chosen from the data rather than
## asked for, because the choice is forced by the column types and making the
## user name it only invites getting it wrong.
##
## All three are Pages' factor analysis of mixed data, which reduces to PCA
## and MCA when one kind of column is absent, computed by ilm_famd() in this
## package (R/ilm_famd.R). It replaced PCAmixdata::PCAmix() after agreeing with
## it and with FactoMineR::FAMD() to 4e-12 on eigenvalues, coordinates and
## loadings, and running faster from 2,000 rows up; their outputs are stored
## in tests/testthat/fixtures and the agreement is tested there and reported
## on the package site. Nothing needs to be installed for it.
## ---------------------------------------------------------------------------

#' Reduce a data frame's variables to a few dimensions
#'
#' Takes the columns of a data frame and summarises them as a small number of
#' dimensions. Which method that means is decided by the column types: PCA when
#' they are all numeric, multiple correspondence analysis when they are all
#' categorical, and a mixed method when both are present. A date or date-time
#' column is used as the time elapsed since its earliest value, which keeps its
#' order, and by the rhythms in it that the other columns follow; see `time`.
#'
#' The mixed method is Chavent et al.'s, which belongs to the same
#' generalised-PCA family as the FAMD of \enc{Pagès}{Pages} without being a
#' reimplementation of it. The two were checked against each other directly and
#' agree for this purpose, matching on eigenvalues and on individual
#' coordinates.
#'
#' @section Size:
#' The default method is closed-form: on one core of a 16 GB Windows machine
#' ([`dev/studies/scale_check.R`](https://github.com/huttoncp/illumex/blob/main/dev/studies/scale_check.R)) it took under a second at 250,000 rows.
#' `method = "glrm"` fits iteratively and first chooses its penalty from 18
#' held-out fits: 30 seconds at 1,000 rows, 5 minutes at 10,000 and over 15
#' minutes at 50,000. Giving `lambda` (passed to [ilm_glrm()]) saves the
#' search.
#'
#' @param data A data frame.
#' @param cols Columns to use. A character vector of names, a
#'   regular expression, a predicate function such as `is.numeric`, or
#'   `NULL` for all of them -- see [ilm_selection].
#' @param cols_negate If `TRUE`, `cols` names the columns to leave out, and every
#'   other eligible column is used; see [ilm_selection]. It needs `cols`.
#' @param cols_fixed If `TRUE`, a `cols` string read as a pattern is matched
#'   literally, as a substring (as `grepl(fixed = TRUE)` does); a column name
#'   still wins. It changes nothing when `cols` is names or a predicate.
#' @param subset Which rows to use, before anything else: a logical vector
#'   (one value per row; `NA` is left out), row positions, named patterns
#'   (`c(site = "^north")`), or [ilm_sample()]. See [ilm_selection]. Results
#'   keep the data's own row numbers.
#' @param subset_negate If `TRUE`, the rows `subset` would not take: the other
#'   rows, or the rows not sampled (a holdout). With a logical `subset`, rows
#'   where it is `NA` stay out either way.
#' @param subset_fixed If `TRUE`, `subset`'s patterns are matched literally,
#'   as substrings. It changes nothing for a logical, positions or a sample.
#' @param method `"famd"` (the default) for PCA, MCA or FAMD depending on
#'   the column types, in closed form; `"pcamix"`, its name from when
#'   PCAmixdata computed it, is still accepted and means the same. `"glrm"` fits a
#'   generalised low rank model instead, which uses a loss appropriate to each column's type rather
#'   than squared error on one-hot indicators, and reconstructs a category as a
#'   category. It costs an iterative fit, and on all-numeric data the two are
#'   the same model -- see [ilm_glrm()] for when it is worth that.
#' @param time What to do with date and date-time columns. `"cycles"`, the
#'   default, uses each as the time since its earliest value -- its order and
#'   spacing, in one column -- and adds the time of day, the day of the week,
#'   the day of the month and the time of year, each as a sine and cosine so
#'   that the ends of the cycle meet, but only the cycles some other column
#'   varies with, and only where the data cover two full cycles. A cycle
#'   nothing else follows is noise to a clustering: on two known clusters,
#'   every cycle given unasked took recovery from 0.38 to 0.10 where the date
#'   meant nothing, while the tested ones left it at 0.36 there and, where a
#'   rhythm was real, raised it from 0.36 to between 0.58 (month-end) and
#'   0.94 (winter against summer). The test looks at no more than 5,000 rows,
#'   so its cost is bounded however large the data. `"elapsed"` is the time since the
#'   earliest value alone -- it cannot see a rhythm, since a number that only
#'   grows puts every Monday somewhere new -- and skips the test, for very
#'   large data or when only order matters. `"drop"` leaves dates out. A
#'   duration (`difftime`) is used as its number of days.
#' @param ... Passed to [ilm_glrm()] when `method = "glrm"`.
#' @param ndim Number of dimensions to keep. Five suits most data, but not
#'   all: on mixed data with few columns, the dimensions past the first few
#'   can carry a categorical column's own levels, and a clustering on them
#'   splits the groups along those levels. Look at the scree plot
#'   ([ilm_plot_reduce_scree()]) and keep the dimensions before it flattens;
#'   the profiling vignette shows a case. Choosing `ndim` by parallel
#'   analysis instead was measured and is not the default: it helped there,
#'   and did worse on numeric data with well-separated clusters
#'   ([`dev/studies/ndim_choice.R`](https://github.com/huttoncp/illumex/blob/main/dev/studies/ndim_choice.R)).
#' @return An object of class `"ilm_reduce"`: `method` (`"pca"`, `"mca"` or
#'   `"famd"`), `eig` (dimension, eigenvalue, percent of variance and its
#'   cumulative total), `ind_coord` (`row_id` and one column per retained
#'   dimension -- this is what [ilm_cluster()] takes), `var_contrib`
#'   (`variable`, `dim`, `sqload`: how strongly each original variable relates
#'   to each dimension, on a 0 to 1 scale, for numeric and categorical
#'   variables alike), `n`, `cols` (the columns used), `time` (how each date
#'   or date-time column was used), and `fit`: the eigenvalues, coordinates and
#'   squared loadings as computed, for anyone who wants to go past this
#'   wrapper. Each dimension's sign is fixed by a rule, so the same data give
#'   the same coordinates everywhere: the column that loads most on a
#'   dimension loads positively.
#'
#'   With `method = "glrm"`, `method` is `"glrm"` and `fit` is the
#'   [ilm_glrm()] fit. Its factors are not unique -- any rotation of one can be
#'   undone in the other -- so the coordinates and loadings reported are an
#'   orthogonal rotation of the fitted low-rank product, as principal
#'   components are, which leaves the reconstruction unchanged. `pct_var` and
#'   `cum_pct_var` are then shares of the numeric columns' variance, and
#'   `pct_categorical` and `cum_pct_categorical` each dimension's share of the
#'   fitted categorical signal, which is on the logit scale and so reported
#'   apart.
#' @seealso [ilm_cluster()] to group the rows, [ilm_profile()] for the whole
#'   pipeline, [ilm_reduce_na()] for the same thing applied to missingness.
#' @references
#' \enc{Pagès}{Pages}, J. (2004). Analyse factorielle de \enc{données}{donnees} mixtes. Revue de
#' Statistique \enc{Appliquée}{Appliquee} 52(4), 93-111.
#'
#' Chavent, M., Kuentz-Simonet, V., Labenne, A. and Saracco, J. (2014).
#' Multivariate analysis of mixed data: the PCAmixdata R package. arXiv
#' 1411.4911.
#' @examples
#' r <- ilm_reduce(mtcars)
#' r
#' head(r$var_contrib[order(-r$var_contrib$sqload), ])
#' @export
ilm_reduce <- function(data, cols = NULL, ndim = 5,
                       method = c("famd", "glrm", "pcamix"),
                       time = c("cycles", "elapsed", "drop"), ..., cols_negate = FALSE,
                       cols_fixed = FALSE, subset = NULL, subset_negate = FALSE,
                       subset_fixed = FALSE) {
  method <- match.arg(method)
  if (method == "pcamix") method <- "famd"   # the former name, still accepted
  time <- match.arg(time)
  ## An ilm_anomaly() result is accepted directly: the flagged rows are what
  ## the user wants to look at next, and rebuilding that subset by hand from
  ## `row` is both a papercut and a chance to line the wrong rows up.
  if (inherits(data, "ilm_anomaly")) {
    if (!is.null(subset) || isTRUE(subset_negate))
      stop("`subset` cannot be applied to an ilm_anomaly() result: subset the data ",
           "before ilm_anomaly(), or use the rows its result gives", call. = FALSE)
    data <- ilm_from_anomaly(data, "ilm_reduce")
  }
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  rs <- ilm_select_rows(data, subset, subset_negate, subset_fixed)
  data <- rs$data
  if (method == "glrm") {
    out <- ilm_reduce_glrm(data, cols, ndim, time = time, cols_negate = cols_negate,
                           cols_fixed = cols_fixed, ...)
    return(ilm_select_finish(out, data, rs, cols, out$cols, cols_negate = cols_negate,
                             cols_fixed = cols_fixed))
  }
  keep <- ilm_resolve_cols(data, cols, negate = cols_negate, fixed = cols_fixed)
  ilm_stop_not_utf8_names(keep, "ilm_reduce")
  sub <- ilm_time_encode(data[keep], time, "ilm_reduce")
  tmap <- attr(sub, "time_map")

  is_num <- vapply(sub, is.numeric, TRUE)
  is_cat <- vapply(sub, function(x)
    is.factor(x) || is.character(x) || is.logical(x), TRUE)
  dropped <- names(sub)[!is_num & !is_cat]
  if (length(dropped))
    message("ilm_reduce(): dropping column(s) that are neither numeric nor ",
            "categorical, so no method here can use them: ",
            paste(dropped, collapse = ", "))
  if (!any(is_num) && !any(is_cat))
    stop("no numeric or categorical columns to reduce", call. = FALSE)

  ## as.data.frame(), not just the subset. `sub[is_num]` on a TIBBLE returns a
  ## tibble, and PCAmix (which computed this before ilm_famd()) checked its
  ## columns with `is.numeric(X.quanti[, j])` --
  ## which for a tibble is a one-column tibble rather than a vector, so every
  ## column looks non-numeric and the whole call dies on "All variables in
  ## X.quanti must be numeric". Nothing in that message mentions tibbles, and
  ## a tibble is what anyone gets from readr, dplyr or gapminder, so this hit
  ## the most ordinary way to arrive at the function. `quali` was already
  ## coerced on the next line, which is why only the numeric half broke.
  quanti <- if (any(is_num)) as.data.frame(sub[is_num]) else NULL
  quali <- if (any(is_cat)) as.data.frame(lapply(sub[is_cat], as.factor)) else NULL
  method <- if (!is.null(quanti) && !is.null(quali)) "famd"
            else if (!is.null(quanti)) "pca" else "mca"

  ## How many dimensions exist at all. For MCA that is the number of levels
  ## less the number of variables. For PCA and the mixed case it is
  ## min(n - 1, p) -- NOT p - 1, which is a different quantity and one short:
  ## two columns have two components, not one, and asking PCAmix for one was an
  ## error rather than a smaller answer, so ilm_reduce() used to fail outright
  ## on any two-column selection.
  max_dim <- if (method == "mca")
               sum(vapply(quali, nlevels, 1L)) - ncol(quali)
             else min(nrow(sub) - 1L, ncol(sub))
  ## a reduction needs at least two, and with fewer than two available there is
  ## nothing to reduce
  ndim <- min(ndim, max_dim)
  if (ndim < 2L) {
    if (max_dim < 2L)
      stop("there are only ", max_dim, " dimension(s) available from these ",
           "columns, and a reduction needs at least 2", call. = FALSE)
    ndim <- 2L
  }

  fit <- ilm_famd(quanti, quali, ndim = ndim)

  eig <- data.frame(dim = seq_len(nrow(fit$eig)),
                    eigenvalue = unname(fit$eig[, 1]),
                    pct_var = unname(fit$eig[, 2]),
                    cum_pct_var = unname(fit$eig[, 3]),
                    stringsAsFactors = FALSE)
  rownames(eig) <- NULL

  ic <- as.data.frame(fit$ind$coord)
  names(ic) <- paste0("dim", seq_len(ncol(ic)))
  ## the data's own row numbers, which a subset keeps
  ind_coord <- cbind(row_id = ilm_orig_rows(data, seq_len(nrow(ic))), ic)
  rownames(ind_coord) <- NULL

  sq <- fit$sqload
  var_contrib <- data.frame(
    variable = rep(rownames(sq), ncol(sq)),
    dim = rep(seq_len(ncol(sq)), each = nrow(sq)),
    sqload = as.vector(sq), stringsAsFactors = FALSE)
  ## loadings equal but for rounding noise (two variables share a dimension
  ## exactly when there are only two) are a tie, and keep their column order
  var_contrib <- var_contrib[order(var_contrib$dim, -ilm_tie(var_contrib$sqload)), ,
                             drop = FALSE]
  rownames(var_contrib) <- NULL

  out <- structure(list(method = method, eig = eig, ind_coord = ind_coord,
                 var_contrib = var_contrib, n = nrow(sub), ndim = ndim,
                 ## no dimension is cut here (item 265's cut is the GLRM's)
                 ndim_fitted = ndim,
                 cols = keep, fit = fit, time = tmap), class = "ilm_reduce")
  ilm_select_finish(out, data, rs, cols, keep, cols_negate = cols_negate,
                    cols_fixed = cols_fixed)
}

#' @export
print.ilm_reduce <- function(x, ...) {
  tag <- if (inherits(x, "ilm_reduce_na")) "ilm_reduce_na" else "ilm_reduce"
  fitted <- x$ndim_fitted %||% x$ndim
  if (fitted > x$ndim)
    cat(sprintf("<%s> method = %s, n = %d, %d dimensions carry variation\n  (%d fitted; after shrinkage %s)\n",
                tag, x$method, x$n, x$ndim, fitted, ilm_dropped_dims(x$ndim, fitted)))
  else
    cat(sprintf("<%s> method = %s, n = %d, %d dimension(s) retained\n",
                tag, x$method, x$n, x$ndim))
  m <- min(3L, nrow(x$eig))
  if (identical(x$method, "glrm")) {
    ## a categorical column is fitted on the logit scale, so its share is of
    ## the fitted signal and is reported apart from the numeric columns'
    if (is.finite(x$eig$cum_pct_var[m]))
      cat(sprintf("  first %d dimension(s) explain %s%% of the numeric columns' variance\n",
                  m, ilm_fx(x$eig$cum_pct_var[m], 1)))
    if (m < nrow(x$eig) && is.finite(x$eig$cum_pct_categorical[m]))
      cat(sprintf("  and carry %s%% of the fitted categorical signal\n",
                  ilm_fx(x$eig$cum_pct_categorical[m], 1)))
  } else {
    cat(sprintf("  first %d dimension(s) explain %s%% of the variance\n",
                m, ilm_fx(x$eig$cum_pct_var[m], 1)))
  }
  cat("\n  strongest variable per dimension (squared loading)\n")
  for (d in sort(unique(x$var_contrib$dim))) {
    sl <- x$var_contrib[x$var_contrib$dim == d, , drop = FALSE]
    k <- which.max(ilm_tie(sl$sqload))
    cat(sprintf("    dim %-3d %-24s %s\n", d, ilm_show_text(sl$variable[k]), ilm_fx(sl$sqload[k], 3)))
  }
  invisible(x)
}

## ---- the same thing, applied to what is missing ----------------------------
##
## A present/missing marker is a two-level categorical variable, so building a
## frame of those markers and handing it to ilm_reduce() -- which then picks MCA,
## every column being categorical -- reuses the whole pipeline rather than
## duplicating it. Checked on synthetic data with two indicators built to go
## missing together: the resulting dimension recovered them at squared loadings
## of about 0.85 each, while a third, independently missing indicator loaded on
## a separate dimension.

#' @keywords internal
#' @noRd
ilm_build_na_indicator <- function(data, cols) {
  keep0 <- if (is.null(cols) || !length(cols)) names(data) else as.character(cols)
  p_na <- vapply(data[keep0], function(v) mean(is.na(v)), 1)
  degenerate <- p_na == 0 | p_na == 1
  if (any(degenerate))
    message("ilm_reduce_na(): dropping column(s) whose missingness never ",
            "varies (always or never missing): ",
            paste(keep0[degenerate], collapse = ", "))
  keep <- keep0[!degenerate]
  if (length(keep) < 2L)
    stop("at least 2 columns need some -- but not all -- values missing ",
         "before there is a missingness pattern to profile; ", length(keep),
         " qualif", if (length(keep) == 1L) "ies" else "y", call. = FALSE)
  out <- as.data.frame(lapply(keep, function(cn)
    factor(ifelse(is.na(data[[cn]]), "missing", "present"),
           levels = c("present", "missing"))))
  names(out) <- keep
  out
}

#' Reduce a data frame's missingness pattern to a few dimensions
#'
#' The missingness counterpart to [ilm_reduce()]. Builds a present/missing
#' marker for every column, drops any whose missingness never varies, and
#' reduces the markers -- which, being two-level categorical variables, always
#' takes the MCA route. The dimensions that come back describe which columns
#' tend to go missing *together*, which is what separates a block of variables
#' lost to one skipped section from values that went missing independently.
#'
#' @param data A data frame.
#' @inheritParams ilm_reduce
#' @param cols Columns to use. A character vector of names, a
#'   regular expression, a predicate function such as `is.numeric`, or
#'   `NULL` for all of them -- see [ilm_selection].
#' @param ndim Number of dimensions to keep.
#' @return An object of class `"ilm_reduce_na"`, which is also an
#'   `"ilm_reduce"` -- see [ilm_reduce()] for the shared structure. In
#'   `var_contrib`, `sqload` means how strongly a column's *missingness*
#'   relates to a dimension, not its values.
#' @seealso [ilm_profile_na()] for the whole pipeline,
#'   [ilm_check_missing()] for whether any of it matters to your model.
#' @examples
#' r <- ilm_reduce_na(airquality)
#' r
#' @export
ilm_reduce_na <- function(data, cols = NULL, ndim = 5, cols_negate = FALSE,
                          cols_fixed = FALSE, subset = NULL, subset_negate = FALSE,
                          subset_fixed = FALSE) {
  ## An ilm_anomaly() result is accepted directly: the flagged rows are what
  ## the user wants to look at next, and rebuilding that subset by hand from
  ## `row` is both a papercut and a chance to line the wrong rows up.
  if (inherits(data, "ilm_anomaly")) {
    if (!is.null(subset) || isTRUE(subset_negate))
      stop("`subset` cannot be applied to an ilm_anomaly() result: subset the data ",
           "before ilm_anomaly(), or use the rows its result gives", call. = FALSE)
    data <- ilm_from_anomaly(data, "ilm_reduce_na")
  }
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  rs <- ilm_select_rows(data, subset, subset_negate, subset_fixed)
  data <- rs$data
  keep <- ilm_resolve_cols(data, cols, negate = cols_negate, fixed = cols_fixed)
  ind <- ilm_build_na_indicator(data, keep)
  attr(ind, "ilm_rows") <- attr(data, "ilm_rows")    # row numbers carried through
  out <- ilm_select_unmark(ilm_reduce(ind, ndim = ndim))
  class(out) <- c("ilm_reduce_na", class(out))
  ilm_select_finish(out, data, rs, cols, out$cols, cols_negate = cols_negate,
                    cols_fixed = cols_fixed)
}

## ---- the generalized low rank route ----------------------------------------
##
## The same SHAPE of answer as the FAMD one -- scores per row, a contribution
## per variable per dimension -- so ilm_cluster() and ilm_profile() need to
## know nothing about which produced it. What differs is underneath: a loss per
## column type rather than squared error on scaled indicators.
##
## The fitted factors are not identified: any invertible k x k matrix moves
## between the scores and the archetypes without changing their product, and
## the optimiser returns whichever pair it reached. Their dimensions overlap,
## so a share of variation per dimension double-counts, and the cumulative
## share could pass 100%. So the fitted low-rank product is rotated to
## orthogonal components, as principal components are: centred (its column
## means move into the offset), decomposed by SVD, the coordinates taken as
## the left singular vectors times the singular values, and the loadings as
## the right singular vectors. The product, and so the reconstruction, is
## unchanged; `fit` keeps the optimiser's own factors.
##
## With orthogonal coordinates each dimension's share adds up exactly. For the
## numeric columns (quadratic and Poisson losses) it is a share of their total
## variation, as in PCA. A categorical column is fitted on the logit scale,
## which is not the scale of its indicators, so its share is reported apart:
## each dimension's part of the fitted categorical signal, summing to 100%
## across the dimensions kept.

#' @keywords internal
#' @noRd
ilm_reduce_glrm <- function(data, cols, ndim, ..., cols_negate = FALSE, cols_fixed = FALSE) {
  g <- ilm_glrm(data, cols = cols, rank = ndim, cols_negate = cols_negate,
                cols_fixed = cols_fixed, ...)
  k <- g$rank
  n <- nrow(g$scores)
  P <- as.matrix(g$scores) %*% g$archetypes
  P <- sweep(P, 2L, colMeans(P), "-")
  sv <- svd(P, nu = k, nv = k)
  ## Only the dimensions that carry variation are kept (Craig's item 265): a
  ## singular value at or below sqrt(.Machine$double.eps) times the first,
  ## the numerical-rank cut MASS::ginv uses, is zero to rounding. Heavy
  ## shrinkage can leave the centred product of lower rank than was fitted,
  ## and a zero singular value's vector is arbitrary -- any direction in the
  ## null space is a correct answer, and each LAPACK build returns a
  ## different one -- so its loadings would be noise. (A cut on size does
  ## not catch two nearly equal non-zero values, whose vectors can turn
  ## within their plane; none of the recorded prints has a gap near
  ## rounding, the narrowest being FAMD's 0.37%, whose vectors agree on
  ## every platform.)
  k_fitted <- k
  k <- max(1L, sum(sv$d[seq_len(k)] > ilm_rank_tol(sv$d)))
  s2 <- sv$d[seq_len(k)]^2
  R <- sv$v[, seq_len(k), drop = FALSE]
  blocks <- g$encoding$blocks
  is_num <- vapply(blocks, function(b) b$loss %in% c("quadratic", "poisson"), TRUE)
  num_cols <- unlist(lapply(blocks[is_num], `[[`, "cols"))
  cat_cols <- unlist(lapply(blocks[!is_num], `[[`, "cols"))
  ## each component's sum of squares within a set of columns; the components
  ## are orthogonal, so these add up across dimensions
  part <- function(j) if (length(j))
    s2 * colSums(R[j, , drop = FALSE]^2) else rep(NA_real_, k)
  ss_num <- part(num_cols); ss_cat <- part(cat_cols)
  ## the numeric columns' total variation, on the standardised scale they are
  ## fitted on (a missing cell counts at the column's centre)
  tot <- if (length(num_cols)) max(sum(vapply(blocks[is_num], function(b) {
    v <- b$target; v[is.na(v)] <- 0; sum((v - mean(v))^2)
  }, 0)), .Machine$double.eps) else NA_real_
  pct <- 100 * ss_num / tot
  cat_share <- if (length(cat_cols)) 100 * ss_cat / max(sum(ss_cat), .Machine$double.eps)
               else rep(NA_real_, k)
  eig <- data.frame(dim = seq_len(k), eigenvalue = s2,
                    pct_var = pct, cum_pct_var = cumsum(pct),
                    pct_categorical = cat_share,
                    cum_pct_categorical = cumsum(cat_share),
                    row.names = NULL)
  ## a variable contributes to a dimension through every column its block
  ## occupies, so the squared loadings are summed over the block; the loadings
  ## are unit vectors, so each dimension's contributions sum to one
  vc <- do.call(rbind, lapply(seq_len(k), function(j) {
    sq <- vapply(blocks, function(b) sum(R[b$cols, j]^2), 0)
    data.frame(dim = j, variable = names(blocks),
               sqload = sq / max(sum(sq), .Machine$double.eps),
               row.names = NULL)
  }))
  co <- as.data.frame(sv$u[, seq_len(k), drop = FALSE] *
                        rep(sv$d[seq_len(k)], each = n))
  names(co) <- paste0("dim", seq_len(k))
  ## a row_id first, as the FAMD route and the help give it: the data's own
  ## row numbers, which a subset keeps
  co <- cbind(row_id = ilm_orig_rows(data, seq_len(n)), co)
  structure(list(method = "glrm", eig = eig, ind_coord = co,
                 var_contrib = vc, n = n, ndim = k, ndim_fitted = k_fitted,
                 cols = g$columns, fit = g, time = g$time), class = "ilm_reduce")
}

## the numerical-rank cut on singular values (item 265): a value at or below
## sqrt(.Machine$double.eps) times the largest is zero to rounding
#' @keywords internal
#' @noRd
ilm_rank_tol <- function(d) sqrt(.Machine$double.eps) * max(d, 0)

## "the 5th has none", "the 4th and 5th have none", "the 3rd to 5th have none"
#' @keywords internal
#' @noRd
ilm_dropped_dims <- function(kept, fitted) {
  ord <- function(i) paste0(i, if (i %% 100 %in% 11:13) "th"
                            else c("th", "st", "nd", "rd", rep("th", 6))[i %% 10 + 1])
  first <- kept + 1L
  if (first == fitted) sprintf("the %s has none", ord(first))
  else if (first + 1L == fitted) sprintf("the %s and %s have none", ord(first), ord(fitted))
  else sprintf("the %s to %s have none", ord(first), ord(fitted))
}
