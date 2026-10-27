## ---------------------------------------------------------------------------
## Choosing rows (items 277 and 279).
##
## `subset` is dplyr::filter() before the call, as an ordinary value: a
## logical vector, row positions, named patterns, a random sample from
## ilm_sample(), or a model, for the rows it analysed (R/ilm_subset_model.R).
## It runs first, before `cols`, `by` and everything else, and
## `subset_negate` takes the rows it would not. The rows kept keep their
## original numbers wherever a result names a row, since those are the
## numbers a user joins back on.
##
## One implementation for the family: illume re-exports ilm_sample() and
## calls ilm_resolve_rows().
## ---------------------------------------------------------------------------

#' A random sample of rows, or of whole groups, for `subset`
#'
#' Given as `subset` to any illumex function that takes it, `ilm_sample()`
#' draws the rows to use. Give `n` for a number of rows or `prop` for a share
#' of them. It samples in one of four ways:
#'
#' * **Rows**, the default: `n` rows, or a share `prop` of them.
#' * **Whole groups**, with `by`: `n` and `prop` count groups, every row of a
#'   group drawn is kept and the other groups are left out, so a grouped
#'   holdout (`subset_negate = TRUE`) never splits a group.
#' * **Crossed factors**, with `by` naming each factor and its share:
#'   `by = c(rater = 0.3, item = 0.2)` draws 30% of the raters and 20% of
#'   the items, each on its own, and keeps the rows whose rater and item were
#'   both drawn -- a two-way design sampled as one. A factor takes a share
#'   between 0 and 1, a count of its levels (2 or more), or 1 (or `"all"`)
#'   to keep it whole, which is how a factor with few levels is kept. `n` and
#'   `prop` are left out, since each factor carries its own.
#' * **Inside every sampling cluster**, with `within` -- a site, a school, a
#'   participant measured repeatedly: rows are drawn inside each
#'   cluster and every cluster is kept -- the remedy when data are too large
#'   to fit but each cluster must stay in the model. The draw is proportional:
#'   a share `prop` of each cluster's rows, or `n` rows in all shared out in
#'   proportion to the clusters' sizes. Every cluster keeps at least `min`
#'   rows, or all of them when it has fewer than `min`. That floor makes a
#'   small cluster's share larger than a large one's: a row's chance of being
#'   kept is its cluster's rows kept over its cluster's rows, and the result
#'   keeps the rule, the smallest, median and largest of those shares, and
#'   what derives each row's share (`within`, `min` and `n` or `prop`), so a
#'   weighted analysis can be checked against an unweighted one.
#'
#' `within` may name nested levels, coarsest first or in any order:
#' `within = c("school", "classroom")` draws inside each classroom, so every
#' classroom, and so every school, is kept. The levels must be nested: each
#' classroom in one school. A classroom id that repeats across schools is
#' refused, since the data cannot say whether it is a different classroom in
#' each school (give them unique ids, such as `paste(school, classroom)`) or
#' the same classroom across schools -- a crossed design, for which `within`
#' is not meant: draw each factor with `by = c(...)`, or rows with
#' `ilm_sample(prop = )`. `by` and `within` together are an error.
#'
#' The draw follows the family's rule for random numbers: with a `seed`, the
#' same rows every time, and your own random stream is left as it was; with
#' `seed = NULL`, it draws from your stream as any R code does. The result
#' keeps the seed and the generator's kind.
#'
#' A number given to `subset` on its own always means row positions:
#' `subset = 10` is row 10, and `subset = ilm_sample(10)` is ten rows drawn.
#'
#' **What a sample leaves.** Sampling rows can thin a grouping out: a rater
#' left with one row, or raters and items that no longer meet. With
#' `report_by = c("rater", "item")`, after any of the four ways, the result
#' keeps, in its `"ilm_select"` attribute, for each column the levels given and kept and the fewest and median
#' rows per kept level, and for each pair whether their levels, joined where
#' a kept row has both, are still in one piece. A level left with fewer than
#' 2 rows, or a pair split into pieces, gives a warning -- with counts, never
#' the levels' values.
#'
#' @param n A number of rows (or of groups, with `by`) to draw: a whole
#'   number, at most the number there are. With `within`, the total to share
#'   out across the clusters; the floor `min` can raise it.
#' @param prop A share of rows (or of groups) to draw, strictly between 0
#'   and 1; the count is rounded, and must come to at least 1. With
#'   `within`, the share of each cluster's rows, rounded within each.
#' @param by A column whose values are the groups to draw whole; or, for
#'   crossed factors, each factor named with its share, count or `"all"`:
#'   `c(rater = 0.3, item = 0.2)`. `NULL` draws rows. A missing value is a
#'   group, or a level, of its own.
#' @param seed An integer seed for the draw, or `NULL`.
#' @param within One column, or nested columns, whose sampling clusters are each
#'   sampled inside and all kept. A missing value is a cluster of its own.
#' @param min With `within`, the fewest rows any cluster keeps (all of a
#'   cluster with fewer). A whole number of at least 1; 2 by default.
#' @param report_by Grouping columns to report on after the draw: their
#'   levels and rows kept, and whether each pair is still connected. `NULL`
#'   reports nothing.
#' @return An object of class `"ilm_sample"`, which does nothing until it is
#'   given as `subset`.
#' @seealso [ilm_selection] for `subset` and `cols`, [ilm_subset()] for the
#'   data itself, which with `within` also carries each row's chance of being
#'   kept as the attribute `"ilm_inclusion"`.
#' @examples
#' d <- ilm_sim()
#' ilm_describe(d, "score", subset = ilm_sample(prop = 0.5, seed = 1))
#' ## a grouped holdout: the ids not drawn
#' ilm_describe(d, "score", subset = ilm_sample(5, by = "id", seed = 1),
#'              subset_negate = TRUE)
#' ## half the ids and every site: the rows of the ids drawn
#' ilm_describe(d, "score", subset = ilm_sample(by = c(id = 0.5, site = "all"), seed = 1))
#' ## a fifth of the rows, and what it leaves of each id and site
#' ilm_describe(d, "score", subset = ilm_sample(prop = 0.2, seed = 1,
#'                                              report_by = c("id", "site")))
#' ## a third of every site's rows, at least 2 from each
#' ilm_describe(d, "score", subset = ilm_sample(prop = 1/3, within = "site", seed = 1))
#' @export
ilm_sample <- function(n = NULL, prop = NULL, by = NULL, seed = NULL, within = NULL,
                       min = 2L, report_by = NULL) {
  ## by named with each factor's share: a crossed design, drawn by factor
  factors <- ilm_sample_factors(by)
  if (!is.null(factors)) {
    if (!is.null(n) || !is.null(prop))
      stop("with by = c(", names(by)[1L], " = ...), each factor's share or count is in `by`; ",
           "leave `n` and `prop` out", call. = FALSE)
    by <- NULL
  } else if (is.null(n) == is.null(prop))
    stop("ilm_sample() takes `n` (a number of rows) or `prop` (a share of them), ",
         "one of the two", call. = FALSE)
  if (!is.null(n) && !(is.numeric(n) && length(n) == 1L && is.finite(n) &&
                       n >= 1 && n == round(n)))
    stop("`n` must be a whole number of at least 1",
         if (is.numeric(n) && length(n) == 1L && n > 0 && n < 1)
           sprintf("; for a share of the rows, use prop = %s", format(n)),
         call. = FALSE)
  if (!is.null(prop) && !(is.numeric(prop) && length(prop) == 1L && is.finite(prop) &&
                          prop > 0 && prop < 1))
    stop("`prop` must be a share strictly between 0 and 1", call. = FALSE)
  if (!is.null(by) && !(is.character(by) && length(by) == 1L && !is.na(by)))
    stop("`by` names one column whose groups are drawn whole, or, for crossed factors, ",
         "names each with its share: by = c(rater = 0.3, item = 0.2)", call. = FALSE)
  if (!is.null(within) && !(is.character(within) && length(within) >= 1L &&
                            !anyNA(within) && !anyDuplicated(within)))
    stop("`within` must name one column, or several nested ones, each once", call. = FALSE)
  if ((!is.null(by) || !is.null(factors)) && !is.null(within))
    stop("ilm_sample() takes `by` (whole groups, the rest left out) or `within` ",
         "(rows inside every cluster, all kept), not both", call. = FALSE)
  if (!(is.numeric(min) && length(min) == 1L && is.finite(min) && min >= 1 && min == round(min)))
    stop("`min` must be a whole number of at least 1", call. = FALSE)
  if (!is.null(seed) && !(is.numeric(seed) && length(seed) == 1L && is.finite(seed)))
    stop("`seed` must be a single number, or NULL", call. = FALSE)
  ## the argument's name lives here and in the help only (Craig may rename
  ## it); inside, the sample keeps it as `report`
  if (!is.null(report_by) && !(is.character(report_by) && length(report_by) >= 1L &&
                               !anyNA(report_by) && !anyDuplicated(report_by)))
    stop("`report_by` names the grouping columns to report on, each once", call. = FALSE)
  structure(list(n = if (!is.null(n)) as.integer(n), prop = prop, by = by, factors = factors,
                 within = within, min = if (!is.null(within)) as.integer(min),
                 seed = if (!is.null(seed)) as.integer(seed), report = report_by),
            class = "ilm_sample")
}

## `by` with a share or count per factor (item 284): c(rater = 0.3, item =
## 0.2), or a list; each value a share in (0, 1), a count of levels (a whole
## number of at least 2), or 1 or "all" to keep the factor whole. NULL when
## `by` names one column the old way.
#' @keywords internal
#' @noRd
ilm_sample_factors <- function(by) {
  if (is.null(by) || is.null(names(by))) return(NULL)
  nm <- names(by)
  if (any(is.na(nm) | !nzchar(nm)) || anyDuplicated(nm))
    stop("every factor in `by` needs its own name: by = c(rater = 0.3, item = 0.2)",
         call. = FALSE)
  lapply(stats::setNames(seq_along(by), nm), function(i) {
    v <- by[[i]]
    bad <- function() stop("`by` gives ", nm[i], " = ", format(v), "; each factor takes a share ",
                           "between 0 and 1, a count of levels (2 or more), or 1 or \"all\" to ",
                           "keep it whole", call. = FALSE)
    if (length(v) != 1L || is.na(v)) bad()
    if (identical(tolower(as.character(v)), "all")) return(list(whole = TRUE))
    x <- suppressWarnings(as.numeric(v))
    if (is.na(x)) bad()
    if (x == 1) list(whole = TRUE)
    else if (x > 0 && x < 1) list(prop = x)
    else if (x >= 2 && x == round(x)) list(n = as.integer(x))
    else bad()
  })
}

#' @export
print.ilm_sample <- function(x, ...) {
  if (!is.null(x$factors)) {
    each <- vapply(names(x$factors), function(f) {
      s <- x$factors[[f]]
      paste0(f, " ", if (isTRUE(s$whole)) "all" else if (!is.null(s$n)) paste(s$n, "levels")
             else paste0(format(100 * s$prop), "%"))
    }, "")
    cat("<ilm_sample> rows whose levels were all drawn: ", paste(each, collapse = ", "),
        ", seed ", if (is.null(x$seed)) "none" else x$seed, ilm_sample_report_note(x), "\n",
        sep = "")
    return(invisible(x))
  }
  unit <- if (!is.null(x$by)) "groups" else "rows"
  what <- if (!is.null(x$n)) paste(x$n, unit) else paste0(format(100 * x$prop), "% of ", unit)
  how <- if (!is.null(x$by)) paste0(" of ", x$by)
         else if (!is.null(x$within))
           paste0(" inside every ", paste(x$within, collapse = " / "), ", at least ", x$min,
                  " from each")
         else ""
  cat("<ilm_sample> ", what, how, ", seed ", if (is.null(x$seed)) "none" else x$seed,
      ilm_sample_report_note(x), "\n", sep = "")
  invisible(x)
}

#' @keywords internal
#' @noRd
ilm_sample_report_note <- function(x)
  if (length(x$report)) paste0("; reports on ", paste(x$report, collapse = ", ")) else ""

## The rows `subset` keeps, as positions in `data` in the data's own order,
## and an account of them for the result to keep. `allow_sample` is
## FALSE where a random sample makes no sense (ilm_recode_errors()).
#' @keywords internal
#' @noRd
ilm_resolve_rows <- function(data, subset, negate = FALSE, fixed = FALSE,
                             allow_sample = TRUE, fn = NULL) {
  N <- nrow(data)
  if (is.null(subset)) {
    if (isTRUE(negate))
      stop("`subset_negate = TRUE` needs `subset` to say which rows to leave out",
           call. = FALSE)
    return(list(rows = seq_len(N), info = NULL))
  }
  negate <- isTRUE(negate)
  info <- list(negate = negate, n_rows_given = N)
  incl <- NULL
  if (inherits(subset, "ilm_sample")) {
    if (!allow_sample)
      stop("`subset` cannot be a random sample here", if (!is.null(fn)) paste0(" (", fn, ")"),
           ": give the rows as a condition, positions or patterns", call. = FALSE)
    s <- ilm_draw_sample(data, subset)
    keep <- s$keep
    incl <- s$inclusion
    info$form <- "sample"
    info$sample <- s$sample
  } else if (inherits(subset, c("ilm_model", "ilm_dag_model"))) {
    m <- ilm_model_rows(subset, data)
    keep <- m$keep
    info$form <- "model"
    info$model <- m$model
  } else if (is.logical(subset)) {
    if (length(subset) != N)
      stop("a logical `subset` needs one value per row: it has ", length(subset),
           " and the data ", N, call. = FALSE)
    ## NA is out either way, as in dplyr::filter()
    keep <- if (negate) !subset & !is.na(subset) else subset & !is.na(subset)
    negate <- FALSE                       # applied here already
    info$form <- "logical"
  } else if (is.numeric(subset)) {
    if (anyNA(subset) || any(subset != round(subset)))
      stop("row positions in `subset` must be whole numbers", call. = FALSE)
    if (any(subset < 0))
      stop("row positions in `subset` must be positive; to leave rows out, give ",
           "them and set subset_negate = TRUE", call. = FALSE)
    if (any(subset < 1 | subset > N))
      stop("row positions in `subset` must be between 1 and ", N, "; ",
           paste(utils::head(subset[subset < 1 | subset > N], 5), collapse = ", "),
           " is not", call. = FALSE)
    if (anyDuplicated(subset))
      stop("row positions in `subset` must not repeat; ",
           paste(unique(subset[duplicated(subset)])[1:min(5, sum(duplicated(subset)))],
                 collapse = ", "), " does", call. = FALSE)
    keep <- seq_len(N) %in% subset
    info$form <- "positions"
  } else if (is.character(subset)) {
    nm <- names(subset)
    if (is.null(nm) || any(is.na(nm) | !nzchar(nm)))
      stop("a character `subset` is named patterns, one per column: ",
           "subset = c(species = \"^set\") keeps the rows whose species matches",
           call. = FALSE)
    miss <- setdiff(nm, names(data))
    if (length(miss))
      stop("`subset` names column(s) not in the data: ", paste(miss, collapse = ", "),
           call. = FALSE)
    keep <- rep(TRUE, N)
    for (i in seq_along(subset)) {
      v <- as.character(data[[nm[i]]])
      hit <- tryCatch(grepl(subset[[i]], v, fixed = isTRUE(fixed)),
                      error = function(e)
                        stop("`subset` pattern for ", nm[i], " is not a valid regular ",
                             "expression: ", subset[[i]], call. = FALSE))
      keep <- keep & hit & !is.na(v)      # a missing value matches nothing
    }
    info$form <- "patterns"
    info$patterns <- as.list(subset)
  } else {
    stop("`subset` must be a logical vector, row positions, named patterns, ",
         "ilm_sample(), or a model from illume's ilm_model() or ilm_dag_model(); it is ",
         class(subset)[1], call. = FALSE)
  }
  if (negate) keep <- !keep
  rows <- which(keep)
  ## what the sample left of the grouping columns asked for, on the rows
  ## kept -- after negation, the rows not drawn
  if (inherits(subset, "ilm_sample") && length(subset$report) && length(rows))
    info$sample$report <- ilm_sample_report(data, rows, subset$report)
  ## each kept row's chance of being kept, when a sample inside clusters
  ## drew it (or, negated, of being left out)
  inclusion <- if (!is.null(incl)) (if (negate) 1 - incl else incl)[rows]
  if (!length(rows))
    stop("`subset` kept no rows: ", ilm_subset_describe(subset, info), call. = FALSE)
  info$n_rows_kept <- length(rows)
  list(rows = rows, inclusion = inclusion,
       info = info[c("form", "negate", "n_rows_given", "n_rows_kept",
                     intersect(c("patterns", "sample", "model"), names(info)))])
}

## the rows an ilm_sample() draws, an account of the draw (its size, its
## groups, and its seed with the generator's kinds), and, when it samples
## inside clusters, each row's chance of being kept
#' @keywords internal
#' @noRd
ilm_draw_sample <- function(data, s) {
  N <- nrow(data)
  if (!is.null(s$by) && !s$by %in% names(data))
    stop("ilm_sample(by = ) names a column not in the data: ", s$by, call. = FALSE)
  ## the family's rule: a seed draws the same every time and puts the user's
  ## stream back; no seed draws from the stream as any R code does
  kinds <- RNGkind()
  seed_rec <- c(if (!is.null(s$seed)) list(value = s$seed),
                list(kind = kinds[1], normal_kind = kinds[2], sample_kind = kinds[3]))
  if (!is.null(s$within)) return(ilm_draw_within(data, s, seed_rec))
  if (!is.null(s$factors)) return(ilm_draw_factors(data, s, seed_rec))
  units <- if (is.null(s$by)) seq_len(N) else {
    g <- data[[s$by]]
    match(g, unique(g))                   # NA is a group of its own
  }
  K <- max(units, 0L)
  what <- if (is.null(s$by)) "rows" else paste0("groups of ", s$by)
  k <- if (!is.null(s$n)) s$n else as.integer(round(s$prop * K))
  if (k > K)
    stop("ilm_sample() asks for ", k, " ", what, " and there are ", K, call. = FALSE)
  if (k < 1L)
    stop("ilm_sample(prop = ", format(s$prop), ") comes to no ", what, " of ", K,
         call. = FALSE)
  if (!is.null(s$seed)) {
    ilm_rng_restore(s$seed)
    set.seed(s$seed)
  }
  chosen <- sample.int(K, k)
  list(keep = units %in% chosen,
       sample = c(if (!is.null(s$n)) list(n = s$n), if (!is.null(s$prop)) list(prop = s$prop),
                  if (!is.null(s$by)) list(by = s$by), list(seed = seed_rec)))
}

## A crossed design drawn by factor (item 284): each factor's levels are
## drawn on their own -- a share, a count, or all of them -- and a row is
## kept when every one of its levels was drawn.
#' @keywords internal
#' @noRd
ilm_draw_factors <- function(data, s, seed_rec) {
  N <- nrow(data)
  miss <- setdiff(names(s$factors), names(data))
  if (length(miss))
    stop("ilm_sample(by = ) names column(s) not in the data: ", paste(miss, collapse = ", "),
         call. = FALSE)
  if (!is.null(s$seed)) {
    ilm_rng_restore(s$seed)
    set.seed(s$seed)
  }
  keep <- rep(TRUE, N)
  rec <- list()
  for (f in names(s$factors)) {
    sp <- s$factors[[f]]
    g <- data[[f]]
    u <- match(g, unique(g))              # NA is a level of its own
    K <- max(u, 0L)
    k <- if (isTRUE(sp$whole)) K else if (!is.null(sp$n)) sp$n else as.integer(round(sp$prop * K))
    if (k > K)
      stop("ilm_sample(by = ) asks for ", k, " levels of ", f, " and there are ", K, call. = FALSE)
    if (k < 1L)
      stop("ilm_sample(by = ) asks for ", format(sp$prop), " of ", f, "'s ", K,
           " levels, which comes to none", call. = FALSE)
    chosen <- if (k == K) seq_len(K) else sample.int(K, k)
    keep <- keep & u %in% chosen
    rec[[length(rec) + 1L]] <- c(list(column = f),
                                 if (isTRUE(sp$whole)) list(whole = TRUE),
                                 if (!is.null(sp$n)) list(n = sp$n),
                                 if (!is.null(sp$prop)) list(prop = sp$prop),
                                 list(levels_given = K, levels_kept = k))
  }
  list(keep = keep, sample = list(factors = rec, seed = seed_rec))
}

## Rows drawn inside every cluster, all clusters kept (item 283): each
## cluster keeps round(prop x its rows), or its share of `n` by the largest
## remainders, but never fewer than `min` -- or all of it, when it has fewer.
#' @keywords internal
#' @noRd
ilm_draw_within <- function(data, s, seed_rec) {
  N <- nrow(data)
  cl <- ilm_within_clusters(data, s$within)
  sizes <- tabulate(cl, max(cl))
  if (!is.null(s$n) && s$n > N)
    stop("ilm_sample() asks for ", s$n, " rows and there are ", N, call. = FALSE)
  want <- if (!is.null(s$prop)) round(s$prop * sizes) else {
    exact <- s$n * sizes / N
    base <- floor(exact)
    extra <- s$n - sum(base)
    if (extra > 0L) {
      o <- order(-(exact - base), seq_along(exact))[seq_len(extra)]
      base[o] <- base[o] + 1
    }
    base
  }
  m <- as.integer(pmin(sizes, pmax(want, pmin(s$min, sizes))))
  if (!is.null(s$seed)) {
    ilm_rng_restore(s$seed)
    set.seed(s$seed)
  }
  idx <- split(seq_len(N), factor(cl, levels = seq_along(sizes)))
  chosen <- unlist(lapply(seq_along(idx), function(c) {
    r <- idx[[c]]
    if (m[c] >= length(r)) r else r[sample.int(length(r), m[c])]
  }), use.names = FALSE)
  frac <- m / sizes
  list(keep = seq_len(N) %in% chosen, inclusion = frac[cl],
       sample = c(if (!is.null(s$n)) list(n = s$n), if (!is.null(s$prop)) list(prop = s$prop),
                  list(within = as.list(s$within), min = s$min,
                       n_clusters = length(sizes),
                       n_clusters_whole = sum(m == sizes),
                       fraction_min = min(frac), fraction_median = stats::median(frac),
                       fraction_max = max(frac), seed = seed_rec)))
}

## Each row's cluster for `within`: one column's values, or nested columns'
## finest level. Nesting is checked: each value of a finer column must sit
## under one value of the coarser, else the levels are refused -- an id
## repeated across them is either a different unit in each (give it a unique
## id) or the same unit across them (crossed groupings, not supported yet),
## and the data cannot say which.
#' @keywords internal
#' @noRd
ilm_within_clusters <- function(data, within) {
  miss <- setdiff(within, names(data))
  if (length(miss))
    stop("ilm_sample(within = ) names column(s) not in the data: ",
         paste(miss, collapse = ", "), call. = FALSE)
  ids <- lapply(data[within], function(v) { v <- as.character(v); v[is.na(v)] <- "<NA>"; v })
  if (length(within) > 1L) {
    ## coarsest first: the fewest distinct values
    ord <- order(vapply(ids, function(v) length(unique(v)), 1L))
    lv <- within[ord]
    for (i in seq_len(length(lv) - 1L)) {
      coarse <- ids[[lv[i]]]; fine <- ids[[lv[i + 1L]]]
      pairs <- unique(data.frame(coarse = coarse, fine = fine, stringsAsFactors = FALSE))
      rep_id <- pairs$fine[duplicated(pairs$fine)]
      if (length(rep_id)) {
        under <- sum(pairs$fine == rep_id[1L])
        stop(sprintf(paste0(
          "ilm_sample(within = ): `%s` values repeat across `%s` (\"%s\" is under %d of ",
          "them), so the levels are not nested, and within = is for nested levels only. If a ",
          "`%s` value names a different unit under each `%s`, give them unique ids, such as ",
          "paste(%s, %s). If it is the same unit across them, the design is crossed: draw ",
          "each factor's levels with by = c(%s = 0.5, %s = 0.5), or rows with ",
          "ilm_sample(prop = )."),
          lv[i + 1L], lv[i], rep_id[1L], under, lv[i + 1L], lv[i], lv[i], lv[i + 1L],
          lv[i], lv[i + 1L]),
          call. = FALSE)
      }
    }
    finest <- ids[[lv[length(lv)]]]
  } else finest <- ids[[1L]]
  match(finest, unique(finest))
}

## a subset in words, for an error that says what it was
#' @keywords internal
#' @noRd
ilm_subset_describe <- function(subset, info) {
  base <- switch(info$form,
    sample = "the sample drawn",
    model = if (identical(info$model$scope, "sets")) "the rows every adjustment set analysed"
            else "the rows the model analysed",
    logical = "a condition true on no row",
    positions = paste0("positions ", paste(utils::head(subset, 5), collapse = ", "),
                       if (length(subset) > 5) ", ..."),
    patterns = paste(sprintf("%s matching \"%s\"", names(subset), subset), collapse = " and "))
  paste0(base, if (isTRUE(info$negate)) ", negated, which leaves every row out" else "",
         " (", info$n_rows_given, " rows given)")
}

## What `cols` chose: the form, the specification, and the data's columns
## not used, `by` kept out of them.
#' @keywords internal
#' @noRd
ilm_selection_info <- function(data, cols, used, by = character(), negate = FALSE,
                               fixed = FALSE) {
  if (is.null(cols) && !isTRUE(negate)) return(NULL)
  form <- if (is.function(cols)) "predicate"
          else if (length(cols) == 1L && !cols %in% names(data)) "pattern" else "names"
  spec <- switch(form,
    predicate = NULL,
    pattern = list(pattern = cols),
    names = list(names = as.list(cols)))
  ## the N and n of "Columns: n of N", the by columns set aside
  given <- setdiff(names(data), by)
  c(list(form = form), spec,
    list(negate = isTRUE(negate), fixed = isTRUE(fixed),
         n_cols_given = length(given), n_cols_kept = length(used),
         columns_excluded = as.list(setdiff(given, used))))
}

## The two lines a result prints above itself when a call chose its rows or
## columns: "119 of 600 rows (subset)" and "Columns: 5 of 8 (excluded: ...)".
#' @keywords internal
#' @noRd
ilm_select_lines <- function(sel) {
  if (is.null(sel)) return(character())
  s <- sel$subset; c_ <- sel$selection
  rows <- if (!is.null(s))
    sprintf("%s of %s rows (%s)", format(s$n_rows_kept, big.mark = ","),
            format(s$n_rows_given, big.mark = ","),
            if (identical(s$form, "model")) ilm_model_rows_words(s)
            else paste0("subset", if (isTRUE(s$negate)) ", negated" else ""))
  cols <- if (!is.null(c_)) {
    ex <- unlist(c_$columns_excluded)
    shown <- utils::head(ex, 8L)
    paste0("Columns: ", c_$n_cols_kept, " of ", c_$n_cols_given,
           if (length(ex)) paste0(" (excluded: ", paste(shown, collapse = ", "),
                                  if (length(ex) > 8L) paste0(", and ", length(ex) - 8L, " more"),
                                  ")"))
  }
  c(rows, cols)
}

## Mark a result with what its call chose, so it prints the lines above.
## Nothing is added when the call chose neither.
#' @keywords internal
#' @noRd
ilm_select_mark <- function(x, sel) {
  if (is.null(x) || is.null(sel) || (is.null(sel$subset) && is.null(sel$selection)))
    return(x)
  attr(x, "ilm_select") <- sel
  class(x) <- unique(c("ilm_selected", class(x)))
  x
}

#' @export
print.ilm_selected <- function(x, ...) {
  lines <- ilm_select_lines(attr(x, "ilm_select"))
  if (length(lines)) cat(lines, sep = "\n")
  NextMethod()
}

## A function's first steps, in order: the rows `subset` keeps, then the
## columns `cols` chooses among the rest. Returns the data cut to those rows
## (every column still there, for `by` and the like), the original numbers
## of the rows kept, and an account of them.
#' @keywords internal
#' @noRd
ilm_select_rows <- function(data, subset, subset_negate = FALSE, subset_fixed = FALSE,
                            allow_sample = TRUE, fn = NULL) {
  if (is.null(subset) && !isTRUE(subset_negate))
    return(list(data = data, rows = NULL, subset = NULL))
  if (!is.data.frame(data))
    stop("`subset` applies to a data frame", call. = FALSE)
  r <- ilm_resolve_rows(data, subset, subset_negate, subset_fixed, allow_sample, fn)
  out <- data[r$rows, , drop = FALSE]
  attr(out, "ilm_rows") <- r$rows
  list(data = out, rows = r$rows, subset = r$info, inclusion = r$inclusion)
}

## the original row numbers of a subset's rows, given positions within it
#' @keywords internal
#' @noRd
ilm_orig_rows <- function(data, pos) {
  o <- attr(data, "ilm_rows")
  if (is.null(o)) pos else o[pos]
}

#' Rows and columns of a data frame, chosen as illumex chooses them
#'
#' The data an illumex function would use after `subset` and `cols`: the rows
#' first, then the columns. Row names keep the original row numbers (or the
#' original names, if the data had them), and the attribute `"ilm_select"`
#' says what was chosen.
#'
#' @param data A data frame.
#' @param subset,subset_negate,subset_fixed Which rows; see [ilm_selection].
#' @param cols,cols_negate,cols_fixed Which columns; see [ilm_selection].
#' @return A data frame.
#' @seealso [ilm_selection], [ilm_sample()].
#' @examples
#' d <- ilm_sim()
#' ilm_subset(d, subset = c(grp = "^a"), cols = "^score|^income")
#' ilm_subset(d, subset = ilm_sample(10, seed = 1), cols = c("id", "date"),
#'            cols_negate = TRUE)
#' @export
ilm_subset <- function(data, subset = NULL, subset_negate = FALSE, subset_fixed = FALSE,
                       cols = NULL, cols_negate = FALSE, cols_fixed = FALSE) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  rs <- ilm_select_rows(data, subset, subset_negate, subset_fixed)
  d <- rs$data
  use <- ilm_resolve_cols(d, cols, negate = cols_negate, fixed = cols_fixed)
  out <- d[use]
  if (!is.null(rs$subset) && !.row_names_info(data) > 0L)
    rownames(out) <- rs$rows
  attr(out, "ilm_rows") <- NULL
  if (!is.null(rs$inclusion)) attr(out, "ilm_inclusion") <- rs$inclusion
  sel <- list(subset = rs$subset,
              selection = ilm_selection_info(data, cols, use, negate = cols_negate,
                                             fixed = cols_fixed))
  if (!is.null(sel$subset) || !is.null(sel$selection)) attr(out, "ilm_select") <- sel
  out
}

## A function's last step: mark its result with the rows and columns its
## call chose. `data` is the data after `subset` (every column), `used` the
## columns the result covers.
#' @keywords internal
#' @noRd
ilm_select_finish <- function(x, data, rs, cols, used, by = character(),
                              cols_negate = FALSE, cols_fixed = FALSE) {
  ilm_select_mark(x, list(
    subset = rs$subset,
    selection = ilm_selection_info(data, cols, used, by, cols_negate, cols_fixed)))
}

## For a plot, which returns nothing to print: the same lines, as a message,
## when rows were left out -- which a plot cannot show. The columns chosen
## are the panels drawn, so a choice of columns alone says nothing more.
#' @keywords internal
#' @noRd
ilm_select_note <- function(fn, data, rs, cols = NULL, used = NULL, by = character(),
                            cols_negate = FALSE, cols_fixed = FALSE) {
  if (is.null(rs$subset)) return(invisible(NULL))
  sel <- list(subset = rs$subset,
              selection = if (!is.null(used))
                ilm_selection_info(data, cols, used, by, cols_negate, cols_fixed))
  lines <- ilm_select_lines(sel)
  if (length(lines)) message(fn, "(): ", paste(lines, collapse = "; "))
  invisible(sel)
}

## a result used inside another function, which marks its own: no lines of
## its own
#' @keywords internal
#' @noRd
ilm_select_unmark <- function(x) {
  attr(x, "ilm_select") <- NULL
  class(x) <- setdiff(class(x), "ilm_selected")
  x
}

## What a sample left of the grouping columns named (item 284), for the
## result's sample report and for illume's memory check, which calls this
## on a model's grouping factors: per column, the levels given and kept and
## the fewest and median rows per kept level; per pair of columns, whether
## the graph of their levels -- joined where a kept row has both -- is still
## in one piece, and how many pieces it is in. A level left with fewer than
## 2 rows gives a warning with counts, never the levels' values.
#' @keywords internal
#' @noRd
ilm_sample_report <- function(data, rows, cols, warn = TRUE) {
  miss <- setdiff(cols, names(data))
  if (length(miss))
    stop("the sample's report names column(s) not in the data: ",
         paste(miss, collapse = ", "), call. = FALSE)
  key <- function(v) { v <- as.character(v); v[is.na(v)] <- "<NA>"; v }
  per <- lapply(cols, function(cn) {
    all_v <- key(data[[cn]])
    kept <- table(all_v[rows])
    kept <- kept[kept > 0L]
    list(column = cn, levels_given = length(unique(all_v)), levels_kept = length(kept),
         rows_min = if (length(kept)) as.integer(min(kept)) else 0L,
         rows_median = if (length(kept)) as.numeric(stats::median(kept)) else 0,
         levels_below_2 = sum(kept < 2L))
  })
  pairs <- if (length(cols) >= 2L) {
    cmb <- utils::combn(cols, 2L, simplify = FALSE)
    lapply(cmb, function(p) {
      a <- paste0("a:", key(data[[p[1]]])[rows]); b <- paste0("b:", key(data[[p[2]]])[rows])
      pieces <- ilm_graph_pieces(a, b)
      list(columns = as.list(p), connected = pieces == 1L, pieces = pieces)
    })
  }
  if (warn) {
    low <- Filter(function(x) x$levels_below_2 > 0L, per)
    if (length(low))
      warning("after sampling, ", paste(vapply(low, function(x)
        sprintf("%d of %d %s levels", x$levels_below_2, x$levels_kept, x$column), ""),
        collapse = " and "), " have fewer than 2 rows", call. = FALSE)
    split_up <- Filter(function(x) !x$connected, pairs %||% list())
    if (length(split_up))
      warning("after sampling, ", paste(vapply(split_up, function(x)
        sprintf("%s and %s fall into %d unconnected pieces", x$columns[[1]], x$columns[[2]],
                x$pieces), ""), collapse = "; "), call. = FALSE)
  }
  list(columns = per, pairs = pairs)
}

## the number of connected pieces of the graph whose edges join a[i] to b[i]
#' @keywords internal
#' @noRd
ilm_graph_pieces <- function(a, b) {
  if (!length(a)) return(0L)
  nodes <- unique(c(a, b))
  parent <- seq_along(nodes)
  find <- function(i) { while (parent[i] != i) { parent[i] <<- parent[parent[i]]; i <- parent[i] }; i }
  e <- unique(data.frame(a = match(a, nodes), b = match(b, nodes)))
  for (k in seq_len(nrow(e))) {
    ra <- find(e$a[k]); rb <- find(e$b[k])
    if (ra != rb) parent[ra] <- rb
  }
  length(unique(vapply(seq_along(nodes), find, 1L)))
}
