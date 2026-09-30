## ---------------------------------------------------------------------------
## Choosing rows (items 277 and 279).
##
## `subset` is dplyr::filter() before the call, as an ordinary value: a
## logical vector, row positions, named patterns, or a random sample from
## ilm_sample(). It runs first, before `cols`, `by` and everything else, and
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
#' of them. With `by`, whole groups are drawn instead: `n` and `prop` then
#' count groups, and every row of a group drawn is kept, so a grouped
#' holdout (`subset_negate = TRUE`) never splits a group.
#'
#' The draw follows the family's rule for random numbers: with a `seed`, the
#' same rows every time, and your own random stream is left as it was; with
#' `seed = NULL`, it draws from your stream as any R code does. The result
#' keeps the seed and the generator's kind.
#'
#' A number given to `subset` on its own always means row positions:
#' `subset = 10` is row 10, and `subset = ilm_sample(10)` is ten rows drawn.
#'
#' @param n A number of rows (or of groups, with `by`) to draw: a whole
#'   number, at most the number there are.
#' @param prop A share of rows (or of groups) to draw, strictly between 0
#'   and 1; the count is rounded, and must come to at least 1.
#' @param by A column whose values are the groups to draw whole; `NULL` draws
#'   rows. A missing value is a group of its own.
#' @param seed An integer seed for the draw, or `NULL`.
#' @return An object of class `"ilm_sample"`, which does nothing until it is
#'   given as `subset`.
#' @seealso [ilm_selection] for `subset` and `cols`, [ilm_subset()] for the
#'   data itself.
#' @examples
#' d <- ilm_sim()
#' ilm_describe(d, "score", subset = ilm_sample(prop = 0.5, seed = 1))
#' ## a grouped holdout: the ids not drawn
#' ilm_describe(d, "score", subset = ilm_sample(5, by = "id", seed = 1),
#'              subset_negate = TRUE)
#' @export
ilm_sample <- function(n = NULL, prop = NULL, by = NULL, seed = NULL) {
  if (is.null(n) == is.null(prop))
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
    stop("`by` must name one column", call. = FALSE)
  if (!is.null(seed) && !(is.numeric(seed) && length(seed) == 1L && is.finite(seed)))
    stop("`seed` must be a single number, or NULL", call. = FALSE)
  structure(list(n = if (!is.null(n)) as.integer(n), prop = prop, by = by,
                 seed = if (!is.null(seed)) as.integer(seed)),
            class = "ilm_sample")
}

#' @export
print.ilm_sample <- function(x, ...) {
  what <- if (!is.null(x$n)) paste(x$n, if (is.null(x$by)) "rows" else "groups")
          else paste0(format(100 * x$prop), "% of ", if (is.null(x$by)) "rows" else "groups")
  cat("<ilm_sample> ", what, if (!is.null(x$by)) paste0(" of ", x$by), ", seed ",
      if (is.null(x$seed)) "none" else x$seed, "\n", sep = "")
  invisible(x)
}

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
  if (inherits(subset, "ilm_sample")) {
    if (!allow_sample)
      stop("`subset` cannot be a random sample here", if (!is.null(fn)) paste0(" (", fn, ")"),
           ": give the rows as a condition, positions or patterns", call. = FALSE)
    s <- ilm_draw_sample(data, subset)
    keep <- s$keep
    info$form <- "sample"
    info$sample <- s$sample
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
    stop("`subset` must be a logical vector, row positions, named patterns or ",
         "ilm_sample(); it is ", class(subset)[1], call. = FALSE)
  }
  if (negate) keep <- !keep
  rows <- which(keep)
  if (!length(rows))
    stop("`subset` kept no rows: ", ilm_subset_describe(subset, info), call. = FALSE)
  info$n_rows_kept <- length(rows)
  list(rows = rows, info = info[c("form", "negate", "n_rows_given", "n_rows_kept",
                                  intersect(c("patterns", "sample"), names(info)))])
}

## the rows an ilm_sample() draws, and an account of the draw: its size, its
## groups, and its seed with the generator's kinds
#' @keywords internal
#' @noRd
ilm_draw_sample <- function(data, s) {
  N <- nrow(data)
  if (!is.null(s$by) && !s$by %in% names(data))
    stop("ilm_sample(by = ) names a column not in the data: ", s$by, call. = FALSE)
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
  ## the family's rule: a seed draws the same every time and puts the user's
  ## stream back; no seed draws from the stream as any R code does
  kinds <- RNGkind()
  if (!is.null(s$seed)) {
    ilm_rng_restore(s$seed)
    set.seed(s$seed)
  }
  chosen <- sample.int(K, k)
  list(keep = units %in% chosen,
       sample = c(if (!is.null(s$n)) list(n = s$n), if (!is.null(s$prop)) list(prop = s$prop),
                  if (!is.null(s$by)) list(by = s$by),
                  list(seed = c(if (!is.null(s$seed)) list(value = s$seed),
                                list(kind = kinds[1], normal_kind = kinds[2],
                                     sample_kind = kinds[3])))))
}

## a subset in words, for an error that says what it was
#' @keywords internal
#' @noRd
ilm_subset_describe <- function(subset, info) {
  base <- switch(info$form,
    sample = "the sample drawn",
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
  c(list(form = form), spec,
    list(negate = isTRUE(negate), fixed = isTRUE(fixed),
         columns_excluded = as.list(setdiff(names(data), c(used, by)))))
}

## The two lines a result prints above itself when a call chose its rows or
## columns: "119 of 600 rows (subset)" and "Columns: 5 of 8 (excluded: ...)".
#' @keywords internal
#' @noRd
ilm_select_lines <- function(sel) {
  if (is.null(sel)) return(character())
  s <- sel$subset; c_ <- sel$selection
  rows <- if (!is.null(s))
    sprintf("%s of %s rows (subset%s)", format(s$n_rows_kept, big.mark = ","),
            format(s$n_rows_given, big.mark = ","), if (isTRUE(s$negate)) ", negated" else "")
  cols <- if (!is.null(c_)) {
    ex <- unlist(c_$columns_excluded)
    shown <- utils::head(ex, 8L)
    paste0("Columns: ", sel$n_cols_used, " of ", sel$n_cols_used + length(ex),
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
  list(data = out, rows = r$rows, subset = r$info)
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
  sel <- list(subset = rs$subset,
              selection = ilm_selection_info(data, cols, use, negate = cols_negate,
                                             fixed = cols_fixed),
              n_cols_used = length(use))
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
    selection = ilm_selection_info(data, cols, used, by, cols_negate, cols_fixed),
    n_cols_used = length(used)))
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
                ilm_selection_info(data, cols, used, by, cols_negate, cols_fixed),
              n_cols_used = length(used))
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
