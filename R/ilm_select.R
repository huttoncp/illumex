## ---------------------------------------------------------------------------
## Choosing columns without non-standard evaluation.
##
## Naming forty of two hundred columns as a character vector is the one place
## the string-based API genuinely hurts. The tidyselect answer to that is
## data masking, which buys the convenience at the cost of an rlang dependency,
## of making the package harder to drive FROM code -- illume's own study scripts
## loop over column names -- and of silent resolution when a bare name also
## exists in the calling environment.
##
## None of that is necessary. A `cols` argument can take a character vector, a
## regular expression, or a predicate function, and all three are ordinary
## values: assignable to a variable, passable through `...`, and printable. The
## expressive part of tidyselect is the SELECTION, not the masking.
## ---------------------------------------------------------------------------

#' How columns can be chosen
#'
#' Wherever illumex takes a `cols` argument, it accepts any of four things.
#' All are ordinary values, so any of them can be held in a variable and
#' passed along -- which a bare-name interface cannot.
#'
#' * `NULL`, meaning every eligible column.
#' * A **character vector** of names: `c("age", "income")`. Every name must
#'   exist, and a typo is an error naming the columns that do exist.
#' * A **regular expression**, as a single string that is not itself a column
#'   name: `"^score_"` takes every column whose name starts with `score_`.
#'   Matching nothing is an error rather than a silent empty selection.
#' * A **predicate function** applied to each column: `is.numeric`, or
#'   `function(v) is.numeric(v) && !anyNA(v)`. A column on which the function
#'   fails counts as not matching.
#'
#' The one ambiguity is a single string, which could be a name or a pattern.
#' A name wins: `cols = "score"` selects the column called `score` even if
#' `scores_2024` also exists. Anything that is not a column name is read as a
#' pattern.
#'
#' **Leaving columns out.** With `cols_negate = TRUE`, `cols` says which columns to
#' leave out, and every other eligible column is used: `cols = c("id",
#' "site")` uses all but `id` and `site`; `cols = "^score_"` all but the
#' columns whose names start with `score_`; `cols = is.numeric` all but the
#' numeric columns, which is every column of another type. The selection is
#' made within the columns the function can use (a numeric-only function
#' leaves out text columns either way), so `cols_negate = TRUE` takes exactly
#' the eligible columns that `cols_negate = FALSE` would not. A column on which a
#' predicate fails counts as not matching, so `cols_negate = TRUE` selects
#' it. `cols_negate = TRUE` needs `cols`, and leaving out every eligible column is an
#' error naming them.
#'
#' **Matching literally.** With `cols_fixed = TRUE`, a `cols` string read as
#' a pattern is matched as it is written, as a substring: `cols = "wt.",
#' cols_fixed = TRUE` takes `wt.kg` and `wt.lb` but not `wt_2`. A column name
#' still wins, and names or a predicate are unaffected.
#'
#' **Choosing rows.** A function that takes `subset` uses only the rows it
#' gives, before `cols`, `by` and everything else -- `dplyr::filter()` before
#' the call, as an ordinary value. `subset` is one of:
#'
#' * A **logical vector**, one value per row: `subset = d$age >= 18`. Rows
#'   where it is `NA` are left out, as in `dplyr::filter()`.
#' * **Row positions**, positive whole numbers: `subset = 1:100`. A number on
#'   its own always means a position.
#' * **Named patterns**, one per column: `subset = c(site = "^north", arm =
#'   "drug")` keeps the rows where every named column matches; a missing
#'   value matches nothing. `subset_fixed = TRUE` matches them literally.
#' * A **random sample**, [ilm_sample()]: `subset = ilm_sample(prop = 0.2,
#'   seed = 1)`, or whole groups with `by`.
#' * A **model**, for the rows it analysed: a fit from illume's `ilm_model()`,
#'   whose dropped rows are left out, or an `ilm_dag_model()`, whose
#'   adjustment sets can drop different rows, for the rows every set used.
#'   `data` must be the data the model was fitted to. A table of
#'   characteristics beside the model's estimates describes these rows:
#'   `ilm_describe_all(d, subset = fit)`.
#'
#' `subset_negate = TRUE` takes the rows `subset` would not: the other rows,
#' the rows not sampled (a holdout), or the rows a model dropped -- the check
#' on whether they differ from the rows it analysed. For a logical `subset`, rows where it
#' is `NA` stay out either way. A subset that keeps no row is an error saying
#' what it was. Results name rows by the data's own row numbers, never by
#' their place in the subset, so they join back onto the data as they are.
#' [ilm_subset()] returns the rows and columns themselves.
#'
#' A result made from a subset, or from a choice of columns, says so above
#' what it prints -- `119 of 600 rows (subset)`, `297 of 300 rows (analysed
#' by the model)` and `Columns: 5 of 8 (excluded: id, name, date)` -- and
#' keeps what was chosen in its attribute `"ilm_select"`, with, for a model,
#' copies of what identifies each fit (formula, rows given, rows dropped and
#' any subset), to check against it with `identical()`. A plot says the same in a message.
#'
#' A `by` argument takes column names. [ilm_outliers_all()]'s `by` also takes
#' a pattern or a predicate, as `cols` does; elsewhere a pattern or a function
#' given as `by` is an error. `by` is never negated.
#'
#' @name ilm_selection
#' @examples
#' d <- data.frame(id = 1:5, score_a = rnorm(5), score_b = rnorm(5),
#'                 label = letters[1:5])
#' ilm_outliers_all(d, cols = "^score_")
#' ilm_outliers_all(d, cols = is.numeric)
#' ## leaving columns out
#' ilm_outliers_all(d, cols = "id", cols_negate = TRUE)
#' ilm_outliers_all(d, cols = "^score_", cols_negate = TRUE)
#' ilm_outliers_all(d, cols = function(v) all(v == round(v)), cols_negate = TRUE)
#' ## choosing rows
#' ilm_outliers_all(d, subset = d$id > 2)
#' ilm_outliers_all(d, subset = c(label = "^[ab]$"), subset_negate = TRUE)
#' ilm_outliers_all(d, subset = ilm_sample(3, seed = 1))
NULL

#' @keywords internal
#' @noRd
ilm_resolve_cols <- function(data, cols, exclude = character(),
                             arg = "cols", eligible = NULL, negate = FALSE,
                             fixed = FALSE) {
  nms <- setdiff(names(data), exclude)
  if (!is.null(eligible)) nms <- intersect(nms, eligible)
  if (isTRUE(negate) && is.null(cols))
    stop("`", arg, "_negate = TRUE` needs `", arg, "` to say which columns to leave out",
         call. = FALSE)
  if (is.null(cols)) return(nms)
  hit <- ilm_select_matches(data, cols, nms, arg, negate = isTRUE(negate),
                            fixed = isTRUE(fixed))
  if (!isTRUE(negate)) return(hit)
  ## the eligible columns the same `cols` would not select
  out <- setdiff(nms, hit)
  if (!length(out))
    stop("`", arg, "_negate = TRUE` left no column to use: `", arg, "` covers every eligible ",
         "column (", paste(nms, collapse = ", "), ")", call. = FALSE)
  out
}

## the eligible columns (`nms`) that `cols` selects. A typo'd name is an error
## either way; an empty selection is an error unless it is about to be negated
## (a predicate that matches nothing, or names only of columns not eligible,
## leave every column in)
#' @keywords internal
#' @noRd
ilm_select_matches <- function(data, cols, nms, arg, negate = FALSE, fixed = FALSE) {
  if (is.function(cols)) {
    keep <- vapply(nms, function(v) {
      ok <- tryCatch(cols(data[[v]]), error = function(e) FALSE)
      isTRUE(ok)
    }, TRUE)
    out <- nms[keep]
    if (!length(out) && !negate)
      stop("`", arg, "` is a function that matched no column", call. = FALSE)
    return(out)
  }

  if (!is.character(cols))
    stop("`", arg, "` must be column names, a regular expression, or a ",
         "function; it is ", class(cols)[1], call. = FALSE)

  ## every element a real column name: take them as names
  known <- cols %in% names(data)
  if (all(known)) {
    out <- intersect(cols, nms)
    if (!length(out) && !negate)
      stop("`", arg, "` named only columns that are excluded here: ",
           paste(cols, collapse = ", "), call. = FALSE)
    return(out)
  }
  ## a single string that is not a column name: read it as a pattern
  if (length(cols) == 1L) {
    out <- tryCatch(grep(cols, nms, value = TRUE, fixed = fixed),
                    error = function(e)
                      stop("`", arg, "` is neither a column name nor a valid ",
                           "regular expression: ", cols, call. = FALSE))
    ## A single unknown string is ambiguous: a typo'd name and a pattern that
    ## matches nothing look identical from here, so the message covers both
    ## readings rather than guessing which the user meant -- negated or not,
    ## since leaving out a typo would silently leave out nothing.
    if (!length(out))
      stop("column not found in the data, and matched nothing as a pattern ",
           "either: ", cols, ". Available: ",
           paste(utils::head(nms, 12), collapse = ", "), call. = FALSE)
    return(out)
  }
  stop("column(s) not found in the data: ",
       paste(cols[!known], collapse = ", "), ". Available: ",
       paste(utils::head(names(data), 12), collapse = ", "), call. = FALSE)
}

## A `by` that is not ilm_outliers_all()'s takes column names only: a
## function, or a string that is not a column (a pattern, or a typo), is one
## clear error rather than a failure deep inside the function
#' @keywords internal
#' @noRd
ilm_check_by <- function(data, by, arg = "by") {
  if (is.null(by)) return(invisible(by))
  if (is.function(by))
    stop("`", arg, "` takes column names; a function is not one", call. = FALSE)
  if (!is.character(by))
    stop("`", arg, "` takes column names; it is ", class(by)[1], call. = FALSE)
  bad <- by[!by %in% names(data)]
  if (length(bad))
    stop("`", arg, "` takes column names; ", paste(bad, collapse = ", "),
         if (length(bad) == 1L) " is not one" else " are not", ". Available: ",
         paste(utils::head(names(data), 12), collapse = ", "), call. = FALSE)
  invisible(by)
}
