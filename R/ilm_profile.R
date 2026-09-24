## ---------------------------------------------------------------------------
## Reduce, cluster, then say what each cluster IS.
##
## A cluster number tells you nothing. What is wanted is "cluster 2 is the
## retirees: 86% retired, the middle half aged 64 to 72, living alone or in
## twos", and that comes from comparing each cluster with all rows on the
## original variables -- see ilm_profile_describe.R for how, and for why not
## through the reduction's dimensions any more.
##
## The same description serves the missingness variant, with present/missing
## markers for variables: "income is missing for 92% of them".
## ---------------------------------------------------------------------------

#' Profile a data set: reduce, cluster, and describe the clusters
#'
#' Runs [ilm_reduce()], then [ilm_cluster()] on the dimensions it produces,
#' then says what sets each cluster apart, in the variables' own units -- a
#' paragraph per cluster such as "employment is 'retired' for 86% of them,
#' against 21% overall; age is higher: the middle half 64 to 72, against 29
#' to 54 overall". The aim is the one profiling exists for: to find the
#' subgroups in a sample and learn what each of them is.
#'
#' @section How a cluster gets described:
#'
#' Each cluster is compared with all rows on every variable the clustering
#' used. A number is described by its middle half (its quartiles) in the
#' cluster against the same among all rows; a category by the value whose
#' share in the cluster sits furthest from its share among all rows; a date by
#' its middle half, and by the stretch of any cycle the reduction used (see
#' `time`) where the cluster stands out -- "falls on Sat-Sun for 87% of them,
#' against 50% overall".
#'
#' The variables are ranked by a v-test -- how far the cluster's mean or share
#' sits from everyone's, in standard errors of a subset that size (Lebart's,
#' as in FactoMineR's `catdes()`) -- and named, strongest first, up to
#' `top_n_vars`, when the v-test clears `vtest_threshold` and the difference
#' is big enough to matter: 0.2 standard deviations for a number, 10
#' percentage points for a share. With many rows a chance difference clears
#' 1.96 on its own: on 1,200 rows, a variable that was noise by construction
#' did in two clusters of three, by 3 and 6 points. The variables that set no
#' cluster apart are named after the paragraphs, or counted when there are
#' many; every value of every categorical variable, per cluster, is in
#' `frequencies`.
#'
#' It is **not a p-value**. The clusters were found from these same
#' variables, so the ones the clustering used will differ between the
#' clusters they helped make. Treat a profile as a description of the
#' partition you have, not as evidence that the partition is real -- the
#' stability and silhouette figures in [ilm_cluster()] are what speak to that.
#'
#' @inheritParams ilm_reduce
#' @param time What to do with date and date-time columns; passed to
#'   [ilm_reduce()], which describes the choices.
#' @param ... Passed to [ilm_cluster()], for instance `k`, `k_max`, `method`,
#'   `B` or `seed`.
#' @param vtest_threshold Smallest `|v-test|` for a variable to be named in a
#'   cluster's description.
#' @param top_n_vars Most variables to name for one cluster.
#' @param var_contrib Run [ilm_var_contrib()] on the result and report it.
#'   Nothing in this pipeline selects variables, and an irrelevant one is not
#'   neutral: on three known clusters with two informative columns, adding two
#'   pure-noise factors took recovery from 0.301 to 0.006. So the check runs
#'   here rather than waiting to be asked for, and a clustering that turns out
#'   to be one variable's levels under another name raises a warning -- the
#'   cluster descriptions would otherwise be read at face value, and they
#'   would all be true and all about that variable. Costs roughly 70% on top
#'   of the clustering at 500 rows and five columns.
#' @param var_contrib_B Permutations for that check.
#' @return An object of class `"ilm_profile"`: `reduce`, `cluster`,
#'   `characterization` (one row per cluster and variable -- and per aspect of
#'   a date -- with its v-test, the size of the difference and the words for
#'   it), `frequencies` (every value of every categorical variable, per
#'   cluster: its count, its share, and its share among all rows),
#'   `by_cluster` ([ilm_describe_all()] of the variables, by cluster),
#'   `var_contrib` (or `NULL`), `summary` (a paragraph per cluster) and
#'   `not_distinctive` (the variables that set no cluster apart).
#' @seealso [ilm_reduce()], [ilm_cluster()], [ilm_plot_profile()],
#'   [ilm_profile_na()].
#' @examples
#' p <- ilm_profile(mtcars, k_max = 5, B = 25, seed = 1)
#' cat(p$summary, sep = "\n")
#' @export
ilm_profile <- function(data, cols = NULL, ndim = 5,
                        method = c("pcamix", "glrm"),
                        time = c("cycles", "elapsed", "drop"), ...,
                        vtest_threshold = 1.96, top_n_vars = 4,
                        var_contrib = TRUE, var_contrib_B = 199L) {
  ## An ilm_anomaly() result is accepted directly: the flagged rows are what
  ## the user wants to look at next, and rebuilding that subset by hand from
  ## `row` is both a papercut and a chance to line the wrong rows up.
  if (inherits(data, "ilm_anomaly"))
    data <- ilm_from_anomaly(data, "ilm_profile")
  rr <- ilm_reduce(data, cols = cols, ndim = ndim, method = method,
                   time = match.arg(time))
  cr <- ilm_cluster(rr, ...)
  rows <- cr$ind_cluster$row_id
  dsub <- if (!is.null(rows) && length(rows) == nrow(cr$ind_cluster) &&
              max(rows) <= nrow(data)) data[rows, , drop = FALSE] else data
  cl <- cr$ind_cluster$cluster
  ## Run the variable check HERE rather than leaving it for the user to find.
  ## Nothing in this pipeline selects variables, and an irrelevant one is not
  ## neutral: on three known clusters with two informative columns, adding two
  ## pure-noise factors took recovery from 0.301 to 0.006. A diagnostic in a
  ## function nobody calls produces exactly the analyses this package exists
  ## to prevent, so it goes in the path and its verdict is printed.
  ##
  ## It is cheap -- eta squared and Cramer's V on permuted labels, no models
  ## refitted -- so it costs a fraction of the clustering it follows.
  vc <- if (isFALSE(var_contrib)) NULL else {
    tryCatch(ilm_var_contrib(structure(list(reduce = rr, cluster = cr),
                                       class = "ilm_profile"),
                             dsub, B = var_contrib_B),
             error = function(e) NULL)
  }
  used <- dsub[ilm_profile_cols(rr, dsub)]
  de <- ilm_profile_describe(used, cl, rr, cr, top_n_vars, vtest_threshold)
  out <- structure(list(reduce = rr, cluster = cr,
                        characterization = de$characterization,
                        frequencies = de$frequencies,
                        by_cluster = ilm_profile_by_cluster(used, cl),
                        var_contrib = vc, summary = de$summary,
                        not_distinctive = de$not_distinctive),
                   class = "ilm_profile")
  ## The dominance case is a warning rather than a line of output: a clustering
  ## that is one variable's levels under another name will otherwise have its
  ## cluster descriptions read at face value, and they will all be true and all
  ## be about that one variable.
  if (!is.null(vc) && !is.null(attr(vc, "dominated_by")))
    warning("this clustering is a re-labelling of `", attr(vc, "dominated_by"),
            "`: it separates the clusters almost perfectly while nothing else ",
            "does. If that is not the grouping you were looking for, exclude ",
            "it with cols =. See the variable contribution table in the ",
            "printed output.", call. = FALSE)
  out
}

#' Profile which values are missing, and for whom
#'
#' The missingness counterpart to [ilm_profile()]: runs [ilm_reduce_na()] then
#' [ilm_cluster_na()], and describes each cluster of rows by which columns'
#' *missingness* sets it apart -- "income is missing for 92% of them, against
#' 18% overall" -- rather than by those columns' values.
#'
#' This is how a structured gap shows itself: a block of variables that go
#' missing together points at a shared cause, such as a section of a form
#' everyone in one group skipped, which is a different problem from values
#' going missing one at a time. [ilm_check_missing()] then says whether any of
#' it threatens the model you intend to fit.
#'
#' @inheritParams ilm_reduce_na
#' @param ... Passed to [ilm_cluster_na()].
#' @param vtest_threshold Smallest `|v-test|` for a column to be named in a
#'   cluster's description; see [ilm_profile()].
#' @param top_n_vars Most columns to name for one cluster.
#' @return An object of class `"ilm_profile_na"`, which is also an
#'   `"ilm_profile"`.
#' @seealso [ilm_check_missing()], `illume::ilm_impute()`, [ilm_profile()].
#' @examples
#' p <- ilm_profile_na(airquality, k_max = 4, B = 25, seed = 1)
#' cat(p$summary, sep = "\n")
#' @export
ilm_profile_na <- function(data, cols = NULL, ndim = 5, ...,
                           vtest_threshold = 1.96, top_n_vars = 4) {
  if (inherits(data, "ilm_anomaly"))
    data <- ilm_from_anomaly(data, "ilm_profile_na")
  rr <- ilm_reduce_na(data, cols = cols, ndim = ndim)
  cr <- ilm_cluster_na(rr, ...)
  ## the present/missing markers the clustering was built from
  marks <- as.data.frame(lapply(rr$cols, function(cn) is.na(data[[cn]])))
  names(marks) <- rr$cols
  de <- ilm_profile_describe(marks, cr$ind_cluster$cluster, rr, cr,
                             top_n_vars, vtest_threshold, missing = TRUE)
  structure(list(reduce = rr, cluster = cr,
                 characterization = de$characterization,
                 frequencies = de$frequencies, summary = de$summary,
                 not_distinctive = de$not_distinctive),
            class = c("ilm_profile_na", "ilm_profile"))
}

#' @export
print.ilm_profile <- function(x, ...) {
  cat(if (inherits(x, "ilm_profile_na"))
        "<ilm_profile_na>  (profiling the pattern of missing values)"
      else "<ilm_profile>", "\n\n")
  print(x$reduce)
  cat("\n")
  print(x$cluster)
  cat("\n  what each cluster is\n")
  for (s in x$summary) cat(ilm_wrap(s, 76L, "    "), "\n\n")
  ## what the paragraphs leave out, said once rather than left to be wondered at
  closing <- ilm_profile_closing(x$not_distinctive,
                                 attr(x$not_distinctive, "of"))
  if (!is.null(closing)) cat(ilm_wrap(closing, 76L, "    "), "\n\n")
  ## The cluster descriptions above are always true OF THE CLUSTERS FOUND.
  ## Whether those clusters are worth describing is a separate question, and
  ## this is where it gets answered rather than left to a function the reader
  ## would have to know to call.
  if (!is.null(x$var_contrib)) {
    v <- x$var_contrib
    dom <- attr(v, "dominated_by")
    dead <- v$variable[v$verdict == "no better than chance"]
    if (!is.null(dom)) {
      cat("  ! this clustering is a re-labelling of `", dom, "`: it separates\n",
          "    the clusters almost perfectly while nothing else does, so the\n",
          "    descriptions above are all about that one variable. Exclude it\n",
          "    with cols = if it is not the grouping you wanted.\n", sep = "")
    } else if (length(dead)) {
      cat("  ! ", paste(dead, collapse = ", "), " separate",
          if (length(dead) == 1L) "s" else "",
          " the clusters no better than a\n",
          "    shuffled label does, and nothing here selects variables, so ",
          if (length(dead) == 1L) "it is" else "they are", "\n",
          "    still contributing distance. Refit with cols = to drop ",
          if (length(dead) == 1L) "it." else "them.", "\n", sep = "")
    }
    cat("    ilm_var_contrib() for the full table.\n")
  }
  invisible(x)
}
