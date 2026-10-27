## ---------------------------------------------------------------------------
## Data whose rows sit in clusters -- patients within sites, pupils within
## schools, repeated visits within a person -- need two tables of
## characteristics, one of the rows and one of the clusters. A variable that
## is the same on every row of a cluster (a site's region, a school's size)
## describes the cluster; described over the rows, a cluster of 200 rows
## would count 200 times and a cluster of 2 twice. So such a variable is
## described once per cluster, beside how many clusters there are and how
## many rows each has (Craig's item 121).
## ---------------------------------------------------------------------------

## How the printed summary reads, one constant each (Craig's item 147)
ILM_CLUSTERS_COUNT <- "Clusters: %s by `%s`, %s rows"
ILM_CLUSTERS_NONE <- " (%s without a cluster, left out)"
ILM_CLUSTERS_SIZES <- "Rows per cluster: mean %s, median %s (the middle 50%% between %s and %s), from %s to %s"
ILM_CLUSTERS_SAME <- "Rows per cluster: %s in every cluster"
ILM_CLUSTERS_ONCE <- "Described once per cluster (the same on every row of a cluster):"
ILM_CLUSTERS_VARY <- "Vary within clusters, so described over rows by ilm_describe_all(): %s"

#' Describe data whose rows sit in sampling clusters
#'
#' Counts the clusters and the rows in each, and describes each variable
#' that is the same on every row of a cluster -- a site's region, a school's
#' size -- once per cluster rather than once per row, where a large cluster
#' would count many times over. The variables that vary within clusters are
#' named, for [ilm_describe_all()] to describe over the rows.
#'
#' A variable belongs to the cluster when no cluster has two different values
#' of it; a missing value on some of a cluster's rows does not count against
#' that, and the cluster takes its value from the rows that have it. Rows
#' without a cluster are left out, and counted.
#'
#' @param data A data frame.
#' @param cluster The name of the column that says which cluster each row is
#'   in.
#' @param cols The columns to consider; every column but `cluster` by default.
#'   See [ilm_selection] for the forms it takes.
#' @param digits Rounding for printing; the values are kept whole.
#' @param ... Passed to [ilm_describe_all()] for the variables described once
#'   per cluster: `gauss`, `probs` and the like.
#' @param cols_negate,cols_fixed How `cols` is read; see [ilm_selection].
#' @param subset,subset_negate,subset_fixed Which rows; see [ilm_selection].
#'   A model given as `subset` describes the clusters of the rows it analysed.
#' @return An `"ilm_describe_clusters"` object, a list of `clusters` (one row:
#'   the cluster column, the number of clusters, of rows and of rows without a
#'   cluster, and the rows per cluster: their mean, minimum, quartiles and
#'   maximum), `cluster_level` ([ilm_describe_all()] of the variables that
#'   belong to the cluster, one row per cluster, or `NULL` when there are
#'   none), and the names of those variables (`cluster_vars`) and of the
#'   variables that vary within a cluster (`row_vars`).
#' @seealso [ilm_describe_all()] for the rows.
#' @examples
#' d <- ilm_sim(n_id = 30, n_period = 6)
#' ilm_describe_clusters(d, "id")
#' @export
ilm_describe_clusters <- function(data, cluster, cols = NULL, digits = 3, ...,
                                  cols_negate = FALSE, cols_fixed = FALSE, subset = NULL,
                                  subset_negate = FALSE, subset_fixed = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.character(cluster) || length(cluster) != 1L || !cluster %in% names(data))
    stop("`cluster` must name one column of `data`.", call. = FALSE)
  ## the rows first, then the columns; the cluster column is never described
  rsel <- ilm_select_rows(data, subset, subset_negate, subset_fixed)
  data <- rsel$data
  use <- ilm_resolve_cols(data, cols, exclude = cluster, negate = cols_negate,
                          fixed = cols_fixed)
  has <- !is.na(data[[cluster]])
  if (!any(has))
    stop("no row has a cluster: `", cluster, "` is missing throughout.", call. = FALSE)
  d <- data[has, , drop = FALSE]
  g <- factor(d[[cluster]])
  sizes <- as.vector(table(g))
  q <- fquantile(sizes, c(0.25, 0.5, 0.75))
  clusters <- data.frame(cluster = cluster, n_clusters = nlevels(g), n_rows = nrow(d),
                         n_no_cluster = sum(!has), rows_mean = mean(sizes),
                         rows_min = min(sizes), rows_p25 = q[[1L]], rows_p50 = q[[2L]],
                         rows_p75 = q[[3L]], rows_max = max(sizes), stringsAsFactors = FALSE)
  ## the cluster's own: at most one value in every cluster, missing aside
  own <- vapply(use, function(v) all(fndistinct(d[[v]], g = g) <= 1L), TRUE)
  lev <- use[own]
  cluster_level <- if (length(lev)) {
    per <- ffirst(d[lev], g = g)
    rownames(per) <- NULL
    ilm_describe_all(per, digits = digits, ...)
  }
  res <- list(clusters = clusters, cluster_level = cluster_level, cluster_vars = lev,
              row_vars = use[!own])
  attr(res, "digits") <- digits
  ilm_select_finish(ilm_as_result(res, "ilm_describe_clusters"), data, rsel, cols, use,
                    cluster, cols_negate, cols_fixed)
}

#' @export
print.ilm_describe_clusters <- function(x, ...) {
  s <- x$clusters
  ## counts whole; the mean an estimate; the quartiles data values
  k <- ilm_fmt_count
  cat(sprintf(ILM_CLUSTERS_COUNT, k(s$n_clusters), s$cluster, k(s$n_rows)),
      if (s$n_no_cluster > 0) sprintf(ILM_CLUSTERS_NONE, k(s$n_no_cluster)), "\n", sep = "")
  cat(if (s$rows_min == s$rows_max) sprintf(ILM_CLUSTERS_SAME, k(s$rows_min))
      else sprintf(ILM_CLUSTERS_SIZES, ilm_fmt_num(s$rows_mean), ilm_fmt_data(s$rows_p50),
                   ilm_fmt_data(s$rows_p25), ilm_fmt_data(s$rows_p75), k(s$rows_min),
                   k(s$rows_max)), "\n", sep = "")
  if (!is.null(x$cluster_level)) {
    cat("\n", ILM_CLUSTERS_ONCE, "\n", sep = "")
    print(x$cluster_level, ...)
  }
  if (length(x$row_vars))
    cat("\n", ilm_wrap(sprintf(ILM_CLUSTERS_VARY, paste(x$row_vars, collapse = ", ")), 76L, ""),
        "\n", sep = "")
  invisible(x)
}
