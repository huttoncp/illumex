## ---------------------------------------------------------------------------
## Named EDA plots, one per geometry.
##
## `ilm_plot()` picks a geometry for you and takes `geom =` to override it.
## These are the same drawing done the other way round: you name the plot you
## want. Both sit on tinyplot, which is base graphics with no dependency cost
## beyond what illumex already carries, and columns are named as strings
## throughout, the way every other ilm_ function takes them.
##
## No interactivity and no pie chart, both dropped deliberately.
## ---------------------------------------------------------------------------

## Resolve a column argument that may be NULL, returning the vector.
#' @keywords internal
#' @noRd
ilm_col_vec <- function(data, nm, arg, required = FALSE, discrete = FALSE) {
  if (is.null(nm)) {
    if (required)
      stop("`", arg, "` must name a column of `data`", call. = FALSE)
    return(NULL)
  }
  if (!is.character(nm) || length(nm) != 1L)
    stop("`", arg, "` must be a single column name, as a string", call. = FALSE)
  if (!nm %in% names(data))
    stop("column not found in the data: ", nm, ". Available: ",
         paste(utils::head(names(data), 12), collapse = ", "), call. = FALSE)
  v <- data[[nm]]
  ## A grouping column for a discrete geometry is a factor whatever it is
  ## stored as. Handing tinyplot a numeric one makes it try a continuous
  ## legend, fail, and warn on every call.
  if (discrete && !is.null(v) && !is.factor(v)) v <- factor(v)
  v
}

#' @keywords internal
#' @noRd
ilm_plot_frame_check <- function(data) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
}

## `facet` names a column, as `by` does (Craig's item 203). tinyplot's own
## formula form is turned away with the string form it should be.
#' @keywords internal
#' @noRd
ilm_facet_vec <- function(data, facet) {
  if (inherits(facet, "formula")) {
    v <- all.vars(facet)
    stop("`facet` takes a column name, as a string, the way `by` does: facet = \"",
         if (length(v)) v[1] else "<column>", "\"",
         if (length(v) > 1L) " (one column)", call. = FALSE)
  }
  ilm_col_vec(data, facet, "facet", discrete = TRUE)
}

## Columns reach tinyplot as vectors, and tinyplot titles a `by` legend with
## the code that made the vector. The legend is titled with the column's name
## instead, unless the caller gave a title or no legend. `expr` is the
## caller's `legend` unevaluated, so tinyplot's own legend("bottom!", ...)
## form is read as tinyplot reads it rather than drawn. The result is passed
## to tinyplot as a variable, which tinyplot evaluates.
#' @keywords internal
#' @noRd
ilm_by_legend <- function(expr, env, by) {
  val <- if (is.call(expr) && identical(expr[[1L]], as.name("legend"))) {
    a <- lapply(as.list(expr)[-1L], eval, envir = env)
    if (length(a) && (is.null(names(a)) || !nzchar(names(a)[1L])))
      names(a)[1L] <- "x"
    a
  } else eval(expr, env)
  if (is.null(by) || isFALSE(val)) return(val)
  if (is.null(val)) return(list(title = by))
  if (is.character(val)) return(list(x = val, title = by))
  if (is.list(val) && is.null(val$title)) val$title <- by
  val
}

#' Histogram
#'
#' @param data A data frame.
#' @param x Name of the numeric column to plot.
#' @param by Optional grouping column, overlaying one histogram per level.
#' @param facet Optional column to draw in panels, one per level, named as a
#'   string the way `by` is: `facet = "site"`.
#' @param legend Passed to [tinyplot::tinyplot()], in any form it takes. With
#'   `by`, the legend is titled with the `by` column's name unless you
#'   give a title.
#' @param breaks Passed to [tinyplot::type_histogram()].
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return `NULL`, invisibly.
#' @seealso [ilm_plot()], which picks a geometry for you.
#' @examples
#' ilm_plot_histogram(mtcars, "mpg")
#' ilm_plot_histogram(mtcars, "mpg", by = "cyl")
#' ilm_plot_histogram(mtcars, "mpg", facet = "am")
#' @export
ilm_plot_histogram <- function(data, x, by = NULL, breaks = "Sturges", ...,
                               facet = NULL, legend = NULL) {
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  tinyplot::tinyplot(x = ilm_col_vec(data, x, "x", TRUE),
                     by = ilm_col_vec(data, by, "by", discrete = TRUE),
                     legend = legend,
                     facet = ilm_facet_vec(data, facet),
                     type = tinyplot::type_histogram(breaks = breaks),
                     xlab = x, ...)
  invisible(NULL)
}

#' Density plot
#'
#' @inheritParams ilm_plot_histogram
#' @return `NULL`, invisibly.
#' @seealso [ilm_plot_histogram()].
#' @examples
#' ilm_plot_density(mtcars, "mpg", by = "cyl")
#' @export
ilm_plot_density <- function(data, x, by = NULL, ..., facet = NULL, legend = NULL) {
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  tinyplot::tinyplot(x = ilm_col_vec(data, x, "x", TRUE),
                     by = ilm_col_vec(data, by, "by", discrete = TRUE),
                     legend = legend,
                     facet = ilm_facet_vec(data, facet), type = "density",
                     xlab = x, ...)
  invisible(NULL)
}

#' Boxplot
#'
#' The whiskers use the same 1.5 interquartile-range fence that
#' [ilm_outliers()] scores against, so the points beyond them are the values
#' that function flags under `method = "iqr"`.
#'
#' @param data A data frame.
#' @param y Name of the numeric column.
#' @param x Optional categorical column to split by.
#' @param by Optional grouping column.
#' @param facet Optional column to draw in panels, one per level, named as a
#'   string the way `by` is: `facet = "site"`.
#' @param legend Passed to [tinyplot::tinyplot()], in any form it takes. With
#'   `by`, the legend is titled with the `by` column's name unless you
#'   give a title.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @param pch Plotting character. Takes a NAME as well as a number:
#'   `"filled circle"` is 16, and every code from 0 to 25 has one. Case,
#'   spaces, underscores and hyphens are ignored. A single character is
#'   drawn literally, so `pch = "x"` is still the letter x.
#' @return `NULL`, invisibly.
#' @seealso [ilm_outliers()], [ilm_plot_violin()].
#' @examples
#' ilm_plot_box(mtcars, "mpg", x = "cyl")
#' @export
ilm_plot_box <- function(data, y, x = NULL, by = NULL, ..., pch = NULL,
                         facet = NULL, legend = NULL) {
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  xv <- ilm_col_vec(data, x, "x")
  tinyplot::tinyplot(x = if (is.null(xv)) "" else xv,
                     y = ilm_col_vec(data, y, "y", TRUE),
                     by = ilm_col_vec(data, by, "by", discrete = TRUE),
                     legend = legend,
                     facet = ilm_facet_vec(data, facet),
                     type = "boxplot", xlab = x %||% "", ylab = y,
                     pch = ilm_pch(pch), ...)
  invisible(NULL)
}

#' Violin plot
#'
#' @inheritParams ilm_plot_box
#' @return `NULL`, invisibly.
#' @seealso [ilm_plot_box()], which shows the quartiles rather than the shape.
#' @examples
#' ilm_plot_violin(mtcars, "mpg", x = "cyl")
#' @export
ilm_plot_violin <- function(data, y, x = NULL, by = NULL, ..., pch = NULL,
                            facet = NULL, legend = NULL) {
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  xv <- ilm_col_vec(data, x, "x")
  tinyplot::tinyplot(x = if (is.null(xv)) "" else xv,
                     y = ilm_col_vec(data, y, "y", TRUE),
                     by = ilm_col_vec(data, by, "by", discrete = TRUE),
                     legend = legend,
                     facet = ilm_facet_vec(data, facet),
                     type = "violin", xlab = x %||% "", ylab = y,
                     pch = ilm_pch(pch), ...)
  invisible(NULL)
}

#' Scatter plot
#'
#' @section Trend bands:
#' Each trend is fitted separately in every group and panel and drawn over
#' the points on 100 values spanning that group's `x`, with a 95% band for the
#' fitted line:
#' * `"lm"`: a straight line; the band is the confidence interval for the
#'   mean from [stats::predict.lm()], with Student's t on the residual degrees
#'   of freedom (what tinyplot's own `"lm"` type draws).
#' * `"loess"`: [stats::loess()] at its defaults (span 0.75, degree 2); the
#'   band is the fit plus or minus t times loess's standard error, on loess's
#'   own degrees of freedom (what tinyplot's `"loess"` type draws).
#' * `"gam"`: `mgcv::gam(y ~ s(x), method = "REML")`, a smooth whose
#'   wiggliness is chosen from the data; the band is the fit plus or minus
#'   1.96 of mgcv's standard errors, mgcv's Bayesian credible interval, which
#'   holds close to 95% coverage averaged across the curve rather than at each
#'   point (Marra and Wood 2012). Where a group has fewer than ten distinct
#'   `x` values the smooth's basis is cut to that number; with fewer than
#'   four no line is drawn.
#'   Needs the mgcv package, which comes with R.
#'
#' A trend line is a description of the two columns shown and nothing more --
#' it holds nothing else fixed, so it is not the effect `illume::ilm_model()`
#' would estimate.
#'
#' @param data A data frame.
#' @param y,x Names of the numeric columns.
#' @param by Optional grouping column.
#' @param facet Optional column to draw in panels, one per level, named as a
#'   string the way `by` is: `facet = "site"`.
#' @param legend Passed to [tinyplot::tinyplot()], in any form it takes. With
#'   `by`, the legend is titled with the `by` column's name unless you
#'   give a title.
#' @param trend `"none"`, `"lm"`, `"loess"` or `"gam"`, drawn over the points
#'   with its band; see Trend bands.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @param pch Plotting character. Takes a NAME as well as a number:
#'   `"filled circle"` is 16, and every code from 0 to 25 has one. Case,
#'   spaces, underscores and hyphens are ignored. A single character is
#'   drawn literally, so `pch = "x"` is still the letter x.
#' @return `NULL`, invisibly.
#' @references
#' Marra, G. and Wood, S. N. (2012). Coverage properties of confidence
#' intervals for generalized additive model components. Scandinavian
#' Journal of Statistics, 39(1), 53-74.
#'
#' Wood, S. N. (2017). Generalized Additive Models: An Introduction with R,
#' 2nd edition. Chapman and Hall/CRC.
#' @seealso [ilm_plot_var_pairs()] for every pair at once.
#' @examples
#' ilm_plot_scatter(mtcars, "mpg", "wt", by = "cyl", trend = "lm")
#' ilm_plot_scatter(mtcars, "mpg", "wt", facet = "cyl")
#' ilm_plot_scatter(mtcars, "mpg", "hp", trend = "gam")
#' @export
ilm_plot_scatter <- function(data, y, x, by = NULL,
                             trend = c("none", "lm", "loess", "gam"), ...,
                             pch = NULL, facet = NULL, legend = NULL) {
  trend <- match.arg(trend)
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  xv <- ilm_col_vec(data, x, "x", TRUE)
  yv <- ilm_col_vec(data, y, "y", TRUE)
  bv <- ilm_col_vec(data, by, "by")
  fv <- ilm_facet_vec(data, facet)
  band <- if (trend != "none") ilm_trend_band(xv, yv, bv, fv, trend)
  ## the band can reach beyond the points; the axis is drawn to hold both,
  ## unless a range was given
  ylim <- if (!is.null(band) && nrow(band) && !"ylim" %in% names(list(...)))
    range(yv, band$ymin, band$ymax, finite = TRUE)
  tinyplot::tinyplot(x = xv, y = yv, by = bv, facet = fv, type = "points",
                     legend = legend,
                     xlab = x, ylab = y, pch = ilm_pch(pch), ylim = ylim, ...)
  if (!is.null(band) && nrow(band))
    tinyplot::tinyplot_add(x = band$x, y = band$y, ymin = band$ymin,
                           ymax = band$ymax,
                           by = if (!is.null(bv)) band$by,
                           facet = if (!is.null(fv)) band$facet,
                           type = "ribbon")
  invisible(NULL)
}

## A trend and its 95% band in every group and panel, on 100 values of x
## spanning the group; see ilm_plot_scatter()'s Trend bands. Groups too small
## to fit are left out and named in a message.
#' @keywords internal
#' @noRd
ilm_trend_band <- function(x, y, by = NULL, facet = NULL,
                           method = c("lm", "loess", "gam"),
                           level = 0.95, n = 100L) {
  method <- match.arg(method)
  if (method == "gam" && !requireNamespace("mgcv", quietly = TRUE))
    stop("trend = \"gam\" needs the mgcv package, which comes with R; ",
         "install it with install.packages(\"mgcv\")", call. = FALSE)
  keys <- list()
  if (!is.null(by)) keys$by <- by
  if (!is.null(facet)) keys$facet <- facet
  grp <- if (length(keys)) interaction(keys, drop = TRUE, lex.order = TRUE)
         else factor(rep("all", length(x)))
  q <- (1 + level) / 2
  skipped <- character(0)
  out <- lapply(split(seq_along(x), grp), function(i) {
    i <- i[is.finite(x[i]) & is.finite(y[i])]
    d <- data.frame(x = x[i], y = y[i])
    ux <- length(unique(d$x))
    ## a line needs two distinct x values and a band a third row; a smooth
    ## needs more (Anscombe's fourth set has two x values and gets its line)
    enough <- nrow(d) >= 3L &&
      ux >= switch(method, lm = 2L, loess = 3L, gam = 4L)
    nd <- if (enough) data.frame(x = seq(min(d$x), max(d$x), length.out = n))
    p <- if (enough) tryCatch(switch(method,
      lm = {
        fit <- stats::lm(y ~ x, data = d)
        pr <- stats::predict(fit, nd, se.fit = TRUE)
        list(fit = pr$fit, se = pr$se.fit, z = stats::qt(q, fit$df.residual))
      },
      loess = {
        fit <- stats::loess(y ~ x, data = d)
        pr <- stats::predict(fit, nd, se = TRUE)
        list(fit = pr$fit, se = pr$se.fit, z = stats::qt(q, pr$df))
      },
      gam = {
        fit <- mgcv::gam(y ~ s(x, k = min(10L, ux)), data = d, method = "REML")
        pr <- mgcv::predict.gam(fit, nd, se.fit = TRUE)
        list(fit = as.numeric(pr$fit), se = as.numeric(pr$se.fit),
             z = stats::qnorm(q))
      }), error = function(e) NULL)
    if (is.null(p)) {
      skipped <<- c(skipped, if (length(keys)) as.character(grp[i[1]]) else "all")
      return(NULL)
    }
    r <- data.frame(x = nd$x, y = p$fit, ymin = p$fit - p$z * p$se,
                    ymax = p$fit + p$z * p$se)
    if (!is.null(by)) r$by <- rep(by[i[1]], n)
    if (!is.null(facet)) r$facet <- rep(facet[i[1]], n)
    r
  })
  if (length(skipped))
    message("trend = \"", method, "\" is not drawn for ",
            if (identical(skipped, "all")) "these data" else
              paste("group", ilm_and(skipped)),
            ": too few distinct x values to fit it")
  out <- do.call(rbind, out)
  if (is.null(out)) out <- data.frame(x = numeric(0), y = numeric(0),
                                      ymin = numeric(0), ymax = numeric(0))
  rownames(out) <- NULL
  out
}

#' Bar plot
#'
#' @param data A data frame.
#' @param x Name of the categorical column.
#' @param by Optional grouping column.
#' @param facet Optional column to draw in panels, one per level, named as a
#'   string the way `by` is: `facet = "site"`.
#' @param legend Passed to [tinyplot::tinyplot()], in any form it takes. With
#'   `by`, the legend is titled with the `by` column's name unless you
#'   give a title.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @return `NULL`, invisibly.
#' @seealso [ilm_counts()] for the same information as a table.
#' @examples
#' ilm_plot_bar(mtcars, "cyl", by = "am")
#' @export
ilm_plot_bar <- function(data, x, by = NULL, ..., facet = NULL, legend = NULL) {
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  tinyplot::tinyplot(x = ilm_col_vec(data, x, "x", TRUE),
                     by = ilm_col_vec(data, by, "by", discrete = TRUE),
                     legend = legend,
                     facet = ilm_facet_vec(data, facet), type = "barplot",
                     xlab = x, ...)
  invisible(NULL)
}

#' Line plot
#'
#' @param data A data frame.
#' @param y,x Column names; `x` is usually a date or a sequence.
#' @param by Optional grouping column.
#' @param facet Optional column to draw in panels, one per level, named as a
#'   string the way `by` is: `facet = "site"`.
#' @param legend Passed to [tinyplot::tinyplot()], in any form it takes. With
#'   `by`, the legend is titled with the `by` column's name unless you
#'   give a title.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @param pch Plotting character. Takes a NAME as well as a number:
#'   `"filled circle"` is 16, and every code from 0 to 25 has one. Case,
#'   spaces, underscores and hyphens are ignored. A single character is
#'   drawn literally, so `pch = "x"` is still the letter x.
#' @return `NULL`, invisibly.
#' @seealso `illume::ilm_plot_acf()` for what a line plot of residuals cannot show.
#' @examples
#' d <- data.frame(t = 1:40, v = cumsum(rnorm(40)))
#' ilm_plot_line(d, "v", "t")
#' @export
ilm_plot_line <- function(data, y, x, by = NULL, ..., pch = NULL,
                          facet = NULL, legend = NULL) {
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  tinyplot::tinyplot(x = ilm_col_vec(data, x, "x", TRUE),
                     y = ilm_col_vec(data, y, "y", TRUE),
                     by = ilm_col_vec(data, by, "by"),
                     legend = legend,
                     facet = ilm_facet_vec(data, facet), type = "lines",
                     xlab = x, ylab = y, pch = ilm_pch(pch), ...)
  invisible(NULL)
}

#' Group means or medians with an error bar
#'
#' A summary per group with a measure of spread around it.
#'
#' The two `stat` options show different things and are not interchangeable.
#' `"mean"` draws the standard error, which is about where the *mean* is;
#' `"median"` draws the quartiles, which is about where the *data* are. The
#' first shrinks as the sample grows and the second does not.
#'
#' Overlapping error bars are a poor test of a difference -- they are
#' conservative and lossy. [ilm_boot_diff()] gives the difference itself with
#' its own interval.
#'
#' @param data A data frame.
#' @param y Name of the numeric column to summarise.
#' @param x Name of the column defining the groups.
#' @param by Optional secondary grouping column.
#' @param facet Optional column to draw in panels, one per level, named as a
#'   string the way `by` is: `facet = "site"`.
#' @param legend Passed to [tinyplot::tinyplot()], in any form it takes. With
#'   `by`, the legend is titled with the `by` column's name unless you
#'   give a title.
#' @param stat `"mean"` with its standard error, or `"median"` with the
#'   quartiles.
#' @param ... Passed to [tinyplot::tinyplot()].
#' @param pch Plotting character. Takes a NAME as well as a number:
#'   `"filled circle"` is 16, and every code from 0 to 25 has one. Case,
#'   spaces, underscores and hyphens are ignored. A single character is
#'   drawn literally, so `pch = "x"` is still the letter x.
#' @return `NULL`, invisibly.
#' @seealso [ilm_boot_diff()] for the comparison this plot invites.
#' @examples
#' ilm_plot_stat_error(mtcars, "mpg", "cyl")
#' @export
ilm_plot_stat_error <- function(data, y, x, by = NULL,
                                stat = c("mean", "median"), ...,
                                pch = NULL, facet = NULL, legend = NULL) {
  stat <- match.arg(stat)
  ilm_plot_frame_check(data)
  legend <- ilm_by_legend(substitute(legend), parent.frame(), by)
  yv <- ilm_col_vec(data, y, "y", TRUE)
  xv <- ilm_col_vec(data, x, "x", TRUE)
  bv <- ilm_col_vec(data, by, "by", discrete = TRUE)
  fv <- ilm_facet_vec(data, facet)
  key <- data.frame(x = xv, stringsAsFactors = FALSE)
  if (!is.null(bv)) key$by <- bv
  if (!is.null(fv)) key$facet <- fv
  sp <- split(seq_along(yv), interaction(key, drop = TRUE, sep = "\r"))
  rows <- lapply(sp, function(i) {
    v <- yv[i]; v <- v[!is.na(v)]
    if (!length(v)) return(NULL)
    ct <- if (stat == "mean") {
      se <- stats::sd(v) / sqrt(length(v))
      c(mean(v), mean(v) - se, mean(v) + se)
    } else unname(stats::quantile(v, c(0.5, 0.25, 0.75)))
    data.frame(x = key$x[i[1]],
               by = if (is.null(bv)) NA else key$by[i[1]],
               facet = if (is.null(fv)) NA else key$facet[i[1]],
               center = ct[1], lo = ct[2], hi = ct[3],
               stringsAsFactors = FALSE)
  })
  s <- do.call(rbind, rows)
  if (is.null(s) || !nrow(s))
    stop("no non-missing values to summarise", call. = FALSE)
  tinyplot::tinyplot(x = s$x, y = s$center, ymin = s$lo, ymax = s$hi,
                     by = if (is.null(bv)) NULL else s$by, legend = legend,
                     facet = if (is.null(fv)) NULL else s$facet, type = "pointrange",
                     xlab = x, ylab = paste(stat, y), pch = ilm_pch(pch), ...)
  invisible(NULL)
}
