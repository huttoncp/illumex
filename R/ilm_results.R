## ---------------------------------------------------------------------------
## What illumex's results share: a class in front of what they already are, so
## they print as themselves; a description's rounding for its print; and the
## seed a result drawn from random numbers kept.
## ---------------------------------------------------------------------------

## an empty named list
ILM_EMPTY <- stats::setNames(list(), character(0))

## A result's class, in front of what it already is (a data frame or a list),
## so its print method is found. Nothing else changes: a data frame still
## prints, subsets and binds as one.
#' @keywords internal
#' @noRd
ilm_as_result <- function(x, cls) {
  if (is.null(x)) return(x)
  class(x) <- unique(c(cls, class(x)))
  x
}

## Values that differ only by floating-point noise, compared as equal: a
## table stored at full precision still breaks a true tie by its order
#' @keywords internal
#' @noRd
ilm_tie <- function(x) round(x, 12)

## A description keeps its values whole and prints them to `digits`: the
## same columns rounded as they always were (tests/testthat/
## test-print-unchanged.R), by the shared display rules' rounding, half away
## from zero after clearing floating-point noise (Craig's ruling 272).
ILM_DESCRIBE_ROUNDED <- c("sum", "mean", "sd", "se", "skew", "kurt", "p_zero",
                          "dispersion", "p_max", "p_TRUE", "span_days", "p_na", "smd")

#' @keywords internal
#' @noRd
ilm_describe_rounded <- function(x, digits = attr(x, "digits")) {
  y <- as.data.frame(x)
  class(y) <- "data.frame"
  attributes(y)[c("digits", "by", "variable")] <- NULL
  rd <- function(v, d) if (is.double(v) && !is.object(v)) ilm_disp_round(v, d) else v
  if (!is.null(digits)) {
    cols <- intersect(names(y), c(ILM_DESCRIBE_ROUNDED, grep("^p[0-9.]+$", names(y), value = TRUE)))
    for (cn in cols) y[[cn]] <- rd(y[[cn]], digits)
  }
  ## the gaussian index and distance print as ilm_gauss_check() prints them,
  ## whatever `digits` is
  if (!is.null(y$gauss)) y$gauss <- rd(y$gauss, 3)
  if (!is.null(y$ks_d)) y$ks_d <- rd(y$ks_d, 4)
  y
}

## A description's table as text, the same on every computer (Craig's ruling
## 272). R's print of a numeric column chooses how many significant figures
## each value needs, up to getOption("digits"), and works that out in long
## double on an Intel machine and in double on Apple silicon, so a value on a
## rounding boundary -- a median of 34420.195 -- printed one way on one and
## the other way on the other. Here each value is rounded by the shared
## display rules and written as text in the layout R's print gives a column:
## one number of decimals down the column, the fewest that show every value
## to its significant figures (each value then shown in full to those
## decimals), and scientific notation where R would choose it. The rounded
## table decides the layout; each value's text is the value as kept, rounded
## once to the decimals shown, so a mean of 40410.4647 shown to two decimals
## is 40410.46, never 40410.47 by way of 40410.465. Columns of other kinds (counts, dates, words) print as they did.
#' @keywords internal
#' @noRd
ilm_describe_text <- function(y, raw = y) {
  for (cn in names(y))
    if (is.double(y[[cn]]) && !is.object(y[[cn]]))
      y[[cn]] <- ilm_print_num(y[[cn]], raw = raw[[cn]] %||% y[[cn]])
  y
}

## One numeric column as R would print it, by the shared rounding: see
## ilm_describe_text()
#' @keywords internal
#' @noRd
ilm_print_num <- function(v, digits = getOption("digits", 7L), raw = v) {
  out <- rep("NA", length(v))
  out[is.nan(v)] <- "NaN"
  inf <- is.infinite(v)
  out[inf] <- ifelse(v[inf] > 0, "Inf", "-Inf")
  ok <- is.finite(v)
  if (!any(ok)) return(out)
  x <- ilm_disp_signif(v[ok], digits)
  x[x == 0] <- 0                          # no "-0"
  ax <- abs(x)
  e <- ifelse(ax == 0, 0, floor(log10(ax)))
  ## each value's significant figures as a whole number, and how many it
  ## needs once trailing zeros are dropped
  m <- round(ax / 10^(e - digits + 1))
  up <- m >= 10^digits
  e[up] <- e[up] + 1; m[up] <- round(m[up] / 10)
  nz <- vapply(m, function(k) {
    z <- 0L
    while (k > 0 && k %% 10 == 0 && z < digits - 1L) { k <- k %/% 10; z <- z + 1L }
    z
  }, 1L)
  nsig <- ifelse(ax == 0, 1L, digits - nz)
  ## R's choice: fixed notation unless it is wider than scientific
  neg <- x < 0
  rgt <- max(pmax(0, nsig - e - 1))
  w_fixed <- max(neg + ifelse(e >= 0, e + 1, 1)) + rgt + (rgt > 0)
  mxns <- max(nsig)
  w_sci <- any(neg) + mxns + (mxns > 1) + if (any(abs(e) >= 100)) 5 else 4
  ## the significant figures decide the column's decimals; each value is
  ## then shown in full to that many, as R shows it
  if (w_fixed <= w_sci + getOption("scipen", 0)) {
    r <- ilm_disp_round(raw[ok], rgt)
    r[r == 0] <- 0
    out[ok] <- formatC(r, format = "f", digits = rgt)
  } else out[ok] <- formatC(x, format = "e", digits = mxns - 1L)
  out
}

#' @export
print.ilm_describe <- function(x, ...) {
  print(ilm_describe_text(ilm_describe_rounded(x), as.data.frame(x)), ...)
  invisible(x)
}

#' @export
print.ilm_describe_na <- function(x, ...) {
  print(ilm_describe_text(ilm_describe_rounded(x), as.data.frame(x)), ...)
  invisible(x)
}

## ilm_describe_all()'s list of tables prints as the plain list it was
#' @export
print.ilm_describe_all <- function(x, ...) {
  d <- attr(x, "digits")
  y <- lapply(unclass(x), function(t)
    if (is.data.frame(t)) ilm_describe_text(ilm_describe_rounded(t, d), as.data.frame(t)) else t)
  attributes(y) <- list(names = names(x))
  print(y, ...)
  invisible(x)
}

## a description knows which column it describes and how it was grouped,
## since a single column's row does not name it
#' @keywords internal
#' @noRd
ilm_describe_result <- function(x, cls, y, by, digits = NULL) {
  if (is.character(y)) attr(x, "variable") <- y
  if (length(by)) attr(x, "by") <- by
  if (!is.null(digits)) attr(x, "digits") <- digits
  ilm_as_result(x, cls)
}

## round() when there are digits to round to; a description keeps its values
## whole and passes NULL
#' @keywords internal
#' @noRd
ilm_rd <- function(x, digits = 0) if (is.null(digits)) x else round(x, digits)

## a bootstrap result keeps the seed it drew with, and the generator's kind
#' @keywords internal
#' @noRd
ilm_boot_result <- function(x, cls, seed) {
  ilm_as_result(ilm_seed_mark(x, seed), cls)
}

## A result drawn from random numbers keeps the seed given (none is kept as
## none) and the generator's kind: all three of RNGkind(), since the uniform
## generator alone cannot reproduce normal draws (normal.kind) or sample()
## (sample.kind, changed in R 3.6)
#' @keywords internal
#' @noRd
ilm_seed_mark <- function(x, seed) {
  k <- RNGkind()
  attr(x, "seed") <- if (!is.null(seed)) as.integer(seed)
  attr(x, "rng_kind") <- k[1]
  attr(x, "rng_normal_kind") <- k[2]
  attr(x, "rng_sample_kind") <- k[3]
  x
}
