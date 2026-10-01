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

## A description keeps its values whole and prints them to `digits`, as it
## used to store them: the same columns rounded the same way, so what prints
## is what printed before (tests/testthat/test-print-unchanged.R).
ILM_DESCRIBE_ROUNDED <- c("sum", "mean", "sd", "se", "skew", "kurt", "p_zero",
                          "dispersion", "p_max", "p_TRUE", "span_days", "p_na", "smd")

#' @keywords internal
#' @noRd
ilm_describe_rounded <- function(x, digits = attr(x, "digits")) {
  y <- as.data.frame(x)
  class(y) <- "data.frame"
  attributes(y)[c("digits", "by", "variable")] <- NULL
  if (is.null(digits)) {
    if (is.numeric(y$gauss)) y$gauss <- round(y$gauss, 3)
    if (is.numeric(y$ks_d)) y$ks_d <- round(y$ks_d, 4)
    return(y)
  }
  cols <- intersect(names(y), c(ILM_DESCRIBE_ROUNDED, grep("^p[0-9.]+$", names(y), value = TRUE)))
  for (cn in cols) if (is.numeric(y[[cn]])) y[[cn]] <- round(y[[cn]], digits)
  ## the gaussian index and distance print as ilm_gauss_check() prints them,
  ## whatever `digits` is
  if (is.numeric(y$gauss)) y$gauss <- round(y$gauss, 3)
  if (is.numeric(y$ks_d)) y$ks_d <- round(y$ks_d, 4)
  y
}

#' @export
print.ilm_describe <- function(x, ...) {
  print(ilm_describe_rounded(x), ...)
  invisible(x)
}

#' @export
print.ilm_describe_na <- function(x, ...) {
  print(ilm_describe_rounded(x), ...)
  invisible(x)
}

## ilm_describe_all()'s list of tables prints as the plain list it was
#' @export
print.ilm_describe_all <- function(x, ...) {
  d <- attr(x, "digits")
  y <- lapply(unclass(x), function(t) if (is.data.frame(t)) ilm_describe_rounded(t, d) else t)
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
