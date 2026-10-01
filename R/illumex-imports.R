#' Internal imports
#'
#' Everything else from `stats`, `graphics`, `grDevices`, `utils` and
#' `tinyplot` is called with `::`. The `collapse` functions are imported by
#' name because they sit in the inner loops of the descriptive functions,
#' where they are the reason the exploratory tools are usable on real data.
#'
#' @importFrom collapse fcount fmatch fmean fndistinct fnobs fquantile
#'   fsd fsum fvar group
#' @importFrom stats setNames
#' @name illumex-imports
#' @noRd
NULL
