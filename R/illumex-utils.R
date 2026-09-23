## ---------------------------------------------------------------------------
## Helpers illumex shares with illume.
##
## illume keeps its own copies of `%||%`, ilm_bq() and ilm_progress() rather
## than reaching into this package's internals, and its test suite compares
## the two copies, so a change made here has to be made there as well. The
## same holds for the plotting-symbol helpers in ilm_plot.R.
## ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

## Prose wrapped to the console, with an indent.
#' @keywords internal
#' @noRd
ilm_wrap <- function(x, width = 76L, indent = "") {
  paste0(indent, strwrap(x, width = width - nchar(indent)), collapse = "\n")
}

## Names as they must be written in R code: backticked when not syntactic,
## untouched when they are. deparse() of a symbol knows the rules, reserved
## words included, which a comparison against make.names() gets subtly wrong.
#' @keywords internal
#' @noRd
ilm_bq <- function(x) {
  if (!length(x)) return(character(0))
  vapply(as.character(x), function(v) deparse(as.name(v), backtick = TRUE), "",
         USE.NAMES = FALSE)
}

## ---------------------------------------------------------------------------
## Progress reporting.
##
## Measured before being added: against a realistic loop -- 2000 bootstrap
## replicates on 20,000 rows -- a bar updated every 1% ran 2.48s against 2.53s
## without one, and updating every single iteration cost 2%. Both are inside the
## noise. The overhead only shows against a body so fast that a bar would be
## pointless anyway: 200,000 iterations of `r + 1` went 0.11s to 0.15s.
##
## So the bar is free wherever it is worth having, and the 1% step is there to
## avoid writing to a slow console rather than to save time.
##
## The default is interactive(): visible when a person is watching, silent in
## scripts, tests and knitr. A bar written into a vignette or a test log is
## noise, and would break every expect_silent() in the suite.
## ---------------------------------------------------------------------------

#' Progress reporting in illumex and illume
#'
#' Long-running functions take a `progress` argument. It defaults to
#' [interactive()], so a bar appears when someone is watching and nothing is
#' written in a script, a test or a knitted document.
#'
#' The bar costs nothing worth measuring. On 2000 bootstrap replicates over
#' 20,000 rows, the loop took 2.48s with a bar and 2.53s without.
#'
#' Where work is spread over several cores, the bar advances as each **chunk**
#' of the work returns rather than each replicate: the workers are separate
#' processes and cannot write to the parent's console. It is coarser, and it
#' still tells you the run is alive and roughly how far along.
#'
#' @name ilm_progress_arg
#' @examples
#' d <- ilm_sim()
#' ilm_boot_ci(d, "score", R = 200, progress = FALSE)
NULL

## A bar, or a silent stand-in with the same shape so callers need no branch.
#' @keywords internal
#' @noRd
ilm_progress <- function(n, progress = NULL, label = NULL) {
  on <- isTRUE(progress) ||
    (is.null(progress) && interactive() && n > 1L)
  if (!on || !is.finite(n) || n < 1L)
    return(list(tick = function(i) invisible(NULL),
                done = function() invisible(NULL)))
  if (!is.null(label)) message(label)
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  step <- max(1L, as.integer(n) %/% 100L)
  list(
    tick = function(i) {
      if (i %% step == 0L || i == n) utils::setTxtProgressBar(pb, i)
      invisible(NULL)
    },
    done = function() { utils::setTxtProgressBar(pb, n); close(pb); invisible(NULL) })
}
