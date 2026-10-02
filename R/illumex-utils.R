## The `progress` argument's help page. The helper behind it, ilm_progress(),
## is in R/shared-helpers.R with the other helpers illumex shares with illume.

#' Progress reporting in illumex and illume
#'
#' Long-running functions take a `progress` argument. It defaults to
#' [interactive()], so a bar appears when someone is watching and nothing is
#' written in a script, a test or a knitted document.
#'
#' The bar costs nothing worth measuring: on 2000 bootstrap replicates over
#' 20,000 rows the loop took no longer with a bar than without one.
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

