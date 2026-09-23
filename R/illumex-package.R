#' illumex: Describe, Clean, Plot and Profile Data Before a Model
#'
#' The exploratory half of the `illume` workflow, usable on its own: the
#' routine work before a model, each step one call with a consistent
#' interface. `illume` attaches this package, so with `illume` loaded
#' everything here is available as it always was.
#'
#' @section What is in it:
#' \describe{
#'   \item{Describing}{[ilm_describe_all()] summarises every column of a data
#'     frame at once, with a gaussian index in place of a normality test;
#'     [ilm_counts()], [ilm_dupes()], [ilm_copies()] and [ilm_frame_issues()]
#'     find what is wrong with a data frame before any model sees it.}
#'   \item{Cleaning}{[ilm_wash_df()], [ilm_recode_errors()], [ilm_translate()].}
#'   \item{Uncertainty without a model}{[ilm_boot_ci()] and [ilm_boot_diff()].}
#'   \item{Unusual values}{[ilm_outliers()] for a value extreme in its own
#'     column; [ilm_anomaly()] and [ilm_plot_anomaly()] for a row implausible
#'     as a combination.}
#'   \item{Structure}{[ilm_profile()] reduces, clusters and describes the
#'     clusters in one call, for numeric and mixed columns alike;
#'     [ilm_reduce()], [ilm_cluster()] and [ilm_glrm()] are its parts.}
#'   \item{Missing values}{[ilm_check_missing()], [ilm_describe_na()] and
#'     [ilm_plot_missing()] describe them. Imputing and pooling them is
#'     `illume`'s: see `illume::ilm_impute()`.}
#'   \item{Plots}{[ilm_plot()] and the `ilm_plot_*()` family, built on
#'     `tinyplot` and named for what they show.}
#'   \item{Example data}{[ilm_sim()], a grouped data set with a known
#'     structure.}
#' }
#'
#' @section Two spellings:
#' Every exported function whose name starts `ilm_` also answers to `iml_`, an
#' easy transposition to type: `iml_describe_all()` is [ilm_describe_all()]
#' itself, not a wrapper, and opens the same help page.
#'
#' @section Lineage:
#' This is a direct descendant of `elucidate`, by the same author, rebuilt on
#' `collapse` and `tinyplot`. The two share an interface rather than an
#' implementation.
#'
#' @keywords internal
"_PACKAGE"
