# Provenance: written from a specification and the published sources (White and
# Carlin 2010; Hughes et al. 2019; Little and Rubin 2019; Agresti 2013; Liang
# and Zeger 1986), without reference to illumex's earlier implementation or to
# any other package's source. The association screen's per-variable form,
# effect measures and thresholds, the outcome test and its effect measure, the
# verdict labels, and the rule choosing between one row per unit and a
# cluster-robust test are illumex's own operationalisations of the published
# conditions.
#
# ilm_check_missing(): how much of a data frame is missing, which variables go
# missing together, what missingness is associated with among the observed
# values, and, with an outcome named, whether missingness depends on the
# outcome once the covariates are held fixed.
#
# The central published condition [published: White and Carlin 2010, section
# 2.2, p. 2921; Hughes et al. 2019, p. 1296 and Table 1, p. 1297; Little and
# Rubin 2019, Example 3.3, p. 49]: complete-case analysis of a regression of y
# on covariates X is unbiased when being a complete case is independent of y
# given X, even when it depends strongly on X.
#
# The outcome test below is illumex's own operationalisation of that
# condition. None of White and Carlin (2010), Hughes et al. (2019) or Little
# and Rubin (2019) prescribes a test for it.
#
# Labels used in the comments:
#   [published: <source, page>]  the rule comes from that source, at that page;
#   [illumex's own choice]       this package's own operationalisation or
#                                choice; no source prescribes it.


#' Check missing values, and what they imply for complete cases
#'
#' @description
#' `ilm_check_missing()` reports how much of a data frame is missing, which
#' variables go missing together, what missingness is associated with among
#' the observed values, and, when an outcome `y` is named, whether missingness
#' depends on the outcome once the covariates are held fixed. It then states
#' what that implies for a complete-case analysis of a regression of `y` on
#' the covariates.
#'
#' @details
#' **How much, and where.** `variables` gives each column's missing count and
#' share. `patterns` tabulates the rows' patterns of missingness over the
#' columns that have any missing value. `monotone` says whether those columns
#' can be ordered so that each is missing wherever the one before it is
#' missing (Little and Rubin 2019, p. 11). The check orders the columns by
#' their missing counts, smallest first, and compares neighbours.
#'
#' **The association screen.** For each column `v` with missing values and
#' each other column `w`, the indicator "`v` is missing" is compared across the
#' values of `w`, over the rows where `w` is observed.
#' * A numeric `w` (integer or double) gives the absolute Pearson correlation
#'   between the indicator and `w`, with the usual two-sided t test on n - 2
#'   degrees of freedom.
#' * Any other `w` (logical, character, factor, Date, date-time or duration;
#'   one level per distinct value) is cross-tabulated against the indicator.
#'   The p-value is Pearson's chi-squared test of independence without a
#'   continuity correction, and the effect is \enc{Cramér}{Cramer}'s V
#'   (Agresti 2013, p. 110).
#' * A pair is skipped when fewer than 10 rows remain, when the indicator
#'   takes one value there, or when `w` has no spread (or fewer than two
#'   levels).
#' * The p-values are adjusted across all pairs by `adjust`. A pair is flagged
#'   when its adjusted p-value is below `alpha` AND its effect is at least
#'   `min_effect`: with many rows, a negligible association is significant.
#'
#' A correlation and a \enc{Cramér}{Cramer}'s V are not the same quantity;
#' they are sorted together on one 0-1 scale for convenience. The screen
#' describes; a flag on the outcome column does not by itself mean that
#' complete cases are biased (see "Why 'not MCAR' does not mean 'impute'").
#'
#' **The outcome test** (only when `y` is named). For each column `v` with
#' missing values, over the rows with `y` and the covariates observed, two
#' logistic regressions of "`v` is missing" are fitted, each with an
#' intercept: one on the covariates, one on the covariates plus `y`. Numeric
#' covariates enter linearly, categorical ones (factor, character, logical)
#' as treatment-coded indicators. A numeric `y` enters as one column; a
#' categorical `y`, with k levels present, as k - 1 indicators.
#' * A Date, date-time (POSIXct) or duration (difftime) covariate, or `y`, is
#'   judged as a numeric one is: it is left out only when it has no spread on
#'   the rows used. It enters both regressions, and the effect's least-squares
#'   step, as ONE numeric column, its numeric value (days for a Date, seconds
#'   for a date-time, the stored number for a duration) counted from its
#'   earliest value on those rows, whatever its number of distinct values: a
#'   daily date is kept, as a linear time term. For a linear term with an
#'   intercept, neither the unit nor the starting point changes the test or
#'   the effect. In the screen such columns stay categorical.
#' * The p-value is the likelihood-ratio test between the two fits.
#' * The effect is the multiple correlation between the Pearson residuals of
#'   the covariates-only fit and `y`'s model columns, each residualised on the
#'   covariates by least squares. For a numeric, date or two-level `y` this is
#'   the absolute partial correlation ("partial r"); for a `y` with more
#'   levels it is a multiple partial correlation ("partial R"). It does not
#'   depend on which level is the reference, or on the order of the levels.
#'   It measures the association given the covariates, because bias depends
#'   on that conditional association, not on the marginal one.
#' * Covariates with no spread, with fewer than two levels, or with more than
#'   20 levels are left out of the models.
#' * Fewer than 20 rows, an indicator that takes one value, or a failed fit
#'   give NA. Missingness in `y` itself gives NA (see below).
#' * The covariates are `covariates` without `v` and `y`; when `covariates` is
#'   not given, every column except `y`, `v` and `unit`.
#'
#' **The verdict**, in order of precedence: `"NONE"` (nothing is missing);
#' `"RELATED_TO_OUTCOME"` (an outcome-test row is flagged);
#' `"RELATED_TO_COVARIATES"` (a screen row is flagged); otherwise
#' `"MCAR_NOT_REJECTED"`.
#'
#' @section What can and cannot be established:
#' Missingness completely at random (MCAR) can be REJECTED by the screen, never
#' confirmed. A screen that flags nothing is consistent with MCAR, but such
#' tests have limited power when few units are incomplete (Little and Rubin
#' 2019, section 3.2, p. 50). The verdict `"MCAR_NOT_REJECTED"` says that and
#' no more.
#'
#' Whether missingness depends on the unseen values themselves (missing at
#' random against missing not at random: MAR against MNAR) cannot be tested
#' from the observed data. Such comparisons give "no direct evidence" on MAR
#' (Little and Rubin 2019, p. 50), and there can be no evidence against MAR in
#' the observed data (Little and Rubin 2019, Example 13.2, p. 306). Every
#' verdict except `"NONE"` therefore ends with this caveat, and recommends a
#' sensitivity analysis to assess departures from MAR.
#'
#' The outcome test has a limit of its own. When a covariate goes missing
#' because of its OWN unseen values (for example, a covariate missing more
#' often when it is large), complete cases are still unbiased: Little and
#' Rubin (2019, Example 3.3, p. 49) include such mechanisms among the unbiased
#' ones, as do Hughes et al. (2019, Table 1, p. 1297, row "Exposure"). The
#' outcome test cannot hold that covariate's unseen values fixed, so the
#' outcome carries information about them, and the test can flag the outcome
#' there although complete cases are unbiased. That is this package's own
#' finding about its own test, shown by simulation. So `"RELATED_TO_OUTCOME"`
#' means that complete-case estimates MAY be biased, never that they are.
#'
#' @section Why 'not MCAR' does not mean 'impute':
#' Complete-case analysis of a regression of `y` on covariates X is unbiased
#' when the chance of being a complete case is independent of `y` given X,
#' even when it depends strongly on X (White and Carlin 2010, section 2.2,
#' p. 2921; Hughes et al. 2019, p. 1296 and Table 1, p. 1297; Little and Rubin
#' 2019, Example 3.3, p. 49). That condition is
#' weaker than MCAR, and it cuts across MCAR, MAR and MNAR. X includes the
#' incomplete covariate's own values: the condition can hold when a
#' covariate's missingness depends on its own missing values (White and
#' Carlin 2010, section 2.2, p. 2921; Little and Rubin 2019, Example 3.3,
#' p. 49, which includes such mechanisms).
#'
#' Each verdict's message gives the number of missing values and the number
#' of columns that have them, so that the weight of the evidence can be
#' judged.
#'
#' When nothing is flagged (`"MCAR_NOT_REJECTED"`), there is no evidence that
#' missingness is related to any observed variable. Such checks are useful,
#' but they have limited power when the incomplete units are few (Little and
#' Rubin 2019, section 3.2, p. 50): "not rejected" is not evidence of absence.
#'
#' When missingness is related to the covariates, with no evidence of a
#' relation to the outcome given them (`"RELATED_TO_COVARIATES"`), complete
#' cases stay unbiased for the regression on those covariates, unless
#' missingness depends on unseen values. That holds whenever being complete
#' is independent of `y` given the covariates, the incomplete covariate's own
#' values included (White and Carlin 2010, section 2.2, p. 2921; Little and
#' Rubin 2019, Example 3.3, p. 49). Imputation is not needed for correctness,
#' though it can recover precision. This function distinguishes that case and
#' says so, rather than sending you to imputation.
#'
#' When missingness is related to the outcome given the covariates
#' (`"RELATED_TO_OUTCOME"`), complete-case estimates MAY be biased.
#' * Not when a covariate is missing because of its own value. Complete cases
#'   are unbiased when missingness depends on the covariate, including the
#'   missing-not-at-random mechanism p(C | X, Y) = f(X), where C is being
#'   complete (White and Carlin 2010, p. 2921). With one incomplete covariate,
#'   multiple imputation is valid if missingness depends on `y` only, complete
#'   cases if it depends on the covariate only, and both only under MCAR
#'   (White and Carlin 2010, p. 2922). When missingness is independent of the
#'   outcome given the covariates, as it is there, multiple imputation is
#'   biased away from the null (White and Carlin 2010, abstract, p. 2920):
#'   imputation assuming MAR is the biased choice. The outcome test can flag
#'   this case (see "What can and cannot be established").
#' * Nor in the cases of Hughes et al. (2019, Table 1, p. 1297, and p. 1298):
#'   a linear regression whose true coefficient is zero; and the exposure odds
#'   ratio of a logistic regression when completeness depends on the outcome
#'   alone, on the outcome and the confounders, or on the outcome and the
#'   exposure independently. That logistic exception does not apply when the
#'   binary outcome dichotomises a continuous one on which missingness depends
#'   (Hughes et al. 2019, p. 1298).
#'
#' Multiple imputation (`illume::ilm_impute()`, then `illume::ilm_mi_pool()`)
#' is the analysis under MAR. It is often preferred when auxiliary variables
#' make MAR more plausible (Hughes et al. 2019, p. 1302; White and Carlin
#' 2010, p. 2928). Run the complete-case analysis as well: "It is, however,
#' valuable to perform both MI and CC analyses ... Any differences should be
#' explained" (White and Carlin 2010, p. 2928). Choosing between the two by
#' their standard errors is misleading (White and Carlin 2010, abstract,
#' p. 2920, and p. 2928).
#'
#' When `y` itself has missing values, its own row of the outcome test is NA:
#' rows missing `y` leave the analysis, so the indicator is constant there.
#' With missing outcome only, complete cases are unbiased exactly when the
#' outcome is MAR given the covariates (White and Carlin 2010, p. 2921), and
#' whether the outcome's missingness depends on its own unseen values cannot be
#' tested (Little and Rubin 2019, p. 50).
#'
#' @section Several rows per unit:
#' The sources assume independent rows (Little and Rubin 2019, p. 13). With
#' several rows per unit (a subject seen at several visits, the members of a
#' household), every p-value computed as if the rows were independent is too
#' small. Name the column that identifies the unit in `unit`. Each test is
#' then run in one of two ways.
#' * **One row per unit**, when, over the rows the test uses, every unit has a
#'   single value of the missingness indicator AND a single value of each
#'   variable the test uses (`w` for the screen; `y` and the covariates for
#'   the outcome test). Each unit's first such row is kept, and the test runs
#'   exactly as above with units in place of rows; the 10- and 20-row floors
#'   count units. The test's variables must be constant too, not only
#'   missingness, because a variable that varies within a unit has no single
#'   value to keep.
#' * **Otherwise, a cluster-robust test.** The logistic regression is fitted
#'   by ordinary maximum likelihood on the rows, its covariance is estimated by
#'   the sandwich with the scores summed within units (Liang and Zeger 1986),
#'   and the tested coefficients (of `w`, or of `y`) get a Wald test. With
#'   fewer than 30 units among the rows the test uses, the p-value is NA. No
#'   small-sample correction is applied. Standard errors from the sandwich tend
#'   to be too small in finite samples (Firth 1993, p. 344), and with few units
#'   they tend to underestimate the true ones (Agresti 2013, p. 468), so the
#'   test can reject too often. illumex's own size study measured this for this
#'   function, at alpha = 0.05 over 2,000 simulated data sets per setting: with
#'   5 rows in every unit, the rejection rate was 0.08 to 0.09 at 20 and 25
#'   units, and between 0.05 and 0.07 from 30 units up, within the band of 0.025
#'   to 0.075 that Bradley (1978, p. 146) calls liberal; with between 2 and 8
#'   rows per unit, it was 0.08 to 0.10 at 20 and at 30 units, above that band.
#'   Treat a p-value near alpha with caution when units are few, or when their
#'   sizes differ.
#'
#' On the cluster-robust route, effects are computed on the rows as described
#' above; on the one-row-per-unit route, on the units. The `method` column of
#' `associations` and `outcome_test` says how each test ran: `"rows"` (no
#' `unit`), `"units"` (one row per unit) or `"sandwich"`. The `unit` column is
#' left out of the screen, the covariates, the patterns and the monotone
#' check; it still appears in `variables`.
#'
#' When `unit` is `NULL` and the data look like several rows per unit, a
#' warning says that the p-values assume independent rows and suggests
#' `unit`. It is given at most once per call, even with `verbose = FALSE`, and
#' it never changes the result. The check behind it finds a grouping only in a
#' categorical column, so a numeric identifier column is not detected: name
#' `unit` whenever the data have several rows per unit.
#'
#' @section What is published and what is this package's own:
#' Published: the condition under which complete cases are unbiased (White and
#' Carlin 2010, p. 2921; Hughes et al. 2019, pp. 1296-1298; Little and Rubin
#' 2019, p. 49); comparing the complete with the incomplete units to judge
#' MCAR, and the limits of doing so (Little and Rubin 2019, p. 50); the
#' definition of a monotone pattern (Little and Rubin 2019, p. 11);
#' \enc{Cramér}{Cramer}'s V, the square root of \enc{Cramér}{Cramer}'s measure
#' of association X^2 / (n (q - 1)), q the smaller of the table's numbers of
#' rows and columns (\enc{Cramér}{Cramer} 1946, section 30.5, p. 443; Agresti
#' 2013, p. 110); and the sandwich variance of the independence working model
#' (Liang and Zeger 1986, pp. 14-15; Agresti 2013, pp. 467-469).
#'
#' illumex's own operationalisations, which no source prescribes: the
#' association screen's per-variable form (it compares by each variable's own
#' missingness indicator, a variant of Little and Rubin's comparison of
#' complete with incomplete units); its effect measures, and sorting them on
#' one scale; the thresholds (`min_effect`, the 10- and 20-row floors, the
#' 30-unit floor, the 20-level cut-off) and the defaults of `alpha` and
#' `adjust`; the multiplicity rule and the requirement of both significance
#' and size for a flag; the way monotonicity is checked; the outcome test and
#' its effect measure; the treatment of Date, date-time and duration columns
#' (categorical in the screen; one numeric column in the outcome test,
#' whatever their number of distinct values); the verdict labels, their
#' precedence and the wording of their messages; the rule choosing between
#' one row per unit and the sandwich; and the Wald test of several
#' coefficients at once.
#'
#' @param data A data frame.
#' @param y Optional outcome: a single string naming a column, or a formula
#'   `outcome ~ predictors`. With a formula, the response becomes `y` and the
#'   variables on the right-hand side become `covariates`, exactly as if they
#'   had been given by name; an explicit `covariates` takes precedence. A
#'   right-hand side of `.` means every other column.
#' @param covariates The columns held fixed in the outcome test, in any of the
#'   forms described in [ilm_selection]. `NULL` (the default) means every
#'   column except `y` and `unit`.
#' @param covariates_negate,covariates_fixed How `covariates` is read, as
#'   described in [ilm_selection]. `covariates_negate = TRUE` keeps every
#'   column except those that `covariates` selects, and so needs `covariates`.
#'   `covariates_fixed = TRUE` matches a pattern in `covariates` literally
#'   rather than as a regular expression.
#' @param min_effect The smallest effect that counts as a finding, in the
#'   screen and in the outcome test (default 0.1). An illumex choice.
#' @param alpha The level for the adjusted p-values (default 0.05).
#' @param adjust The multiplicity method: any method that [stats::p.adjust()]
#'   accepts (default `"holm"`). The screen's p-values are adjusted across all
#'   of its rows; the outcome test's across its rows.
#' @param verbose If `TRUE` (the default), narrate the findings as messages.
#' @param unit Optional: a single string naming the column that identifies the
#'   unit (subject, household) when the data have several rows per unit. It
#'   may not be `y` or one of the named `covariates`, and it may not have
#'   missing values. See "Several rows per unit".
#' @inheritParams ilm_reduce
#'
#' @return An object of class `"ilm_missing"`: a list with these fields, in
#'   this order.
#'   * `variables`: a data frame with one row per column (after `subset`):
#'     `variable`, `n_missing` (an integer) and `p_missing` (the share
#'     missing), largest share first.
#'   * `patterns`: `NULL` when nothing is missing; otherwise a data frame with
#'     `pattern` (a string of 0s and 1s, one character per column with missing
#'     values, in the data's order, 1 where that column is missing), `n` (the
#'     rows with that pattern) and `n_missing` (the number of 1s), most common
#'     first. Its attribute `"variables"` names those columns.
#'   * `associations`: the association screen, or `NULL` when no pair
#'     qualifies (and when nothing is missing). Columns `missing_in`,
#'     `related_to`, `effect`, `p_value`, `is_outcome` (`related_to` is `y`),
#'     `p_adj`, `flag` and `method`; largest effect first.
#'   * `outcome_test`: `NULL` when `y` is not named or nothing is missing;
#'     otherwise one row per column with missing values, in the data's order:
#'     `missing_in`, `effect`, `p_value`, `p_adj`, `flag` and `method`.
#'   * `monotone`: `TRUE` or `FALSE`; `NA` when nothing is missing.
#'   * `n`: the number of rows after `subset` (an integer).
#'   * `n_complete`: the number of rows with no missing value (an integer).
#'   * `y`: the outcome's name, or `NULL`.
#'   * `min_effect`: as given.
#'   * `unit`: the unit column's name, or `NULL`.
#'   * `n_units`: the number of distinct units after `subset` (an integer);
#'     `NA` without `unit`.
#'   * `verdict`: `"NONE"`, `"RELATED_TO_OUTCOME"`, `"RELATED_TO_COVARIATES"`
#'     or `"MCAR_NOT_REJECTED"`.
#'
#' @references
#' Agresti, A. (2013). *Categorical Data Analysis*, 3rd ed. Wiley.
#'
#' Bradley, J. V. (1978). Robustness? *British Journal of Mathematical and
#' Statistical Psychology* 31, 144-152.
#'
#' \enc{Cramér}{Cramer}, H. (1946). *Mathematical Methods of Statistics*.
#' Princeton University Press.
#'
#' Firth, D. (1993). Recent developments in quasi-likelihood methods.
#' *Bulletin of the International Statistical Institute* 55, 341-358.
#'
#' Hughes, R. A., Heron, J., Sterne, J. A. C. and Tilling, K. (2019).
#' Accounting for missing data in statistical analyses: multiple imputation is
#' not always the answer. *International Journal of Epidemiology* 48(4),
#' 1294-1304.
#'
#' Liang, K.-Y. and Zeger, S. L. (1986). Longitudinal data analysis using
#' generalized linear models. *Biometrika* 73, 13-22.
#'
#' Little, R. J. A. and Rubin, D. B. (2019). *Statistical Analysis with
#' Missing Data*, 3rd ed. Wiley.
#'
#' White, I. R. and Carlin, J. B. (2010). Bias and efficiency of multiple
#' imputation compared with complete-case analysis for missing covariate
#' values. *Statistics in Medicine* 29, 2920-2931.
#'
#' @seealso `illume::ilm_impute()`, [ilm_describe_na_all()],
#'   [ilm_plot_missing()]
#'
#' @examples
#' d <- ilm_sim()
#' ilm_check_missing(d, y = "score", verbose = FALSE)
#'
#' # A formula names the outcome and the covariates at once
#' ilm_check_missing(d, score ~ income + grp, verbose = FALSE)
#'
#' # `id` identifies the unit: several rows per unit
#' ilm_check_missing(d, y = "score", unit = "id", verbose = FALSE)
#' @export
ilm_check_missing <- function(data, y = NULL, covariates = NULL,
                              min_effect = 0.1, alpha = 0.05, adjust = "holm",
                              verbose = TRUE, unit = NULL,
                              covariates_negate = FALSE,
                              covariates_fixed = FALSE, subset = NULL,
                              subset_negate = FALSE, subset_fixed = FALSE) {

  # ---- 1. Arguments that do not look at the rows ---------------------------
  # [illumex's own choice] the defaults min_effect = 0.1 (the smallest effect
  # that counts as a finding), alpha = 0.05 and adjust = "holm".
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame, not an object of class \"",
         paste(class(data), collapse = "\", \""), "\".", call. = FALSE)
  }
  # A formula `outcome ~ predictors` names y, and its right-hand side counts as
  # covariates given by name (an explicit `covariates` wins).
  parsed <- ilm_miss_parse_y(y, covariates)
  y <- parsed$y
  covariates <- parsed$covariates
  if (!is.null(y) && !(y %in% names(data))) {
    stop("`y` = \"", ilm_show_text(y), "\" is not a column of `data`.",
         call. = FALSE)
  }
  ilm_miss_check_args(min_effect, alpha, adjust, verbose)
  ilm_miss_check_unit(unit, data, y, covariates)

  # ---- 2. Rows: the package's row selector comes first ---------------------
  rs <- ilm_select_rows(data, subset, subset_negate = subset_negate,
                        subset_fixed = subset_fixed)
  data <- rs$data
  n <- as.integer(nrow(data))

  # ---- 3. Covariates for the outcome test ----------------------------------
  # Given covariates (by name, pattern, predicate, or a formula's right-hand
  # side) go through the package's resolver with y and unit excluded. Not
  # given: every column except y, v and unit, decided per v in step 8.
  covariates_given <- !is.null(covariates) || isTRUE(covariates_negate)
  if (covariates_given) {
    if (is.character(covariates) && !length(covariates) &&
        !isTRUE(covariates_negate)) {
      cov_used <- character(0)  # e.g. `y ~ 1`: no covariates at all
    } else {
      cov_used <- ilm_resolve_cols(data, covariates,
                                   exclude = as.character(c(y, unit)),
                                   arg = "covariates",
                                   negate = covariates_negate,
                                   fixed = covariates_fixed)
    }
    sel_cols <- covariates
    sel_used <- cov_used
  } else {
    cov_used <- NULL
    sel_cols <- NULL
    sel_used <- names(data)
  }
  finish <- function(res) {
    ilm_select_finish(res, data = data, rs = rs, cols = sel_cols,
                      used = sel_used, cols_negate = covariates_negate,
                      cols_fixed = covariates_fixed)
  }

  # ---- 4. Units -------------------------------------------------------------
  # [illumex's own choice] a row without a unit cannot be placed in a cluster,
  # so it is an error that gives the count.
  units <- NULL
  n_units <- NA_integer_
  if (!is.null(unit)) {
    units <- data[[unit]]
    n_bad <- sum(ilm_miss_na(units))
    if (n_bad > 0L) {
      stop("the `unit` column \"", ilm_show_text(unit), "\" has ", n_bad,
           " missing value", if (n_bad > 1L) "s", "; every row needs a unit.",
           call. = FALSE)
    }
    n_units <- as.integer(length(unique(units)))
  }

  # ---- 5. How much is missing, and where ------------------------------------
  na_list <- lapply(data, ilm_miss_na)
  n_missing <- vapply(na_list, function(m) as.integer(sum(m)), integer(1))
  any_na <- Reduce(`|`, na_list, logical(n))
  n_complete <- as.integer(sum(!any_na))
  variables <- data.frame(
    variable = names(data),
    n_missing = unname(n_missing),
    p_missing = if (n > 0L) unname(n_missing) / n else rep(NA_real_, length(n_missing)),
    stringsAsFactors = FALSE
  )
  # Largest share first; order() keeps ties in the data's column order.
  variables <- variables[order(-variables$n_missing), , drop = FALSE]
  rownames(variables) <- NULL

  incomplete <- names(data)[n_missing > 0L]

  if (!length(incomplete)) {
    res <- ilm_miss_result(variables = variables, patterns = NULL,
                           associations = NULL, outcome_test = NULL,
                           monotone = NA, n = n, n_complete = n, y = y,
                           min_effect = min_effect, unit = unit,
                           n_units = n_units, verdict = "NONE")
    res <- finish(res)
    if (verbose) message(paste(ilm_miss_lines(res, narrate = TRUE), collapse = "\n"))
    return(res)
  }

  # [illumex's own choice] Without `unit`, the package's frame check looks for
  # the shape of several rows per unit; the caution is a warning, given once,
  # at the end, and it never changes the result.
  frame_note <- if (is.null(unit)) ilm_miss_frame_note(data) else NULL

  # ---- 6. Patterns and monotonicity -----------------------------------------
  # The unit column never has missing values, so it never enters here.
  patterns <- ilm_miss_patterns(na_list, incomplete)
  monotone <- ilm_miss_monotone(na_list, incomplete)

  # ---- 7. The association screen --------------------------------------------
  others <- setdiff(names(data), unit)
  associations <- ilm_miss_screen(data, incomplete, others, na_list, units, y,
                                  adjust, alpha, min_effect)

  # ---- 8. The outcome test ---------------------------------------------------
  outcome_test <- NULL
  if (!is.null(y)) {
    outcome_test <- ilm_miss_outcome(data, incomplete, y, cov_used, others,
                                     na_list, units, adjust, alpha, min_effect)
  }

  # ---- 9. The verdict --------------------------------------------------------
  # [illumex's own choice] the four labels and their order of precedence.
  verdict <- if (!is.null(outcome_test) && any(outcome_test$flag)) {
    "RELATED_TO_OUTCOME"
  } else if (!is.null(associations) && any(associations$flag)) {
    "RELATED_TO_COVARIATES"
  } else {
    "MCAR_NOT_REJECTED"
  }

  res <- ilm_miss_result(variables = variables, patterns = patterns,
                         associations = associations,
                         outcome_test = outcome_test, monotone = monotone,
                         n = n, n_complete = n_complete, y = y,
                         min_effect = min_effect, unit = unit,
                         n_units = n_units, verdict = verdict)
  res <- finish(res)
  if (verbose) message(paste(ilm_miss_lines(res, narrate = TRUE), collapse = "\n"))
  # A warning, not narration: it concerns the validity of the p-values, so it
  # is given even when verbose = FALSE, once per call.
  if (!is.null(frame_note)) warning(frame_note, call. = FALSE)
  res
}


#' @rdname ilm_check_missing
#' @param x An `ilm_missing` object.
#' @param ... Not used.
#' @export
print.ilm_missing <- function(x, ...) {
  cat(ilm_miss_lines(x, narrate = FALSE), sep = "\n")
  invisible(x)
}


# ===========================================================================
# Internal helpers
# ===========================================================================

#' Read `y`: one column name, or a formula `outcome ~ predictors`
#'
#' A formula's response becomes `y`; its right-hand side's variable names
#' become `covariates` unless `covariates` was given. A right-hand side
#' containing `.` leaves `covariates` as not given (every other column).
#' @keywords internal
#' @noRd
ilm_miss_parse_y <- function(y, covariates) {
  if (inherits(y, "formula")) {
    if (length(y) != 3L) {
      stop("the formula given as `y` needs a response: write it as ",
           "`outcome ~ predictors`.", call. = FALSE)
    }
    lhs <- all.vars(y[[2L]])
    if (length(lhs) != 1L) {
      stop("the left-hand side of the formula given as `y` must name one ",
           "outcome; it names ", length(lhs), ".", call. = FALSE)
    }
    rhs <- all.vars(y[[3L]])
    if (is.null(covariates) && !("." %in% rhs)) covariates <- rhs
    y <- lhs
  }
  if (!is.null(y) && (!is.character(y) || length(y) != 1L || is.na(y))) {
    stop("`y` must be a single string naming a column, or a formula ",
         "`outcome ~ predictors`.", call. = FALSE)
  }
  list(y = y, covariates = covariates)
}

#' Check the scalar arguments
#' @keywords internal
#' @noRd
ilm_miss_check_args <- function(min_effect, alpha, adjust, verbose) {
  if (!is.numeric(min_effect) || length(min_effect) != 1L ||
      is.na(min_effect) || min_effect < 0) {
    stop("`min_effect` must be a single number of at least 0.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  # Any method stats::p.adjust() accepts: let p.adjust() itself decide.
  ok <- is.character(adjust) && length(adjust) == 1L && !is.na(adjust) &&
    !inherits(tryCatch(stats::p.adjust(0.5, method = adjust),
                       error = function(e) e), "error")
  if (!ok) {
    stop("`adjust` must be a method that stats::p.adjust() accepts: ",
         paste(stats::p.adjust.methods, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check `unit` against the data, `y` and the named covariates
#' @keywords internal
#' @noRd
ilm_miss_check_unit <- function(unit, data, y, covariates) {
  if (is.null(unit)) return(invisible(TRUE))
  if (!is.character(unit) || length(unit) != 1L || is.na(unit)) {
    stop("`unit` must be a single string naming a column of `data`.",
         call. = FALSE)
  }
  if (!(unit %in% names(data))) {
    stop("`unit` = \"", ilm_show_text(unit), "\" is not a column of `data`.",
         call. = FALSE)
  }
  if (!is.null(y) && identical(unit, y)) {
    stop("`unit` cannot also be the outcome `y`.", call. = FALSE)
  }
  # Named covariates (by name, or on a formula's right-hand side). A pattern
  # or predicate that happens to match the unit column is resolved with the
  # unit excluded instead.
  if (is.character(covariates) && unit %in% covariates) {
    stop("`unit` cannot also be one of the named `covariates`.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Which rows of a column are missing (one logical per row)
#' @keywords internal
#' @noRd
ilm_miss_na <- function(x) {
  if (is.null(dim(x))) return(as.vector(is.na(x)))
  rowSums(is.na(as.matrix(x))) > 0
}

#' A plain column (an atomic vector) that a test can use
#' @keywords internal
#' @noRd
ilm_miss_plain <- function(x) is.atomic(x) && is.null(dim(x))

#' Numeric in the sense of this function: integer or double
#'
#' is.numeric() is FALSE for logical, factor, Date, date-time and difftime
#' columns. The screen treats all of them as categorical. The outcome test
#' treats Date, date-time and difftime columns by their own rule (see
#' ilm_miss_is_time()).
#' @keywords internal
#' @noRd
ilm_miss_is_num <- function(x) is.numeric(x) && !is.factor(x)

#' A date, date-time or duration column (Date, POSIXct or difftime)
#'
#' [illumex's own choice] in the screen such a column is categorical, one
#' level per distinct value. In the outcome test, as a covariate or as y, it
#' is judged by the numeric rule (dropped only when it has zero spread on the
#' rows used) and enters as ONE numeric column, its numeric value (days for a
#' Date, seconds for a date-time, the stored number for a duration), whatever
#' its number of distinct values: a daily date is a linear time term. For a
#' linear term the unit changes neither the test nor the effect. (The value
#' is counted from its earliest value on the rows used; see
#' ilm_miss_prepare().)
#' @keywords internal
#' @noRd
ilm_miss_is_time <- function(x) inherits(x, c("Date", "POSIXct", "difftime"))

#' A categorical column as a factor of the levels present
#'
#' One level per distinct value. A factor keeps its own level order (so its
#' first present level is the reference); other types are ordered by value.
#' @keywords internal
#' @noRd
ilm_miss_as_factor <- function(x) {
  if (is.factor(x)) {
    present <- levels(x)[tabulate(as.integer(x), nbins = nlevels(x)) > 0L]
    return(factor(as.character(x), levels = present))
  }
  ux <- unique(x)
  ux <- ux[order(ux, method = "radix")]
  factor(match(x, ux), levels = seq_along(ux),
         labels = make.unique(as.character(ux)))
}

#' Is every value constant within each unit?
#'
#' `vals` is a list of vectors over the same rows as `u`.
#' @keywords internal
#' @noRd
ilm_miss_constant_within <- function(u, vals) {
  first <- match(u, u)  # each row's unit's first row
  for (x in vals) {
    if (any(x != x[first])) return(FALSE)
  }
  TRUE
}

#' Assemble the result, with every field in its fixed order
#' @keywords internal
#' @noRd
ilm_miss_result <- function(variables, patterns, associations, outcome_test,
                            monotone, n, n_complete, y, min_effect, unit,
                            n_units, verdict) {
  structure(list(variables = variables, patterns = patterns,
                 associations = associations, outcome_test = outcome_test,
                 monotone = monotone, n = n, n_complete = n_complete, y = y,
                 min_effect = min_effect, unit = unit, n_units = n_units,
                 verdict = verdict),
            class = "ilm_missing")
}


# ---- Patterns and monotonicity ----------------------------------------------

#' Missingness patterns over the incomplete columns
#'
#' Each row's pattern is a string with one character per incomplete column
#' (in the data's order), "1" where that column is missing.
#' @keywords internal
#' @noRd
ilm_miss_patterns <- function(na_list, incomplete) {
  key <- do.call(paste0, lapply(na_list[incomplete],
                                function(m) ifelse(m, "1", "0")))
  pat <- unique(key)
  cnt <- tabulate(match(key, pat), nbins = length(pat))
  # Most common first. [illumex's own choice] ties are broken by the pattern
  # string, so the table does not depend on the order of the rows.
  o <- order(-cnt, pat, method = "radix")
  out <- data.frame(pattern = pat[o], n = as.integer(cnt[o]),
                    n_missing = as.integer(nchar(gsub("0", "", pat[o],
                                                      fixed = TRUE))),
                    stringsAsFactors = FALSE)
  attr(out, "variables") <- incomplete
  out
}

#' Is the missingness pattern monotone?
#'
#' [published: Little and Rubin 2019, p. 11, Example 1.6, Figure 1.1c] the
#' pattern is monotone when the variables can be arranged so that all later
#' variables are missing wherever an earlier one is missing; that is, the
#' sets of rows missing each variable are nested.
#' [illumex's own choice] the procedure: order the incomplete columns by their
#' missing count, smallest first, and check each adjacent pair. Nested sets
#' must have increasing counts, so this order is the only candidate; columns
#' with tied counts are nested only if they are missing in the same rows, and
#' then their order does not matter.
#' @keywords internal
#' @noRd
ilm_miss_monotone <- function(na_list, incomplete) {
  if (length(incomplete) < 2L) return(TRUE)  # one column: monotone by definition
  cnt <- vapply(na_list[incomplete], sum, integer(1))
  ord <- incomplete[order(cnt)]
  for (j in seq_len(length(ord) - 1L)) {
    earlier <- na_list[[ord[j]]]
    later <- na_list[[ord[j + 1L]]]
    if (any(earlier & !later)) return(FALSE)
  }
  TRUE
}


# ---- The association screen --------------------------------------------------

#' The association screen over all (v, w) pairs
#'
#' [published: Little and Rubin 2019, section 3.2, p. 50] the incomplete units
#' can be used to study whether the complete units are plausibly a random
#' subsample; Little and Rubin compare each variable's distribution in the
#' complete units with that in the incomplete units where it is recorded.
#' [illumex's own choice] this screen is a variant of that comparison: for
#' each incomplete column v and each other column w, it relates the indicator
#' "v is missing" to w over the rows where w is observed.
#' @keywords internal
#' @noRd
ilm_miss_screen <- function(data, incomplete, others, na_list, units, y,
                            adjust, alpha, min_effect) {
  mi <- rt <- me <- character(0)
  ef <- pv <- numeric(0)
  # Built in the data's column order: v, then w. Sorting below is stable, so
  # ties in the effect keep this order.
  for (v in incomplete) {
    for (w in others) {
      if (identical(w, v)) next
      r <- ilm_miss_screen_pair(na_list[[v]], data[[w]], w, na_list[[w]],
                                units)
      if (is.null(r)) next
      mi <- c(mi, v)
      rt <- c(rt, w)
      ef <- c(ef, r$effect)
      pv <- c(pv, r$p)
      me <- c(me, r$method)
    }
  }
  if (!length(mi)) return(NULL)
  tab <- data.frame(missing_in = mi, related_to = rt, effect = ef,
                    p_value = pv,
                    is_outcome = if (is.null(y)) rep(FALSE, length(rt)) else rt == y,
                    stringsAsFactors = FALSE)
  # [illumex's own choice] adjust across ALL rows of the screen.
  tab$p_adj <- stats::p.adjust(tab$p_value, method = adjust)
  # [illumex's own choice] a flag needs both significance and size: with many
  # rows a negligible association is significant.
  tab$flag <- !is.na(tab$p_adj) & tab$p_adj < alpha & tab$effect >= min_effect
  tab$method <- me
  # [illumex's own choice] correlations and Cramér's V sorted together on one
  # 0-1 scale; they are not the same quantity.
  tab <- tab[order(-tab$effect), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

#' One screen pair: the indicator "v is missing" against w
#'
#' Returns NULL when the pair is skipped, else list(effect, p, method).
#' @keywords internal
#' @noRd
ilm_miss_screen_pair <- function(miss_v, w, w_name, miss_w, units) {
  if (!ilm_miss_plain(w)) return(NULL)
  keep <- !miss_w
  z <- miss_v[keep]  # TRUE where v is missing, over the rows where w is observed
  wv <- w[keep]
  u <- NULL
  method <- "rows"
  if (!is.null(units)) {
    # [illumex's own choice] one row per unit when the indicator and w are
    # constant within every unit; otherwise the cluster-robust test.
    u <- units[keep]
    if (ilm_miss_constant_within(u, list(z, wv))) {
      first <- !duplicated(u)
      z <- z[first]
      wv <- wv[first]
      u <- NULL
      method <- "units"
    } else {
      method <- "sandwich"
    }
  }

  # [illumex's own choice] skip rules; on the "units" path they count units.
  if (length(z) < 10L || all(z) || !any(z)) return(NULL)
  numeric_w <- ilm_miss_is_num(wv)
  if (numeric_w) {
    wv <- as.numeric(wv)
    if (!(max(wv) > min(wv))) return(NULL)  # zero spread
    # [illumex's own choice] effect: the absolute Pearson correlation between
    # the 0/1 indicator and w. Its test is the usual t test that the
    # correlation is zero, t = r sqrt((n - 2) / (1 - r^2)) on n - 2 df.
    r <- stats::cor(as.numeric(z), wv)
    if (!is.finite(r)) return(NULL)  # e.g. infinite values in w
    r <- max(-1, min(1, r))
    effect <- abs(r)
    p <- ilm_miss_cor_p(r, length(z))
  } else {
    f <- ilm_miss_as_factor(wv)
    k <- nlevels(f)
    if (k < 2L) return(NULL)  # fewer than two levels present
    # Pearson's chi-squared test of independence on the 2 x k table, WITHOUT
    # a continuity correction. [published: Cramér 1946, section 30.5, p. 443,
    # the measure X^2 / (n (q - 1)); its square root is Cramér's V, Agresti
    # 2013, p. 110, Exercise 3.32] Cramér's V^2 = X^2 / (n min(I - 1, J - 1));
    # here I = 2 (missing or not), so min(I - 1, J - 1) = 1.
    cs <- ilm_miss_chisq(z, as.integer(f), k)
    effect <- sqrt(cs$stat / (length(z) * min(2L - 1L, k - 1L)))
    p <- cs$p
  }

  if (method == "sandwich") {
    # The effect above stays as computed on the rows; the p-value comes from
    # the cluster-robust Wald test of w's coefficients.
    frame <- list(if (numeric_w) wv else f)
    names(frame) <- w_name
    frame <- list2DF(frame)
    X <- ilm_miss_design(frame)
    test <- which(attr(X, "assign") == 1L)
    p <- if (length(unique(u)) < 30L) {
      NA_real_  # [illumex's own choice] fewer than 30 units: no test
    } else {
      ilm_miss_cluster_wald(X, z, u, ilm_miss_logit(X, z), test)
    }
  }
  list(effect = min(1, effect), p = p, method = method)
}

#' Two-sided p-value of the t test that a Pearson correlation is zero
#' @keywords internal
#' @noRd
ilm_miss_cor_p <- function(r, n) {
  df <- n - 2
  if (abs(r) >= 1) return(0)
  t <- r * sqrt(df / (1 - r^2))
  2 * stats::pt(-abs(t), df)
}

#' Pearson's chi-squared test of independence, uncorrected, on a 2 x k table
#'
#' `z` is the logical indicator, `g` integer level codes 1..k (all present).
#' @keywords internal
#' @noRd
ilm_miss_chisq <- function(z, g, k) {
  obs <- rbind(tabulate(g[z], nbins = k), tabulate(g[!z], nbins = k))
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  stat <- sum((obs - expected)^2 / expected)
  df <- k - 1L
  list(stat = stat, df = df, p = stats::pchisq(stat, df, lower.tail = FALSE))
}


# ---- The outcome test --------------------------------------------------------

#' The outcome test, one row per incomplete column
#'
#' [illumex's own choice, throughout] this is the package's own
#' operationalisation of the published condition (White and Carlin 2010,
#' p. 2921; Hughes et al. 2019, p. 1296): is being complete independent of y
#' given the covariates? None of the sources prescribes a test for it.
#' @keywords internal
#' @noRd
ilm_miss_outcome <- function(data, incomplete, y, cov_used, others, na_list,
                             units, adjust, alpha, min_effect) {
  rows <- lapply(incomplete, function(v) {
    # The covariates: the resolved ones without v and y; when none were given,
    # every column except y, v and unit.
    covs <- if (is.null(cov_used)) setdiff(others, c(y, v)) else setdiff(cov_used, c(y, v))
    ilm_miss_outcome_one(data, v, y, covs, na_list, units)
  })
  out <- data.frame(missing_in = incomplete,
                    effect = vapply(rows, `[[`, numeric(1), "effect"),
                    p_value = vapply(rows, `[[`, numeric(1), "p"),
                    stringsAsFactors = FALSE)
  # [illumex's own choice] adjust across the incomplete columns; flag only
  # when both the adjusted p-value and the effect pass.
  out$p_adj <- stats::p.adjust(out$p_value, method = adjust)
  out$flag <- !is.na(out$p_adj) & out$p_adj < alpha &
    !is.na(out$effect) & out$effect >= min_effect
  out$method <- vapply(rows, `[[`, character(1), "method")
  # For printing: "partial R" when y is categorical with more than two levels.
  # A Date, date-time or duration y enters as one column: "partial r".
  yv <- data[[y]]
  y_levels <- length(unique(yv[!na_list[[y]]]))
  attr(out, "effect_label") <- if (!ilm_miss_is_num(yv) &&
                                   !ilm_miss_is_time(yv) && y_levels > 2L) {
    "partial R"
  } else {
    "partial r"
  }
  out
}

#' The outcome test for one incomplete column v
#'
#' Returns list(effect, p, method); effect and p are NA when the test cannot
#' be run.
#' @keywords internal
#' @noRd
ilm_miss_outcome_one <- function(data, v, y, covs, na_list, units) {
  answer <- function(method, effect = NA_real_, p = NA_real_) {
    list(effect = effect, p = p, method = method)
  }
  method0 <- if (is.null(units)) "rows" else "sandwich"
  if (!ilm_miss_plain(data[[y]])) return(answer(method0))
  covs <- covs[vapply(covs, function(cn) ilm_miss_plain(data[[cn]]),
                      logical(1))]

  # Rows: y and every candidate covariate observed. The indicator "v is
  # missing" is defined on all of them. When v is y, these rows all have y
  # observed, the indicator is constant, and the test returns NA below.
  keep <- !na_list[[y]]
  for (cn in covs) keep <- keep & !na_list[[cn]]
  z <- na_list[[v]][keep]
  fr <- data[keep, c(covs, y), drop = FALSE]
  u <- if (is.null(units)) NULL else units[keep]

  # [illumex's own choice] drop covariates that cannot enter a model on these
  # rows: numeric, or a Date, date-time or duration, with zero spread;
  # categorical with fewer than 2 levels present, or with more than 20 levels.
  covs <- ilm_miss_usable(fr, covs)

  method <- "rows"
  if (!is.null(u)) {
    # [illumex's own choice] one row per unit when the indicator, y and the
    # covariates are all constant within every unit; else the sandwich.
    if (ilm_miss_constant_within(u, c(list(z), as.list(fr[c(covs, y)])))) {
      first <- !duplicated(u)
      z <- z[first]
      fr <- fr[first, , drop = FALSE]
      u <- NULL
      method <- "units"
    } else {
      method <- "sandwich"
    }
  }

  # [illumex's own choice] fewer than 20 rows (units, on the "units" path), or
  # a constant indicator: no test.
  if (length(z) < 20L || all(z) || !any(z)) return(answer(method))

  # The model columns: numeric covariates linearly, categorical ones as
  # treatment-coded indicators, Date, date-time and duration ones as one
  # numeric column each, whatever their number of distinct values; y last
  # (one column if numeric, a Date, a date-time or a duration, else k - 1).
  # A numeric or date y with zero spread is aliased with the intercept, which
  # gives NA below.
  prepared <- ilm_miss_prepare(fr[c(covs, y)])
  y_prepared <- prepared[[ncol(prepared)]]
  if (is.factor(y_prepared) && nlevels(y_prepared) < 2L) return(answer(method))
  X <- ilm_miss_design(prepared)
  y_cols <- which(attr(X, "assign") == ncol(prepared))
  X0 <- X[, -y_cols, drop = FALSE]  # intercept and covariates only

  # Two logistic regressions of the indicator, each with an intercept: on the
  # covariates, and on the covariates plus y.
  fit0 <- ilm_miss_logit(X0, z)
  fit1 <- ilm_miss_logit(X, z)
  if (is.null(fit0) || is.null(fit1)) return(answer(method))
  # y's columns aliased with the covariates: nothing to test.
  if (anyNA(fit1$coefficients[y_cols])) return(answer(method))

  # [illumex's own choice] the effect: the multiple correlation of the
  # covariates-only model's Pearson residuals on y's columns residualised on
  # the covariates. Computed on the rows used (units on the "units" path).
  effect <- ilm_miss_partial_effect(z, fit0$fitted.values, X0,
                                    X[, y_cols, drop = FALSE])
  if (is.na(effect)) return(answer(method))

  if (method == "sandwich") {
    # Cluster-robust Wald test of y's coefficients in the larger model.
    p <- if (length(unique(u)) < 30L) {
      NA_real_  # [illumex's own choice] fewer than 30 units: no test
    } else {
      ilm_miss_cluster_wald(X, z, u, fit1, y_cols)
    }
  } else {
    # Likelihood-ratio test: the drop in deviance, referred to chi-squared on
    # the number of added (estimable) parameters, 1 for a numeric y and k - 1
    # for a categorical one.
    lr <- max(0, fit0$deviance - fit1$deviance)
    df <- fit1$rank - fit0$rank
    p <- if (df >= 1L) stats::pchisq(lr, df, lower.tail = FALSE) else NA_real_
  }
  answer(method, effect = effect, p = p)
}

#' Covariates that can enter a model on the given rows
#'
#' Numeric, and also Date, date-time and duration columns: dropped with zero
#' spread, whatever their number of distinct values. Categorical: kept only
#' with 2 to 20 levels present.
#' @keywords internal
#' @noRd
ilm_miss_usable <- function(frame, covs) {
  ok <- vapply(covs, function(cn) {
    x <- frame[[cn]]
    if (!length(x)) return(FALSE)
    if (ilm_miss_is_num(x) || ilm_miss_is_time(x)) {
      x <- as.numeric(x)
      return(max(x) > min(x))  # zero spread: dropped
    }
    k <- length(unique(x))
    k >= 2L && k <= 20L  # [illumex's own choice] the 20-level cut-off
  }, logical(1))
  covs[ok]
}

#' The model columns' types
#'
#' Numeric columns as doubles; Date, date-time and duration columns as one
#' numeric column each [illumex's own choice]; every other column as a
#' factor of its levels.
#'
#' A Date, date-time or duration enters as its numeric value counted from its
#' earliest value on these rows: the time elapsed since then (days for a
#' Date, seconds for a date-time, the stored unit for a duration). Every model
#' has an intercept, so the shift changes neither the test nor the effect.
#' It keeps the numbers small: date-times stored as seconds since 1970 that
#' lie a few minutes apart would otherwise look constant to qr()'s default
#' tolerance in the effect's least-squares step.
#' @keywords internal
#' @noRd
ilm_miss_prepare <- function(frame) {
  out <- lapply(frame, function(x) {
    if (ilm_miss_is_time(x)) {
      t <- as.numeric(x)
      t - min(t)
    } else if (ilm_miss_is_num(x)) {
      as.numeric(x)
    } else {
      ilm_miss_as_factor(x)
    }
  })
  names(out) <- names(frame)
  list2DF(out, nrow = nrow(frame))
}

#' Model matrix with an intercept, factors treatment-coded
#'
#' The formula is built from ilm_bq() so that non-syntactic names work.
#' Treatment coding is requested explicitly, whatever options("contrasts")
#' says. The "assign" attribute maps columns to variables (0 = intercept).
#' @keywords internal
#' @noRd
ilm_miss_design <- function(frame) {
  if (!ncol(frame)) {
    X <- matrix(1, nrow(frame), 1L, dimnames = list(NULL, "(Intercept)"))
    attr(X, "assign") <- 0L
    return(X)
  }
  fml <- stats::reformulate(ilm_bq(names(frame)))
  facs <- names(frame)[vapply(frame, is.factor, logical(1))]
  ctr <- if (length(facs)) {
    stats::setNames(rep(list("contr.treatment"), length(facs)), facs)
  } else {
    NULL
  }
  stats::model.matrix(fml, data = frame, contrasts.arg = ctr)
}

#' Logistic regression by maximum likelihood; NULL when the fit fails
#'
#' A fit fails when it errors, does not converge, or has no finite deviance.
#' Warnings (such as fitted probabilities of 0 or 1) are not failures.
#' @keywords internal
#' @noRd
ilm_miss_logit <- function(X, z) {
  fit <- tryCatch(
    withCallingHandlers(
      stats::glm.fit(X, as.numeric(z), family = stats::binomial()),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || !isTRUE(fit$converged) || !is.finite(fit$deviance)) {
    return(NULL)
  }
  fit
}

#' The outcome test's effect: a (multiple) partial correlation
#'
#' r: Pearson residuals of the covariates-only logistic model,
#' (z - mu0) / sqrt(mu0 (1 - mu0)). E: y's model columns, each replaced by its
#' least-squares residuals on the covariates' design X0 (with intercept; with
#' no covariates, that is centring). The effect is sqrt(R^2) of the
#' least-squares regression of r on E with an intercept. With one column in E
#' it equals |cor(r, E)|. R^2 depends only on the column space of E, so the
#' choice of reference level, or the order of the levels, does not change it.
#' @keywords internal
#' @noRd
ilm_miss_partial_effect <- function(z, mu0, X0, E) {
  r <- (as.numeric(z) - mu0) / sqrt(mu0 * (1 - mu0))
  if (!all(is.finite(r))) return(NA_real_)
  E_res <- qr.resid(qr(X0), as.matrix(E))
  rss <- sum(qr.resid(qr(cbind(1, E_res)), r)^2)
  tss <- sum((r - mean(r))^2)
  if (!is.finite(tss) || tss <= 0 || !is.finite(rss)) return(NA_real_)
  sqrt(min(1, max(0, 1 - rss / tss)))
}


# ---- The cluster-robust (sandwich) test ---------------------------------------

#' Cluster-robust sandwich covariance of a logistic regression
#'
#' [published: Liang and Zeger 1986, Theorem 1 and the estimator under eq. (4),
#' p. 15; Agresti 2013, p. 467, eq. (12.5), and p. 469] the independence
#' working model's estimates are the ordinary maximum-likelihood estimates,
#' and their covariance is estimated by B^{-1} M B^{-1}. For the logistic
#' model with its canonical link, Liang and Zeger's Delta is the identity and
#' A = diag(mu (1 - mu)), so
#'   B = sum over rows of x x' mu (1 - mu)   (the model's information),
#'   M = sum over UNITS of s_u s_u',  s_u = sum over the unit's rows of
#'       x (z - mu),
#' with z the 0/1 indicator and mu the fitted probability.
#' [illumex's own choice] no finite-cluster correction is applied.
#' Returns NULL when B cannot be inverted.
#' @keywords internal
#' @noRd
ilm_miss_sandwich <- function(X, z, mu, u) {
  X <- as.matrix(X)
  e <- as.numeric(z) - mu
  B <- crossprod(X, X * (mu * (1 - mu)))         # row i weighted by mu_i (1 - mu_i)
  S <- rowsum(X * e, group = u, reorder = FALSE)  # one summed score per unit
  M <- crossprod(S)
  B_inv <- tryCatch(solve(B), error = function(err) NULL)
  if (is.null(B_inv) || !all(is.finite(B_inv))) return(NULL)
  V <- B_inv %*% M %*% B_inv
  V <- (V + t(V)) / 2  # symmetric up to rounding
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

#' Wald test of the columns `test` of X, with the cluster-robust covariance
#'
#' `fit` is the ordinary maximum-likelihood fit on all the rows (or NULL when
#' it failed, giving NA). Aliased columns are left out of the sandwich; if a
#' tested column is aliased, the result is NA.
#' @keywords internal
#' @noRd
ilm_miss_cluster_wald <- function(X, z, u, fit, test) {
  if (is.null(fit)) return(NA_real_)
  b <- fit$coefficients
  if (anyNA(b[test])) return(NA_real_)
  kept <- which(!is.na(b))
  V <- ilm_miss_sandwich(X[, kept, drop = FALSE], z, fit$fitted.values, u)
  if (is.null(V)) return(NA_real_)
  idx <- match(test, kept)
  ilm_miss_wald_p(b[test], V[idx, idx, drop = FALSE])
}

#' p-value of a Wald test
#'
#' One coefficient [published: Agresti 2013, p. 467]: z = estimate / SE,
#' two-sided. Several coefficients [illumex's own choice; Agresti gives only
#' the one-parameter form]: the quadratic form b' V^{-1} b, referred to
#' chi-squared on length(b) degrees of freedom. NA when V is not invertible.
#' @keywords internal
#' @noRd
ilm_miss_wald_p <- function(b, V) {
  b <- as.numeric(b)
  V <- as.matrix(V)
  if (!all(is.finite(b)) || !all(is.finite(V))) return(NA_real_)
  if (length(b) == 1L) {
    if (!(V[1L, 1L] > 0)) return(NA_real_)
    return(2 * stats::pnorm(-abs(b / sqrt(V[1L, 1L]))))
  }
  V_inv <- tryCatch(solve(V), error = function(err) NULL)
  if (is.null(V_inv)) return(NA_real_)
  W <- drop(crossprod(b, V_inv %*% b))
  if (!is.finite(W) || W < 0) return(NA_real_)
  stats::pchisq(W, df = length(b), lower.tail = FALSE)
}


# ---- Several rows per unit, without `unit` --------------------------------------

#' The caution given when the data look like several rows per unit
#'
#' [illumex's own choice] reuses the package's frame check: its "unit_level"
#' finding (a numeric column taking one value per level of a categorical
#' column) is the shape of several rows per unit. The caution never changes
#' the result, so a failure of the check itself gives no caution rather than
#' an error. Returns the warning's text, or NULL.
#' @keywords internal
#' @noRd
ilm_miss_frame_note <- function(data) {
  ff <- tryCatch(ilm_frame_find(data), error = function(e) NULL)
  tab <- ff$table
  if (is.null(tab) || !NROW(tab)) return(NULL)
  hit <- which(tab$issue == "unit_level")
  if (!length(hit)) return(NULL)
  groups <- unique(vapply(ff$vars[hit], function(v) as.character(v[2L]),
                          character(1)))
  groups <- groups[!is.na(groups)]
  if (!length(groups)) return(NULL)
  shown <- ilm_show_text(groups)
  paste0("The data look like several rows per unit: other columns take one ",
         "value per level of ", paste0("`", shown, "`", collapse = ", "),
         ". The p-values assume independent rows. If ",
         if (length(groups) > 1L) "one of these" else "it",
         " identifies the unit, name it in `unit` (for example `unit = \"",
         shown[1L], "\"`).")
}


# ---- Printing and narration ---------------------------------------------------

#' The one-line meaning of each verdict
#'
#' `k` is the number of missing values and `m` the number of columns with any
#' missing value. [illumex's own choice] the wording of the lines, and "value"
#' and "column" in the singular when the count is 1.
#'
#' The basis of each line:
#' * MCAR_NOT_REJECTED [published: Little and Rubin 2019, section 3.2, p. 50]
#'   such checks are useful but have limited power when the incomplete units
#'   are few; "not rejected" is not evidence of absence.
#' * RELATED_TO_COVARIATES [published: White and Carlin 2010, section 2.2,
#'   p. 2921; Little and Rubin 2019, Example 3.3, p. 49] complete-case
#'   regression is unbiased whenever being complete is independent of y given
#'   the covariates, the incomplete covariate's own values included.
#' * RELATED_TO_OUTCOME [published: White and Carlin 2010, p. 2921, including
#'   the mechanism p(C | X, Y) = f(X), and p. 2922] complete cases are unbiased
#'   when missingness depends on the covariate, its own missing values
#'   included; with one covariate, multiple imputation is valid if missingness
#'   depends on y only, complete cases if it depends on the covariate only,
#'   both only under MCAR. [published: White and Carlin 2010, abstract,
#'   p. 2920] when missingness is independent of the outcome given the
#'   covariates (as when a covariate is missing because of its own value),
#'   multiple imputation is biased away from the null. [published: White and
#'   Carlin 2010, p. 2928] perform both analyses and explain any differences;
#'   [published: White and Carlin 2010, abstract, p. 2920, and p. 2928]
#'   choosing between them by their standard errors is misleading.
#'   [published: Hughes et al. 2019, p. 1302; White and Carlin 2010, p. 2928]
#'   multiple imputation is often preferred when auxiliary variables make MAR
#'   more plausible. The sources do not call the complete-case analysis a
#'   sensitivity analysis, so the line does not.
#' @keywords internal
#' @noRd
ilm_miss_meaning <- function(verdict, has_y, k = 0L, m = 0L) {
  counts <- sprintf("%d missing value%s in %d column%s",
                    as.integer(k), if (k == 1) "" else "s",
                    as.integer(m), if (m == 1) "" else "s")
  switch(verdict,
    MCAR_NOT_REJECTED = paste0(
      "no evidence that missingness is related to any observed variable (",
      counts, "; with few missing values such checks have little power); ",
      "complete cases are unbiased unless missingness depends on unseen values"
    ),
    RELATED_TO_COVARIATES = if (has_y) {
      paste0("related to covariates, with no evidence of a relation to the ",
             "outcome given them (", counts, "): complete cases stay unbiased ",
             "for the regression on those covariates, unless missingness ",
             "depends on unseen values")
    } else {
      paste0("related to observed variables (", counts, "); name `y` to ",
             "check whether complete cases stay unbiased")
    },
    RELATED_TO_OUTCOME = paste0(
      "related to the OUTCOME given the covariates: complete-case estimates ",
      "may be biased. Not when a covariate is missing because of its own ",
      "value: there complete cases are unbiased and imputation assuming MAR ",
      "is the biased choice. Multiple imputation (illume::ilm_impute()) is ",
      "the analysis under MAR; run the complete-case analysis as well and ",
      "explain any difference."
    ),
    NONE = "no missing values",
    ""
  )
}

#' The lines shared by print() and the verbose narration
#'
#' `narrate = TRUE` gives the narration: the counts of columns and rows in the
#' first line, and at most 6 flagged associations; print shows at most 8.
#' @keywords internal
#' @noRd
ilm_miss_lines <- function(x, narrate = FALSE) {
  pct <- function(k, n) {
    paste0(ilm_fx(if (n > 0) 100 * k / n else rep(NA_real_, length(k)), 1), "%")
  }
  fmt_p <- function(p) format.pval(ilm_disp_signif(p, 2), digits = 2, eps = 1e-4)
  max_assoc <- if (narrate) 6L else 8L

  lines <- if (narrate) {
    sprintf("ilm_check_missing(): %d rows, %d columns; %d of %d rows complete (%s)",
            x$n, NROW(x$variables), x$n_complete, x$n,
            pct(x$n_complete, x$n))
  } else {
    sprintf("<ilm_missing> %d of %d rows complete (%s)", x$n_complete, x$n,
            pct(x$n_complete, x$n))
  }
  if (identical(x$verdict, "NONE")) return(c(lines, "  no missing values"))

  if (!is.null(x$unit)) {
    lines <- c(lines, sprintf("  %d units, identified by column `%s`",
                              x$n_units, ilm_show_text(x$unit)))
  }

  # Missing count and percentage for each incomplete column.
  vt <- x$variables[x$variables$n_missing > 0L, , drop = FALSE]
  lines <- c(lines, "  missing values:",
             sprintf("    %s: %d (%s)", ilm_show_text(vt$variable),
                     vt$n_missing, pct(vt$n_missing, x$n)))

  n_pat <- NROW(x$patterns)
  lines <- c(lines, sprintf("  pattern: %s, %d distinct pattern%s",
                            if (isTRUE(x$monotone)) "monotone" else "non-monotone",
                            n_pat, if (n_pat == 1L) "" else "s"))

  # k: the number of missing values; m: the columns with any missing value.
  lines <- c(lines, paste0("  verdict: ", x$verdict),
             paste0("    ", ilm_miss_meaning(x$verdict, !is.null(x$y),
                                             k = sum(x$variables$n_missing),
                                             m = sum(x$variables$n_missing > 0L))))

  ot <- x$outcome_test
  if (!is.null(ot)) {
    label <- attr(ot, "effect_label")
    if (is.null(label)) label <- "partial r"
    lines <- c(lines, sprintf("  outcome test (missingness against `%s`, given the covariates):",
                              ilm_show_text(x$y)))
    for (i in seq_len(nrow(ot))) {
      name <- ilm_show_text(ot$missing_in[i])
      how <- if (ot$method[i] == "rows") "" else paste0(" [", ot$method[i], "]")
      lines <- c(lines, if (identical(ot$missing_in[i], x$y)) {
        sprintf("    %s: not testable (missingness in the outcome itself)", name)
      } else {
        sprintf("    %s: %s = %s, adjusted p = %s%s%s", name, label,
                ilm_fx(ot$effect[i], 3), fmt_p(ot$p_adj[i]), how,
                if (isTRUE(ot$flag[i])) "  *" else "")
      })
    }
  }

  a <- x$associations
  flagged <- if (is.null(a)) NULL else a[a$flag, , drop = FALSE]
  if (NROW(flagged)) {
    shown <- flagged[seq_len(min(nrow(flagged), max_assoc)), , drop = FALSE]
    lines <- c(lines, "  flagged associations (effect; adjusted p):",
               sprintf("    missing in %s ~ %s: %s; p = %s%s",
                       ilm_show_text(shown$missing_in),
                       ilm_show_text(shown$related_to),
                       ilm_fx(shown$effect, 3), fmt_p(shown$p_adj),
                       ifelse(shown$is_outcome, "  [the outcome]", "")))
    if (nrow(flagged) > max_assoc) {
      lines <- c(lines, sprintf("    ... and %d more", nrow(flagged) - max_assoc))
    }
  }

  # [published: Little and Rubin 2019, p. 50 and Example 13.2, p. 306] no test
  # of the observed data can rule out dependence on the unseen values.
  c(lines, paste0("  not testable: whether missingness depends on the unseen ",
                  "values themselves (MNAR); no test of the observed data can ",
                  "rule it out, so assess it with a sensitivity analysis"))
}
