## ---------------------------------------------------------------------------
## A model as `subset`: the rows it analysed (Craig's items 146 and 150).
##
## A table of characteristics beside a model's estimates has to describe the
## rows the model analysed, not every row it was given. illume's ilm_model()
## records the rows it dropped (`na.action`, their positions in the data it
## was given), so `subset = fit` keeps the others, and `subset_negate = TRUE`
## the dropped rows themselves -- the check on whether they differ. An
## ilm_dag_model() holds one fit per adjustment set, and the sets can drop
## different rows: it keeps the rows every set used (item 150).
##
## Beside the rows, the result keeps copies of what identifies each fit --
## its formula, the number of rows it was given, its na.action and any
## subset it records -- so that what sits beside the model can be checked
## against it with identical().
## ---------------------------------------------------------------------------

## what identifies a fit, as the fit holds it; [[ ]] so that no field is
## matched by the start of another's name
#' @keywords internal
#' @noRd
ilm_model_fingerprint <- function(fit)
  list(formula = fit[["formula"]], n_input = fit[["n_input"]],
       na.action = fit[["na.action"]], subset = fit[["subset"]])

## the positions of the rows one fit analysed, among the n rows of `data`
#' @keywords internal
#' @noRd
ilm_model_rows_fit <- function(fit, n) {
  n_input <- fit[["n_input"]]
  if (!is.null(n_input) && n_input != n)
    stop("`data` has ", n, " rows, but the model given as `subset` was given ", n_input,
         ". Use the data the model was fitted to.", call. = FALSE)
  dropped <- as.integer(fit[["na.action"]])
  if (length(dropped) && max(dropped) > n)
    stop("`data` has ", n, " rows, fewer than the model given as `subset` was given. ",
         "Use the data the model was fitted to.", call. = FALSE)
  keep <- setdiff(seq_len(n), dropped)
  used <- NROW(fit[["model"]])
  if (used && length(keep) != used)
    stop("`data` is not the data the model given as `subset` was fitted to: leaving ",
         "out the rows it dropped leaves ", length(keep), ", but it analysed ", used, ".",
         call. = FALSE)
  keep
}

## a model as `subset`: whether each row was analysed, and what the result
## keeps about the model
#' @keywords internal
#' @noRd
ilm_model_rows <- function(model, data) {
  n <- nrow(data)
  if (inherits(model, "ilm_dag_model")) {
    fits <- model[["fits"]]
    if (!length(fits))
      stop("`subset` is an ilm_dag_model() with no fitted adjustment set, so there ",
           "are no analysed rows to keep.", call. = FALSE)
    rows <- Reduce(intersect, lapply(fits, ilm_model_rows_fit, n = n))
    crude <- model[["crude"]]
    return(list(keep = seq_len(n) %in% rows, model = list(
      scope = "sets", fits = unname(lapply(fits, ilm_model_fingerprint)),
      crude = if (!is.null(crude)) ilm_model_fingerprint(crude))))
  }
  list(keep = seq_len(n) %in% ilm_model_rows_fit(model, n),
       model = list(scope = "model", fit = ilm_model_fingerprint(model)))
}

## the words a result prints for a model's rows, in "297 of 300 rows (...)"
#' @keywords internal
#' @noRd
ilm_model_rows_words <- function(s) {
  sets <- identical(s$model$scope, "sets")
  if (isTRUE(s$negate)) {
    if (sets) "left out by an adjustment set" else "dropped by the model"
  } else if (sets) "analysed by every adjustment set" else "analysed by the model"
}
