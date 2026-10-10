## ---------------------------------------------------------------------------
## The cleaning loop: checks that find messy data, remedies as code, and a
## record of what was done and why (item 348).
##
## Guided, never automated. A check says what it found, with the evidence,
## and names one to three ranked remedies; nothing changes until the user
## applies one; every remedy applied is logged with its reason, and the log
## writes out as an R script that replays the cleaning on the raw data.
##
## Rules the checks keep:
##   - no complaint without a remedy: a finding that is not OK names at least
##     one, even if only one made by hand (tested);
##   - the user decides: nothing is changed or removed because a check
##     singled it out;
##   - a remedy is a call to an exported function that takes the data first
##     and returns it, so the script is plain R anyone can read and re-run.
## ---------------------------------------------------------------------------

## the data tiers, least to most consequential (ruled 2026-10-09, item 381)
ilm_clean_tiers <- c("representation", "values", "rows")

ilm_clean_note <- paste0(
  "Apply one with ilm_apply_remedy(data, <this list>, id or key, reason = \"...\"). ",
  "Representation reads the same values correctly; values changes values, ",
  "sets them missing or removes columns; rows drops or excludes rows, so ",
  "apply one of those only by choice.")

ilm_clean_status_order <- c("OK", "WARN", "FAIL")

## ---- check results ----------------------------------------------------------

## One check's result. `findings` has one row per finding (finding, columns,
## status, detail); `candidates` is a list of remedies, each a list with
## finding, status, tier, remedy, key and call (NULL when made by hand).
ilm_data_check_new <- function(check, fun, args, data, findings, candidates, headline) {
  st <- if (nrow(findings))
    ilm_clean_status_order[max(match(findings$status, ilm_clean_status_order))] else "OK"
  structure(list(check = check, status = st, headline = headline,
                 findings = findings, candidates = candidates,
                 target_id = ilm_data_id(data), recheck = list(fun = fun, args = args)),
            class = "ilm_data_check")
}

#' @export
print.ilm_data_check <- function(x, ...) {
  cat(sprintf("<ilm_data_check> %s: %s\n", x$check, x$status))
  cat(ilm_wrap(x$headline, indent = "  "), "\n", sep = "")
  if (nrow(x$findings)) {
    f <- x$findings
    for (i in seq_len(nrow(f)))
      cat(sprintf("  %-4s %s  %s: %s\n", f$status[i], f$finding[i], f$columns[i], f$detail[i]))
  }
  k <- sum(!vapply(x$candidates, function(r) identical(r$status, "OK"), TRUE))
  if (k) cat(sprintf("ilm_remedies() lists %d remed%s.\n", k, if (k == 1L) "y" else "ies"))
  invisible(x)
}

## the table rows of one check's candidates
ilm_clean_rows <- function(chk) {
  lapply(chk$candidates, function(r) list(
    check = r$finding, status = r$status, tier = r$tier, remedy = r$remedy, key = r$key,
    change = if (is.null(r$call)) "" else ilm_clean_deparse(r$call),
    payload = list(call = r$call, recheck = chk$recheck)))
}

ilm_clean_deparse <- function(call) paste(deparse(call, width.cutoff = 500L), collapse = " ")

#' @rdname ilm_remedies
#' @order 2
#' @export
ilm_remedies.ilm_data_check <- function(object, ...)
  ilm_remedy_assemble(ilm_clean_rows(object), object$target_id, ilm_clean_tiers, ilm_clean_note)

#' @rdname ilm_remedies
#' @order 2
#' @export
ilm_remedies.ilm_data_checks <- function(object, ...)
  ilm_remedy_assemble(unlist(lapply(object$checks, ilm_clean_rows), recursive = FALSE),
                      object$target_id, ilm_clean_tiers, ilm_clean_note)

## ---- the checks -------------------------------------------------------------

## the checks ilm_check_data() runs, by name; each later phase adds its own
ilm_clean_checks <- c(frame = "ilm_check_frame")

#' Check a data frame for problems, all at once
#'
#' Runs every data check (or those named) and gathers what they found. Each
#' check says what it saw and how sure it is, and names remedies as code;
#' list them with [ilm_remedies()] and make one with [ilm_apply_remedy()].
#' Nothing in the data changes until a remedy is applied.
#'
#' The checks so far:
#' \describe{
#'   \item{`frame`}{[ilm_check_frame()]: constant, identifier-like,
#'     duplicated, collinear, aliased, nested and redundant columns, and
#'     columns that are an exact combination of others.}
#' }
#'
#' @param data A data frame.
#' @param checks Names of the checks to run; `NULL` runs them all.
#' @param ... Passed to every check.
#' @return An object of class `"ilm_data_checks"`: `checks`, the list of each
#'   check's result, `summary`, a table of each check's status and headline,
#'   and `target_id`, the data's [ilm_data_id()].
#' @seealso [ilm_remedies()], [ilm_apply_remedy()], [ilm_cleaning_script()].
#' @examples
#' d <- data.frame(id = 1:30, x = rnorm(30), same = 1)
#' d$x2 <- d$x
#' chk <- ilm_check_data(d)
#' chk
#' ilm_remedies(chk)
#' @export
ilm_check_data <- function(data, checks = NULL, ...) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  if (is.null(checks)) checks <- names(ilm_clean_checks)
  bad <- setdiff(checks, names(ilm_clean_checks))
  if (length(bad))
    stop("unknown check(s): ", paste(bad, collapse = ", "), ". The checks are ",
         paste(names(ilm_clean_checks), collapse = ", "), ".", call. = FALSE)
  res <- lapply(checks, function(k) get(ilm_clean_checks[[k]], mode = "function")(data, ...))
  names(res) <- checks
  summary <- data.frame(check = checks,
                        status = vapply(res, `[[`, "", "status"),
                        headline = vapply(res, `[[`, "", "headline"),
                        stringsAsFactors = FALSE, row.names = NULL)
  structure(list(checks = res, summary = summary, target_id = ilm_data_id(data)),
            class = "ilm_data_checks")
}

#' @export
print.ilm_data_checks <- function(x, ...) {
  s <- x$summary
  cat(sprintf("<ilm_data_checks> %d check%s\n", nrow(s), if (nrow(s) == 1L) "" else "s"))
  for (i in seq_len(nrow(s))) {
    cat(sprintf("  %-4s %s\n", s$status[i], s$check[i]))
    cat(ilm_wrap(s$headline[i], indent = "       "), "\n", sep = "")
  }
  k <- sum(vapply(x$checks, function(c)
    sum(!vapply(c$candidates, function(r) identical(r$status, "OK"), TRUE)), 0))
  if (k) cat(sprintf("ilm_remedies() lists the remedies (%d before duplicates are merged).\n", k))
  invisible(x)
}

#' Check the data frame as a whole
#'
#' The problems [ilm_frame_issues()] finds -- constant and identifier-like
#' columns, duplicated and collinear ones, categorical columns that carry the
#' same grouping or one inside another, and columns that are an exact
#' combination of others -- as a check, with remedies that can be applied.
#'
#' A constant column, the second of a duplicated or aliased pair, and a
#' column that is an exact combination of others can be left out
#' ([ilm_drop_cols()], tier `values`). Which of two collinear or redundant
#' columns to keep, and whether an identifier stays as a key, are decisions
#' about what the data mean, so those remedies are made by hand. A nested
#' pair is expected of grouping factors and is reported as OK.
#'
#' @inheritParams ilm_frame_issues
#' @param ... Unused.
#' @return An object of class `"ilm_data_check"`; see [ilm_check_data()].
#' @examples
#' d <- data.frame(a = rnorm(20), b = rnorm(20), same = 1)
#' d$total <- d$a + d$b
#' chk <- ilm_check_frame(d)
#' chk
#' ilm_remedies(chk)
#' @export
ilm_check_frame <- function(data, cor_cut = 0.999, v_cut = 0.95, ...) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  ff <- ilm_frame_find(data, cor_cut, v_cut)
  t <- ff$table
  status <- ifelse(t$issue %in% c("nested"), "OK",
            ifelse(t$issue %in% c("id_like", "rank_unchecked", "redundant_categories",
                                  "collinear"), "WARN", "FAIL"))
  findings <- data.frame(finding = t$issue, columns = t$columns, status = status,
                         detail = t$detail, stringsAsFactors = FALSE)
  cand <- list()
  addc <- function(finding, status, tier, remedy, key, call)
    cand[[length(cand) + 1L]] <<- list(finding = finding, status = status, tier = tier,
                                       remedy = remedy, key = key, call = call)
  drop <- function(i, v, words)
    addc(t$issue[i], status[i], "values", words, ilm_remedy_key("drop_cols", v),
         call("ilm_drop_cols", quote(data), v))
  for (i in seq_len(nrow(t))) {
    v <- ff$vars[[i]]
    switch(t$issue[i],
      constant = drop(i, v, sprintf("Leave out %s: a column with one value cannot explain anything.", v)),
      id_like = addc(t$issue[i], status[i], "values", paste0(
        "Keep ", v, " to identify or join rows, and leave it out of the predictors ",
        "of any model or clustering."), ilm_remedy_key("as_key", v), NULL),
      duplicate_columns = drop(i, v[2L], sprintf("Leave out %s, an exact copy of %s.", v[2L], v[1L])),
      aliased_factors = drop(i, v[2L], sprintf(
        "Leave out %s, the same grouping as %s under other labels.", v[2L], v[1L])),
      collinear = addc(t$issue[i], status[i], "values", t$remedy[i],
                       ilm_remedy_key("keep_one", v), NULL),
      redundant_categories = addc(t$issue[i], status[i], "values", t$remedy[i],
                                  ilm_remedy_key("keep_one", v), NULL),
      rank_unchecked = addc(t$issue[i], status[i], "values", t$remedy[i],
                            ilm_remedy_key("check_subset", v), NULL),
      rank_deficient = drop(i, v[1L], if (length(v) == 1L)
        sprintf("Leave out %s, constant where the other columns are complete.", v)
        else sprintf("Leave out %s, an exact combination of %s.", v[1L], ilm_and(v[-1L]))),
      nested = NULL)
  }
  bad <- sum(status != "OK")
  headline <- if (!nrow(t)) "Nothing found: no column is constant, duplicated or a combination of others."
    else sprintf("%d finding%s about the frame as a whole%s.", nrow(t), if (nrow(t) == 1L) "" else "s",
                 if (bad < nrow(t)) sprintf(", %d of them expected (OK)", nrow(t) - bad) else "")
  ilm_data_check_new("frame", "ilm_check_frame", list(cor_cut = cor_cut, v_cut = v_cut),
                     data, findings, cand, headline)
}

## ---- remedy functions ---------------------------------------------------------

#' Leave columns out
#'
#' A remedy: returns the data without the columns named. Written into
#' cleaning scripts by [ilm_check_frame()] and the other checks, and usable
#' by hand.
#'
#' @param data A data frame.
#' @param cols Names of the columns to leave out.
#' @return The data frame without them.
#' @examples
#' ilm_drop_cols(data.frame(a = 1:3, b = 4:6), "b")
#' @export
ilm_drop_cols <- function(data, cols) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  miss <- setdiff(cols, names(data))
  if (length(miss))
    stop("no column(s) named ", paste(miss, collapse = ", "), " to leave out.", call. = FALSE)
  data[setdiff(names(data), cols)]
}

## ---- tables of one's own --------------------------------------------------------

#' @rdname ilm_remedy_table
#' @order 2
#' @export
ilm_remedy_table.data.frame <- function(object, check, status, tier, remedy, change = NULL,
                                        key, ...) {
  txt <- list(check = check, status = status, tier = tier, remedy = remedy, key = key)
  for (nm in names(txt)) {
    v <- txt[[nm]]
    if (!is.character(v) || anyNA(v) || !all(nzchar(trimws(v))))
      stop("`", nm, "` must be character, with no missing or empty entries.", call. = FALSE)
  }
  n <- length(check)
  if (any(lengths(txt) != n))
    stop("`check`, `status`, `tier`, `remedy` and `key` must have one entry per remedy.",
         call. = FALSE)
  if (any(grepl(",", check, fixed = TRUE)))
    stop("`check` names must not contain a comma.", call. = FALSE)
  bad <- setdiff(status, c("WARN", "FAIL"))
  if (length(bad))
    stop("`status` must be WARN or FAIL -- a check that is OK names no remedy -- not ",
         paste(unique(bad), collapse = ", "), ".", call. = FALSE)
  if (is.null(change)) change <- vector("list", n)
  if (!is.list(change) || length(change) != n)
    stop("`change` must be a list with one call per remedy, or NULL for one made by hand.",
         call. = FALSE)
  for (i in seq_len(n)) if (!is.null(change[[i]]) && !is.call(change[[i]]))
    stop("`change[[", i, "]]` must be a call, as quote(my_fix(data, \"x\")), or NULL.",
         call. = FALSE)
  rows <- lapply(seq_len(n), function(i) list(
    check = check[i], status = status[i], tier = tier[i], remedy = remedy[i], key = key[i],
    change = if (is.null(change[[i]])) "" else ilm_clean_deparse(change[[i]]),
    payload = list(call = change[[i]], recheck = NULL)))
  ilm_remedy_assemble(rows, ilm_data_id(object), ilm_clean_tiers, ilm_clean_note)
}

## ---- applying, the log, the script ------------------------------------------------

#' @rdname ilm_apply_remedy
#' @param reason Why the remedy is being made, in a sentence -- "the lab
#'   reports values below 0.5 as '<0.5'". Kept in the cleaning log and
#'   written into the cleaning script beside the step.
#' @param recheck Run the check that called for the remedy again on the
#'   result, and say what it finds now.
#' @order 2
#' @export
ilm_apply_remedy.data.frame <- function(object, remedies, which, reason = NULL,
                                        recheck = TRUE, ...) {
  if (!is.null(reason)) {
    if (!is.character(reason) || length(reason) != 1L || is.na(reason) || !nzchar(trimws(reason)))
      stop("`reason` must be a single sentence saying why the remedy is made, or NULL.",
           call. = FALSE)
    reason <- trimws(reason)
  }
  if (!inherits(remedies, "ilm_remedies"))
    stop("`remedies` must come from ilm_remedies() or ilm_remedy_table().", call. = FALSE)
  if (!ilm_rem_whole(remedies))
    stop("`remedies` has lost what ties it to its data, as a subset of it does. ",
         "Pass the whole table from ilm_remedies() and choose with `which`.", call. = FALSE)
  if (!identical(attr(remedies, "tiers"), ilm_clean_tiers))
    stop("`remedies` was listed for a fitted model, not for data.", call. = FALSE)
  id0 <- ilm_data_id(object)
  if (!identical(attr(remedies, "target_id"), id0))
    stop("`remedies` was listed for other data, or for these data before they were ",
         "changed or put in another order. List them again for these data with ",
         "ilm_remedies().", call. = FALSE)
  i <- ilm_remedy_find(remedies, which)
  pl <- attr(remedies, "payload")[[as.character(remedies$id[i])]]
  if (is.null(pl$call))
    stop("remedy ", remedies$key[i], " is made by hand: ", remedies$remedy[i], call. = FALSE)

  old <- attr(object, "cleaning_log")
  if (!is.null(old) && !id0 %in% old$id_after)
    warning("these data were changed since the last remedy by something other than a ",
            "remedy, so the cleaning script will not replay them. Make changes with ",
            "remedies, or start the log again from these data.", call. = FALSE)

  ## the call runs with the data as `data`, an illumex remedy found in
  ## illumex whether or not it is attached, anything else where the caller is
  env <- new.env(parent = parent.frame())
  assign("data", object, envir = env)
  fn <- pl$call[[1L]]
  ns <- asNamespace("illumex")
  if (is.name(fn) && as.character(fn) %in% getNamespaceExports(ns))
    assign(as.character(fn), get(as.character(fn), envir = ns), envir = env)
  if (identical(fn, as.name("ilm_secret")) || length(ilm_secret_vars(pl$call)))
    assign("ilm_secret", ilm_secret, envir = env)
  out <- tryCatch(eval(pl$call, env), error = function(e)
    stop("the remedy ", remedies$key[i], " failed: ", conditionMessage(e), call. = FALSE))
  if (!is.data.frame(out))
    stop("the remedy ", remedies$key[i], " did not return a data frame.", call. = FALSE)
  id1 <- ilm_data_id(out)

  log_row <- data.frame(
    step = (if (is.null(old)) 0L else nrow(old)) + 1L,
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    key = remedies$key[i], check = remedies$check[i], status = remedies$status[i],
    tier = remedies$tier[i], remedy = remedies$remedy[i], change = remedies$change[i],
    reason = if (is.null(reason)) NA_character_ else reason,
    secrets = paste(ilm_secret_vars(pl$call), collapse = ", "),
    id_before = id0, id_after = id1, stringsAsFactors = FALSE)
  attr(out, "cleaning_log") <- rbind(old, log_row)

  said <- character(0)
  if (recheck && !is.null(pl$recheck)) {
    again <- tryCatch(do.call(get(pl$recheck$fun, envir = ns),
                              c(list(out), pl$recheck$args)), error = function(e) NULL)
    if (!is.null(again)) {
      still <- vapply(again$candidates, function(r) ilm_rem_key_norm(r$key), "")
      said <- if (ilm_rem_key_norm(remedies$key[i]) %in% still)
        sprintf("  %s: %s before, and still found", remedies$check[i], remedies$status[i])
      else sprintf("  %s: %s before, not found now", remedies$check[i], remedies$status[i])
      said <- c(said, sprintf("  %s check now: %s", again$check, again$status))
    }
  } else if (recheck) {
    said <- sprintf("  %s: %s before; run its check again on the result",
                    remedies$check[i], remedies$status[i])
  }
  message("ilm_apply_remedy(): ", remedies$change[i], "\n",
          if (!is.null(reason)) paste0("  reason given: ", reason, "\n"),
          paste(said, collapse = "\n"))
  out
}

#' The cleaning log
#'
#' Every remedy applied to reach these data, in order: its key, the check
#' and status that called for it, its tier, the remedy and the change made,
#' the reason given, any secret's environment variable (never its value), and
#' the data's [ilm_data_id()] before and after.
#'
#' The log travels with the data as an attribute. Some operations drop
#' attributes (base subsetting, some joins); the script from
#' [ilm_cleaning_script()] is the durable record, so write it when the
#' cleaning is done.
#'
#' @param data A data frame that remedies were applied to.
#' @return A data frame, one row per remedy, or `NULL` with a message when
#'   there is no log.
#' @examples
#' d <- data.frame(x = rnorm(10), same = 1)
#' rem <- ilm_remedies(ilm_check_frame(d))
#' d2 <- ilm_apply_remedy(d, rem, "drop_cols/same", reason = "one value only")
#' ilm_cleaning_log(d2)
#' @export
ilm_cleaning_log <- function(data) {
  lg <- attr(data, "cleaning_log")
  if (is.null(lg)) {
    message("ilm_cleaning_log(): no remedies have been applied to these data, or an ",
            "operation since dropped the log.")
    return(invisible(NULL))
  }
  lg
}

#' Write the cleaning as an R script
#'
#' Turns the cleaning log into a script that replays every remedy, in order,
#' on the raw data: one plain call per step, with the step's key, the check
#' that called for it, its tier and the reason given as comments above it. It
#' does not run the checks again, so it replays the same way under later
#' versions of illumex; it needs only the remedy functions it calls.
#'
#' A secret is never written: the step calls `ilm_secret("VAR")`, and a
#' comment says to set that environment variable first.
#'
#' @param data The cleaned data, carrying its log.
#' @param file A file to write the script to; `NULL` returns it only.
#' @return The script's lines, invisibly when written to a file.
#' @examples
#' d <- data.frame(x = rnorm(10), same = 1)
#' rem <- ilm_remedies(ilm_check_frame(d))
#' d2 <- ilm_apply_remedy(d, rem, "drop_cols/same", reason = "one value only")
#' ilm_cleaning_script(d2)
#' @export
ilm_cleaning_script <- function(data, file = NULL) {
  lg <- attr(data, "cleaning_log")
  if (is.null(lg) || !nrow(lg))
    stop("these data carry no cleaning log: no remedies were applied, or an ",
         "operation since dropped it.", call. = FALSE)
  ver <- tryCatch(as.character(utils::packageVersion("illumex")), error = function(e) "?")
  lines <- c(
    sprintf("## Cleaning script, written by illumex %s on %s.", ver, format(Sys.Date())),
    sprintf("## It replays %d remed%s, in order, on the data they were made for,",
            nrow(lg), if (nrow(lg) == 1L) "y" else "ies"),
    sprintf("## whose ilm_data_id() is %s. Read those data into `data` first.", lg$id_before[1L]),
    "library(illumex)",
    sprintf("if (!identical(ilm_data_id(data), \"%s\"))", lg$id_before[1L]),
    "  warning(\"these are not the data the cleaning was made on\")")
  for (k in seq_len(nrow(lg))) {
    lines <- c(lines, "",
               sprintf("## [%d] %s -- %s (%s), tier %s", lg$step[k], lg$key[k], lg$check[k],
                       lg$status[k], lg$tier[k]),
               if (!is.na(lg$reason[k])) paste0("## reason: ", lg$reason[k]),
               if (nzchar(lg$secrets[k]))
                 paste0("## secret: set the environment variable(s) ", lg$secrets[k],
                        " before running this step"),
               paste0("data <- ", lg$change[k]))
  }
  lines <- c(lines, "")
  if (!is.null(file)) {
    writeLines(lines, file, useBytes = FALSE)
    return(invisible(structure(lines, class = "ilm_cleaning_script")))
  }
  structure(lines, class = "ilm_cleaning_script")
}

#' @export
print.ilm_cleaning_script <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}
