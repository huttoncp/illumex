## ---------------------------------------------------------------------------
## Remedies: the family's one system for saying what to do about a finding.
##
## A check finds something; a remedy is a change that answers it, written out
## as code so it can be made rather than retyped; applying one records what
## was done and why. illume uses this for its models and illumex for data, so
## the generics, the table and its rules live here, in the package illume
## depends on, and illume registers its model methods on them and re-exports
## them (item 102). The model rules -- which refits answer which checks, and
## what an ilm_model() argument may be -- stay in illume (item 236).
##
## A remedies table belongs to one TARGET: one fit, or one data frame, named
## by `target_id`. Tables for different targets are never combined: data are
## cleaned, then a model is fitted, then the model's remedies are read.
##
## Each table carries its own ordered TIER set, least to most consequential,
## and the closing paragraph that says what its tiers mean. Models use
## numerical, structural, estimand; data use representation, values, rows.
## The names differ on purpose: a data "values" change and a model
## "structural" change mean different things.
##
## Each remedy has a KEY, `<kind>/<target>[:qualifier]`, that names the
## change rather than the check that called for it: one change often answers
## several checks (more optimiser restarts answer two; dropping a column that
## is constant and id-like answers two), and a key built from the checks
## would change whenever a different set of them fired. Tuning numbers stay
## out of keys; `change` records what was applied. A cleaning script refers to
## its steps by key, so keys are part of the interface: each check's keys are
## pinned by a test, and a change to one is a NEWS item.
## ---------------------------------------------------------------------------

## the statuses a check can report when it names a remedy; OK names none
ilm_rem_statuses <- c("WARN", "FAIL", "BOUNDARY", "INCONCLUSIVE")

#' Remedies for what a check found
#'
#' Lists the remedies for every finding of a check that is not `"OK"`, each
#' written out as the change that makes it, so that it can be applied with
#' [ilm_apply_remedy()] rather than retyped. A generic: illumex gives it
#' methods for its data checks ([ilm_check_data()] and the `ilm_check_*()`
#' functions), and illume for fitted models.
#'
#' Every remedy has a `tier`, which says what applying it would change, from
#' the least to the most consequential. For data:
#' \describe{
#'   \item{`representation`}{The same values, read correctly: text parsed
#'     as numbers, a header set, leading zeros restored.}
#'   \item{`values`}{Values changed or set missing, or columns removed:
#'     a unit converted, a sentinel set to `NA`, levels merged.}
#'   \item{`rows`}{Rows dropped or excluded.}
#' }
#' Some remedies can only be made by hand -- which of two columns to keep is
#' a question about what they mean -- and those have no `change`. A remedy is
#' a candidate, not a cure: apply it, and read the check again.
#'
#' Each remedy also has a `key` that names the change (`drop_cols/id`,
#' `convert/temp:degF>degC`) and stays the same from one version of the
#' package to the next, so a cleaning script can name the remedy it applied.
#'
#' @param object A result whose package gives `ilm_remedies()` a method: an
#'   [ilm_check_data()] result or one `ilm_check_*()` result.
#' @param ... Arguments for methods.
#' @return A data frame of class `"ilm_remedies"`, one row per remedy, with
#'   `id`, `key`, the `check` (or checks) it answers and its `status`, the
#'   `tier`, the `remedy` in words, and the `change` that makes it, as code,
#'   or `""` when it is made by hand. An empty table when every check is OK.
#' @seealso [ilm_apply_remedy()] to make one, [ilm_remedy_table()] to build a
#'   table of one's own.
#' @examples
#' d <- data.frame(x = rnorm(20), same = 1, y = rnorm(20))
#' ilm_remedies(ilm_check_frame(d))
#' @order 1
#' @export
ilm_remedies <- function(object, ...) UseMethod("ilm_remedies")

#' @export
ilm_remedies.default <- function(object, ...)
  stop("there are no remedies for an object of class ", class(object)[1],
       ". Remedies come from a check: ilm_check_data() for data, or a ",
       "package whose results give ilm_remedies() a method.", call. = FALSE)

#' Make one remedy
#'
#' Applies one remedy from an [ilm_remedies()] table to the target the table
#' was listed for. A generic: illumex applies data remedies (the method for
#' data frames returns the cleaned data with a cleaning log), and illume
#' refits models.
#'
#' @param object What the remedy is applied to: the data frame, or the
#'   fitted model, the table was listed for.
#' @param remedies The whole table from [ilm_remedies()] the remedy was
#'   chosen from. Required rather than recomputed, so the remedy made is
#'   always the one that was read.
#' @param which The remedy: its `id`, or its `key`. Spaces in a key are
#'   ignored, so `"drop/(1|g)"` finds `"drop/(1 | g)"`.
#' @param ... Arguments for methods.
#' @return For a data frame, the data with the remedy made, carrying the
#'   cleaning log; see [ilm_cleaning_log()].
#' @seealso [ilm_remedies()], [ilm_cleaning_script()].
#' @order 1
#' @export
ilm_apply_remedy <- function(object, remedies, which, ...) UseMethod("ilm_apply_remedy")

#' @export
ilm_apply_remedy.default <- function(object, remedies, which, ...)
  stop("a remedy cannot be applied to an object of class ", class(object)[1],
       ": remedies are applied to the data frame, or the fitted model, ",
       "their table was listed for.", call. = FALSE)

#' A table of remedies of one's own
#'
#' For remedies that come from somewhere other than illumex's checks -- a
#' rule of one's own, another package's diagnostics -- so that they are
#' listed, applied and logged as illumex's own are. A generic: the method for
#' a data frame is here, and illume has one for fitted models.
#'
#' `c()` combines tables for the same target into one, numbered afresh: a
#' change several tables name (the same key) is listed once, with every check
#' that named it, and where two tables put it in different tiers it takes the
#' more cautious. Tables for different targets are not combined.
#'
#' @param object The target: the data frame the remedies would change.
#' @param check Character: the name of the check each remedy answers.
#' @param status Character: what the check found, `"WARN"` or `"FAIL"`.
#' @param tier Character: `"representation"`, `"values"` or `"rows"`, as
#'   described in [ilm_remedies()].
#' @param remedy Character: the remedy in words, a sentence a person can act
#'   on.
#' @param change A list with one element per remedy: a call that makes it,
#'   with the data written as `data` (`quote(my_fix(data, "x"))`), or `NULL`
#'   for a remedy made by hand.
#' @param key Character: each remedy's key, `<kind>/<columns>`; see
#'   [ilm_remedy_key()].
#' @param ... For `c()`: remedy tables for the same target.
#' @return A data frame of class `"ilm_remedies"`, as from [ilm_remedies()].
#' @examples
#' d <- data.frame(id = 1:5, temp = c(20, 21, 70, 22, 68))
#' rem <- ilm_remedy_table(d, check = "my_range", status = "WARN",
#'                         tier = "values", key = "set_missing/temp",
#'                         remedy = "Set temperatures above 50 to missing.",
#'                         change = list(quote(within(data, temp[temp > 50] <- NA))))
#' rem
#' ilm_apply_remedy(d, rem, "set_missing/temp", reason = "out of range for the site")
#' @order 1
#' @export
ilm_remedy_table <- function(object, ...) UseMethod("ilm_remedy_table")

## ---- keys -----------------------------------------------------------------

#' Build a remedy key
#'
#' The one way keys are written, so that the same change always gets the same
#' key: `<kind>/<target>`, with `:qualifier` only where the qualifier is part
#' of what the change is (`convert/temp:degF>degC`), never a tuning number.
#' Several columns are sorted and joined by commas; a model term is written
#' as its formula label.
#'
#' @param kind The change's verb: `"drop_cols"`, `"set_missing"`, ...
#' @param target Character: the columns, or a term (a formula, or its label).
#' @param qualifier Optional: what distinguishes two changes of the same kind
#'   to the same target.
#' @return A single string.
#' @examples
#' ilm_remedy_key("drop_cols", c("b", "a"))
#' ilm_remedy_key("convert", "temp", "degF>degC")
#' ilm_remedy_key("drop", ~ (1 | g))
#' @export
ilm_remedy_key <- function(kind, target, qualifier = NULL) {
  if (!is.character(kind) || length(kind) != 1L || !nzchar(kind) || grepl("/", kind, fixed = TRUE))
    stop("`kind` must be one word with no slash.", call. = FALSE)
  if (inherits(target, "formula"))
    target <- paste(deparse(target[[length(target)]], width.cutoff = 500L), collapse = " ")
  target <- if (length(target) > 1L) paste(sort(as.character(target)), collapse = ",")
            else as.character(target)
  key <- paste0(kind, "/", target)
  if (!is.null(qualifier)) key <- paste0(key, ":", qualifier)
  key
}

## a key as it is compared: with every space removed
ilm_rem_key_norm <- function(k) gsub("[[:space:]]+", "", k)

#' Find a remedy in a table by id or key
#'
#' The lookup every [ilm_apply_remedy()] method uses, so that a remedy is
#' found the same way whatever it is applied to. Spaces are ignored on both
#' sides of a key.
#'
#' @param remedies An `"ilm_remedies"` table.
#' @param which An `id` or a `key`.
#' @return The table's row number.
#' @keywords internal
#' @export
ilm_remedy_find <- function(remedies, which) {
  if (!inherits(remedies, "ilm_remedies"))
    stop("`remedies` must come from ilm_remedies() or ilm_remedy_table().", call. = FALSE)
  if (!nrow(remedies))
    stop("there is nothing to remedy: every check is OK.", call. = FALSE)
  if (length(which) != 1L || is.na(which))
    stop("`which` must be one remedy: its id or its key.", call. = FALSE)
  i <- if (is.numeric(which)) match(which, remedies$id)
       else match(ilm_rem_key_norm(which), ilm_rem_key_norm(remedies$key))
  if (is.na(i))
    stop("`which` must be one of the remedy ids (",
         paste(remedies$id, collapse = ", "), ") or keys (",
         paste(remedies$key, collapse = ", "), "), not ", which, ".", call. = FALSE)
  i
}

## ---- the table ------------------------------------------------------------

#' Assemble a remedies table
#'
#' The table's one constructor, for packages that build remedies of their
#' own kind of target (illume's models). Rows answering the same key are
#' listed once, with their checks joined; where they put the change in
#' different tiers the more cautious is taken; the table is ordered by tier
#' and numbered.
#'
#' @param rows A list of remedies, each a list with `check`, `status`,
#'   `tier`, `remedy`, `key`, `change` (the code as text, `""` by hand) and
#'   `payload` (whatever the package's apply method needs, kept by id).
#' @param target_id What the table belongs to.
#' @param tiers The ordered tier set.
#' @param tier_note The paragraph print() closes with, saying how to apply
#'   one and what the tiers mean.
#' @param by_hand What print() says of a remedy with no change.
#' @return An `"ilm_remedies"` table.
#' @keywords internal
#' @export
ilm_remedy_assemble <- function(rows, target_id, tiers, tier_note,
                                by_hand = "by hand: a decision about what the data mean") {
  fields <- c("check", "status", "tier", "remedy", "key", "change")
  col <- function(f) vapply(rows, function(r) as.character(r[[f]]), "")
  if (length(rows)) {
    tab <- as.data.frame(stats::setNames(lapply(fields, col), fields), stringsAsFactors = FALSE)
    bad <- setdiff(tab$tier, tiers)
    if (length(bad))
      stop("tier ", paste(unique(bad), collapse = ", "), " is not one of ",
           paste(tiers, collapse = ", "), ".", call. = FALSE)
    bad <- setdiff(unlist(strsplit(tab$status, ", ", fixed = TRUE)), ilm_rem_statuses)
    if (length(bad))
      stop("status ", paste(unique(bad), collapse = ", "), " is not one of ",
           paste(ilm_rem_statuses, collapse = ", "), ".", call. = FALSE)
    payload <- lapply(rows, `[[`, "payload")
    kn <- ilm_rem_key_norm(tab$key)
    first <- !duplicated(kn)
    for (k in which(first)) {
      same <- which(kn == kn[k])
      ## one key, two different changes: a key that does not name its change
      if (length(unique(tab$change[same])) > 1L)
        stop("internal error: key ", tab$key[k], " names two different changes (",
             paste(unique(tab$change[same]), collapse = "; "), ").", call. = FALSE)
      ck <- unlist(strsplit(tab$check[same], ", ", fixed = TRUE))
      st <- unlist(strsplit(tab$status[same], ", ", fixed = TRUE))
      keep <- !duplicated(ck)
      tab$check[k] <- paste(ck[keep], collapse = ", ")
      tab$status[k] <- paste(st[keep], collapse = ", ")
      tab$tier[k] <- tiers[max(match(tab$tier[same], tiers))]
    }
    tab <- tab[first, , drop = FALSE]; payload <- payload[first]
    o <- order(match(tab$tier, tiers), seq_len(nrow(tab)))
    tab <- tab[o, , drop = FALSE]; payload <- payload[o]
  } else {
    tab <- as.data.frame(stats::setNames(rep(list(character(0)), length(fields)), fields),
                         stringsAsFactors = FALSE)
    payload <- list()
  }
  tab <- cbind(id = seq_len(nrow(tab)), tab[c("key", "check", "status", "tier", "remedy", "change")])
  rownames(tab) <- NULL
  ## keyed by id, not position: a subset of the rows keeps the attribute
  ## whole, and must not hand row 3's remedy the payload of row 1
  names(payload) <- as.character(tab$id)
  structure(tab, class = c("ilm_remedies", "data.frame"), payload = payload,
            target_id = target_id, tiers = tiers, tier_note = tier_note,
            by_hand = by_hand)
}

## what makes a table whole: the attributes a subset of it loses
ilm_rem_whole <- function(t)
  !is.null(attr(t, "payload")) && !is.null(attr(t, "target_id")) &&
    !is.null(attr(t, "tiers")) && all(c("id", "key", "change") %in% names(t))

#' @rdname ilm_remedy_table
#' @order 3
#' @export
c.ilm_remedies <- function(...) {
  tabs <- list(...)
  tabs <- tabs[!vapply(tabs, is.null, TRUE)]
  if (!all(vapply(tabs, inherits, TRUE, "ilm_remedies")))
    stop("only remedy tables, from ilm_remedies() or ilm_remedy_table(), ",
         "can be combined.", call. = FALSE)
  if (!all(vapply(tabs, ilm_rem_whole, TRUE)))
    stop("a remedy table has lost what ties it to its target, as a subset of ",
         "it does. Combine the whole tables.", call. = FALSE)
  ids <- lapply(tabs, attr, "target_id")
  if (length(unique(ids)) > 1L)
    stop("these remedy tables were listed for different targets -- different ",
         "data, or different fits -- and only one target's remedies can be ",
         "combined.", call. = FALSE)
  rows <- list()
  for (t in tabs) {
    p <- attr(t, "payload")
    for (i in seq_len(nrow(t)))
      rows[[length(rows) + 1L]] <- list(
        check = t$check[i], status = t$status[i], tier = t$tier[i], remedy = t$remedy[i],
        key = t$key[i], change = t$change[i], payload = p[[as.character(t$id[i])]])
  }
  t1 <- tabs[[1L]]
  ilm_remedy_assemble(rows, ids[[1L]], attr(t1, "tiers"), attr(t1, "tier_note"),
                      attr(t1, "by_hand") %||% "by hand")
}

#' @rdname ilm_remedies
#' @param x An `"ilm_remedies"` table.
#' @order 3
#' @export
print.ilm_remedies <- function(x, ...) {
  ## a subset of the columns keeps the class but not what this layout reads;
  ## it is a plain table, so print it as one
  if (!all(c("id", "key", "check", "status", "tier", "remedy", "change") %in% names(x)))
    return(NextMethod())
  if (!nrow(x)) {
    cat("No remedies: every check is OK.\n")
    return(invisible(x))
  }
  cat(sprintf("%d remed%s\n\n", nrow(x), if (nrow(x) == 1L) "y" else "ies"))
  for (i in seq_len(nrow(x))) {
    cat(sprintf("[%d] %s -- %s (%s)   key: %s\n", x$id[i], x$tier[i], x$check[i],
                x$status[i], x$key[i]))
    cat(ilm_wrap(x$remedy[i], indent = "    "), "\n", sep = "")
    cat(if (nzchar(x$change[i])) paste0("    change: ", x$change[i])
        else paste0("    ", attr(x, "by_hand") %||% "by hand"), "\n\n", sep = "")
  }
  note <- attr(x, "tier_note")
  if (!is.null(note)) cat(ilm_wrap(note), "\n", sep = "")
  invisible(x)
}
