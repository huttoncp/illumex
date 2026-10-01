## ---------------------------------------------------------------------------
## Counts, duplicate detection, cleaning and recoding.
##
## Built on collapse primitives (fcount, group) and base R.
## ---------------------------------------------------------------------------

## ---- counts ----------------------------------------------------------------

#' Frequency counts of a vector's unique values
#'
#' Errors tend to show up at the two ends of a count: a typo as a very rare
#' value, a default or missing-value code as a very common one.
#' [ilm_counts_tb()] shows both ends side by side.
#'
#' @param y A vector.
#' @param n Number of rows to return, or `"all"`.
#' @param order `"d"` by count descending, `"a"` by count ascending, or `"i"`
#'   by the value itself (factor level order, not label order).
#' @param na.rm Drop missing values before counting.
#' @return A data frame with `value` and `n`.
#' @seealso [ilm_counts_tb()] for both ends at once, [ilm_counts_all()] for a
#'   whole data frame.
#' @examples
#' d <- ilm_sim()
#' ilm_counts(d$grp)
#' ilm_counts(d$site, n = 3)
#' @export
ilm_counts <- function(y, n = "all", order = c("d", "a", "i"), na.rm = TRUE) {
  order <- match.arg(order)
  if (is.data.frame(y))
    stop("`ilm_counts()` summarises a vector; use `ilm_counts_all()` for a data frame",
         call. = FALSE)
  keep <- if (na.rm) !is.na(y) else rep(TRUE, length(y))
  yv <- y[keep]
  tb <- fcount(yv)                       # columns: x, N
  names(tb) <- c("value", "n")
  ## "i" sorts by the value itself, which for a factor means level order, not
  ## alphabetical order of the labels
  idx <- switch(order,
                d = order(-tb$n, tb$value),
                a = order(tb$n, tb$value),
                i = order(tb$value))
  tb <- tb[idx, , drop = FALSE]
  ## `n` was documented as the number of rows to return but had no effect in
  ## the previous implementation; it is honoured here
  if (!identical(n, "all")) {
    n <- as.integer(n)
    if (is.na(n) || n < 1L)
      stop("`n` must be \"all\" or a positive whole number", call. = FALSE)
    tb <- utils::head(tb, n)
  }
  rownames(tb) <- NULL
  tb
}

## top and bottom of the frequency distribution side by side: the two ends are
## where the problems live (a dominant level, and levels too thin to model)
#' The most and least frequent values, side by side
#'
#' The two ends of a frequency distribution are where the problems live: a
#' level that dominates, and levels too thin to estimate.
#'
#' @inheritParams ilm_counts
#' @param n How many values from each end.
#' @return A data frame with `top_value`, `top_n`, `bot_value` and `bot_n`.
#' @examples
#' ilm_counts_tb(ilm_sim()$grp, n = 2)
#' @export
ilm_counts_tb <- function(y, n = 10L, na.rm = TRUE) {
  full <- ilm_counts(y, n = "all", order = "d", na.rm = na.rm)
  k <- min(as.integer(n), nrow(full))
  if (k < 1L) return(data.frame(top_value = character(0), top_n = integer(0),
                                bot_value = character(0), bot_n = integer(0)))
  top <- full[seq_len(k), , drop = FALSE]
  bot <- full[rev(seq(nrow(full) - k + 1L, nrow(full))), , drop = FALSE]
  data.frame(top_value = top$value, top_n = top$n,
             bot_value = bot$value, bot_n = bot$n,
             stringsAsFactors = FALSE, row.names = NULL)
}

## values are stacked across variables of different classes, so they are
## rendered as character; the counts stay integer
#' Frequency counts for every column
#'
#' Values are stacked across columns of different classes, so they are rendered
#' as character; the counts stay integer.
#'
#' @inheritParams ilm_counts
#' @param data A data frame.
#' @param by Optional grouping columns.
#' @return A data frame with `variable`, `value` and `n`.
#' @examples
#' ilm_counts_all(ilm_sim()[, c("grp", "site")], n = 2)
#' @export
ilm_counts_all <- function(data, by = NULL, n = "all", order = c("d", "a", "i"),
                           na.rm = TRUE) {
  order <- match.arg(order)
  miss <- setdiff(by, names(data))
  if (length(miss))
    stop("`by` variable(s) not found in the data: ", paste(miss, collapse = ", "),
         call. = FALSE)
  cols <- setdiff(names(data), by)
  one <- function(d, cols) do.call(rbind, lapply(cols, function(cn) {
    tb <- ilm_counts(d[[cn]], n = n, order = order, na.rm = na.rm)
    data.frame(variable = cn, value = as.character(tb$value), n = tb$n,
               stringsAsFactors = FALSE)
  }))
  if (is.null(by)) return(one(data, cols))
  g <- interaction(data[by], drop = TRUE)
  parts <- lapply(split(seq_len(nrow(data)), g),
                  function(i) one(data[i, , drop = FALSE], cols))
  res <- do.call(rbind, parts)
  cbind(setNames(data.frame(rep(names(parts), vapply(parts, nrow, 1L)),
                            stringsAsFactors = FALSE), paste(by, collapse = ".")),
        res, row.names = NULL)
}

#' Most and least frequent values for every column
#'
#' @inheritParams ilm_counts_tb
#' @param data A data frame.
#' @return A data frame with `variable` and the top/bottom columns.
#' @examples
#' ilm_counts_tb_all(ilm_sim()[, c("grp", "site")], n = 2)
#' @export
ilm_counts_tb_all <- function(data, n = 10L, na.rm = TRUE) {
  do.call(rbind, lapply(names(data), function(cn) {
    tb <- ilm_counts_tb(data[[cn]], n = n, na.rm = na.rm)
    if (!nrow(tb)) return(NULL)
    ## assigned by name rather than with transform(), whose non-standard
    ## evaluation reads as an undefined global to R CMD check
    tb$top_value <- as.character(tb$top_value)
    tb$bot_value <- as.character(tb$bot_value)
    cbind(variable = cn, tb, stringsAsFactors = FALSE, row.names = NULL)
  }))
}

## ---- copies / dupes --------------------------------------------------------

#' Find copied or duplicated rows
#'
#' Name the columns that should identify one row, such as an id and a date,
#' and run it before and after a join: a join that went wrong shows up as
#' copies. `ilm_copies(data, filter = "first")` keeps one of each.
#'
#' @param data A data frame.
#' @param ... Columns defining a duplicate. All columns if none are given.
#' @param filter `"all"` appends `copy_number` and `n_copies`; `"dupes"` keeps
#'   only repeated rows; `"first"`, `"last"` and `"unique"` filter rows.
#' @param na_last Sort missing values last.
#' @param sort_by Sort the result by the key columns.
#' @return A data frame; the columns depend on `filter`. With `"all"` or
#'   `"dupes"`, a message says how many rows are copies and gives the call
#'   that keeps one of each; [suppressMessages()] silences it.
#' @seealso [ilm_dupes()] for the common case.
#' @examples
#' d <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
#' ilm_copies(d)
#' ilm_copies(d, filter = "unique")
#' @export
ilm_copies <- function(data, ..., filter = c("all", "dupes", "first", "last", "unique"),
                       na_last = TRUE, sort_by = TRUE) {
  filter <- match.arg(filter)
  ilm_copies_run(data, as.character(unlist(list(...))), filter, na_last, sort_by,
                 ilm_copies_arg_name(substitute(data)))
}

## ilm_copies() and ilm_dupes() share this, so both say the same thing when
## they find copies (ilm_copies_note()) and both name the data as the caller
## wrote it
#' @keywords internal
#' @noRd
ilm_copies_run <- function(data, vars, filter, na_last, sort_by, data_name) {
  keyed <- length(vars) > 0L
  if (!length(vars)) vars <- names(data)
  miss <- setdiff(vars, names(data))
  if (length(miss))
    stop("column(s) not found in the data: ", paste(miss, collapse = ", "),
         call. = FALSE)

  key <- data[vars]
  g <- group(key)                       # integer group id per row
  sizes <- tabulate(g, attr(g, "N.groups"))
  n_copies <- sizes[g]
  ## position within the group, in row order
  copy_number <- ilm_ave_seq(g)
  if (filter %in% c("all", "dupes"))
    ilm_copies_note(sum(n_copies > 1L), length(g), attr(g, "N.groups"),
                    data_name, if (keyed) vars)

  out <- switch(filter,
    all    = cbind(data, copy_number = copy_number, n_copies = n_copies),
    dupes  = cbind(data, n_copies = n_copies)[n_copies > 1L, , drop = FALSE],
    first  = data[copy_number == 1L, , drop = FALSE],
    last   = data[copy_number == n_copies, , drop = FALSE],
    unique = data[n_copies == 1L, , drop = FALSE])

  if (sort_by && filter %in% c("all", "dupes")) {
    ord <- do.call(order, c(unname(as.list(out[vars])),
                            list(na.last = na_last)))
    out <- out[ord, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

## sequence position within each group, without a split-apply round trip
#' @keywords internal
#' @noRd
ilm_ave_seq <- function(g) {
  o <- order(g)
  r <- integer(length(g))
  gs <- g[o]
  r[o] <- sequence(tabulate(gs, attr(g, "N.groups")))
  r
}

#' Duplicated rows only
#'
#' A shorthand for `ilm_copies(data, ..., filter = "dupes")`.
#'
#' @inheritParams ilm_copies
#' @return A data frame of repeated rows with `n_copies` appended, and the
#'   same message as [ilm_copies()] when there are any.
#' @examples
#' ilm_dupes(data.frame(a = c(1, 1, 2), b = c("x", "x", "y")))
#' @export
ilm_dupes <- function(data, ..., na_last = TRUE) {
  ilm_copies_run(data, as.character(unlist(list(...))), "dupes", na_last, TRUE,
                 ilm_copies_arg_name(substitute(data)))
}

## The data as the caller named it, for a remedy that can be run as written;
## "data" when it was an expression rather than a name
#' @keywords internal
#' @noRd
ilm_copies_arg_name <- function(expr) if (is.name(expr)) as.character(expr) else "data"

## The one line ilm_copies() and ilm_dupes() give when rows are copies, with
## the call that keeps one of each (Craig's item 206). Nothing when there
## are none.
#' @keywords internal
#' @noRd
ilm_copies_note <- function(n_copied, n_rows, n_distinct, data_name = "data",
                            vars = NULL) {
  if (!n_copied) return(invisible(NULL))
  f <- function(n) format(n, big.mark = ",", scientific = FALSE)
  what <- if (is.null(vars)) "their values" else
    paste("their values of", ilm_and(vars))
  keys <- if (is.null(vars)) "" else if (length(vars) == 1L)
    sprintf(", \"%s\"", vars) else
    sprintf(", c(%s)", paste0("\"", vars, "\"", collapse = ", "))
  message(sprintf("%s of %s rows share %s with another row; ilm_copies(%s%s, filter = \"first\") keeps one of each, leaving %s.",
                  f(n_copied), f(n_rows), what, data_name, keys, f(n_distinct)))
}

## ---- wash_df ---------------------------------------------------------------

## snake_case as janitor's make_clean_names() does it, in base R, with no
## letter dropped silently (Craig's item 204). Quotes go; "%" and "#" become
## "percent" and "number"; camelCase and acronyms split (someFlag, HTMLParser);
## letters are lower-cased and accented Latin letters made plain by the table
## below (e with an accent -> e, sharp s -> ss, oe ligature -> oe); anything
## else that is not a letter, mark or digit becomes one underscore. Letters
## the table does not hold -- other scripts, rarer Latin ones -- are kept as
## they are, where janitor transliterates them. A name left empty is "x", one
## starting with a digit gains an "x", and a repeated name is numbered as
## janitor numbers it: the second "_2", the third "_3".
#' @keywords internal
#' @noRd
ilm_snake <- function(x) {
  ## a name that is not valid UTF-8 keeps its stray bytes as e9, never lost
  x <- ilm_show_text(enc2utf8(as.character(x)))
  x[is.na(x)] <- "NA"
  x <- gsub("['\"]", "", x)
  x <- gsub("%", "_percent_", x, fixed = TRUE)
  x <- gsub("#", "_number_", x, fixed = TRUE)
  x <- gsub("(\\p{Ll})(\\p{Lu})", "\\1_\\2", x, perl = TRUE)
  x <- gsub("(\\p{Lu})(\\p{Lu}\\p{Ll})", "\\1_\\2", x, perl = TRUE)
  x <- ilm_latin_plain(tolower(x))
  ## an accent typed as a separate mark after a plain letter
  x <- gsub("(?<=[a-z])\\p{Mn}+", "", x, perl = TRUE)
  x <- gsub("[^\\p{L}\\p{M}\\p{N}]+", "_", x, perl = TRUE)
  x <- gsub("^_+|_+$", "", x)
  x[!nzchar(x)] <- "x"
  x <- sub("^([0-9])", "x\\1", x)
  ilm_number_repeats(x)
}

## Lower-case Latin letters with a diacritic, and ligatures, with the plain
## letters they become: Latin-1 Supplement and Latin Extended-A in full, plus
## Romanian s and t with a comma below. Written as escapes so the source stays
## ASCII.
ILM_LATIN_PLAIN <- c(
  a = "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105",
  ae = "\u00e6",
  c = "\u00e7\u0107\u0109\u010b\u010d",
  d = "\u00f0\u010f\u0111",
  e = "\u00e8\u00e9\u00ea\u00eb\u0113\u0115\u0117\u0119\u011b",
  g = "\u011d\u011f\u0121\u0123",
  h = "\u0125\u0127",
  i = "\u00ec\u00ed\u00ee\u00ef\u0129\u012b\u012d\u012f\u0131",
  ij = "\u0133",
  j = "\u0135",
  k = "\u0137",
  l = "\u013a\u013c\u013e\u0140\u0142",
  n = "\u00f1\u0144\u0146\u0148\u0149",
  ng = "\u014b",
  o = "\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u014f\u0151",
  oe = "\u0153",
  q = "\u0138",
  r = "\u0155\u0157\u0159",
  s = "\u0219\u015b\u015d\u015f\u0161\u017f",
  ss = "\u00df",
  t = "\u021b\u0163\u0165\u0167",
  th = "\u00fe",
  u = "\u00f9\u00fa\u00fb\u00fc\u0169\u016b\u016d\u016f\u0171\u0173",
  w = "\u0175",
  y = "\u00fd\u00ff\u0177",
  z = "\u017a\u017c\u017e"
)

#' @keywords internal
#' @noRd
ilm_latin_plain <- function(x) {
  for (to in names(ILM_LATIN_PLAIN))
    x <- gsub(paste0("[", ILM_LATIN_PLAIN[[to]], "]"), to, x, perl = TRUE)
  x
}

## janitor's numbering of repeats: the second of a name gains "_2", the third
## "_3"; a name that then collides with one already there is numbered again
## (a, a, a_2 -> a, a_2, a_2_2)
#' @keywords internal
#' @noRd
ilm_number_repeats <- function(x) {
  repeat {
    dup <- duplicated(x)
    if (!any(dup)) return(x)
    g <- match(x, unique(x))
    k <- integer(length(x))
    k[order(g)] <- sequence(tabulate(g))
    x[dup] <- paste0(x[dup], "_", k[dup])
  }
}

## A column read as text may still be numeric or logical. Convert only when
## EVERY non-missing value converts cleanly, so a single stray "n/a" cannot
## silently turn a column into NAs.
#' @keywords internal
#' @noRd
ilm_retype <- function(v) {
  if (!is.character(v)) return(v)
  s <- ilm_trimws(v)
  s[!nzchar(s)] <- NA_character_
  obs <- s[!is.na(s)]
  if (!length(obs)) return(s)
  ## text that is not valid UTF-8 is neither a number nor TRUE/FALSE, and
  ## tolower() would stop on it: the column stays text
  if (!all(validUTF8(obs))) return(s)
  lu <- tolower(obs)
  if (all(lu %in% c("true", "false", "t", "f"))) {
    r <- rep(NA, length(s))
    r[!is.na(s)] <- tolower(trimws(s[!is.na(s)])) %in% c("true", "t")
    return(r)
  }
  num <- suppressWarnings(as.numeric(obs))
  if (!anyNA(num)) {
    r <- rep(NA_real_, length(s))
    r[!is.na(s)] <- num
    if (all(abs(num - round(num)) < 1e-8) && all(abs(num) < .Machine$integer.max))
      return(as.integer(r))
    return(r)
  }
  s
}

#' Clean up a messy data frame
#'
#' Removes empty rows and columns, standardises names to snake_case, and
#' converts text columns that are really numeric or logical.
#'
#' Conversion happens only when every non-missing value converts cleanly, so a
#' single stray `"n/a"` cannot silently turn a column into `NA`s. A text column
#' of whole numbers becomes integer; the values are unchanged. Columns that
#' are already numbers, and factors, are left as they are.
#'
#' Text that is not valid UTF-8 -- what a file saved in another encoding,
#' such as Windows-1252, gives when read as UTF-8 -- is left as it is, its
#' bytes unchanged: no encoding is guessed. A warning names each column
#' holding such text, with its count and first rows, and says how to read
#' the file in its own encoding; the result keeps the same table as
#' `attr(x, "not_utf8")`. A column name that is not valid UTF-8 is cleaned
#' with each stray byte written out (`caf_e9`).
#'
#' Given the file's encoding, `encoding = "windows-1252"` say, the text that
#' is not valid UTF-8 -- values, factor levels and column names -- is
#' converted from it, and a message says how many values were converted in
#' each column. Nothing that is already valid UTF-8 is touched, and no
#' encoding is ever guessed. A value that does not convert, or does not
#' convert back to the same bytes, is left as it is and reported as above.
#'
#' Names follow the rules of janitor's `make_clean_names()`, without needing
#' janitor, and no letter is dropped: `"%"` becomes `percent` and `"#"`
#' `number`; camelCase is split; accented Latin letters become plain ones (an
#' e with an accent becomes e, a sharp s ss, the oe ligature oe); letters of
#' other scripts are kept as they are; a name starting with a digit gains an
#' `x`; and a repeated name gains `_2`, `_3` and so on, as janitor numbers it.
#'
#' @param data A data frame.
#' @param clean_names Standardise column names to snake_case.
#' @param retype Convert character columns that are really numeric or logical.
#' @param drop_empty Drop rows and columns that are entirely missing or blank.
#' @param column_to_rownames Use a column's values as row names.
#' @param names_col The column to use when `column_to_rownames = TRUE`.
#' @param encoding The encoding the file was saved in, such as
#'   `"windows-1252"` (or `"latin1"`), to convert the text that is not valid
#'   UTF-8 from. `NULL`, the default, converts nothing. [iconvlist()] lists
#'   the encodings this system knows.
#' @return A cleaned data frame, of the input's class for a tibble or a
#'   data.table (a plain data.frame when `column_to_rownames = TRUE`, since
#'   neither keeps row names) and a plain data.frame for any other. When
#'   some text is not valid UTF-8, `attr(x, "not_utf8")` is a data frame of
#'   the columns holding it (by their cleaned names): `column`, `n`, and the
#'   first `rows` of `data` where it is. With `encoding`, `attr(x,
#'   "converted")` gives the values converted per column (`column`, `n`).
#' @examples
#' m <- data.frame("Col One" = c("1", "2", ""), someFlag = c("TRUE", "FALSE", ""),
#'                 check.names = FALSE)
#' ilm_wash_df(m)
#' @export
ilm_wash_df <- function(data, clean_names = TRUE, retype = TRUE,
                        drop_empty = TRUE, column_to_rownames = FALSE,
                        names_col = NULL, encoding = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame", call. = FALSE)
  d <- as.data.frame(data, stringsAsFactors = FALSE)

  if (column_to_rownames) {
    if (is.null(names_col) || !names_col %in% names(d))
      stop("`names_col` must name a column of `data` when `column_to_rownames = TRUE`",
           call. = FALSE)
    rn <- as.character(d[[names_col]])
    d[[names_col]] <- NULL
  } else rn <- NULL

  ## the text that is not valid UTF-8, converted from the encoding given
  ## (Craig's item 298); never guessed
  conv <- NULL
  if (!is.null(encoding)) {
    conv <- ilm_wash_convert(d, encoding)
    d <- conv$data
    if (!is.null(rn)) rn <- ilm_utf8_from(rn, encoding)$x
  }

  ## text that is not valid UTF-8, found before anything is dropped, so
  ## the rows named are the rows of `data`
  bad <- ilm_not_utf8_cols(d)
  bad_names <- names(d)[!validUTF8(names(d))]

  ## a factor is judged by its levels, each once
  blank <- function(v) {
    if (is.factor(v)) return(is.na(v) | (!nzchar(ilm_trimws(levels(v))))[as.integer(v)] %in% TRUE)
    if (is.character(v)) return(is.na(v) | !nzchar(ilm_trimws(v)))
    is.na(v)
  }
  if (drop_empty && nrow(d) && ncol(d)) {
    ## a row or column is empty when every cell is missing or whitespace
    b <- lapply(d, blank)
    keep_col <- !vapply(b, all, TRUE)
    d <- d[, keep_col, drop = FALSE]
    ## with no column left, every row was empty
    keep_row <- if (ncol(d)) !Reduce(`&`, b[keep_col]) else logical(nrow(d))
    d <- d[keep_row, , drop = FALSE]
    if (!is.null(rn)) rn <- rn[keep_row]
  }
  if (retype && ncol(d)) d[] <- lapply(d, ilm_retype)
  old_names <- names(d)
  if (clean_names && ncol(d)) names(d) <- ilm_snake(names(d))
  if (!is.null(rn)) rownames(d) <- make.unique(rn) else rownames(d) <- NULL
  out <- ilm_wash_class(d, data, has_rownames = !is.null(rn))
  ilm_wash_utf8(out, bad, bad_names, old_names, names(d), conv, encoding)
}

## A tibble comes back a tibble and a data.table a data.table: the input's
## class means its package is installed, so no dependency is added. Neither
## holds row names, so with column_to_rownames the result is a data.frame.
## Any other data frame subclass comes back a plain data.frame.
#' @keywords internal
#' @noRd
ilm_wash_class <- function(d, data, has_rownames = FALSE) {
  if (has_rownames) return(d)
  if (inherits(data, "data.table") &&
      requireNamespace("data.table", quietly = TRUE))
    return(data.table::as.data.table(d))
  if (inherits(data, "tbl_df") && requireNamespace("tibble", quietly = TRUE))
    return(tibble::as_tibble(d))
  d
}

## Text that is not valid UTF-8, reported: what was converted from the
## encoding given, in a message and as attr(, "converted"); what is left, in
## a warning naming each column and its first rows, with the remedy, and as
## attr(, "not_utf8"). Columns are named as they are in the result.
#' @keywords internal
#' @noRd
ilm_wash_utf8 <- function(out, bad, bad_names, old, new, conv = NULL, encoding = NULL) {
  now <- function(x) { i <- match(x, old); ifelse(is.na(i), x, new[i]) }
  if (!is.null(conv) && (length(conv$converted) || conv$names)) {
    done <- character(0)
    if (length(conv$converted)) {
      cc <- data.frame(column = now(names(conv$converted)),
                       n = unname(conv$converted), stringsAsFactors = FALSE)
      attr(out, "converted") <- cc
      done <- sprintf("%s (%s %s)", cc$column, format(cc$n, big.mark = ",", trim = TRUE),
                      ifelse(cc$n == 1L, "value", "values"))
    }
    if (conv$names)
      done <- c(done, sprintf("%d column %s", conv$names,
                              if (conv$names == 1L) "name" else "names"))
    message("ilm_wash_df(): converted from ", encoding, " to UTF-8: ", ilm_and(done), ".")
  }
  if (is.null(bad) && !length(bad_names)) return(out)
  msg <- character(0)
  if (!is.null(bad)) {
    bad$column <- now(bad$column)
    each <- sprintf("%s (%s %s; %s)", ilm_show_text(bad$column),
                    format(bad$n, big.mark = ",", trim = TRUE),
                    ifelse(bad$n == 1L, "value", "values"),
                    vapply(seq_len(nrow(bad)), function(i)
                      ilm_rows_text(as.integer(strsplit(bad$rows[i], ", ", fixed = TRUE)[[1]]),
                                    bad$n[i]), ""))
    msg <- c(msg, sprintf("text in %d %s is not valid UTF-8%s and was left as it is: %s.",
                          nrow(bad), if (nrow(bad) == 1L) "column" else "columns",
                          if (is.null(encoding)) ""
                          else paste0(" and did not convert from ", encoding),
                          ilm_and(each)))
    attr(out, "not_utf8") <- bad
  }
  if (length(bad_names))
    msg <- c(msg, sprintf("%d column %s not valid UTF-8, so %s written with each stray byte spelled out: %s.",
                          length(bad_names),
                          if (length(bad_names) == 1L) "name is" else "names are",
                          if (length(bad_names) == 1L) "it is" else "they are",
                          ilm_and(ilm_show_text(now(bad_names)))))
  remedy <- if (is.null(encoding)) ILM_UTF8_REMEDY else paste0(
    "Such a value is not valid ", encoding, " either, or did not convert back to the ",
    "same bytes: the file may be in another encoding (iconvlist() lists those this ",
    "system knows).")
  warning("ilm_wash_df(): ", paste(msg, collapse = " "), " ", remedy, call. = FALSE)
  out
}

## ---- translate -------------------------------------------------------------

## Recode against a dictionary held as two parallel vectors -- the shape a
## lookup table takes when it comes out of a spreadsheet. Values with no entry
## in `old` become NA, which is deliberate: a silent pass-through would hide
## codes the dictionary does not cover.
#' Recode a variable against a dictionary held as two vectors
#'
#' The shape a lookup table takes when it comes out of a spreadsheet: one
#' vector of old codes, one of new, matched by position.
#'
#' Values with no entry in `old` become `NA` by default, which is deliberate: a
#' silent pass-through would hide codes the dictionary does not cover. Use
#' `default` to change that.
#'
#' @param y A vector to recode.
#' @param old Values of the old coding scheme.
#' @param new Replacements, the same length as `old`.
#' @param default Value for entries not found in `old`. `NA` by default.
#' @return A vector of the same length as `y`.
#' @seealso [match()], [ilm_recode_errors()].
#' @examples
#' ilm_translate(c(1, 2, 3, 99), old = 1:3, new = c("low", "mid", "high"))
#' ilm_translate(c(1, 2, 99), old = 1:2, new = c(10, 20), default = -1)
#' @export
ilm_translate <- function(y, old, new, default = NA) {
  if (length(old) != length(new))
    stop("`old` and `new` must be the same length (they are ",
         length(old), " and ", length(new), ")", call. = FALSE)
  if (anyDuplicated(old))
    warning("`old` contains duplicate entries; the first match is used",
            call. = FALSE)
  i <- fmatch(as.character(y), as.character(old))
  out <- new[i]
  if (!identical(default, NA)) out[is.na(i) & !is.na(y)] <- default
  out
}

## ---- recode_errors ---------------------------------------------------------

## Replace known-bad codes (999, -1, "unknown", ...) with NA or another value.
## Restricting to rows/cols matters: a sentinel that means "missing" in one
## column can be a legitimate measurement in another.

#' Replace known-bad values in a vector
#'
#' The vector path of [ilm_recode_errors()], exposed for use inside other
#' pipelines. Factor levels that are recoded away are dropped.
#'
#' @param x A vector.
#' @param errors Values to recode.
#' @param replacement What to put in their place.
#' @return A vector of the same length as `x`.
#' @examples
#' ilm_recode_errors_vec(factor(c("a", "b", "unknown")), errors = "unknown")
#' @export
ilm_recode_errors_vec <- function(x, errors, replacement = NA) {
  if (missing(errors) || !length(errors))
    stop("`errors` must contain at least one value to recode", call. = FALSE)
  hit <- if (is.factor(x)) as.character(x) %in% as.character(errors)
         else x %in% errors
  if (is.factor(x)) {
    lv <- setdiff(levels(x), as.character(errors))
    x <- factor(ifelse(hit, NA_character_, as.character(x)), levels = lv)
    if (!is.na(replacement)) {
      x <- factor(ifelse(hit, as.character(replacement), as.character(x)),
                  levels = unique(c(lv, as.character(replacement))))
    }
    return(x)
  }
  ## only where something matched: assigning a text replacement to no
  ## elements still turns a numeric column into text
  if (any(hit)) x[hit] <- replacement
  x
}

#' Replace known-bad values with NA or another value
#'
#' Sentinels such as 999, -1 or `"unknown"` mean missing in one column and can
#' be a legitimate measurement in another, which is why the replacement can be
#' restricted to particular rows and columns.
#'
#' @param data A vector, data frame or matrix.
#' @param errors Values to recode.
#' @param replacement What to put in their place. `NA` by default.
#' @param rows,cols Restrict the replacement (data frame or matrix input).
#' @param ind Restrict the replacement (vector input).
#' @return An object of the same shape as `data`.
#' @examples
#' ilm_recode_errors(c(1, 2, 999, -1), errors = c(999, -1))
#' d <- data.frame(x = c(1, 999, 3), y = c(999, 2, 3))
#' ilm_recode_errors(d, errors = 999, cols = "x")
#' @export
ilm_recode_errors <- function(data, errors, replacement = NA,
                              rows = NULL, cols = NULL, ind = NULL) {
  if (missing(errors) || !length(errors))
    stop("`errors` must contain at least one value to recode", call. = FALSE)

  if (is.data.frame(data) || is.matrix(data)) {
    if (!is.null(ind))
      stop("`ind` applies to vector input; use `rows` and `cols` for a ",
           if (is.matrix(data)) "matrix" else "data frame", call. = FALSE)
    nms <- if (is.data.frame(data)) names(data) else colnames(data)
    ci <- if (is.null(cols)) seq_along(nms) else {
      if (is.character(cols)) {
        miss <- setdiff(cols, nms)
        if (length(miss))
          stop("column(s) not found: ", paste(miss, collapse = ", "),
               ". Available: ", paste(nms, collapse = ", "), call. = FALSE)
        match(cols, nms)
      } else {
        bad <- cols[cols < 1 | cols > length(nms)]
        if (length(bad))
          stop("column index out of range: ", paste(bad, collapse = ", "),
               " (data has ", length(nms), " columns)", call. = FALSE)
        as.integer(cols)
      }
    }
    ri <- if (is.null(rows)) seq_len(nrow(data)) else {
      bad <- rows[rows < 1 | rows > nrow(data)]
      if (length(bad))
        stop("row index out of range: ", paste(bad, collapse = ", "),
             " (data has ", nrow(data), " rows)", call. = FALSE)
      as.integer(rows)
    }
    if (is.matrix(data)) {
      sub <- data[ri, ci, drop = FALSE]
      sub[sub %in% errors] <- replacement
      data[ri, ci] <- sub
      return(data)
    }
    for (j in ci) {
      v <- data[[j]]
      v[ri] <- ilm_recode_errors_vec(v[ri], errors, replacement)
      data[[j]] <- v
    }
    return(data)
  }

  if (!is.null(rows) || !is.null(cols))
    stop("`rows` and `cols` apply to data frame or matrix input; use `ind` ",
         "for a vector", call. = FALSE)
  if (is.null(ind)) return(ilm_recode_errors_vec(data, errors, replacement))
  bad <- ind[ind < 1 | ind > length(data)]
  if (length(bad))
    stop("index out of range: ", paste(bad, collapse = ", "),
         " (vector has ", length(data), " elements)", call. = FALSE)
  data[ind] <- ilm_recode_errors_vec(data[ind], errors, replacement)
  data
}
