## ---------------------------------------------------------------------------
## Text that is not valid UTF-8.
##
## A file saved in another encoding (Windows-1252, Latin-1) and read as UTF-8
## leaves strings whose bytes are not valid UTF-8: "cafe" with an acute e
## as the bytes 63 61 66 e9. Sorting, matching, tables and factors take
## them in their stride, but R's regular expressions with perl = TRUE or
## fixed = TRUE, trimws(), tolower() and nchar() stop on them with "input
## string 2 is invalid UTF-8", so one such value among millions stopped a
## whole call; and R's graphics devices cannot draw them at all.
## These helpers work around them without changing their bytes or guessing
## their encoding, which only the person who has the file can know.
## ---------------------------------------------------------------------------

## Which values of a character vector or factor are not valid UTF-8; a
## factor's are found from its levels. NA is not counted.
#' @keywords internal
#' @noRd
ilm_not_utf8 <- function(v) {
  if (is.factor(v)) {
    bad <- !validUTF8(levels(v))
    if (!any(bad)) return(logical(length(v)))
    return(!is.na(v) & bad[as.integer(v)] %in% TRUE)
  }
  if (!is.character(v)) return(logical(length(v)))
  !validUTF8(v)
}

## trimws() that does not stop on text that is not valid UTF-8. Such a value
## is trimmed of ASCII spaces, tabs and line breaks byte by byte -- safe in
## any encoding that extends ASCII -- and otherwise keeps its bytes and its
## declared encoding.
#' @keywords internal
#' @noRd
ilm_trimws <- function(x) {
  ok <- validUTF8(x)
  if (all(ok)) return(trimws(x))
  x[ok] <- trimws(x[ok])
  ws <- "[ \t\r\n]+"
  x[!ok] <- sub(paste0(ws, "$"), "", sub(paste0("^", ws), "", x[!ok], useBytes = TRUE),
                useBytes = TRUE)
  x
}

## Text safe to print, pad, draw or turn into a name: a value that is not
## valid UTF-8 shows each stray byte as <e9>, so the rest of it can still be
## read; a factor's levels are shown the same way. Anything else comes back
## as it is.
#' @keywords internal
#' @noRd
ilm_show_text <- function(x) {
  if (is.factor(x)) {
    levels(x) <- ilm_show_text(levels(x))
    return(x)
  }
  if (!is.character(x)) return(x)
  ok <- validUTF8(x)
  if (all(ok)) return(x)
  x[!ok] <- iconv(x[!ok], "UTF-8", "UTF-8", sub = "byte")
  x
}

## "row 3", "rows 3, 17 and 40", or "rows 3, 17, 40, 41, 52 and 1,203
## more": the first rows of n
#' @keywords internal
#' @noRd
ilm_rows_text <- function(i, n = length(i)) {
  shown <- format(i, big.mark = ",", trim = TRUE)
  paste0(if (n == 1L) "row " else "rows ",
         if (n <= length(i)) ilm_and(shown)
         else paste0(paste(shown, collapse = ", "), " and ",
                     format(n - length(i), big.mark = ",", trim = TRUE), " more"))
}

## The columns of a data frame holding text that is not valid UTF-8: one row
## each, with the count and the first rows. NULL when there are none.
#' @keywords internal
#' @noRd
ilm_not_utf8_cols <- function(d, first = 5L) {
  hit <- lapply(d, function(v) which(ilm_not_utf8(v)))
  hit <- hit[lengths(hit) > 0L]
  if (!length(hit)) return(NULL)
  data.frame(column = names(hit), n = lengths(hit, use.names = FALSE),
             rows = vapply(hit, function(i) paste(utils::head(i, first), collapse = ", "), "",
                           USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

## What to do about text that is not valid UTF-8, for a message.
#' @keywords internal
#' @noRd
ILM_UTF8_REMEDY <- paste0(
  "The file was probably saved in another encoding, such as Windows-1252: read it ",
  "again in that encoding (read.csv(file, fileEncoding = \"windows-1252\"), or readr's ",
  "locale(encoding = \"windows-1252\")), or convert a column with ",
  "iconv(x, from = \"windows-1252\", to = \"UTF-8\").")

## The data a plot draws from, safe to draw. R's graphics devices cannot
## draw text that is not valid UTF-8: the pdf device crashes R, and the
## others stop with an error. So a plot draws from a copy whose column
## names, text values and factor levels show their stray bytes as <e9>,
## and the column-name arguments of the calling function (x, y, by, ...)
## are spelled the same way, so they still find their columns. The
## caller's data are not changed.
#' @keywords internal
#' @noRd
ilm_plot_frame <- function(data, env = parent.frame(),
                           args = c("x", "y", "by", "facet", "var1", "var2", "cols")) {
  ilm_plot_frame_check(data)
  for (a in intersect(args, ls(env))) {
    if (eval(call("missing", as.name(a)), env)) next
    v <- get(a, envir = env)
    if (is.character(v)) assign(a, ilm_show_text(v), envir = env)
  }
  bad_nm <- !validUTF8(names(data))
  txt <- vapply(data, function(v) is.character(v) || is.factor(v), TRUE)
  bad_col <- txt
  bad_col[txt] <- vapply(data[txt], function(v) any(ilm_not_utf8(v)), TRUE)
  if (!any(bad_nm) && !any(bad_col)) return(data)
  for (j in which(bad_col)) {
    data[[j]] <- ilm_show_text(data[[j]])
  }
  names(data)[bad_nm] <- ilm_show_text(names(data)[bad_nm])
  data
}

## Stop, naming them, when columns a computation uses have names that are
## not valid UTF-8: building a model's columns from them (make.names(),
## a formula) fails with no column named. ilm_wash_df() cleans such names.
#' @keywords internal
#' @noRd
ilm_stop_not_utf8_names <- function(nms, fn) {
  bad <- nms[!validUTF8(nms)]
  if (!length(bad)) return(invisible(NULL))
  stop(fn, "(): ", if (length(bad) == 1L) "the column name " else "the column names ",
       ilm_and(ilm_show_text(bad)), if (length(bad) == 1L) " is" else " are",
       " not valid UTF-8, so no model can be built from ",
       if (length(bad) == 1L) "it" else "them", ": ilm_wash_df() cleans such names. ",
       ILM_UTF8_REMEDY,
       call. = FALSE)
}
