## ---------------------------------------------------------------------------
## Helpers the family's packages share.
##
## This file is the same, byte for byte, in each of them; illumex's
## dev/check_shared_helpers.R compares it with illume's main. A change is
## made in all of them at once, in the same bytes.
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

## "a", "a and b", "a, b and c"
#' @keywords internal
#' @noRd
ilm_and <- function(x) {
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

## ---------------------------------------------------------------------------
## Progress reporting.
##
## Measured before being added: against a realistic loop -- 2000 bootstrap
## replicates on 20,000 rows -- a bar updated every 1%, or even every single
## iteration, made no difference beyond the run-to-run noise. The overhead
## only shows against a body so fast that a bar would be pointless anyway.
##
## So the bar is free wherever it is worth having, and the 1% step is there to
## avoid writing to a slow console rather than to save time.
##
## The default is interactive(): visible when a person is watching, silent in
## scripts, tests and knitr. A bar written into a vignette or a test log is
## noise, and would break every expect_silent() in the suite.
## ---------------------------------------------------------------------------

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

## ---------------------------------------------------------------------------
## The random-number stream is the user's.
##
## A function given a seed sets it for its own draws -- so its result is the
## same every time -- and used to leave the stream there, so that a user's
## next rnorm() after ilm_sim(), ilm_cluster() or ilm_anomaly() came out the
## same whatever came before it. That is a side effect nobody asked for, and
## one that makes a simulation of the user's own quietly less random.
##
## So a seeded function puts the stream back as it leaves: as it was, or
## absent if it was absent (a fresh session has no .Random.seed until the
## first draw, and should not be given one). The function's own draws are
## untouched -- set.seed() stays where it was -- so every seeded result is
## what it was. With seed = NULL nothing is set and nothing is restored: the
## function draws from the user's stream, as any R code does.
## ---------------------------------------------------------------------------

## Register, in the CALLER's frame, a restore of the global random stream to
## run when the caller exits -- normally or by an error. The deferred call is
## base R's on.exit(), reached from the caller's frame the way withr::defer()
## reaches it, with add = TRUE so a handler the caller registers later cannot
## replace it, as long as that one adds too.
#' @keywords internal
#' @noRd
ilm_rng_restore <- function(seed, envir = parent.frame()) {
  if (is.null(seed)) return(invisible(FALSE))
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE)
  restore <- function() {
    if (had) assign(".Random.seed", old, envir = genv)
    else if (exists(".Random.seed", envir = genv, inherits = FALSE))
      rm(".Random.seed", envir = genv)
  }
  thunk <- as.call(list(function() restore()))
  do.call(base::on.exit, list(thunk, TRUE, FALSE), envir = envir)
  invisible(TRUE)
}

## ---- plotting characters by name -------------------------------------------
##
## Base R's `pch` is 26 integers nobody remembers. Nothing in the argument says
## that 16 is a filled circle, and the difference between 16, 19, 20 and 21 is
## not guessable from the numbers -- so a plot gets whichever code the author
## happened to recall, and a reader comparing two plots cannot tell whether a
## difference in the markers was meant.
##
## Names are also checkable, which numbers are not: a pch outside 0:25 is
## accepted by graphics and quietly draws nothing, while a name that is not in
## the table stops here and lists the ones that are.
##
## A SINGLE character is left alone, because base R draws it literally --
## pch = "x" means the letter x, and translating it would silently turn the
## plot into crosses.

#' Plotting characters, by name
#'
#' The lookup behind the `pch` argument of the plotting functions. Numbers pass
#' through after a range check, a single character passes through (base R draws
#' it literally), and a name becomes the number base R wants.
#'
#' @param x Numeric `pch` codes, single characters, or names such as
#'   `"filled circle"`. Case, spaces, underscores, hyphens and dots are all
#'   ignored, so `"filled_circle"` and `"Filled Circle"` are the same thing.
#' @return An integer `pch` vector, or the input unchanged where it was already
#'   numeric or a single character.
#' @keywords internal
#' @noRd
ilm_pch <- function(x) {
  if (is.null(x) || !length(x)) return(x)
  if (is.numeric(x)) {
    bad <- x[is.finite(x) & (x < 0 | x > 25 | x != as.integer(x))]
    if (length(bad))
      stop("`pch` codes run from 0 to 25; got ",
           paste(unique(bad), collapse = ", "),
           ". Names work too, such as \"filled circle\".", call. = FALSE)
    return(x)
  }
  if (!is.character(x)) return(x)
  tab <- ilm_pch_table()
  key <- tolower(gsub("[ _.-]+", "", trimws(x)))
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) { out[[i]] <- NA_integer_; next }
    if (nchar(x[i]) == 1L) { out[[i]] <- x[i]; next }   # a literal glyph
    j <- match(key[i], names(tab))
    if (is.na(j))
      stop("'", x[i], "' is not a plotting character name. ",
           "The names are: ", paste(ilm_pch_names(), collapse = ", "), ".",
           call. = FALSE)
    out[[i]] <- tab[[j]]
  }
  ## a mix of glyphs and codes has to stay character, since that is the only
  ## vector that can carry both
  if (any(vapply(out, is.character, TRUE))) as.character(unlist(out))
  else as.integer(unlist(out))
}

## The names, in code order, primary name first. Aliases follow it, because a
## person reaching for "solid circle" should not have to find out that the package
## calls it something else.
#' @keywords internal
#' @noRd
ilm_pch_spec <- function() {
  list(
    c("0",  "open square", "square", "hollow square", "empty square"),
    c("1",  "open circle", "circle", "hollow circle", "empty circle"),
    c("2",  "open triangle", "triangle", "triangle up", "hollow triangle"),
    c("3",  "plus"),
    c("4",  "cross", "times"),
    c("5",  "open diamond", "diamond", "hollow diamond"),
    c("6",  "open triangle down", "triangle down", "down triangle"),
    c("7",  "square cross", "crossed square"),
    c("8",  "star", "asterisk"),
    c("9",  "diamond plus"),
    c("10", "circle plus"),
    c("11", "star of david", "double triangle"),
    c("12", "square plus"),
    c("13", "circle cross"),
    c("14", "square triangle"),
    c("15", "filled square", "solid square"),
    c("16", "filled circle", "solid circle", "point", "dot"),
    c("17", "filled triangle", "solid triangle"),
    c("18", "filled diamond", "solid diamond"),
    c("19", "large filled circle", "bold circle"),
    c("20", "small filled circle", "bullet", "small dot"),
    c("21", "circle fill", "filled circle outline", "bg circle"),
    c("22", "square fill", "filled square outline", "bg square"),
    c("23", "diamond fill", "filled diamond outline", "bg diamond"),
    c("24", "triangle fill", "filled triangle outline", "bg triangle"),
    c("25", "triangle down fill", "filled triangle down"))
}

#' @keywords internal
#' @noRd
ilm_pch_table <- function() {
  out <- list()
  for (s in ilm_pch_spec()) {
    code <- as.integer(s[1])
    for (nm in s[-1]) out[[gsub("[ _.-]+", "", nm)]] <- code
  }
  out
}

## The primary name of each code, spelled the way a person would write it --
## the lookup keys have had their spaces stripped and would read badly here.
#' @keywords internal
#' @noRd
ilm_pch_names <- function() {
  vapply(ilm_pch_spec(), function(s) sprintf("%s (%s)", s[2], s[1]), "")
}

## ---------------------------------------------------------------------------
## The display rules every printed number follows, so that each package
## shows the same digits for the same value:
##   1. rounding, half away from zero after clearing floating-point noise;
##   2. a display-rule formatter returning {text, rule, digits}.
## Names carry an `ilm_disp_` prefix where the two packages each have an
## older helper of the same idea with different behaviour (ilm_fmt_pct(): in
## illume a phrase with "under 1%"/"over 99%" ends, in illumex a plain
## percentage), so neither package's existing calls change meaning.
## ---------------------------------------------------------------------------

## Rounding half away from zero, after clearing the floating-point noise
## that makes 100 x 0.2725 come out 27.250000000000004 or 2.675 come out
## 2.67499999999999982. R's round() and sprintf() round a representable tie
## to even and follow the binary value otherwise. The noise is cleared at
## 1e-9 relative (1e-9 absolute below 1).
#' @keywords internal
#' @noRd
ilm_disp_round <- function(x, decimals) {
  m <- 10^decimals
  y <- abs(x) * m
  sign(x) * floor(y + 0.5 + 1e-9 * pmax(1, y)) / m
}

## To `digits` significant figures, by the same rounding.
#' @keywords internal
#' @noRd
ilm_disp_signif <- function(x, digits) {
  e <- ifelse(x == 0 | !is.finite(x), 0, floor(log10(abs(x))))
  r <- ilm_disp_round(x, digits - 1 - e)
  ## rounding can carry into the next power of ten (9.995 to 3 figures is
  ## 10.0); the figures stay `digits`, so round again at the new exponent
  e2 <- ifelse(r == 0 | !is.finite(r), e, floor(log10(abs(r))))
  ifelse(e2 > e, ilm_disp_round(x, digits - 1 - e2), r)
}

## A quantity as a statement shows it, and the rule it was shown by.
## Returns list(text, rule, digits): `text` a character vector the length of
## `x`, NA where `x` is NA; `rule` and `digits` the rule applied (`digits`
## NULL for "p"). Each value's text depends on that value alone.
##   "signif"  `digits` significant figures, trailing zeros kept (51.0, 10.0,
##             2.50; Craig's item 256), or dropped with `trailing_zeros =
##             FALSE` (51, 10, 2.5). An estimate that happens to be whole
##             still shows its figures (51.0, and 19234 is 19,200): a count,
##             or a data value that is a whole number, takes "fixed" with 0
##             digits instead, as its caller declares -- whole numbers are
##             not guessed at here
##   "fixed"   `digits` decimals, zeros kept; a count, or a whole data
##             value, is "fixed" with 0
##   "percent" a proportion times 100 at `digits` decimals, with "%"
##   "p"       Craig's rule: three significant figures down to 0.001, and
##             "< 0.001" below (the text of the number only; the sentence
##             supplies "p = " or "p ")
##   "clock"   an hour (0 to 23) as a time of day: "21:00" at the start of a
##             stretch, and with `end = TRUE` its last minute, "02:59"
##   "ordinal" a day of the month (1 to 31) in English, "1st", "22nd",
##             "28th"
## Thousands are grouped from 1,000 up ("1,230"), the family's rule. Only
## quantities come through here: a year, an identifier or a code that happens
## to be numeric is data, written as it is ("2024", never "2,024").
## Infinite values read "Inf" and "-Inf". The list carries `digits` for
## signif, fixed and percent, `end` for a clock that ends a stretch, and
## `trailing_zeros = FALSE` for a signif that drops them, the exception (kept
## is the rule, and says nothing).
## (ilm_disp_signif() rounds the number; zeros are a matter of its text, so
## the switch is here.)
#' @keywords internal
#' @noRd
ilm_disp <- function(x, rule = c("signif", "fixed", "percent", "p", "clock", "ordinal"),
                     digits = NULL, big_mark = ",", end = FALSE, trailing_zeros = TRUE) {
  rule <- match.arg(rule)
  if (rule %in% c("signif", "fixed", "percent") &&
      (is.null(digits) || length(digits) != 1L || digits < 0))
    stop("ilm_disp(): the \"", rule, "\" rule needs `digits`", call. = FALSE)
  x <- as.numeric(x)
  fin <- is.finite(x)
  txt <- rep(NA_character_, length(x))
  txt[!is.na(x) & !fin] <- ifelse(x[!is.na(x) & !fin] > 0, "Inf", "-Inf")
  if (any(fin)) {
    v <- x[fin]
    txt[fin] <- switch(rule,
      signif = trimws(formatC(ilm_disp_signif(v, digits), format = "fg", digits = digits,
                              flag = if (isTRUE(trailing_zeros)) "#" else "",
                              big.mark = big_mark)),
      fixed = trimws(formatC(ilm_disp_round(v, digits), format = "f", digits = digits,
                             big.mark = big_mark)),
      percent = paste0(trimws(formatC(ilm_disp_round(100 * v, digits), format = "f",
                                      digits = digits, big.mark = big_mark)), "%"),
      p = ifelse(v < 0.001, "< 0.001",
                 trimws(formatC(ilm_disp_signif(v, 3), format = "fg", digits = 3))),
      clock = sprintf(if (isTRUE(end)) "%02d:59" else "%02d:00", as.integer(round(v))),
      ordinal = {
        d <- as.integer(round(v))
        paste0(d, ifelse(d %% 100 %in% 11:13, "th",
                         c("th", "st", "nd", "rd", rep("th", 6))[d %% 10 + 1]))
      })
  }
  c(list(text = txt, rule = rule),
    if (rule %in% c("signif", "fixed", "percent")) list(digits = as.integer(digits)),
    if (rule == "clock" && isTRUE(end)) list(end = TRUE),
    if (rule == "signif" && !isTRUE(trailing_zeros)) list(trailing_zeros = FALSE))
}
