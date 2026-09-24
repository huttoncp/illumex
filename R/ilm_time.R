## Date and date-time columns, turned into numbers the reductions can use.
##
## None of the reductions has a use for a date as it stands: PCA wants a
## number, MCA a category, and a GLRM a loss, and a date is none of those. It
## used to be dropped. What it carries is where each row sits in time -- an
## ORDER, and the spacing between -- and that is kept by the time elapsed
## since the earliest value, one number per row. A timestamp can carry
## rhythms as well -- the hour of the day, the day of the week, the point in
## the month, the season -- as a sine and a cosine each so that 23:00 sits
## next to midnight, the 31st next to the 1st and December next to January.
##
## A rhythm is used only when the other columns vary along it, because every
## column a reduction is given weighs about the same: a date spread over nine
## columns would count nine times over and steer the whole result, and a
## cycle nothing else follows is noise the clustering will split on. See
## ilm_time_rhythms() below for the test and what it was measured to be
## worth. Elapsed time alone skips the test, for very large data.

#' @keywords internal
#' @noRd
ilm_is_time <- function(x) inherits(x, c("Date", "POSIXt", "difftime"))

## The numeric columns one date/time column becomes. A cycle is used only when
## the data cover at least two of it -- two days for the hour, two weeks for
## the day of the week, two months for the point in the month, two years for
## the season: with less, where a row sits in the cycle and how much time has
## passed cannot be told apart, and the pair would only repeat the elapsed
## time.
##
## The point in the month is the share of ITS month gone, not the day number,
## so the last day of February and the last day of March are both at the end:
## month-end is a place in the cycle whatever the month's length.
#' @keywords internal
#' @noRd
ilm_time_features <- function(x, name, cycles = FALSE) {
  ## a duration is already a quantity
  if (inherits(x, "difftime")) {
    out <- list(as.numeric(x, units = "days")); names(out) <- name
    return(out)
  }
  day <- if (inherits(x, "Date")) as.numeric(x) else as.numeric(as.POSIXct(x)) / 86400
  out <- list()
  out[[paste0(name, "_elapsed")]] <- day - min(day, na.rm = TRUE)
  if (!cycles) return(out)
  span <- diff(range(day, na.rm = TRUE))
  lt <- as.POSIXlt(x)
  add <- function(lab, frac, needs) {
    if (!is.finite(span) || span < needs) return(invisible())
    if (length(unique(stats::na.omit(round(frac, 8)))) < 2L) return(invisible())
    out[[paste0(name, "_", lab, "_sin")]] <<- sin(2 * pi * frac)
    out[[paste0(name, "_", lab, "_cos")]] <<- cos(2 * pi * frac)
  }
  hr <- if (inherits(x, "POSIXt")) (lt$hour + lt$min / 60 + lt$sec / 3600) / 24 else 0
  if (inherits(x, "POSIXt")) add("hour", hr, needs = 2)
  add("wday", (lt$wday + hr) / 7, needs = 14)
  add("mday", (lt$mday - 1 + hr) / ilm_month_days(lt), needs = 61)
  add("yday", (lt$yday + hr) / 366, needs = 2 * 365.25)
  out
}

#' @keywords internal
#' @noRd
ilm_month_days <- function(lt) {
  y <- lt$year + 1900
  leap <- (y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0
  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[lt$mon + 1] + (lt$mon == 1 & leap)
}

## Every date/time column of `sub` replaced by its numbers, in place, and a
## message saying what became of each: `time` is "elapsed", "cycles" or
## "drop". Columns of any other type pass through untouched.
#' @keywords internal
#' @noRd
ilm_time_encode <- function(sub, time, caller) {
  tc <- names(sub)[vapply(sub, ilm_is_time, TRUE)]
  if (!length(tc)) return(sub)
  if (identical(time, "drop")) {
    message(caller, "(): dropping date/time column(s), as `time = \"drop\"` ",
            "asks: ", paste(tc, collapse = ", "))
    return(sub[setdiff(names(sub), tc)])
  }
  out <- list(); said <- character(0); map <- list()
  others <- sub[setdiff(names(sub), tc)]
  for (v in names(sub)) {
    if (!v %in% tc) { out[[v]] <- sub[[v]]; next }
    f <- ilm_time_features(sub[[v]], v, cycles = identical(time, "cycles"))
    why <- ""
    cand <- unique(sub("_(sin|cos)$", "", substring(
      names(f)[grepl("_(sin|cos)$", names(f))], nchar(v) + 2L)))
    if (length(cand)) {
      kept <- ilm_time_rhythms(sub[[v]], others, cand)
      out_c <- setdiff(cand, kept)
      f <- f[!grepl("_(sin|cos)$", names(f)) |
               sub("_(sin|cos)$", "", substring(names(f), nchar(v) + 2L)) %in% kept]
      w <- function(a, sep) paste(ilm_time_words[a], collapse = sep)
      left <- if (length(out_c) == 1L) "that is left out" else "those are left out"
      inner <- if (isFALSE(attr(kept, "tested")))
        "there is no other column to test its cycles against, so all are used"
      else if (!length(kept))
        sprintf("the other columns do not vary with its %s, so %s", w(out_c, " or "), left)
      else paste0(sprintf("the other columns vary with its %s", w(kept, " and ")),
                  if (length(out_c)) sprintf("; not with its %s, so %s",
                                             w(out_c, " or "), left))
      ## a column that IS one of the cycles was not counted as following it
      same <- attr(kept, "same")
      sm <- vapply(names(same), function(a)
        sprintf("%s %s its %s already", paste(same[[a]], collapse = " and "),
                if (length(same[[a]]) == 1L) "is" else "are", ilm_time_words[[a]]), "")
      why <- sprintf(" (%s)", paste(c(inner, sm), collapse = "; "))
    }
    ## what each new column carries, recorded before any renaming below
    asp <- ifelse(names(f) == v, "duration",
                  sub("_(sin|cos)$", "", substring(names(f), nchar(v) + 2L)))
    ## a name the data already use -- a `day_elapsed` kept beside `day` --
    ## is not overwritten or doubled; the new column gets a unique name
    taken <- c(setdiff(names(sub), v), names(out))
    clash <- names(f) %in% taken
    if (any(clash))
      names(f)[clash] <- make.unique(c(taken, names(f)[clash]))[
        length(taken) + seq_len(sum(clash))]
    out <- c(out, f)
    map[[v]] <- stats::setNames(asp, names(f))
    said <- c(said, if (length(f) == 1L && identical(names(f), v))
                      sprintf("%s -> its length in days", v)
                    else sprintf("%s -> %s%s", v, paste(names(f), collapse = ", "), why))
  }
  message(caller, "(): date/time column(s) used as numbers -- ",
          paste(said, collapse = "; "), ".")
  out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  ## which encoded columns came from which date, so that what is found can be
  ## described in dates again afterwards
  attr(out, "time_map") <- map
  out
}

## ---------------------------------------------------------------------------
## Which of a date's cycles to use: the ones the other columns vary with.
##
## Measured on two known clusters (400 rows, two informative numeric columns,
## 20 replicates each; the adjusted Rand index against the truth), by
## dev/studies/time_encoding.R:
##
## * Every cycle a date covers, given unasked, is up to eight more columns,
##   and where the date meant nothing they took recovery from 0.38 to 0.10.
##   A cycle the rest of the data does not follow is noise, and noise is not
##   neutral in a clustering: it has its share of the geometry and none of
##   the structure.
## * Busyness is not enough either. Rows falling more often on weekdays, with
##   nothing else following it, halved recovery (0.35 to 0.18) when that was
##   the test for letting the weekday in -- and a rhythm in the VALUES with
##   none in the volume, weekend baskets that differ with weekend rows no
##   more common, was missed by it (0.23).
## * What earns a cycle its place is the other columns varying along it. With
##   that as the test, recovery with no rhythm in the data stayed where it
##   was without the date (0.36 against 0.38), and with one it rose from 0.36
##   to 0.94 for winter against summer, 0.93 for night against day, 0.79 for
##   weekends and 0.58 for month-ends. Elapsed time alone, in those four,
##   stayed at 0.36: a number that only grows cannot see a rhythm.
##
## The test compares the strongest separation any other column shows across
## the cycle's positions (eta squared or Cramer's V, as ilm_var_contrib()
## scores them) with the same when the positions are shuffled -- the maximum,
## so that one column following a month-end rhythm is not averaged away among
## thirty that do not. The cycle is let in when that separation stands five
## standard deviations above the shuffled ones, and 0.01 above them outright.
##
## Five, not the usual two or three, because the two errors are not alike. A
## cycle wrongly let in is two columns of noise, and in the study one such
## admission took a clustering from 0.36 to 0.00; a real rhythm left out only
## costs what the rhythm would have added. With p < 0.01 as the test, chance
## got a cycle in for 2 of 100 data sets with no rhythm at 400 rows and 1 of
## 60 at 2,000 (chance reached 4.8 standard deviations); at five, none did,
## while every rhythm of the study's strength still got in (the weakest stood
## 14 above chance) and 86% of rhythms half that strong
## (dev/studies/time_gate_threshold.R). A column that separates the
## positions almost perfectly (0.9 or more) is not following the cycle, it IS
## the cycle -- a month column, a weekend flag -- and is not counted as
## evidence for it: the cycle would only say again what that column says. It
## uses at most 5,000 rows and leaves the caller's random number stream as it
## found it.
## ---------------------------------------------------------------------------

## Every usable column of `others` as centred columns of one matrix, so the
## separation of all of them across a grouping is one rowsum(): a numeric
## column scaled to unit sum of squares, a categorical one as its indicator
## columns. A categorical with more than `max_levels` levels is left out --
## an identifier separates anything.
#' @keywords internal
#' @noRd
ilm_sep_prep <- function(others, max_levels = 50L) {
  M <- list(); var <- integer(0); p <- numeric(0); lev <- integer(0); j <- 0L
  kept <- character(0)
  for (nm in names(others)) {
    v <- others[[nm]]
    if (is.numeric(v)) {
      ok <- !is.na(v)
      if (sum(ok) < 3L) next
      v[!ok] <- mean(v[ok])
      s <- sqrt(sum((v - mean(v))^2))
      if (!is.finite(s) || s == 0) next
      j <- j + 1L; kept <- c(kept, nm)
      M[[length(M) + 1L]] <- (v - mean(v)) / s
      var <- c(var, j); p <- c(p, NA_real_); lev <- c(lev, 0L)
    } else if (is.factor(v) || is.character(v) || is.logical(v)) {
      f <- factor(v)
      if (nlevels(f) < 2L || nlevels(f) > max_levels) next
      f <- addNA(f, ifany = TRUE)
      j <- j + 1L; kept <- c(kept, nm)
      Z <- outer(as.integer(f), seq_len(nlevels(f)), "==") * 1
      pl <- colMeans(Z)
      for (l in seq_len(ncol(Z))) {
        M[[length(M) + 1L]] <- Z[, l] - pl[l]
        var <- c(var, j); p <- c(p, pl[l]); lev <- c(lev, nlevels(f))
      }
    }
  }
  if (!j) return(NULL)
  list(M = do.call(cbind, M), var = var, p = p, lev = lev, nvar = j,
       names = kept)
}

## The separation of each prepared column across the groups `b`: eta squared
## for a numeric column, Cramer's V for a categorical one -- the values
## ilm_var_sep() gives, from between-group sums of squares of the centred
## columns (for an indicator, that sum over n and over its share is its part
## of chi-squared over n).
#' @keywords internal
#' @noRd
ilm_sep_all <- function(pr, b) {
  b <- as.integer(factor(b)); n <- length(b)
  cnt <- tabulate(b)
  bss <- colSums(rowsum(pr$M, b)^2 / cnt[cnt > 0])
  num <- is.na(pr$p)
  out <- numeric(pr$nvar)
  out[pr$var[num]] <- bss[num]
  if (any(!num)) {
    chi_n <- tapply(bss[!num] / (n * pr$p[!num]), pr$var[!num], sum)
    L <- tapply(pr$lev[!num], pr$var[!num], `[`, 1L)
    out[as.integer(names(chi_n))] <- sqrt(chi_n / (pmin(L, sum(cnt > 0)) - 1))
  }
  out
}

## Where each row sits in a cycle, in bins: the hour, the day of the week, the
## tenth of its month, the month.
#' @keywords internal
#' @noRd
ilm_time_bins <- function(lt, aspect)
  switch(aspect, hour = lt$hour, wday = lt$wday,
         mday = pmin(9L, floor(10 * (lt$mday - 1) / ilm_month_days(lt))),
         yday = lt$mon)

## Of the cycles in `cand`, the ones some column of `others` varies with.
## With nothing else to test against, every one asked for is kept.
#' @keywords internal
#' @noRd
ilm_time_rhythms <- function(x, others, cand, B = 199L, max_n = 5000L) {
  if (!length(cand)) return(character(0))
  if (!length(others)) return(structure(cand, tested = FALSE))
  ## a fixed stream for a fixed answer, and the caller's put back after
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
          else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
            rm(".Random.seed", envir = globalenv()))
  set.seed(1L)
  ## the rows first, so a large table is never laid out in full
  rows <- which(!is.na(ilm_time_number(x)))
  if (length(rows) > max_n) rows <- sort(sample(rows, max_n))
  if (length(rows) < 10L) return(character(0))
  pr <- ilm_sep_prep(others[rows, , drop = FALSE])
  if (is.null(pr)) return(structure(cand, tested = FALSE))
  lt <- as.POSIXlt(x[rows])
  keep <- character(0); same <- list()
  for (a in cand) {
    b <- ilm_time_bins(lt, a)
    if (length(unique(b)) < 2L) next
    each <- ilm_sep_all(pr, b)
    is_it <- each >= 0.9
    if (any(is_it)) same[[a]] <- pr$names[is_it]
    if (all(is_it)) next
    obs <- max(each[!is_it])
    nul <- vapply(seq_len(B), function(i)
      max(ilm_sep_all(pr, sample(b))[!is_it]), 0)
    z <- (obs - mean(nul)) / stats::sd(nul)
    if (is.finite(z) && z >= 5 && obs - mean(nul) >= 0.01) keep <- c(keep, a)
  }
  structure(keep, same = same)
}

## ---------------------------------------------------------------------------
## Back from numbers to time, for the descriptions.
##
## The reductions see a date as the numbers above, and a characterisation
## built on them reported it that way: "high day_elapsed" is true and says
## nothing a reader can use, and "high when_wday_cos" says less. So every
## encoded column is traced back to the date it came from and the aspect of
## it that it carries, and a cluster is then described in dates: the middle
## half of it on the time line, and for a cycle the stretch of the day, week,
## month or year where it stands out, beside the share of all rows in that
## same stretch. A cluster of month-end transactions reads "94% from the 29th
## to the 31st of the month (all rows 11%)".
## ---------------------------------------------------------------------------

ilm_time_words <- c(elapsed = "time line", duration = "length",
                    hour = "time of day", wday = "day of the week",
                    mday = "day of the month", yday = "time of year")

## Which date/time column an encoded column came from, and which aspect of it
## the column carries; NULL for a column that did not come from one. The map
## holds, for each date column, the aspect of every column it became, named by
## that column.
#' @keywords internal
#' @noRd
ilm_time_aspect <- function(v, map) {
  for (col in names(map)) {
    a <- map[[col]]
    if (v %in% names(a)) return(list(column = col, aspect = a[[v]]))
  }
  NULL
}

## How to print a value of `x`'s time line, given as the number it was used as.
#' @keywords internal
#' @noRd
ilm_time_formatter <- function(x) {
  if (inherits(x, "difftime")) {
    u <- units(x)
    return(function(v) paste(format(signif(v, 3)), u))
  }
  if (inherits(x, "Date"))
    return(function(v) format(as.Date(v, origin = "1970-01-01")))
  x <- as.POSIXct(x)
  tz <- attr(x, "tzone"); if (is.null(tz)) tz <- ""
  ## a timestamp column holding nothing but midnights is dates in disguise
  midnight <- all(format(x, "%H:%M:%S") == "00:00:00", na.rm = TRUE)
  function(v) format(as.POSIXct(v, origin = "1970-01-01", tz = tz),
                     if (midnight) "%Y-%m-%d" else "%Y-%m-%d %H:%M")
}

#' @keywords internal
#' @noRd
ilm_time_number <- function(x)
  if (inherits(x, c("difftime", "Date"))) as.numeric(x) else as.numeric(as.POSIXct(x))

## The middle half of the rows in `m` on the time line, and of all rows.
#' @keywords internal
#' @noRd
ilm_time_middle <- function(x, m, fmt) {
  num <- ilm_time_number(x)
  q <- function(z) stats::quantile(z, c(0.25, 0.75), type = 1, na.rm = TRUE,
                                   names = FALSE)
  if (!any(m & !is.na(num)))
    return(list(text = NA_character_, share = NA_real_, share_all = NA_real_))
  qc <- q(num[m]); qa <- q(num)
  list(text = sprintf("middle half %s to %s (all rows %s to %s)", fmt(qc[1]),
                      fmt(qc[2]), fmt(qa[1]), fmt(qa[2])),
       share = NA_real_, share_all = NA_real_)
}

#' @keywords internal
#' @noRd
ilm_ordinal <- function(d)
  paste0(d, ifelse(d %% 100 %in% 11:13, "th",
                   c("th", "st", "nd", "rd", rep("th", 6))[d %% 10 + 1]))

## Where in a cycle the rows in `m` stand out: of every stretch up to half the
## cycle long, the one where their share differs most from all rows' share.
## The stretch may wrap -- 22:00 to 03:59, November to February -- which is
## the reason for the sine and cosine in the first place, and it may be where
## the cluster is ABSENT: a cluster of weekday rows is best said as "only 3%
## on Sat-Sun". The stretch was chosen to flatter the cluster, so a
## difference is guaranteed; it is reported only when it clears 15 points
## and three standard errors of a share at the cluster's size.
#' @keywords internal
#' @noRd
ilm_time_arc <- function(x, m, aspect) {
  lt <- as.POSIXlt(x)
  b <- switch(aspect, hour = lt$hour, wday = lt$wday, mday = lt$mday - 1L,
              yday = lt$mon)
  nb <- c(hour = 24L, wday = 7L, mday = 31L, yday = 12L)[[aspect]]
  ok <- !is.na(b)
  cc <- tabulate(b[m & ok] + 1L, nb); ca <- tabulate(b[ok] + 1L, nb)
  if (!sum(cc))
    return(list(text = NA_character_, share = NA_real_, share_all = NA_real_))
  best <- NULL
  ## shortest first, so a longer stretch has to do strictly better
  for (len in seq_len(nb %/% 2L)) for (s in seq_len(nb) - 1L) {
    idx <- (s + seq_len(len) - 1L) %% nb + 1L
    sh <- sum(cc[idx]) / sum(cc); sa <- sum(ca[idx]) / sum(ca)
    if (is.null(best) || abs(sh - sa) > abs(best$sh - best$sa) + 1e-12)
      best <- list(s = s, len = len, sh = sh, sa = sa)
  }
  se <- sqrt(best$sa * (1 - best$sa) / sum(cc))
  if (abs(best$sh - best$sa) < max(0.15, 3 * se))
    return(list(text = "much as all rows", share = NA_real_, share_all = NA_real_))
  s <- best$s; len <- best$len; e <- (s + len - 1L) %% nb
  rng <- function(labs) if (len == 1L) labs[s + 1L]
                        else paste0(labs[s + 1L], "-", labs[e + 1L])
  lab <- switch(aspect,
    hour = sprintf("between %02d:00 and %02d:59", s, e),
    wday = paste("on", rng(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))),
    mday = if (len == 1L) sprintf("on the %s of the month", ilm_ordinal(s + 1L))
           else sprintf("from the %s to the %s of the month",
                        ilm_ordinal(s + 1L), ilm_ordinal(e + 1L)),
    yday = paste("in", rng(month.abb)))
  pc <- round(100 * best$sh)
  list(text = sprintf("%s %s (all rows %d%%)",
                      if (!pc) "none" else if (best$sh < best$sa)
                        sprintf("only %d%%", pc) else sprintf("%d%%", pc),
                      lab, round(100 * best$sa)),
       share = best$sh, share_all = best$sa)
}

## One row per cluster, date column and aspect of it the reduction used.
#' @keywords internal
#' @noRd
ilm_time_describe <- function(data, map, cl) {
  out <- list()
  for (col in intersect(names(map), names(data))) {
    x <- data[[col]]
    if (!ilm_is_time(x)) next
    asp <- unique(unname(map[[col]]))
    fmt <- ilm_time_formatter(x)
    for (cc in sort(unique(cl[!is.na(cl)]))) {
      m <- !is.na(cl) & cl == cc
      for (a in asp) {
        d <- if (a %in% c("elapsed", "duration")) ilm_time_middle(x, m, fmt)
             else ilm_time_arc(x, m, a)
        out[[length(out) + 1L]] <- data.frame(
          cluster = cc, variable = col, aspect = ilm_time_words[[a]],
          description = d$text, share = d$share, share_all = d$share_all,
          stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(out)) return(NULL)
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

## The sentence a cluster's description gains for the dates among its top
## variables: each date, and each aspect of it that was named, in dates.
#' @keywords internal
#' @noRd
ilm_time_sentence <- function(vars, map, desc, cc) {
  if (is.null(desc) || !length(vars)) return(character(0))
  hit <- Filter(Negate(is.null), lapply(unique(vars), ilm_time_aspect, map = map))
  if (!length(hit)) return(character(0))
  cols <- unique(vapply(hit, `[[`, "", "column"))
  vapply(cols, function(col) {
    asp <- unique(vapply(Filter(function(h) h$column == col, hit), `[[`, "", "aspect"))
    ## an aspect the cluster is unremarkable on says nothing in a sentence;
    ## it stays in the table
    d <- desc[desc$cluster == cc & desc$variable == col &
                desc$aspect %in% ilm_time_words[asp] & !is.na(desc$description) &
                desc$description != "much as all rows", , drop = FALSE]
    if (!nrow(d)) return("")
    paste0(col, ": ", paste(d$description, collapse = "; "), ".")
  }, "", USE.NAMES = FALSE)
}
