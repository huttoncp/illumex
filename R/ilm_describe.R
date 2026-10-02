## ---------------------------------------------------------------------------
## Class-aware description of variables, with a gaussian agreement index.
##
## Built on collapse primitives and base stats, so the only cost beyond base R
## is collapse itself, which depends on nothing but Rcpp.
## ---------------------------------------------------------------------------

## ---- gaussian plausibility, and why not ------------------------------------
##
## This deliberately does NOT report a normality test p-value. Any test of
## "exactly normal" rejects everything once n is large, so the p-value answers
## a question nobody asked. What a researcher wants to know is how FAR from
## normal the variable is, which is an effect size.
##
## The measure is the Kolmogorov distance between the empirical distribution
## and the best-fitting normal -- the largest amount, on the cumulative
## probability scale, by which a normal model misstates the data. It converges
## to a population quantity instead of degenerating with n.
##
## For normal data, sampling noise alone produces a distance of about
## 0.6/sqrt(n), and 95% of the time no more than about 0.9/sqrt(n): the
## Lilliefors 5% critical value, 0.886/sqrt(n) for large n. The score subtracts
## that 95th percentile, so it charges only the excess beyond what normal data
## of the same size would plausibly give. dev/studies/gauss_noise.R measures
## both. `gauss` is therefore an AGREEMENT INDEX on 0-1, not a probability, and
## is documented as such.

#' @keywords internal
#' @noRd
ilm_gauss_d <- function(x) {
  x <- sort(x[is.finite(x)]); n <- length(x)
  s <- fsd(x); if (!is.finite(s) || s == 0) return(NA_real_)
  p <- stats::pnorm(x, fmean(x), s)
  max(pmax(abs(p - seq_len(n) / n), abs(p - (seq_len(n) - 1) / n)))
}

## how many modes does a kernel density estimate show?  This is what catches
## mixtures -- the failure mode a single-family fit gets confidently wrong.
#' @keywords internal
#' @noRd
ilm_n_modes <- function(x, rel = 0.10, drop = 0.5) {
  x <- x[is.finite(x)]
  if (length(x) < 20 || fsd(x) == 0) return(1L)
  d <- try(stats::density(x, n = 512), silent = TRUE)
  if (inherits(d, "try-error")) return(1L)
  pk <- which(diff(sign(diff(d$y))) == -2L) + 1L
  pk <- pk[d$y[pk] > rel * max(d$y)]
  if (length(pk) <= 1L) return(max(1L, length(pk)))
  ## Height alone counts ripple as peaks, which made uniform, exponential and
  ## Poisson data all look "multimodal". Require PROMINENCE: two peaks are
  ## distinct only if the valley between them drops well below both.
  keep <- pk[1L]
  for (i in 2:length(pk)) {
    last <- keep[length(keep)]
    valley <- min(d$y[last:pk[i]])
    if (valley < drop * min(d$y[last], d$y[pk[i]])) {
      keep <- c(keep, pk[i])
    } else if (d$y[pk[i]] > d$y[last]) {
      keep[length(keep)] <- pk[i]
    }
  }
  length(keep)
}

## `cap` is the excess departure at which alignment reaches 0. It is not
## plucked from the air: dev/studies/gauss_cap.R measured the excess of clear
## departures at 500 rows as lognormal 0.182, bimodal 0.104, Poisson 0.094
## and t with 3 df 0.056 (and more at 100,000), where normal data sit below
## 0. The first cap, 0.12, left t(3) at 0.52 and a bimodal mixture at 0.14 --
## the middle of the scale, as if half normal. At 0.06 (Craig's ruling,
## 2026-09-28) every one of them scores 0.07 or less, and normal data 1. It
## is exposed as an argument.
#' How far a variable is from gaussian, and why
#'
#' Reports an agreement index on 0-1 and, when it is low, the reason.
#'
#' The index measures the size of the departure from normal, not its kind. A
#' variable scoring 0.3 may be skewed, heavy-tailed, lumpy or piled on a few
#' values; `gauss_note` names which, and a plot of it -- `ilm_plot(data, x)`
#' -- shows it.
#'
#' This deliberately does not report a normality test p-value. Any test of
#' exact normality rejects everything once `n` is large, so the p-value answers
#' a question nobody asked. What matters is how far from normal a variable is,
#' which is an effect size.
#'
#' The measure is the Kolmogorov distance between the data and the best-fitting
#' normal: the largest amount, on the cumulative probability scale, by which a
#' normal model misstates the data. For normal data, sampling noise alone
#' produces a distance of about `0.6/sqrt(n)`, and 95% of the time no more than
#' about `0.9/sqrt(n)` (Lilliefors, 1967, gives 0.886 for large samples). The
#' index subtracts that 95th percentile, so it charges only the departure
#' beyond what normal data of the same size would plausibly give. It is an
#' agreement index, not a probability.
#'
#' @param x A numeric vector.
#' @param min_n Below this many observations the index is not computed.
#' @param cap The excess distance at which agreement reaches 0. The default
#'   0.06 puts clear departures at the bottom of the scale: with 500 rows a
#'   lognormal, a two-humped mixture, a Poisson count and t with 3 degrees of
#'   freedom all score 0.07 or less, while normal data score 1.
#' @return A list with `gauss` (0-1), `ks_d` (the raw distance) and
#'   `gauss_note` (empty when agreement is high).
#' @references
#' Lilliefors, H. W. (1967). On the Kolmogorov-Smirnov test for normality with
#' mean and variance unknown. Journal of the American Statistical Association,
#' 62(318), 399-402.
#' @examples
#' set.seed(1)
#' ilm_gauss_check(rnorm(500))
#' ilm_gauss_check(rlnorm(500))
#' ilm_gauss_check(c(rnorm(250), rnorm(250, 5)))
#' @export
ilm_gauss_check <- function(x, min_n = 20L, cap = 0.06) {
  a <- ilm_gauss_assess(x, min_n, cap)
  ilm_gauss_result(a$gauss, a$ks_d, a$note)
}

## Sample skewness and excess kurtosis, the type 2 estimators (G1, G2) of
## Joanes and Gill (1998): the ones SAS and SPSS report, and unbiased for
## normal data, where the type 3 illumex gave before (e1071's default)
## averages -0.54 at n = 20 and -0.24 at n = 50 (type 2: 0.01 and -0.01;
## 20,000 normal samples each, dev/studies/kurt_bias.R). One definition, used by
## describe's `skew` and `kurt` columns and by the kurtosis behind
## gauss_note's "heavy-tailed" and "light-tailed", so the number printed and
## the note always agree (Craig's item 205).
#' @keywords internal
#' @noRd
ilm_skew2 <- function(x) {
  n <- length(x)
  if (n < 3L) return(NA_real_)
  d <- x - fmean(x); m2 <- fmean(d^2)
  if (m2 == 0) return(NA_real_)
  fmean(d^3) / m2^1.5 * sqrt(n * (n - 1)) / (n - 2)
}

#' @keywords internal
#' @noRd
ilm_kurt2 <- function(x) {
  n <- length(x)
  if (n < 4L) return(NA_real_)
  d <- x - fmean(x); m2 <- fmean(d^2)
  if (m2 == 0) return(NA_real_)
  g2 <- fmean(d^4) / m2^2 - 3
  ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
}

## The assessment behind ilm_gauss_check(): the score, the distance, and the
## note's reasons, the first of them the kind of departure that most changes
## what to do next.
#' @keywords internal
#' @noRd
ilm_gauss_assess <- function(x, min_n = 20L, cap = 0.06) {
  x <- x[is.finite(x)]; n <- length(x)
  ## values so large their spread overflows (near 1e300, or 1e300 beside
  ## ordinary values) have no finite sd to assess a shape against
  if (n >= min_n && !is.finite(fsd(x)))
    return(list(gauss = NA_real_, ks_d = NA_real_,
                note = "values too large to assess", kind = "not_assessed"))
  if (n < min_n || fsd(x) == 0)
    return(list(gauss = NA_real_, ks_d = NA_real_,
                note = if (n >= min_n) "constant (zero variance)"
                       else "n too small to assess",
                kind = "not_assessed"))
  D <- ilm_gauss_d(x)
  ## the 95th percentile of D for genuinely normal data of this size (0.887 to
  ## 0.905 times 1/sqrt(n) for n from 100 to 2,000; dev/studies/gauss_noise.R),
  ## so the score charges only the departure that exceeds sampling noise
  Dref <- 0.9 / sqrt(n)
  score <- 1 - min(1, max(0, (D - Dref) / cap))

  if (score >= 0.95)
    return(list(gauss = score, ks_d = D, note = "", kind = "none"))

  s <- fsd(x)
  ku <- ilm_kurt2(x)
  ## Moment skewness is unstable under heavy tails -- it called a symmetric
  ## t(3) sample "right-skewed". Bowley's quartile skewness is bounded and
  ## robust, so it reports direction rather than tail noise.
  q <- stats::quantile(x, c(.25, .5, .75), names = FALSE)
  bow <- if (q[3] > q[1]) (q[3] + q[1] - 2 * q[2]) / (q[3] - q[1]) else 0
  nu <- fndistinct(x)
  discrete <- all(abs(x - round(x)) < 1e-8) && nu <= max(20L, n / 50)
  reasons <- character(0); kinds <- character(0)
  why <- function(text, kind) { reasons <<- c(reasons, text); kinds <<- c(kinds, kind) }
  ## ordered by how much the reason should change what the user does next
  ## (a kernel density over discrete values is spiky, so skip modes there)
  if (!discrete && ilm_n_modes(x) >= 2L) why("multimodal (check for subgroups)", "multimodal")
  if (discrete) why(sprintf("discrete (%d distinct values)", nu), "discrete")
  ## A stack of identical values at one end of a continuous variable is not a
  ## shape a distribution produces; it is a limit. A detection floor, an
  ## instrument ceiling, a capped scale. It is worth naming before skewness is,
  ## because the remedy is different: ilm_censor(), not a transformation.
  if (!discrete && nu > 20L) {
    pmin_ <- fmean(x == min(x)); pmax_ <- fmean(x == max(x))
    if (pmin_ >= 0.02 && n * pmin_ >= 5)
      why(sprintf("%.0f%% of values sit exactly at the minimum (%s): a floor, see ilm_censor()",
                  100 * pmin_, format(min(x), digits = 4)), "floor")
    if (pmax_ >= 0.02 && n * pmax_ >= 5)
      why(sprintf("%.0f%% of values sit exactly at the maximum (%s): a ceiling, see ilm_censor()",
                  100 * pmax_, format(max(x), digits = 4)), "ceiling")
  }
  if (all(x >= 0) && (min(x) - 0) < 0.05 * s) why("bounded at zero", "bounded_zero")
  if (bow > 0.1) why("right-skewed", "skewed")
  if (bow < -0.1) why("left-skewed", "skewed")
  if (ku > 1) why("heavy-tailed", "heavy_tailed")
  if (ku < -1) why("light-tailed / flat", "light_tailed")
  if (!length(reasons)) why("departs from normal, no single dominant cause", "unclear")
  list(gauss = score, ks_d = D, note = paste(utils::head(reasons, 2L), collapse = "; "),
       kind = kinds[1L])
}

## ---- describe --------------------------------------------------------------
##
## Every column here answers "what about this variable will break a model?",
## not merely "what does this variable look like". The default output is kept
## narrow on the principle that a summary nobody reads is worse than a short
## one: quartiles, skew and kurtosis are available on request, but `gauss` and
## `gauss_note` already say whether they are worth asking for.

## percentile column names follow whatever `probs` the user asked for:
## c(0, .5, 1) -> p0, p50, p100;  c(.025, .975) -> p2.5, p97.5
#' @keywords internal
#' @noRd
ilm_prob_names <- function(probs)
  ## formatC(digits = 6) pads to that width, which produced names like
  ## "p      0"; as.character on a rounded value gives p0 / p50 / p2.5
  paste0("p", trimws(as.character(round(probs * 100, 6))))

## The index and the distance are kept whole and printed as they used to be
## stored, to 3 and 4 places, so what prints is what printed before.
#' @keywords internal
#' @noRd
ilm_gauss_result <- function(gauss, ks_d, note)
  structure(list(gauss = gauss, ks_d = ks_d, gauss_note = note), class = "ilm_gauss")

#' @export
print.ilm_gauss <- function(x, ...) {
  y <- unclass(x)
  y$gauss <- round(y$gauss, 3); y$ks_d <- round(y$ks_d, 4)
  print(y, ...)
  invisible(x)
}

## ---- numeric ---------------------------------------------------------------

#' @keywords internal
#' @noRd
ilm_describe_num <- function(x, digits = 3,
                             gauss = c("index", "ks_d", "both", "none"),
                             probs = c(0, 0.5, 1), skew = FALSE, kurt = FALSE,
                             dispersion = TRUE, cap = 0.06, min_n = 20L) {
  gauss <- match.arg(gauss)
  if (!is.numeric(probs) || any(probs < 0 | probs > 1))
    stop("`probs` must be numeric values between 0 and 1", call. = FALSE)
  obs <- length(x); nn <- fnobs(x); na <- obs - nn
  xv <- x[is.finite(x)]
  s <- if (nn > 1) fsd(xv) else NA_real_
  m <- if (nn > 0) fmean(xv) else NA_real_
  z <- if (nn > 1 && is.finite(s) && s > 0) (xv - m) / s else numeric(0)

  out <- data.frame(
    obs = obs, n = nn, na = na,
    sum = ilm_rd(fsum(xv), digits), mean = ilm_rd(m, digits),
    sd = ilm_rd(s, digits), se = ilm_rd(s / sqrt(nn), digits),
    stringsAsFactors = FALSE)

  ## quantiles honour `digits` like every other numeric column -- they did not,
  ## which is why a large-scale variable printed unrounded percentiles
  if (length(probs)) {
    q <- if (nn > 0) ilm_rd(fquantile(xv, probs), digits)
         else rep(NA_real_, length(probs))
    qd <- as.data.frame(as.list(q), stringsAsFactors = FALSE)
    names(qd) <- ilm_prob_names(probs)
    out <- cbind(out, qd)
  }
  if (skew) out$skew <- ilm_rd(ilm_skew2(xv), digits)
  if (kurt) out$kurt <- ilm_rd(ilm_kurt2(xv), digits)

  out$p_zero <- ilm_rd(if (nn > 0) fmean(xv == 0) else NA_real_, digits)
  ## Variance-to-mean ratio: 1 is Poisson, above 1 points at family="nbinom".
  ## Reported for any non-negative integer variable, with no guessing about
  ## which of those are "really" counts. A rule keyed on the support starting
  ## near zero was tried and produced an inconsistency users would notice: x1
  ## (1-100) got a ratio while x2 (101-200) did not, though they are the same
  ## kind of variable. The ratio is only meaningful if you intend to model the
  ## variable as a count, which the user knows and the function cannot, so
  ## `dispersion = FALSE` turns the column off.
  if (dispersion) {
    is_count <- nn > 0 && all(abs(xv - round(xv)) < 1e-8) && all(xv >= 0)
    out$dispersion <- if (is_count && is.finite(m) && m > 0)
      ilm_rd(fvar(xv) / m, digits) else NA_real_
  }

  if (gauss != "none") {
    g <- ilm_gauss_assess(xv, min_n = min_n, cap = cap)
    if (gauss %in% c("index", "both")) out$gauss <- g$gauss
    if (gauss %in% c("ks_d", "both"))  out$ks_d  <- g$ks_d
    out$gauss_note <- g$note
  }
  out
}

## ---- factor / character ----------------------------------------------------

#' @keywords internal
#' @noRd
ilm_describe_cat <- function(x, digits = 3, sep = "_", top = 3L, rare_n = 5L) {
  obs <- length(x); nn <- fnobs(x); na <- obs - nn
  chr <- as.character(x); vals <- chr[!is.na(chr)]
  tb <- sort(table(vals), decreasing = TRUE); nu <- length(tb)

  out <- data.frame(
    obs = obs, n = nn, na = na,
    ## "" is not NA to R but is almost always missing to the analyst
    n_empty = sum(ilm_trimws(vals) == ""),
    n_unique = nu,
    ordered = is.ordered(x),
    p_max = ilm_rd(if (nu) max(tb) / sum(tb) else NA_real_, digits),
    ## the usual reason a factor model will not fit
    n_rare = sum(tb < rare_n),
    n_unused = if (is.factor(x)) sum(!(levels(x) %in% vals)) else 0L,
    ## tolower() stops on text that is not valid UTF-8, which has no case
    ## variant anyway
    case_variants = { u <- unique(vals); u <- u[validUTF8(u)]
                      sum(table(tolower(trimws(u))) > 1L) },
    counts_tb = paste(sprintf("%s%s%d", names(tb)[seq_len(min(top, nu))],
                              sep, as.integer(tb)[seq_len(min(top, nu))]),
                      collapse = ", "),
    stringsAsFactors = FALSE)
  z <- character(0)
  n_bad <- sum(!validUTF8(vals))
  if (n_bad > 0)
    z <- c(z, sprintf("%d %s not valid UTF-8 (another encoding?)", n_bad,
                      if (n_bad == 1L) "value" else "values"))
  if (out$n_empty > 0) z <- c(z, sprintf("%d empty strings (not NA)", out$n_empty))
  if (out$n_unused > 0) z <- c(z, sprintf("%d unused levels", out$n_unused))
  if (out$case_variants > 0) z <- c(z, sprintf("%d case/space variants", out$case_variants))
  if (out$n_rare > 0) z <- c(z, sprintf("%d rare levels (separation risk)", out$n_rare))
  if (is.finite(out$p_max) && out$p_max > 0.99) z <- c(z, "near-constant")
  out$note <- paste(utils::head(z, 2L), collapse = "; ")
  out
}

## ---- logical ---------------------------------------------------------------

#' @keywords internal
#' @noRd
ilm_describe_lgl <- function(x, digits = 3) {
  obs <- length(x); nn <- fnobs(x); na <- obs - nn
  p <- if (nn > 0) fmean(x) else NA_real_
  out <- data.frame(obs = obs, n = nn, na = na,
                    n_TRUE = sum(x, na.rm = TRUE), n_FALSE = sum(!x, na.rm = TRUE),
                    p_TRUE = ilm_rd(p, digits),
                    note = if (is.finite(p) && (p < 0.01 || p > 0.99))
                      "near-constant (separation risk)" else "",
                    stringsAsFactors = FALSE)
  out
}

## ---- Date / POSIXct --------------------------------------------------------

#' @keywords internal
#' @noRd
ilm_label_spacing <- function(md, secs) {
  if (!is.finite(md)) return(NA_character_)
  if (secs) {
    if (md == 1) return("1 sec"); if (md == 60) return("minutely")
    if (md == 3600) return("hourly"); if (md == 86400) return("daily")
    if (md == 604800) return("weekly")
    return(sprintf("%g secs", md))
  }
  if (isTRUE(all.equal(md, 1))) return("daily")
  if (isTRUE(all.equal(md, 7))) return("weekly")
  if (md >= 28 && md <= 31) return("monthly")
  if (md >= 90 && md <= 92) return("quarterly")
  if (md >= 365 && md <= 366) return("yearly")
  sprintf("%g days", md)
}

#' @keywords internal
#' @noRd
ilm_describe_time <- function(x, digits = 3) {
  obs <- length(x); nn <- fnobs(x); na <- obs - nn
  xv <- x[!is.na(x)]; u <- sort(unique(xv)); secs <- inherits(x, "POSIXt")
  d <- if (length(u) > 1L) as.numeric(diff(u), units = if (secs) "secs" else "days")
       else numeric(0)
  md <- if (length(d)) as.numeric(names(sort(table(round(d, 6)), decreasing = TRUE))[1]) else NA_real_
  n_gaps <- if (length(d) && is.finite(md) && md > 0) sum(d > 1.5 * md) else 0L
  ## calendar steps are not constant in days, so allow variation proportional
  ## to the usual step; real gaps are still caught
  regular <- length(d) > 0 && is.finite(md) &&
    all(abs(d - md) <= pmax(1e-6, 0.15 * md))

  out <- data.frame(
    obs = obs, n = nn, na = na, n_unique = length(u),
    start = if (length(u)) as.character(min(u)) else NA_character_,
    end   = if (length(u)) as.character(max(u)) else NA_character_,
    span_days = if (length(u) > 1L)
      ilm_rd(as.numeric(difftime(max(u), min(u), units = "days")), digits) else 0,
    spacing = ilm_label_spacing(md, secs), regular = regular, n_gaps = n_gaps,
    n_dup_times = nn - length(u),
    tz = if (secs) { t0 <- attr(x, "tzone")
      if (is.null(t0) || !nzchar(t0[1])) "unset" else t0[1] } else NA_character_,
    stringsAsFactors = FALSE)
  z <- character(0)
  if (!regular && n_gaps > 0)
    z <- c(z, sprintf("irregular, %d gaps (AR(1) needs regular spacing)", n_gaps))
  else if (!regular) z <- c(z, "irregular spacing")
  ## Repeated dates are the norm in long-format panel data -- many units share
  ## a date -- so this is described, not warned about. Duplicates only break
  ## AR(1) indexing WITHIN a group, which is visible when `by` is used.
  if (out$n_dup_times > 0)
    z <- c(z, sprintf("repeated dates: %d unique across %d rows (long format?)",
                      length(u), nn))
  if (secs && identical(out$tz, "unset")) z <- c(z, "timezone unset")
  out$note <- paste(utils::head(z, 2L), collapse = "; ")
  out
}

## ---- dispatch --------------------------------------------------------------
## (ilm_is_time(), which counts a duration as a time, is in R/ilm_time.R; a
## second, narrower copy here was never the one in use)

#' @keywords internal
#' @noRd
ilm_class_of <- function(v) {
  if (is.logical(v)) "logical"
  else if (ilm_is_time(v)) "time"
  else if (is.numeric(v)) "numeric"
  else "categorical"
}

ILM_CLASSES <- c("numeric", "categorical", "logical", "time")

#' Class-aware description of one variable
#'
#' Summarises a vector, or one column of a data frame, with statistics chosen
#' for its class. Numeric variables also get a gaussian agreement index and,
#' where that is low, a plain-language reason.
#'
#' The default output is deliberately narrow. Quartiles, skewness and kurtosis
#' are available on request, but `gauss` and `gauss_note` already say whether
#' they are worth asking for.
#'
#' A minimum or maximum outside what the column can hold, such as an age of
#' 999 or -1, is often a code for a missing value; [ilm_recode_errors()]
#' replaces it with `NA`.
#'
#' @param data A data frame, or a vector when `y` is `NULL`.
#' @param y Name of the column to summarise, or several names. With none,
#'   `data` is taken as the thing to describe: a bare vector, or a data frame
#'   whose columns are all of one kind. For a frame of mixed kinds use
#'   [ilm_describe_all()], which returns one table per kind.
#' @param by Optional character vector of grouping columns.
#' @param digits Rounding for numeric columns, quantiles included.
#' @param gauss One of `"index"` (the 0-1 agreement index), `"ks_d"` (the raw
#'   Kolmogorov distance behind it), `"both"`, or `"none"`.
#' @param probs Quantiles to report, as proportions. `c(0, 0.5, 1)` gives
#'   `p0`, `p50` and `p100`; any vector works and the column names follow it.
#' @param skew,kurt Add skewness and excess kurtosis: type 2, as SAS, SPSS and
#'   Excel print them (Joanes and Gill 1998), 0 on average for normal data.
#'   The same kurtosis decides `gauss_note`'s "heavy-tailed" and
#'   "light-tailed". When `gauss_note` says skewed, the median and quartiles
#'   describe the variable better than the mean and SD:
#'   `probs = c(0.25, 0.5, 0.75)`.
#' @param dispersion Add the variance-to-mean ratio for non-negative integer
#'   variables. Meaningful only if you intend to model the variable as a count.
#' @param rare_n Levels with fewer observations than this count as rare.
#' @param cap,min_n Control the `gauss` index; see [ilm_gauss_check()].
#' @param smd `FALSE` (the default), `TRUE`, or the name of a group. With `by`,
#'   adds `smd`: each group's standardised difference from a reference group,
#'   the first group for `TRUE` or the one named. A number's is the difference
#'   in means over the square root of the average of the two groups'
#'   variances (Austin 2009); a binary's, the difference in proportions over
#'   the square root of the average of their p(1 - p); a category of more
#'   than two levels, Yang and Dalton's (2012) multivariate difference, which
#'   has no sign. A date is compared as a number. Each group's uses its
#'   non-missing values. The reference group's own row has none.
#' @return A one-row data frame, or one row per group when `by` is used.
#' @references
#' Austin, P. C. (2009). Balance diagnostics for comparing the distribution of
#' baseline covariates between treatment groups in propensity-score matched
#' samples. Statistics in Medicine, 28(25), 3083-3107.
#'
#' Joanes, D. N. and Gill, C. A. (1998). Comparing measures of sample skewness
#' and kurtosis. Journal of the Royal Statistical Society: Series D (The
#' Statistician), 47(1), 183-189.
#'
#' Yang, D. and Dalton, J. E. (2012). A unified approach to measuring the
#' effect size between two groups using SAS. SAS Global Forum 2012, paper
#' 335-2012.
#' @seealso [ilm_describe_all()] for every column, [ilm_gauss_check()] for the
#'   index, [ilm_describe_na()] for missingness.
#' @examples
#' d <- ilm_sim()
#' ilm_describe(d, "score")
#' ilm_describe(d, "income", probs = c(0.1, 0.5, 0.9))
#' ilm_describe(d, "score", by = "grp")
#' ilm_describe(d, "score", by = "grp", smd = TRUE)
#' @inheritParams ilm_reduce
#' @export
ilm_describe <- function(data, y = NULL, by = NULL, digits = 3,
                          gauss = c("index", "ks_d", "both", "none"),
                          probs = c(0, 0.5, 1), skew = FALSE, kurt = FALSE,
                          dispersion = TRUE, rare_n = 5L,
                          cap = 0.06, min_n = 20L, smd = FALSE, subset = NULL, subset_negate = FALSE, subset_fixed = FALSE) {
  gauss <- match.arg(gauss)
  if (inherits(data, "ilm_anomaly") && (!is.null(subset) || isTRUE(subset_negate)))
    stop("`subset` cannot be applied to an ilm_anomaly() result: subset the data ",
         "before ilm_anomaly(), or use the rows its result gives", call. = FALSE)
  rsel <- ilm_select_rows(data, subset, subset_negate, subset_fixed)
  data <- rsel$data
  fin <- function(out) ilm_select_finish(out, data, rsel, NULL, y)
  ilm_smd_arg(smd, by)
  ## An ilm_anomaly() result is accepted directly: the flagged rows are what
  ## the user wants to look at next, and rebuilding that subset by hand from
  ## `row` is both a papercut and a chance to line the wrong rows up.
  if (inherits(data, "ilm_anomaly"))
    data <- ilm_from_anomaly(data, "ilm_describe", score = TRUE)
  ## the values are kept whole; `digits` is how they print
  one <- function(v) {
    if (is.logical(v)) ilm_describe_lgl(v, NULL)
    else if (ilm_is_time(v)) ilm_describe_time(v, NULL)
    else if (is.numeric(v)) ilm_describe_num(v, NULL, gauss, probs, skew, kurt, dispersion,
                                             cap, min_n)
    else ilm_describe_cat(v, NULL, rare_n = rare_n)
  }

  ## Resolving what is actually being described.
  ##
  ## Passing a whole data frame and no `y` used to hand the frame itself to
  ## one(), which is not logical, not a time and not numeric, so it fell
  ## through to the categorical branch and failed with "the condition has
  ## length > 1" for any frame of more than one column -- and returned nonsense
  ## rather than failing for a frame of exactly one.
  ##
  ## Columns of different kinds do not share a set of statistics. That is
  ## precisely why ilm_describe_all() returns one table per kind, so a mixed
  ## frame is an error that names that function rather than a mangled table.
  cols <- NULL
  if (!is.null(y)) {
    miss <- setdiff(y, names(data))
    if (length(miss))
      stop("column", if (length(miss) > 1L) "s" else "", " not found in the ",
           "data: ", paste(miss, collapse = ", "), ". Available: ",
           paste(utils::head(names(data), 12), collapse = ", "),
           if (length(names(data)) > 12L) ", ..." else "", call. = FALSE)
    if (length(y) > 1L) cols <- y else x <- data[[y]]
  } else if (is.data.frame(data)) {
    kinds <- unique(vapply(data, ilm_class_of, ""))
    if (length(kinds) > 1L)
      stop("`data` holds columns of more than one kind (",
           paste(sort(kinds), collapse = ", "), "), which do not share a set ",
           "of statistics. Name a column with `y`, or use ilm_describe_all(), ",
           "which returns one table per kind.", call. = FALSE)
    if (!ncol(data))
      stop("`data` has no columns to describe", call. = FALSE)
    cols <- names(data)
  } else x <- data

  if (!is.null(cols)) {
    rs <- lapply(cols, function(cn)
      ilm_describe(data, cn, by = by, digits = digits, gauss = gauss,
                   probs = probs, skew = skew, kurt = kurt,
                   dispersion = dispersion, rare_n = rare_n,
                   cap = cap, min_n = min_n, smd = smd))
    r <- do.call(rbind, rs)
    nrep <- nrow(r) / length(cols)
    return(fin(ilm_describe_result(cbind(variable = rep(cols, each = nrep), r,
                                         stringsAsFactors = FALSE),
                                   "ilm_describe", NULL, by, digits)))
  }

  if (is.null(by)) return(fin(ilm_describe_result(one(x), "ilm_describe", y, by, digits)))
  ilm_check_by(data, by)
  miss <- setdiff(by, names(data))
  if (length(miss))
    stop("`by` variable(s) not found in the data: ", paste(miss, collapse = ", "),
         call. = FALSE)
  g <- interaction(data[by], drop = TRUE)
  parts <- lapply(split(x, g), one)
  res <- cbind(setNames(data.frame(names(parts), stringsAsFactors = FALSE),
                        paste(by, collapse = ".")),
               do.call(rbind, parts), stringsAsFactors = FALSE)
  ## each group against the reference (R/ilm_smd.R); a date as a number
  ref <- if (!isFALSE(smd)) ilm_smd_reference(smd, names(parts))
  if (!is.null(ref)) {
    s <- ilm_smd(if (ilm_is_time(x)) ilm_time_number(x) else x, g, ref)
    res$smd <- as.numeric(s[names(parts)])
  }
  fin(ilm_describe_result(res, "ilm_describe", y, by, digits))
}

## `smd` is FALSE, TRUE or one group's name, and compares groups, so it
## needs `by`
#' @keywords internal
#' @noRd
ilm_smd_arg <- function(smd, by) {
  if (isFALSE(smd)) return(invisible())
  if (!(isTRUE(smd) || (is.character(smd) && length(smd) == 1L && !is.na(smd))))
    stop("`smd` must be FALSE, TRUE, or the name of one group.", call. = FALSE)
  if (!length(by))
    stop("`smd` compares groups with each other, so it needs `by`.", call. = FALSE)
  invisible()
}

## the reference group: the first for TRUE, else the one named
#' @keywords internal
#' @noRd
ilm_smd_reference <- function(smd, groups) {
  if (isTRUE(smd)) return(groups[1L])
  if (!smd %in% groups)
    stop("`smd` names a group that is not in the data: '", smd, "'. The groups: ",
         paste(utils::head(groups, 12), collapse = ", "),
         if (length(groups) > 12L) ", ..." else "", ".", call. = FALSE)
  smd
}

## A variable with one value tells you nothing a table of statistics can show,
## and it will break a model matrix. It is reported separately rather than
## padding every other section with a row of NAs.
#' @keywords internal
#' @noRd
ilm_constant_tbl <- function(data, cols) {
  if (!length(cols)) return(NULL)
  out <- do.call(rbind, lapply(cols, function(cn) {
    v <- data[[cn]]; nn <- fnobs(v); u <- unique(v[!is.na(v)])
    data.frame(variable = cn, class = ilm_class_of(v), obs = length(v),
               n = nn, na = length(v) - nn,
               value = if (length(u)) as.character(u[1]) else NA_character_,
               stringsAsFactors = FALSE)
  }))
  out
}

#' Describe every column of a data frame
#'
#' Applies [ilm_describe()] to each column and returns one table per class, so
#' columns with different summaries are not forced into a common shape.
#'
#' Constant columns are split into their own `constant` section rather than
#' padding every other table with rows of `NA`: a variable with one value tells
#' you nothing a statistic can show, and it will break a model matrix.
#'
#' @inheritParams ilm_describe
#' @inheritParams ilm_reduce
#' @param cols Columns to describe. A character vector of names, a regular
#'   expression, a predicate function such as `is.numeric`, or `NULL` for all
#'   of them -- see [ilm_selection]. `by` columns are never among them, and the choice is made
#'   among the columns `class` allows.
#' @param class `"all"`, or one or more of `"numeric"`, `"categorical"`,
#'   `"logical"`, `"time"`.
#' @return A named list of data frames, one per class present, plus `constant`
#'   when any column has a single value; a single data frame if only one
#'   section results.
#' @seealso [ilm_frame_issues()] for problems belonging to the data frame as a
#'   whole: duplicated, aliased, nested and collinear columns.
#' @examples
#' d <- ilm_sim()
#' ilm_describe_all(d)
#' ilm_describe_all(d, class = "numeric", skew = TRUE)
#' @export
ilm_describe_all <- function(data, by = NULL, cols = NULL, digits = 3,
                              gauss = c("index", "ks_d", "both", "none"),
                              probs = c(0, 0.5, 1), skew = FALSE, kurt = FALSE,
                              dispersion = TRUE, class = "all", rare_n = 5L,
                              cap = 0.06, min_n = 20L, smd = FALSE, cols_negate = FALSE, cols_fixed = FALSE, subset = NULL,
                              subset_negate = FALSE, subset_fixed = FALSE) {
  gauss <- match.arg(gauss)
  if (inherits(data, "ilm_anomaly") && (!is.null(subset) || isTRUE(subset_negate)))
    stop("`subset` cannot be applied to an ilm_anomaly() result: subset the data ",
         "before ilm_anomaly(), or use the rows its result gives", call. = FALSE)
  rsel <- ilm_select_rows(data, subset, subset_negate, subset_fixed)
  data <- rsel$data
  ilm_smd_arg(smd, by)
  ## An ilm_anomaly() result is accepted directly: the flagged rows are what
  ## the user wants to look at next, and rebuilding that subset by hand from
  ## `row` is both a papercut and a chance to line the wrong rows up.
  if (inherits(data, "ilm_anomaly"))
    data <- ilm_from_anomaly(data, "ilm_describe_all", score = TRUE)
  bad <- setdiff(class, c("all", ILM_CLASSES))
  if (length(bad))
    stop("unknown `class`: ", paste(sQuote(bad), collapse = ", "),
         ". Options are ", paste(sQuote(c("all", ILM_CLASSES)), collapse = ", "),
         ".", call. = FALSE)
  ilm_check_by(data, by)
  miss <- setdiff(by, names(data))
  if (length(miss))
    stop("`by` variable(s) not found in the data: ", paste(miss, collapse = ", "),
         call. = FALSE)

  ## grouping variables describe the split, so they are not also described
  cand <- setdiff(names(data), by)
  cl <- vapply(data[cand], ilm_class_of, "")
  ## `cols` chooses among the columns `class` allows, and `cols_negate`
  ## leaves its choice out of those
  if (!is.null(cols) || isTRUE(cols_negate)) {
    elig <- if (identical(class, "all")) cand else cand[cl %in% class]
    cand <- ilm_resolve_cols(data, cols, exclude = by, eligible = elig, negate = cols_negate,
                             fixed = cols_fixed)
    cl <- cl[cand]
  }
  ## constancy is judged on the whole column, so the same variables are set
  ## aside whether or not `by` is used
  is_const <- vapply(data[cand], function(v) fndistinct(v) <= 1L, TRUE)
  const <- ilm_constant_tbl(data, cand[is_const])
  cand <- cand[!is_const]; cl <- cl[!is_const]

  want <- if (identical(class, "all")) unique(cl) else intersect(class, unique(cl))
  ## the columns the result covers: those described, and the constant ones
  ## when they are shown
  used <- c(cand[cl %in% want], if (identical(class, "all") && !is.null(const)) const$variable)
  fin <- function(out) ilm_select_finish(out, data, rsel, cols, used, by, cols_negate, cols_fixed)
  res <- lapply(want, function(k) {
    cols <- cand[cl == k]
    if (!length(cols)) return(NULL)
    rs <- lapply(cols, function(cn)
      ilm_describe(data, cn, by = by, digits = digits, gauss = gauss,
                    probs = probs, skew = skew, kurt = kurt,
                    dispersion = dispersion, rare_n = rare_n,
                    cap = cap, min_n = min_n, smd = smd))
    r <- do.call(rbind, rs)
    nrep <- nrow(r) / length(cols)
    cbind(variable = rep(cols, each = nrep), r, stringsAsFactors = FALSE)
  })
  names(res) <- want
  res <- res[!vapply(res, is.null, TRUE)]
  ## The constant section is attached only for class = "all". Asking for one
  ## class should return that data frame, not a list of it plus something else:
  ## a return type that changes depending on whether the data happen to contain
  ## a constant column is a trap.
  if (identical(class, "all")) {
    if (!is.null(const)) res$constant <- const
  } else if (!is.null(const)) {
    message("ilm_describe_all: ", nrow(const),
            " constant column(s) set aside (", paste(const$variable, collapse = ", "),
            "); use class = \"all\" to see them")
  }
  if (length(res) == 1L) {
    return(fin(ilm_describe_result(res[[1L]], "ilm_describe", NULL, by, digits)))
  }
  attr(res, "digits") <- digits
  if (length(by)) attr(res, "by") <- by
  fin(ilm_as_result(res, "ilm_describe_all"))
}

## ---- whole-frame issues ----------------------------------------------------

#' Problems that belong to the data frame as a whole
#'
#' Constant columns, identifier-like columns, duplicated columns, near-perfect
#' collinearity, categorical columns that carry the same grouping, and columns
#' that are an exact combination of others. These cannot live in a
#' per-variable table because they are properties of the data frame as a
#' whole, and most of them break a model matrix: a model given two columns
#' that say the same thing cannot tell their effects apart.
#'
#' A continuous variable is unique per row by construction, so uniqueness is
#' reported as identifier-like only for discrete-valued columns.
#'
#' Categorical columns (factors, characters and logicals) are compared in
#' pairs. Two whose levels match one to one are `aliased_factors`: the same
#' grouping under different labels. One whose every level falls within a
#' single level of the other is `nested`, as patients within clinics are; that
#' is reported only when at least half of the finer column's levels have two or
#' more rows, since a level seen once sits inside one level of anything.
#' Otherwise a pair is `redundant_categories` when Cramer's V reaches `v_cut`:
#' nearly every level of one predicts a level of the other.
#'
#' Pairs are not the whole story: a column can be an exact combination of
#' several others (a total and its parts, a numeric code fixed by a factor's
#' levels) with no pair looking alike. So the numeric and categorical columns
#' are also put into one model matrix, with an intercept and treatment
#' contrasts, on the rows complete across them, and each column the matrix
#' cannot separate from the rest is reported as `rank_deficient` with what it
#' is a combination of. So that each problem is reported once, columns already
#' reported as constant, identifier-like, duplicated or aliased are left out of
#' that matrix, and a nested pair is not reported again from it.
#'
#' @param data A data frame.
#' @param cor_cut Absolute correlation at or above which a numeric pair is
#'   reported as collinear.
#' @param v_cut Cramer's V at or above which a pair of categorical columns is
#'   reported as redundant.
#' @return A data frame with `issue`, `columns`, `detail` and `remedy`, one row
#'   per problem; zero rows when nothing is found.
#' @references
#' Cramer, H. (1946). Mathematical Methods of Statistics. Princeton University
#' Press.
#' @examples
#' ilm_frame_issues(ilm_sim())
#'
#' d <- data.frame(a = rnorm(50), b = rnorm(50),
#'                 clinic = rep(c("x", "y"), each = 25),
#'                 ward = rep(c("x1", "x2", "y1", "y2"), c(12, 13, 12, 13)))
#' d$total <- d$a + d$b
#' ilm_frame_issues(d)
#' @inheritParams ilm_reduce
#' @export
ilm_frame_issues <- function(data, cor_cut = 0.999, v_cut = 0.95, cols = NULL,
                             cols_negate = FALSE, cols_fixed = FALSE, subset = NULL, subset_negate = FALSE, subset_fixed = FALSE) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  ## the rows first, then the columns; the checks run on what is left
  rsel <- ilm_select_rows(data, subset, subset_negate, subset_fixed)
  data0 <- rsel$data
  use <- ilm_resolve_cols(data0, cols, negate = cols_negate, fixed = cols_fixed)
  data <- data0[use]
  fin <- function(out) ilm_select_finish(out, data0, rsel, cols, use, cols_negate = cols_negate,
                                         cols_fixed = cols_fixed)
  for (a in c("cor_cut", "v_cut")) {
    v <- get(a)
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0 || v > 1)
      stop("`", a, "` must be a single number in (0, 1]", call. = FALSE)
  }
  out <- list()
  ## each row's words for a person
  add <- function(issue, cols, detail, remedy) {
    out[[length(out) + 1L]] <<- data.frame(issue = issue, columns = cols,
                                           detail = detail, remedy = remedy,
                                           stringsAsFactors = FALSE)
  }
  n <- nrow(data)
  ## columns the model-matrix check below leaves out, because a problem
  ## already reported here would only be reported again there
  set_aside <- character(0)
  for (cn in names(data)) {
    v <- data[[cn]]; nu <- fndistinct(v)
    ## a continuous variable is unique per row by construction, so uniqueness
    ## only signals an identifier for discrete-valued columns
    discrete_like <- is.character(v) || is.factor(v) ||
      (is.numeric(v) && !is.logical(v) &&
         all(abs(v[!is.na(v)] - round(v[!is.na(v)])) < 1e-8))
    if (nu <= 1L) {
      add("constant", cn, "zero variance; breaks the model matrix",
          "Leave it out: a column with one value cannot explain anything.")
      set_aside <- c(set_aside, cn)
    } else if (discrete_like && nu == sum(!is.na(v)) && nu == n) {
      add("id_like", cn, "unique per row; a key, not a predictor",
          "Leave it out of the predictors, and keep it to identify or join rows.")
      set_aside <- c(set_aside, cn)
    }
  }
  nm <- names(data); dups <- character(0)
  if (length(nm) > 1L) for (i in seq_len(length(nm) - 1L)) for (j in (i + 1L):length(nm))
    if (identical(unname(data[[i]]), unname(data[[j]]))) {
      add("duplicate_columns", paste(nm[i], nm[j], sep = " = "), "identical values",
          "Keep one of the two.")
      set_aside <- c(set_aside, nm[j])
      dups <- c(dups, paste(nm[i], nm[j]))
    }
  num <- names(data)[vapply(data, function(v) is.numeric(v) && !is.logical(v), TRUE)]
  if (length(num) > 1L) {
    cm <- suppressWarnings(stats::cor(data[num], use = "pairwise.complete.obs"))
    for (i in seq_len(length(num) - 1L)) for (j in (i + 1L):length(num)) {
      r <- cm[i, j]
      ## an identical pair is reported once, as a duplicate
      if (is.finite(r) && abs(r) >= cor_cut &&
          !paste(num[i], num[j]) %in% dups)
        add("collinear", paste(num[i], num[j], sep = " ~ "),
            sprintf("r = %.4f; coefficients will be unstable", r),
            paste0("Keep one, or combine them into one column (their average, ",
                   "or a score from ilm_reduce()); their separate effects ",
                   "cannot be estimated reliably."))
    }
  }
  cat_res <- ilm_frame_categories(data, set_aside, v_cut, add)
  ilm_frame_rank(data, setdiff(names(data), c(set_aside, cat_res$aliased)), add,
                 cat_res$nested)
  if (!length(out))
    return(fin(ilm_as_result(data.frame(issue = character(0), columns = character(0),
                                        detail = character(0), remedy = character(0),
                                        stringsAsFactors = FALSE), "ilm_frame_issues")))
  fin(ilm_as_result(do.call(rbind, out), "ilm_frame_issues"))
}

## Pairs of categorical columns: the same grouping relabelled, one grouping
## inside another, or two groupings that nearly determine each other. Returns
## the second column of each aliased pair, which the model-matrix check leaves
## out, and the nested pairs, which it does not report again.
#' @keywords internal
#' @noRd
ilm_frame_categories <- function(data, skip, v_cut, add) {
  cats <- names(data)[vapply(data, function(v)
    is.factor(v) || is.character(v) || is.logical(v), TRUE)]
  cats <- setdiff(cats, skip)
  aliased <- character(0); nested <- list()
  res <- function() list(aliased = unique(aliased), nested = nested)
  if (length(cats) < 2L) return(res())
  codes <- lapply(data[cats], function(v) as.integer(factor(v)))
  for (i in seq_len(length(cats) - 1L)) for (j in (i + 1L):length(cats)) {
    ## a relabelled copy would only repeat what its original reports
    if (any(cats[c(i, j)] %in% aliased)) next
    ok <- !is.na(codes[[i]]) & !is.na(codes[[j]])
    a <- codes[[i]][ok]; b <- codes[[j]][ok]
    la <- length(unique(a)); lb <- length(unique(b))
    if (la < 2L || lb < 2L) next
    cells <- length(unique((b - 1) * max(a) + a))
    A <- cats[i]; B <- cats[j]
    if (cells == la && cells == lb) {
      add("aliased_factors", paste(A, B, sep = " = "),
          sprintf("the same %d groups under different labels", la),
          paste0("Keep one of the two; a model cannot tell their effects ",
                 "apart."))
      aliased <- c(aliased, B)
      next
    }
    ## a nests in b when each level of a meets one level of b; singletons
    ## would make that true of anything, so most levels must repeat
    fine <- if (cells == la) "a" else if (cells == lb) "b" else ""
    if (nzchar(fine)) {
      x <- if (fine == "a") a else b
      if (mean(tabulate(x) >= 2L) >= 0.5) {
        inner <- if (fine == "a") A else B; outer <- if (fine == "a") B else A
        add("nested", paste(inner, "in", outer),
            sprintf("each of the %d levels of %s falls within one of the %d levels of %s",
                    if (fine == "a") la else lb, inner,
                    if (fine == "a") lb else la, outer),
            sprintf(paste0("Expected for grouping factors: as random effects, ",
                           "write (1 | %s/%s). As fixed effects keep one of ",
                           "them, since %s absorbs every difference between ",
                           "levels of %s."), outer, inner, inner, outer))
        nested[[length(nested) + 1L]] <- c(inner, outer)
      }
      next
    }
    V <- ilm_cramer_v(a, b)
    if (is.finite(V) && V >= v_cut)
      add("redundant_categories", paste(A, B, sep = " ~ "),
          sprintf("Cramer's V = %.3f; each largely predicts the other", V),
          paste0("Keep one, or cross them into one factor with interaction(); ",
                 "their separate effects are barely distinguishable."))
  }
  res()
}

## Cramer's V from two integer codings, without a continuity correction.
#' @keywords internal
#' @noRd
ilm_cramer_v <- function(a, b) {
  O <- table(a, b); N <- sum(O)
  E <- outer(rowSums(O), colSums(O)) / N
  k <- min(dim(O)) - 1L
  if (k < 1L || N == 0) return(NA_real_)
  sqrt(sum((O - E)^2 / E) / (N * k))
}

## Columns that are an exact linear combination of others, found by a pivoted
## QR of the model matrix of `use`: numeric columns standardised, categorical
## ones in treatment contrasts, an intercept, and the rows complete across
## them. Reports each column it cannot separate, and what that column is made
## of.
#' @keywords internal
#' @noRd
ilm_frame_rank <- function(data, use, add, nested = list()) {
  use <- use[vapply(data[use], function(v)
    is.numeric(v) || is.factor(v) || is.character(v) || is.logical(v), TRUE)]
  if (length(use) < 2L) return(invisible(NULL))
  cats <- use[!vapply(data[use], function(v) is.numeric(v) && !is.logical(v), TRUE)]
  cc <- stats::complete.cases(data[use])
  cols <- list(); owner <- character(0)
  for (v in use) {
    x <- data[[v]][cc]
    if (is.numeric(x) && !is.logical(x)) {
      cols[[length(cols) + 1L]] <- as.double(x); owner <- c(owner, v)
    } else {
      f <- factor(x)
      for (l in levels(f)[-1L]) {
        cols[[length(cols) + 1L]] <- as.double(f == l); owner <- c(owner, v)
      }
    }
  }
  if (!length(cols)) return(invisible(NULL))
  X <- cbind(1, do.call(cbind, cols))
  owner <- c("(Intercept)", owner)
  if (sum(cc) <= ncol(X)) {
    add("rank_unchecked", paste(use, collapse = ", "),
        sprintf("%d complete rows for %d model-matrix columns", sum(cc), ncol(X)),
        paste0("Too few complete rows to judge; check a subset of columns, ",
               "as in ilm_frame_issues(data[c(\"x\", \"y\")])."))
    return(invisible(NULL))
  }
  ## scaled so the tolerance means the same thing for every column
  s <- apply(X, 2L, function(z) sqrt(mean((z - mean(z))^2)))
  s[1L] <- 1; s[!is.finite(s) | s == 0] <- 1
  Xs <- sweep(X, 2L, s, "/")
  q <- qr(Xs, tol = 1e-7)
  if (q$rank == ncol(X)) return(invisible(NULL))
  keep <- sort(q$pivot[seq_len(q$rank)])
  dep <- q$pivot[(q$rank + 1L):ncol(X)]
  qk <- qr(Xs[, keep, drop = FALSE])
  done <- character(0)
  for (j in dep) {
    v <- owner[j]
    if (v %in% done) next
    if (v == "(Intercept)") next
    ## every dependent column of this variable, and what they are built from
    js <- dep[owner[dep] == v]
    B <- qr.coef(qk, Xs[, js, drop = FALSE])
    B <- as.matrix(B)
    used <- keep[apply(abs(B) > 1e-6, 1L, any)]
    from <- setdiff(unique(owner[used]), c("(Intercept)", v))
    done <- c(done, v)
    ## a factor inside another is a combination of it by construction, and
    ## was reported as nested already
    if (any(vapply(nested, function(pr) setequal(pr, c(v, from)), TRUE))) next
    if (!length(from)) {
      add("rank_deficient", v, "constant on the rows complete across these columns",
          "Leave it out, or check why it only varies where other columns are missing.")
      next
    }
    lab <- function(x) ifelse(x %in% cats, paste0("the levels of ", x), x)
    add("rank_deficient", paste(v, "~", paste(from, collapse = " + ")),
        sprintf("%s %s an exact linear combination of %s", lab(v),
                if (v %in% cats) "are" else "is", ilm_and(lab(from))),
        sprintf(paste0("Leave out %s or one of the columns it is built from; ",
                       "with all of them a model has no unique solution."), v))
  }
  invisible(NULL)
}
