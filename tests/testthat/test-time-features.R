## Date and date-time columns in the reductions: used as numbers, not dropped.

time_data <- function(n = 120, seed = 1) {
  set.seed(seed)
  start <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  d <- data.frame(
    x = rnorm(n),
    grp = factor(sample(c("a", "b", "c"), n, TRUE)),
    ## two groups that differ only in WHEN: the first half early in the
    ## period, the second half late
    when = start + c(sort(runif(n / 2, 0, 60)), sort(runif(n / 2, 300, 360))) * 86400,
    stringsAsFactors = FALSE)
  d$day <- as.Date(d$when)
  d
}

test_that("a date column is used as elapsed time rather than dropped", {
  d <- time_data()
  expect_message(r <- ilm_reduce(d[c("x", "grp", "day")]), "day -> day_elapsed")
  expect_true("day_elapsed" %in% r$var_contrib$variable)
  expect_identical(r$method, "famd")
  ## asking for the old behaviour still drops it
  expect_message(r0 <- ilm_reduce(d[c("x", "grp", "day")], time = "drop"),
                 "dropping date/time")
  expect_false(any(grepl("^day", r0$var_contrib$variable)))
})

test_that("elapsed time is the time since the earliest value, in days", {
  d <- time_data()
  f <- illumex:::ilm_time_features(d$day, "day")
  expect_named(f, "day_elapsed")
  expect_equal(f$day_elapsed, as.numeric(d$day - min(d$day)))
  g <- illumex:::ilm_time_features(d$when, "when")
  expect_equal(g$when_elapsed,
               as.numeric(difftime(d$when, min(d$when), units = "days")))
  ## a duration is already a quantity
  h <- illumex:::ilm_time_features(as.difftime(c(1, 36), units = "hours"), "dur")
  expect_equal(h$dur, c(1, 36) / 24)
})

test_that("a cycle is added only where the data cover two of it", {
  start <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  ## ten days of timestamps: two days of hours, but not two weeks
  ten <- start + seq(0, 10 * 86400, length.out = 200)
  f <- illumex:::ilm_time_features(ten, "t", cycles = TRUE)
  expect_true(all(c("t_hour_sin", "t_hour_cos") %in% names(f)))
  expect_false(any(grepl("wday|yday", names(f))))
  ## three years of dates: weekday and season both, and no hour for a Date
  yrs <- as.Date("2021-01-01") + 0:(3 * 365)
  g <- illumex:::ilm_time_features(yrs, "d", cycles = TRUE)
  expect_true(all(c("d_wday_sin", "d_yday_cos") %in% names(g)))
  expect_false(any(grepl("hour", names(g))))
  ## a cycle that never varies adds nothing: every timestamp at midnight
  mid <- as.POSIXct(as.Date("2024-01-01") + 0:40, tz = "UTC")
  expect_false(any(grepl("hour", names(illumex:::ilm_time_features(mid, "m", cycles = TRUE)))))
  ## and the pair puts the ends of a cycle together: 23:30 is near 00:30
  a <- illumex:::ilm_time_features(start + c(0.5, 23.5) * 3600 + c(0, 3 * 86400), "q",
                                   cycles = TRUE)
  expect_lt(abs(a$q_hour_cos[1] - a$q_hour_cos[2]), 1e-8)
})

test_that("a clustering can separate rows by when they happened", {
  d <- time_data()
  ## x and z are continuous noise; only the date separates the halves. (A
  ## factor would not do as the noise: its levels are discrete clouds in the
  ## reduced space, and k-means splits on those first.)
  set.seed(2); d$z <- rnorm(nrow(d))
  cl <- suppressMessages(ilm_cluster(d[c("x", "z", "day")], k = 2, B = 10, seed = 1))
  lab <- cl$ind_cluster$cluster
  truth <- rep(1:2, each = nrow(d) / 2)
  agree <- max(mean(lab == truth), mean(lab == 3 - truth))
  expect_gt(agree, 0.9)
  ## and the variable check can say so
  vc <- ilm_var_contrib(cl, d[c("x", "z", "day")], B = 49)
  expect_identical(vc$type[vc$variable == "day"], "date")
  expect_identical(vc$variable[1], "day")
  ## a few of its columns print as the table they are
  expect_output(print(vc[, c("variable", "type")]), "day")
})

test_that("the GLRM route and the whole profile take dates too", {
  d <- time_data(n = 80)
  expect_message(g <- ilm_glrm(d[c("x", "grp", "when")], rank = 2, maxit = 50),
                 "when -> when_elapsed")
  expect_true("when_elapsed" %in% names(g$encoding$blocks))
  p <- suppressMessages(ilm_profile(d[c("x", "grp", "when")], k = 2, B = 5,
                                    time = "cycles", var_contrib = FALSE))
  expect_s3_class(p, "ilm_profile")
})

test_that("a cycle is described where the cluster stands out, wrapping if it must", {
  arc <- illumex:::ilm_time_arc
  set.seed(3)
  start <- as.POSIXct("2024-01-01", tz = "UTC")
  ## the first 200 rows at night, the other 200 by day
  h <- c(runif(200, 22, 28) %% 24, runif(200, 8, 18))
  x <- start + sample(0:400, 400, TRUE) * 86400 + h * 3600
  m <- rep(c(TRUE, FALSE), each = 200)
  expect_identical(arc(x, m, "hour")$text, "100% between 22:00 and 03:59 (all rows 50%)")
  ## where a cluster is absent is as much a description as where it is
  expect_identical(arc(x, !m, "hour")$text, "none between 22:00 and 03:59 (all rows 50%)")
  ## and rows no different from the rest are said to be so
  expect_identical(arc(x, sample(m), "wday")$text, "much as all rows")
  ## month-end is the end of each month, whatever its length
  cal <- as.Date("2022-01-01") + 0:(3 * 365 - 1); l <- as.POSIXlt(cal)
  end <- l$mday > illumex:::ilm_month_days(l) - 3
  y <- c(sample(cal[end], 150, TRUE), sample(cal[!end], 250, TRUE))
  a <- arc(y, rep(c(TRUE, FALSE), c(150, 250)), "mday")
  expect_match(a$text, "^9[0-9]% from the 2[0-9]th to the 31st of the month")
  expect_gt(a$share - a$share_all, 0.5)
})

test_that("the descriptions name a date in time, not as the number it was used as", {
  d <- time_data()
  set.seed(2); d$z <- rnorm(nrow(d))
  p <- suppressMessages(ilm_profile(d[c("x", "z", "day")], k = 2, B = 5, seed = 1,
                                    var_contrib = FALSE))
  tv <- paste(p$characterization$top_variables, collapse = " ")
  expect_match(tv, "(later|earlier) day")
  expect_no_match(tv, "day_elapsed")
  ## and the sentence ends in dates: the two halves of the year, apart
  s <- paste(p$summary, collapse = " ")
  expect_match(s, "day: middle half 2024-0[1-3]-[0-9]{2} to 2024-0[1-3]")
  expect_match(s, "day: middle half 2024-1[0-2]-[0-9]{2} to 2024-1[0-2]")
  expect_identical(unique(p$time$aspect), "time line")
})

test_that("a clustering on a cycle is described, and scored, on that cycle", {
  set.seed(1); n <- 300
  cl <- rep(1:2, each = n / 2)
  cal <- as.Date("2022-01-01") + 0:(3 * 365 - 1)
  wk <- as.POSIXlt(cal)$wday %in% c(0, 6)
  d <- data.frame(amount = rnorm(n, 1.5 * (cl == 2)),
                  items = rpois(n, 3 + 2 * (cl == 2)),
                  channel = factor(sample(c("web", "shop"), n, TRUE)),
                  day = c(sample(cal[wk], n / 2, TRUE), sample(cal[!wk], n / 2, TRUE)))
  p <- suppressMessages(ilm_profile(d, k = 2, B = 5, seed = 1, time = "cycles",
                                    top_n_vars = 3, var_contrib_B = 49))
  s <- paste(p$summary, collapse = " ")
  expect_match(s, "day: day of the week")
  expect_no_match(s, "_sin|_cos")
  expect_match(s, "[0-9]+% on Sat-Sun \\(all rows 50%\\)")
  expect_match(s, "only [0-9]% on Sat-Sun \\(all rows 50%\\)")
  ## scored on the time line alone, the date that defines this clustering
  ## would have looked like noise
  expect_identical(p$var_contrib$type[p$var_contrib$variable == "day"],
                   "date: day of the week")
  expect_lt(p$var_contrib$p[p$var_contrib$variable == "day"], 0.05)
})

test_that("two cycles of one date are named together", {
  set.seed(1); n <- 300
  cl <- rep(1:2, each = n / 2)
  cal <- as.Date("2022-01-01") + 0:(3 * 365 - 1); l <- as.POSIXlt(cal)
  me <- l$mday > illumex:::ilm_month_days(l) - 3
  when <- as.POSIXct(c(sample(cal[me], n / 2, TRUE), sample(cal[!me], n / 2, TRUE)),
                     tz = "UTC") + c(runif(n / 2, 20, 24), runif(n / 2, 8, 17)) * 3600
  d <- data.frame(amount = rnorm(n, 1.5 * (cl == 2)), when = when)
  p <- suppressMessages(ilm_profile(d, k = 2, B = 5, seed = 1, time = "cycles",
                                    top_n_vars = 3, var_contrib = FALSE))
  s <- paste(p$summary, collapse = " ")
  expect_match(s, "when: time of day and day of the month")
  expect_match(s, "between 20:00 and 23:59")
  expect_match(s, "to the 31st of the month")
})

test_that("a cycle is used only when the other columns vary with it", {
  set.seed(1); n <- 300
  cl <- rep(1:2, each = n / 2)
  cal <- as.Date("2022-01-01") + 0:(3 * 365 - 1)
  wk <- as.POSIXlt(cal)$wday %in% c(0, 6)
  d <- data.frame(amount = rnorm(n, 1.5 * (cl == 2)),
                  day = c(sample(cal[wk], n / 2, TRUE), sample(cal[!wk], n / 2, TRUE)))
  expect_message(r <- ilm_reduce(d, time = "cycles"), "vary with its day of the week")
  expect_true(all(c("day_wday_sin", "day_wday_cos") %in% r$var_contrib$variable))
  expect_false(any(grepl("mday|yday", r$var_contrib$variable)))
  ## rows busier on weekdays with nothing else following it: a rhythm in
  ## the volume alone is left out, since the clustering would split on it
  d$day <- cal[sample.int(length(cal), n, TRUE, prob = ifelse(wk, 0.6, 1))]
  expect_message(r2 <- ilm_reduce(d, time = "cycles"), "do not vary with")
  expect_false(any(grepl("_(sin|cos)$", r2$var_contrib$variable)))
  ## a date on its own has nothing to test its cycles against
  expect_message(r3 <- ilm_reduce(d["day"], time = "cycles"), "no other column")
  expect_true(any(grepl("_wday_", r3$var_contrib$variable)))
  ## and the test leaves the caller's random numbers where they were
  set.seed(5); a <- runif(1)
  set.seed(5); suppressMessages(ilm_reduce(d, time = "cycles")); b <- runif(1)
  expect_identical(a, b)
})

test_that("the separation the rhythm test uses is ilm_var_sep's, all at once", {
  set.seed(1); n <- 200
  b <- sample(0:9, n, TRUE)
  d <- data.frame(a = rnorm(n) + (b > 7), c = factor(sample(letters[1:4], n, TRUE)),
                  l = sample(c(TRUE, FALSE), n, TRUE),
                  e = ifelse(b > 5, "x", sample(c("x", "y"), n, TRUE)))
  pr <- illumex:::ilm_sep_prep(d)
  expect_equal(illumex:::ilm_sep_all(pr, b),
               unname(vapply(d, illumex:::ilm_var_sep, 0, cl = b)), tolerance = 1e-10)
})

test_that("a column that is a cycle already does not vouch for it", {
  set.seed(1); n <- 300
  cal <- as.Date("2022-01-01") + 0:(3 * 365 - 1)
  ## the first column has too many levels to be tested against, so the names
  ## of the columns tested are not simply the first few
  d <- data.frame(g = factor(sample(1:60, n, TRUE)), amount = rnorm(n),
                  day = sample(cal, n, TRUE))
  d$month <- factor(format(d$day, "%m"))
  d$weekend <- as.POSIXlt(d$day)$wday %in% c(0, 6)
  expect_message(r <- ilm_reduce(d, time = "cycles"),
                 "weekend is its day of the week already; month is its time of year already")
  expect_false(any(grepl("_(sin|cos)$", r$var_contrib$variable)))
})

test_that("a date's columns never take a name the data already use", {
  set.seed(1); n <- 120
  d <- data.frame(amount = rnorm(n), day = as.Date("2024-01-01") + sample(0:300, n, TRUE))
  ## someone's own elapsed count, kept beside the date, and unrelated to it
  d$day_elapsed <- rnorm(n)
  expect_message(r <- ilm_reduce(d), "day -> day_elapsed.1")
  expect_true(all(c("day_elapsed", "day_elapsed.1") %in% r$var_contrib$variable))
  ## and each is described as what it is
  expect_identical(illumex:::ilm_time_aspect("day_elapsed.1", r$time),
                   list(column = "day", aspect = "elapsed"))
  expect_null(illumex:::ilm_time_aspect("day_elapsed", r$time))
})
