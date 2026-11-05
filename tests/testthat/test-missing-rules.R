# Tests of ilm_check_missing() written from its specification: hand-worked
# screen effects, level-order invariance of the outcome effect, dates in the
# screen and the outcome test, the sandwich by hand, one row per unit, the NA
# cases, the field order and the verdict lines.

# Silences narration, and the several-rows-per-unit warning (tested on its
# own below); any other warning still shows.
quiet <- function(expr) {
  withCallingHandlers(suppressMessages(expr), warning = function(w) {
    if (grepl("several rows per unit", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

fields <- c("variables", "patterns", "associations", "outcome_test",
            "monotone", "n", "n_complete", "y", "min_effect", "unit",
            "n_units", "verdict")

# Cluster sandwich written out row by row and unit by unit, for comparison:
# B = sum_i mu_i (1 - mu_i) x_i x_i';  M = sum_u s_u s_u',
# s_u = sum over the unit's rows of x_i (z_i - mu_i);  V = B^-1 M B^-1.
hand_cluster_vcov <- function(X, z, mu, u) {
  k <- ncol(X)
  B <- matrix(0, k, k)
  M <- matrix(0, k, k)
  for (i in seq_len(nrow(X))) B <- B + mu[i] * (1 - mu[i]) * tcrossprod(X[i, ])
  for (g in unique(u)) {
    s <- rep(0, k)
    for (i in which(u == g)) s <- s + X[i, ] * (z[i] - mu[i])
    M <- M + tcrossprod(s)
  }
  solve(B) %*% M %*% solve(B)
}

print_lines <- function(x) trimws(utils::capture.output(print(x)))


# ---- Field order ----------------------------------------------------------------

test_that("every result has the same fields in the same order, NONE included", {
  d <- data.frame(x = 1:20, v = c(NA, 2:19, NA))
  r <- quiet(ilm_check_missing(d, verbose = FALSE))
  expect_s3_class(r, "ilm_missing")
  expect_identical(names(r), fields)

  r0 <- quiet(ilm_check_missing(data.frame(a = 1:5, b = letters[1:5]),
                                verbose = FALSE))
  expect_identical(names(r0), fields)
  expect_identical(r0$verdict, "NONE")
  expect_null(r0$patterns)
  expect_null(r0$associations)
  expect_null(r0$outcome_test)
  expect_identical(r0$monotone, NA)
  expect_identical(r0$n, 5L)
  expect_identical(r0$n_complete, r0$n)
  expect_null(r0$y)
  expect_null(r0$unit)
  expect_identical(r0$n_units, NA_integer_)
  expect_identical(r0$min_effect, 0.1)
  expect_output(print(r0), "no missing values")

  # NONE with y and unit named: outcome_test still NULL; unit and n_units set.
  d0 <- data.frame(a = 1:6, g = c(1, 1, 2, 2, 3, 3), b = 6:1)
  r0 <- quiet(ilm_check_missing(d0, y = "b", unit = "g", min_effect = 0.2,
                                verbose = FALSE))
  expect_identical(names(r0), fields)
  expect_null(r0$outcome_test)
  expect_identical(r0$y, "b")
  expect_identical(r0$unit, "g")
  expect_identical(r0$n_units, 3L)
  expect_identical(r0$min_effect, 0.2)
  expect_identical(print_lines(r0)[2], "no missing values")
  expect_length(print_lines(r0), 2L)
})

test_that("the tables have their columns in order, and the counts are right", {
  d <- data.frame(x = c(1, NA, 3:4, NA, 6:32, NA, 34:60),
                  g = rep(c("a", "b", "c"), 20),
                  y = seq(0.5, 30, by = 0.5),
                  v = 1:60)
  d$v[c(1, 5, 9, 14, 22, 30, 41, 47, 55)] <- NA
  r <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))

  expect_identical(names(r$variables), c("variable", "n_missing", "p_missing"))
  expect_identical(r$variables$variable, c("v", "x", "g", "y"))  # largest share first
  expect_identical(r$variables$n_missing, c(9L, 3L, 0L, 0L))
  expect_equal(r$variables$p_missing, c(9, 3, 0, 0) / 60)
  expect_identical(r$n, 60L)
  # rows missing x: 2, 5, 33; missing v: 1, 5, 9, 14, 22, 30, 41, 47, 55;
  # union 11 rows, so 49 complete.
  expect_identical(r$n_complete, 49L)

  expect_identical(names(r$patterns), c("pattern", "n", "n_missing"))
  expect_identical(attr(r$patterns, "variables"), c("x", "v"))
  # "00": 49; "01" (v only): 8; "10" (x only: rows 2, 33): 2; "11" (row 5): 1
  expect_identical(r$patterns$pattern, c("00", "01", "10", "11"))
  expect_identical(r$patterns$n, c(49L, 8L, 2L, 1L))
  expect_identical(r$patterns$n_missing, c(0L, 1L, 1L, 2L))
  expect_false(r$monotone)  # rows 2 and 33 miss x but not v

  expect_identical(names(r$associations),
                   c("missing_in", "related_to", "effect", "p_value",
                     "is_outcome", "p_adj", "flag", "method"))
  expect_false(is.unsorted(rev(r$associations$effect)))  # largest first
  expect_identical(r$associations$is_outcome, r$associations$related_to == "y")
  expect_equal(r$associations$p_adj,
               p.adjust(r$associations$p_value, method = "holm"))

  expect_identical(names(r$outcome_test),
                   c("missing_in", "effect", "p_value", "p_adj", "flag", "method"))
  expect_identical(r$outcome_test$missing_in, c("x", "v"))  # data order
  expect_identical(r$outcome_test$method, c("rows", "rows"))
})

test_that("monotone follows the nested-sets definition", {
  # x missing in row 5; v missing in rows 1, 5, 9: {5} is inside {1, 5, 9}.
  d <- data.frame(x = c(1:4, NA, 6:12), v = c(NA, 2:8, NA, 10:12))
  d$v[5] <- NA
  expect_true(quiet(ilm_check_missing(d, verbose = FALSE))$monotone)
  # Tied counts in different rows: not monotone, whichever order.
  d2 <- data.frame(x = c(NA, 2:12), v = c(1, NA, 3:12))
  expect_false(quiet(ilm_check_missing(d2, verbose = FALSE))$monotone)
  d3 <- data.frame(v = c(1, NA, 3:12), x = c(NA, 2:12))
  expect_false(quiet(ilm_check_missing(d3, verbose = FALSE))$monotone)
  # Tied counts in the same rows: monotone.
  d4 <- data.frame(x = c(NA, 2:12), v = c(NA, 2:12))
  expect_true(quiet(ilm_check_missing(d4, verbose = FALSE))$monotone)
  # One incomplete column: monotone by definition.
  d5 <- data.frame(x = 1:12, v = c(NA, 2:12))
  expect_true(quiet(ilm_check_missing(d5, verbose = FALSE))$monotone)
})


# ---- The screen's effects, by hand ------------------------------------------------

test_that("numeric w: the absolute correlation and its t test, by hand", {
  # v is missing in rows 8-10; w = 1, ..., 10. Indicator z = (0,0,0,0,0,0,0,1,1,1).
  # mean(w) = 5.5, mean(z) = 0.3.
  # Swz = sum(w z) - n mean(w) mean(z) = (8 + 9 + 10) - 10 * 5.5 * 0.3
  #     = 27 - 16.5 = 10.5
  # Sww = sum((w - 5.5)^2) = 82.5;  Szz = sum(z^2) - n * 0.3^2 = 3 - 0.9 = 2.1
  # r = 10.5 / sqrt(82.5 * 2.1);  r^2 = 110.25 / 173.25 = 7/11, r = 0.7977
  # t^2 = (n - 2) r^2 / (1 - r^2) = 8 * (7/11) / (4/11) = 14, on 8 df
  d <- data.frame(v = c(11, 12, 13, 14, 15, 16, 17, NA, NA, NA), w = 1:10)
  r <- quiet(ilm_check_missing(d, verbose = FALSE))
  a <- r$associations
  expect_identical(nrow(a), 1L)
  expect_identical(a$missing_in, "v")
  expect_identical(a$related_to, "w")
  expect_equal(a$effect, sqrt(7 / 11))
  expect_equal(a$p_value, 2 * pt(-sqrt(14), df = 8))
  expect_equal(a$p_value, cor.test(as.numeric(is.na(d$v)), d$w)$p.value)
  expect_identical(a$method, "rows")
  expect_false(a$is_outcome)
  expect_true(a$flag)  # p = 0.0057 < 0.05 and 0.80 >= 0.1
  expect_identical(r$verdict, "RELATED_TO_COVARIATES")
})

test_that("categorical w with three levels: uncorrected X^2 and Cramer's V, by hand", {
  # v missing: in a 3 of 4 rows, in b 1 of 4, in c 0 of 4.
  #              a     b     c   | total
  #   missing    3     1     0   |   4
  #   observed   1     3     4   |   8
  #   total      4     4     4   |  12
  # Expected counts: missing row 4 * 4 / 12 = 4/3 per cell; observed row
  # 8 * 4 / 12 = 8/3 per cell.
  # X^2 = (3 - 4/3)^2/(4/3) + (1 - 4/3)^2/(4/3) + (0 - 4/3)^2/(4/3)
  #     + (1 - 8/3)^2/(8/3) + (3 - 8/3)^2/(8/3) + (4 - 8/3)^2/(8/3)
  #     = 25/12 + 1/12 + 16/12 + 25/24 + 1/24 + 16/24 = 3.5 + 1.75 = 5.25
  # df = (2 - 1)(3 - 1) = 2, so p = exp(-5.25 / 2) = exp(-2.625) = 0.0724
  # V = sqrt(X^2 / (n min(2 - 1, 3 - 1))) = sqrt(5.25 / 12) = sqrt(0.4375) = 0.6614
  d <- data.frame(v = c(NA, NA, NA, 1, NA, 2, 3, 4, 5, 6, 7, 8),
                  w = rep(c("a", "b", "c"), each = 4))
  a <- quiet(ilm_check_missing(d, verbose = FALSE))$associations
  expect_identical(nrow(a), 1L)
  expect_equal(a$effect, sqrt(0.4375))
  expect_equal(a$p_value, exp(-2.625))
  expect_false(a$flag)  # p = 0.072 is not below 0.05

  # A factor with its levels in another order gives the same figures.
  d$w <- factor(d$w, levels = c("c", "a", "b"))
  a2 <- quiet(ilm_check_missing(d, verbose = FALSE))$associations
  expect_equal(a2$effect, a$effect)
  expect_equal(a2$p_value, a$p_value)
})

test_that("categorical w with two levels: no continuity correction", {
  #              a     b   | total
  #   missing    4     1   |   5
  #   observed   2     5   |   7
  #   total      6     6   |  12
  # X^2 = n (ad - bc)^2 / (r1 r2 c1 c2) = 12 (4*5 - 1*2)^2 / (5 * 7 * 6 * 6)
  #     = 12 * 324 / 1260 = 108/35 = 3.0857
  # V = sqrt((108/35) / 12) = sqrt(9/35) = 0.5071 (the absolute phi)
  # Yates's correction would give 12 (|18| - 12/2)^2 / 1260 = 48/35; not used.
  d <- data.frame(v = c(NA, NA, NA, NA, 1, 2, NA, 3, 4, 5, 6, 7),
                  w = rep(c("a", "b"), each = 6))
  a <- quiet(ilm_check_missing(d, verbose = FALSE))$associations
  expect_equal(a$effect, sqrt(9 / 35))
  expect_equal(a$p_value, pchisq(108 / 35, df = 1, lower.tail = FALSE))
  expect_false(isTRUE(all.equal(a$p_value,
                                pchisq(48 / 35, df = 1, lower.tail = FALSE))))
  # A logical w is categorical, with the same table.
  d$w <- d$w == "a"
  a2 <- quiet(ilm_check_missing(d, verbose = FALSE))$associations
  expect_equal(a2$effect, sqrt(9 / 35))
  expect_equal(a2$p_value, a$p_value)
})

test_that("screen skip rules", {
  # Fewer than 10 rows where w is observed: no row.
  d <- data.frame(v = c(1:6, NA, NA, NA), w = 1:9)
  expect_null(quiet(ilm_check_missing(d, verbose = FALSE))$associations)
  # w with zero spread: no row.
  d <- data.frame(v = c(1:9, NA, NA, NA), w = rep(5, 12))
  expect_null(quiet(ilm_check_missing(d, verbose = FALSE))$associations)
  # Categorical w with one level: no row.
  d <- data.frame(v = c(1:9, NA, NA, NA), w = rep("a", 12))
  expect_null(quiet(ilm_check_missing(d, verbose = FALSE))$associations)
})


# ---- The outcome test's effect ----------------------------------------------------

test_that("one-column effect is the absolute partial correlation; p is the LR test", {
  set.seed(101)
  n <- 300
  x <- rnorm(n)
  g <- sample(c("p", "q", "r"), n, TRUE)
  y <- 0.5 * x + rnorm(n)
  v <- rnorm(n)
  v[runif(n) < plogis(-1 + 0.8 * x + 0.4 * y)] <- NA
  d <- data.frame(x = x, g = g, y = y, v = v)
  o <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  z <- as.numeric(is.na(v))
  fit0 <- glm(z ~ x + g, family = binomial)
  fit1 <- glm(z ~ x + g + y, family = binomial)
  e <- residuals(lm(y ~ x + g))
  expect_equal(o$effect, abs(cor(residuals(fit0, type = "pearson"), e)),
               tolerance = 1e-6)
  expect_equal(o$p_value,
               pchisq(deviance(fit0) - deviance(fit1), df = 1, lower.tail = FALSE),
               tolerance = 1e-6)

  # A two-level y: the one indicator column, residualised on the covariates.
  d$y <- ifelse(y > 0, "high", "low")
  o2 <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  yl <- as.numeric(d$y == "low")
  e2 <- residuals(lm(yl ~ x + g))
  expect_equal(o2$effect, abs(cor(residuals(fit0, type = "pearson"), e2)),
               tolerance = 1e-6)
  fit1b <- glm(z ~ x + g + yl, family = binomial)
  expect_equal(o2$p_value,
               pchisq(deviance(fit0) - deviance(fit1b), df = 1, lower.tail = FALSE),
               tolerance = 1e-6)
})

test_that("a y with three or more levels gives the same result in any level order", {
  set.seed(42)
  n <- 240
  x <- rnorm(n)
  y <- sample(c("lo", "mid", "hi", "top"), n, TRUE)
  v <- rnorm(n)
  v[runif(n) < plogis(-1 + 0.5 * x + 0.8 * (y == "hi") - 0.6 * (y == "top"))] <- NA
  d1 <- data.frame(x = x, y = factor(y, levels = c("lo", "mid", "hi", "top")), v = v)
  d2 <- d1
  d2$y <- factor(y, levels = c("top", "hi", "mid", "lo"))
  d3 <- d1
  d3$y <- y  # character
  o1 <- quiet(ilm_check_missing(d1, y = "y", verbose = FALSE))$outcome_test
  o2 <- quiet(ilm_check_missing(d2, y = "y", verbose = FALSE))$outcome_test
  o3 <- quiet(ilm_check_missing(d3, y = "y", verbose = FALSE))$outcome_test
  expect_false(is.na(o1$effect))
  expect_equal(o2$effect, o1$effect)
  expect_equal(o2$p_value, o1$p_value)
  expect_equal(o3$effect, o1$effect)
  expect_equal(o3$p_value, o1$p_value)
  expect_identical(attr(o1, "effect_label"), "partial R")

  # The effect is sqrt(R^2) of the Pearson residuals on the three indicator
  # columns, each residualised on x.
  z <- as.numeric(is.na(v))
  rr <- residuals(glm(z ~ x, family = binomial), type = "pearson")
  E <- sapply(c("mid", "hi", "top"), function(l) residuals(lm(as.numeric(y == l) ~ x)))
  expect_equal(o1$effect, sqrt(summary(lm(rr ~ E))$r.squared), tolerance = 1e-6)

  # The same on the sandwich path: three rows per unit, varying within units.
  d1$id <- rep(1:80, each = 3)
  d2$id <- d1$id
  s1 <- quiet(ilm_check_missing(d1, y = "y", unit = "id", verbose = FALSE))$outcome_test
  s2 <- quiet(ilm_check_missing(d2, y = "y", unit = "id", verbose = FALSE))$outcome_test
  expect_identical(s1$method, "sandwich")
  expect_equal(s2$p_value, s1$p_value)
  expect_equal(s1$effect, o1$effect)  # effects are computed on the rows
})


# ---- Dates, date-times and durations ---------------------------------------------

test_that("outcome test: a date with few distinct values enters as one numeric column", {
  set.seed(61)
  n <- 300
  x <- rnorm(n)
  day <- sample(0:9, n, TRUE)  # 10 distinct days
  y <- 0.5 * x + 0.1 * day + rnorm(n)
  v <- rnorm(n)
  v[runif(n) < plogis(-2 + 0.25 * day + 0.6 * y)] <- NA
  dd <- data.frame(x = x, dt = as.Date("2024-03-01") + day, y = y, v = v)
  o_date <- quiet(ilm_check_missing(dd, y = "y", verbose = FALSE))$outcome_test

  # The same column as a plain number (days since 1970-01-01).
  dn <- dd
  dn$dt <- as.numeric(dd$dt)
  o_num <- quiet(ilm_check_missing(dn, y = "y", verbose = FALSE))$outcome_test
  expect_equal(o_date, o_num)

  # By hand: one numeric column, an LR test on 1 df, the partial correlation.
  z <- as.numeric(is.na(v))
  tt <- as.numeric(dd$dt)
  fit0 <- glm(z ~ x + tt, family = binomial)
  fit1 <- glm(z ~ x + tt + y, family = binomial)
  expect_equal(o_date$p_value,
               pchisq(deviance(fit0) - deviance(fit1), df = 1, lower.tail = FALSE),
               tolerance = 1e-6)
  expect_equal(o_date$effect,
               abs(cor(residuals(fit0, type = "pearson"), residuals(lm(y ~ x + tt)))),
               tolerance = 1e-6)
  # Not as 9 treatment-coded indicators.
  f0 <- glm(z ~ x + factor(tt), family = binomial)
  f1 <- glm(z ~ x + factor(tt) + y, family = binomial)
  expect_false(isTRUE(all.equal(
    o_date$p_value, pchisq(deviance(f0) - deviance(f1), df = 1, lower.tail = FALSE))))

  # A date-time (seconds) and a duration (its stored number, here weeks):
  # for a linear term the unit changes neither the test nor the effect.
  dp <- dd
  dp$dt <- as.POSIXct(dd$dt) + 3600
  o_posix <- quiet(ilm_check_missing(dp, y = "y", verbose = FALSE))$outcome_test
  expect_equal(o_posix, o_date, tolerance = 1e-6)
  dw <- dd
  dw$dt <- as.difftime(day, units = "weeks")
  o_dur <- quiet(ilm_check_missing(dw, y = "y", verbose = FALSE))$outcome_test
  expect_equal(o_dur, o_date, tolerance = 1e-6)
})

test_that("outcome test: a daily Date with more than 20 values is kept as one numeric column", {
  set.seed(62)
  n <- 300
  x <- rnorm(n)
  day <- sample(0:364, n, TRUE)  # a daily date over a year
  y <- 0.5 * x + 0.004 * day + rnorm(n)
  v <- rnorm(n)
  v[runif(n) < plogis(-2.5 + 0.006 * day + 0.3 * x + 0.3 * y)] <- NA
  dd <- data.frame(x = x, dt = as.Date("2024-01-01") + day, y = y, v = v)
  expect_gt(length(unique(dd$dt)), 20L)
  o_date <- quiet(ilm_check_missing(dd, y = "y", verbose = FALSE))$outcome_test

  # Kept: the same as the column given as a plain number ...
  dn <- dd
  dn$dt <- as.numeric(dd$dt)
  expect_equal(o_date,
               quiet(ilm_check_missing(dn, y = "y", verbose = FALSE))$outcome_test)
  # ... and, by hand, one linear term: an LR test on 1 df, the partial r.
  z <- as.numeric(is.na(v))
  tt <- as.numeric(dd$dt)
  fit0 <- glm(z ~ x + tt, family = binomial)
  fit1 <- glm(z ~ x + tt + y, family = binomial)
  expect_equal(o_date$p_value,
               pchisq(deviance(fit0) - deviance(fit1), df = 1, lower.tail = FALSE),
               tolerance = 1e-6)
  expect_equal(o_date$effect,
               abs(cor(residuals(fit0, type = "pearson"), residuals(lm(y ~ x + tt)))),
               tolerance = 1e-6)
  # Not dropped: leaving it out gives another result.
  o_without <- quiet(ilm_check_missing(dd[c("x", "y", "v")], y = "y",
                                       verbose = FALSE))$outcome_test
  expect_false(isTRUE(all.equal(o_date$p_value, o_without$p_value)))

  # Named as a covariate, it is kept all the same.
  o_f <- quiet(ilm_check_missing(dd, y ~ x + dt, verbose = FALSE))$outcome_test
  expect_equal(o_f, quiet(ilm_check_missing(dn, y ~ x + dt, verbose = FALSE))$outcome_test)

  # A daily date-time (seconds): the same test and effect.
  dp <- dd
  dp$dt <- as.POSIXct(dd$dt) + 3600
  expect_equal(quiet(ilm_check_missing(dp, y = "y", verbose = FALSE))$outcome_test,
               o_date, tolerance = 1e-6)

  # One distinct value: zero spread, so dropped.
  d1 <- dd
  d1$dt <- as.Date("2024-03-01")
  expect_equal(quiet(ilm_check_missing(d1, y = "y", verbose = FALSE))$outcome_test,
               o_without)
})

test_that("outcome test: date-times seconds apart give the result of the seconds from zero", {
  # Stored as seconds since 1970 (about 1.75e9) with a spread of under a
  # minute; a linear term with an intercept must give the same test and
  # effect as the seconds counted from zero.
  set.seed(71)
  n <- 300
  x <- rnorm(n)
  s <- sample(0:59, n, TRUE)
  y <- 0.5 * x + 0.02 * s + rnorm(n)
  v <- rnorm(n)
  v[runif(n) < plogis(-2 + 0.02 * s + 0.4 * y)] <- NA
  d <- data.frame(x = x, t = as.POSIXct("2025-06-01 12:00:00", tz = "UTC") + s,
                  y = y, v = v)
  o <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  d0 <- d
  d0$t <- s
  expect_equal(o, quiet(ilm_check_missing(d0, y = "y", verbose = FALSE))$outcome_test,
               tolerance = 1e-6)
  z <- as.numeric(is.na(v))
  fit0 <- glm(z ~ x + s, family = binomial)
  expect_equal(o$effect,
               abs(cor(residuals(fit0, type = "pearson"), residuals(lm(y ~ x + s)))),
               tolerance = 1e-6)
})

test_that("outcome test: a date y is one numeric column, however many values", {
  set.seed(63)
  n <- 300
  x <- rnorm(n)
  yd <- sample(0:6, n, TRUE)
  v <- rnorm(n)
  v[runif(n) < plogis(-1.5 + 0.3 * yd + 0.5 * x)] <- NA
  d <- data.frame(x = x, y = as.Date("2024-01-01") + yd, v = v)
  o <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  dn <- d
  dn$y <- as.numeric(d$y)
  expect_equal(o, quiet(ilm_check_missing(dn, y = "y", verbose = FALSE))$outcome_test)
  expect_identical(attr(o, "effect_label"), "partial r")
  expect_false(is.na(o$p_value))
  # A daily y with more than 20 values: still one numeric column.
  d$y <- as.Date("2024-01-01") + sample(0:364, n, TRUE)
  o2 <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  dn$y <- as.numeric(d$y)
  expect_equal(o2, quiet(ilm_check_missing(dn, y = "y", verbose = FALSE))$outcome_test)
  expect_false(is.na(o2$p_value))
  expect_identical(attr(o2, "effect_label"), "partial r")
  # A y with zero spread: nothing to test.
  d$y <- as.Date("2024-01-01")
  o3 <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  expect_true(is.na(o3$p_value))
  expect_true(is.na(o3$effect))
  expect_false(o3$flag)
})

test_that("the screen still treats a date as categorical", {
  # The three-level table of the hand-worked case above, with dates as w:
  # X^2 = 5.25 on 2 df, p = exp(-2.625); V = sqrt(5.25 / 12) = sqrt(0.4375).
  v <- c(NA, NA, NA, 1, NA, 2, 3, 4, 5, 6, 7, 8)
  d <- data.frame(v = v, w = as.Date("2024-05-01") + rep(0:2, each = 4))
  a <- quiet(ilm_check_missing(d, verbose = FALSE))$associations
  expect_equal(a$effect, sqrt(0.4375))
  expect_equal(a$p_value, exp(-2.625))
  # As a number, the same column would give a correlation instead.
  dn <- d
  dn$w <- as.numeric(d$w)
  an <- quiet(ilm_check_missing(dn, verbose = FALSE))$associations
  expect_equal(an$effect, abs(cor(as.numeric(is.na(v)), dn$w)))
  expect_false(isTRUE(all.equal(an$effect, a$effect)))
  # A date-time and a duration are categorical too.
  dp <- d
  dp$w <- as.POSIXct(d$w)
  expect_equal(quiet(ilm_check_missing(dp, verbose = FALSE))$associations$effect,
               sqrt(0.4375))
  dw <- d
  dw$w <- as.difftime(rep(0:2, each = 4), units = "days")
  expect_equal(quiet(ilm_check_missing(dw, verbose = FALSE))$associations$p_value,
               exp(-2.625))
})


# ---- Several rows per unit (`unit =`) --------------------------------------------

test_that("intercept-only sandwich, by hand", {
  # z = (1,1,0, 0,0, 1,0, 0,0,0); units A A A B B C C D D D; n = 10, p = 3/10.
  # Intercept-only logistic model: mu = p = 0.3 for every row, logit log(3/7).
  # B = n p (1 - p) = 10 * 0.3 * 0.7 = 2.1
  # Unit sums of (z - p): A: 0.7 + 0.7 - 0.3 = 1.1;  B: -0.3 - 0.3 = -0.6;
  #   C: 0.7 - 0.3 = 0.4;  D: -0.3 * 3 = -0.9   (they add to 0)
  # M = 1.1^2 + 0.6^2 + 0.4^2 + 0.9^2 = 1.21 + 0.36 + 0.16 + 0.81 = 2.54
  # Sandwich variance of the logit = M / B^2 = 2.54 / 4.41 = 0.57596
  # (the model-based variance would be 1 / B = 0.47619)
  z <- c(1, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  u <- c("A", "A", "A", "B", "B", "C", "C", "D", "D", "D")
  X <- matrix(1, 10, 1, dimnames = list(NULL, "(Intercept)"))
  expect_equal(drop(ilm_miss_sandwich(X, z, rep(0.3, 10), u)), 2.54 / 4.41)
  fit <- ilm_miss_logit(X, z)
  expect_equal(unname(fit$coefficients), log(3 / 7), tolerance = 1e-8)
  expect_equal(drop(ilm_miss_sandwich(X, z, fit$fitted.values, u)), 2.54 / 4.41,
               tolerance = 1e-8)

  # A second case: z = (1,1, 0,0, 1,0), units A A B B C C; p = 0.5.
  # B = 6 * 0.25 = 1.5; unit sums: A 1, B -1, C 0; M = 2; V = 2 / 2.25 = 8/9.
  expect_equal(drop(ilm_miss_sandwich(matrix(1, 6, 1), c(1, 1, 0, 0, 1, 0),
                                      rep(0.5, 6), c(1, 1, 2, 2, 3, 3))), 8 / 9)
})

test_that("one row per unit, the sandwich is the heteroskedasticity-robust form", {
  set.seed(5)
  n <- 80
  x <- rnorm(n)
  g <- sample(c("a", "b", "c"), n, TRUE)
  z <- rbinom(n, 1, plogis(0.3 + x - 0.5 * (g == "b")))
  fit <- glm(z ~ x + g, family = binomial)
  X <- model.matrix(fit)
  mu <- fitted(fit)
  # By hand: B = sum mu(1 - mu) x x', meat = sum (z - mu)^2 x x'.
  B <- matrix(0, 4, 4)
  meat <- matrix(0, 4, 4)
  for (i in 1:n) {
    B <- B + mu[i] * (1 - mu[i]) * tcrossprod(X[i, ])
    meat <- meat + (z[i] - mu[i])^2 * tcrossprod(X[i, ])
  }
  hc0 <- solve(B) %*% meat %*% solve(B)
  V <- ilm_miss_sandwich(X, z, mu, u = seq_len(n))
  expect_equal(unname(V), unname(hc0), tolerance = 1e-10)
  # Through the package's own fit.
  fit2 <- ilm_miss_logit(X, z)
  expect_equal(unname(fit2$coefficients), unname(coef(fit)), tolerance = 1e-8)
  V2 <- ilm_miss_sandwich(X, z, fit2$fitted.values, u = seq_len(n))
  expect_equal(unname(V2), unname(hc0), tolerance = 1e-6)
})

test_that("constant within units gives the same p-values as one row per unit", {
  set.seed(7)
  K <- 40
  ids <- sprintf("u%02d", 1:K)
  xu <- rnorm(K)
  gu <- sample(c("a", "b", "c"), K, TRUE)
  yu <- xu + rnorm(K)
  vu <- rnorm(K)
  vu[runif(K) < plogis(-0.5 + 1.2 * xu)] <- NA
  idx <- rep(1:K, times = sample(1:4, K, TRUE))  # 1 to 4 rows per unit
  long <- data.frame(id = ids[idx], x = xu[idx], g = gu[idx], y = yu[idx],
                     v = vu[idx])
  short <- long[!duplicated(long$id), c("x", "g", "y", "v")]
  rl <- quiet(ilm_check_missing(long, y = "y", unit = "id", verbose = FALSE))
  rs <- quiet(ilm_check_missing(short, y = "y", verbose = FALSE))
  expect_true(all(rl$associations$method == "units"))
  expect_true(all(rs$associations$method == "rows"))
  keep <- c("missing_in", "related_to", "effect", "p_value", "is_outcome",
            "p_adj", "flag")
  expect_equal(rl$associations[keep], rs$associations[keep])
  expect_identical(rl$outcome_test$method, "units")
  expect_equal(rl$outcome_test$p_value, rs$outcome_test$p_value)
  expect_equal(rl$outcome_test$effect, rs$outcome_test$effect)
  expect_identical(rl$n_units, 40L)
  expect_identical(rl$n, nrow(long))
  # The unit column is in `variables` but nowhere else.
  expect_true("id" %in% rl$variables$variable)
  expect_false("id" %in% rl$associations$related_to)
})

test_that("varying within units: the cluster-robust Wald test, by hand", {
  set.seed(9)
  K <- 30
  m <- 4
  id <- rep(1:K, each = m)
  ue <- rnorm(K)[id]  # a shared unit effect makes rows within a unit alike
  x <- rnorm(K * m) + ue
  g <- sample(c("a", "b", "c"), K * m, TRUE)
  y <- sample(c("k1", "k2", "k3"), K * m, TRUE)
  v <- rnorm(K * m)
  v[runif(K * m) < plogis(-0.7 + 0.8 * x + ue)] <- NA
  d <- data.frame(id = id, x = x, g = g, y = y, v = v)
  r <- quiet(ilm_check_missing(d, y = "y", unit = "id", verbose = FALSE))
  z <- as.numeric(is.na(v))

  # Screen pair (v, x): one coefficient, z = b / SE, two-sided.
  a <- r$associations[r$associations$related_to == "x", ]
  expect_identical(a$method, "sandwich")
  fit <- glm(z ~ x, family = binomial)
  V <- hand_cluster_vcov(model.matrix(fit), z, fitted(fit), id)
  expect_equal(a$p_value, unname(2 * pnorm(-abs(coef(fit)[2] / sqrt(V[2, 2])))),
               tolerance = 1e-6)
  expect_equal(a$effect, abs(cor(z, x)))  # the effect stays on the rows

  # Outcome test: y has 3 levels, so a 2-df Wald quadratic form.
  o <- r$outcome_test
  expect_identical(o$method, "sandwich")
  fit1 <- glm(z ~ x + g + y, family = binomial)
  V1 <- hand_cluster_vcov(model.matrix(fit1), z, fitted(fit1), id)
  j <- 5:6  # the y columns
  b <- coef(fit1)[j]
  W <- drop(t(b) %*% solve(V1[j, j]) %*% b)
  expect_equal(o$p_value, pchisq(W, df = 2, lower.tail = FALSE), tolerance = 1e-6)
})


# ---- NA cases --------------------------------------------------------------------------

test_that("the outcome test is NA for the outcome itself", {
  set.seed(21)
  n <- 100
  d <- data.frame(x = rnorm(n), y = rnorm(n), v = rnorm(n))
  d$y[1:15] <- NA
  d$v[c(20:34)] <- NA
  r <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))
  o <- r$outcome_test
  expect_identical(o$missing_in, c("y", "v"))
  expect_true(is.na(o$effect[1]))
  expect_true(is.na(o$p_value[1]))
  expect_true(is.na(o$p_adj[1]))
  expect_false(o$flag[1])
  expect_false(is.na(o$p_value[2]))
  expect_true("y: not testable (missingness in the outcome itself)" %in% print_lines(r))
})

test_that("the outcome test is NA with fewer than 20 rows, or a constant indicator", {
  d <- data.frame(x = c(1:14, 20), y = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9),
                  v = c(NA, 2, NA, 4:9, NA, 11:15))
  o <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  expect_true(is.na(o$effect))
  expect_true(is.na(o$p_value))
  expect_false(o$flag)

  # v is missing only where y is missing: over the rows with y observed the
  # indicator is constant.
  set.seed(22)
  d <- data.frame(x = rnorm(60), y = rnorm(60), v = rnorm(60))
  d$y[1:10] <- NA
  d$v[1:5] <- NA
  o <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))$outcome_test
  expect_true(all(is.na(o$effect)))
  expect_true(all(is.na(o$p_value)))
})

test_that("the sandwich test is NA with fewer than 30 units", {
  set.seed(23)
  K <- 10
  m <- 5
  id <- rep(1:K, each = m)
  d <- data.frame(id = id, x = rnorm(K * m), y = rnorm(K * m), v = rnorm(K * m))
  d$v[c(1, 3, 8, 12, 17, 21, 26, 30, 33, 38, 44, 49)] <- NA
  r <- quiet(ilm_check_missing(d, y = "y", unit = "id", verbose = FALSE))
  a <- r$associations
  expect_true(all(a$method == "sandwich"))
  expect_true(all(is.na(a$p_value)))
  expect_false(any(is.na(a$effect)))
  expect_false(any(a$flag))
  expect_identical(r$outcome_test$method, "sandwich")
  expect_true(is.na(r$outcome_test$p_value))
  expect_identical(r$verdict, "MCAR_NOT_REJECTED")
})

test_that("the sandwich test's floor is 30 units: NA at 29, a p-value at 30", {
  build <- function(K) {
    set.seed(24)
    m <- 5
    id <- rep(seq_len(K), each = m)
    d <- data.frame(id = id, x = rnorm(K * m), y = rnorm(K * m), v = rnorm(K * m))
    # v is missing on a different row pattern in different units, so the
    # indicator varies within units and the sandwich route is taken.
    first <- (seq_len(K) - 1L) * m
    d$v[c(first[seq(1L, K, by = 2L)] + 1L, first[seq(1L, K, by = 3L)] + 3L,
          first[seq(2L, K, by = 4L)] + 5L)] <- NA
    d
  }
  for (K in c(29, 30)) {
    r <- quiet(ilm_check_missing(build(K), y = "y", unit = "id", verbose = FALSE))
    a <- r$associations
    expect_identical(r$n_units, as.integer(K))
    expect_true(all(a$method == "sandwich"))
    expect_identical(r$outcome_test$method, "sandwich")
    expect_false(any(is.na(a$effect)))
    if (K == 29) {
      expect_true(all(is.na(a$p_value)))
      expect_true(is.na(r$outcome_test$p_value))
      expect_false(any(a$flag))
    } else {
      expect_false(any(is.na(a$p_value)))
      expect_false(is.na(r$outcome_test$p_value))
    }
  }
})


# ---- Arguments ---------------------------------------------------------------------------

test_that("argument errors", {
  d <- data.frame(x = 1:30, y = rnorm(30), v = c(NA, 2:30), id = rep(1:10, 3))
  expect_error(ilm_check_missing(as.matrix(d)), "matrix")
  expect_error(ilm_check_missing(d, ~ x), "needs a response")
  expect_error(ilm_check_missing(d, y + x ~ v), "one outcome")
  expect_error(ilm_check_missing(d, y = c("y", "x")), "single string")
  expect_error(ilm_check_missing(d, y = 3), "single string")
  expect_error(ilm_check_missing(d, y = "nope"), "not a column of `data`")
  expect_error(ilm_check_missing(d, y = "y", unit = "nope"), "not a column of `data`")
  expect_error(ilm_check_missing(d, y = "y", unit = "y"), "`unit` cannot also be")
  expect_error(ilm_check_missing(d, y = "y", covariates = c("x", "id"), unit = "id"),
               "`unit` cannot also be")
  expect_error(ilm_check_missing(d, y ~ x + id, unit = "id"), "`unit` cannot also be")
  d2 <- d
  d2$id[c(2, 7)] <- NA
  expect_error(ilm_check_missing(d2, unit = "id"), "2 missing values")
  expect_error(ilm_check_missing(d, y = "y", covariates_negate = TRUE),
               "`covariates_negate = TRUE` needs `covariates`")
  ## one unknown name reads as a pattern ("column not found in the data, and
  ## matched nothing as a pattern"); several names, one unknown, give "column(s)
  ## not found in the data"
  expect_error(ilm_check_missing(d, y ~ x + nope), "not found in the data")
  expect_error(ilm_check_missing(d, adjust = "nope"), "adjust")
})

test_that("a formula and the same names given as strings give equal results", {
  set.seed(31)
  n <- 120
  d <- data.frame(x = rnorm(n), z = sample(c("a", "b"), n, TRUE), w = rnorm(n),
                  y = rnorm(n), v = rnorm(n))
  d$v[runif(n) < plogis(-1 + d$x)] <- NA
  r1 <- quiet(ilm_check_missing(d, y ~ x + z, verbose = FALSE))
  r2 <- quiet(ilm_check_missing(d, y = "y", covariates = c("x", "z"), verbose = FALSE))
  expect_identical(r1, r2)
})

test_that("non-syntactic names work, in the string and formula forms", {
  set.seed(32)
  n <- 150
  d <- data.frame(a = rnorm(n), b = sample(c("p", "q", "r"), n, TRUE),
                  y = rnorm(n), v = rnorm(n))
  accented <- paste0("gr", intToUtf8(233), " up")  # an accented e, kept ASCII here
  names(d) <- c("my x", accented, "the outcome", "lab value")
  d[["lab value"]][runif(n) < plogis(-1 + d[["my x"]] + 0.5 * d[["the outcome"]])] <- NA
  r1 <- quiet(ilm_check_missing(d, y = "the outcome", verbose = FALSE))
  expect_false(is.na(r1$outcome_test$effect))
  fml <- as.formula(paste0("`the outcome` ~ `my x` + `", accented, "`"))
  r2 <- quiet(ilm_check_missing(d, fml, verbose = FALSE))
  expect_equal(r2$associations$p_value, r1$associations$p_value)
  expect_equal(r2$outcome_test$p_value, r1$outcome_test$p_value)
  d$id <- rep(1:50, each = 3)
  r3 <- quiet(ilm_check_missing(d, y = "the outcome", unit = "id", verbose = FALSE))
  expect_identical(r3$outcome_test$method, "sandwich")
  expect_false(is.na(r3$outcome_test$p_value))
})

test_that("treatment coding does not depend on options(contrasts)", {
  set.seed(33)
  n <- 150
  d <- data.frame(x = rnorm(n), g = sample(c("a", "b", "c"), n, TRUE),
                  y = sample(c("k1", "k2", "k3"), n, TRUE), v = rnorm(n))
  d$v[runif(n) < plogis(-1 + d$x + (d$y == "k2"))] <- NA
  r1 <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(op))
  r2 <- quiet(ilm_check_missing(d, y = "y", verbose = FALSE))
  expect_equal(r2$outcome_test, r1$outcome_test)
})


# ---- The verdict lines, exactly ---------------------------------------------------
# k is the number of missing values, m the number of columns with any.

outcome_line <- paste0(
  "related to the OUTCOME given the covariates: complete-case estimates may ",
  "be biased. Not when a covariate is missing because of its own value: ",
  "there complete cases are unbiased and imputation assuming MAR is the ",
  "biased choice. Multiple imputation (illume::ilm_impute()) is the analysis ",
  "under MAR; run the complete-case analysis as well and explain any ",
  "difference.")

test_that("MCAR_NOT_REJECTED prints its exact line", {
  # v missing in rows 1 and 20; x = 1..20; the missing rows' mean x is 10.5,
  # the overall mean, so the correlation is exactly 0. k = 2, m = 1.
  d <- data.frame(x = 1:20, v = c(NA, 2:19, NA))
  r <- quiet(ilm_check_missing(d, verbose = FALSE))
  expect_identical(r$verdict, "MCAR_NOT_REJECTED")
  expect_equal(r$associations$effect, 0)
  out <- print_lines(r)
  expect_identical(out[1], "<ilm_missing> 18 of 20 rows complete (90.0%)")
  line <- paste0("no evidence that missingness is related to any observed ",
                 "variable (2 missing values in 1 column; with few missing ",
                 "values such checks have little power); complete cases are ",
                 "unbiased unless missingness depends on unseen values")
  expect_true(line %in% out)
  expect_match(out[length(out)], "sensitivity analysis")
  # The narration uses the same line.
  expect_message(ilm_check_missing(d, verbose = TRUE), line, fixed = TRUE)
})

test_that("the counts k and m: plural, singular, and several columns", {
  # One missing value in one column: "1 missing value in 1 column".
  d <- data.frame(x = 1:20, v = c(1:9, NA, 11:20))
  r <- quiet(ilm_check_missing(d, verbose = FALSE))
  expect_identical(r$verdict, "MCAR_NOT_REJECTED")
  expect_true(any(grepl("(1 missing value in 1 column; ", print_lines(r),
                        fixed = TRUE)))
  # x missing in rows 3 and 17, v in rows 1 and 20: k = 4, m = 2.
  d2 <- data.frame(w = 1:20, x = c(1, 2, NA, 4:16, NA, 18:20),
                   v = c(NA, 2:19, NA))
  r2 <- quiet(ilm_check_missing(d2, verbose = FALSE))
  expect_identical(r2$verdict, "MCAR_NOT_REJECTED")
  expect_true(any(grepl("(4 missing values in 2 columns; ", print_lines(r2),
                        fixed = TRUE)))
})

test_that("RELATED_TO_COVARIATES prints its exact line, with and without y", {
  # k = 10, m = 1.
  d <- data.frame(x = 1:40, v = c(1:30, rep(NA, 10)))
  r <- quiet(ilm_check_missing(d, verbose = FALSE))
  expect_identical(r$verdict, "RELATED_TO_COVARIATES")
  line <- paste0("related to observed variables (10 missing values in 1 ",
                 "column); name `y` to check whether complete cases stay ",
                 "unbiased")
  expect_true(line %in% print_lines(r))
  expect_message(ilm_check_missing(d, verbose = TRUE), line, fixed = TRUE)

  set.seed(41)
  n <- 400
  x <- rnorm(n)
  y <- x + rnorm(n)
  v <- rnorm(n)
  v[runif(n) < plogis(-1 + 2 * x)] <- NA
  r <- quiet(ilm_check_missing(data.frame(x = x, y = y, v = v), y = "y",
                               verbose = FALSE))
  expect_false(any(r$outcome_test$flag))
  expect_identical(r$verdict, "RELATED_TO_COVARIATES")
  out <- print_lines(r)
  k <- sum(is.na(v))
  line <- sprintf(paste0("related to covariates, with no evidence of a relation ",
                         "to the outcome given them (%d missing values in 1 ",
                         "column): complete cases stay unbiased for the ",
                         "regression on those covariates, unless missingness ",
                         "depends on unseen values"), k)
  expect_true(line %in% out)
  # test-missing.R's phrase is still there.
  expect_true(any(grepl("complete cases stay unbiased", out, fixed = TRUE)))
  # The marginal association with y is flagged and marked as the outcome.
  expect_true(any(grepl("~ y: .*\\[the outcome\\]$", out)))
})

test_that("RELATED_TO_OUTCOME prints its exact line", {
  set.seed(43)
  n <- 400
  x <- rnorm(n)
  y <- rnorm(n)
  v <- rnorm(n)
  v[runif(n) < plogis(-1 + 2 * y)] <- NA
  r <- quiet(ilm_check_missing(data.frame(x = x, y = y, v = v), y = "y",
                               verbose = FALSE))
  expect_identical(r$verdict, "RELATED_TO_OUTCOME")
  out <- print_lines(r)
  expect_true(outcome_line %in% out)
  # test-missing.R's phrase is still there.
  expect_true(any(grepl("complete-case estimates may be biased", out, fixed = TRUE)))
  # The line does not call the complete-case analysis a sensitivity analysis;
  # only the MNAR line, last, mentions one.
  expect_false(grepl("sensitivity", outcome_line))
  expect_true(any(grepl("^v: partial r = .*  \\*$", out)))
  expect_match(out[length(out)], "sensitivity analysis")
  expect_message(ilm_check_missing(data.frame(x = x, y = y, v = v), y = "y",
                                   verbose = TRUE),
                 outcome_line, fixed = TRUE)
})

test_that("print shows the unit line, and verbose narrates as messages", {
  set.seed(44)
  d <- data.frame(id = rep(1:30, each = 2), x = rnorm(60), v = rnorm(60))
  d$v[c(3, 10, 11, 25, 40, 41, 52)] <- NA
  r <- quiet(ilm_check_missing(d, unit = "id", verbose = FALSE))
  expect_true("30 units, identified by column `id`" %in% print_lines(r))
  expect_message(ilm_check_missing(d, unit = "id", verbose = TRUE),
                 "60 rows, 3 columns")
  expect_silent(ilm_check_missing(d, unit = "id", verbose = FALSE))
})

test_that("without `unit`, several rows per unit give one warning, verbose or not", {
  set.seed(45)
  K <- 40
  id <- rep(sprintf("s%02d", 1:K), each = 3)
  age <- rep(round(runif(K, 20, 70)), each = 3)  # a subject's age repeats
  x <- rnorm(3 * K)
  v <- rnorm(3 * K)
  v[runif(3 * K) < plogis(-1 + x)] <- NA
  d <- data.frame(id = id, age = age, x = x, v = v)
  warned <- character(0)
  r <- withCallingHandlers(
    suppressMessages(ilm_check_missing(d, verbose = FALSE)),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warned, 1L)
  expect_match(warned, "independent rows")
  expect_match(warned, "`id`")
  # The caution never changes the result.
  r2 <- suppressWarnings(ilm_check_missing(d, verbose = FALSE))
  expect_identical(r2, r)
  # Named, there is no caution.
  expect_silent(ilm_check_missing(d, unit = "id", verbose = FALSE))
})
