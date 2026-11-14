## Numeric columns, and groups of them, with no values to summarise.
##
## A column or group with no non-missing values (all NA, all NaN, or no rows):
## ilm_describe() and ilm_describe_all() describe it rather than stopping. n is
## 0, the sum is 0, and every other statistic is NA.
##
## Infinite values are set aside: the statistics and n use the finite values
## only, so the standard error divides by the number of values it used. A
## group whose non-missing values are all infinite has n = 0, NA for every
## other statistic, and as its sum what base R's sum() gives for them: Inf,
## -Inf, or NaN for a mix of the two.

## the statistics that must be NA for a group with nothing to summarise
empty_stats <- c("mean", "sd", "se", "p0", "p25", "p50", "p75", "p100", "skew", "kurt",
                 "p_zero", "dispersion", "gauss", "ks_d")

expect_empty_row <- function(row, obs, sum = 0, na = obs) {
  row <- as.data.frame(row)
  expect_equal(nrow(row), 1L)
  expect_equal(row$obs, obs)
  expect_equal(row$n, 0L)
  expect_equal(row$na, na)
  expect_identical(row$sum, as.numeric(sum))
  for (s in intersect(empty_stats, names(row)))
    expect_true(is.na(row[[s]]), info = s)
}

## six rows in two groups; x has no values in group a, y none at all
empty_data <- function() {
  data.frame(x = c(NA, NA, NA, 1.5, 2.5, 4),
             y = rep(NA_real_, 6),
             w = c(NaN, NaN, NaN, 2, 3, 5),
             i = c(NA, NA, NA, 2L, 3L, 7L),
             z = c(1, 2, 3, 4, 5, 6),
             g = rep(c("a", "b"), each = 3),
             h = rep(c("p", "q"), 3),
             stringsAsFactors = FALSE)
}

## ---- no non-missing values ----------------------------------------------------

test_that("a vector or column with no values is described, not stopped on", {
  d <- empty_data()
  expect_empty_row(ilm_describe(rep(NA_real_, 4)), 4L)
  expect_empty_row(ilm_describe(rep(NaN, 4)), 4L)
  expect_empty_row(ilm_describe(d, "y"), 6L)
  expect_empty_row(ilm_describe(numeric(0)), 0L)
  expect_empty_row(ilm_describe(integer(0)), 0L)
  expect_empty_row(ilm_describe(data.frame(x = numeric(0)), "x"), 0L)
})

test_that("a group with no values is described, and the other groups are unchanged", {
  d <- empty_data()
  for (v in c("x", "w", "i")) {
    r <- as.data.frame(ilm_describe(d, v, by = "g"))
    expect_equal(r$g, c("a", "b"), info = v)
    expect_empty_row(r[1, ], 3L)
    ## group b's row is what describing group b alone gives
    alone <- as.data.frame(ilm_describe(d[d$g == "b", ], v))
    expect_equal(r[2, names(alone)], alone, ignore_attr = TRUE, info = v)
  }
})

test_that("a column with no values at all is described by group too", {
  r <- as.data.frame(ilm_describe(empty_data(), "y", by = "g"))
  expect_empty_row(r[1, ], 3L)
  expect_empty_row(r[2, ], 3L)
})

test_that("with two by variables, the empty combinations are described", {
  r <- as.data.frame(ilm_describe(empty_data(), "x", by = c("g", "h")))
  a <- r[grepl("^a", r$g.h), ]
  expect_equal(nrow(a), 2L)
  expect_empty_row(a[1, ], a$obs[1])
  expect_empty_row(a[2, ], a$obs[2])
  expect_true(all(r$n[!grepl("^a", r$g.h)] > 0L))
})

test_that("every option the description takes copes with a group with no values", {
  d <- empty_data()
  for (gs in c("index", "ks_d", "both", "none")) {
    r <- as.data.frame(ilm_describe(d, "x", by = "g", gauss = gs, skew = TRUE, kurt = TRUE,
                                    probs = c(0, 0.25, 0.5, 0.75, 1), dispersion = TRUE))
    expect_empty_row(r[1, ], 3L)
    if (gs %in% c("index", "both")) expect_true("gauss" %in% names(r), info = gs)
    if (gs %in% c("ks_d", "both")) expect_true("ks_d" %in% names(r), info = gs)
  }
  expect_empty_row(as.data.frame(ilm_describe(d, "x", by = "g", dispersion = FALSE))[1, ], 3L)
  expect_empty_row(as.data.frame(ilm_describe(d, "x", by = "g", probs = numeric(0)))[1, ], 3L)
})

test_that("a standardised difference involving a group with no values is NA, not a stop", {
  d <- empty_data()
  r <- as.data.frame(ilm_describe(d, "x", by = "g", smd = TRUE))
  expect_true(all(is.na(r$smd)))
  r2 <- as.data.frame(ilm_describe(d, "x", by = "g", smd = "b"))
  expect_true(is.na(r2$smd[r2$g == "a"]))
  r3 <- as.data.frame(ilm_describe(d, "x", by = c("g", "h"), smd = TRUE))
  expect_true(all(is.na(r3$smd[grepl("^a", r3$g.h)])))
})

test_that("ilm_describe_all() describes a group with no values", {
  d <- empty_data()
  for (b in list("g", c("g", "h"))) {
    r <- ilm_describe_all(d[c("x", "w", "i", "z", "g", "h")], by = b)
    num <- as.data.frame(if (is.data.frame(r)) r else r$numeric)
    gcol <- paste(b, collapse = ".")
    for (v in c("x", "w", "i")) {
      rows <- num[num$variable == v & grepl("^a", num[[gcol]]), ]
      expect_true(nrow(rows) >= 1L, info = v)
      for (k in seq_len(nrow(rows))) expect_empty_row(rows[k, ], rows$obs[k])
    }
    expect_true(all(num$n[num$variable == "z"] > 0L))
  }
  r <- ilm_describe_all(d[c("x", "z", "g")], by = "g", smd = TRUE, gauss = "both",
                        skew = TRUE, kurt = TRUE)
  num <- as.data.frame(if (is.data.frame(r)) r else r$numeric)
  expect_empty_row(num[num$variable == "x" & num$g == "a", ], 3L)
})

## ---- infinite values ---------------------------------------------------------------

test_that("n counts the finite values the statistics use, so the SE is right", {
  r <- ilm_describe(c(1, 2, 3, Inf, NA))
  expect_equal(r$obs, 5L)
  expect_equal(r$n, 3L)
  expect_equal(r$na, 1L)
  expect_equal(r$sum, 6)
  expect_equal(r$mean, 2)
  expect_equal(r$sd, 1)
  expect_equal(r$se, 1 / sqrt(3))
  ## one finite value: n is 1, and the SD and SE are NA
  r1 <- ilm_describe(c(5, Inf))
  expect_equal(r1$n, 1L)
  expect_equal(r1$sum, 5)
  expect_true(is.na(r1$sd) && is.na(r1$se))
})

test_that("by group, and in ilm_describe_all(), n counts the finite values", {
  d <- data.frame(x = c(1, 2, 3, Inf, 4, 5, 6, -Inf), z = 1:8, g = rep(c("a", "b"), each = 4))
  r <- as.data.frame(ilm_describe(d, "x", by = "g"))
  expect_equal(r$n, c(3L, 3L))
  expect_equal(r$se, rep(1 / sqrt(3), 2))
  r <- ilm_describe_all(d, by = "g"); num <- as.data.frame(if (is.data.frame(r)) r else r$numeric)
  expect_equal(num$n[num$variable == "x"], c(3L, 3L))
  expect_equal(num$n[num$variable == "z"], c(4L, 4L))
  r <- ilm_describe_all(d[c("x", "z")]); num <- as.data.frame(if (is.data.frame(r)) r else r$numeric)
  expect_equal(num$n[num$variable == "x"], 6L)
  expect_equal(num$se[num$variable == "x"], stats::sd(c(1:6)) / sqrt(6))
})

test_that("a group of only infinite values has n 0, base R's sum, and NA otherwise", {
  expect_empty_row(ilm_describe(c(Inf, Inf)), 2L, sum = Inf, na = 0L)
  expect_empty_row(ilm_describe(c(-Inf, -Inf, NA)), 3L, sum = -Inf, na = 1L)
  expect_empty_row(ilm_describe(c(Inf, -Inf, Inf)), 3L, sum = NaN, na = 0L)
  d <- data.frame(x = c(Inf, Inf, NA, 1, 2, 4), z = 1:6, g = rep(c("a", "b"), each = 3))
  for (gs in c("index", "ks_d", "both", "none")) {
    r <- as.data.frame(ilm_describe(d, "x", by = "g", gauss = gs, skew = TRUE, kurt = TRUE,
                                    probs = c(0, 0.5, 1), smd = TRUE))
    expect_empty_row(r[1, ], 3L, sum = Inf, na = 1L)
    expect_equal(r$n[2], 3L)
  }
  r <- ilm_describe_all(d, by = "g"); num <- as.data.frame(if (is.data.frame(r)) r else r$numeric)
  expect_empty_row(num[num$variable == "x" & num$g == "a", ], 3L, sum = Inf, na = 1L)
})

test_that("the standardised difference of a group with an infinite value uses its finite values", {
  ## by hand on the finite values: (5 - 2) / sqrt((1 + 1) / 2) = 3
  d <- data.frame(x = c(1, 2, 3, Inf, 4, 5, 6, -Inf), g = rep(c("a", "b"), each = 4))
  r <- as.data.frame(ilm_describe(d, "x", by = "g", smd = TRUE))
  expect_equal(r$smd[r$g == "b"], 3)
  fin <- d[is.finite(d$x), ]
  expect_equal(r$smd, as.data.frame(ilm_describe(fin, "x", by = "g", smd = TRUE))$smd)
  ## and with two by variables
  d$h <- rep(c("p", "q"), each = 2, times = 2)
  d2 <- rbind(d, transform(d, h = ifelse(h == "p", "q", "p")))
  r2 <- as.data.frame(ilm_describe(d2, "x", by = c("h", "g"), smd = TRUE))
  f2 <- as.data.frame(ilm_describe(d2[is.finite(d2$x), ], "x", by = c("h", "g"), smd = TRUE))
  expect_equal(r2$smd, f2$smd)
  ## a group with no finite values has no standardised difference
  d3 <- data.frame(x = c(1, 2, 3, Inf, Inf, Inf), g = rep(c("a", "b"), each = 3))
  expect_true(is.na(as.data.frame(ilm_describe(d3, "x", by = "g", smd = TRUE))$smd[2]))
})

## ---- the empty argument -------------------------------------------------------------

test_that("`empty` sets the sum of a column or group with no values", {
  d <- empty_data()
  expect_empty_row(ilm_describe(d, "y", empty = NA), 6L, sum = NA)
  expect_empty_row(ilm_describe(d, "y", empty = -999), 6L, sum = -999)
  r <- as.data.frame(ilm_describe(d, "x", by = "g", empty = NA))
  expect_empty_row(r[1, ], 3L, sum = NA)
  expect_equal(r$sum[2], 8)
  r <- ilm_describe_all(d[c("x", "z", "g")], by = "g", empty = -1)
  num <- as.data.frame(if (is.data.frame(r)) r else r$numeric)
  expect_empty_row(num[num$variable == "x" & num$g == "a", ], 3L, sum = -1)
  ## a group with values is unaffected, whatever `empty` is
  expect_equal(num$sum[num$variable == "z"], c(6, 15))
  ## and a group of infinite values shows its own sum, whatever `empty` is
  expect_identical(ilm_describe(c(Inf, Inf), empty = NA)$sum, Inf)
})

test_that("`empty` takes one number or NA, and refuses anything else", {
  d <- empty_data()
  for (bad in list("none", c(0, 1), NULL, TRUE, list(0), numeric(0)))
    expect_error(ilm_describe(d, "y", empty = bad), "`empty` must be one number or NA")
  expect_error(ilm_describe_all(d, empty = "x"), "`empty` must be one number or NA")
  expect_no_error(ilm_describe(d, "y", empty = NA_real_))
  expect_no_error(ilm_describe(d, "y", empty = 0L))
})

## ---- the count of infinite values, and the message ------------------------------------

inf_messages <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  grep("infinite", msgs, value = TRUE)
}

test_that("n_inf counts the infinite values set aside, and appears only when there are some", {
  r <- suppressMessages(ilm_describe(c(1, 2, 3, Inf, NA)))
  expect_equal(r$n_inf, 1L)
  expect_equal(match("n_inf", names(r)), match("na", names(r)) + 1L)
  expect_equal(r$obs, r$n + r$na + r$n_inf)
  expect_false("n_inf" %in% names(ilm_describe(c(1, 2, 3, NA))))
  d <- data.frame(x = c(1, Inf, 3, 4, 5, 6), z = c(1, 2, 3, 4, 5, 6),
                  g = rep(c("a", "b"), each = 3))
  r <- as.data.frame(suppressMessages(ilm_describe(d, "x", by = "g")))
  expect_equal(r$n_inf, c(1L, 0L))
  expect_false("n_inf" %in% names(ilm_describe(d, "z", by = "g")))
  r <- suppressMessages(ilm_describe_all(d, by = "g"))
  num <- as.data.frame(if (is.data.frame(r)) r else r$numeric)
  expect_equal(num$n_inf, c(1L, 0L, 0L, 0L))
  expect_false("n_inf" %in% names(as.data.frame(ilm_describe_all(d[c("z", "g")], by = "g"))))
  expect_equal(suppressMessages(ilm_describe(c(Inf, -Inf, NA)))$n_inf, 2L)
})

test_that("a call that meets infinite values says so once, and names ilm_recode_errors()", {
  m <- inf_messages(ilm_describe(c(1, 2, Inf, -Inf)))
  expect_length(m, 1L)
  expect_match(m, "ilm_recode_errors()", fixed = TRUE)
  expect_match(m, "2 infinite values")
  d <- data.frame(x = c(1, Inf, 3, 4, 5, Inf), w = c(Inf, 2, 3, 4, 5, 6), z = 1:6,
                  g = rep(c("a", "b"), each = 3))
  m <- inf_messages(ilm_describe_all(d, by = "g"))
  expect_length(m, 1L)
  expect_match(m, "3 infinite values")
  expect_match(m, "x (a: 1, b: 1)", fixed = TRUE)
  expect_match(m, "w (a: 1)", fixed = TRUE)
  m <- inf_messages(ilm_describe(d, c("x", "w")))
  expect_length(m, 1L)
  expect_length(inf_messages(ilm_describe_all(d[c("z", "g")], by = "g")), 0L)
  expect_length(inf_messages(ilm_describe(1:5)), 0L)
})
