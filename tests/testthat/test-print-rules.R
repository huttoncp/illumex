# Every print shows the same numbers on every computer (Craig's ruling 272):
# numbers are rounded by the shared display rules and then written as text,
# a table in the layout R's print gives a column, a line through ilm_fx() or
# ilm_gx(). These cases sit on rounding boundaries, where R's print, or the
# C library's sprintf(), could differ between systems, so the five platforms
# the package is checked on hold them.

text_of <- function(r) ilm_print_text(ilm_describe_rounded(r), as.data.frame(r))

test_that("a median on a boundary prints one way everywhere (34420.195)", {
  d <- data.frame(x = c(34420.19, 34420.20))
  t <- text_of(ilm_describe(d, "x"))
  ## seven figures, half away from zero: 34420.2, never 34420.19
  expect_identical(t$p50, "34420.2")
  expect_identical(t$mean, "34420.2")
  ## and so prints: the smallest value is 34420.19, the mean and median 34420.2
  out <- utils::capture.output(print(ilm_describe(d, "x")))
  expect_identical(out[2], "1   2 2  0 68840.39 34420.2 0.007 0.005 34420.19 34420.2 34420.2      0")
})

test_that("decimal ties round half away from zero, after the noise is cleared", {
  tie <- function(a, b, digits) text_of(ilm_describe(data.frame(x = c(a, b)), "x", digits = digits))$mean
  expect_identical(tie(2.67, 2.68, 2), "2.68")     # 2.675, stored as 2.67499999...
  expect_identical(tie(0.12, 0.13, 2), "0.13")     # 0.125 exactly: not to even
  expect_identical(tie(-0.12, -0.13, 2), "-0.13")  # away from zero, negative too
  expect_identical(tie(1.00, 1.01, 2), "1.01")     # 1.005, stored as 1.00499999...
  expect_identical(tie(12.3, 12.4, 1), "12.4")     # 12.35
})

test_that("a value is rounded once, from the value kept", {
  ## 40410.4647 shown to two decimals is 40410.46, not 40410.47 by way of 40410.465
  t <- text_of(ilm_describe(data.frame(x = rep(40410.4647, 2)), "x"))
  expect_identical(t$mean, "40410.46")
  ## a number of many figures keeps its digits: no noise allowance reaches them
  expect_identical(ilm_print_num(c(-15.85974, 7195166.76)), c("-15.85974", "7195166.76000"))
  expect_identical(ilm_print_num(c(4878897.65, 1964.85)), c("4878897.65", "1964.85"))
})

test_that("zero, signs, missing values and scientific notation print as R prints them", {
  t <- text_of(ilm_describe(data.frame(x = c(-0.0001, -0.0003)), "x"))
  expect_identical(t$mean, "0")                    # never "-0"
  expect_identical(ilm_print_num(c(1.5e-10, NA)), c("1.5e-10", "NA"))
  expect_identical(ilm_print_num(c(NaN, Inf, -Inf, 2)), c("NaN", "Inf", "-Inf", "2"))
  expect_identical(ilm_print_num(c(1.234567e10, 1)), c("12345670000", "1"))
  expect_identical(ilm_print_num(c(1.2345678e15, 0.5)), c("1.234568e+15", "5.000000e-01"))
})

test_that("away from a tie, a column prints exactly as R prints it", {
  set.seed(272)
  for (i in 1:500) {
    k <- sample(1:6, 1)
    v <- stats::rnorm(k) * 10^sample(-6:12, k, TRUE)
    if (i %% 2) v <- signif(v, sample(1:6, 1))
    if (i %% 5 == 0) v[sample(k, 1)] <- 0
    if (i %% 7 == 0) v[sample(k, 1)] <- NA
    expect_identical(ilm_print_num(v), trimws(format(v)), label = paste(v, collapse = ", "))
  }
})

test_that("a line's numbers round by the shared rule, not the C library's", {
  ## exact binary ties, which sprintf() rounds to even on some systems
  expect_identical(ilm_fx(c(12.25, 87.75), 1), c("12.3", "87.8"))
  expect_identical(ilm_fx(c(0.125, -0.125), 2), c("0.13", "-0.13"))
  expect_identical(ilm_fx(0.5, 0), "1")
  ## decimal ties stored a hair below, the noise cleared
  expect_identical(ilm_fx(c(2.675, 1.005), 2), c("2.68", "1.01"))
  expect_identical(ilm_fx(0.4125, 3), "0.413")
  expect_identical(ilm_fx(NA_real_, 1), "NA")
  ## "%g": six figures, trailing zeros dropped, scientific where %g goes
  expect_identical(ilm_gx(c(86400, 0.05, 1234567, 2.5e-7)),
                   c("86400", "0.05", "1.23457e+06", "2.5e-07"))
  expect_identical(ilm_gx(0.125, 2), "0.13")
  ## format(x, digits = 4), as the floor and ceiling notes use it
  expect_identical(vapply(c(1.2345, 1234567, 0.00012345), ilm_print_num, "", 4L),
                   c("1.235", "1234567", "0.0001235"))
})

test_that("a cluster's table rounds its ties one way (12.25%)", {
  x <- structure(list(method = "kmeans", k = 2L, gap = NULL,
                      clusters = data.frame(cluster = 1:2, size = c(49L, 351L), pct = c(12.25, 87.75),
                                            jaccard = c(0.8125, 0.9), stability = c("stable", "stable"),
                                            mean_silhouette = c(0.4125, 0.5), anomalous = c(FALSE, FALSE)),
                      ind_cluster = data.frame(is_small_cluster = rep(FALSE, 2), is_ambiguous = rep(FALSE, 2)),
                      small_cluster_frac = 0.125, ambiguous_threshold = 0.125),
                 class = "ilm_cluster")
  out <- utils::capture.output(print(x))
  expect_identical(out[4], "        1     49   12.3    0.813     stable   0.413")
  expect_identical(out[5], "        2    351   87.8    0.900     stable   0.500")
})

test_that("anomaly and contribution tables round and print one way", {
  a <- structure(data.frame(row = 1:2, score = c(12345.675, 1.5), p = c(0.0125, 0.5),
                            p_adj = c(0.0125, 0.5), flag = c(TRUE, FALSE), driver = c("a", "b")),
                 alpha = 0.125, method = "reconstruction", trim = 0.125, rank = 2L,
                 columns = c("a", "b"), n_null = 99L, class = c("ilm_anomaly", "data.frame"))
  out <- utils::capture.output(print(a))
  expect_identical(out[1], "Multivariate anomalies: 1 of 2 rows flagged at 0.125")
  expect_identical(out[2], "  rank 2 over 2 columns, fitted on the best 88% of rows, 99 null scores")
  expect_identical(out[4:5], c("   1 12345.68 0.0125 0.0125  TRUE      a",
                               "   2     1.50 0.5000 0.5000 FALSE      b"))
  v <- structure(data.frame(variable = c("x", "y"), separation = c(0.4125, 0.1),
                            permuted = c(0.0625, 0.05), p = c(0.0125, 0.5), p_adj = c(0.025, 0.5),
                            verdict = c("defines clusters", "no better than chance")),
                 k = 2L, B = 199L, class = c("ilm_var_contrib", "data.frame"))
  out <- utils::capture.output(print(v))
  expect_identical(out[3], "        x      0.413    0.063 0.0125 0.025      defines clusters")
})

test_that("the gaussian check, and results with no print of their own, print one way", {
  g <- structure(list(gauss = 0.4125, ks_d = 0.11825, gauss_note = ""), class = "ilm_gauss")
  out <- utils::capture.output(print(g))
  expect_identical(out[c(2, 5)], c("[1] 0.413", "[1] 0.1183"))
  b <- structure(data.frame(variable = "x", estimate = 34420.195, lower = 1.5, upper = 34500),
                 class = c("ilm_boot_ci", "data.frame"))
  expect_identical(utils::capture.output(print(b))[2],
                   "1        x  34420.2   1.5 34500")
})
