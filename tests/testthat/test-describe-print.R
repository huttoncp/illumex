# A description prints the same on every computer (Craig's ruling 272): its
# numbers are rounded by the shared display rules and written as text, in
# the layout R's print gives a column. These cases sit on rounding
# boundaries, where R's own print could differ between an Intel machine and
# Apple silicon, so the five platforms the package is checked on hold them.

text_of <- function(r) ilm_describe_text(ilm_describe_rounded(r), as.data.frame(r))

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
