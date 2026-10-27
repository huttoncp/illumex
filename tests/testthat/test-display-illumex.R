# illumex's own number formatters, built on the shared display rules of
# R/shared-helpers.R. Kept apart from test-shared-display.R, which every
# package of the family carries and which tests only the shared code.

test_that("illumex prints the tie by the trailing-zeros convention", {
  ## Under the convention, an estimate keeps its
  ## trailing zeros and is never shown whole for being whole (10.0, 42.0,
  ## 0.610, 12,300); a count, or a data value that is a whole number, is
  ## whole. Before it, 9.9951 printed "10", 42 "42", 0.61 "0.61", 12345
  ## "12,345".
  num <- c(0.125, 1.2345, 12.35, -12.35, 2.675, 1.005, 0.0125, 9.9951, 99.95, 999.95, 999,
           1000, 1230, 12345, 19234, 19234.5, 12345.6, 1234567.8, -0.125, 0, 42, 48.9, 0.61)
  expect_identical(vapply(num, ilm_fmt_num, ""),
    c("0.125", "1.23", "12.4", "-12.4", "2.68", "1.01", "0.0125", "10.0", "100", "1,000",
      "999", "1,000", "1,230", "12,300", "19,200", "19,200", "12,300", "1,230,000",
      "-0.125", "0", "42.0", "48.9", "0.610"))
  ## a data value: whole when it is a whole number, else three figures
  expect_identical(vapply(num, ilm_fmt_data, ""),
    c("0.125", "1.23", "12.4", "-12.4", "2.68", "1.01", "0.0125", "10.0", "100", "1,000",
      "999", "1,000", "1,230", "12,345", "19,234", "19,200", "12,300", "1,230,000",
      "-0.125", "0", "42", "48.9", "0.610"))
  expect_identical(ilm_fmt_count(c(40, 1234)), c("40", "1,234"))
  pct <- c(0.2725, 109 / 400, 0.2724, 0.125, 0.005, 0.995, 0.5, 0.35, 0.294, 0.033)
  expect_identical(vapply(pct, ilm_fmt_pct, "", 0L),
                   c("27%", "27%", "27%", "13%", "1%", "100%", "50%", "35%", "29%", "3%"))
  expect_identical(vapply(pct, ilm_fmt_pct, "", 1L),
                   c("27.3%", "27.3%", "27.2%", "12.5%", "0.5%", "99.5%", "50.0%", "35.0%",
                     "29.4%", "3.3%"))
})
