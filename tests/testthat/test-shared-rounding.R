# The shared rounding (R/shared-helpers.R) against an independent one: half
# away from zero done by hand on the decimal string sprintf("%.14e") gives,
# 15 significant figures correctly rounded by the C library. Random values
# from 1e-6 to 1e13, constructed ties and values cut to 1 to 15 figures, at
# 0 to 6 decimals, agree. (The one kind of value where they may not is an
# exact binary tie at the 16th figure, which sprintf() rounds to even; its
# case is in fixtures/format_cases.csv.)

ref_fixed <- function(x, d) {
  if (x == 0) return(if (d > 0) paste0("0.", strrep("0", d)) else "0")
  s <- sprintf("%.14e", abs(x))
  dig <- gsub(".", "", sub("e.*$", "", s), fixed = TRUE)
  E <- as.integer(sub("^.*e", "", s))
  k <- E + 1L + d
  if (k >= 15L) {
    kept <- paste0(dig, strrep("0", k - 15L))
  } else if (k < 0L) {
    kept <- "0"
  } else {
    kept <- if (k == 0L) "0" else substr(dig, 1L, k)
    if (substr(dig, k + 1L, k + 1L) >= "5") {
      ch <- rev(strsplit(kept, "")[[1]])
      carry <- TRUE
      for (p in seq_along(ch)) {
        if (!carry) break
        if (ch[p] == "9") ch[p] <- "0" else { ch[p] <- as.character(as.integer(ch[p]) + 1L); carry <- FALSE }
      }
      kept <- paste(rev(ch), collapse = "")
      if (carry) kept <- paste0("1", kept)
    }
  }
  kept <- sub("^0+(?=.)", "", kept, perl = TRUE)
  if (nchar(kept) <= d) kept <- paste0(strrep("0", d - nchar(kept) + 1L), kept)
  int <- substr(kept, 1L, nchar(kept) - d)
  frac <- if (d > 0) substr(kept, nchar(kept) - d + 1L, nchar(kept)) else ""
  neg <- x < 0 && grepl("[1-9]", kept)
  paste0(if (neg) "-", int, if (d > 0) paste0(".", frac))
}

test_that("the shared rounding agrees with half away on the 15-figure string", {
  set.seed(20261001)
  n <- 700
  x <- c(10^stats::runif(n, -6, 13) * sample(c(-1, 1), n, TRUE),
         (floor(10^stats::runif(n, 0, 9)) + 0.5) / 10^(dd <- sample(0:6, n, TRUE)),
         signif(10^stats::runif(n, -6, 13), sample(1:15, n, TRUE)))
  d <- c(sample(0:6, n, TRUE), dd, sample(0:6, n, TRUE))
  ## the extremes, 1e-300 to 1e300, rounded inside the mantissa
  x4 <- 10^stats::runif(200, -300, 300) * sample(c(-1, 1), 200, TRUE)
  x <- c(x, x4)
  d <- c(d, pmax(0, 14 - floor(log10(abs(x4))) - sample(1:14, 200, TRUE)))
  ref <- mapply(ref_fixed, x, d)
  got <- vapply(seq_along(x), function(i) ilm_disp(x[i], "fixed", d[i], big_mark = "")$text, "")
  expect_identical(got, ref)
})

test_that("rounding to a number goes by the same digits", {
  expect_identical(ilm_disp_round(c(2.675, 1.005, -0.125, 95074673.3685499, NaN, Inf, -0.0001), 2),
                   c(2.68, 1.01, -0.13, 95074673.37, NaN, Inf, 0))
  expect_identical(ilm_disp_round(95074673.3685499, 4), 95074673.3685)
  expect_identical(ilm_disp_signif(c(9.995, 19234, 0.000123456, 0), 3), c(10, 19200, 0.000123, 0))
})

test_that("the largest double and the subnormals round without error", {
  ## not the literal 1.7976931348623157e308: where long double is double
  ## (macOS on arm64), R's parser builds a decimal literal in double, and
  ## that one overflows to Inf
  big <- .Machine$double.xmax
  ## the rounded 1.8e308 is beyond any double: the number is returned as it
  ## is, and the text is written from the rounded digits
  expect_identical(ilm_disp_signif(big, 2), big)
  expect_identical(ilm_disp_round(big, 0), big)
  expect_identical(nchar(ilm_disp(big, "signif", 2)$text), 411L)
  expect_true(startsWith(ilm_disp(big, "signif", 2)$text, "180,000,"))
  for (v in c(5e-324, 1e-310, -1e-310)) {
    expect_true(is.finite(ilm_disp_signif(v, 2)))
    expect_true(grepl("^-?0[.]0+[1-9][0-9]?$", ilm_disp(v, "signif", 2)$text))
  }
})
