# Two speed-ups that must change nothing: ilm_copies() sorts text keys by
# rank and a radix sort instead of order()'s locale shell sort, and
# ilm_describe() counts empty strings on the distinct values. Each is held
# to the plain computation it replaced.

test_that("sorting by rank and radix gives order()'s own result, ties included", {
  set.seed(11)
  n <- 600
  ## text with mixed case, accents, blanks and NAs, and strings the locale
  ## may call equal though their bytes differ: "ss" and "ß", and "ab"
  ## and "a" + soft hyphen + "b" (both tie under English_Canada.utf8 on
  ## Windows; where they do not, the identity must hold all the same)
  words <- c("apple", "Apple", "APPLE", "été", "ete", "Été", "zebra", "",
             " ", "ss", "ß", "ab", "a­b", NA)
  d <- data.frame(
    w = sample(words, n, TRUE),
    v = sample(c("b", "B", "à", "a", NA), n, TRUE),
    f = factor(sample(c("lo", "mid", "hi", NA), n, TRUE), levels = c("lo", "mid", "hi")),
    day = as.Date("2024-01-01") + sample(c(0:5, NA), n, TRUE),
    x = sample(c(1.5, -2, 0, NA, NaN), n, TRUE),
    stringsAsFactors = FALSE)
  for (vars in list("w", c("w", "v"), c("v", "f", "day", "x"), names(d)))
    for (nl in c(TRUE, FALSE)) {
      keys <- unname(as.list(d[vars]))
      plain <- do.call(order, c(keys, list(na.last = nl)))
      fast <- do.call(order, c(lapply(keys, ilm_sort_key), list(na.last = nl, method = "radix")))
      expect_identical(fast, plain, label = paste(paste(vars, collapse = "+"), nl))
      ## and ilm_copies() end to end: its sorted result is the unsorted one
      ## put in order()'s order, each row keeping the data's row number
      for (flt in c("all", "dupes")) {
        got <- suppressMessages(ilm_copies(d, vars, filter = flt, na_last = nl))
        raw <- suppressMessages(ilm_copies(d, vars, filter = flt, na_last = nl, sort_by = FALSE))
        want <- raw[do.call(order, c(unname(as.list(raw[vars])), list(na.last = nl))), , drop = FALSE]
        expect_identical(got, want, label = paste(flt, paste(vars, collapse = "+"), nl))
      }
    }
})

test_that("copies of text that is not valid UTF-8 sort together", {
  ## such text has no consistent collation: order() put the two copies of
  ## "caf\xe9" here at the 3rd and 8th places
  w <- c("caf\xe9", "abc", "caf\xe9", "Abc", NA, "", "\xff\xfe", "zz", "abc")
  d <- data.frame(w = w, n = seq_along(w))
  for (nl in c(TRUE, FALSE)) {
    got <- suppressMessages(ilm_copies(d, "w", na_last = nl))
    ## each value's rows are one run
    runs <- rle(ifelse(is.na(got$w), "<NA>", got$w))$values
    expect_false(anyDuplicated(runs) > 0L)
    expect_identical(sort(got$n), d$n)
    expect_identical(is.na(got$w[if (nl) nrow(got) else 1L]), TRUE)
  }
})

test_that("empty strings are counted on the distinct values, the same number", {
  set.seed(12)
  x <- sample(c("a", "b", "", " ", "\t", "  x ", NA), 5000, TRUE)
  r <- suppressMessages(ilm_describe(data.frame(x = x, stringsAsFactors = FALSE), "x"))
  expect_identical(r$n_empty, sum(trimws(x[!is.na(x)]) == ""))
  f <- suppressMessages(ilm_describe(data.frame(x = factor(x)), "x"))
  expect_identical(f$n_empty, sum(trimws(x[!is.na(x)]) == ""))
  none <- suppressMessages(ilm_describe(data.frame(x = c(NA_character_, NA)), "x"))
  expect_identical(none$n_empty, 0L)
  ## with text that is not valid UTF-8 among the values
  y <- c(x[1:200], "caf\xe9 ", " \xff", "caf\xe9 ", "")
  b <- suppressWarnings(suppressMessages(
    ilm_describe(data.frame(y = y, stringsAsFactors = FALSE), "y")))
  expect_identical(b$n_empty, sum(ilm_trimws(y[!is.na(y)]) == ""))
})
