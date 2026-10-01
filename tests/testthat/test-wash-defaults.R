# Craig's item 208a: ilm_wash_df(data), with no other argument, does the
# documented cleaning, on messy_cars and at the edges.

test_that("with defaults, messy_cars is cleaned as documented", {
  mc <- messy_cars_data()
  w <- ilm_wash_df(mc)
  expect_s3_class(w, "data.frame")
  expect_identical(names(w), c("miles_per_gallon", "number_of_cylinders", "disp",
                               "hp", "wt", "gear", "am"))   # notes, all NA, dropped
  expect_identical(nrow(w), 44L)                           # copies are not its job
  expect_type(w$number_of_cylinders, "character")          # "six" is not a number
  expect_identical(w$hp, mc$hp)                            # a number stays as it was
  expect_identical(rownames(w), as.character(seq_len(44)))
})

test_that("after the error codes are recoded, the text columns become numbers", {
  q <- function(e) suppressMessages(e)
  mc <- q(ilm_copies(messy_cars_data(), filter = "first"))
  mc <- ilm_recode_errors(mc, "six", "6")
  mc <- ilm_recode_errors(mc, "four", "4")
  mc <- ilm_recode_errors(mc, c(-1, 999), cols = c("hp", "wt"))
  mc <- ilm_recode_errors(mc, "N/A")
  w <- ilm_wash_df(mc)
  expect_identical(dim(w), c(32L, 7L))
  ## text columns of whole numbers become integer; columns that were numbers
  ## keep their type; the values are unchanged
  expect_identical(vapply(w, function(x) class(x)[1], ""),
                   c(miles_per_gallon = "numeric", number_of_cylinders = "integer",
                     disp = "numeric", hp = "numeric", wt = "numeric",
                     gear = "integer", am = "numeric"))
  ## the first of each copy is mtcars' own row, so the values are mtcars',
  ## with NA where a code for missing was
  expect_equal(w$number_of_cylinders, as.integer(mtcars$cyl))
  expect_equal(w$gear, as.integer(mtcars$gear))
  expect_equal(w$disp, ifelse(mtcars$disp > 300, NA, mtcars$disp))
  expect_equal(w$hp, ifelse(mtcars$hp == max(mtcars$hp), NA, mtcars$hp))
  expect_equal(w$wt, ifelse(mtcars$wt < stats::quantile(mtcars$wt, 0.1), NA, mtcars$wt))
})

test_that("recoding one value leaves the columns it does not touch as they were", {
  ## a text replacement assigned to no elements used to turn every numeric
  ## column into text
  d <- data.frame(n = c(1, 2, 3), s = c("six", "4", "8"), l = c(TRUE, FALSE, NA))
  r <- ilm_recode_errors(d, "six", "6")
  expect_identical(r$n, d$n)
  expect_identical(r$l, d$l)
  expect_identical(r$s, c("6", "4", "8"))
  expect_identical(ilm_recode_errors_vec(c(1, 2), "six", "6"), c(1, 2))
})

test_that("a tibble comes back a tibble, cleaned as its data frame is", {
  skip_if_not_installed("tibble")
  mc <- messy_cars_data()
  w <- ilm_wash_df(tibble::as_tibble(mc))
  expect_s3_class(w, "tbl_df")
  expect_identical(as.data.frame(w), ilm_wash_df(mc))
  ## a tibble keeps no row names, so with them the result is a data.frame
  r <- ilm_wash_df(tibble::as_tibble(data.frame(k = c("a", "b"), v = 1:2)),
                   column_to_rownames = TRUE, names_col = "k")
  expect_identical(class(r), "data.frame")
  expect_identical(rownames(r), c("a", "b"))
})

test_that("another data frame subclass comes back a data.frame", {
  sub <- structure(messy_cars_data(), class = c("my_frame", "data.frame"))
  expect_identical(class(ilm_wash_df(sub)), "data.frame")
})

test_that("a data.table comes back a data.table, cleaned as its data frame is", {
  skip_if_not_installed("data.table")
  mc <- messy_cars_data()
  w <- ilm_wash_df(data.table::as.data.table(mc))
  expect_s3_class(w, "data.table")
  expect_identical(as.data.frame(w), ilm_wash_df(mc))
})

test_that("zero rows: names are cleaned and every column kept", {
  w <- ilm_wash_df(messy_cars_data()[0, ])
  expect_identical(nrow(w), 0L)
  expect_identical(names(w), c("miles_per_gallon", "number_of_cylinders", "disp",
                               "hp", "wt", "gear", "am", "notes"))
  expect_identical(dim(ilm_wash_df(data.frame())), c(0L, 0L))
})

test_that("all-empty columns go, and a frame with nothing in it has no rows", {
  d <- data.frame(a = c(1, 2), b = c(NA, NA), c = c("", " "),
                  f = factor(c("", "")))
  expect_identical(names(ilm_wash_df(d)), "a")
  expect_identical(dim(ilm_wash_df(d[c("b", "c", "f")])), c(0L, 0L))
  ## a row empty in every column goes
  expect_identical(nrow(ilm_wash_df(data.frame(a = c(1, NA), c = c("x", " ")))), 1L)
})

test_that("factor columns keep their levels and values", {
  d <- data.frame(f = factor(c("1", "2", "")), g = factor(c("x", NA, "y")))
  w <- ilm_wash_df(d)
  expect_identical(w$f, d$f)          # a factor is not retyped
  expect_identical(w$g, d$g)
})
