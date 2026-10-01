# Text that is not valid UTF-8 -- a file in Windows-1252 or Latin-1 read as
# UTF-8 -- once stopped ilm_wash_df() on a 1.4M-row frame at a single value
# ("input string 1328351 is invalid UTF-8", from trimws()). Such text is now
# left as it is, reported, and drawn and printed with its stray bytes
# spelled out.

cafe <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))     # "cafe", its e acute in Latin-1
bytes <- function(x) lapply(as.character(x), charToRaw)

test_that("the test string is the case: bytes that are not valid UTF-8, encoding unknown", {
  expect_false(validUTF8(cafe))
  expect_identical(Encoding(cafe), "unknown")
})

test_that("ilm_wash_df() completes, names the columns and leaves the values as they are", {
  d <- data.frame(id = 1:6,
                  txt = c("tea", cafe, "", "milk", cafe, NA),
                  fac = factor(c("north", "south", cafe, "north", NA, "south")),
                  stringsAsFactors = FALSE)
  expect_warning(w <- ilm_wash_df(d), "text in 2 columns is not valid UTF-8")
  expect_warning(ilm_wash_df(d), "txt (2 values; rows 2 and 5) and fac (1 value; row 3)",
                 fixed = TRUE)
  expect_warning(ilm_wash_df(d), "fileEncoding = \"windows-1252\"", fixed = TRUE)
  ## the values come back byte for byte; only the blank became NA, as before
  expect_identical(bytes(w$txt[c(2, 5)]), bytes(c(cafe, cafe)))
  expect_identical(Encoding(w$txt[2]), "unknown")
  expect_true(is.na(w$txt[3]))
  expect_identical(w$fac, d$fac)
  ## the result keeps the table
  nu <- attr(w, "not_utf8")
  expect_identical(nu$column, c("txt", "fac"))
  expect_identical(nu$n, c(2L, 1L))
  expect_identical(nu$rows, c("2, 5", "3"))
})

test_that("the rows named are the first five of the input, with how many more", {
  d <- data.frame(x = c(rep(cafe, 7), "a"), stringsAsFactors = FALSE)
  expect_warning(w <- ilm_wash_df(d), "x (7 values; rows 1, 2, 3, 4, 5 and 2 more)", fixed = TRUE)
  expect_identical(attr(w, "not_utf8")$rows, "1, 2, 3, 4, 5")
})

test_that("blank rows and columns are still found around such text, a factor's by its levels", {
  d <- data.frame(a = c(" ", cafe, NA), b = factor(c(" ", " x", NA)), c = NA,
                  stringsAsFactors = FALSE)
  w <- suppressWarnings(ilm_wash_df(d))
  expect_identical(names(w), c("a", "b"))       # c, all missing, is dropped
  expect_identical(nrow(w), 1L)                  # rows 1 and 3 are blank
  expect_identical(bytes(w$a), bytes(cafe))
})

test_that("a text column holding such text stays text; the others are retyped", {
  d <- data.frame(n = c("1", "2", "3"), t = c("1", cafe, "3"), stringsAsFactors = FALSE)
  w <- suppressWarnings(ilm_wash_df(d))
  expect_type(w$n, "integer")
  expect_type(w$t, "character")
})

test_that("a column name that is not valid UTF-8 is cleaned with its stray byte spelled out", {
  d <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  names(d)[2] <- paste0("note ", cafe)
  expect_warning(w <- ilm_wash_df(d), "1 column name is not valid UTF-8")
  expect_identical(names(w), c("a", "note_caf_e9"))
  expect_identical(w$note_caf_e9, c("x", "y"))
  expect_null(attr(w, "not_utf8"))
})

test_that("clean data give no warning and no table", {
  expect_no_warning(w <- ilm_wash_df(data.frame(a = c(" caf00e9 ", "b"))))
  expect_null(attr(w, "not_utf8"))
})

test_that("ilm_trimws() trims such text byte by byte and keeps its bytes otherwise", {
  x <- c(" a ", paste0(" ", cafe, "\t"), NA, cafe)
  t <- illumex:::ilm_trimws(x)
  expect_identical(t[1], "a")
  expect_identical(bytes(t[c(2, 4)]), bytes(c(cafe, cafe)))
  expect_true(is.na(t[3]))
  expect_identical(illumex:::ilm_show_text(cafe), "caf<e9>")
})

test_that("a description of such a column completes and says so", {
  d <- data.frame(t = c("a", "A", " ", cafe, cafe), stringsAsFactors = FALSE)
  r <- ilm_describe(d, "t")
  expect_identical(r$n_empty, 1L)
  expect_identical(r$case_variants, 1L)          # "a" and "A"
  expect_match(r$note, "2 values not valid UTF-8 (another encoding?)", fixed = TRUE)
  f <- ilm_describe(data.frame(f = factor(c("x", cafe))), "f")
  expect_match(f$note, "1 value not valid UTF-8", fixed = TRUE)
  expect_no_error(ilm_describe_all(d))
})

test_that("plots draw such text with its stray bytes spelled out", {
  ## the pdf device crashes R on such text and png stops with an error, so
  ## draw to png: a regression fails here rather than ending the run
  grDevices::png(tempfile(fileext = ".png"))
  on.exit(grDevices::dev.off())
  set.seed(1)
  d <- data.frame(y = stats::rnorm(60), z = stats::rnorm(60),
                  g = sample(c("tea", cafe), 60, TRUE),
                  f = factor(sample(c("north", cafe), 60, TRUE)),
                  stringsAsFactors = FALSE)
  names(d)[2] <- paste0("z_", cafe)
  expect_no_error(ilm_plot(d, "g"))
  expect_no_error(ilm_plot(d, "y", "f"))
  expect_no_error(ilm_plot_bar(d, "f"))
  expect_no_error(ilm_plot_box(d, "y", "g"))
  expect_no_error(ilm_plot_scatter(d, "y", names(d)[2], by = "f"))
  expect_no_error(ilm_plot_all(d))
  expect_no_error(ilm_plot_missing(d))
  ## the caller's data are untouched
  expect_identical(bytes(names(d)[2]), bytes(paste0("z_", cafe)))
})

test_that("printing names such columns with their stray bytes spelled out", {
  d <- data.frame(a = c(1, NA, 3, 4), b = c(2, 3, NA, 5))
  names(d)[2] <- paste0("b_", cafe)
  expect_output(print(ilm_check_missing(d)), "b_caf<e9>", fixed = TRUE)
})

test_that("a model on columns so named stops and names them", {
  d <- data.frame(a = stats::rnorm(30), b = stats::rnorm(30), c = stats::rnorm(30))
  names(d)[2] <- paste0("b_", cafe)
  expect_error(ilm_reduce(d), "the column name b_caf<e9> is not valid UTF-8", fixed = TRUE)
  expect_error(ilm_reduce(d), "ilm_wash_df() cleans such names", fixed = TRUE)
  expect_error(ilm_glrm(d), "the column name b_caf<e9> is not valid UTF-8", fixed = TRUE)
  if (requireNamespace("isotree", quietly = TRUE))
    expect_error(ilm_anomaly(d, method = "iforest"),
                 "ilm_anomaly(): the column name b_caf<e9> is not valid UTF-8", fixed = TRUE)
  expect_no_error(suppressWarnings(ilm_reduce(ilm_wash_df(d))))
})

## ---- encoding = (Craig's item 298) --------------------------------------

test_that("without encoding, the warning names the argument that converts", {
  d <- data.frame(t = c("a", cafe), stringsAsFactors = FALSE)
  expect_warning(ilm_wash_df(d), "ilm_wash_df(data, encoding = \"windows-1252\") converts",
                 fixed = TRUE)
})

test_that("encoding converts the text that is not valid UTF-8, and says how much", {
  d <- data.frame(t = c("a", cafe, cafe, NA),
                  f = factor(c("north", cafe, "north", cafe)),
                  stringsAsFactors = FALSE)
  expect_message(w <- ilm_wash_df(d, encoding = "windows-1252"),
                 "converted from windows-1252 to UTF-8: t (2 values) and f (2 values).",
                 fixed = TRUE)
  expect_no_warning(suppressMessages(ilm_wash_df(d, encoding = "windows-1252")))
  expect_identical(w$t, c("a", "caf\u00e9", "caf\u00e9", NA))
  expect_true(all(validUTF8(w$t)))
  expect_identical(as.character(w$f), c("north", "caf\u00e9", "north", "caf\u00e9"))
  expect_identical(attr(w, "converted"),
                   data.frame(column = c("t", "f"), n = c(2L, 2L), stringsAsFactors = FALSE))
  expect_null(attr(w, "not_utf8"))
  ## latin1 names the same bytes
  w1 <- suppressMessages(ilm_wash_df(d, encoding = "latin1"))
  expect_identical(w1$t, w$t)
})

test_that("a factor level that converts to one already there joins it", {
  f <- factor(c("caf\u00e9", cafe, "tea"))
  w <- suppressMessages(ilm_wash_df(data.frame(f = f), encoding = "windows-1252"))
  expect_identical(levels(w$f), c("caf\u00e9", "tea"))
  expect_identical(as.integer(table(w$f)), c(2L, 1L))
})

test_that("valid text is never touched, and nothing is converted without encoding", {
  d <- data.frame(t = c("caf\u00e9", " x ", cafe), stringsAsFactors = FALSE)
  w <- suppressMessages(ilm_wash_df(d, encoding = "windows-1252"))
  expect_identical(w$t, c("caf\u00e9", "x", "caf\u00e9"))
  w0 <- suppressWarnings(ilm_wash_df(d))
  expect_identical(bytes(w0$t[3]), bytes(cafe))
})

test_that("a column name is converted too", {
  d <- data.frame(a = 1:2)
  names(d) <- paste0("note ", cafe)
  expect_message(w <- ilm_wash_df(d, encoding = "windows-1252"), "1 column name", fixed = TRUE)
  expect_identical(names(w), "note_cafe")
})

test_that("a value that does not convert is left as it is and counted", {
  ## e9 is no ASCII character: glibc's iconv() refuses it and Windows' maps
  ## it to "i", which does not convert back to e9; either way it stays
  d <- data.frame(t = c("a", cafe), stringsAsFactors = FALSE)
  expect_warning(w <- suppressMessages(ilm_wash_df(d, encoding = "ASCII")),
                 "t (1 value; row 2)", fixed = TRUE)
  expect_warning(suppressMessages(ilm_wash_df(d, encoding = "ASCII")),
                 "did not convert from ASCII", fixed = TRUE)
  expect_identical(bytes(w$t[2]), bytes(cafe))
  expect_identical(attr(w, "not_utf8")$n, 1L)
  expect_null(attr(w, "converted"))
})

test_that("an encoding iconv() does not know is an error, as is anything but one name", {
  d <- data.frame(t = cafe, stringsAsFactors = FALSE)
  expect_error(ilm_wash_df(d, encoding = "no-such-encoding"), "iconvlist()", fixed = TRUE)
  expect_error(ilm_wash_df(d, encoding = c("latin1", "windows-1252")), "the name of one encoding")
  expect_error(ilm_wash_df(d, encoding = NA_character_), "the name of one encoding")
})
