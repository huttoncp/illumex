## fast = TRUE leaves out the costly parts of a description: the Gaussian
## index and the numeric floor and ceiling checks (as gauss = "none" does),
## the dispersion column (as dispersion = FALSE does), and a date-time's
## spacing, whose columns are NA and whose note then says nothing about
## spacing, and a text column's count of case variants, whose column is left
## out and whose sentence leaves the note. Tied top counts come in code-point
## order rather than dictionary order. Everything else still worked out is
## identical to the default's; an explicit gauss or dispersion wins over
## fast. The data helper is helper-describe-speed.R's.

tables_of <- function(x) if (is.data.frame(x)) list(x) else x

## Code-point order worked out without the code under test, and without the
## locale: each value written as its code points in fixed-width hexadecimal,
## so that the keys compare alike under any collation.
cp_key <- function(s) vapply(s, function(v) paste(sprintf("%08x", utf8ToInt(v)), collapse = ""),
                             "", USE.NAMES = FALSE)
## the top three counts as the categorical table writes them, most common
## first, values with the same count in code-point order
top_cp <- function(v, top = 3L) {
  v <- as.character(v); v <- v[!is.na(v)]
  if (!length(v)) return("")
  tb <- table(v); k <- as.integer(tb); u <- names(tb)
  o <- order(-k, cp_key(u))[seq_len(min(top, length(u)))]
  paste(sprintf("%s_%d", u[o], k[o]), collapse = ", ")
}
## The default's categorical table with what fast changes in it set aside:
## no case_variants, the top counts worked out again by code point, and the
## note left as fast gives it (the next test checks the note). Groups of two
## or more by columns are named as the table names them, "p.u".
cat_fast_expected <- function(def, d, b, fst) {
  e <- as.data.frame(def); e$case_variants <- NULL
  key <- if (is.null(b)) NULL else do.call(paste, c(unname(d[b]), sep = "."))
  ok <- if (is.null(b)) NULL else Reduce(`&`, lapply(b, function(bb) !is.na(d[[bb]])))
  gcol <- paste(b, collapse = ".")
  for (i in seq_len(nrow(e))) {
    rows <- if (is.null(b)) rep(TRUE, nrow(d)) else ok & key == e[[gcol]][i]
    e$counts_tb[i] <- top_cp(d[[e$variable[i]]][rows])
  }
  e$note <- as.data.frame(fst)$note
  e
}
spacing_cols <- c("spacing", "regular", "n_gaps")

test_that("fast = FALSE is the default", {
  d <- describe_speed_data()
  expect_identical(suppressMessages(ilm_describe_all(d, by = "g3", fast = FALSE)),
                   suppressMessages(ilm_describe_all(d, by = "g3")))
})

test_that("fast = TRUE is the two switches for numbers, and the default elsewhere", {
  d <- describe_speed_data()
  for (b in list(NULL, "g3", c("g2", "g3"))) {
    def <- suppressMessages(ilm_describe_all(d, by = b))
    off <- suppressMessages(ilm_describe_all(d, by = b, gauss = "none", dispersion = FALSE))
    fst <- suppressMessages(ilm_describe_all(d, by = b, fast = TRUE))
    expect_identical(names(fst), names(def))
    expect_identical(fst$numeric, off$numeric)
    expect_identical(as.data.frame(fst$categorical),
                     cat_fast_expected(def$categorical, d, b, fst$categorical))
    expect_identical(attributes(fst$categorical)[setdiff(names(attributes(fst$categorical)), "names")],
                     attributes(def$categorical)[setdiff(names(attributes(def$categorical)), "names")])
    expect_identical(fst$logical, def$logical)
    expect_identical(fst$constant, def$constant)
    expect_false(any(c("gauss", "ks_d", "gauss_note", "dispersion") %in% names(fst$numeric)))
  }
})

test_that("under fast a date-time's spacing is NA and its note says nothing of it; a date's stays", {
  d <- describe_speed_data()
  for (b in list(NULL, "g3")) {
    def <- as.data.frame(suppressMessages(ilm_describe_all(d, by = b))$time)
    fst <- as.data.frame(suppressMessages(ilm_describe_all(d, by = b, fast = TRUE))$time)
    expect_identical(names(fst), names(def))
    day <- fst$variable == "day"; when <- fst$variable == "when"
    expect_identical(fst[day, ], def[day, ])
    expect_true(all(is.na(fst$spacing[when])) && all(is.na(fst$regular[when])) &&
                  all(is.na(fst$n_gaps[when])))
    expect_identical(class(fst$spacing), class(def$spacing))
    expect_identical(class(fst$regular), class(def$regular))
    expect_identical(class(fst$n_gaps), class(def$n_gaps))
    other <- setdiff(names(def), c(spacing_cols, "note"))
    expect_identical(fst[when, other], def[when, other])
    expect_false(any(grepl("spacing|gaps", fst$note[when])))
    ## the note's other sentences stay
    rep_def <- grepl("repeated dates", def$note[when]); rep_fst <- grepl("repeated dates", fst$note[when])
    expect_identical(rep_fst, rep_def)
  }
  ## and for one date-time described alone
  r <- ilm_describe(d, "when", fast = TRUE)
  expect_true(is.na(r$spacing) && is.na(r$regular) && is.na(r$n_gaps))
  expect_identical(ilm_describe(d, "day", fast = TRUE), ilm_describe(d, "day"))
})

test_that("an explicit gauss or dispersion wins over fast", {
  d <- describe_speed_data()
  def <- suppressMessages(ilm_describe_all(d, by = "g2"))
  g <- suppressMessages(ilm_describe_all(d, by = "g2", fast = TRUE, gauss = "index"))
  expect_true(all(c("gauss", "gauss_note") %in% names(g$numeric)))
  expect_identical(g$numeric$gauss, def$numeric$gauss)
  expect_false("dispersion" %in% names(g$numeric))
  dd <- suppressMessages(ilm_describe_all(d, by = "g2", fast = TRUE, dispersion = TRUE))
  expect_identical(dd$numeric$dispersion, def$numeric$dispersion)
  expect_false("gauss" %in% names(dd$numeric))
  one <- ilm_describe(d, "num", fast = TRUE, gauss = "both")
  expect_true(all(c("gauss", "ks_d") %in% names(one)))
})

test_that("fast works with one column, several, notes and by", {
  d <- describe_speed_data()
  expect_identical(ilm_describe(d, "num", by = "g3", fast = TRUE),
                   ilm_describe(d, "num", by = "g3", gauss = "none", dispersion = FALSE))
  expect_identical(suppressMessages(ilm_describe(d, c("num", "inf"), by = "g2", fast = TRUE)),
                   suppressMessages(ilm_describe(d, c("num", "inf"), by = "g2", gauss = "none",
                                                 dispersion = FALSE)))
  both <- suppressMessages(ilm_describe_all(d, by = "g3", fast = TRUE, notes = FALSE))
  expect_false(any(c("note", "gauss_note") %in% unlist(lapply(both, names))))
  expect_true(all(is.na(both$time$spacing[both$time$variable == "when"])))
})

test_that("fast takes TRUE or FALSE only", {
  d <- describe_speed_data()
  for (bad in list(NA, "yes", 1, c(TRUE, TRUE), NULL))
    expect_error(ilm_describe(d, "num", fast = bad), "`fast` must be TRUE or FALSE")
  expect_error(ilm_describe_all(d, fast = "yes"), "`fast` must be TRUE or FALSE")
})

test_that("under fast a text column has no case-variant count, and its note says nothing of them", {
  d <- data.frame(t = c("North", "north", "North ", "South", "south", "East", ""),
                  stringsAsFactors = FALSE)
  def <- ilm_describe(d, "t"); fst <- ilm_describe(d, "t", fast = TRUE)
  expect_identical(def$case_variants, 2L)
  expect_true(grepl("case/space variants", def$note))
  expect_false("case_variants" %in% names(fst))
  expect_identical(names(fst), setdiff(names(def), "case_variants"))
  expect_false(grepl("case", fst$note))
  ## the sentences after it move up: two are shown, as without fast
  expect_identical(fst$note, "1 empty strings (not NA); 7 rare levels (separation risk)")
  expect_identical(def$note, "1 empty strings (not NA); 2 case/space variants")
  same <- setdiff(names(fst), c("counts_tb", "note"))
  expect_identical(fst[same], def[same])
  ## and in ilm_describe_all()'s categorical table, with and without by
  dd <- describe_speed_data()
  for (b in list(NULL, "g3"))
    expect_false("case_variants" %in% names(suppressMessages(ilm_describe_all(dd, by = b, fast = TRUE))$categorical))
})

test_that("under fast tied top counts come in code-point order, the default's in dictionary order", {
  ## each value once, so all are tied; dictionary and code-point order differ
  ## (capitals before small letters, "_" and accented letters elsewhere)
  v <- c("b", "B", "a", "A", "_", "é", "e", "Z")
  d <- data.frame(t = c(v, "e", "e", "b"), f = factor(c(v, "e", "e", "b")),
                  stringsAsFactors = FALSE)
  for (col in c("t", "f")) {
    fst <- ilm_describe(d, col, fast = TRUE); def <- ilm_describe(d, col)
    expect_identical(fst$counts_tb, top_cp(d[[col]]))
    expect_identical(fst$counts_tb, "e_3, b_2, A_1")
    ## the default keeps table()'s order for ties, the locale's
    tb <- sort(table(as.character(d[[col]])), decreasing = TRUE)
    expect_identical(def$counts_tb, paste(sprintf("%s_%d", names(tb)[1:3], as.integer(tb)[1:3]),
                                          collapse = ", "))
  }
  ## all distinct: the top three are the first three by code point
  a <- data.frame(t = v, stringsAsFactors = FALSE)
  expect_identical(ilm_describe(a, "t", fast = TRUE)$counts_tb, "A_1, B_1, Z_1")
  ## by group
  g <- data.frame(t = c(v, v), k = rep(c("p", "q"), each = length(v)), stringsAsFactors = FALSE)
  r <- ilm_describe(g, "t", by = "k", fast = TRUE)
  expect_identical(r$counts_tb, rep("A_1, B_1, Z_1", 2))
})

test_that("under fast the top counts are the same under two collation settings", {
  v <- c("b", "B", "a", "A", "_", "é", "e", "Z", "É", "z")
  d <- data.frame(t = c(v, "e", "e", "b", "B"), stringsAsFactors = FALSE)
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
  here <- ilm_describe(d, "t", fast = TRUE)
  ok <- suppressWarnings(Sys.setlocale("LC_COLLATE", "C"))
  skip_if(identical(ok, ""), "the C collation could not be set")
  in_c <- ilm_describe(d, "t", fast = TRUE)
  Sys.setlocale("LC_COLLATE", old)
  expect_identical(in_c, here)
  expect_identical(here$counts_tb, "e_3, B_2, b_2")
})

test_that("under fast the categorical columns other than the top counts and note are the default's", {
  d <- describe_speed_data()
  for (b in list(NULL, "g3", c("g2", "g3"))) {
    def <- as.data.frame(suppressMessages(ilm_describe_all(d, by = b))$categorical)
    fst <- as.data.frame(suppressMessages(ilm_describe_all(d, by = b, fast = TRUE))$categorical)
    same <- setdiff(names(def), c("case_variants", "counts_tb", "note"))
    expect_identical(fst[same], def[same])
    ## the note: as the default's where no case variants were found; where
    ## some were, without that sentence, and with the default's sentences
    ## before it still first
    none <- def$case_variants == 0
    expect_identical(fst$note[none], def$note[none])
    expect_false(any(grepl("case/space", fst$note)))
    before <- sub("(; )?[0-9]+ case/space variants.*$", "", def$note[!none])
    expect_true(all(startsWith(fst$note[!none], before)))
  }
})

test_that("under fast tied values marked latin1 come in code-point order, alone or beside UTF-8", {
  ## distinct and tied: by code point a-grave (224), e-acute (233), u-umlaut (252)
  utf <- c("é", "à", "ü")
  lat <- iconv(utf, "UTF-8", "latin1")
  expect_identical(Encoding(lat), rep("latin1", 3))
  want <- "à_1, é_1, ü_1"
  mixed <- c(utf[1], lat[2], utf[3])
  for (v in list(utf, lat, mixed)) {
    r <- ilm_describe(data.frame(t = v, stringsAsFactors = FALSE), "t", fast = TRUE)
    expect_identical(enc2utf8(r$counts_tb), want)
    expect_identical(enc2utf8(r$counts_tb), top_cp(enc2utf8(v)))
  }
  ## four tied at the cut of three, two marked latin1: which three are shown
  ## depends on the order. By code point a-grave (224), a-umlaut (228),
  ## e-acute (233), u-umlaut (252), so u-umlaut is the one left out
  cut <- c("é", iconv("à", "UTF-8", "latin1"), "ü", iconv("ä", "UTF-8", "latin1"))
  r <- ilm_describe(data.frame(t = cut, stringsAsFactors = FALSE), "t", fast = TRUE)
  expect_identical(enc2utf8(r$counts_tb), "à_1, ä_1, é_1")
  ## a leader, then ties across marks
  v <- c(lat, utf[1], "b", "b", "b")
  r <- ilm_describe(data.frame(t = v, stringsAsFactors = FALSE), "t", fast = TRUE)
  expect_identical(enc2utf8(r$counts_tb), "b_3, é_2, à_1")
})
