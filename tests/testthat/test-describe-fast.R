## fast = TRUE leaves out the costly parts of a description: the Gaussian
## index and the numeric floor and ceiling checks (as gauss = "none" does),
## the dispersion column (as dispersion = FALSE does), and a date-time's
## spacing, whose columns are NA and whose note then says nothing about
## spacing. Everything still worked out is identical to the default's; an
## explicit gauss or dispersion wins over fast. The data helper is
## helper-describe-speed.R's.

tables_of <- function(x) if (is.data.frame(x)) list(x) else x
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
    expect_identical(fst$categorical, def$categorical)
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
