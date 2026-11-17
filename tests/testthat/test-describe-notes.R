## notes = FALSE leaves out the columns of sentences -- gauss_note in the
## numeric table, note in the categorical, logical and time tables -- and
## changes nothing else. The data helper is helper-describe-speed.R's.

note_cols <- c("gauss_note", "note")

expect_without_notes <- function(with, without) {
  with <- as.data.frame(with); without <- as.data.frame(without)
  expect_identical(names(without), setdiff(names(with), note_cols))
  for (v in names(without)) expect_identical(without[[v]], with[[v]], info = v)
}

test_that("notes = FALSE drops the note columns of every table, and nothing else", {
  d <- describe_speed_data()
  for (b in list(NULL, "g3", c("g2", "g3"))) {
    on <- suppressMessages(ilm_describe_all(d, by = b))
    off <- suppressMessages(ilm_describe_all(d, by = b, notes = FALSE))
    expect_identical(names(off), names(on))
    for (k in names(on)) expect_without_notes(on[[k]], off[[k]])
    expect_true(all(c("gauss_note", "note") %in% unlist(lapply(on, names))))
    expect_false(any(note_cols %in% unlist(lapply(off, names))))
  }
  ## with the Gaussian index off there is no gauss_note to drop
  on <- suppressMessages(ilm_describe_all(d, by = "g2", gauss = "none"))
  off <- suppressMessages(ilm_describe_all(d, by = "g2", gauss = "none", notes = FALSE))
  for (k in names(on)) expect_without_notes(on[[k]], off[[k]])
})

test_that("ilm_describe() takes notes, for one column and several", {
  d <- describe_speed_data()
  expect_without_notes(ilm_describe(d, "num", by = "g2"),
                       ilm_describe(d, "num", by = "g2", notes = FALSE))
  expect_without_notes(ilm_describe(d, "fac"), ilm_describe(d, "fac", notes = FALSE))
  expect_without_notes(ilm_describe(d, "when", by = "g3"),
                       ilm_describe(d, "when", by = "g3", notes = FALSE))
  expect_without_notes(suppressMessages(ilm_describe(d, c("num", "inf"), by = "g3")),
                       suppressMessages(ilm_describe(d, c("num", "inf"), by = "g3", notes = FALSE)))
  expect_without_notes(ilm_describe(d$lgl), ilm_describe(d$lgl, notes = FALSE))
})

test_that("notes = TRUE is the default and gives what the record holds", {
  d <- describe_speed_data()
  expect_identical(ilm_describe_all(d, by = "g2", notes = TRUE),
                   suppressMessages(ilm_describe_all(d, by = "g2")))
})

test_that("notes takes TRUE or FALSE only", {
  d <- describe_speed_data()
  for (bad in list(NA, "no", 0, c(TRUE, FALSE), NULL))
    expect_error(ilm_describe(d, "num", notes = bad), "`notes` must be TRUE or FALSE")
  expect_error(ilm_describe_all(d, notes = "no"), "`notes` must be TRUE or FALSE")
})

test_that("the message on infinite values is the same with notes off", {
  d <- describe_speed_data()
  m <- function(...) {
    out <- character(0)
    withCallingHandlers(ilm_describe_all(d, by = "g3", ...), message = function(e) {
      out <<- c(out, conditionMessage(e)); invokeRestart("muffleMessage") })
    out
  }
  expect_identical(m(notes = FALSE), m())
})
