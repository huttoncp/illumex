# A floor or a ceiling (Craig's items 274 and 275): checked on every numeric
# variable with at least 5 distinct values, before the note's other checks;
# named when the count at the bound is significantly above the next value's
# (one-sided exact test at 0.001) and at least twice it; worded by the
# variable's kind, and first in the note.

note_of <- function(x) ilm_gauss_assess(x)$note
has <- function(note, text) grepl(text, note, fixed = TRUE)

test_that("a continuous floor or ceiling points to ilm_censor()", {
  set.seed(274)
  x <- pmax(stats::rlnorm(1000), 0.5)          # a detection limit at 0.5
  expect_true(has(note_of(x), "of values sit exactly at the minimum (0.5): a floor, see ilm_censor()"))
  expect_true(grepl("^[0-9]+% ", note_of(x)))
  y <- pmin(stats::rnorm(1000, 50, 10), 60)    # an instrument's ceiling
  expect_true(has(note_of(y), "of values sit exactly at the maximum (60): a ceiling, see ilm_censor()"))
})

test_that("a count's excess zeros point to a two-part or zero-inflated model", {
  set.seed(275)
  x <- ifelse(stats::runif(1000) < 0.3, 0, stats::rnbinom(1000, size = 3, mu = 15))
  expect_true(has(note_of(x),
                  "of values are 0, far more than at the next value: excess zeros, see a two-part or zero-inflated model"))
})

test_that("a rating's floor or ceiling says the scale cannot separate people there", {
  set.seed(276)
  r <- sample(1:7, 1000, TRUE, prob = c(0.30, 0.10, 0.15, 0.15, 0.15, 0.10, 0.05))
  expect_true(has(note_of(r),
    "of values sit at the scale's lowest point (1): it cannot separate people there, see an ordinal model"))
  ## the ceiling on a 1-5 item is kept
  l <- sample(1:5, 1000, TRUE, prob = c(0.05, 0.10, 0.15, 0.20, 0.50))
  expect_true(has(note_of(l), "of values sit at the scale's highest point (5)"))
  ## a 0-10 scale is a rating too
  z <- sample(0:10, 1000, TRUE, prob = c(0.25, rep(0.075, 10)))
  expect_true(has(note_of(z), "the scale's lowest point (0)"))
})

test_that("a count falling gently from zero, or a few distinct values, is no floor", {
  set.seed(277)
  g <- stats::rnbinom(1000, size = 1, mu = 10 / 3)   # P(0) 0.23 against P(1) 0.18
  expect_false(grepl("excess zeros|a floor", note_of(g)))
  four <- sample(0:3, 1000, TRUE, prob = c(0.7, 0.1, 0.1, 0.1))
  expect_false(grepl("excess zeros|a floor|lowest point", note_of(four)))
})

test_that("the floor is checked before the note's other gates, and comes first", {
  set.seed(278)
  ## too few values for a shape, but a pile all the same: 14 zeros against
  ## one 1 is P = 15 / 2^14 < 0.001 (12 against one, 0.0017, is not)
  small <- c(rep(0, 14), 1, 3, 5, 8, 13)
  expect_true(startsWith(note_of(small), "74% of values are 0"))
  expect_false(grepl("of values are 0", note_of(c(rep(0, 12), 1, 3, 5, 8, 13))))
  expect_true(endsWith(note_of(small), "n too small to assess"))
  ## ahead of the discrete reason
  x <- ifelse(stats::runif(1000) < 0.3, 0, sample(1:15, 1000, TRUE))
  expect_true(grepl("^[0-9]+% of values are 0.*; discrete", note_of(x)))
  expect_identical(ilm_gauss_assess(x)$kind, "floor")
})
