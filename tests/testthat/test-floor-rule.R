# A floor or a ceiling (Craig's items 274 and 275): checked on every numeric
# variable with at least 5 distinct values, or 4 on a short scale (item
# 292), before the note's other checks; named when the count at the bound
# is significantly above the next value's (one-sided exact test at 0.001)
# and at least twice it; worded by the variable's kind, and first in the
# note.

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
                  "% are 0, far above the next value: excess zeros, see a two-part or zero-inflated model"))
  expect_false(has(note_of(x), "if a rating"))
})

test_that("a rating's floor or ceiling says the scale cannot separate people there", {
  set.seed(276)
  r <- sample(1:7, 1000, TRUE, prob = c(0.30, 0.10, 0.15, 0.15, 0.15, 0.10, 0.05))
  expect_true(has(note_of(r),
    "of values sit at the scale's lowest point (1): it cannot separate people there, see an ordinal model"))
  ## the ceiling on a 1-5 item is kept
  l <- sample(1:5, 1000, TRUE, prob = c(0.05, 0.10, 0.15, 0.20, 0.50))
  expect_true(has(note_of(l), "of values sit at the scale's highest point (5)"))
  ## a scale centred on its neutral point, -3 to 3, is a rating, at its
  ## floor and at its ceiling
  lo <- sample(-3:3, 1000, TRUE, prob = c(0.30, 0.10, 0.15, 0.15, 0.15, 0.10, 0.05))
  expect_true(has(note_of(lo),
    "of values sit at the scale's lowest point (-3): it cannot separate people there, see an ordinal model"))
  hi <- sample(-3:3, 1000, TRUE, prob = rev(c(0.30, 0.10, 0.15, 0.15, 0.15, 0.10, 0.05)))
  expect_true(has(note_of(hi), "of values sit at the scale's highest point (3)"))
  ## a 1-4 item, at the gate of 4 distinct values
  f <- sample(1:4, 1000, TRUE, prob = c(0.1, 0.1, 0.2, 0.6))
  expect_true(has(note_of(f), "of values sit at the scale's highest point (4)"))
  ## negative values not centred on 0 are read as continuous
  a <- sample(-2:5, 1000, TRUE, prob = c(0.3, rep(0.1, 7)))
  expect_true(has(note_of(a), "of values sit exactly at the minimum (-2): a floor, see ilm_censor()"))
})

test_that("whole numbers from 0 to at most 10 name both readings (item 292)", {
  set.seed(292)
  ## a 0-3 PHQ-9 item and a 0-3 count of visits look alike: both readings,
  ## at the floor and at the ceiling
  phq <- sample(0:3, 1000, TRUE, prob = c(0.6, 0.15, 0.15, 0.1))
  expect_true(startsWith(note_of(phq), sprintf(
    "%s%% are 0, far above the next value: excess zeros if a count (see a two-part or zero-inflated model); if a rating, it can't separate people there (see an ordinal model)",
    ilm_fx(100 * mean(phq == 0), 0))))
  visits <- sample(0:3, 1000, TRUE, prob = c(0.1, 0.1, 0.2, 0.6))
  expect_true(startsWith(note_of(visits), sprintf(
    "%s%% are 3, far above the next value: a cap if a count (see ilm_censor()); if a rating, it can't separate people there (see an ordinal model)",
    ilm_fx(100 * mean(visits == 3), 0))))
  ## so does 0 to 10
  z <- sample(0:10, 1000, TRUE, prob = c(0.25, rep(0.075, 10)))
  expect_true(has(note_of(z), "are 0, far above the next value: excess zeros if a count"))
  ## past 10 it is a count
  w <- sample(0:11, 1000, TRUE, prob = c(0.25, rep(0.75 / 11, 11)))
  expect_true(has(note_of(w), "are 0, far above the next value: excess zeros, see"))
})

test_that("an ordered factor's floor or ceiling leads its note, by the level's label", {
  set.seed(293)
  lv <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
  o <- data.frame(item = factor(sample(lv, 1000, TRUE, prob = c(0.35, 0.1, 0.2, 0.2, 0.15)),
                                levels = lv, ordered = TRUE))
  expect_true(startsWith(ilm_describe(o, "item")$note, sprintf(
    "%s%% of values sit at the scale's lowest point (Strongly disagree): it cannot separate people there, see an ordinal model",
    ilm_fx(100 * mean(o$item == "Strongly disagree"), 0))))
  ## a 0-10 scale stored as an ordered factor is a rating alone
  p <- data.frame(pain = factor(sample(0:10, 1000, TRUE, prob = c(0.25, rep(0.075, 10))),
                                levels = 0:10, ordered = TRUE))
  expect_true(startsWith(ilm_describe(p, "pain")$note, sprintf(
    "%s%% of values sit at the scale's lowest point (0): it cannot separate people",
    ilm_fx(100 * mean(p$pain == "0"), 0))))
  ## an unordered factor has no floor
  u <- data.frame(item = factor(o$item, ordered = FALSE))
  expect_false(grepl("lowest point", ilm_describe(u, "item")$note))
  ## nor does a 3-level ordered factor
  t3 <- data.frame(item = factor(sample(c("low", "mid", "high"), 1000, TRUE, prob = c(0.7, 0.2, 0.1)),
                                 levels = c("low", "mid", "high"), ordered = TRUE))
  expect_false(grepl("lowest point", ilm_describe(t3, "item")$note))
})

test_that("a count falling gently from zero, or a 3-point scale, is no floor", {
  set.seed(277)
  g <- stats::rnbinom(1000, size = 1, mu = 10 / 3)   # P(0) 0.23 against P(1) 0.18
  expect_false(grepl("excess zeros|a floor", note_of(g)))
  ## a pile at the end of a 3-point scale, or of 0/1, is only its coarseness
  for (v in list(sample(0:2, 1000, TRUE, prob = c(0.7, 0.2, 0.1)),
                 sample(-1:1, 1000, TRUE, prob = c(0.7, 0.2, 0.1)),
                 sample(0:1, 1000, TRUE, prob = c(0.9, 0.1))))
    expect_false(grepl("excess zeros|a floor|lowest point|are 0", note_of(v)))
  ## 4 distinct values that are no short scale stay below the gate of 5
  s4 <- sample(c(0, 20, 40, 60), 1000, TRUE, prob = c(0.7, 0.1, 0.1, 0.1))
  expect_false(grepl("are 0|a floor", note_of(s4)))
})

test_that("the floor is checked before the note's other gates, and comes first", {
  set.seed(278)
  ## too few values for a shape, but a pile all the same: 14 zeros against
  ## one 1 is P = 15 / 2^14 < 0.001 (12 against one, 0.0017, is not)
  small <- c(rep(0, 14), 1, 3, 5, 8, 13)
  expect_true(startsWith(note_of(small), "74% are 0, far above the next value"))
  expect_false(grepl("are 0, far above", note_of(c(rep(0, 12), 1, 3, 5, 8, 13))))
  expect_true(endsWith(note_of(small), "n too small to assess"))
  ## ahead of the discrete reason
  x <- ifelse(stats::runif(1000) < 0.3, 0, sample(1:15, 1000, TRUE))
  expect_true(grepl("^[0-9]+% are 0, far above.*; discrete", note_of(x)))
  expect_identical(ilm_gauss_assess(x)$kind, "floor")
})
