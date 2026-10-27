## ilm_vtest_share(): tests written with the implementation,
## from its specification's hand-worked cases (Lebart, Piron and Morineau
## 2006; Husson, Le and Pages 2017, p. 157).

## The published example (Husson, Le and Pages 2017, p. 157, Table 3.7):
## n = 300 rows, n_j = 30 with the level (specialist shop), n_k = 53 in the
## cluster (luxury), x = 21 in both.
husson_m <- rep(c(TRUE, FALSE), c(53, 247))
husson_hit <- c(rep(TRUE, 21), rep(FALSE, 32),   # the 53 cluster rows
                rep(TRUE, 9), rep(FALSE, 238))   # the 247 other rows

test_that("(a) the Husson et al. example gives 6.639", {
  # x / n_k = 21 / 53 = 0.396 > n_j / n = 30 / 300 = 0.1: over-represented.
  # E(N) = 53 * 30 / 300 = 5.3. P(N >= 21) = 1.580e-11 (full upper tail,
  # observed count included), and the z with P(Z >= z) = 1.580e-11 is
  # 6.639, matching the published 6.64.
  z <- ilm_vtest_share(husson_hit, husson_m)
  expect_type(z, "double")
  expect_length(z, 1L)
  expect_lt(abs(z - 6.639), 1e-3)
  # A mid-p tail would give 6.733; the full tail must not.
  expect_gt(abs(z - 6.733), 0.05)
})

test_that("(b) the mirror of the Husson et al. example gives -6.639", {
  # Swap the level and its complement: n_j = 270, x = 53 - 21 = 32.
  # x / n_k = 32 / 53 = 0.604 < 270 / 300 = 0.9: under-represented.
  # N' = 53 - N, so P(N' <= 32) = P(N >= 21) = 1.580e-11, and the z with
  # P(Z <= z) = 1.580e-11 is -6.639.
  z <- ilm_vtest_share(!husson_hit, husson_m)
  expect_lt(abs(z - (-6.639)), 1e-3)
  expect_equal(z, -ilm_vtest_share(husson_hit, husson_m))
})

test_that("(c) n = 6, n_k = 3, n_j = 2 gives +/- 0.8416", {
  # choose(6, 3) = 20 ways to draw the cluster.
  # x = 2: P(N = 2) = choose(2, 2) * choose(4, 1) / 20 = 4 / 20 = 0.2, and
  # 2 is the largest possible count, so P(N >= 2) = 0.2;
  # z = qnorm(0.8) = 0.8416.
  hit <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  m_over <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  z_over <- ilm_vtest_share(hit, m_over)
  expect_equal(z_over, qnorm(0.8))
  expect_lt(abs(z_over - 0.8416), 1e-4)

  # x = 0: P(N = 0) = choose(2, 0) * choose(4, 3) / 20 = 4 / 20 = 0.2, the
  # whole lower tail, so P(N <= 0) = 0.2; z = qnorm(0.2) = -0.8416.
  m_under <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  z_under <- ilm_vtest_share(hit, m_under)
  expect_equal(z_under, qnorm(0.2))
  expect_lt(abs(z_under - (-0.8416)), 1e-4)
})

test_that("(d) x at its expectation gives 0", {
  # n = 6, n_k = 3, n_j = 2, x = 1: x / n_k = 1 / 3 = n_j / n = 2 / 6.
  hit <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  m <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  expect_identical(ilm_vtest_share(hit, m), 0)

  # n = 300, n_k = 50, n_j = 30, x = 5: 5 / 50 = 30 / 300 = 0.1.
  m2 <- rep(c(TRUE, FALSE), c(50, 250))
  hit2 <- c(rep(TRUE, 5), rep(FALSE, 45), rep(TRUE, 25), rep(FALSE, 225))
  expect_identical(ilm_vtest_share(hit2, m2), 0)
})

test_that("(e) each NA case returns NA", {
  hit <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)

  # n_k < 2: an empty cluster, and a one-row cluster.
  expect_identical(ilm_vtest_share(hit, rep(FALSE, 6)), NA_real_)
  expect_identical(
    ilm_vtest_share(hit, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    NA_real_
  )

  # n_k = n: the cluster is every row.
  expect_identical(ilm_vtest_share(hit, rep(TRUE, 6)), NA_real_)

  # n_j = 0: the level is absent.
  m <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  expect_identical(ilm_vtest_share(rep(FALSE, 6), m), NA_real_)

  # n_j = n: the level is every row's.
  expect_identical(ilm_vtest_share(rep(TRUE, 6), m), NA_real_)

  # Fewer than 2 rows after dropping missing `hit`: one row left, none
  # left, and an empty input.
  expect_identical(
    ilm_vtest_share(c(TRUE, NA, NA), c(TRUE, FALSE, TRUE)),
    NA_real_
  )
  expect_identical(
    ilm_vtest_share(c(NA, NA, NA), c(TRUE, FALSE, TRUE)),
    NA_real_
  )
  expect_identical(ilm_vtest_share(logical(0), logical(0)), NA_real_)

  # n_k becomes too small once the missing rows are dropped: of the three
  # cluster rows, two have a missing level, leaving n_k = 1.
  expect_identical(
    ilm_vtest_share(c(TRUE, NA, NA, TRUE, FALSE, FALSE),
                    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)),
    NA_real_
  )
})

test_that("rows with a missing hit are dropped before counting", {
  # The Husson et al. example with 20 extra rows whose level is missing,
  # half of them in the cluster: the result must not change.
  hit <- c(husson_hit, rep(NA, 20))
  m <- c(husson_m, rep(c(TRUE, FALSE), 10))
  expect_equal(ilm_vtest_share(hit, m),
               ilm_vtest_share(husson_hit, husson_m))
  expect_lt(abs(ilm_vtest_share(hit, m) - 6.639), 1e-3)

  # The n = 6 case of (c) with missing levels interleaved.
  hit6 <- c(TRUE, NA, TRUE, FALSE, NA, FALSE, FALSE, FALSE)
  m6 <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  expect_equal(ilm_vtest_share(hit6, m6), qnorm(0.8))
})

test_that("a very extreme case returns a large finite value", {
  # n = 20000, n_k = n_j = x = 10000: the cluster is exactly the rows with
  # the level. P(N >= 10000) = 1 / choose(20000, 10000), about
  # exp(-13858), far below the smallest double, so only the log-scale
  # computation keeps it finite. The value is about 166.4; the header's
  # bound is sqrt(2 * n * log(2)) = 166.5.
  n <- 20000
  hit <- rep(c(TRUE, FALSE), c(10000, 10000))
  m <- hit
  z <- ilm_vtest_share(hit, m)
  expect_true(is.finite(z))
  expect_gt(z, 100)
  expect_lte(z, sqrt(2 * n * log(2)))

  # The mirror: the cluster has none of the level's rows.
  z_under <- ilm_vtest_share(hit, !m)
  expect_true(is.finite(z_under))
  expect_lt(z_under, -100)
  expect_gte(z_under, -sqrt(2 * n * log(2)))
})
