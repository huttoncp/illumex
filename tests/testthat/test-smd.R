# Standardised mean differences (R/ilm_smd.R), pinned by cases worked by hand:
# each group against the reference, over the square root of the average of
# the two groups' variances (Austin 2009); a category of more than two levels
# by Yang and Dalton's multivariate difference.

test_that("a number: the difference in means over the root of the average variance", {
  ## reference 1, 2, 3: mean 2, variance 1; group 2, 3, 4, 5: mean 3.5,
  ## variance 5/3. (3.5 - 2) / sqrt((1 + 5/3) / 2) = 1.5 / sqrt(4/3)
  v <- c(1, 2, 3, 2, 3, 4, 5)
  g <- c("r", "r", "r", "t", "t", "t", "t")
  expect_equal(ilm_smd(v, g, "r")[["t"]], 1.5 / sqrt(4 / 3))
  expect_equal(ilm_smd(v, g, "r")[["t"]], 1.299038105676658, tolerance = 1e-14)
  expect_true(is.na(ilm_smd(v, g, "r")[["r"]]))
  ## signed: the other way round it is negative
  expect_equal(ilm_smd(v, g, "t")[["r"]], -1.5 / sqrt(4 / 3))
  ## missing values are left out, group by group
  expect_equal(ilm_smd(c(v, NA), c(g, "t"), "r")[["t"]], 1.5 / sqrt(4 / 3))
})

test_that("a binary: the difference in proportions over the root of the average p(1 - p)", {
  ## reference 1 of 5 TRUE (0.2), group 3 of 5 (0.6):
  ## (0.6 - 0.2) / sqrt((0.2 * 0.8 + 0.6 * 0.4) / 2) = 0.4 / sqrt(0.2)
  v <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  g <- rep(c("r", "t"), each = 5)
  expect_equal(ilm_smd(v, g, "r")[["t"]], 0.4 / sqrt(0.2))
  expect_equal(ilm_smd(v, g, "r")[["t"]], 0.894427190999916, tolerance = 1e-14)
  ## a two-level factor is the same, by the share of its second level
  f <- factor(ifelse(v, "yes", "no"), levels = c("no", "yes"))
  expect_equal(ilm_smd(f, g, "r")[["t"]], 0.4 / sqrt(0.2))
})

test_that("a category of three levels: Yang and Dalton's multivariate difference", {
  ## reference shares (0.5, 0.3, 0.2), group (0.2, 0.3, 0.5), 10 rows each.
  ## Dropping the first level, T = (0.3 - 0.3, 0.5 - 0.2) = (0, 0.3).
  ## S_ref = [0.21, -0.06; -0.06, 0.16], S_grp = [0.21, -0.15; -0.15, 0.25],
  ## S = their average = [0.21, -0.105; -0.105, 0.205], det S = 0.032025,
  ## (S^-1)[2, 2] = 0.21 / 0.032025, so T' S^-1 T = 0.09 * 0.21 / 0.032025
  ## = 0.590164, and the difference is its square root, 0.768221.
  lev <- c("a", "b", "c")
  v <- factor(c(rep(lev, c(5, 3, 2)), rep(lev, c(2, 3, 5))), levels = lev)
  g <- rep(c("r", "t"), each = 10)
  expect_equal(ilm_smd(v, g, "r")[["t"]], sqrt(0.09 * 0.21 / 0.032025))
  expect_equal(ilm_smd(v, g, "r")[["t"]], 0.768221279597376, tolerance = 1e-14)
  ## unsigned, and the same either way round
  expect_equal(ilm_smd(v, g, "t")[["r"]], ilm_smd(v, g, "r")[["t"]])
  ## a level seen in neither group changes nothing
  v4 <- factor(as.character(v), levels = c(lev, "unseen"))
  expect_equal(ilm_smd(v4, g, "r")[["t"]], ilm_smd(v, g, "r")[["t"]])
  ## text sorts its levels the same way in every locale
  expect_equal(ilm_smd(as.character(v), g, "r")[["t"]], ilm_smd(v, g, "r")[["t"]])
})

test_that("with two levels the multivariate difference is the binary one, unsigned", {
  ## a level seen in neither group leaves two, and the binary formula
  lev <- c("a", "b", "c")
  v <- factor(c(rep("a", 4), rep("b", 6), rep("a", 7), rep("b", 3)), levels = lev)
  g <- rep(c("r", "t"), each = 10)
  p0 <- 0.6; p1 <- 0.3
  expect_equal(ilm_smd(v, g, "r")[["t"]], (p1 - p0) / sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2))
})

test_that("each group is compared with the reference, and nothing breaks at the edges", {
  set.seed(3)
  v <- stats::rnorm(90)
  g <- rep(c("r", "s", "t"), 30)
  s <- ilm_smd(v, g, "r")
  expect_named(s, c("r", "s", "t"))
  expect_equal(s[["s"]], as.numeric(ilm_smd_pair(v[g == "s"], v[g == "r"])))
  expect_true(all(is.na(ilm_smd(v, g, "absent"))))
  ## a constant variable has no spread to divide by
  expect_true(is.na(ilm_smd(rep(1, 6), rep(c("r", "t"), 3), "r")[["t"]]))
})

test_that("every kind agrees with tableone's SMDs, stored in a fixture", {
  ## dev/studies/make_smd_fixtures.R recorded tableone's (unsigned) SMDs for
  ## the cases in helper-smd-cases.R: numbers, a binary factor, and
  ## categories of three and five levels, with missing values
  ref <- utils::read.csv(test_path("fixtures", "smd_tableone.csv"), comment.char = "#",
                         stringsAsFactors = FALSE)
  cs <- smd_cases()
  got <- vapply(seq_len(nrow(ref)), function(i) {
    d <- cs[[ref$case[i]]]
    abs(ilm_smd(d[[ref$variable[i]]], d$g, "ref")[["trt"]])
  }, 0)
  expect_equal(got, ref$smd, tolerance = 1e-12)
})
