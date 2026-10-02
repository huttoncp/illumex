# The describe layer. These check behaviour a user would notice, and in a few
# places they pin bugs that were found by testing rather than by inspection.

test_that("numeric output carries the documented columns", {
  d <- ilm_sim(n_id = 20)
  r <- ilm_describe(d, "score")
  expect_true(all(c("obs", "n", "na", "mean", "sd", "se", "p0", "p50", "p100",
                    "gauss", "gauss_note") %in% names(r)))
  expect_equal(nrow(r), 1L)
  # removed on purpose: p_na belongs to ilm_describe_na(), cases became obs
  expect_false(any(c("p_na", "cases") %in% names(r)))
  # off by default, since gauss and gauss_note say whether they are needed
  expect_false(any(c("skew", "kurt") %in% names(r)))
})

test_that("digits reaches the quantile columns when printed, and the values stay whole", {
  # it did not: every other numeric column honoured digits and the percentiles
  # printed unrounded, which is only visible on a large-scale variable. The
  # values are now kept whole and `digits` is how they print.
  d <- ilm_sim(n_id = 20)
  r <- ilm_describe(d, "income", digits = 1)
  shown <- ilm_describe_rounded(r)
  q <- unname(stats::quantile(d$income, c(0, 0.5, 1), na.rm = TRUE))
  for (i in 1:3) {
    cn <- c("p0", "p50", "p100")[i]
    expect_equal(shown[[cn]], round(r[[cn]], 1), info = cn)
    expect_equal(r[[cn]], q[i], info = cn)
  }
})

test_that("probs drives both the quantiles and the column names", {
  d <- ilm_sim(n_id = 20)
  r <- ilm_describe(d, "score", probs = c(0.025, 0.25, 0.5, 0.975))
  expect_true(all(c("p2.5", "p25", "p50", "p97.5") %in% names(r)))
  expect_false("p0" %in% names(r))
  expect_equal(unname(r$p50), unname(stats::median(d$score)), tolerance = 1e-6)
})

test_that("skew and kurt appear only when asked for", {
  d <- ilm_sim(n_id = 20)
  r <- ilm_describe(d, "score", skew = TRUE, kurt = TRUE)
  expect_true(all(c("skew", "kurt") %in% names(r)))
})

test_that("dispersion separates Poisson from negative binomial", {
  d <- ilm_sim()
  r <- ilm_describe_all(d, class = "numeric")
  disp <- setNames(r$dispersion, r$variable)
  expect_lt(abs(disp[["visits"]] - 1), 0.6)   # Poisson: variance ~ mean
  expect_gt(disp[["claims"]], 2)              # negative binomial: overdispersed
  expect_true(is.na(disp[["score"]]))         # continuous: not a count
})

test_that("a variable that is not a count gets no dispersion", {
  # an earlier rule keyed on support starting near zero gave 1-100 a ratio and
  # denied 101-200 one, though they are the same kind of variable
  d <- data.frame(a = as.integer(sample(1:100, 400, TRUE)),
                  b = as.integer(sample(101:200, 400, TRUE)),
                  c = rnorm(400))
  r <- ilm_describe_all(d, class = "numeric")
  disp <- setNames(r$dispersion, r$variable)
  expect_false(is.na(disp[["a"]]))
  expect_false(is.na(disp[["b"]]))            # treated the same as a
  expect_true(is.na(disp[["c"]]))
})

test_that("constant columns are split into their own section", {
  d <- ilm_sim(n_id = 20)
  r <- ilm_describe_all(d)
  expect_true("constant" %in% names(r))
  expect_true("cohort" %in% r$constant$variable)
  # and they do not appear in the ordinary sections
  expect_false("cohort" %in% r$categorical$variable)
  expect_true(all(c("variable", "class", "obs", "n", "na", "value") %in%
                    names(r$constant)))
})

test_that("categorical checks fire on the things that break models", {
  d <- ilm_sim()
  r <- ilm_describe_all(d, class = "categorical")
  g <- r[r$variable == "grp", ]
  s <- r[r$variable == "site", ]
  expect_equal(g$n_unused, 1L)          # a declared but unobserved level
  expect_gte(g$n_rare, 1L)              # a level too thin to estimate
  expect_gt(s$n_empty, 0L)              # "" is not NA
  expect_gte(s$case_variants, 1L)       # North / north / "North "
  expect_false("id_like" %in% names(r)) # moved to ilm_frame_issues()
})

test_that("a near-constant logical is flagged as a separation risk", {
  d <- ilm_sim()
  r <- ilm_describe_all(d, class = "logical")
  expect_match(r$note[r$variable == "consented"], "separation risk")
  expect_equal(r$note[r$variable == "flag"], "")
})

test_that("date summaries report spacing, gaps and duplicates", {
  d <- ilm_sim()
  r <- ilm_describe_all(d, class = "time")
  expect_equal(r$spacing, "monthly")
  expect_true(r$regular)
  expect_equal(r$n_gaps, 0L)
  # repeated dates are normal in long format, so the note describes rather
  # than warns
  expect_match(r$note, "long format")
})

test_that("calendar spacing is not called irregular", {
  # months run 28 to 31 days, so an exact test condemns every monthly series
  m <- data.frame(d = seq(as.Date("2020-01-01"), by = "month", length.out = 60))
  q <- data.frame(d = seq(as.Date("2020-01-01"), by = "quarter", length.out = 40))
  expect_true(ilm_describe(m, "d")$regular)
  expect_true(ilm_describe(q, "d")$regular)
})

test_that("real gaps are still caught", {
  g <- data.frame(d = as.Date("2020-01-01") + c(0:40, 60:98, 150:169))
  r <- ilm_describe(g, "d")
  expect_false(r$regular)
  expect_equal(r$n_gaps, 2L)
  expect_match(r$note, "AR\\(1\\)")
})

test_that("by works on describe and on describe_all", {
  d <- ilm_sim(n_id = 30)
  r1 <- ilm_describe(d, "score", by = "grp")
  expect_gt(nrow(r1), 1L)
  expect_true("grp" %in% names(r1))
  r2 <- ilm_describe_all(d, by = "grp", class = "numeric")
  expect_true("grp" %in% names(r2))
  expect_false("grp" %in% r2$variable)   # a grouping variable is not described
})

test_that("misspecified arguments name the valid options", {
  d <- ilm_sim(n_id = 20)
  expect_error(ilm_describe_all(d, class = "numerical"), "Options are")
  expect_error(ilm_describe_all(d, class = "numerical"), "'numeric'")
  expect_error(ilm_describe_all(d, by = "nope"), "`by` takes column names; nope is not one", fixed = TRUE)
  expect_error(ilm_describe(d, "score", probs = c(-1, 2)), "between 0 and 1")
})

test_that("ilm_frame_issues finds pairwise problems", {
  d <- data.frame(x = rnorm(100), k = 1L, id = seq_len(100),
                  code = paste0("a", 1:100), stringsAsFactors = FALSE)
  d$dup <- d$x
  d$near <- d$x + rnorm(100, sd = 1e-3)
  r <- ilm_frame_issues(d)
  expect_true("constant" %in% r$issue)
  expect_true("duplicate_columns" %in% r$issue)
  expect_true("collinear" %in% r$issue)
  # an identical pair is a duplicate, and not reported again as collinear
  expect_false("x ~ dup" %in% r$columns)
  expect_true("x ~ near" %in% r$columns)
  # a continuous column is unique per row by construction, not an identifier
  idl <- r$columns[r$issue == "id_like"]
  expect_true(all(c("id", "code") %in% idl))
  expect_false("x" %in% idl)
  # every problem comes with what to do about it
  expect_true(all(nzchar(r$remedy)))
})

## planted structure: a total and its parts, wards inside clinics, a clinic
## code that relabels the clinic, a dose fixed by the clinic, and an arm
## recorded twice with four rows disagreeing
frame_fixture <- function(n = 200) {
  set.seed(1)
  d <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  d$total <- d$a + 2 * d$b - d$c
  d$clinic <- sample(c("x", "y", "z"), n, TRUE)
  d$ward <- paste0(d$clinic, sample(1:3, n, TRUE))
  d$clinic_code <- c(x = "C1", y = "C2", z = "C3")[d$clinic]
  d$dose <- c(x = 10, y = 20, z = 25)[d$clinic]
  d$arm <- sample(c("t", "p"), n, TRUE)
  d$arm2 <- d$arm
  flip <- sample(n, 4)
  d$arm2[flip] <- ifelse(d$arm[flip] == "t", "p", "t")
  d$sex <- sample(c("f", "m"), n, TRUE)
  d
}

test_that("ilm_frame_issues finds structure that breaks a model matrix", {
  r <- ilm_frame_issues(frame_fixture())
  expect_identical(names(r), c("issue", "columns", "detail", "remedy"))
  got <- paste(r$issue, r$columns, sep = ": ")
  expect_setequal(got, c("nested: ward in clinic",
                         "aliased_factors: clinic = clinic_code",
                         "redundant_categories: arm ~ arm2",
                         "rank_deficient: total ~ a + b + c",
                         "rank_deficient: dose ~ clinic"))
  expect_true(all(nzchar(r$remedy)))
  expect_match(r$remedy[r$issue == "nested"], "(1 | clinic/ward)", fixed = TRUE)
})

test_that("the rank check agrees with stats::alias() on a fitted lm", {
  d <- frame_fixture()
  d$clinic_code <- NULL           # aliased pairs are left out of the matrix
  d$y <- rnorm(nrow(d))
  al <- alias(lm(y ~ a + b + c + total + clinic + dose, data = d))
  expect_identical(rownames(al$Complete), c("total", "dose"))
  r <- ilm_frame_issues(d[c("a", "b", "c", "total", "clinic", "dose")])
  expect_identical(sub(" ~.*", "", r$columns[r$issue == "rank_deficient"]),
                   c("total", "dose"))
})

test_that("Cramer's V matches the chi-square definition, and v_cut is honoured", {
  d <- frame_fixture()
  a <- as.integer(factor(d$arm)); b <- as.integer(factor(d$arm2))
  x2 <- suppressWarnings(chisq.test(table(a, b), correct = FALSE))$statistic
  expect_equal(ilm_cramer_v(a, b), sqrt(unname(x2) / nrow(d)))
  a <- as.integer(factor(d$clinic)); b <- as.integer(factor(d$sex))
  x2 <- suppressWarnings(chisq.test(table(a, b), correct = FALSE))$statistic
  expect_equal(ilm_cramer_v(a, b), sqrt(unname(x2) / (nrow(d) * 1)))
  r <- ilm_frame_issues(d[c("arm", "arm2")], v_cut = 0.99)
  expect_equal(nrow(r), 0L)
  expect_error(ilm_frame_issues(d, v_cut = 2), "v_cut")
})

test_that("nesting is not claimed from levels seen once", {
  set.seed(2)
  d <- data.frame(g = sample(c("a", "b"), 70, TRUE),
                  h = c(paste0("s", 1:60), rep(c("t", "u"), 5)))
  expect_false("nested" %in% ilm_frame_issues(d)$issue)
})

test_that("ilm_frame_issues stays quiet on ordinary data and says when it cannot judge", {
  expect_equal(nrow(ilm_frame_issues(mtcars)), 0L)
  expect_equal(nrow(ilm_frame_issues(iris)), 0L)
  expect_identical(ilm_frame_issues(ilm_sim())$issue, "constant")
  r <- ilm_frame_issues(frame_fixture()[1:5, c("a", "b", "c", "clinic", "sex")])
  expect_true("rank_unchecked" %in% r$issue)
  z <- ilm_frame_issues(data.frame(x = rnorm(20), y = rnorm(20)))
  expect_identical(names(z), c("issue", "columns", "detail", "remedy"))
  expect_equal(nrow(z), 0L)
})

test_that("skewness and kurtosis are the type 2 estimators (Joanes and Gill 1998)", {
  ## the values elucidate::skewness() and kurtosis() give at their default,
  ## type 2, on this sample (elucidate 0.1.1.9001, 9f7a5536, measured
  ## 2026-09-28); type 3, which illumex gave before, is 1.493986 and 2.803699
  set.seed(1); v <- stats::rexp(200)
  d <- ilm_describe(v, skew = TRUE, kurt = TRUE)
  expect_equal(d$skew, 1.516660, tolerance = 1e-6)
  expect_equal(d$kurt, 2.965648, tolerance = 1e-6)
  ## and by the formula, independently
  n <- length(v); m <- mean(v); m2 <- mean((v - m)^2)
  g1 <- mean((v - m)^3) / m2^1.5; g2 <- mean((v - m)^4) / m2^2 - 3
  expect_equal(ilm_skew2(v), g1 * sqrt(n * (n - 1)) / (n - 2))
  expect_equal(ilm_kurt2(v), ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3)))
  ## too few values, or none varying, give NA
  expect_true(is.na(ilm_skew2(c(1, 2))) && is.na(ilm_kurt2(c(1, 2, 3))) && is.na(ilm_kurt2(rep(2, 10))))
})

test_that("values too large for a finite spread are described, not assessed", {
  set.seed(1)
  cols <- list(huge = stats::rnorm(50) * 1e300, neg = -abs(stats::rnorm(50)) * 1e300,
               mixed = c(stats::rnorm(49), 1e300), with_inf = c(stats::rnorm(49), Inf))
  for (nm in names(cols)) {
    d <- data.frame(v = cols[[nm]])
    r <- suppressWarnings(ilm_describe(d, "v"))
    expect_no_error(utils::capture.output(print(r)))
    if (nm == "with_inf") {
      ## an infinite value is left out, and the rest is ordinary
      expect_false(grepl("too large to assess", r$gauss_note), label = nm)
      expect_true(is.finite(r$gauss), label = nm)
    } else {
      expect_match(r$gauss_note, "too large to assess", label = nm)
      expect_true(is.na(r$gauss), label = nm)
    }
  }
})
