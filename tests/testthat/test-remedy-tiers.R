## ilm_remedy_tiers(): one tier set per target, asked for by every package
## that builds a remedies table for it, and c() refusing tables ranked by
## different sets.

## a Bayesian fit's tiers, as another package would give its class
ilm_remedy_tiers.test_bayes_fit <- function(object, ...)
  list(tiers = c("numerical", "structural", "prior", "estimand"),
       note = "Prior revises a prior; say why, and whether the data were seen.")

trow <- function(check, tier, key, status = "WARN")
  list(check = check, status = status, tier = tier, remedy = "do it", key = key,
       change = paste0("f_", key, "()"), payload = NULL)

test_that("a data frame's tiers are the data set, with its note", {
  ti <- ilm_remedy_tiers(data.frame(x = 1:3))
  expect_equal(ti$tiers, c("representation", "values", "rows"))
  expect_match(ti$note, "Representation reads the same values correctly", fixed = TRUE)
})

test_that("a fitted model's default tiers are the model set, its note left to its package", {
  ti <- ilm_remedy_tiers(stats::lm(mpg ~ wt, data = mtcars))
  expect_equal(ti$tiers, c("numerical", "structural", "estimand"))
  expect_null(ti$note)
})

test_that("a class with its own method gets its own ordered set", {
  fit <- structure(list(), class = c("test_bayes_fit", "lm"))
  ti <- ilm_remedy_tiers(fit)
  expect_equal(ti$tiers, c("numerical", "structural", "prior", "estimand"))
  ## the model tiers keep their order inside the larger set
  expect_equal(intersect(ti$tiers, ilm_remedy_tiers.default(NULL)$tiers),
               c("numerical", "structural", "estimand"))
})

test_that("tables built with the target's set combine, and rank a prior between", {
  fit <- structure(list(), class = c("test_bayes_fit", "lm"))
  ti <- ilm_remedy_tiers(fit)
  model <- ilm_remedy_assemble(list(trow("variance_boundary", "structural", "drop/g"),
                                    trow("optimizer", "numerical", "restarts")),
                               "FIT", ti$tiers, "model note")
  bayes <- ilm_remedy_assemble(list(trow("prior_predictive", "prior", "prior/p2:weaker"),
                                    trow("approximation", "numerical", "nodes/aghq")),
                               "FIT", ti$tiers, ti$note)
  both <- c(model, bayes)
  expect_equal(both$tier, c("numerical", "numerical", "structural", "prior"))
  expect_equal(both$key[both$tier == "prior"], "prior/p2:weaker")
})

test_that("tables for one target ranked by different sets are refused, plainly", {
  fit <- structure(list(), class = c("test_bayes_fit", "lm"))
  three <- ilm_remedy_assemble(list(trow("optimizer", "numerical", "restarts")), "FIT",
                               ilm_remedy_tiers.default(fit)$tiers, "n")
  four <- ilm_remedy_assemble(list(trow("prior_predictive", "prior", "prior/p2:weaker")), "FIT",
                              ilm_remedy_tiers(fit)$tiers, "n")
  expect_error(c(three, four), "different tier sets")
  expect_error(c(four, three), "ilm_remedy_tiers\\(\\)")
})

test_that("illumex's own data tables use the data frame's set", {
  d <- data.frame(x = stats::rnorm(10), same = 1)
  rem <- ilm_remedies(ilm_check_frame(d))
  expect_equal(attr(rem, "tiers"), ilm_remedy_tiers(d)$tiers)
  expect_equal(attr(rem, "tier_note"), ilm_remedy_tiers(d)$note)
})
