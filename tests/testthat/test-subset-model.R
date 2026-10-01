# A model as `subset`: the rows it analysed (Craig's items 146 and 150,
# folded into `subset`). illume is not needed here: a fit is what
# ilm_model() leaves behind, its formula, its model frame and the rows it
# dropped, built the same way (dev/check_subset_model.R checks real fits).

fit_like <- function(data, formula, cls = "ilm_model") {
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  structure(list(formula = formula, model = mf, na.action = attr(mf, "na.action")),
            class = cls)
}
model_data <- function() {
  set.seed(4)
  n <- 300
  d <- data.frame(y = stats::rnorm(n), x = stats::rnorm(n), z = stats::rnorm(n),
                  g = rep(c("a", "b", "c"), length.out = n), stringsAsFactors = FALSE)
  d$x[c(3, 7, 50)] <- NA
  d$z[c(7, 90)] <- NA
  d
}

test_that("subset = a model keeps the rows it analysed, and keeps what identifies it", {
  d <- model_data()
  fit <- fit_like(d, y ~ x)
  t <- ilm_describe_all(d, subset = fit)
  kept <- d[-c(3, 7, 50), ]
  num <- t$numeric
  expect_identical(num$n[num$variable == "y"], nrow(kept))
  expect_equal(num$mean[num$variable == "z"], mean(kept$z, na.rm = TRUE))
  s <- attr(t, "ilm_select")$subset
  expect_identical(s$form, "model")
  expect_identical(c(s$n_rows_kept, s$n_rows_given), c(297L, 300L))
  ## the copies to check against the fit with identical()
  expect_identical(s$model$fit$formula, fit$formula)
  expect_identical(s$model$fit$na.action, fit$na.action)
  expect_null(s$model$fit$n_input); expect_null(s$model$fit$subset)
  ## ilm_describe() too, grouped or not, and any function that takes subset
  expect_identical(ilm_describe(d, "y", subset = fit)$n, 297L)
  expect_identical(sum(ilm_describe(d, "y", by = "g", subset = fit)$n), 297L)
  expect_identical(nrow(ilm_subset(d, subset = fit)), 297L)
  ## a fit that dropped nothing keeps every row
  expect_identical(ilm_describe(d, "y", subset = fit_like(d, y ~ g))$n, 300L)
})

test_that("the result says which rows it covers, and negated, the rows the model dropped", {
  d <- model_data()
  fit <- fit_like(d, y ~ x)
  expect_identical(utils::capture.output(print(ilm_describe(d, "y", subset = fit)))[1],
                   "297 of 300 rows (analysed by the model)")
  dropped <- ilm_describe(d, "y", subset = fit, subset_negate = TRUE)
  expect_identical(dropped$n, 3L)
  expect_identical(utils::capture.output(print(dropped))[1], "3 of 300 rows (dropped by the model)")
  expect_identical(rownames(ilm_subset(d, subset = fit, subset_negate = TRUE)), c("3", "7", "50"))
})

test_that("an ilm_dag_model() keeps the rows every adjustment set used", {
  d <- model_data()
  dag <- structure(list(fits = list(fit_like(d, y ~ x), fit_like(d, y ~ x + z))),
                   class = "ilm_dag_model")
  t <- ilm_describe_all(d, subset = dag)
  s <- attr(t, "ilm_select")$subset
  ## the first set drops 3, 7 and 50, the second 90 as well
  expect_identical(s$n_rows_kept, 296L)
  expect_identical(t$numeric$n[t$numeric$variable == "y"], 296L)
  expect_length(s$model$fits, 2L)
  expect_identical(s$model$fits[[2]]$formula, y ~ x + z)
  expect_null(s$model$crude)
  expect_identical(utils::capture.output(print(t))[1],
                   "296 of 300 rows (analysed by every adjustment set)")
  expect_identical(utils::capture.output(print(ilm_describe(d, "y", subset = dag,
                                                            subset_negate = TRUE)))[1],
                   "4 of 300 rows (left out by an adjustment set)")
  ## the crude, once built, is kept to be checked too
  dag$crude <- fit_like(d, y ~ 1)
  expect_identical(attr(ilm_describe(d, "y", subset = dag), "ilm_select")$subset$model$crude$formula,
                   y ~ 1)
  ## one set on its own gives its own rows
  expect_identical(ilm_describe(d, "y", subset = dag$fits[[1]])$n, 297L)
})

test_that("a model that does not fit the data is refused", {
  d <- model_data()
  fit <- fit_like(d, y ~ x)
  expect_error(ilm_describe(d[1:200, ], "y", subset = fit), "was fitted to")
  expect_error(ilm_describe(d[-1, ], "y", subset = fit), "was fitted to")
  fit$n_input <- 250L
  expect_error(ilm_describe(d, "y", subset = fit), "was given 250")
  expect_error(ilm_describe(d, "y", subset = lm(y ~ x, d)), "ilm_model")
  expect_error(ilm_describe(d, "y", subset = structure(list(fits = list()), class = "ilm_dag_model")),
               "no fitted adjustment set")
})
