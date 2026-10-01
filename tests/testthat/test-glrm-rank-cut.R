# Craig's item 265: a GLRM reduction keeps only the dimensions that carry
# variation. Heavy shrinkage can leave the centred low-rank product of lower
# rank than was fitted; a zero singular value's vector is arbitrary (each
# LAPACK build returns a different one), so its loadings would be noise and
# the print would differ between platforms.

test_that("a dimension with no variation is not kept, printed or recorded", {
  q <- function(e) suppressMessages(suppressWarnings(e))
  d <- ilm_sim(n_id = 20, n_period = 6)
  dd <- d[c("score", "income", "visits", "claims", "grp")]
  r <- q(ilm_reduce(dd, method = "glrm", progress = FALSE))
  ## this case, the one the print test holds: 5 fitted, 4 with variation
  expect_identical(c(r$ndim_fitted, r$ndim), c(5L, 4L))
  expect_identical(ncol(r$ind_coord), 4L)
  expect_identical(nrow(r$eig), 4L)
  expect_identical(sort(unique(r$var_contrib$dim)), 1:4)
  expect_identical(r$fit$rank_kept, 4L)
  ## every kept singular value clears the cut, and the dropped one does not
  P <- as.matrix(r$fit$scores) %*% r$fit$archetypes
  P <- sweep(P, 2L, colMeans(P), "-")
  sv <- svd(P)$d
  expect_true(all(sv[1:4] > sqrt(.Machine$double.eps) * sv[1]))
  expect_lte(sv[5], sqrt(.Machine$double.eps) * sv[1])
  out <- capture.output(print(r))
  expect_match(out[1], "4 dimensions carry variation (5 fitted; after shrinkage the 5th has none)",
               fixed = TRUE)
  expect_false(any(grepl("dim 5", out)))
  expect_true(any(grepl("after shrinkage 4 of the 5 dimensions carry variation",
                        capture.output(print(r$fit)), fixed = TRUE)))
  expect_identical(c(r$ndim_fitted, r$ndim), c(5L, 4L))
  expect_identical(nrow(r$eig), 4L)
  ## a FAMD reduction has no cut: both counts are its dimensions
  f <- q(ilm_reduce(dd))
  expect_identical(f$ndim_fitted, f$ndim)
})

test_that("the dropped dimensions are named in plain words", {
  expect_identical(ilm_dropped_dims(4L, 5L), "the 5th has none")
  expect_identical(ilm_dropped_dims(3L, 5L), "the 4th and 5th have none")
  expect_identical(ilm_dropped_dims(2L, 5L), "the 3rd to 5th have none")
  expect_identical(ilm_dropped_dims(1L, 3L), "the 2nd and 3rd have none")
})
