# Two defects the census study found (dev/studies/census): k-means stopped at
# stats::kmeans()'s 10 iterations and warned once per start, and the note on
# incomplete rows rounded 99.8% complete to "only 100%".

test_that("a share never prints as a limit it has not reached", {
  expect_identical(ilm_pct_share(c(0, 1, 0.5, 0.998, 0.9996, 0.0004, 0.004, 0.996, 0.12)),
                   c("0%", "100%", "50%", "99.8%", "over 99.9%", "under 0.1%", "0.4%",
                     "99.6%", "12%"))
  expect_identical(ilm_pct_share(NA_real_), "NA")
})

test_that("the note on incomplete rows says 99.8%, not 100%", {
  d <- data.frame(a = stats::rnorm(1000), b = stats::rnorm(1000))
  d$a[1:2] <- NA
  msg <- tryCatch(ilm_cluster_na_note(d, "ilm_anomaly"), message = conditionMessage)
  expect_true(grepl("only 99.8% of rows are complete", msg, fixed = TRUE))
  expect_true(grepl("a 0.2%", msg, fixed = TRUE))
  expect_false(grepl("100%", msg, fixed = TRUE))
})

test_that("k-means allows 100 iterations, and a start that stops is counted, not warned", {
  expect_identical(ILM_KMEANS_ITER_MAX, 100L)
  ## a limit of 1 on data with no clusters stops most starts
  set.seed(3)
  x <- matrix(stats::runif(2000), ncol = 2)
  tally <- new.env(); tally$starts <- 0L; tally$not_converged <- 0L
  fk <- ilm_cluster_funcluster("kmeans", "euclidean", "ward.D2", nstart = 5L, tally, iter_max = 1L)
  expect_no_warning(fk(x, 6))
  expect_identical(tally$starts, 5L)
  expect_gt(tally$not_converged, 0L)
  ## at the package's limit the same fit converges
  tally2 <- new.env(); tally2$starts <- 0L; tally2$not_converged <- 0L
  fk2 <- ilm_cluster_funcluster("kmeans", "euclidean", "ward.D2", nstart = 5L, tally2)
  expect_no_warning(fk2(x, 6))
  expect_identical(tally2$not_converged, 0L)
  ## the result records its starts, and prints a line only when some stopped
  cl <- suppressMessages(ilm_cluster(ilm_reduce(ilm_sim()[c("score", "income", "visits")]),
                                     k = 3, B = 5, seed = 1, progress = FALSE))
  expect_gt(cl$kmeans_starts, 0L)
  expect_identical(cl$kmeans_not_converged, 0L)
  expect_false(any(grepl("iteration limit", utils::capture.output(print(cl)))))
  cl$kmeans_not_converged <- 7L
  out <- utils::capture.output(print(cl))
  expect_true(any(grepl(sprintf("7 of %s k-means starts stopped at the 100-iteration limit",
                                ilm_fmt_num(cl$kmeans_starts)), out, fixed = TRUE)))
})
