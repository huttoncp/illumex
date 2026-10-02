# No distinct clusters (Craig's item 259): at k = 1 the print drops the
# one-row table and its stability, says there are no distinct clusters and
# what describes a continuum; a profile then has no clusters to describe.

no_groups <- function() {
  set.seed(259)
  data.frame(a = stats::rnorm(300), b = stats::rnorm(300), c = stats::rnorm(300))
}

test_that("k = 1 found by the gap statistic says the rows are a continuum", {
  skip_if_not_installed("cluster")
  x <- suppressMessages(ilm_cluster(no_groups(), B = 20, seed = 1))
  expect_identical(x$k, 1L)
  out <- utils::capture.output(print(x))
  expect_identical(out[1], "<ilm_cluster> method = kmeans, k = 1 (chosen by gap statistic)")
  expect_identical(out[3], "  No distinct clusters: the gap statistic finds no grouping better than one")
  expect_false(any(grepl("stability|jaccard|stable", out)))
  p <- utils::capture.output(print(suppressMessages(ilm_profile(no_groups(), B = 20, seed = 1))))
  expect_true(any(grepl("No distinct clusters", p, fixed = TRUE)))
  expect_false(any(grepl("what each cluster is|Cluster 1|ilm_var_contrib", p)))
})

test_that("k = 1 as given says the rows are one group", {
  x <- suppressMessages(ilm_cluster(no_groups(), k = 1, B = 5, seed = 1))
  out <- utils::capture.output(print(x))
  expect_identical(out[3], "  No distinct clusters: with k = 1, as given, the 300 rows are one group.")
})
