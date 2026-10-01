# What results print is unchanged by their having a class of their own
# and by ilm_cluster()'s and ilm_reduce()'s tables keeping full precision:
# every case prints exactly the text dev/make_print_fixtures.R recorded from
# the code before those changes (tests/testthat/fixtures/print/README).

test_that("every recorded print is unchanged, line for line", {
  skip_if_not_installed("cluster")
  cases <- print_cases()
  dir <- test_path("fixtures", "print")
  expect_setequal(names(cases), sub("[.]txt$", "", list.files(dir, pattern = "[.]txt$")))
  for (nm in names(cases)) {
    want <- readLines(file.path(dir, paste0(nm, ".txt")), warn = FALSE, encoding = "UTF-8")
    expect_identical(print_text(cases[[nm]]), want, label = nm)
  }
})

test_that("the results carry their class, in front of what they were", {
  d <- ilm_sim(n_id = 10, n_period = 4)
  q <- function(e) suppressMessages(suppressWarnings(e))
  expect_identical(class(q(ilm_describe(d, "score"))), c("ilm_describe", "data.frame"))
  expect_s3_class(q(ilm_describe_all(d)), "ilm_describe_all")
  expect_identical(class(q(ilm_describe_all(d, class = "numeric"))),
                   c("ilm_describe", "data.frame"))
  expect_identical(class(q(ilm_frame_issues(d))), c("ilm_frame_issues", "data.frame"))
  expect_identical(class(q(ilm_describe_na_all(d))), c("ilm_describe_na", "data.frame"))
  expect_identical(class(q(ilm_outliers(d$score))), c("ilm_outliers", "data.frame"))
  o <- q(ilm_outliers_all(d, method = "mad"))
  expect_identical(class(o), c("ilm_outliers_all", "data.frame"))
  expect_identical(attr(o, "method"), "mad")
  expect_identical(attr(o, "threshold"), 3.5)
  expect_true(all(attr(o, "checked") >= 0L))
  expect_identical(class(q(ilm_boot_ci(d, "score", R = 20, seed = 1))),
                   c("ilm_boot_ci", "data.frame"))
  expect_identical(class(q(ilm_boot_diff(d, "score", "grp", R = 20, seed = 1))),
                   c("ilm_boot_diff", "data.frame"))
  ## and they are still data frames to everything else
  x <- q(ilm_describe_na_all(d))
  expect_true(is.data.frame(x))
  expect_s3_class(x[1:2, ], "ilm_describe_na")
})

test_that("the cluster and reduction tables keep full precision", {
  skip_if_not_installed("cluster")
  d <- ilm_sim(n_id = 20, n_period = 6)[c("score", "income", "visits", "claims", "grp")]
  r <- suppressMessages(ilm_reduce(d))
  expect_equal(r$eig$pct_var, unname(r$fit$eig[, 2]), tolerance = 0)
  cl <- suppressMessages(ilm_cluster(r, k = 3, B = 10, seed = 1))
  expect_equal(cl$clusters$pct, 100 * cl$clusters$size / sum(cl$clusters$size),
               tolerance = 0)
})
