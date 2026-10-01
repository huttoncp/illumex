# Craig's item 202: ilm_boot_ci() takes several columns in y, or none for
# every numeric column, and returns one long table with a variable column.
# Checked column by column: against the single-column call exactly, and
# against boot::boot.ci to within Monte Carlo error.

test_that("each column's rows are the ones it gets on its own", {
  d <- ilm_sim()
  several <- ilm_boot_ci(d, c("score", "income"), stat = "median", R = 300, seed = 4)
  expect_identical(names(several)[1], "variable")
  expect_identical(several$variable, c("score", "income"))
  for (v in c("score", "income")) {
    alone <- ilm_boot_ci(d, v, stat = "median", R = 300, seed = 4)
    got <- several[several$variable == v, setdiff(names(several), "variable")]
    rownames(got) <- NULL
    expect_equal(as.data.frame(got), as.data.frame(alone), ignore_attr = TRUE,
                 label = v)
  }
  ## and with groups
  g2 <- ilm_boot_ci(d, c("score", "visits"), by = "grp", R = 200, seed = 5)
  alone <- ilm_boot_ci(d, "visits", by = "grp", R = 200, seed = 5)
  got <- g2[g2$variable == "visits", -1]; rownames(got) <- NULL
  expect_equal(as.data.frame(got), as.data.frame(alone), ignore_attr = TRUE)
})

test_that("with no y, every numeric column except by", {
  d <- ilm_sim()
  all_ <- suppressWarnings(ilm_boot_ci(d, by = "grp", R = 50, seed = 1))
  num <- setdiff(names(d)[vapply(d, is.numeric, TRUE)], "grp")
  expect_identical(unique(all_$variable), num)
  expect_identical(unique(ilm_boot_ci(mtcars[c("mpg", "wt")], R = 50, seed = 1)$variable),
                   c("mpg", "wt"))
})

test_that("each column agrees with boot::boot.ci", {
  skip_if_not_installed("boot")
  set.seed(11)
  d <- data.frame(a = stats::rlnorm(200, 0, 0.7), b = stats::rexp(200), c = stats::rnorm(200))
  mine <- ilm_boot_ci(d, R = 3000, ci_type = "bca", seed = 11)
  for (v in names(d)) {
    bb <- boot::boot(d[[v]], function(x, i) mean(x[i]), R = 3000)
    ref <- boot::boot.ci(bb, type = c("perc", "bca"), conf = 0.95)
    r <- mine[mine$variable == v, ]
    ## different RNG streams, so agreement is to within Monte Carlo error
    expect_equal(r$lower, ref$bca[4], tolerance = 0.05, label = v)
    expect_equal(r$upper, ref$bca[5], tolerance = 0.05, label = v)
  }
})

test_that("a column that is not numeric is named", {
  d <- ilm_sim()
  expect_error(ilm_boot_ci(d, c("score", "grp")), "grp is factor")
  expect_error(ilm_boot_ci(d["grp"]), "no numeric columns")
  expect_error(ilm_boot_ci(d, c("score", "nope")), "not found")
})

test_that("the BCa warning names column and group together", {
  d <- ilm_sim()
  expect_warning(ilm_boot_ci(d, c("score", "income"), by = "grp", ci_type = "bca",
                             R = 100, seed = 1),
                 "group delta \\(score and income\\) have 3 each")
})
