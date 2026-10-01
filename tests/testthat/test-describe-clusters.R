# Data whose rows sit in clusters: the counts, and the variables described
# once per cluster (Craig's item 121).

clustered <- function() {
  set.seed(8)
  sites <- data.frame(site = sprintf("s%02d", 1:20),
                      region = factor(sample(c("north", "south"), 20, TRUE)),
                      beds = round(stats::runif(20, 50, 500)),
                      size = sample(c(2L, 5L, 8L, 12L, 40L), 20, TRUE),
                      stringsAsFactors = FALSE)
  d <- sites[rep(seq_len(20), sites$size), ]
  d$size <- NULL
  n <- nrow(d)
  d$age <- round(stats::rnorm(n, 60, 10))
  d$sex <- sample(c("F", "M"), n, TRUE)
  d$region[c(2, 9)] <- NA                      # missing on some of a site's rows
  d <- rbind(d, data.frame(site = NA, region = "north", beds = 99, age = 50, sex = "F"))
  rownames(d) <- NULL
  list(d = d, sites = sites)
}

test_that("clusters are counted, and a cluster's own variables described once per cluster", {
  cs <- clustered(); d <- cs$d; sites <- cs$sites
  x <- ilm_describe_clusters(d, "site")
  expect_s3_class(x, "ilm_describe_clusters")
  s <- x$clusters
  expect_identical(s$n_clusters, 20L)
  expect_identical(s$n_rows, sum(sites$size))
  expect_identical(s$n_no_cluster, 1L)
  expect_identical(c(s$rows_min, s$rows_max), range(sites$size))
  expect_equal(s$rows_mean, mean(sites$size))
  expect_identical(sort(x$cluster_vars), c("beds", "region"))
  expect_identical(sort(x$row_vars), c("age", "sex"))
  ## once per cluster: a site of 40 rows counts once
  num <- x$cluster_level$numeric %||% x$cluster_level
  expect_identical(num$n[num$variable == "beds"], 20L)
  expect_equal(num$mean[num$variable == "beds"], mean(sites$beds))
  ## a site whose region is missing on some rows takes it from the others
  cat_tab <- x$cluster_level$categorical
  expect_identical(cat_tab$n[cat_tab$variable == "region"], 20L)
})

test_that("a variable that differs within any cluster is a row's, not the cluster's", {
  cs <- clustered(); d <- cs$d
  d$beds[5] <- d$beds[5] + 1                  # one row of one site disagrees
  x <- ilm_describe_clusters(d, "site")
  expect_true("beds" %in% x$row_vars)
  expect_false("beds" %in% x$cluster_vars)
  ## cols limits what is considered, and the cluster column is never described
  y <- ilm_describe_clusters(d, "site", cols = c("region", "site"))
  expect_identical(y$cluster_vars, "region")
  expect_length(y$row_vars, 0L)
})

test_that("the summary prints the counts, then the clusters' table, then the rest", {
  cs <- clustered()
  out <- utils::capture.output(print(ilm_describe_clusters(cs$d, "site")))
  expect_identical(out[1], sprintf("Clusters: 20 by `site`, %d rows (1 without a cluster, left out)",
                                   sum(cs$sites$size)))
  expect_true(startsWith(out[2], "Rows per cluster: mean"))
  expect_true(any(out == "Described once per cluster (the same on every row of a cluster):"))
  expect_true(any(grepl("^Vary within clusters, so described over rows by ilm_describe_all\\(\\): ", out)))
  ## clusters all of one size say so plainly
  same <- utils::capture.output(print(ilm_describe_clusters(ilm_sim(n_id = 30, n_period = 6), "id")))
  expect_identical(same[1:2], c("Clusters: 30 by `id`, 180 rows", "Rows per cluster: 6 in every cluster"))
})

test_that("subset = a model gives the clusters of the rows it analysed, and cols chooses", {
  cs <- clustered(); d <- cs$d
  d$age[d$site %in% c("s01", "s02")] <- NA   # two sites' every row dropped by the model
  mf <- stats::model.frame(age ~ sex, d, na.action = stats::na.omit)
  fit <- structure(list(formula = age ~ sex, model = mf, na.action = attr(mf, "na.action")),
                   class = "ilm_model")
  x <- ilm_describe_clusters(d, "site", subset = fit)
  expect_identical(x$clusters$n_clusters, 18L)
  out <- utils::capture.output(print(x))
  expect_identical(out[1], sprintf("%d of %d rows (analysed by the model)", nrow(mf), nrow(d)))
  expect_true(startsWith(out[2], "Clusters: 18 by `site`"))
  ## cols in its every form, the cluster column kept out
  y <- ilm_describe_clusters(d, "site", cols = "age", cols_negate = TRUE)
  expect_identical(sort(c(y$cluster_vars, y$row_vars)), c("beds", "region", "sex"))
  expect_identical(utils::capture.output(print(y))[1], "Columns: 3 of 4 (excluded: age)")
})

test_that("a cluster column that is not there, or missing throughout, is refused", {
  cs <- clustered()
  expect_error(ilm_describe_clusters(cs$d, "clinic"), "one column")
  d <- cs$d; d$site <- NA
  expect_error(ilm_describe_clusters(d, "site"), "missing throughout")
  expect_error(ilm_describe_clusters(cs$d$age, "site"), "data frame")
})
