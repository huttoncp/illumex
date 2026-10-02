# A seeded function puts the user's random stream back as it leaves.

## the stream after a call, against the stream after the same user code
## without it
untouched <- function(call) {
  set.seed(99); stats::runif(1); a <- .Random.seed
  set.seed(99); stats::runif(1); force(call()); b <- .Random.seed
  identical(a, b)
}

rng_data <- function() {
  set.seed(5); n <- 120
  f <- stats::rnorm(n)
  data.frame(a = f + stats::rnorm(n, 0, 0.3), b = f + stats::rnorm(n, 0, 0.3),
             c = -f + stats::rnorm(n, 0, 0.3), d = stats::rnorm(n),
             g = factor(sample(c("x", "y"), n, TRUE)))
}

test_that("every seeded function starts by arranging to restore the stream", {
  ## the list is every export with a `seed` argument and every S3 method with
  ## one, so a new seeded function cannot be added without it
  ns <- asNamespace("illumex")
  ex <- getNamespaceExports("illumex")
  ex <- ex[!startsWith(ex, "iml_")]
  s3 <- getNamespaceInfo(ns, "S3methods")
  meth <- ifelse(is.na(s3[, 3L]), paste(s3[, 1L], s3[, 2L], sep = "."), s3[, 3L])
  cand <- unique(c(ex, meth[vapply(meth, exists, TRUE, envir = ns,
                                   inherits = FALSE)]))
  ## ilm_sample() only keeps a seed for the draw a `subset` makes later,
  ## which restores the stream itself (R/ilm_subset.R)
  cand <- setdiff(cand, "ilm_sample")
  seeded <- Filter(function(f) {
    fn <- get(f, envir = ns)
    is.function(fn) && "seed" %in% names(formals(fn)) &&
      !any(grepl("UseMethod", deparse(body(fn)), fixed = TRUE))
  }, cand)
  expect_gte(length(seeded), 7L)
  expect_true("ilm_boot_diff.data.frame" %in% seeded)
  first <- vapply(seeded, function(f) {
    b <- body(get(f, envir = ns))
    deparse(if (is.call(b) && identical(b[[1L]], as.name("{"))) b[[2L]] else b)[1L]
  }, "")
  bad <- seeded[!startsWith(first, "ilm_rng_restore(seed")]
  expect_identical(bad, character(0),
                   label = "seeded functions that do not restore the stream")
})

test_that("the user's stream is untouched and the results reproducible", {
  d <- rng_data()
  expect_true(untouched(function() ilm_sim(n_id = 5, n_period = 2)))
  expect_true(untouched(function() ilm_boot_ci(d, "a", R = 50, seed = 1)))
  expect_true(untouched(function() ilm_boot_diff(d, "a", "g", R = 50, seed = 1)))
  expect_true(untouched(function() ilm_anomaly(d[1:4], progress = FALSE)))
  expect_true(untouched(function()
    ilm_glrm(d, rank = 1L, lambda = 1, maxit = 50L, progress = FALSE)))
  skip_if_not_installed("cluster")
  co <- as.matrix(scale(d[1:4]))
  expect_true(untouched(function() ilm_cluster(co, k = 2, B = 5, seed = 1)))
  cl <- ilm_cluster(co, k = 2, B = 5, seed = 1)
  expect_true(untouched(function() ilm_var_contrib(cl, d, B = 19)))
  ## and a seeded result does not depend on the user's stream
  set.seed(1); r1 <- ilm_anomaly(d[1:4], progress = FALSE)
  set.seed(2); r2 <- ilm_anomaly(d[1:4], progress = FALSE)
  expect_identical(r1, r2)
})

test_that("an absent stream stays absent, and seed = NULL draws from it", {
  d <- rng_data()
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    rm(".Random.seed", envir = globalenv())
  invisible(ilm_boot_ci(d, "a", R = 20, seed = 6))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  ## with no seed the function is ordinary R code: it moves the stream on
  set.seed(7)
  a <- ilm_boot_ci(d, "a", R = 20); b <- ilm_boot_ci(d, "a", R = 20)
  expect_false(identical(a, b))
  ## and from the user's stream rather than a fresh one: set.seed(NULL) would
  ## re-seed from the clock, and the user's own set.seed() would then fix
  ## nothing
  set.seed(7); a <- ilm_sim(n_id = 3, n_period = 2, seed = NULL)
  set.seed(7); b <- ilm_sim(n_id = 3, n_period = 2, seed = NULL)
  expect_identical(a, b)
})

test_that("the stream is put back even when the function stops", {
  d <- rng_data()
  ## a statistic that fails part-way through the resampling, after the seed is
  ## set and draws have been taken
  failing <- local({
    calls <- 0L
    function(x) { calls <<- calls + 1L; if (calls > 3L) stop("boom"); mean(x) }
  })
  set.seed(8); stats::runif(1); a <- .Random.seed
  set.seed(8); stats::runif(1)
  expect_error(ilm_boot_ci(d, "a", stat = failing, R = 20, seed = 1), "boom")
  expect_identical(.Random.seed, a)
})
