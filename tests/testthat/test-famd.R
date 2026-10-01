# ilm_famd(), which ilm_reduce() computes with, held for good to what
# PCAmixdata::PCAmix() gave on the same data (recorded at full precision by
# dev/studies/make_pcamix_fixtures.R), so neither package has to be
# installed for the check. test-factominer-agreement.R does the same against
# FactoMineR::FAMD() through ilm_reduce() itself.

pcamix <- function() {
  f <- utils::read.csv(test_path("fixtures", "pcamix_cases.csv"), comment.char = "#",
                       stringsAsFactors = FALSE, colClasses = c(row = "character"))
  split(f, f$case)
}

test_that("eigenvalues, coordinates and loadings are PCAmix's, on every kind of data", {
  ref <- pcamix()
  cases <- famd_cases()
  expect_setequal(names(ref), names(cases))
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    r <- ref[[nm]]
    f <- ilm_famd(cs$quanti, cs$quali, ndim = cs$ndim)
    eig <- r$value[r$what == "eigenvalue"]
    expect_equal(unname(f$eig[seq_along(eig), "eigenvalue"]), eig, tolerance = 1e-10,
                 label = paste(nm, "eigenvalues"))
    co <- r[r$what == "coord", ]
    for (k in sort(unique(co$dim))) {
      theirs <- co$value[co$dim == k]
      mine <- unname(f$ind$coord[, k])
      ## a dimension's sign is arbitrary in both; the sign rule fixes ours
      s <- sign(sum(mine * theirs))
      expect_equal(s * mine, theirs, tolerance = 1e-10, label = paste(nm, "dim", k))
    }
    sq <- r[r$what == "sqload", ]
    for (k in sort(unique(sq$dim))) {
      theirs <- sq[sq$dim == k, ]
      expect_equal(unname(f$sqload[theirs$row, k]), theirs$value, tolerance = 1e-10,
                   label = paste(nm, "squared loadings, dim", k))
    }
  }
})

test_that("each dimension's sign is fixed: the largest loading is positive", {
  for (cs in famd_cases()) {
    f <- ilm_famd(cs$quanti, cs$quali, ndim = cs$ndim)
    V <- f$loadings
    big <- V[cbind(apply(abs(V), 2L, which.max), seq_len(ncol(V)))]
    expect_true(all(big > 0))
  }
  ## so the same data give the same coordinates whatever the column order
  d <- ilm_sim()[c("score", "income", "visits", "claims", "grp")]
  a <- suppressMessages(ilm_reduce(d))
  b <- suppressMessages(ilm_reduce(d[c(5, 4, 3, 2, 1)]))
  expect_equal(a$ind_coord, b$ind_coord, tolerance = 1e-10)
})

test_that("ilm_reduce() needs nothing installed for mixed data", {
  d <- ilm_sim()[c("score", "grp", "site")]
  r <- suppressMessages(ilm_reduce(d))
  expect_identical(r$method, "famd")
  expect_equal(sum(r$eig$pct_var), 100, tolerance = 1e-10)
})

test_that("method = \"pcamix\", the former name, still means \"famd\"", {
  d <- ilm_sim()[c("score", "income", "grp", "site")]
  f <- suppressMessages(ilm_reduce(d, method = "famd"))
  expect_identical(suppressMessages(ilm_reduce(d)), f)
  expect_identical(suppressMessages(ilm_reduce(d, method = "pcamix")), f)
  pf <- suppressMessages(ilm_profile(d, k = 3, seed = 1, var_contrib = FALSE))
  pp <- suppressMessages(ilm_profile(d, method = "pcamix", k = 3, seed = 1,
                                     var_contrib = FALSE))
  expect_identical(pp$summary, pf$summary)
  expect_identical(pp$cluster, pf$cluster)
})
