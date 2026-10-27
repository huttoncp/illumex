## ilm_reduce() and ilm_profile()'s v-tests held to FactoMineR, whose numbers
## are stored in fixtures/ by dev/studies/make_factominer_fixtures.R, so
## FactoMineR need not be installed for the check to run.

fixture <- function(f) utils::read.csv(test_path("fixtures", f),
                                       stringsAsFactors = FALSE)

test_that("ilm_reduce() on mixed data agrees with FactoMineR::FAMD()", {
  d <- ilm_sim()[c("score", "income", "visits", "claims", "grp")]
  r <- suppressMessages(ilm_reduce(d, ndim = 5))
  fa <- fixture("famd_ilm_sim.csv")
  ## to 1e-10 now that the eigenvalues are kept whole; they were stored
  ## rounded to four places, which is why this once read 1e-4
  expect_equal(r$eig$eigenvalue[1:5], fa$value[fa$what == "eigenvalue"],
               tolerance = 1e-10)
  ## a dimension's sign is arbitrary, so the coordinates agree up to it
  for (k in 1:3) {
    ref <- fa$value[fa$what == "coord" & fa$dim == k]
    expect_gt(abs(stats::cor(ref, r$ind_coord[[paste0("dim", k)]])), 1 - 1e-8)
    expect_equal(abs(r$ind_coord[[paste0("dim", k)]]), abs(ref),
                 tolerance = 1e-10)
  }
})

test_that("the v-test for a numeric variable is FactoMineR::catdes()'s", {
  d <- ilm_sim()
  cl <- cut(d$score, stats::quantile(d$score, 0:3 / 3), include.lowest = TRUE,
            labels = FALSE)
  cd <- fixture("catdes_ilm_sim.csv")
  cd <- cd[is.na(cd$level), ]
  mine <- mapply(function(k, v) ilm_vtest_mean(d[[v]], cl == k),
                 cd$cluster, cd$variable)
  expect_equal(unname(mine), cd$v_test, tolerance = 1e-10)
})

## The category test-value follows the published definition (Lebart, Piron
## and Morineau 2006, p. 293): the full tail P(N >= x), observed count
## included. catdes() uses a mid-p tail, which is never larger, so the
## published value agrees in sign and is never the larger in size; catdes()
## is an outside comparison here, not a target.
test_that("the category test-value agrees with FactoMineR::catdes() in sign, and is the more cautious", {
  d <- ilm_sim()
  cl <- cut(d$score, stats::quantile(d$score, 0:3 / 3), include.lowest = TRUE,
            labels = FALSE)
  cd <- fixture("catdes_ilm_sim.csv")
  cd <- cd[!is.na(cd$level), ]
  expect_gt(nrow(cd), 0L)
  mine <- unname(mapply(function(k, v, l) ilm_vtest_share(d[[v]] == l, cl == k),
                        cd$cluster, cd$variable, cd$level))
  both <- mine != 0 & cd$v_test != 0
  expect_identical(sign(mine[both]), sign(cd$v_test[both]))
  expect_true(all(abs(mine) <= abs(cd$v_test) + 1e-8))
})
