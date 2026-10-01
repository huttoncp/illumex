## Reference numbers from FactoMineR, stored so the tests can hold
## ilm_reduce() and ilm_profile()'s v-tests to them without FactoMineR being
## a dependency (it brings 119 packages).
##
##   tests/testthat/fixtures/famd_ilm_sim.csv
##       FactoMineR::FAMD() on five columns of ilm_sim(): the first five
##       eigenvalues, and the individual coordinates on the first three
##       dimensions, one row per row of the data
##   tests/testthat/fixtures/catdes_ilm_sim.csv
##       FactoMineR::catdes() v-tests for the same columns against a fixed
##       partition (the terciles of `score`): one row per cluster and
##       numeric variable, and one per cluster and category
##
## The data are ilm_sim()'s defaults, so the tests rebuild them rather than
## storing them. Rerun this when FactoMineR changes its method, not when
## illumex changes:
##   Rscript dev/studies/make_factominer_fixtures.R
pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
if (!requireNamespace("FactoMineR", quietly = TRUE)) stop("needs FactoMineR")
dir.create("tests/testthat/fixtures", showWarnings = FALSE)

d <- ilm_sim()[c("score", "income", "visits", "claims", "grp")]
f <- FactoMineR::FAMD(d, ncp = 5, graph = FALSE)
eig <- data.frame(what = "eigenvalue", dim = 1:5, row = NA_integer_,
                  value = unname(f$eig[1:5, 1]))
co <- do.call(rbind, lapply(1:3, function(k)
  data.frame(what = "coord", dim = k, row = seq_len(nrow(d)),
             value = unname(f$ind$coord[, k]))))
utils::write.csv(rbind(eig, co), "tests/testthat/fixtures/famd_ilm_sim.csv",
                 row.names = FALSE)

dc <- d[c("income", "visits", "claims", "grp")]
dc$cluster <- factor(cut(d$score, stats::quantile(d$score, 0:3 / 3),
                         include.lowest = TRUE, labels = FALSE))
cd <- FactoMineR::catdes(dc, num.var = ncol(dc), proba = 1)
num <- do.call(rbind, lapply(names(cd$quanti), function(k) {
  q <- cd$quanti[[k]]
  if (is.null(q)) return(NULL)
  data.frame(cluster = k, variable = rownames(q), level = NA_character_,
             v_test = unname(q[, "v.test"]))
}))
cat_ <- do.call(rbind, lapply(names(cd$category), function(k) {
  q <- cd$category[[k]]
  if (is.null(q)) return(NULL)
  data.frame(cluster = k, variable = "grp", level = sub("^grp=", "", rownames(q)),
             v_test = unname(q[, "v.test"]))
}))
utils::write.csv(rbind(num, cat_), "tests/testthat/fixtures/catdes_ilm_sim.csv",
                 row.names = FALSE)
cat("written:", nrow(eig) + nrow(co), "FAMD rows,", nrow(num) + nrow(cat_),
    "catdes rows\n")
