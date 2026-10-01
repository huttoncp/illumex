## Reference numbers from PCAmixdata::PCAmix(), stored so the tests hold
## ilm_famd() to it for good without PCAmixdata being installed: ilm_reduce()
## computed with PCAmix until Craig's ruling of 2026-09-28 replaced it with
## ilm_famd() (dev/studies/famd_own.R measured the two).
##
##   tests/testthat/fixtures/pcamix_cases.csv
##       for each case in tests/testthat/helper-famd-cases.R: every
##       eigenvalue, the coordinates on the retained dimensions (one row per
##       row of the data) and the squared loadings, written at 17 significant
##       digits so nothing is lost in the file
##
## Rerun this when PCAmixdata changes its method, not when illumex changes:
##   Rscript dev/studies/make_pcamix_fixtures.R
pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
if (!requireNamespace("PCAmixdata", quietly = TRUE)) stop("needs PCAmixdata")
source(file.path("tests", "testthat", "helper-famd-cases.R"))
num <- function(x) sprintf("%.17g", x)
rows <- character(0)
for (nm in names(cases <- famd_cases())) {
  cs <- cases[[nm]]
  f <- PCAmixdata::PCAmix(X.quanti = cs$quanti, X.quali = cs$quali, ndim = cs$ndim,
                          rename.level = TRUE, graph = FALSE)
  eig <- f$eig[, 1]
  co <- as.matrix(f$ind$coord)
  sq <- f$sqload
  rows <- c(rows,
    sprintf("%s,eigenvalue,%d,,%s", nm, seq_along(eig), num(eig)),
    unlist(lapply(seq_len(ncol(co)), function(k)
      sprintf("%s,coord,%d,%d,%s", nm, k, seq_len(nrow(co)), num(co[, k])))),
    unlist(lapply(seq_len(ncol(sq)), function(k)
      sprintf("%s,sqload,%d,%s,%s", nm, k, rownames(sq), num(sq[, k])))))
}
out <- file.path("tests", "testthat", "fixtures", "pcamix_cases.csv")
writeLines(c(sprintf("# PCAmixdata %s, by dev/studies/make_pcamix_fixtures.R",
                     format(utils::packageVersion("PCAmixdata"))),
             "case,what,dim,row,value", rows), out)
cat(length(rows), "values written to", out, "\n")
