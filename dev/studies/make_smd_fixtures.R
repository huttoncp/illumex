## Records tableone's standardised mean differences for the cases in
## tests/testthat/helper-smd-cases.R, so that illumex's (R/ilm_smd.R) are held
## to them in tests/testthat/test-smd.R without tableone installed. Craig
## approved tableone for this (2026-09-27), in a private library only.
##
## tableone reports each SMD unsigned; for a category of more than two levels
## it is Yang and Dalton's multivariate difference, as illumex's is.
##
## Run from the package root, with tableone on the library path:
##   Rscript dev/studies/make_smd_fixtures.R
if (!requireNamespace("tableone", quietly = TRUE)) stop("this needs tableone")
source("tests/testthat/helper-smd-cases.R")
rows <- list()
for (i in seq_along(cs <- smd_cases())) {
  d <- cs[[i]]
  t1 <- tableone::CreateTableOne(vars = c("x", "b", "c3", "c5"), strata = "g",
                                 data = d, test = FALSE)
  sm <- tableone::ExtractSmd(t1)
  for (v in rownames(sm))
    rows[[length(rows) + 1L]] <- data.frame(case = i, variable = v, smd = sm[v, 1])
}
out <- do.call(rbind, rows)
hdr <- c(sprintf("# tableone %s, R %s, written by dev/studies/make_smd_fixtures.R",
                 utils::packageVersion("tableone"), getRversion()))
f <- "tests/testthat/fixtures/smd_tableone.csv"
writeLines(hdr, f)
suppressWarnings(utils::write.table(format(out, digits = 17), f, append = TRUE, sep = ",",
                                    row.names = FALSE, quote = FALSE))
print(out, digits = 12)
