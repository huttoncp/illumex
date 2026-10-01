# A messy copy of mtcars for the cleaning tests, built in base R: a number
# typed as a word, missing-value codes (999, -1, "N/A"), names that need
# cleaning, an empty column and twelve copied rows (44 rows, 32 distinct).
messy_cars_data <- function() {
  mc <- mtcars[c("mpg", "cyl", "disp", "hp", "wt", "gear", "am")]
  mc$cyl <- ifelse(mc$cyl == 6, "six", as.character(mc$cyl))
  mc$gear <- ifelse(mc$gear == 4 & mc$mpg > 20, "four", as.character(mc$gear))
  names(mc)[1:3] <- c("Miles per gallon", "# of cylinders", "DISP")
  mc$notes <- NA
  mc$hp <- ifelse(mc$hp == max(mc$hp), 999, mc$hp)
  mc$wt <- ifelse(mc$wt < stats::quantile(mc$wt, 0.1), -1, mc$wt)
  mc$DISP <- ifelse(mc$DISP > 300, "N/A", as.character(mc$DISP))
  set.seed(1234)
  mc <- rbind(mc, mc[sample(nrow(mc), 12, replace = TRUE), ])
  rownames(mc) <- NULL
  mc
}
