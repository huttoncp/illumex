## Mean excess kurtosis of normal samples, type 3 (e1071's default, what
## illumex gave before item 205) against type 2 (Joanes and Gill 1998).
## Rscript dev/studies/kurt_bias.R .   Measured 2026-09-28:
##   n 20 type3 mean -0.544 type2 mean 0.009 se 0.007
##   n 50 type3 mean -0.242 type2 mean -0.012 se 0.005
suppressMessages(pkgload::load_all(commandArgs(TRUE)[1], quiet = TRUE))
set.seed(205)
for (n in c(20, 50)) { r <- replicate(20000, { x <- rnorm(n); z <- (x - mean(x)) / sd(x); c(mean(z^4) - 3, ilm_kurt2(x)) })
  cat("n", n, "type3 mean", round(mean(r[1, ]), 3), "type2 mean", round(mean(r[2, ]), 3), "se", round(sd(r[2, ]) / sqrt(20000), 3), "\n") }
