## Probe, not a study run: does column noise push the default selector to heavy shrinkage?
suppressMessages(pkgload::load_all(commandArgs(TRUE)[1], quiet = TRUE, helpers = FALSE))
q <- function(e) suppressMessages(suppressWarnings(e))
for (nsd in c(0.5, 1.5, 2.5)) for (s in 1:2) {
  set.seed(9000 + s)
  g <- sample(rep_len(1:3, 300)); z <- rbind(c(0,0), c(3,0), c(0,3))[g, ] + matrix(rnorm(600), 300, 2)
  X <- z %*% matrix(rnorm(12), 2, 6) + matrix(rnorm(1800, sd = nsd), 300, 6)
  d <- as.data.frame(X); M <- matrix(runif(1800) < 0.1, 300, 6); for (j in 1:6) d[[j]][M[, j]] <- NA
  cat("noise sd", nsd, "rep", s, "default lambda", q(ilm_glrm(d, rank = 5, lambda = NULL, progress = FALSE))$lambda, "\n")
}
