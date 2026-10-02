## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## What a user sees today for whole-number variables with a real pile at a
## bound (floor-rule study, b940e35): describe's print and gauss_note, and
## whether ilm_frame_issues() says anything. (A line that printed a field
## of an export feature since removed is left out.)
.libPaths(c(commandArgs(TRUE)[1], .libPaths()))
suppressMessages(library(illumex))
held <- function(x, lo, hi) pmin(pmax(x, lo), hi)
set.seed(7)
mk <- list(
  "rating 0-10, 20% extra at 0, n = 300" = function() { x <- held(round(rnorm(300, 5, 2)), 0, 10); x[runif(300) < .2] <- 0; x },
  "rating 1-10, 20% extra at 1, n = 300" = function() { x <- held(round(rnorm(300, 5, 2)), 1, 10); x[runif(300) < .2] <- 1; x },
  "zero-inflated count, 10% extra zeros, n = 300" = function() ifelse(runif(300) < .1, 0, rnbinom(300, size = 3, mu = 15)),
  "zero-inflated count, 10% extra zeros, n = 5000" = function() ifelse(runif(5000) < .1, 0, rnbinom(5000, size = 3, mu = 15)),
  "0-100 rating, 10% extra at 0, n = 5000" = function() { x <- held(round(rnorm(5000, 45, 20)), 0, 100); x[runif(5000) < .1] <- 0; x })
options(width = 110)
for (nm in names(mk)) {
  x <- mk[[nm]]()
  cat("\n=====", nm, "\n")
  cat(sprintf("share at the minimum %.3f, at the next value %.3f, distinct values %d\n",
              mean(x == min(x)), mean(x == sort(unique(x))[2]), length(unique(x))))
  d <- data.frame(x = x)
  r <- suppressMessages(ilm_describe(d, "x", gauss = "both"))
  print(r)
  cat("gauss_note:", if (nzchar(r$gauss_note)) r$gauss_note else "(empty)", "\n")
  fi <- suppressMessages(ilm_frame_issues(d))
  cat("ilm_frame_issues:", if (NROW(fi)) paste(fi$issue, fi$detail, collapse = "; ") else "nothing", "\n")
}
