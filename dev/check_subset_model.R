## `subset = <model>` against real illume fits, which the tests imitate
## (illume is not among illumex's Suggests): the rows an ilm_model()
## analysed and those every set of an ilm_dag_model() used, and the copies
## kept beside the result equal to the fit's own under identical().
## Run from illumex's root, with illume installed:
##   Rscript dev/check_subset_model.R
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
stopifnot(requireNamespace("illume", quietly = TRUE))
cat("illume", format(utils::packageVersion("illume")), "\n")
set.seed(1); n <- 300
z <- rnorm(n); x <- 0.5 * z + rnorm(n); w <- rnorm(n); y <- 0.4 * x + 0.6 * z + rnorm(n)
d <- data.frame(x = x, y = y, z = z, w = w)
d$x[c(3, 7)] <- NA; d$z[c(7, 90, 91)] <- NA; d$w[c(5, 200)] <- NA
fit <- suppressMessages(illume::ilm_model(y ~ x + z, data = d))
t <- ilm_describe_all(d, subset = fit)
s <- attr(t, "ilm_select")$subset
same <- function(a, b) if (identical(a, b)) "identical" else "DIFFERENT"
cat(sprintf("ilm_model: described %d of %d rows; the fit analysed %d\n", s$n_rows_kept,
            s$n_rows_given, nrow(fit$model)))
cat("  formula", same(s$model$fit$formula, fit[["formula"]]),
    "| na.action", same(s$model$fit$na.action, fit[["na.action"]]),
    "| n_input", same(s$model$fit$n_input, fit[["n_input"]]),
    "| subset", same(s$model$fit$subset, fit[["subset"]]), "\n")
stopifnot(s$n_rows_kept == nrow(fit$model), identical(s$model$fit$formula, fit[["formula"]]),
          identical(s$model$fit$na.action, fit[["na.action"]]))
g <- illume::ilm_dag("dag { x [exposure] ; y [outcome] ; z -> x -> y ; z -> y ; w -> y }")
dm <- suppressMessages(illume::ilm_dag_model(g, d, verbose = FALSE))
td <- ilm_describe_all(d, subset = dm)
sd <- attr(td, "ilm_select")$subset
used <- Reduce(intersect, lapply(dm$fits, function(f) setdiff(seq_len(nrow(d)), f$na.action)))
cat(sprintf("ilm_dag_model: %d sets; described %d rows, every set used %d\n",
            length(dm$fits), sd$n_rows_kept, length(used)))
for (k in seq_along(dm$fits))
  cat(sprintf("  set %d: %s, formula %s, na.action %s\n", k, deparse(dm$fits[[k]]$formula),
              same(sd$model$fits[[k]]$formula, dm$fits[[k]][["formula"]]),
              same(sd$model$fits[[k]]$na.action, dm$fits[[k]][["na.action"]])))
stopifnot(sd$n_rows_kept == length(used))
print(td)
