# Printed output that must not change when results gain a class or keep full
# precision: every function whose result has a class of its own, and
# every print that shows ilm_cluster()'s or ilm_reduce()'s tables.
# dev/make_print_fixtures.R records these texts from the code before the
# change; test-print-unchanged.R holds the code after it to them. Each case
# sets its own seed, so the text is the same wherever it runs.
print_cases <- function() {
  q <- function(e) suppressMessages(suppressWarnings(e))
  d <- ilm_sim(n_id = 20, n_period = 6)
  dd <- d[c("score", "income", "visits", "claims", "grp")]
  mt <- mtcars
  mt$wt_copy <- mt$wt
  mt$cyl_f <- factor(mt$cyl)
  mt$cyl_g <- factor(mt$cyl, labels = c("four", "six", "eight"))
  list(
    gauss_normal = function() q(ilm_gauss_check(stats::qnorm(stats::ppoints(500)))),
    gauss_lognormal = function() q(ilm_gauss_check(exp(stats::qnorm(stats::ppoints(500))))),
    gauss_mixture = function() q(ilm_gauss_check(c(stats::qnorm(stats::ppoints(250)),
                                                   stats::qnorm(stats::ppoints(250)) + 3))),
    ## 119 rows: with all 120, a printed value (34420.195) sits on a decimal
    ## tie that x86 and arm64 round to different decimal counts
    describe_gauss_both = function() q(ilm_describe(d[-1, ], "income", gauss = "both")),
    describe_number = function() q(ilm_describe(d, "score")),
    describe_category = function() q(ilm_describe(d, "grp")),
    describe_by = function() q(ilm_describe(d, "score", by = "grp")),
    describe_all = function() q(ilm_describe_all(d)),
    describe_all_numeric = function() q(ilm_describe_all(d, class = "numeric")),
    describe_all_by = function() q(ilm_describe_all(dd, by = "grp")),
    frame_issues_sim = function() q(ilm_frame_issues(d)),
    frame_issues_mtcars = function() q(ilm_frame_issues(mt)),
    describe_na = function() q(ilm_describe_na(d, "lab_value")),
    describe_na_by = function() q(ilm_describe_na(d, "lab_value", by = "grp")),
    describe_na_all = function() q(ilm_describe_na_all(d)),
    describe_na_all_by = function() q(ilm_describe_na_all(d, by = "grp")),
    outliers_iqr = function() q(ilm_outliers(d$income)),
    outliers_mad = function() q(ilm_outliers(d$income, "mad")),
    outliers_all = function() q(ilm_outliers_all(d)),
    outliers_all_by = function() q(ilm_outliers_all(d, by = "grp")),
    outliers_all_everything = function() q(ilm_outliers_all(mtcars, flagged_only = FALSE)),
    boot_ci = function() q(ilm_boot_ci(d, "score", R = 200, seed = 1)),
    boot_ci_by = function() q(ilm_boot_ci(d, "score", by = "grp", R = 200, seed = 1)),
    boot_ci_vector = function() q(ilm_boot_ci(d$score, R = 200, seed = 1)),
    boot_diff = function() q(ilm_boot_diff(d, "score", "grp", R = 200, seed = 1)),
    boot_diff_formula = function() q(ilm_boot_diff(score ~ grp, data = d, ref = "alpha",
                                                   R = 200, seed = 1)),
    reduce = function() q(ilm_reduce(dd)),
    reduce_glrm = function() q(ilm_reduce(dd, method = "glrm", progress = FALSE)),
    cluster = function() q(ilm_cluster(ilm_reduce(dd), k_max = 4, B = 10, seed = 1)),
    cluster_k3 = function() q(ilm_cluster(ilm_reduce(dd), k = 3, B = 10, seed = 1)),
    cluster_hclust = function() q(ilm_cluster(ilm_reduce(dd), k = 3, method = "hclust",
                                              B = 10, seed = 1)),
    profile = function() q(ilm_profile(dd, k = 3, B = 10, seed = 1, var_contrib_B = 19)),
    reduce_na = function() q(ilm_reduce_na(airquality)),
    cluster_na = function() q(ilm_cluster_na(ilm_reduce_na(airquality), k_max = 4,
                                             B = 10, seed = 1)),
    profile_na = function() q(ilm_profile_na(airquality, k_max = 4, B = 10, seed = 1))
  )
}

## the text a result prints, at a fixed width, built after a fixed seed
print_text <- function(make) {
  old <- options(width = 80); on.exit(options(old), add = TRUE)
  set.seed(20260928)
  x <- make()
  utils::capture.output(print(x))
}
