# ilm_plot_var_pairs() without `by` failed on every call ("formal argument
# by matched by multiple actual arguments"): it passed by = NULL to
# tinyplot::tinypairs(), which keeps that from its call and adds its own.
# Only an error case was tested, so nothing drew a plot.

test_that("a pairs plot draws without by, the call that failed", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(ilm_plot_var_pairs(ilm_sim()[c("score", "income", "visits")]))
  expect_null(ilm_plot_var_pairs(ilm_sim()[c("score", "income", "visits")]))
})

test_that("a pairs plot draws with by, and with cols", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  d <- ilm_sim()
  expect_no_error(ilm_plot_var_pairs(d[c("score", "income", "visits", "grp")], by = "grp"))
  expect_no_error(ilm_plot_var_pairs(d, cols = c("score", "income")))
  expect_no_error(ilm_plot_var_pairs(d, cols = c("score", "income"), by = c("grp", "site")))
})
