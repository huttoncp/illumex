# ilm_plot_anomaly(type = "row") stopped with "invalid graphics state" on a
# device 3 inches wide or narrower: its legend, set outside the plot on the
# right, left the bars no room. html_vignette's default figure is 3 x 3, and
# so is a small plot pane. Every view must draw there.

narrow_scan <- function(names = c("height", "weight", "waist", "shoe")) {
  set.seed(1)
  n <- 300
  size <- rnorm(n)
  a <- data.frame(170 + 9 * size + rnorm(n, 0, 4), 72 + 11 * size + rnorm(n, 0, 6),
                  85 + 9 * size + rnorm(n, 0, 5), 41 + 2.5 * size + rnorm(n, 0, 1))
  names(a) <- names
  a[c(17, 120, 230), 1] <- c(152, 150, 155)
  a[c(17, 120, 230), 2] <- c(98, 100, 97)
  suppressMessages(ilm_anomaly(a, seed = 1))
}

test_that("every view draws on a 3 x 3 inch device", {
  r <- narrow_scan()
  for (ty in c("scores", "drivers", "map", "row")) {
    grDevices::pdf(NULL, width = 3, height = 3)
    expect_no_error(suppressMessages(
      if (ty == "row") ilm_plot_anomaly(r, ty, row = 17) else ilm_plot_anomaly(r, ty)))
    grDevices::dev.off()
  }
})

test_that("the row view drops its legend when it would not fit, and says so", {
  r <- narrow_scan()
  grDevices::pdf(NULL, width = 3, height = 3)
  expect_message(ilm_plot_anomaly(r, "row", row = 17), "too narrow")
  grDevices::dev.off()
  ## with room for it, the legend stays and nothing is said
  grDevices::pdf(NULL, width = 7, height = 4.5)
  expect_no_message(ilm_plot_anomaly(r, "row", row = 17))
  grDevices::dev.off()
})

test_that("long column names still draw on a narrow device", {
  r <- narrow_scan(c("standing_height_in_centimetres", "body_weight_in_kilograms",
                     "waist_circumference_cm", "shoe_size_eu"))
  for (ty in c("drivers", "row")) {
    grDevices::pdf(NULL, width = 3, height = 3)
    expect_no_error(suppressMessages(
      if (ty == "row") ilm_plot_anomaly(r, ty, row = 17) else ilm_plot_anomaly(r, ty)))
    grDevices::dev.off()
  }
})
