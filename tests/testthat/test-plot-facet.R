# Craig's item 203: `facet` names a column, as `by` does. Each plot is held
# to tinyplot drawing the same panels from its own formula interface: both
# are rendered to PNG in this session and must be the same file, byte for
# byte. With `by`, the legend is titled with the column's name, as tinyplot's
# formula interface titles it.

facet_png <- function(expr) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 700, 500)
  on.exit(grDevices::dev.off())
  suppressMessages(force(expr))
  f
}
expect_same_panels <- function(ours, theirs) {
  a <- facet_png(ours); b <- facet_png(theirs)
  expect_identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
}
facet_cars <- function() {
  mt <- mtcars
  mt$trans <- factor(mt$am, labels = c("auto", "manual"))
  mt$cyl_f <- factor(mt$cyl)
  mt
}

test_that("each plot draws the panels tinyplot draws", {
  skip_if_not(capabilities("png"))
  mt <- facet_cars()
  tp <- tinyplot::tinyplot
  expect_same_panels(ilm_plot_histogram(mt, "mpg", facet = "cyl"),
    tp(~mpg, data = mt, facet = ~cyl, type = tinyplot::type_histogram(breaks = "Sturges"),
       xlab = "mpg"))
  expect_same_panels(ilm_plot_density(mt, "mpg", facet = "am"),
    tp(~mpg, data = mt, facet = ~am, type = "density", xlab = "mpg"))
  expect_same_panels(ilm_plot_scatter(mt, "mpg", "wt", facet = "cyl"),
    tp(mpg ~ wt, data = mt, facet = ~cyl, xlab = "wt", ylab = "mpg"))
  expect_same_panels(ilm_plot_box(mt, "mpg", x = "gear", facet = "am"),
    tp(mpg ~ gear, data = mt, facet = ~am, type = "boxplot", ylab = "mpg"))
  expect_same_panels(ilm_plot_violin(mt, "mpg", x = "cyl_f", facet = "trans"),
    tp(mpg ~ cyl_f, data = mt, facet = ~trans, type = "violin", ylab = "mpg"))
  expect_same_panels(ilm_plot_bar(mt, "gear", facet = "trans"),
    tp(~gear, data = mt, facet = ~trans, type = "barplot", xlab = "gear"))
  o <- mt[order(mt$wt), ]
  expect_same_panels(ilm_plot_line(o, "mpg", "wt", facet = "cyl"),
    tp(mpg ~ wt, data = o, facet = ~cyl, type = "lines", xlab = "wt", ylab = "mpg"))
  expect_same_panels(ilm_plot(mt, "wt", "mpg", facet = "cyl", verdict = FALSE),
    tp(mpg ~ wt, data = mt, facet = ~cyl, type = "p", main = "mpg by wt",
       xlab = "wt", ylab = "mpg"))
  ## through ilm_plot_var(), which hands it on
  expect_same_panels(ilm_plot_var(mt, "mpg", "wt", facet = "cyl"),
    tp(mpg ~ wt, data = mt, facet = ~cyl, xlab = "wt", ylab = "mpg"))
})

test_that("facet and by together draw the panels tinyplot draws", {
  skip_if_not(capabilities("png"))
  mt <- facet_cars()
  tp <- tinyplot::tinyplot
  expect_same_panels(
    ilm_plot_scatter(mt, "mpg", "wt", by = "am", facet = "cyl"),
    tp(mpg ~ wt | am, data = mt, facet = ~cyl, xlab = "wt", ylab = "mpg"))
  expect_same_panels(
    ilm_plot_box(mt, "mpg", x = "gear", by = "cyl_f", facet = "trans"),
    tp(mpg ~ gear | cyl_f, data = mt, facet = ~trans, type = "boxplot", ylab = "mpg"))
  expect_same_panels(
    ilm_plot_histogram(mt, "mpg", by = "trans", facet = "cyl"),
    tp(~mpg | trans, data = mt, facet = ~cyl,
       type = tinyplot::type_histogram(breaks = "Sturges"), xlab = "mpg"))
  expect_same_panels(
    ilm_plot_bar(mt, "gear", by = "cyl_f", facet = "trans"),
    tp(~gear | cyl_f, data = mt, facet = ~trans, type = "barplot", xlab = "gear"))
  expect_same_panels(ilm_plot(mt, "wt", "mpg", by = "trans", facet = "cyl", verdict = FALSE),
    tp(mpg ~ wt | trans, data = mt, facet = ~cyl, type = "p", main = "mpg by wt",
       xlab = "wt", ylab = "mpg"))
})

test_that("stat_error draws each panel's own summaries", {
  skip_if_not(capabilities("png"))
  mt <- facet_cars()
  ## the means and standard errors, worked out independently
  s <- do.call(rbind, lapply(split(mt, list(mt$cyl, mt$trans), drop = TRUE), function(g)
    data.frame(x = g$cyl[1], facet = g$trans[1], m = mean(g$mpg),
               se = stats::sd(g$mpg) / sqrt(nrow(g)))))
  s <- s[order(s$facet, s$x), ]
  expect_same_panels(ilm_plot_stat_error(mt, "mpg", "cyl", facet = "trans"),
    tinyplot::tinyplot(x = s$x, y = s$m, ymin = s$m - s$se, ymax = s$m + s$se,
                       facet = s$facet, type = "pointrange", xlab = "cyl",
                       ylab = "mean mpg"))
})

test_that("a formula is turned away with the column-name form", {
  expect_error(ilm_plot_scatter(mtcars, "mpg", "wt", facet = ~cyl),
               'facet = "cyl"', fixed = TRUE)
  expect_error(ilm_plot_histogram(mtcars, "mpg", facet = ~ cyl + am),
               'facet = "cyl" (one column)', fixed = TRUE)
  expect_error(ilm_plot(mtcars, "wt", "mpg", facet = ~cyl), 'facet = "cyl"', fixed = TRUE)
  expect_error(ilm_plot_scatter(mtcars, "mpg", "wt", facet = mtcars$cyl),
               "single column name, as a string")
  expect_error(ilm_plot_bar(mtcars, "gear", facet = "nope"), "`facet` takes column names; nope is not one", fixed = TRUE)
  expect_error(ilm_plot(mtcars, "wt", "mpg", facet = "nope"), "`facet` takes column names; nope is not one", fixed = TRUE)
})

test_that("a binned density has no panels, and says what to use", {
  d <- data.frame(a = stats::rnorm(60), b = stats::rnorm(60), g = rep(1:3, 20))
  expect_error(ilm_plot(d, "a", "b", geom = "bin2d", facet = "g"),
               'geom = "point"', fixed = TRUE)
})

test_that("a legend the caller gives is kept, in each form tinyplot takes", {
  skip_if_not(capabilities("png"))
  mt <- facet_cars()
  tp <- tinyplot::tinyplot
  expect_same_panels(ilm_plot_scatter(mt, "mpg", "wt", by = "trans", legend = "bottom!"),
    tp(mpg ~ wt | trans, data = mt, legend = "bottom!", xlab = "wt", ylab = "mpg"))
  expect_same_panels(
    ilm_plot_scatter(mt, "mpg", "wt", by = "trans", legend = legend("bottom!", title = "T")),
    tp(mpg ~ wt | trans, data = mt, legend = legend("bottom!", title = "T"),
       xlab = "wt", ylab = "mpg"))
  expect_same_panels(
    ilm_plot_line(mt[order(mt$wt), ], "mpg", "wt", by = "trans", legend = list(title = "Gearbox")),
    tp(mpg ~ wt | trans, data = mt[order(mt$wt), ], type = "lines",
       legend = list(title = "Gearbox"), xlab = "wt", ylab = "mpg"))
  expect_same_panels(ilm_plot_bar(mt, "gear", by = "cyl_f", legend = FALSE),
    tp(~gear | cyl_f, data = mt, type = "barplot", legend = FALSE, xlab = "gear"))
  ## and with a trend, whose band is added to the same call
  expect_silent(facet_png(ilm_plot_scatter(mt, "mpg", "wt", by = "trans",
                                           trend = "lm", legend = "bottom!")))
})
