# ilm_plot_scatter()'s trends (Craig's item 208b adds "gam"): each band is
# held to the fitting function's own predictions, and the plot to tinyplot
# drawing the same points and ribbon.

trend_grid <- function(x, n = 100L) data.frame(x = seq(min(x), max(x), length.out = n))

test_that("the gam trend is mgcv's own fit and credible band", {
  skip_if_not_installed("mgcv")
  b <- ilm_trend_band(mtcars$hp, mtcars$mpg, method = "gam")
  d <- data.frame(x = mtcars$hp, y = mtcars$mpg)
  fit <- mgcv::gam(y ~ s(x), data = d, method = "REML")
  p <- mgcv::predict.gam(fit, trend_grid(d$x), se.fit = TRUE)
  expect_equal(b$x, trend_grid(d$x)$x)
  expect_equal(b$y, as.numeric(p$fit))
  expect_equal(b$ymin, as.numeric(p$fit - stats::qnorm(0.975) * p$se.fit))
  expect_equal(b$ymax, as.numeric(p$fit + stats::qnorm(0.975) * p$se.fit))
})

test_that("the lm trend is predict.lm's confidence band", {
  b <- ilm_trend_band(mtcars$wt, mtcars$mpg, method = "lm")
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  p <- stats::predict(fit, data.frame(wt = trend_grid(mtcars$wt)$x),
                      interval = "confidence", level = 0.95)
  expect_equal(b$y, unname(p[, "fit"]))
  expect_equal(b$ymin, unname(p[, "lwr"]))
  expect_equal(b$ymax, unname(p[, "upr"]))
})

test_that("the loess trend is loess's fit with t on its own degrees of freedom", {
  b <- ilm_trend_band(mtcars$wt, mtcars$mpg, method = "loess")
  fit <- stats::loess(mpg ~ wt, data = mtcars)
  p <- stats::predict(fit, data.frame(wt = trend_grid(mtcars$wt)$x), se = TRUE)
  expect_equal(b$y, unname(p$fit))
  expect_equal(b$ymax - b$y, unname(stats::qt(0.975, p$df) * p$se.fit))
})

test_that("each group and panel gets its own fit", {
  skip_if_not_installed("mgcv")
  b <- ilm_trend_band(mtcars$hp, mtcars$mpg, by = factor(mtcars$am),
                      facet = factor(mtcars$vs), method = "gam")
  expect_identical(nrow(b), 400L)
  one <- mtcars[mtcars$am == 1 & mtcars$vs == 0, ]
  k <- min(10L, length(unique(one$hp)))
  fit <- mgcv::gam(mpg ~ s(hp, k = k), data = one, method = "REML")
  p <- mgcv::predict.gam(fit, data.frame(hp = trend_grid(one$hp)$x))
  expect_equal(b$y[b$by == "1" & b$facet == "0"], as.numeric(p))
})

test_that("a group with too few distinct x values is named and left out", {
  skip_if_not_installed("mgcv")
  d <- data.frame(x = c(1:20, 1, 2, 3), y = stats::rnorm(23), g = rep(c("a", "b"), c(20, 3)))
  expect_message(b <- ilm_trend_band(d$x, d$y, by = factor(d$g), method = "gam"),
                 "not drawn for group b")
  expect_identical(unique(as.character(b$by)), "a")
})

test_that("the plot is the points with the band over them", {
  skip_if_not(capabilities("png"))
  skip_if_not_installed("mgcv")
  png_of <- function(expr) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 500); on.exit(grDevices::dev.off())
    force(expr); f
  }
  for (tr in c("lm", "loess", "gam")) {
    b <- ilm_trend_band(mtcars$hp, mtcars$mpg, method = tr)
    ours <- png_of(ilm_plot_scatter(mtcars, "mpg", "hp", trend = tr))
    theirs <- png_of({
      tinyplot::tinyplot(x = mtcars$hp, y = mtcars$mpg, type = "points",
                         xlab = "hp", ylab = "mpg",
                         ylim = range(mtcars$mpg, b$ymin, b$ymax))
      tinyplot::tinyplot_add(x = b$x, y = b$y, ymin = b$ymin, ymax = b$ymax,
                             type = "ribbon")
    })
    expect_identical(unname(tools::md5sum(ours)), unname(tools::md5sum(theirs)),
                     label = tr)
  }
  ## the points are there: the plot is not the band alone
  band_only <- png_of(tinyplot::tinyplot(mpg ~ hp, data = mtcars, type = "lm"))
  expect_false(identical(unname(tools::md5sum(band_only)),
                         unname(tools::md5sum(png_of(
                           ilm_plot_scatter(mtcars, "mpg", "hp", trend = "lm"))))))
})

test_that("trend, by and facet draw together", {
  skip_if_not(capabilities("png"))
  skip_if_not_installed("mgcv")
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  on.exit(grDevices::dev.off())
  expect_silent(ilm_plot_scatter(transform(mtcars, am = factor(am)), "mpg", "hp",
                                 by = "am", facet = "vs", trend = "gam"))
})

test_that("a line needs only two distinct x values: Anscombe's fourth set", {
  b <- ilm_trend_band(anscombe$x4, anscombe$y4, method = "lm")
  fit <- stats::lm(y4 ~ x4, data = anscombe)
  p <- stats::predict(fit, data.frame(x4 = trend_grid(anscombe$x4)$x),
                      interval = "confidence")
  expect_equal(b$y, unname(p[, "fit"]))
  expect_equal(b$ymax, unname(p[, "upr"]))
  expect_message(ilm_trend_band(c(1, 1, 2, 2), c(1, 2, 3, 4), method = "gam"),
                 "not drawn")
})
