# A description by two variables: standardised differences compare the
# second's groups within each level of the first (exposure within a
# modifier; controls split by exposure; Craig's item 120).

two_data <- function() {
  set.seed(1)
  n <- 240
  data.frame(m = sample(c("young", "old"), n, TRUE), e = sample(c("ctl", "trt"), n, TRUE),
             x = stats::rnorm(n), k = sample(c("p", "q", "r"), n, TRUE),
             stringsAsFactors = FALSE)
}

test_that("standardised differences compare the second's groups within a level of the first", {
  d <- two_data()
  t <- ilm_describe(d, "x", by = c("m", "e"), smd = "ctl")
  s <- stats::setNames(t$smd, t$m.e)
  o <- d[d$m == "old", ]; y <- d[d$m == "young", ]
  expect_equal(s[["old.trt"]], ilm_smd_pair(o$x[o$e == "trt"], o$x[o$e == "ctl"]))
  expect_equal(s[["young.trt"]], ilm_smd_pair(y$x[y$e == "trt"], y$x[y$e == "ctl"]))
  expect_true(is.na(s[["old.ctl"]]) && is.na(s[["young.ctl"]]))
  ## TRUE takes the second's first level
  expect_identical(ilm_describe(d, "x", by = c("m", "e"), smd = TRUE)$smd, t$smd)
  ## a name that is not a level of the second is refused
  expect_error(ilm_describe(d, "x", by = c("m", "e"), smd = "old.ctl"), "not in the data")
  ## a level of the first without the reference group has none
  d2 <- d[!(d$m == "young" & d$e == "ctl"), ]
  s2 <- stats::setNames(ilm_describe(d2, "x", by = c("m", "e"), smd = "ctl")$smd,
                        ilm_describe(d2, "x", by = c("m", "e"))$m.e)
  expect_true(is.na(s2[["young.trt"]]))
  expect_false(is.na(s2[["old.trt"]]))
})

test_that("one variable, or three, compare every group with the reference as before", {
  d <- two_data()
  t1 <- ilm_describe(d, "x", by = "e", smd = "ctl")
  expect_equal(t1$smd[t1$e == "trt"], ilm_smd_pair(d$x[d$e == "trt"], d$x[d$e == "ctl"]))
  d$z <- rep(c("a", "b"), length.out = nrow(d))
  t3 <- ilm_describe(d, "x", by = c("m", "e", "z"))
  expect_identical(nrow(t3), 8L)
})
