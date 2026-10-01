# Data for the SMD comparison with tableone: made the same way here and by
# dev/studies/make_smd_fixtures.R, which records tableone's SMDs for them
# (tests/testthat/fixtures/smd_tableone.csv), so tableone itself is not
# needed to run the tests.
smd_cases <- function() {
  lapply(1:4, function(s) {
    set.seed(100 + s)
    n <- 60 + 20 * s
    g <- sample(c("ref", "trt"), n, TRUE, prob = c(0.5 + s / 20, 0.5 - s / 20))
    d <- data.frame(
      g = g,
      x = stats::rnorm(n, ifelse(g == "trt", 0.3 * s, 0), 1 + s / 4),
      b = factor(stats::rbinom(n, 1, ifelse(g == "trt", 0.3 + s / 20, 0.3)),
                 levels = 0:1, labels = c("no", "yes")),
      c3 = factor(sample(c("a", "b", "c"), n, TRUE), levels = c("a", "b", "c")),
      c5 = factor(sample(letters[1:5], n, TRUE,
                         prob = if (s %% 2) c(0.3, 0.3, 0.2, 0.1, 0.1) else rep(0.2, 5)),
                  levels = letters[1:5]),
      stringsAsFactors = FALSE)
    d$x[sample(n, 3)] <- NA
    d$c3[sample(n, 2)] <- NA
    d
  })
}
