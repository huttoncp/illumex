## The data and calls that pin what the describe functions return while their
## grouping is made faster: test-describe-speed.R holds each call's result and
## messages to the record in fixtures/describe_speed.rds, which
## dev/describe_speed/make_fixtures.R made from these same calls on the commit
## before the change (it records which).

## made-up data: every class of column, missing, infinite and empty groups,
## and two grouping variables (g3 has a missing value)
describe_speed_data <- function() {
  set.seed(4401)
  n <- 240L
  d <- data.frame(
    g2 = rep(c("p", "q"), length.out = n),
    g3 = rep(c("u", "v", "w"), each = n / 3),
    num = round(stats::rnorm(n, 10, 3), 3),
    int = sample(0:40, n, TRUE),
    rate = sample(1:5, n, TRUE, prob = c(0.5, 0.2, 0.1, 0.1, 0.1)),
    cnt = stats::rpois(n, 2),
    ord = factor(sample(c("low", "mid", "high", "top"), n, TRUE, prob = c(0.6, 0.2, 0.1, 0.1)),
                 levels = c("low", "mid", "high", "top"), ordered = TRUE),
    fac = factor(sample(c("a", "b", "c", "d", "e"), n, TRUE)),
    chr = sample(c("x", "y", "z", "X ", ""), n, TRUE),
    lgl = stats::runif(n) < 0.3,
    day = as.Date("2021-01-01") + sample(0:400, n, TRUE),
    when = as.POSIXct("2021-01-01", tz = "UTC") + sample(0:5e6, n, TRUE),
    same = 7,
    stringsAsFactors = FALSE)
  d$num[sample(n, 12)] <- NA
  d$int[sample(n, 5)] <- NA
  d$fac[sample(n, 7)] <- NA
  d$day[sample(n, 4)] <- NA
  d$g3[c(5, 100)] <- NA
  ## infinite values, and a column with no values in group w
  d$inf <- d$num
  d$inf[c(2, 9, 150)] <- c(Inf, -Inf, Inf)
  d$part <- ifelse(d$g3 %in% "w", NA_real_, d$num)
  d
}

## each call: a function of the data, named by what it covers
describe_speed_calls <- list(
  all            = function(d) ilm_describe_all(d),
  all_by1        = function(d) ilm_describe_all(d, by = "g2"),
  all_by2        = function(d) ilm_describe_all(d, by = c("g2", "g3")),
  all_by1_smd    = function(d) ilm_describe_all(d, by = "g2", smd = TRUE),
  all_by2_smd    = function(d) ilm_describe_all(d, by = c("g3", "g2"), smd = TRUE),
  all_by1_opts   = function(d) ilm_describe_all(d, by = "g3", gauss = "both", skew = TRUE,
                                                kurt = TRUE, probs = c(0.1, 0.5, 0.9)),
  all_by1_none   = function(d) ilm_describe_all(d, by = "g3", gauss = "none", dispersion = FALSE),
  all_none       = function(d) ilm_describe_all(d, gauss = "none", dispersion = FALSE),
  all_by1_disp   = function(d) ilm_describe_all(d, by = "g2", gauss = "none", dispersion = TRUE),
  all_by1_ks     = function(d) ilm_describe_all(d, by = "g2", gauss = "ks_d"),
  all_class      = function(d) ilm_describe_all(d, by = "g3", class = "numeric"),
  all_cols       = function(d) ilm_describe_all(d, by = "g2", cols = "^(num|ord|day)$"),
  all_subset     = function(d) ilm_describe_all(d, by = "g3", subset = d$int > 10),
  all_empty_na   = function(d) ilm_describe_all(d, by = "g3", empty = NA),
  all_digits     = function(d) ilm_describe_all(d, by = "g2", digits = 1),
  one_by1        = function(d) ilm_describe(d, "num", by = "g2"),
  one_by2_smd    = function(d) ilm_describe(d, "inf", by = c("g2", "g3"), smd = TRUE),
  one_part       = function(d) ilm_describe(d, "part", by = "g3", empty = -1),
  several_by1    = function(d) ilm_describe(d, c("num", "int", "inf"), by = "g3"),
  several_none   = function(d) ilm_describe(d, c("num", "rate"), by = "g2", gauss = "none",
                                            dispersion = FALSE),
  na_all         = function(d) ilm_describe_na_all(d),
  na_all_by1     = function(d) ilm_describe_na_all(d, by = "g2"),
  na_all_by2     = function(d) ilm_describe_na_all(d, by = c("g2", "g3")),
  na_all_nosort  = function(d) ilm_describe_na_all(d, by = "g3", sort = FALSE),
  na_all_cols    = function(d) ilm_describe_na_all(d, by = "g3", cols = c("num", "fac", "day")),
  na_all_subset  = function(d) ilm_describe_na_all(d, by = "g2", subset = d$int > 10),
  na_all_digits  = function(d) ilm_describe_na_all(d, by = "g2", digits = 2),
  na_one_by1     = function(d) ilm_describe_na(d, "num", by = "g3"))

## a call's result and the messages it gives
describe_speed_run <- function(f, d) {
  msgs <- character(0)
  value <- withCallingHandlers(f(d), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  list(value = value, messages = msgs)
}
