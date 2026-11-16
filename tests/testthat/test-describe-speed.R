## The describe functions work out the groups once for a call and skip the
## floor scale when nothing reads it. Neither may change what they return or
## the messages they give: each call in helper-describe-speed.R is held to
## the record made on the commit before those changes
## (fixtures/describe_speed.rds, made by dev/describe_speed/make_fixtures.R,
## which stores the commit it was made on).
##
## The record was made on one platform and R version (Windows, R 4.4.3). On
## other platforms and versions many numbers differ from it in their last
## digits -- among them sums, standard deviations and errors, dispersion,
## standardised differences, the Gaussian index, skewness and kurtosis -- by
## at most about 3 parts in 10^15 in the checks run so far. So the results
## are held to the record with a relative tolerance of 1e-12 (a single value
## changed by one part in 10^10 still fails), while every element's type and
## every message must be exactly the same. Off the platform the record was made on, this shows agreement to one
## part in 10^12, not identity; on it, each change was checked with
## identical() when it was built.

record <- readRDS(test_path("fixtures", "describe_speed.rds"))

## every element's type, all the way down, which a numeric tolerance ignores
types_of <- function(x) rapply(unclass(x), function(v) c(typeof(v), class(v)), how = "list")

test_that("the record covers every call", {
  expect_setequal(names(record), names(describe_speed_calls))
  expect_true(nzchar(attr(record, "recorded_on")))
})

test_that("every call returns exactly what it did, with the same messages", {
  d <- describe_speed_data()
  for (k in names(describe_speed_calls)) {
    now <- describe_speed_run(describe_speed_calls[[k]], d)
    expect_equal(now$value, record[[k]]$value, tolerance = 1e-12, label = k)
    expect_identical(types_of(now$value), types_of(record[[k]]$value),
                     label = paste(k, "types"))
    expect_identical(now$messages, record[[k]]$messages, label = paste(k, "messages"))
  }
})

test_that("the record holds the cases it is meant to", {
  ## an empty group, infinite values and two by variables are in it
  num <- record$all_by1_none$value$numeric
  expect_true(any(num$variable == "part" & num$n == 0L))
  expect_true("n_inf" %in% names(record$several_by1$value))
  expect_true(any(grepl("infinite", record$all_by2$messages)))
  expect_true("g2.g3" %in% names(record$all_by2$value$numeric))
  expect_true(all(is.na(record$all_empty_na$value$numeric$sum[
    record$all_empty_na$value$numeric$variable == "part" &
      record$all_empty_na$value$numeric$g3 == "w"])))
})

test_that("kept groups are used only for the rows they were made from", {
  d <- describe_speed_data()
  kept <- illumex:::ilm_describe_groups_keep(d, "g3")
  on.exit(illumex:::ilm_describe_groups_restore(kept))
  ## the same frame: the kept groups
  same <- illumex:::ilm_describe_groups_get(d, "g3")
  expect_identical(same$g, interaction(d["g3"], drop = TRUE))
  ## a frame of the same height in another row order: its own groups
  r <- d[rev(seq_len(nrow(d))), ]
  other <- illumex:::ilm_describe_groups_get(r, "g3")
  expect_identical(other$g, interaction(r["g3"], drop = TRUE))
  expect_false(identical(other$g, same$g))
  ## and the same height with other values in the by column
  d2 <- d; d2$g3 <- rev(d2$g3)
  expect_identical(illumex:::ilm_describe_groups_get(d2, "g3")$g,
                   interaction(d2["g3"], drop = TRUE))
})

test_that("ilm_describe_na_all() counts in one pass what ilm_describe_na() counts column by column", {
  d <- describe_speed_data()
  for (b in list(NULL, "g3", c("g2", "g3"))) {
    one <- ilm_describe_na_all(d, by = b, sort = FALSE)
    use <- setdiff(names(d), b)
    each <- do.call(rbind, lapply(use, function(cn)
      cbind(variable = cn, as.data.frame(ilm_describe_na(d, cn, by = b)), stringsAsFactors = FALSE)))
    rownames(each) <- NULL
    expect_equal(as.data.frame(one), each, ignore_attr = TRUE)
  }
  ## a list column is counted as ilm_describe_na() counts it
  dl <- data.frame(g = rep(c("a", "b"), 3), x = c(1, NA, 3, 4, NA, 6))
  dl$l <- I(list(1, NULL, 3, NA, 5, 6))
  r <- ilm_describe_na_all(dl, by = "g", sort = FALSE)
  expect_equal(r$variable, rep(c("x", "l"), each = 2))
  expect_equal(r$n[r$variable == "x"], c(2L, 2L))
})
