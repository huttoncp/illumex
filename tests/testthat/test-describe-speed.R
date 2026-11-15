## The describe functions work out the groups once for a call and skip the
## floor scale when nothing reads it. Neither may change what they return or
## the messages they give: each call in helper-describe-speed.R is held to
## the record made on the commit before those changes
## (fixtures/describe_speed.rds, made by dev/describe_speed/make_fixtures.R,
## which stores the commit it was made on).

record <- readRDS(test_path("fixtures", "describe_speed.rds"))

test_that("the record covers every call", {
  expect_setequal(names(record), names(describe_speed_calls))
  expect_true(nzchar(attr(record, "recorded_on")))
})

test_that("every call returns exactly what it did, with the same messages", {
  d <- describe_speed_data()
  for (k in names(describe_speed_calls)) {
    now <- describe_speed_run(describe_speed_calls[[k]], d)
    expect_identical(now$value, record[[k]]$value, label = k)
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
