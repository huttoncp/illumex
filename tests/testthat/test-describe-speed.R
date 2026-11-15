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
