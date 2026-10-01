# Craig's item 206: when rows are copies, ilm_copies() and ilm_dupes() say
# how many and give the call that keeps one of each, from one helper.

test_that("the copies line is the ruled wording, on messy_cars", {
  data <- messy_cars_data()
  want <- paste0("22 of 44 rows share their values with another row; ",
                 "ilm_copies(data, filter = \"first\") keeps one of each, leaving 32.")
  expect_message(ilm_copies(data), want, fixed = TRUE)
  expect_message(ilm_dupes(data), want, fixed = TRUE)
  expect_message(ilm_copies(data, filter = "dupes"), want, fixed = TRUE)
  ## and the remedy it names does what it says
  expect_identical(nrow(ilm_copies(data, filter = "first")), 32L)
})

test_that("the line names the data and the key columns as the caller wrote them", {
  mc <- messy_cars_data()
  expect_message(ilm_dupes(mc), "ilm_copies(mc, filter = \"first\")", fixed = TRUE)
  expect_message(ilm_dupes(mc[1:40, ]), "ilm_copies(data, filter = \"first\")", fixed = TRUE)
  expect_message(ilm_dupes(mc, "am"),
                 "44 of 44 rows share their values of am with another row; ilm_copies(mc, \"am\", filter = \"first\") keeps one of each, leaving 2.",
                 fixed = TRUE)
  expect_message(ilm_copies(mc, c("am", "gear")),
                 "share their values of am and gear with another row; ilm_copies(mc, c(\"am\", \"gear\"), filter = \"first\")",
                 fixed = TRUE)
  expect_message(iml_dupes(mc), "ilm_copies(mc, filter = \"first\")", fixed = TRUE)
})

test_that("the line counts with thousands separators", {
  expect_message(ilm_copies_note(1200L, 25000L, 24400L),
                 "1,200 of 25,000 rows share their values with another row; ilm_copies(data, filter = \"first\") keeps one of each, leaving 24,400.",
                 fixed = TRUE)
})

test_that("nothing is said when there are no copies, or when filtering", {
  expect_silent(ilm_copies(mtcars))
  expect_silent(ilm_dupes(mtcars[0, ]))
  mc <- messy_cars_data()
  for (f in c("first", "last", "unique")) expect_silent(ilm_copies(mc, filter = f))
})
