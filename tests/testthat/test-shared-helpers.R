# The helpers illumex shares with illume (R/shared-helpers.R, the same in
# both packages; dev/check_shared_helpers.R compares the two files).

test_that("ilm_and() joins a list for a sentence, and gives '' for nothing", {
  expect_identical(ilm_and(character(0)), "")
  expect_identical(ilm_and(""), "")
  expect_identical(ilm_and("a"), "a")
  expect_identical(ilm_and(c("a", "b")), "a and b")
  expect_identical(ilm_and(c("a", "b", "c")), "a, b and c")
})
