# The helpers illumex shares with illume (R/shared-helpers.R, the same in
# both packages; dev/check_shared_helpers.R compares the two files).

test_that("ilm_and() joins a list for a sentence, and gives '' for nothing", {
  expect_identical(ilm_and(character(0)), "")
  expect_identical(ilm_and(""), "")
  expect_identical(ilm_and("a"), "a")
  expect_identical(ilm_and(c("a", "b")), "a and b")
  expect_identical(ilm_and(c("a", "b", "c")), "a, b and c")
})

## R/shared-helpers.R collates late, so a name it defines that is also
## defined elsewhere in R/ silently replaces the other (illume's
## ilm_disp_scale(), its dispersion helper, became NA standard errors). The
## installed namespace keeps only the winner, so the check reads the sources:
## R/ in a source tree, or the copy R CMD check keeps in 00_pkg_src.
test_that("no name defined in R/shared-helpers.R is defined anywhere else in R/", {
  dirs <- c(test_path("..", "..", "R"),
            test_path("..", "..", "00_pkg_src", "illumex", "R"))
  r_dir <- dirs[file.exists(file.path(dirs, "shared-helpers.R"))][1]
  skip_if(is.na(r_dir), "the package's R sources are not here")
  ## names assigned at the top level, with <-, = or <<-, by name or string
  top_names <- function(f) {
    ex <- parse(f, keep.source = FALSE)
    nm <- vapply(ex, function(e) {
      if (!is.call(e) || !is.name(e[[1L]]) || length(e) != 3L ||
          !as.character(e[[1L]]) %in% c("<-", "=", "<<-")) return(NA_character_)
      lhs <- e[[2L]]
      if (is.name(lhs)) as.character(lhs)
      else if (is.character(lhs) && length(lhs) == 1L) lhs
      else NA_character_
    }, "")
    nm[!is.na(nm)]
  }
  files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  others <- files[basename(files) != "shared-helpers.R"]
  shared <- top_names(file.path(r_dir, "shared-helpers.R"))
  expect_gt(length(shared), 10L)
  clash <- intersect(shared, unlist(lapply(others, top_names)))
  expect_identical(clash, character(0))
})
