## The remedy system (R/ilm_remedies.R), the data checks and the cleaning
## log and script (R/ilm_clean.R), and the fingerprint (R/ilm_data_id.R).

frame_data <- function() {
  set.seed(3)
  n <- 40L
  d <- data.frame(id = seq_len(n), a = stats::rnorm(n), b = stats::rnorm(n), same = 7,
                  site = rep(c("north", "south"), each = n / 2))
  d$a_copy <- d$a
  d$total <- d$a + d$b
  d$site_code <- ifelse(d$site == "north", "N", "S")
  d
}

## ---- the table ------------------------------------------------------------------

rows_of <- function(...) list(...)
row <- function(check, tier, key, change = "f(data)", status = "WARN", remedy = "do it")
  list(check = check, status = status, tier = tier, remedy = remedy, key = key,
       change = change, payload = list(key = key))

test_that("assembly merges a key named twice, takes the more cautious tier, orders by tier", {
  tiers <- c("representation", "values", "rows")
  t <- ilm_remedy_assemble(rows_of(
    row("c1", "rows", "drop_rows/x"),
    row("c2", "representation", "parse/y"),
    row("c3", "values", "drop_rows/x")), "T", tiers, "note")
  expect_s3_class(t, "ilm_remedies")
  expect_equal(t$key, c("parse/y", "drop_rows/x"))
  expect_equal(t$check[2], "c1, c3")
  expect_equal(t$tier[2], "rows")
  expect_equal(t$id, 1:2)
  ## the payload follows its row by id
  expect_equal(attr(t, "payload")[["2"]]$key, "drop_rows/x")
})

test_that("one key naming two different changes is an internal error", {
  expect_error(ilm_remedy_assemble(rows_of(
    row("c1", "values", "k/x", change = "f(data)"),
    row("c2", "values", "k/x", change = "g(data)")), "T", c("values"), "n"),
    "names two different changes")
})

test_that("a tier outside the table's set, or an OK status, is refused", {
  expect_error(ilm_remedy_assemble(rows_of(row("c", "structural", "k/x")), "T",
                                   c("representation", "values", "rows"), "n"), "not one of")
  expect_error(ilm_remedy_assemble(rows_of(row("c", "values", "k/x", status = "OK")), "T",
                                   c("values"), "n"), "status OK")
})

test_that("keys: built one way, found by id or key, spaces ignored", {
  expect_equal(ilm_remedy_key("drop_cols", c("b", "a")), "drop_cols/a,b")
  expect_equal(ilm_remedy_key("convert", "temp", "degF>degC"), "convert/temp:degF>degC")
  expect_equal(ilm_remedy_key("drop", ~ (1 | g)), "drop/(1 | g)")
  t <- ilm_remedy_assemble(rows_of(row("c", "values", "drop/(1 | g)"),
                                   row("c", "values", "drop_cols/a,b")),
                           "T", c("values"), "n")
  expect_equal(ilm_remedy_find(t, 2), 2L)
  expect_equal(ilm_remedy_find(t, "drop/(1|g)"), 1L)
  expect_equal(ilm_remedy_find(t, " drop_cols/a, b "), 2L)
  expect_error(ilm_remedy_find(t, "nope/x"), "drop/\\(1 \\| g\\)")
})

test_that("c() combines tables for one target and refuses two targets", {
  tiers <- c("representation", "values", "rows")
  t1 <- ilm_remedy_assemble(rows_of(row("c1", "values", "k/a")), "T", tiers, "n")
  t2 <- ilm_remedy_assemble(rows_of(row("c2", "rows", "k/a"), row("c2", "values", "k/b")),
                            "T", tiers, "n")
  both <- c(t1, t2)
  expect_equal(nrow(both), 2L)
  expect_equal(both$check[both$key == "k/a"], "c1, c2")
  expect_equal(both$tier[both$key == "k/a"], "rows")
  t3 <- ilm_remedy_assemble(rows_of(row("c3", "values", "k/c")), "OTHER", tiers, "n")
  expect_error(c(t1, t3), "different targets")
  expect_error(c(t1, t2[, c("id", "check", "tier")]), "lost what ties it")
})

test_that("print gives each remedy, its key and the table's own closing note", {
  d <- frame_data()
  rem <- ilm_remedies(ilm_check_frame(d))
  out <- paste(capture.output(print(rem)), collapse = "\n")
  expect_match(out, "key: drop_cols/same", fixed = TRUE)
  expect_match(out, "change: ilm_drop_cols(data, \"same\")", fixed = TRUE)
  expect_match(out, "by hand: a decision about what the data mean", fixed = TRUE)
  expect_match(out, "Representation reads the same values correctly", fixed = TRUE)
  expect_output(print(rem[0, ]), "No remedies")
})

## ---- the fingerprint -------------------------------------------------------------

test_that("the fingerprint is fixed, and moves with order and offsetting edits", {
  d <- data.frame(a = c(1.5, -2, NA, 4), b = c("x", "y", NA, "é"),
                  f = factor(c("u", "v", "u", NA)), l = c(TRUE, FALSE, NA, TRUE),
                  i = c(1L, NA, 3L, 4L), stringsAsFactors = FALSE)
  id <- ilm_data_id(d)
  ## the same in every session and on every platform: exact integer arithmetic
  expect_equal(id, ilm_data_id(d))
  expect_match(id, "^4x5:[0-9a-f]{10}$")
  expect_false(ilm_data_id(d[c(2, 1, 3, 4), ]) == id)            # rows reordered
  expect_false(ilm_data_id(d[c(2, 1, 3, 4, 5)]) == id)           # columns reordered
  e <- d; e$a[1] <- e$a[1] + 1; e$a[2] <- e$a[2] - 1
  expect_false(ilm_data_id(e) == id)                             # offsetting edits
  e <- d; e$i[1] <- 2L; e$i[3] <- 2L
  expect_false(ilm_data_id(e) == id)                             # offsetting, integers
  e <- d; e$b[c(1, 2)] <- e$b[c(2, 1)]
  expect_false(ilm_data_id(e) == id)                             # two values swapped
  e <- d; names(e)[1] <- "z"
  expect_false(ilm_data_id(e) == id)                             # a name
  e <- d; e$i <- as.numeric(e$i)
  expect_false(ilm_data_id(e) == id)                             # a class
  r <- d; rownames(r) <- letters[1:4]
  expect_equal(ilm_data_id(r), id)                               # row names do not enter
  e <- d; levels(e$f) <- c("u", "w")
  expect_false(ilm_data_id(e) == id)                             # a level relabelled
})

test_that("the fingerprint's value is pinned, so it stays the same across versions", {
  d <- data.frame(a = c(1, 2.5), b = c("p", "q"), stringsAsFactors = FALSE)
  expect_equal(ilm_data_id(d), ilm_data_id(data.frame(a = c(1, 2.5), b = c("p", "q"))))
  expect_equal(ilm_data_id(d), "2x2:6fdce43a9b")
})

## ---- checks and remedies ------------------------------------------------------------

test_that("the frame check finds what ilm_frame_issues() finds, with a remedy for each", {
  d <- frame_data()
  chk <- ilm_check_frame(d)
  expect_s3_class(chk, "ilm_data_check")
  fi <- ilm_frame_issues(d)
  expect_setequal(chk$findings$finding, fi$issue)
  ## no complaint without a remedy: every finding that is not OK names one
  bad <- chk$findings$finding[chk$findings$status != "OK"]
  named <- vapply(chk$candidates, `[[`, "", "finding")
  expect_true(all(bad %in% named))
  expect_equal(chk$status, "FAIL")
})

test_that("every kind of frame finding has a remedy (no complaint without one)", {
  kinds <- c("constant", "id_like", "duplicate_columns", "collinear", "aliased_factors",
             "redundant_categories", "rank_unchecked", "rank_deficient")
  src <- deparse(body(ilm_check_frame))
  for (k in kinds) expect_true(any(grepl(paste0(k, " = "), src, fixed = TRUE)), info = k)
})

test_that("the frame check's keys are pinned (a changed key is a NEWS item)", {
  d <- frame_data()
  rem <- ilm_remedies(ilm_check_frame(d))
  expect_equal(sort(rem$key),
               sort(c("drop_cols/same", "as_key/id", "drop_cols/a_copy",
                      "drop_cols/site_code", "drop_cols/total")))
})

test_that("ilm_check_data() runs the checks and lists their remedies together", {
  d <- frame_data()
  chk <- ilm_check_data(d)
  expect_s3_class(chk, "ilm_data_checks")
  expect_equal(chk$summary$check, "frame")
  expect_equal(nrow(ilm_remedies(chk)), nrow(ilm_remedies(ilm_check_frame(d))))
  expect_error(ilm_check_data(d, checks = "nope"), "unknown check")
  expect_output(print(chk), "FAIL frame")
})

## ---- applying, the log, the script --------------------------------------------------

test_that("a remedy is applied by key, logged with its reason, and re-checked", {
  d <- frame_data()
  rem <- ilm_remedies(ilm_check_frame(d))
  expect_message(d2 <- ilm_apply_remedy(d, rem, "drop_cols/same", reason = "one value only"),
                 "constant: FAIL before, not found now")
  expect_false("same" %in% names(d2))
  lg <- ilm_cleaning_log(d2)
  expect_equal(nrow(lg), 1L)
  expect_equal(lg$key, "drop_cols/same")
  expect_equal(lg$reason, "one value only")
  expect_equal(lg$id_before, ilm_data_id(d))
  expect_equal(lg$id_after, ilm_data_id(d2))
})

test_that("a row subset still makes the right remedy: the payload is kept by id", {
  d <- frame_data()
  rem <- ilm_remedies(ilm_check_frame(d))
  one <- rem[rem$key == "drop_cols/total", ]
  d2 <- suppressMessages(ilm_apply_remedy(d, one, one$id))
  expect_equal(setdiff(names(d), names(d2)), "total")
})

test_that("a table for other data, a column subset, a by-hand remedy, and a model's table are refused", {
  d <- frame_data()
  rem <- ilm_remedies(ilm_check_frame(d))
  expect_error(ilm_apply_remedy(d[2:1, ], rem, 1), "listed for other data")
  expect_error(ilm_apply_remedy(d[c(2:1, 3:40), ], rem, 1), "another order")
  expect_error(ilm_apply_remedy(d, rem[, c("id", "key", "check")], 1), "lost what ties it")
  expect_error(ilm_apply_remedy(d, rem, "as_key/id"), "made by hand")
  mod <- ilm_remedy_assemble(list(row("c", "numerical", "restarts")), ilm_data_id(d),
                             c("numerical", "structural", "estimand"), "n")
  expect_error(ilm_apply_remedy(d, mod, 1), "fitted model")
  expect_error(ilm_apply_remedy(d, rem, 1, reason = ""), "single sentence")
})

test_that("a change outside a remedy is warned about at the next remedy", {
  d <- frame_data()
  d2 <- suppressMessages(ilm_apply_remedy(d, ilm_remedies(ilm_check_frame(d)), "drop_cols/same"))
  d3 <- d2; d3$b[1] <- 0                           # an edit by hand, keeping the attribute
  expect_warning(suppressMessages(
    ilm_apply_remedy(d3, ilm_remedies(ilm_check_frame(d3)), "drop_cols/a_copy")),
    "changed since the last remedy")
})

test_that("the script replays the cleaning on the raw data, identically", {
  d <- frame_data()
  c1 <- suppressMessages(ilm_apply_remedy(d, ilm_remedies(ilm_check_frame(d)),
                                          "drop_cols/same", reason = "one value only"))
  c2 <- suppressMessages(ilm_apply_remedy(c1, ilm_remedies(ilm_check_frame(c1)),
                                          "drop_cols/a_copy", reason = "a copy of a"))
  scr <- ilm_cleaning_script(c2)
  expect_s3_class(scr, "ilm_cleaning_script")
  expect_true(any(grepl("^## reason: a copy of a$", scr)))
  expect_true(any(grepl("^data <- ilm_drop_cols\\(data, \"a_copy\"\\)$", scr)))
  env <- new.env()
  env$data <- d
  ## library(illumex) is the script's first call; under test the package is loaded
  code <- setdiff(scr, "library(illumex)")
  eval(parse(text = code), envir = env)
  clean <- c2; attr(clean, "cleaning_log") <- NULL
  expect_identical(env$data, clean)
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f))
  ilm_cleaning_script(c2, f)
  expect_equal(readLines(f), unclass(scr))
})

test_that("a secret never reaches the table, the log or the script", {
  secret <- "s3cr3t-Value-9271"
  withr::local_envvar(ILM_TEST_SECRET = secret)
  d <- data.frame(x = c("a", "b", "c"), stringsAsFactors = FALSE)
  tag <- function(data, key) { data$code <- paste0(data$x, nchar(key)); data }
  rem <- ilm_remedy_table(d, check = "ids", status = "WARN", tier = "values",
                          key = "pseudonymise/x", remedy = "Replace x by a code.",
                          change = list(quote(tag(data, ilm_secret("ILM_TEST_SECRET")))))
  d2 <- suppressMessages(ilm_apply_remedy(d, rem, "pseudonymise/x", reason = "privacy"))
  expect_equal(d2$code, paste0(d$x, nchar(secret)))
  everything <- c(unlist(rem), unlist(ilm_cleaning_log(d2)), ilm_cleaning_script(d2),
                  capture.output(print(rem)))
  expect_false(any(grepl(secret, everything, fixed = TRUE)))
  scr <- ilm_cleaning_script(d2)
  expect_true(any(grepl("set the environment variable(s) ILM_TEST_SECRET", scr, fixed = TRUE)))
  expect_true(any(grepl("ilm_secret(\"ILM_TEST_SECRET\")", scr, fixed = TRUE)))
  withr::local_envvar(ILM_TEST_SECRET = NA)
  expect_error(ilm_apply_remedy(d, rem, 1), "ILM_TEST_SECRET, which is not set")
})

test_that("each generic says plainly when it has no method for an object", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_error(ilm_remedy_table(fit), "object of class lm")
  expect_error(ilm_remedies(fit), "object of class lm")
  expect_error(ilm_apply_remedy(fit, NULL, 1), "object of class lm")
})

test_that("a table of one's own is checked, and a call is required to be a call", {
  d <- data.frame(x = 1:3)
  expect_error(ilm_remedy_table(d, "c", "OK", "values", "r", list(quote(f(data))), "k/x"),
               "WARN or FAIL")
  expect_error(ilm_remedy_table(d, "c", "WARN", "values", "r", list("f(data)"), "k/x"),
               "must be a call")
  expect_error(ilm_remedy_table(d, "a,b", "WARN", "values", "r", NULL, "k/x"), "comma")
})
