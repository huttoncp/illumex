# `cols_negate = TRUE`: `cols` names what to leave out (Craig's item 276). The
# negation is the exact complement of the same selection within the columns a
# function can use; `by` is never negated; and a `by` outside
# ilm_outliers_all() takes names, with one clear error for anything else.

neg_data <- function() {
  set.seed(276)
  data.frame(id = 1:40, score = stats::rnorm(40), score_b = stats::rnorm(40),
             income = stats::rexp(40), grp = rep(c("a", "b"), 20),
             label = sample(letters[1:4], 40, TRUE), stringsAsFactors = FALSE)
}

test_that("each form, negated, is the eligible columns the same form would not select", {
  d <- neg_data()
  num <- names(d)[vapply(d, is.numeric, TRUE)]
  sel <- function(cols, negate = FALSE, eligible = NULL)
    ilm_resolve_cols(d, cols, eligible = eligible, negate = negate)
  ## names, a pattern, a predicate, and a predicate that fails on some columns
  forms <- list(c("id", "label"), "^score", is.numeric,
                function(v) sum(v) > 5)
  for (f in forms) for (el in list(NULL, num)) {
    pos <- tryCatch(sel(f, eligible = el), error = function(e) character())
    elig <- if (is.null(el)) names(d) else el
    want <- setdiff(elig, pos)
    if (length(want)) expect_identical(sel(f, TRUE, el), want)
    else expect_error(sel(f, TRUE, el), "left no column")
  }
  ## the forms as the help describes them
  expect_identical(sel(c("id", "label"), TRUE), c("score", "score_b", "income", "grp"))
  expect_identical(sel("^score", TRUE), c("id", "income", "grp", "label"))
  expect_identical(sel(is.numeric, TRUE), c("grp", "label"))
  ## a name wins over a pattern, negated too: "score" leaves out score only
  expect_identical(sel("score", TRUE), c("id", "score_b", "income", "grp", "label"))
})

test_that("a column on which the predicate fails counts as not matching, so cols_negate selects it", {
  d <- neg_data()
  strict <- function(v) { stopifnot(is.numeric(v)); mean(v) > 0.5 }
  pos <- ilm_resolve_cols(d, strict)
  expect_false(any(c("grp", "label") %in% pos))
  out <- ilm_resolve_cols(d, strict, negate = TRUE)
  expect_true(all(c("grp", "label") %in% out))
  expect_identical(sort(c(pos, out)), sort(names(d)))
})

test_that("the negation is taken within what the function can use", {
  d <- neg_data()
  q <- function(e) suppressMessages(e)
  ## ilm_outliers_all() checks numeric columns: leaving out a text column
  ## leaves every numeric one, and leaving out the text types does too
  all_num <- unique(q(ilm_outliers_all(d, flagged_only = FALSE))$variable)
  expect_identical(unique(q(ilm_outliers_all(d, cols = "label", cols_negate = TRUE,
                                             flagged_only = FALSE))$variable), all_num)
  expect_identical(unique(q(ilm_outliers_all(d, cols = is.character, cols_negate = TRUE,
                                             flagged_only = FALSE))$variable), all_num)
  expect_identical(unique(q(ilm_outliers_all(d, cols = c("id", "income"), cols_negate = TRUE,
                                             flagged_only = FALSE))$variable),
                   c("score", "score_b"))
  ## `by` is never negated: grouped by grp, with every numeric column but id
  r <- q(ilm_outliers_all(d, by = "grp", cols = "id", cols_negate = TRUE, flagged_only = FALSE))
  expect_identical(sort(unique(r$variable)), sort(c("score", "score_b", "income")))
  expect_true("grp" %in% names(r))
})

test_that("every function with cols takes cols_negate", {
  d <- neg_data()
  q <- function(e) suppressMessages(suppressWarnings(e))
  want <- c("score", "score_b", "income", "grp", "label")
  expect_identical(q(ilm_reduce(d, cols = "id", cols_negate = TRUE))$cols, want)
  expect_identical(q(ilm_profile(d, cols = "id", cols_negate = TRUE, k = 2, B = 5, seed = 1,
                                 var_contrib = FALSE))$reduce$cols, want)
  full <- attr(q(ilm_anomaly(d, B = 5, progress = FALSE)), "columns")
  expect_identical(attr(q(ilm_anomaly(d, cols = "^id$|label", cols_negate = TRUE, B = 5,
                                      progress = FALSE)), "columns"),
                   setdiff(full, c("id", "label")))
  g <- q(ilm_glrm(d, cols = c("id", "label"), cols_negate = TRUE, rank = 1L, lambda = 0.1,
                  progress = FALSE))
  expect_identical(g$columns, c("score", "score_b", "income", "grp"))
  rg <- q(ilm_reduce(d, cols = c("id", "label"), cols_negate = TRUE, ndim = 1, method = "glrm",
                     lambda = 0.1, progress = FALSE))
  expect_false(any(c("id", "label") %in% rg$cols))
  ## the missingness routes: every column with some missing values but one
  a <- airquality; a$Wind[c(2, 7, 11, 20, 33)] <- NA
  expect_identical(q(ilm_reduce_na(a, cols = "Ozone", cols_negate = TRUE))$cols, c("Solar.R", "Wind"))
  expect_error(q(ilm_reduce_na(a, cols = "Ozone|Solar", cols_negate = TRUE)), "at least 2 columns")
  pn <- q(ilm_profile_na(a, cols = "^Wind$", cols_negate = TRUE, k = 2, B = 5, seed = 1))
  expect_identical(pn$reduce$cols, c("Ozone", "Solar.R"))
  ## the plots draw from the complement
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(q(ilm_plot_var_all(d, cols = c("id", "grp", "label"), cols_negate = TRUE)))
  expect_no_error(q(ilm_plot_var_pairs(d, cols = "^score|label|grp", cols_negate = TRUE)))
  expect_error(q(ilm_plot_var_pairs(d, cols = "^score", cols_negate = TRUE, by = "nope")),
               "`by` takes column names; nope is not one", fixed = TRUE)
})

test_that("cols_negate's errors say what was asked", {
  d <- neg_data()
  expect_error(ilm_resolve_cols(d, NULL, negate = TRUE),
               "`cols_negate = TRUE` needs `cols` to say which columns to leave out", fixed = TRUE)
  expect_error(ilm_resolve_cols(d, names(d), negate = TRUE),
               "`cols_negate = TRUE` left no column to use: `cols` covers every eligible column (id, score, score_b, income, grp, label)",
               fixed = TRUE)
  expect_error(ilm_resolve_cols(d, function(v) TRUE, negate = TRUE), "left no column")
  ## a typo'd name is still an error, as is a pattern that matches nothing:
  ## leaving out nothing silently would hide the typo
  expect_error(ilm_resolve_cols(d, c("id", "labl"), negate = TRUE), "not found in the data: labl")
  expect_error(ilm_resolve_cols(d, "^zzz", negate = TRUE), "matched nothing as a pattern")
  ## a predicate that matches nothing is not an error when negated: it leaves
  ## every eligible column in
  expect_identical(ilm_resolve_cols(d, is.logical, negate = TRUE), names(d))
  expect_error(ilm_resolve_cols(d, is.logical), "matched no column")
  expect_error(suppressMessages(ilm_reduce(d, cols_negate = TRUE)), "needs `cols`")
})

test_that("a by outside ilm_outliers_all() takes names, and says so for anything else", {
  d <- neg_data()
  q <- function(e) suppressMessages(suppressWarnings(e))
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  calls <- list(
    describe = function(b) ilm_describe(d, "score", by = b),
    describe_all = function(b) ilm_describe_all(d, by = b),
    describe_na = function(b) ilm_describe_na(d, "score", by = b),
    describe_na_all = function(b) ilm_describe_na_all(d, by = b),
    boot_ci = function(b) ilm_boot_ci(d, "score", by = b, R = 20, seed = 1),
    counts_all = function(b) ilm_counts_all(d[c("grp", "label")], by = b),
    plot = function(b) ilm_plot(d, "score", by = b),
    plot_all = function(b) ilm_plot_all(d, by = b),
    plot_na = function(b) ilm_plot_na(d, "score", by = b),
    plot_na_all = function(b) ilm_plot_na_all(d, by = b),
    plot_var = function(b) ilm_plot_var(d, "score", by = b),
    plot_var_all = function(b) ilm_plot_var_all(d, by = b),
    plot_var_pairs = function(b) ilm_plot_var_pairs(d, cols = c("score", "income"), by = b),
    plot_histogram = function(b) ilm_plot_histogram(d, "score", by = b))
  for (nm in names(calls)) {
    expect_error(q(calls[[nm]]("^gr")), "`by` takes column names; ^gr is not one",
                 fixed = TRUE, label = nm)
    expect_error(q(calls[[nm]](is.character)), "`by` takes column names; a function is not one",
                 fixed = TRUE, label = nm)
    expect_no_error(q(calls[[nm]]("grp")), message = nm)
  }
  ## ilm_outliers_all()'s by takes every form, as cols does
  expect_identical(q(ilm_outliers_all(d, by = "^gr", flagged_only = FALSE)),
                   q(ilm_outliers_all(d, by = "grp", flagged_only = FALSE)))
})

test_that("the _all functions take cols and cols_negate, after by (item 278)", {
  d <- neg_data()
  d$note <- ifelse(seq_len(nrow(d)) %% 5 == 0, NA, "x")
  q <- function(e) suppressMessages(suppressWarnings(e))
  vars <- function(res) unique(unlist(lapply(if (is.data.frame(res)) list(res) else res,
                                                function(t) t$variable)))
  ## describe_all: cols, a pattern, a predicate, and their complements
  expect_identical(vars(q(ilm_describe_all(d, cols = c("score", "grp")))), c("score", "grp"))
  expect_setequal(vars(q(ilm_describe_all(d, cols = "^score"))), c("score", "score_b"))
  expect_setequal(vars(q(ilm_describe_all(d, cols = "^score|^id|note", cols_negate = TRUE))),
                  c("income", "grp", "label"))
  ## within class: numeric columns but id; a text column named with a numeric
  ## class leaves every numeric one
  expect_setequal(vars(q(ilm_describe_all(d, class = "numeric", cols = "id", cols_negate = TRUE))),
                  c("score", "score_b", "income"))
  expect_setequal(vars(q(ilm_describe_all(d, class = "numeric", cols = "label", cols_negate = TRUE))),
                  c("id", "score", "score_b", "income"))
  expect_error(q(ilm_describe_all(d, class = "numeric", cols = "label")), "excluded here")
  ## by is never negated, and never among the columns
  r <- q(ilm_describe_all(d, by = "grp", cols = "^score", cols_negate = TRUE, class = "numeric"))
  expect_setequal(vars(r), c("id", "income"))
  ## describe_na_all
  expect_identical(sort(q(ilm_describe_na_all(d, cols = c("note", "id")))$variable), c("id", "note"))
  expect_false("note" %in% q(ilm_describe_na_all(d, cols = "note", cols_negate = TRUE))$variable)
  expect_identical(sort(unique(q(ilm_describe_na_all(d, by = "grp", cols = is.character,
                                                     cols_negate = TRUE))$variable)),
                   sort(c("id", "score", "score_b", "income")))
  ## counts_all and counts_tb_all
  expect_identical(unique(q(ilm_counts_all(d, cols = "label"))$variable), "label")
  expect_identical(unique(q(ilm_counts_all(d, by = "grp", cols = is.numeric, cols_negate = TRUE))$variable),
                   c("label", "note"))
  expect_identical(unique(q(ilm_counts_tb_all(d, cols = "^label|note"))$variable), c("label", "note"))
  expect_identical(unique(q(ilm_counts_tb_all(d, cols = is.numeric, cols_negate = TRUE))$variable),
                   c("grp", "label", "note"))
  ## the plots
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(q(ilm_plot_all(d, cols = "^score")))
  expect_no_error(q(ilm_plot_all(d, class = "numeric", cols = "id", cols_negate = TRUE)))
  expect_error(q(ilm_plot_all(d, class = "numeric", cols = names(d)[vapply(d, is.numeric, TRUE)],
                              cols_negate = TRUE)), "left no column")
  expect_no_error(q(ilm_plot_na_all(d, cols = c("note", "label"))))
  expect_no_error(q(ilm_plot_na_all(d, by = "grp", cols = "note", cols_negate = TRUE)))
  ## the errors are the resolver's
  for (f in list(function(...) ilm_describe_all(d, ...), function(...) ilm_describe_na_all(d, ...),
                 function(...) ilm_counts_all(d, ...), function(...) ilm_counts_tb_all(d, ...),
                 function(...) ilm_plot_all(d, ...), function(...) ilm_plot_na_all(d, ...))) {
    expect_error(q(f(cols_negate = TRUE)), "needs `cols`")
    expect_error(q(f(cols = "labl")), "matched nothing as a pattern")
  }
})
