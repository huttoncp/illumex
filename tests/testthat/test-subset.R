# Choosing rows (items 277 and 279): `subset` in its four forms, first in
# every function that takes it; `subset_negate`; the `_fixed` options;
# ilm_sample() by rows and by groups; the original row numbers kept; the
# lines a result prints and the account of the choice it keeps;
# ilm_subset(); and ilm_recode_errors()'s subset and keep_all.

sub_data <- function() {
  set.seed(277)
  data.frame(id = rep(1:20, each = 3), site = rep(c("north", "south", "east"), 20),
             score = stats::rnorm(60, 50, 10), income = stats::rexp(60, 1 / 40000),
             visits = stats::rpois(60, 3), flag = c(NA, rep(c(TRUE, FALSE), length.out = 59)),
             wt.kg = stats::rnorm(60, 70, 8), wt_2 = stats::rnorm(60),
             stringsAsFactors = FALSE)
}
q <- function(e) suppressMessages(suppressWarnings(e))

test_that("each form keeps the rows it says, in the data's order", {
  d <- sub_data()
  rows <- function(...) ilm_resolve_rows(d, ...)$rows
  ## a logical: NA is out, negated or not
  expect_identical(rows(d$flag), which(d$flag %in% TRUE))
  expect_identical(rows(d$flag, negate = TRUE), which(d$flag %in% FALSE))
  ## positions: kept in the data's order, and negated the others
  expect_identical(rows(c(9, 2, 5)), c(2L, 5L, 9L))
  expect_identical(rows(c(9, 2, 5), negate = TRUE), setdiff(1:60, c(2, 5, 9)))
  ## named patterns: every name must match; NA matches nothing, so negating
  ## takes it in
  d$site[4] <- NA
  expect_identical(rows(c(site = "^n")), setdiff(which(grepl("^n", d$site)), 4L))
  expect_identical(rows(c(site = "^n", id = "^1$")), 1L)
  expect_true(4L %in% rows(c(site = "^n"), negate = TRUE))
  ## fixed: the pattern as written
  d$site[5] <- "so.th"
  expect_identical(rows(c(site = "so.th"), fixed = TRUE), 5L)
  expect_true(length(rows(c(site = "so.th"))) > 1L)
})

test_that("subset's errors say what went wrong", {
  d <- sub_data()
  r <- function(...) ilm_resolve_rows(d, ...)
  expect_error(r(-3), "to leave rows out, give them and set subset_negate = TRUE")
  expect_error(r(c(1, 99)), "between 1 and 60")
  expect_error(r(c(1, 1, 2)), "must not repeat")
  expect_error(r(1.5), "whole numbers")
  expect_error(r(c(TRUE, FALSE)), "one value per row")
  expect_error(r("^n"), "subset = c(species = \"^set\")", fixed = TRUE)
  expect_error(r(c(nope = "x")), "not in the data: nope")
  expect_error(r(list(1)), "must be a logical vector")
  expect_error(r(NULL, negate = TRUE), "needs `subset`")
  expect_error(r(c(site = "^zzz")), "kept no rows: site matching \"^zzz\" (60 rows given)", fixed = TRUE)
  expect_error(r(rep(FALSE, 60)), "kept no rows: a condition true on no row")
  expect_error(r(1:60, negate = TRUE), "negated, which leaves every row out")
})

test_that("ilm_sample() draws rows or whole groups, and a seed leaves the stream alone", {
  d <- sub_data()
  expect_error(ilm_sample(), "one of the two")
  expect_error(ilm_sample(n = 2, prop = 0.5), "one of the two")
  expect_error(ilm_sample(0.3), "use prop = 0.3")
  expect_error(ilm_sample(prop = 1), "strictly between 0 and 1")
  expect_error(ilm_resolve_rows(d, ilm_sample(61))$rows, "asks for 61 rows and there are 60")
  expect_error(ilm_resolve_rows(d, ilm_sample(prop = 0.001))$rows, "comes to no rows")
  set.seed(9); before <- .Random.seed
  a <- ilm_resolve_rows(d, ilm_sample(10, seed = 1))
  expect_identical(.Random.seed, before)
  expect_identical(ilm_resolve_rows(d, ilm_sample(10, seed = 1))$rows, a$rows)
  expect_length(a$rows, 10L)
  expect_identical(a$info$sample$seed$value, 1L)
  ## by groups: n counts groups, every row of a group drawn is kept, and the
  ## holdout is the other groups whole
  g <- ilm_resolve_rows(d, ilm_sample(4, by = "id", seed = 2))$rows
  expect_length(unique(d$id[g]), 4L)
  expect_identical(length(g), 12L)
  h <- ilm_resolve_rows(d, ilm_sample(4, by = "id", seed = 2), negate = TRUE)$rows
  expect_length(intersect(d$id[g], d$id[h]), 0L)
  expect_identical(sort(c(g, h)), 1:60)
  expect_identical(ilm_resolve_rows(d, ilm_sample(prop = 0.5, by = "site", seed = 3))$info$sample$prop,
                   0.5)
  ## a sample makes no sense where cells are recoded
  expect_error(ilm_recode_errors(d, errors = 999, subset = ilm_sample(5)), "cannot be a random sample")
})

test_that("results keep the data's own row numbers", {
  d <- sub_data()
  keep <- which(d$site != "north")
  ## outliers: the row a value is reported on is the data's row with that value
  o <- q(ilm_outliers_all(d, cols = "score", subset = d$site != "north", flagged_only = FALSE))
  expect_true(all(o$row_id %in% keep))
  expect_equal(o$value, d$score[o$row_id])
  ## anomaly: rows index the data as given, which ilm_anomalous() returns
  a <- q(ilm_anomaly(d[c("score", "income", "visits", "wt.kg", "wt_2")], subset = keep, B = 9,
                     progress = FALSE))
  expect_true(all(a$row %in% keep))
  an <- q(ilm_anomalous(a, flagged = FALSE))
  expect_true(all(an$.row %in% keep))
  expect_equal(an$score, d$score[an$.row])
  ## reduce and profile: coordinates and clusters by the data's numbers
  r <- q(ilm_reduce(d[c("score", "income", "visits", "site")], subset = keep))
  expect_identical(r$ind_coord$row_id, keep)
  p <- q(ilm_profile(d[c("score", "income", "visits", "site")], subset = keep, k = 2, B = 5,
                     seed = 1, var_contrib = FALSE))
  expect_identical(p$cluster$ind_cluster$row_id, keep)
  g <- q(ilm_reduce(d[c("score", "income", "visits")], subset = keep, method = "glrm",
                    ndim = 2, lambda = 0.1, progress = FALSE))
  expect_identical(g$ind_coord$row_id, keep)
  ## subset on an anomaly result is refused
  expect_error(q(ilm_reduce(a, subset = 1:5)), "subset the data before ilm_anomaly()")
  expect_error(q(ilm_describe_all(a, subset = 1:5)), "subset the data before ilm_anomaly()")
})

test_that("a result says which rows and columns it used, and keeps what was chosen", {
  d <- sub_data()
  r <- q(ilm_describe(d, "score", subset = c(site = "^n")))
  expect_identical(capture.output(print(r))[1], "20 of 60 rows (subset)")
  rn <- q(ilm_describe(d, "score", subset = c(site = "^n"), subset_negate = TRUE))
  expect_identical(capture.output(print(rn))[1], "40 of 60 rows (subset, negated)")
  ## columns: excluded listed, by kept out of them and out of the count, a
  ## long list cut at 8
  a <- q(ilm_describe_all(d, by = "site", cols = c("score", "income")))
  expect_identical(capture.output(print(a))[1], "Columns: 2 of 7 (excluded: id, visits, flag, wt.kg, wt_2)")
  wide <- as.data.frame(stats::setNames(replicate(14, stats::rnorm(10), simplify = FALSE),
                                        paste0("v", 1:14)))
  w <- q(ilm_describe_all(wide, cols = "1", cols_fixed = TRUE, subset = 1:5))
  expect_identical(capture.output(print(w))[1:2],
                   c("5 of 10 rows (subset)",
                     "Columns: 6 of 14 (excluded: v2, v3, v4, v5, v6, v7, v8, v9)"))
  expect_identical(capture.output(print(q(ilm_describe_all(wide, cols = "^v1$"))))[1],
                   "Columns: 1 of 14 (excluded: v2, v3, v4, v5, v6, v7, v8, v9, and 5 more)")
  ## nothing is added when nothing was chosen
  plain <- q(ilm_describe(d, "score"))
  expect_false(inherits(plain, "ilm_selected"))
  expect_null(attr(plain, "ilm_select"))
  ## what the result keeps
  s <- attr(r, "ilm_select")
  expect_identical(s$subset[c("form", "negate", "n_rows_given", "n_rows_kept")],
                   list(form = "patterns", negate = FALSE, n_rows_given = 60L, n_rows_kept = 20L))
  expect_identical(s$subset$patterns, list(site = "^n"))
  expect_null(s$selection)
  sel <- attr(q(ilm_describe_all(d, cols = is.numeric, cols_negate = TRUE, by = "site")),
              "ilm_select")$selection
  expect_identical(sel$form, "predicate")
  ## flag, the one column neither numeric nor the grouping
  expect_identical(c(sel$n_cols_given, sel$n_cols_kept), c(7L, 1L))
  expect_true(sel$negate)
  expect_false("site" %in% unlist(sel$columns_excluded))
  ## a plot says it in a message
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_message(ilm_plot_histogram(d, "score", subset = 1:30), "30 of 60 rows \\(subset\\)")
  expect_message(ilm_plot_all(d, cols = "^wt", subset = 1:30), "Columns: 2 of 8")
})

test_that("every function that takes subset uses it first", {
  d <- sub_data()
  s <- d$site == "south"
  n_kept <- function(x) attr(x, "ilm_select")$subset$n_rows_kept
  expect_identical(n_kept(q(ilm_describe_na(d, "flag", subset = s))), 20L)
  expect_identical(n_kept(q(ilm_describe_na_all(d, subset = s))), 20L)
  expect_identical(n_kept(q(ilm_counts_all(d, cols = "site", subset = s))), 20L)
  expect_identical(unique(q(ilm_counts_all(d, cols = "site", subset = s))$value), "south")
  expect_identical(n_kept(q(ilm_counts_tb_all(d, cols = "site", subset = s))), 20L)
  expect_identical(n_kept(q(ilm_frame_issues(d, subset = s))), 20L)
  expect_identical(n_kept(q(ilm_boot_ci(d, "score", subset = s, R = 20, seed = 1))), 20L)
  expect_identical(n_kept(q(ilm_boot_diff(d, "score", "site", subset = d$site != "east",
                                          R = 20, seed = 1))), 40L)
  expect_identical(n_kept(q(ilm_boot_diff(score ~ site, data = d, subset = c(site = "o"),
                                          R = 20, seed = 1))), 40L)
  expect_identical(n_kept(q(ilm_glrm(d[c("score", "income", "visits")], subset = s, rank = 1L,
                                     lambda = 0.1, progress = FALSE))), 20L)
  expect_identical(n_kept(q(ilm_reduce_na(airquality, subset = airquality$Month > 6))), 92L)
  expect_identical(n_kept(q(ilm_profile_na(airquality, subset = airquality$Month > 6, k = 2,
                                           B = 5, seed = 1))), 92L)
  ## the grouped describe splits only the rows kept
  bd <- q(ilm_describe(d, "score", by = "site", subset = s))
  expect_identical(nrow(bd), 1L)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  for (f in list(function(...) ilm_plot(d, "score", ...), function(...) ilm_plot_bar(d, "site", ...),
                 function(...) ilm_plot_box(d, "score", ...), function(...) ilm_plot_density(d, "score", ...),
                 function(...) ilm_plot_line(d, "score", "id", ...),
                 function(...) ilm_plot_scatter(d, "score", "income", ...),
                 function(...) ilm_plot_stat_error(d, "score", "site", ...),
                 function(...) ilm_plot_violin(d, "score", ...), function(...) ilm_plot_var(d, "score", ...),
                 function(...) ilm_plot_na(airquality, "Ozone", by = "Month", ...),
                 function(...) ilm_plot_missing(airquality, ...),
                 function(...) ilm_plot_na_all(airquality, ...),
                 function(...) ilm_plot_var_all(d, cols = c("score", "site"), ...),
                 function(...) ilm_plot_var_pairs(d, cols = c("score", "income"), ...)))
    expect_message(f(subset = 1:30), "30 of")
})

test_that("cols_fixed matches literally, and a name still wins", {
  d <- sub_data()
  expect_identical(ilm_resolve_cols(d, "wt.", fixed = TRUE), "wt.kg")
  expect_identical(ilm_resolve_cols(d, "wt."), c("wt.kg", "wt_2"))
  expect_identical(ilm_resolve_cols(d, "wt.kg", fixed = TRUE), "wt.kg")
  expect_identical(ilm_resolve_cols(d, "wt.", fixed = TRUE, negate = TRUE),
                   setdiff(names(d), "wt.kg"))
  expect_identical(ilm_resolve_cols(d, is.numeric, fixed = TRUE),
                   ilm_resolve_cols(d, is.numeric))
})

test_that("ilm_subset() returns the rows, then the columns, with their numbers", {
  d <- sub_data()
  s <- ilm_subset(d, subset = c(site = "^e"), cols = "^wt", cols_negate = TRUE)
  expect_identical(names(s), setdiff(names(d), c("wt.kg", "wt_2")))
  expect_identical(rownames(s), as.character(which(d$site == "east")))
  sel <- attr(s, "ilm_select")
  expect_identical(sel$subset$n_rows_kept, 20L)
  expect_identical(sel$selection$form, "pattern")
  expect_identical(unlist(sel$selection$columns_excluded), c("wt.kg", "wt_2"))
  ## original row names survive
  rownames(d) <- paste0("r", 1:60)
  expect_identical(rownames(ilm_subset(d, subset = 2:3)), c("r2", "r3"))
  ## nothing chosen, nothing changed
  expect_identical(ilm_subset(d), d)
})

test_that("ilm_recode_errors() recodes the chosen cells, and keep_all decides what returns", {
  d <- data.frame(x = c(1, 999, 3, 999), y = c(999, 2, 999, 4), site = c("a", "b", "a", "b"),
                  stringsAsFactors = FALSE)
  all_rows <- ilm_recode_errors(d, errors = 999, subset = c(site = "^a$"))
  expect_identical(dim(all_rows), dim(d))
  expect_true(is.na(all_rows$y[1]) && is.na(all_rows$y[3]))
  expect_identical(all_rows$x[2], 999)             # site b untouched
  only <- ilm_recode_errors(d, errors = 999, subset = c(site = "^a$"), cols = "y", keep_all = FALSE)
  expect_identical(dim(only), c(2L, 1L))
  expect_true(all(is.na(only$y)))
  neg <- ilm_recode_errors(d, errors = 999, subset = c(site = "^a$"), subset_negate = TRUE)
  expect_true(is.na(neg$x[2]) && is.na(neg$x[4]))
  expect_identical(neg$y[1], 999)
  ## cols by position, pattern and predicate
  expect_true(is.na(ilm_recode_errors(d, errors = 999, cols = 1)$x[2]))
  expect_true(is.na(ilm_recode_errors(d, errors = 999, cols = "^y")$y[1]))
  expect_identical(ilm_recode_errors(d, errors = 999, cols = is.numeric, cols_negate = TRUE), d)
  ## a matrix too
  m <- matrix(c(1, 999, 3, 999), 2)
  expect_identical(ilm_recode_errors(m, errors = 999, subset = 2), matrix(c(1, NA, 3, NA), 2))
})

test_that("ilm_cluster(): cols before a reduction, never on one; subset everywhere (item 280)", {
  d <- sub_data()
  keep <- which(d$site != "east")
  ## a mixed data frame is reduced here: cols chooses what goes in
  c1 <- q(ilm_cluster(d[c("score", "income", "visits", "site", "id")], cols = "id",
                      cols_negate = TRUE, subset = keep, k = 2, B = 5, seed = 1))
  expect_identical(c1$ind_cluster$row_id, keep)
  expect_identical(unlist(attr(c1, "ilm_select")$selection$columns_excluded), "id")
  ## coordinates: cols chooses them
  co <- data.frame(a = stats::rnorm(60), b = stats::rnorm(60), c = stats::rnorm(60))
  c2 <- q(ilm_cluster(co, cols = c("a", "b"), subset = 1:40, k = 2, B = 5, seed = 1))
  expect_identical(colnames(c2$coords), c("a", "b"))
  expect_identical(c2$ind_cluster$row_id, 1:40)
  c3 <- q(ilm_cluster(as.matrix(co), cols = "c", cols_negate = TRUE, k = 2, B = 5, seed = 1))
  expect_identical(colnames(c3$coords), c("a", "b"))
  ## a reduction: rows by its own numbers, and no cols
  r <- q(ilm_reduce(d[c("score", "income", "visits", "site")], subset = keep))
  c4 <- q(ilm_cluster(r, subset = 1:10, k = 2, B = 5, seed = 1))
  expect_identical(c4$ind_cluster$row_id, keep[1:10])
  expect_error(q(ilm_cluster(r, cols = "dim1")), "choose its columns with ilm_reduce(cols = )",
               fixed = TRUE)
  ## the missingness route takes subset only, and keeps its mark first
  rn <- q(ilm_reduce_na(airquality))
  cn <- q(ilm_cluster_na(rn, subset = 1:100, k = 2, B = 5, seed = 1))
  expect_identical(class(cn)[1:2], c("ilm_selected", "ilm_cluster_na"))
  expect_identical(attr(cn, "ilm_select")$subset$n_rows_kept, 100L)
})

test_that("ilm_copies() and ilm_dupes() take their key as cols, in every form (item 280)", {
  d <- data.frame(a = c(1, 1, 2, 2, 3), b = c("x", "x", "y", "z", "z"), n = 1:5,
                  stringsAsFactors = FALSE)
  expect_identical(nrow(q(ilm_dupes(d, cols = c("a", "b")))), 2L)
  expect_identical(nrow(q(ilm_dupes(d, "a"))), 4L)
  expect_identical(nrow(q(ilm_dupes(d, cols = "n", cols_negate = TRUE))), 2L)
  expect_identical(nrow(q(ilm_dupes(d, cols = is.character))), 4L)
  expect_identical(nrow(q(ilm_copies(d, "a", filter = "first", subset = 3:5))), 2L)
  ## the rows returned keep the data's own numbers, subset or not
  expect_identical(rownames(q(ilm_dupes(d, "a", subset = 2:5))), c("3", "4"))
  expect_identical(rownames(q(ilm_copies(d, "a", filter = "last"))), c("2", "4", "5"))
  expect_identical(attr(q(ilm_copies(d, "b", subset = 1:4)), "ilm_select")$subset$n_rows_kept, 4L)
  expect_message(ilm_dupes(d, cols = "^a$"), "ilm_copies(d, \"a\", filter = \"first\")", fixed = TRUE)
})

test_that("ilm_check_missing()'s covariates take every form (item 280)", {
  set.seed(4)
  d <- data.frame(x = stats::rnorm(200), z = stats::rnorm(200), w = stats::rnorm(200))
  d$y <- 0.4 * d$x + stats::rnorm(200); d$y[d$x > 1] <- NA
  a <- q(ilm_check_missing(d, y = "y", covariates = c("x", "z"), verbose = FALSE))
  b <- q(ilm_check_missing(d, y = "y", covariates = "w", covariates_negate = TRUE, verbose = FALSE))
  expect_equal(unclass(a)[names(unclass(a))], unclass(b)[names(unclass(b))], ignore_attr = TRUE)
  expect_identical(attr(b, "ilm_select")$selection$negate, TRUE)
  expect_error(ilm_check_missing(d, y = "y", covariates_negate = TRUE, verbose = FALSE),
               "`covariates_negate = TRUE` needs `covariates`")
  s <- q(ilm_check_missing(d, y = "y", subset = 1:100, verbose = FALSE))
  expect_identical(s$n, 100L)
})

test_that("ilm_wash_df() takes neither subset nor cols (item 280)", {
  expect_false(any(c("subset", "cols") %in% names(formals(ilm_wash_df))))
})

## ---- sampling inside clusters (item 283) ---------------------------------

within_data <- function() {
  set.seed(283)
  d <- data.frame(school = rep(c("s1", "s2", "s3"), c(60, 30, 4)), y = stats::rnorm(94),
                  stringsAsFactors = FALSE)
  d$room <- stats::ave(seq_len(94), d$school, FUN = function(i) (seq_along(i) - 1L) %/% 10L + 1L)
  d$classroom <- paste(d$school, d$room, sep = "_")
  d
}

test_that("within = keeps every cluster, in proportion, with the floor", {
  d <- within_data()
  ## prop: a share of each school's rows, rounded within each, at least min
  r <- ilm_resolve_rows(d, ilm_sample(prop = 0.2, within = "school", seed = 1))
  kept <- table(factor(d$school[r$rows], levels = c("s1", "s2", "s3")))
  expect_identical(as.integer(kept), c(12L, 6L, 2L))
  s <- r$info$sample
  expect_identical(s$min, 2L)
  expect_identical(unlist(s$within), "school")
  expect_identical(c(s$fraction_min, s$fraction_max), c(0.2, 0.5))
  expect_equal(s$fraction_median, 0.2)
  expect_identical(s$n_clusters, 3L)
  ## each kept row's chance is its cluster's share
  expect_equal(r$inclusion, c(s1 = 0.2, s2 = 0.2, s3 = 0.5)[d$school[r$rows]], ignore_attr = TRUE)
  ## n: shared out in proportion (60, 30, 4 of 94), the floor raising the smallest
  r2 <- ilm_resolve_rows(d, ilm_sample(n = 20, within = "school", seed = 1))
  expect_identical(as.integer(table(d$school[r2$rows])), c(13L, 6L, 2L))
  ## a cluster smaller than min is kept whole
  r3 <- ilm_resolve_rows(d, ilm_sample(prop = 0.1, within = "school", min = 5, seed = 1))
  expect_identical(sum(d$school[r3$rows] == "s3"), 4L)
  expect_identical(r3$info$sample$n_clusters_whole, 1L)
  expect_identical(sum(d$school[r3$rows] == "s2"), 5L)
  expect_error(ilm_sample(prop = 0.1, within = "school", min = 0), "whole number of at least 1")
  expect_error(ilm_resolve_rows(d, ilm_sample(n = 200, within = "school")),
               "asks for 200 rows and there are 94")
})

test_that("nested levels draw inside the finest, so every level above is kept", {
  d <- within_data()
  r <- ilm_resolve_rows(d, ilm_sample(prop = 0.2, within = c("classroom", "school"), seed = 2))
  expect_setequal(unique(d$classroom[r$rows]), unique(d$classroom))
  expect_true(all(table(d$classroom[r$rows]) >= 2L))
  expect_identical(r$info$sample$n_clusters, length(unique(d$classroom)))
  ## a room number reused across schools is refused, with both readings
  expect_error(ilm_resolve_rows(d, ilm_sample(prop = 0.2, within = c("school", "room"))),
               "give them unique ids, such as paste(school, room)", fixed = TRUE)
  expect_error(ilm_resolve_rows(d, ilm_sample(prop = 0.2, within = c("school", "room"))),
               "the design is crossed: draw each factor's levels with by = c(", fixed = TRUE)
  ## crossed groupings: raters who work in every school
  d$rater <- rep(c("r1", "r2", "r3", "r4"), length.out = 94)
  expect_error(ilm_resolve_rows(d, ilm_sample(prop = 0.2, within = c("school", "rater"))),
               "not nested")
  expect_error(ilm_sample(prop = 0.2, by = "school", within = "room"), "not both")
  expect_error(ilm_resolve_rows(d, ilm_sample(prop = 0.2, within = "nope")), "not in the data: nope")
})

test_that("within = restores the stream with a seed, and negates to the rows not drawn", {
  d <- within_data()
  set.seed(5); before <- .Random.seed
  a <- ilm_resolve_rows(d, ilm_sample(prop = 0.3, within = "school", seed = 9))
  expect_identical(.Random.seed, before)
  expect_identical(ilm_resolve_rows(d, ilm_sample(prop = 0.3, within = "school", seed = 9))$rows,
                   a$rows)
  b <- ilm_resolve_rows(d, ilm_sample(prop = 0.3, within = "school", seed = 9), negate = TRUE)
  expect_identical(sort(c(a$rows, b$rows)), seq_len(94))
  ## the chance of being left out, for the rows left out
  expect_equal(b$inclusion, 1 - c(s1 = 0.3, s2 = 0.3, s3 = 0.5)[d$school[b$rows]],
               ignore_attr = TRUE)
  ## ilm_subset() carries each row's chance; a result keeps the summary
  s <- ilm_subset(d, subset = ilm_sample(prop = 0.3, within = "school", seed = 9))
  expect_length(attr(s, "ilm_inclusion"), nrow(s))
  rec <- attr(suppressMessages(ilm_describe(d, "y", subset = ilm_sample(prop = 0.3,
    within = "school", seed = 9))), "ilm_select")$subset$sample
  expect_identical(rec[c("prop", "min")], list(prop = 0.3, min = 2L))
  expect_identical(rec$seed$value, 9L)
})

## ---- crossed factors drawn by factor (item 284) ---------------------------

crossed_data <- function()
  expand.grid(rater = paste0("r", 1:10), item = paste0("i", 1:20), rep = 1:2,
              stringsAsFactors = FALSE)

test_that("by = c(rater = , item = ) keeps the rows whose levels were all drawn", {
  d <- crossed_data()
  r <- ilm_resolve_rows(d, ilm_sample(by = c(rater = 0.3, item = 0.2), seed = 4))
  raters <- unique(d$rater[r$rows]); items <- unique(d$item[r$rows])
  expect_length(raters, 3L); expect_length(items, 4L)
  ## the keep rule: every row of a drawn rater and a drawn item, no other
  expect_identical(r$rows, which(d$rater %in% raters & d$item %in% items))
  f <- r$info$sample$factors
  expect_identical(vapply(f, `[[`, "", "column"), c("rater", "item"))
  expect_identical(vapply(f, `[[`, 1L, "levels_kept"), c(3L, 4L))
  expect_identical(vapply(f, `[[`, 1L, "levels_given"), c(10L, 20L))
  expect_identical(r$info$sample$seed$value, 4L)
  ## counts against shares: 3 raters by count is 3 by share of 0.3
  rc <- ilm_resolve_rows(d, ilm_sample(by = c(rater = 3, item = 4), seed = 4))
  expect_identical(lengths(lapply(list(d$rater[rc$rows], d$item[rc$rows]), unique)), c(3L, 4L))
  expect_identical(rc$info$sample$factors[[1]]$n, 3L)
  ## a factor kept whole, as 1 or "all"
  for (whole in list(c(rater = 3, item = 1), c(rater = "3", item = "all"),
                     list(rater = 3, item = "all"))) {
    rw <- ilm_resolve_rows(d, ilm_sample(by = whole, seed = 1))
    expect_length(unique(d$item[rw$rows]), 20L)
    expect_length(unique(d$rater[rw$rows]), 3L)
    expect_true(rw$info$sample$factors[[2]]$whole)
  }
  ## one factor named works as one group column did
  expect_length(unique(d$rater[ilm_resolve_rows(d, ilm_sample(by = c(rater = 0.5), seed = 1))$rows]), 5L)
})

test_that("crossed sampling restores the stream, negates, and says what it needs", {
  d <- crossed_data()
  set.seed(8); before <- .Random.seed
  a <- ilm_resolve_rows(d, ilm_sample(by = c(rater = 0.3, item = 0.2), seed = 4))
  expect_identical(.Random.seed, before)
  b <- ilm_resolve_rows(d, ilm_sample(by = c(rater = 0.3, item = 0.2), seed = 4), negate = TRUE)
  expect_identical(sort(c(a$rows, b$rows)), seq_len(nrow(d)))
  expect_error(ilm_sample(prop = 0.2, by = c(rater = 0.3)), "leave `n` and `prop` out")
  expect_error(ilm_sample(by = c(rater = 0.3, 0.2)), "needs its own name")
  expect_error(ilm_sample(by = c(rater = 1.5)), "a share between 0 and 1, a count")
  expect_error(ilm_sample(by = c(rater = 0)), "a share between 0 and 1, a count")
  expect_error(ilm_sample(by = c("rater", "item"), prop = 0.2), "names each with its share")
  expect_error(ilm_resolve_rows(d, ilm_sample(by = c(rater = 11))), "11 levels of rater and there are 10")
  expect_error(ilm_resolve_rows(d, ilm_sample(by = c(rater = 0.01))), "comes to none")
  expect_error(ilm_sample(by = c(rater = 0.3), within = "item"), "not both")
  ## within is for nested levels; a crossed pair is pointed to by = c(...)
  expect_error(ilm_resolve_rows(d, ilm_sample(prop = 0.2, within = c("rater", "item"))),
               "draw each factor's levels with by = c(", fixed = TRUE)
  ## and the result keeps the factors
  rec <- attr(suppressMessages(ilm_describe(transform(d, y = seq_len(nrow(d))), "y",
    subset = ilm_sample(by = c(rater = 0.3, item = "all"), seed = 2))), "ilm_select")$subset$sample
  expect_identical(rec$factors[[2]][c("column", "whole", "levels_kept")],
                   list(column = "item", whole = TRUE, levels_kept = 20L))
})
