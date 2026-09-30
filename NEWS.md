# illumex 0.0.8.9003

* Choosing rows (items 277, 279 and 280). Every function that acts on a data
  frame takes `subset`, applied first, before `cols`, `by` and everything
  else: a logical vector (`NA` left out), row positions, named patterns
  (`subset = c(site = "^north")`), or `ilm_sample()`. `subset_negate = TRUE`
  takes the other rows -- a holdout -- and `subset_fixed = TRUE` matches the
  patterns literally. Results name rows by the data's own row numbers, so
  they join back as they are. A subset of an `ilm_anomaly()` result is an
  error: subset the data before `ilm_anomaly()`. See `?ilm_selection`.
* `ilm_sample(n, prop, by, seed)`, new, draws rows for `subset`: `n` of them,
  or a share `prop`; with `by`, whole groups, so a holdout never splits one.
  A seed draws the same rows every time and leaves your random stream as it
  was.
* `ilm_subset()`, new, returns the rows and columns a function would use,
  rows first, with the original row numbers as row names.
* `cols_fixed = TRUE` matches a `cols` pattern literally, as a substring; a
  column name still wins.
* A result made from chosen rows or columns says so above what it prints --
  `119 of 600 rows (subset)`, `481 of 600 rows (subset, negated)`, `Columns:
  5 of 8 (excluded: id, name, date)`, with more than eight excluded names cut
  to "and N more" -- and a plot says it in a message when rows were left out.
* `ilm_cluster()` takes `cols` and `subset`: on a data frame it reduces, or
  on coordinates, `cols` chooses the columns before any reduction; on an
  `ilm_reduce()` result, whose dimensions `ndim` chose, `cols` is an error.
  `ilm_cluster_na()` takes `subset`.
* Pre-release changes to arguments:
  - `ilm_copies()` and `ilm_dupes()` take their key columns as `cols`, in
    every form and with `cols_negate`, in place of `...`: `ilm_copies(d, "id")`
    still works; `ilm_copies(d, "id", "date")` becomes
    `ilm_copies(d, c("id", "date"))`.
  - `ilm_recode_errors()`'s `rows` is renamed `subset`, and it takes
    `subset_negate`, `subset_fixed`, `cols` in every form, `cols_negate` and
    `cols_fixed` (a random sample is refused there). `keep_all = TRUE`, the
    default, returns every row and column with the chosen cells recoded;
    `keep_all = FALSE` returns only the chosen rows and columns.
  - `ilm_check_missing()`'s `covariates` takes every form of `cols`, with
    `covariates_negate` and `covariates_fixed`.
* `ilm_copies()` and `ilm_dupes()` return rows with the data's own row
  numbers (or names) as row names, where they were numbered afresh, so the
  rows found join back onto the data.
* `ilm_reduce(method = "glrm")`'s `ind_coord` starts with `row_id`, as its
  help says and as the other routes give it; it had only the dimensions.
* `ilm_wash_df()` takes neither `subset` nor `cols`, and its help says to
  choose the part to wash with `ilm_subset()` first.
* Every `ilm_plot_*()` that passes `...` to tinyplot now has its own
  `subset`, which takes the place of tinyplot's.
* `ilm_describe_all()`, `ilm_describe_na_all()`, `ilm_counts_all()`,
  `ilm_counts_tb_all()`, `ilm_plot_all()` and `ilm_plot_na_all()` take `cols`
  and `cols_negate`, as `ilm_outliers_all()` does (item 278). Where `class`
  already narrows the columns, `cols` chooses among those. `cols` comes right
  after `by` (after `data` in `ilm_counts_tb_all()`, which has no `by`), so a
  call that passed the next argument by position now needs its name:
  `ilm_describe_all(d, "grp", 2)` becomes `ilm_describe_all(d, "grp", digits
  = 2)`.
* Every function that takes `cols` takes `cols_negate`: with `cols_negate = TRUE`,
  `cols` names the columns to leave out -- `cols = c("id", "site")`,
  `cols = "^score_"` or `cols = is.numeric` -- and every other column the
  function can use is used (item 276). `by` is never negated. See
  `?ilm_selection`.
* A `by` argument takes column names, as the help now says; only
  `ilm_outliers_all()`'s also takes a pattern or a predicate. A pattern or a
  function given as `by` elsewhere is one clear error ("`by` takes column
  names; ^grp is not one") rather than a message from deep inside the call.
* `ilm_reduce_na()`, `ilm_profile_na()`, `ilm_plot_var_all()` and
  `ilm_plot_var_pairs()` take a pattern or a predicate as `cols`, as the help
  always said; they took names only.
* Numbers are written in full with at most 15 figures before the point
  and, below 1, at most 6 decimals, after rounding, and in R's scientific
  notation beyond, so sentences and reports match R's printed tables (item
  290): "1.23e+300", "1.00e-05", "5.05e-08", with the exponent's sign and
  at least two digits. To 3 figures, 0.0001 is "0.000100" and 0.00001 is
  "1.00e-05"; the decimals are counted with the zeros at the end kept, so
  dropping them does not move the switch. A value shown to significant
  figures keeps them in the mantissa ("1.00e+15"). One shown to fixed
  decimals keeps its declared decimals however small (0.0000001 to 8
  decimals is "0.00000010"), and goes scientific only from 1e15, at 7
  figures, as R prints it ("1.234568e+20"). p-values, clock times and
  ordinals are unchanged.
* A value just below a power of ten whose 15 figures are all nines keeps
  them: 999,999,999,999,999 was shown as 1,000,000,000,000,000 when asked
  for 15 figures, because `log10()` of it is 15.
* The shared display rules take a group of numbers read together -- an
  estimate with its interval, a list of predicted values -- and write every
  value at the group's decimals (item 306): none when every value is whole;
  otherwise enough for 3 figures at the group's median size, and for its
  smallest value to show a figure, at most 6. A value that needs more than
  6 decimals to show a figure, or is 1e15 or more, is written in
  scientific notation at 3 figures and sets nothing, so it does not drag
  the others to 6 decimals: 1.25, 0.00000004 and 3.5 read "1.25",
  "4.00e-08" and "3.50". So 12.3456, 0.5 and 250.75 read "12.3", "0.5" and
  "250.8", and -0.034, 0.0125 and -0.5 read "-0.0340", "0.0125" and
  "-0.5000".
* The development version moves to 0.0.8.9003, so that a package needing
  the scientific notation and the group decimals of the shared display
  rules can require them.

# illumex 0.0.8.9002

* The display rules every printed number follows live in
  `R/shared-helpers.R`, the same byte for byte in illume, with the helpers
  the packages already shared (`ilm_wrap()`, `ilm_and()`, the progress bar,
  the random-stream restore and the plotting-symbol names), so they cannot
  drift apart. Their hand-written cases are in
  `tests/testthat/fixtures/format_cases.csv`. Numbers round half away from
  zero, after clearing floating-point noise, where R's own rounding took a
  tie to even: a profile's paragraph showed 109 of 400 rows as 27.2%, now
  27.3%. A duration's quartiles are printed by the same rule as a number's
  ("1,230 days", not "1230 days").
* Numbers shown at three significant figures keep their trailing zeros
  (10.0, 2.50, 51.0; Craig's item 256), and an estimate that happens to be
  whole still shows its figures: 42 is "42.0" and 19234 "19,200". A count,
  or a data value that is a whole number (a profile's quartile of
  whole-number data), is shown whole. The prints the tests hold are
  unchanged; profile sentences quoting a quartile that is not whole show its
  figures ("2.50").
* `ilm_describe()` describes values too large for a finite spread (near
  1e300) without assessing their shape, and says so in the note: "values
  too large to assess".
* The development version moves to 0.0.8.9002, so that a package needing
  the shared display rules can require them: illume, whose copy of
  `R/shared-helpers.R` is tested against the installed illumex, then
  fails clearly against an older illumex instead of finding none of the
  file's names.

# illumex 0.0.8.9001

* Text that is not valid UTF-8 -- a file saved in Windows-1252 or Latin-1
  and read as UTF-8 -- no longer stops illumex. One such value among 1.4
  million rows stopped `ilm_wash_df()` ("input string 1328351 is invalid
  UTF-8", from `trimws()`). Such text is now left as it is, its bytes
  unchanged and no encoding guessed:
  - `ilm_wash_df()` completes, and warns which columns hold such text, how
    many values and their first rows, and how to read the file in its own
    encoding or convert it with `iconv()`; the result keeps the same table
    as `attr(x, "not_utf8")`. A column name that is not valid UTF-8 is
    cleaned with its stray byte spelled out (`caf_e9`).
  - `ilm_wash_df(encoding = )` converts such text, given the file's encoding
    (item 298): `encoding = "windows-1252"` converts the values, factor
    levels and column names that are not valid UTF-8 from it, and a message
    says how many values were converted in each column (also
    `attr(x, "converted")`). Valid text is never touched and no encoding is
    guessed. A value that does not convert, or does not convert back to the
    same bytes, is left as it is and reported. Without the argument, the
    warning suggests it by name.
  - `ilm_describe()` and `ilm_describe_all()` describe such a column, and
    its note says how many values are not valid UTF-8.
  - Plots draw such text with its stray bytes spelled out (`caf<e9>`):
    R's graphics devices cannot draw it, and the pdf device crashed R.
    Prints that list column names show them the same way.
  - `ilm_reduce()`, `ilm_glrm()` and `ilm_anomaly(method = "iforest")`
    stop, naming the columns, when a column's name is not valid UTF-8,
    since no model can be built from it.
* `ilm_plot_var_pairs()` without `by` draws again. It failed on every such
  call with "formal argument by matched by multiple actual arguments":
  `tinyplot::tinypairs()` keeps an explicit `by = NULL` from its call and
  adds its own, so `by` is now passed only when there is one.
* `ilm_reduce(method = "glrm")` keeps only the dimensions that carry
  variation (Craig's item 265): a singular value of the centred low-rank
  product at or below `sqrt(.Machine$double.eps)` times the first, the
  numerical-rank cut `MASS::ginv()` uses, is zero to rounding. Heavy
  shrinkage can leave fewer such dimensions than were fitted, and a zero
  singular value's direction is arbitrary, so its loadings differed from one
  platform to the next (0.964 on one, 0.930 on another, for the same fit).
  The print says so ("4 dimensions carry variation (5 fitted; after
  shrinkage the 5th has none)"), the coordinates and loadings cover the
  dimensions kept, and `ilm_glrm()` reports its own (`rank_kept`).
* `ilm_cluster()`, and so `ilm_profile()`, lets each k-means start run to 100
  iterations, where `stats::kmeans()` stops at 10. On the gap statistic's
  reference sets the old limit stopped some starts early and warned once for
  each -- 128 warnings on one census study -- though the chosen number of
  clusters was the same (`dev/studies/kmeans_iter.R`). A start that still
  stops at the limit is now counted, and the printed result says so once.
* The note on incomplete rows (from `ilm_cluster()` and `ilm_anomaly()`) no
  longer rounds a share to a limit it has not reached: 99.8% of rows complete
  printed as "only 100%", and a column 0.2% missing as "0%".
* The help of `ilm_cluster()`, `ilm_profile()`, `ilm_profile_na()`,
  `ilm_anomaly()` and `ilm_reduce()` says how large a data set each handles,
  measured on one core of a 16 GB Windows machine at 10,000 to 250,000 rows
  (`dev/studies/scale_check.R`, results beside it):

  | function | 10,000 rows | 50,000 | 100,000 | 250,000 |
  |---|---|---|---|---|
  | describe, frame checks, counts, outliers, missing-data check, cleaning, plots | under 1 s | under 4 s | under 6 s | under 16 s |
  | `ilm_boot_ci()` (2,000 resamples) | 2 s | 14 s | 24 s | 53 s |
  | `ilm_reduce()`, the default | under 1 s | under 1 s | under 1 s | under 1 s |
  | `ilm_var_contrib()` | 5 s | 17 s | 33 s | 83 s |
  | `ilm_anomaly()` | 32 s | 13 min | over 15 min | -- |
  | `ilm_anomaly(method = "iforest")` | 3.5 min | over 15 min | -- | -- |
  | `ilm_reduce(method = "glrm")` | 5 min | over 15 min | -- | -- |
  | `ilm_cluster()` with `k` given | 35 s, 1.2 GB | fails: 9.3 GB | -- | -- |
  | `ilm_cluster()`, `k` chosen (the default); `ilm_profile()`, `ilm_profile_na()` | over 15 min | -- | -- | -- |

  Every clustering computes silhouettes from a full distance matrix, which
  is what stops it at about 40,000 rows on 16 GB.

* The development version moves to 0.0.8.9001, so that a package needing
  this cycle's changes can require them. Builds made before these changes
  also called themselves 0.0.8.9000, among them ones whose `ilm_reduce()`
  still needs PCAmixdata.

* `ilm_describe()`'s `skew` and `kurt` are now the type 2 estimators of
  Joanes and Gill (1998), the ones SAS and SPSS report, where they were type
  3 (e1071's default). Type 2 kurtosis is unbiased for normal data; type 3
  averages -0.54 at n = 20 and -0.24 at n = 50. The same kurtosis decides
  `gauss_note`'s "heavy-tailed" and "light-tailed", so a small sample is
  called light-tailed less often: in the recorded prints, one group of 20
  (excess kurtosis -1.045 before, -0.715 now) no longer is.
* The help for `ilm_copies()`, `ilm_counts()`, `ilm_describe()` and
  `ilm_boot_ci()` says when each is worth running and what to read in it:
  copies before and after a join, errors at the two ends of a count, codes
  for a missing value at the minimum or maximum, what the 95% of a bootstrap
  interval means, and the median and quartiles when `gauss_note` says skewed.
* `ilm_boot_ci(ci_type = "bca")` warns when its interval cannot be relied
  on, naming the groups and the remedy: below 8 rows, where its corrections
  are estimated too poorly to help (measured: at 3 and 5 rows the median's
  BCa interval covered 70% and 82% against the percentile interval's 79% and
  94%), or when its bias correction or acceleration is undefined. The
  intervals themselves are unchanged.
* `ilm_boot_ci()` takes several columns in `y`, or none for every numeric
  column besides `by`, and returns one long table led by a `variable`
  column, as `ilm_counts_tb_all()` does. Each column starts from `seed`, so
  its rows are the ones it gets alone; each is checked against
  `boot::boot.ci()`.
* The exploring-data vignette gains four sections: a worked cleaning of a
  messy copy of mtcars, from the copies to the cleaned table checked against
  the original; a median for each group with its BCa interval; Anscombe's
  four data sets, the same summary drawn four ways; and change over time,
  with what a line hides about how many rows lie behind each point.
* `ilm_copies()` and `ilm_dupes()` say so when rows are copies, and give the
  call that keeps one of each: "22 of 44 rows share their values with another
  row; ilm_copies(data, filter = "first") keeps one of each, leaving 32." The
  call names the data and the key columns as they were given.
* `ilm_wash_df()`'s names follow janitor's `make_clean_names()`, in base R,
  and no letter is dropped: "%" becomes `percent` and "#" `number` (they
  were dropped, so "% change" became `change`), accented Latin letters become
  plain ones through an explicit table (an accented e becomes e, where before
  the letter was dropped and a word split around it), letters of other scripts are kept, acronyms split
  (`html_parser`), a name starting with a digit gains an `x`, an empty one is
  `x` (it was `v`), and a repeated name gains `_2` where it gained `_1`. On 73
  test names illumex gives janitor 2.2.0's result for 66; the other 7 are the
  deliberate departures, letters of other scripts and two ligatures.
* `ilm_wash_df(data)` with no other argument is tested on a messy copy of
  mtcars, a tibble, zero rows, all-empty columns and factors. A factor column
  of blanks now counts as empty, as a text one does, and a data frame whose
  every column is empty now has no rows left either. Its help says that a text
  column of whole numbers becomes integer, with the values unchanged.
* `ilm_wash_df()` returns a tibble for a tibble and a data.table for a
  data.table, as it was given (a plain data.frame with `column_to_rownames`,
  since neither keeps row names); any other data frame comes back a
  data.frame. It returned a data.frame for all of them.
* `ilm_recode_errors()` no longer changes the type of columns it finds nothing
  to recode in: recoding `"six"` to `"6"` across a data frame turned every
  numeric column into text, because R converts a vector when a text value is
  assigned to none of its elements.
* `ilm_plot()` and the named plots (`ilm_plot_histogram()`, `_density()`,
  `_box()`, `_violin()`, `_scatter()`, `_bar()`, `_line()` and
  `_stat_error()`, and so `ilm_plot_var()`) gain `facet`, a column named as a
  string the way `by` is: `facet = "site"` draws one panel per level. Each is
  tested against tinyplot drawing the same panels from its own formula
  interface, the two images identical byte for byte. A formula
  (`facet = ~site`) is an error that shows the string form. The binned
  density `ilm_plot()` switches to above `n_max` points has no panels and
  says so.
* `ilm_plot_scatter()` gains `trend = "gam"`, `mgcv::gam(y ~ s(x), method =
  "REML")` with mgcv's credible band, held to mgcv's own predictions in the
  tests. mgcv, which comes with R, joins Suggests. Its help says how each
  trend's band is made: lm's confidence band from `predict.lm()`, loess's
  and gam's fit plus or minus t or z standard errors.
* `ilm_plot_scatter()` with a trend now draws the points with the line over
  them; it drew the line and band alone. The axis is drawn to hold the band
  as well as the points, so a band is no longer cut off at the edge of the
  plot.
* The named plots title a `by` legend with the column's name; they printed
  the code that picked the column, such as `ilm_col_vec(data, by, "by")`.
  They gain `legend`, which takes anything tinyplot's does, a title of your
  own included.
* `ilm_plot_box()` and `ilm_plot_violin()` label the horizontal axis with the
  column's name; they printed the code that picked the column.

* `ilm_describe()` and `ilm_describe_all()` gain `smd`. With `by`, `smd = TRUE`
  adds each group's standardised difference from the first group, and
  `smd = "control"` from the group named: for a number, the difference in
  means over the square root of the average of the two groups' variances
  (Austin 2009); for a binary, the difference in proportions over the square
  root of the average of their p(1 - p); for a category of more than two
  levels, Yang and Dalton's multivariate difference. They agree with
  tableone's to 1e-12 (`dev/studies/make_smd_fixtures.R`).
* `ilm_anomaly(method = "iforest")` names a row's driver differently: each
  column is replaced by the value the other columns predict for it -- a
  number by regression on them, a category by the commonest level among
  the row's nearest rows -- where it was replaced by its median or
  commonest level. Against planted anomalies (`dev/studies/driver_redesign.R`,
  50 replicates), a row whose category contradicts its numbers was
  attributed to that category for 0.34 to 0.38 of such rows, where it was
  0.17 to 0.25; rows pushed out of a column's range and rare pairings of
  categories were attributed as well or better (0.85 to 0.99, 0.90 to
  0.95); a scan takes about half as long again. Two in three rows whose
  category contradicts their numbers are still attributed elsewhere, and
  the help says so. An earlier comment's "94%" for that case was not what
  the studies measured. Above 2,000 rows a category's nearest rows are
  searched among 2,000 drawn under the scan's seed, which at 10,000 rows
  gave the same driver as searching every row for 0.998 to 1.000 of
  flagged rows (`dev/studies/driver_pool.R`). These rates are the
  forest's; the default method's driver, the column with the largest
  residual, is unchanged and has not been measured against planted
  anomalies.
* `ilm_reduce()` computes the factor analysis of mixed data itself, rather
  than through PCAmixdata, which is no longer used: the same eigenvalues,
  coordinates and loadings to within 4e-12, faster from 2,000 rows up, and
  nothing to install. Each dimension's sign is now fixed by a rule -- the
  column that loads most on it loads positively -- so a coordinate may have
  the opposite sign to before and a map may be mirrored; distances,
  clusters and descriptions are unchanged, though on data with many tied
  rows, such as `ilm_cluster_na()`'s markers of missingness, k-means may
  number the same clusters differently. The agreement with FactoMineR
  and PCAmixdata is tested against their stored outputs. The method is
  now called `"famd"` in `ilm_reduce()` and `ilm_profile()`; `"pcamix"`,
  its former name, is still accepted and means the same, so no call
  breaks.
* The gaussian index (`gauss`, from `ilm_gauss_check()` and the describe
  functions) reaches 0 at an excess distance of 0.06 rather than 0.12, so
  clear departures sit at the bottom of the scale: with 500 rows, t with 3
  degrees of freedom scored 0.52 and a two-humped mixture 0.14, and now
  score 0.07 and 0 (`dev/studies/gauss_cap.R`). Normal data still score 1.
  The help says what the index is for: the size of a departure, while
  `gauss_note` and a plot show its kind. The index and the distance are
  now kept whole, as the describe functions' values are, and print as
  before; `ilm_gauss_check()` returns a list of class `"ilm_gauss"`.
* Results that were plain data frames or lists now carry a class of their
  own, in front of `data.frame` (or `list`): `ilm_describe()` (and a single table from
  `ilm_describe_all()`), `ilm_describe_all()`, `ilm_frame_issues()`,
  `ilm_describe_na()` and `ilm_describe_na_all()`, `ilm_outliers()`,
  `ilm_outliers_all()`, `ilm_boot_ci()` and `ilm_boot_diff()`. They print,
  subset and combine as before; what changes is `class()`, and
  `identical()` against a plain data frame. `ilm_outliers_all()` also
  keeps the method, the threshold in effect, the grouping and how many
  values each column had checked, and the bootstrap results the seed they
  drew with.
* `ilm_cluster()`'s `clusters` (`pct`, `jaccard`, `mean_silhouette`) and
  `ind_cluster$silhouette`, and `ilm_reduce()`'s `eig` and `var_contrib` on
  the default method, now hold full precision: they were rounded when
  stored, so `r$eig` and `cl$clusters` show more digits than before.
  Printing rounds as it did, and prints the same. `ilm_describe()`,
  `ilm_describe_all()`, `ilm_describe_na()` and `ilm_describe_na_all()`
  likewise keep their values whole: `digits` now sets how they print, not
  what they store.
* A function given a seed puts the user's random-number stream back as it
  leaves -- as it was, or absent if it was absent. Every function with a
  `seed` argument set it and left the stream there, so the user's next
  random draw came out the same whatever came before it. Seeded results are
  unchanged. The seven functions: `ilm_anomaly()`, `ilm_boot_ci()`,
  `ilm_boot_diff()`, `ilm_cluster()`, `ilm_glrm()`, `ilm_sim()` and
  `ilm_var_contrib()`; a test fails if a new seeded function does not do the
  same. With `seed = NULL`, `ilm_anomaly()`, `ilm_glrm()`, `ilm_sim()` and
  `ilm_var_contrib()` now draw from the user's stream, as the other three
  already did; they called `set.seed(NULL)`, which re-seeds from the clock,
  so a `set.seed()` of the user's own before the call fixed nothing. One
  consequence: `ilm_profile(method = "glrm")` without a `seed` used to
  cluster from the stream `ilm_glrm()` left behind, and so gave the same
  clusters every time by accident; it now draws from the user's stream, as
  the default method always did. Pass `seed` for a fixed answer.
* `ilm_profile()` computes the v-test for a categorical value as
  FactoMineR's `catdes()` does, from the hypergeometric probability of the
  cluster's count, rather than by a normal approximation to it; the two
  differed by up to 0.18 on a thin level. Numeric v-tests were already the
  same. Both are now held to stored `catdes()` output in the tests, as are
  `ilm_reduce()`'s eigenvalues and coordinates to `FAMD()`'s.
* `ilm_reduce(method = "glrm")` now reports an orthogonal rotation of the
  fitted low-rank model, as principal components are, rather than the
  optimiser's own factors. Those factors are not unique -- any rotation of
  one can be undone in the other -- so their dimensions overlapped, and a
  share of variance per dimension double-counted, with the cumulative share
  able to pass 100%. `pct_var` and `cum_pct_var` are now shares of the
  numeric columns' variance and add up exactly; the categorical columns,
  fitted on the logit scale, get their own `pct_categorical` and
  `cum_pct_categorical`. The reconstruction is unchanged and `fit` keeps the
  original factors, but the coordinates and contributions change, and so do
  any clusters built on a low-rank reduction. On numeric data with no
  penalty the shares now match principal components'.
* `ilm_frame_issues()` checks the structure that breaks a model matrix and
  that no pair of numeric correlations shows: categorical columns that are
  the same grouping under different labels (`aliased_factors`), one grouping
  inside another (`nested`, which is expected for random effects and a trap
  for fixed ones), categorical pairs with Cramer's V at or above the new
  `v_cut` (default 0.95, `redundant_categories`), and columns that are an
  exact linear combination of others, found from a pivoted QR of the model
  matrix and named with what they are made of (`rank_deficient`). Every row
  now carries a `remedy`, and an identical pair is reported once, as a
  duplicate, rather than again as collinear.
* The version moves in step with `illume` 0.0.8.9000, which now requires
  `illumex (>= 0.0.8.9000)`.
* Dates are used in clustering rather than dropped. `ilm_reduce()`,
  `ilm_glrm()` and `ilm_profile()` gain `time`. By default (`"cycles"`) a
  date or date-time column becomes the time since its earliest value -- R's
  own number for it, which keeps order and spacing in one column -- plus the
  time of day, the day of the week, the day of the month and the time of
  year as sine-cosine pairs, so that 23:00 sits next to midnight and December
  next to January; but only the cycles some other column varies with, since a
  cycle nothing else follows is noise the clustering will split on. A cycle
  is let in when that variation stands five standard deviations above what
  shuffled dates give, a bar set from its measured errors: at the usual p <
  0.01 chance let a cycle in for 3 of 160 data sets with no rhythm, and one
  such admission took a clustering from 0.36 to 0.00. `"elapsed"` keeps the
  time line alone and skips the test, for very large data; `"drop"` is the
  old behaviour. A duration is used as its number of days.
* Measured on two known clusters (400 rows, 20 replicates, adjusted Rand
  index): elapsed time costs nothing where the date is irrelevant (0.38 to
  0.36) but cannot see a rhythm; every cycle given unasked took recovery to
  0.10 where the date meant nothing; the tested cycles left it at 0.36 there
  and raised it from 0.36 to 0.94 for winter against summer, 0.93 for night
  against day, 0.79 for weekends and 0.58 for month-ends. Expanding a date
  into year, month, day and weekday numbers instead scored 0.00 to 0.02 in
  all of those: the year and the date count the trend twice and take over the
  reduction, and a numbered month puts December as far from January as it
  can.
* `ilm_profile()` and `ilm_profile_na()` describe each cluster by the original
  variables that set it apart, in their own units, rather than by the
  reduction's dimensions: "employment is 'retired' for 86% of them, against
  21% across all rows; age is higher: the middle 50% of its values lie
  between 64 and 72, against 29 to 54 across all rows". The dimension-based
  sentences named which variables a cluster's
  position was made of without saying which way the cluster lay on them: on a
  sample with three known subgroups, which the clustering recovered almost
  exactly, two of the three got the same sentence word for word. Variables are
  ranked by a v-test against all rows (as FactoMineR's `catdes()`) and named
  when it clears `vtest_threshold` and the difference is big enough to matter,
  0.2 standard deviations or 10 percentage points -- on 1,200 rows a column of
  pure noise cleared 1.96 in two clusters of three. A date is described in
  dates, down to the stretch of a cycle where a cluster stands out ("falls on
  Sat-Sun for 87% of them, against 50% across all rows"); markers of missingness as
  "income is missing for 92% of them". The variables that set no cluster apart
  are named after the paragraphs, or counted when there are many.
* `ilm_profile()`'s `characterization` is now one row per cluster and variable
  (and aspect of a date); `frequencies` is new (every value of every
  categorical variable, per cluster, with its count, its share and its share
  among all rows), as is `by_cluster` (`ilm_describe_all()` of the variables,
  by cluster). `top_n_vars` now caps the variables named for one cluster, and
  defaults to 4.
* `ilm_var_contrib()` scores a date on the aspects the clustering used, its
  time line or a cycle, and says which in `type` ("date: day of the week").
  Scored on the time line alone, a clustering that split on the day of the
  week called its own defining variable no better than chance.
* `print.ilm_var_contrib()` prints a subset of its columns as the plain table
  it is, rather than stopping on a column that is not there.
* A variable that `ilm_var_contrib()` cannot score, such as a constant
  column, has no verdict. The advice under its table, and under a profile's
  print, named such a variable "NA" as one that separates the clusters no
  better than chance, and said to drop it; it now leaves it out.

# illumex 0.0.7.9000

* First version: the exploratory half of `illume` 0.0.7.9000, split out into a
  package of its own. Every function keeps its name, its arguments and its
  behaviour, and `illume` attaches `illumex`, so code written against `illume`
  runs unchanged.
* What moved: description and cleaning (`ilm_describe*()`, `ilm_counts*()`,
  `ilm_dupes()`, `ilm_copies()`, `ilm_wash_df()`, `ilm_recode_errors*()`,
  `ilm_translate()`, `ilm_frame_issues()`, `ilm_gauss_check()`); bootstrap
  intervals without a model (`ilm_boot_ci()`, `ilm_boot_diff()`); outliers and
  multivariate anomalies; dimension reduction, clustering and profiling,
  `ilm_glrm()` included; describing missing values (`ilm_check_missing()` and
  the `*_na()` functions); the plots of data rather than of a model; and the
  example data, `ilm_sim()`.
* What stayed in `illume`: everything that fits or reads a model, including
  imputation and pooling (`ilm_impute()`, `ilm_mi_pool()`), which need one.
* It needs neither TMB nor RTMB. Its only imports are `collapse`, `tinyplot`
  and base R's own packages, and `cluster`, `isotree` and `PCAmixdata` are used
  where they are installed.
* illumex has a hex sticker of its own. It is shown in the README and on the
  pkgdown site, and the site's favicons are made from it.
