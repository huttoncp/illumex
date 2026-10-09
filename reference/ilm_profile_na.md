# Profile which values are missing, and for whom

The missingness counterpart to
[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md):
runs
[`ilm_reduce_na()`](https://huttoncp.github.io/illumex/reference/ilm_reduce_na.md)
then
[`ilm_cluster_na()`](https://huttoncp.github.io/illumex/reference/ilm_cluster_na.md),
and describes each cluster of rows by which columns' *missingness* sets
it apart – "income is missing for 92% of them, against 18% across all
rows" – rather than by those columns' values.

## Usage

``` r
ilm_profile_na(
  data,
  cols = NULL,
  ndim = 5,
  ...,
  vtest_threshold = 1.96,
  top_n_vars = 4,
  cols_negate = FALSE,
  cols_fixed = FALSE,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame.

- cols:

  Columns to use. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- ndim:

  Number of dimensions to keep.

- ...:

  Passed to
  [`ilm_cluster_na()`](https://huttoncp.github.io/illumex/reference/ilm_cluster_na.md).

- vtest_threshold:

  Smallest `|v-test|` for a column to be named in a cluster's
  description; see
  [`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md).

- top_n_vars:

  Most columns to name for one cluster.

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`.

- cols_fixed:

  If `TRUE`, a `cols` string read as a pattern is matched literally, as
  a substring (as `grepl(fixed = TRUE)` does); a column name still wins.
  It changes nothing when `cols` is names or a predicate.

- subset:

  Which rows to use, before anything else: a logical vector (one value
  per row; `NA` is left out), row positions, named patterns
  (`c(site = "^north")`), or
  [`ilm_sample()`](https://huttoncp.github.io/illumex/reference/ilm_sample.md).
  See
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  Results keep the data's own row numbers.

- subset_negate:

  If `TRUE`, the rows `subset` would not take: the other rows, or the
  rows not sampled (a holdout). With a logical `subset`, rows where it
  is `NA` stay out either way.

- subset_fixed:

  If `TRUE`, `subset`'s patterns are matched literally, as substrings.
  It changes nothing for a logical, positions or a sample.

## Value

An object of class `"ilm_profile_na"`, which is also an `"ilm_profile"`.

## Details

This is how a structured gap shows itself: a block of variables that go
missing together points at a shared cause, such as a section of a form
everyone in one group skipped, which is a different problem from values
going missing one at a time.
[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
then says whether any of it threatens the model you intend to fit.

## Size

It runs
[`ilm_cluster_na()`](https://huttoncp.github.io/illumex/reference/ilm_cluster_na.md),
and at its defaults shares that function's limits (see Size in
[`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)):
on one core of a 16 GB Windows machine
([`dev/studies/scale_check.R`](https://github.com/huttoncp/illumex/blob/main/dev/studies/scale_check.R))
it took 17 seconds at 1,000 rows and over 15 minutes at 10,000, nearly
all of it the gap statistic choosing `k`; giving `k` saves that search.

## See also

[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md),
`illume::ilm_impute()`,
[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md).

## Examples

``` r
p <- ilm_profile_na(airquality, k_max = 4, B = 25, seed = 1)
#> ilm_reduce_na(): dropping column(s) whose missingness never varies (always or never missing): Wind, Temp, Month, Day
#> Warning: k was chosen as 4, which is the largest value searched. The curve had not turned, so this is where the search stopped rather than where the evidence pointed. Raise `k_max`, or set `k` from what the design says. On mixed data a selector can also lock onto the number of category combinations rather than the number of clusters; plot(x) shows the gap curve.
cat(p$summary, sep = "\n")
#> Cluster 1 is too small to describe reliably: 5 rows, 3.3% of the data (stable). What sets it apart: Solar.R is missing for 100% of them, against 5% across all rows. It could be a real minority pattern or a data problem, and is worth looking at either way.
#> Cluster 2 holds 35 rows, 22.9% of the data (stable). What sets it apart: Ozone is missing for 100% of them, against 24% across all rows.
#> Cluster 3 is too small to describe reliably: 2 rows, 1.3% of the data (stable). What sets it apart: Solar.R is missing for 100% of them, against 5% across all rows. It could be a real minority pattern or a data problem, and is worth looking at either way.
#> Cluster 4 holds 111 rows, 72.5% of the data (stable). What sets it apart: Ozone is missing for none of them, against 24% across all rows.
```
