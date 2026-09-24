# Profile which values are missing, and for whom

The missingness counterpart to
[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md):
runs
[`ilm_reduce_na()`](https://huttoncp.github.io/illumex/reference/ilm_reduce_na.md)
then
[`ilm_cluster_na()`](https://huttoncp.github.io/illumex/reference/ilm_cluster_na.md),
and describes each cluster of rows by which columns' *missingness* sets
it apart – "income is missing for 92% of them, against 18% overall" –
rather than by those columns' values.

## Usage

``` r
ilm_profile_na(
  data,
  cols = NULL,
  ndim = 5,
  ...,
  vtest_threshold = 1.96,
  top_n_vars = 4
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

## Value

An object of class `"ilm_profile_na"`, which is also an `"ilm_profile"`.

## Details

This is how a structured gap shows itself: a block of variables that go
missing together points at a shared cause, such as a section of a form
everyone in one group skipped, which is a different problem from values
going missing one at a time.
[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
then says whether any of it threatens the model you intend to fit.

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
#> Cluster 1 holds 35 rows, 22.9% of the data (stable). What sets it apart: Ozone is missing for 100% of them, against 24% overall.
#> Cluster 2 holds 2 rows, 1.3% of the data (stable). What sets it apart: Solar.R is missing for 100% of them, against 5% overall; and Ozone is missing for 100% of them, against 24% overall. It is a small cluster, 1.3% of observations: possibly a real minority pattern, possibly a data problem, but worth looking at either way.
#> Cluster 3 holds 111 rows, 72.5% of the data (stable). What sets it apart: Ozone is missing for none of them, against 24% overall.
#> Cluster 4 holds 5 rows, 3.3% of the data (stable). What sets it apart: Solar.R is missing for 100% of them, against 5% overall. It is a small cluster, 3.3% of observations: possibly a real minority pattern, possibly a data problem, but worth looking at either way.
```
