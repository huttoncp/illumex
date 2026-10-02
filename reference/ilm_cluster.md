# Cluster observations, choosing the number of clusters

Groups the rows of an
[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md)
result, or any numeric coordinates, by k-means or hierarchical
clustering. When `k` is not given it is searched for over `1..k_max` by
the gap statistic. Every cluster gets a stability score from bootstrap
resampling, every observation gets a silhouette width, and clusters
holding less than `small_cluster_frac` of the data are flagged.

## Usage

``` r
ilm_cluster(
  x,
  k = NULL,
  k_max = 10,
  method = c("kmeans", "hclust"),
  dist_method = "euclidean",
  hclust_method = "ward.D2",
  nstart = 25,
  B = 100,
  gap_method = "firstSEmax",
  small_cluster_frac = 0.05,
  ambiguous_threshold = 0.1,
  seed = NULL,
  progress = NULL,
  cols = NULL,
  cols_negate = FALSE,
  cols_fixed = FALSE,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- x:

  An
  [`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md)
  result, or a numeric matrix or data frame of coordinates with one row
  per observation, or a data frame with non-numeric columns, which is
  reduced first.

- k:

  Number of clusters. Omit to choose it by the gap statistic.

- k_max:

  Largest `k` to consider in that search. Ignored when `k` is given.

- method:

  `"kmeans"` or `"hclust"`.

- dist_method:

  Distance for `method = "hclust"`. Euclidean by default, which is right
  here because the clustering runs on continuous dimension-reduction
  coordinates rather than raw mixed-type columns.

- hclust_method:

  Linkage for `method = "hclust"`; `"ward.D2"` by default.

- nstart:

  Random starts for k-means, guarding against a poor local optimum.

- B:

  Bootstrap resamples, used both for the gap statistic and for the
  stability score.

- gap_method:

  Rule for reading `k` off the gap curve; see
  [`cluster::maxSE()`](https://rdrr.io/pkg/cluster/man/clusGap.html).

- small_cluster_frac:

  Clusters holding less than this fraction of the data are flagged.

- ambiguous_threshold:

  Silhouette width below which an observation is flagged individually.

- seed:

  Random seed.

- progress:

  Show a progress bar. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html), so a bar
  appears when someone is watching and nothing is written in a script or
  a knitted document. See
  [ilm_progress_arg](https://huttoncp.github.io/illumex/reference/ilm_progress_arg.md).

- cols:

  Which columns to use, before any reduction: on a data frame that is
  reduced here, the variables that go into the reduction; on a matrix or
  data frame of coordinates, those coordinates. A reduction's dimensions
  are chosen after it, with `ndim` in
  [`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md),
  so `cols` on an
  [`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md)
  result is an error. See
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`. A `by` argument is never negated.

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

An object of class `"ilm_cluster"`: `method`, `k`, `gap` (the search, or
`NULL` when `k` was given), `clusters` (one row per cluster, with
`size`, `pct`, `jaccard`, `stability` labelled `"stable"` above 0.75,
`"moderate"` from 0.5 to 0.75 and `"unstable"` below, following Hennig,
plus `mean_silhouette` and `anomalous`), `ind_cluster` (one row per
observation, with `silhouette`, `is_small_cluster`, `is_ambiguous` and
`is_anomalous`), and `coords`.

## Two different things called anomalous

A **small cluster** is a property of the cluster: a handful of rows that
sit apart from everything else. It may be a real minority pattern or it
may be a data problem, and either way it is worth looking at.

An **ambiguous observation** is a property of the row, taken from its
silhouette width – its average distance to its own cluster against its
average distance to the nearest other one. Near 1 means clearly placed,
near 0 means sitting on a boundary, negative means it is closer to
another cluster than to its own. This is independent of cluster size: a
point can be ambiguous inside a large and otherwise stable cluster,
which a size-based flag alone would never show. Kaufman and Rousseeuw
treat widths below 0.25 as no substantial structure; the default
threshold here is stricter at 0.1.

## Size

Every clustering computes each row's silhouette from a full distance
matrix of n^2 / 2 numbers, so memory is the first limit. On one core of
a 16 GB Windows machine (`dev/studies/scale_check.R` in the source),
with `k` given, k-means took 35 seconds and 1.2 GB at 10,000 rows and
failed at 50,000 rows for want of 9.3 GB; `method = "hclust"` took 13
minutes at 10,000. Choosing `k` by the gap statistic, the default, took
about a minute at 1,000 rows and over 15 minutes at 10,000. On larger
data, give `k`, or cluster a sample of the rows.

## References

Hennig, C. (2007). Cluster-wise assessment of cluster stability.
Computational Statistics and Data Analysis 52(1).

Tibshirani, R., Walther, G. and Hastie, T. (2001). Estimating the number
of clusters in a data set via the gap statistic. JRSS B 63(2).

## See also

[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md),
[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md),
[`ilm_plot_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_plot_cluster.md).

## Examples

``` r
cl <- ilm_cluster(ilm_reduce(mtcars), k_max = 5, B = 25, seed = 1)
cl
#> <ilm_cluster> method = kmeans, k = 4 (chosen by gap statistic)
#> 
#>   cluster   size    pct  jaccard  stability     sil
#>         1      5   15.6    0.834     stable   0.281
#>         2      7   21.9    0.944     stable   0.363
#>         3     12   37.5    1.000     stable   0.625
#>         4      8   25.0    0.881     stable   0.487
```
