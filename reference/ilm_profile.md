# Profile a data set: reduce, cluster, and describe the clusters

Runs
[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md),
then
[`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)
on the dimensions it produces, then says what sets each cluster apart,
in the variables' own units – a paragraph per cluster such as
"employment is 'retired' for 86% of them, against 21% overall; age is
higher: the middle half 64 to 72, against 29 to 54 overall". The aim is
the one profiling exists for: to find the subgroups in a sample and
learn what each of them is.

## Usage

``` r
ilm_profile(
  data,
  cols = NULL,
  ndim = 5,
  method = c("pcamix", "glrm"),
  time = c("cycles", "elapsed", "drop"),
  ...,
  vtest_threshold = 1.96,
  top_n_vars = 4,
  var_contrib = TRUE,
  var_contrib_B = 199L
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

- method:

  `"pcamix"` (the default) for PCA, MCA or FAMD depending on the column
  types, in closed form. `"glrm"` fits a generalized low rank model
  instead, which uses a loss appropriate to each column's type rather
  than squared error on one-hot indicators, and reconstructs a category
  as a category. It costs an iterative fit, and on all-numeric data the
  two are the same model – see
  [`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md)
  for when it is worth that.

- time:

  What to do with date and date-time columns; passed to
  [`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md),
  which describes the choices.

- ...:

  Passed to
  [`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md),
  for instance `k`, `k_max`, `method`, `B` or `seed`.

- vtest_threshold:

  Smallest `|v-test|` for a variable to be named in a cluster's
  description.

- top_n_vars:

  Most variables to name for one cluster.

- var_contrib:

  Run
  [`ilm_var_contrib()`](https://huttoncp.github.io/illumex/reference/ilm_var_contrib.md)
  on the result and report it. Nothing in this pipeline selects
  variables, and an irrelevant one is not neutral: on three known
  clusters with two informative columns, adding two pure-noise factors
  took recovery from 0.301 to 0.006. So the check runs here rather than
  waiting to be asked for, and a clustering that turns out to be one
  variable's levels under another name raises a warning – the cluster
  descriptions would otherwise be read at face value, and they would all
  be true and all about that variable. Costs roughly 70% on top of the
  clustering at 500 rows and five columns.

- var_contrib_B:

  Permutations for that check.

## Value

An object of class `"ilm_profile"`: `reduce`, `cluster`,
`characterization` (one row per cluster and variable – and per aspect of
a date – with its v-test, the size of the difference and the words for
it), `frequencies` (every value of every categorical variable, per
cluster: its count, its share, and its share among all rows),
`by_cluster`
([`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
of the variables, by cluster), `var_contrib` (or `NULL`), `summary` (a
paragraph per cluster) and `not_distinctive` (the variables that set no
cluster apart).

## How a cluster gets described

Each cluster is compared with all rows on every variable the clustering
used. A number is described by its middle half (its quartiles) in the
cluster against the same among all rows; a category by the value whose
share in the cluster sits furthest from its share among all rows; a date
by its middle half, and by the stretch of any cycle the reduction used
(see `time`) where the cluster stands out – "falls on Sat-Sun for 87% of
them, against 50% overall".

The variables are ranked by a v-test – how far the cluster's mean or
share sits from everyone's, in standard errors of a subset that size
(Lebart's, as in FactoMineR's `catdes()`) – and named, strongest first,
up to `top_n_vars`, when the v-test clears `vtest_threshold` and the
difference is big enough to matter: 0.2 standard deviations for a
number, 10 percentage points for a share. With many rows a chance
difference clears 1.96 on its own: on 1,200 rows, a variable that was
noise by construction did in two clusters of three, by 3 and 6 points.
The variables that set no cluster apart are named after the paragraphs,
or counted when there are many; every value of every categorical
variable, per cluster, is in `frequencies`.

It is **not a p-value**. The clusters were found from these same
variables, so the ones the clustering used will differ between the
clusters they helped make. Treat a profile as a description of the
partition you have, not as evidence that the partition is real – the
stability and silhouette figures in
[`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)
are what speak to that.

## See also

[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md),
[`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md),
[`ilm_plot_profile()`](https://huttoncp.github.io/illumex/reference/ilm_plot_profile.md),
[`ilm_profile_na()`](https://huttoncp.github.io/illumex/reference/ilm_profile_na.md).

## Examples

``` r
p <- ilm_profile(mtcars, k_max = 5, B = 25, seed = 1)
cat(p$summary, sep = "\n")
#> Cluster 1 holds 5 rows, 15.6% of the data (stable). What sets it apart: carb is higher: the middle half 4 to 6, against 2 to 4 overall; qsec is lower: the middle half 14.6 to 16.5, against 16.9 to 18.9 overall; gear is higher: the middle half 4 to 5, against 3 to 4 overall; and am is higher: the middle half 1 to 1, against 0 to 1 overall. Less strongly, 1 more variable sets it apart as well.
#> Cluster 2 holds 7 rows, 21.9% of the data (stable). What sets it apart: qsec is higher: the middle half 18.9 to 20.2, against 16.9 to 18.9 overall; vs is higher: the middle half 1 to 1, against 0 to 1 overall; and am is lower: the middle half 0 to 0, against 0 to 1 overall.
#> Cluster 3 holds 12 rows, 37.5% of the data (stable). What sets it apart: disp is higher: the middle half  276 to 400, against  120 to 318 overall; cyl is higher: the middle half 8 to 8, against 4 to 8 overall; gear is lower: the middle half 3 to 3, against 3 to 4 overall; and wt is higher: the middle half 3.52 to 4.07, against 2.46 to 3.57 overall. Less strongly, 5 more variables set it apart as well.
#> Cluster 4 holds 8 rows, 25.0% of the data (stable). What sets it apart: mpg is higher: the middle half 22.8 to 30.4, against 15.2 to 22.8 overall; cyl is lower: the middle half 4 to 4, against 4 to 8 overall; wt is lower: the middle half 1.62 to  2.2, against 2.46 to 3.57 overall; and am is higher: the middle half 1 to 1, against 0 to 1 overall. Less strongly, 6 more variables set it apart as well.
```
