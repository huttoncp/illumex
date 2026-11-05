# Describe data whose rows sit in sampling clusters

Counts the clusters and the rows in each, and describes each variable
that is the same on every row of a cluster – a site's region, a school's
size – once per cluster rather than once per row, where a large cluster
would count many times over. The variables that vary within clusters are
named, for
[`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
to describe over the rows.

## Usage

``` r
ilm_describe_clusters(
  data,
  cluster,
  cols = NULL,
  digits = 3,
  ...,
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

- cluster:

  The name of the column that says which cluster each row is in.

- cols:

  The columns to consider; every column but `cluster` by default. See
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md)
  for the forms it takes.

- digits:

  Rounding for printing; the values are kept whole.

- ...:

  Passed to
  [`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
  for the variables described once per cluster: `gauss`, `probs` and the
  like.

- cols_negate, cols_fixed:

  How `cols` is read; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- subset, subset_negate, subset_fixed:

  Which rows; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  A model given as `subset` describes the clusters of the rows it
  analysed.

## Value

An `"ilm_describe_clusters"` object, a list of `clusters` (one row: the
cluster column, the number of clusters, of rows and of rows without a
cluster, and the rows per cluster: their mean, minimum, quartiles and
maximum), `cluster_level`
([`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
of the variables that belong to the cluster, one row per cluster, or
`NULL` when there are none), and the names of those variables
(`cluster_vars`) and of the variables that vary within a cluster
(`row_vars`).

## Details

A variable belongs to the cluster when no cluster has two different
values of it; a missing value on some of a cluster's rows does not count
against that, and the cluster takes its value from the rows that have
it. Rows without a cluster are left out, and counted.

## See also

[`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
for the rows.

## Examples

``` r
d <- ilm_sim(n_id = 30, n_period = 6)
ilm_describe_clusters(d, "id")
#> Clusters: 30 by `id`, 180 rows
#> Rows per cluster: 6 in every cluster
#> 
#> Described once per cluster (the same on every row of a cluster):
#>    variable       class obs  n na value
#> 1 consented     logical  30 30  0  TRUE
#> 2    cohort categorical  30 30  0  2024
#> 
#> Vary within clusters, so described over rows by ilm_describe_all(): date,
#> grp, site, flag, score, income, visits, claims, downtime, lab_value
```
