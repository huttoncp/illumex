# A random sample of rows, or of whole groups, for `subset`

Given as `subset` to any illumex function that takes it, `ilm_sample()`
draws the rows to use. Give `n` for a number of rows or `prop` for a
share of them. It samples in one of four ways:

## Usage

``` r
ilm_sample(
  n = NULL,
  prop = NULL,
  by = NULL,
  seed = NULL,
  within = NULL,
  min = 2L,
  report_by = NULL
)
```

## Arguments

- n:

  A number of rows (or of groups, with `by`) to draw: a whole number, at
  most the number there are. With `within`, the total to share out
  across the clusters; the floor `min` can raise it.

- prop:

  A share of rows (or of groups) to draw, strictly between 0 and 1; the
  count is rounded, and must come to at least 1. With `within`, the
  share of each cluster's rows, rounded within each.

- by:

  A column whose values are the groups to draw whole; or, for crossed
  factors, each factor named with its share, count or `"all"`:
  `c(rater = 0.3, item = 0.2)`. `NULL` draws rows. A missing value is a
  group, or a level, of its own.

- seed:

  An integer seed for the draw, or `NULL`.

- within:

  One column, or nested columns, whose sampling clusters are each
  sampled inside and all kept. A missing value is a cluster of its own.

- min:

  With `within`, the fewest rows any cluster keeps (all of a cluster
  with fewer). A whole number of at least 1; 2 by default.

- report_by:

  Grouping columns to report on after the draw: their levels and rows
  kept, and whether each pair is still connected. `NULL` reports
  nothing.

## Value

An object of class `"ilm_sample"`, which does nothing until it is given
as `subset`.

## Details

- **Rows**, the default: `n` rows, or a share `prop` of them.

- **Whole groups**, with `by`: `n` and `prop` count groups, every row of
  a group drawn is kept and the other groups are left out, so a grouped
  holdout (`subset_negate = TRUE`) never splits a group.

- **Crossed factors**, with `by` naming each factor and its share:
  `by = c(rater = 0.3, item = 0.2)` draws 30% of the raters and 20% of
  the items, each on its own, and keeps the rows whose rater and item
  were both drawn – a two-way design sampled as one. A factor takes a
  share between 0 and 1, a count of its levels (2 or more), or 1 (or
  `"all"`) to keep it whole, which is how a factor with few levels is
  kept. `n` and `prop` are left out, since each factor carries its own.

- **Inside every sampling cluster**, with `within` – a site, a school, a
  participant measured repeatedly: rows are drawn inside each cluster
  and every cluster is kept – the remedy when data are too large to fit
  but each cluster must stay in the model. The draw is proportional: a
  share `prop` of each cluster's rows, or `n` rows in all shared out in
  proportion to the clusters' sizes. Every cluster keeps at least `min`
  rows, or all of them when it has fewer than `min`. That floor makes a
  small cluster's share larger than a large one's: a row's chance of
  being kept is its cluster's rows kept over its cluster's rows, and the
  result keeps the rule, the smallest, median and largest of those
  shares, and what derives each row's share (`within`, `min` and `n` or
  `prop`), so a weighted analysis can be checked against an unweighted
  one.

`within` may name nested levels, coarsest first or in any order:
`within = c("school", "classroom")` draws inside each classroom, so
every classroom, and so every school, is kept. The levels must be
nested: each classroom in one school. A classroom id that repeats across
schools is refused, since the data cannot say whether it is a different
classroom in each school (give them unique ids, such as
`paste(school, classroom)`) or the same classroom across schools – a
crossed design, for which `within` is not meant: draw each factor with
`by = c(...)`, or rows with `ilm_sample(prop = )`. `by` and `within`
together are an error.

The draw follows the family's rule for random numbers: with a `seed`,
the same rows every time, and your own random stream is left as it was;
with `seed = NULL`, it draws from your stream as any R code does. The
result keeps the seed and the generator's kind.

A number given to `subset` on its own always means row positions:
`subset = 10` is row 10, and `subset = ilm_sample(10)` is ten rows
drawn.

**What a sample leaves.** Sampling rows can thin a grouping out: a rater
left with one row, or raters and items that no longer meet. With
`report_by = c("rater", "item")`, after any of the four ways, the result
keeps, in its `"ilm_select"` attribute, for each column the levels given
and kept and the fewest and median rows per kept level, and for each
pair whether their levels, joined where a kept row has both, are still
in one piece. A level left with fewer than 2 rows, or a pair split into
pieces, gives a warning – with counts, never the levels' values.

## See also

[ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md)
for `subset` and `cols`,
[`ilm_subset()`](https://huttoncp.github.io/illumex/reference/ilm_subset.md)
for the data itself, which with `within` also carries each row's chance
of being kept as the attribute `"ilm_inclusion"`.

## Examples

``` r
d <- ilm_sim()
ilm_describe(d, "score", subset = ilm_sample(prop = 0.5, seed = 1))
#> 450 of 900 rows (subset)
#>   obs   n na      sum   mean    sd    se    p0    p50  p100 p_zero dispersion
#> 1 450 450  0 22278.64 49.508 5.746 0.271 29.99 49.485 63.91      0         NA
#>   gauss gauss_note
#> 1     1           
## a grouped holdout: the ids not drawn
ilm_describe(d, "score", subset = ilm_sample(5, by = "id", seed = 1),
             subset_negate = TRUE)
#> 840 of 900 rows (subset, negated)
#>   obs   n na      sum   mean    sd    se    p0   p50  p100 p_zero dispersion
#> 1 840 840  0 41711.09 49.656 5.849 0.202 29.99 49.88 65.34      0         NA
#>   gauss gauss_note
#> 1     1           
## half the ids and every site: the rows of the ids drawn
ilm_describe(d, "score", subset = ilm_sample(by = c(id = 0.5, site = "all"), seed = 1))
#> 456 of 900 rows (subset)
#>   obs   n na      sum   mean    sd    se    p0    p50  p100 p_zero dispersion
#> 1 456 456  0 22383.94 49.088 5.915 0.277 29.99 48.685 64.38      0         NA
#>   gauss gauss_note
#> 1     1           
## a fifth of the rows, and what it leaves of each id and site
ilm_describe(d, "score", subset = ilm_sample(prop = 0.2, seed = 1,
                                             report_by = c("id", "site")))
#> Warning: after sampling, 21 of 72 id levels have fewer than 2 rows
#> 180 of 900 rows (subset)
#>   obs   n na     sum   mean    sd    se    p0  p50  p100 p_zero dispersion
#> 1 180 180  0 8896.72 49.426 5.642 0.421 33.84 49.1 63.91      0         NA
#>   gauss gauss_note
#> 1     1           
## a third of every site's rows, at least 2 from each
ilm_describe(d, "score", subset = ilm_sample(prop = 1/3, within = "site", seed = 1))
#> 298 of 900 rows (subset)
#>   obs   n na      sum   mean    sd   se    p0   p50  p100 p_zero dispersion
#> 1 298 298  0 14756.14 49.517 5.861 0.34 33.83 49.66 65.33      0         NA
#>   gauss gauss_note
#> 1     1           
```
