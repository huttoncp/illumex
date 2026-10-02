# Pairwise plots

Every pair of columns at once, handling mixed numeric and categorical
columns rather than only numeric ones as a classic scatterplot matrix
does.

## Usage

``` r
ilm_plot_var_pairs(
  data,
  cols = NULL,
  by = NULL,
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

- cols:

  Columns to use. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- by:

  Optional grouping column.

- ...:

  Passed to
  [`tinyplot::tinypairs()`](https://grantmcdermott.com/tinyplot/man/tinyplot.data.frame.html),
  which needs tinyplot 0.7.0 or later. On an earlier tinyplot this is
  the one plot in the package that cannot be drawn, and it says so.

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

`NULL`, invisibly.

## See also

[`ilm_plot_scatter()`](https://huttoncp.github.io/illumex/reference/ilm_plot_scatter.md)
for one pair, `illume::ilm_check_collinearity()` for what a pairs plot
cannot show about a fit.

## Examples

``` r
ilm_plot_var_pairs(mtcars, cols = c("mpg", "wt", "hp"), by = "cyl")
```
