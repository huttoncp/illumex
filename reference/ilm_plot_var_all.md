# Plot every column of a data frame

[`ilm_plot_var()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var.md)
once per column, arranged in a grid.

## Usage

``` r
ilm_plot_var_all(
  data,
  var2 = NULL,
  by = NULL,
  cols = NULL,
  nrow = NULL,
  ncol = NULL,
  verbose = FALSE,
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

- var2:

  Optional name of a second column.

- by:

  Optional grouping column.

- cols:

  Columns to use. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- nrow, ncol:

  Panel grid. Default is as square as it goes.

- verbose:

  Say which plot was chosen and why.

- ...:

  Passed to the underlying plot function.

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

[`ilm_plot_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_all.md),
[`ilm_plot_var_pairs()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var_pairs.md).

## Examples

``` r
ilm_plot_var_all(mtcars, cols = c("mpg", "cyl", "wt", "gear"))
```
