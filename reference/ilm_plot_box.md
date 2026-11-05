# Boxplot

The whiskers use the same 1.5 interquartile-range fence that
[`ilm_outliers()`](https://huttoncp.github.io/illumex/reference/ilm_outliers.md)
scores against, so the points beyond them are the values that function
flags under `method = "iqr"`.

## Usage

``` r
ilm_plot_box(
  data,
  y,
  x = NULL,
  by = NULL,
  ...,
  pch = NULL,
  facet = NULL,
  legend = NULL,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame.

- y:

  Name of the numeric column.

- x:

  Optional categorical column to split by.

- by:

  Optional grouping column.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

- pch:

  Plotting character. Takes a name as well as a number:
  `"filled circle"` is 16, and every code from 0 to 25 has one. Case,
  spaces, underscores and hyphens are ignored. A single character is
  drawn literally, so `pch = "x"` is still the letter x.

- facet:

  Optional column to draw in panels, one per level, named as a string
  the way `by` is: `facet = "site"`.

- legend:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html),
  in any form it takes. With `by`, the legend is titled with the `by`
  column's name unless you give a title.

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

[`ilm_outliers()`](https://huttoncp.github.io/illumex/reference/ilm_outliers.md),
[`ilm_plot_violin()`](https://huttoncp.github.io/illumex/reference/ilm_plot_violin.md).

## Examples

``` r
ilm_plot_box(mtcars, "mpg", x = "cyl")
```
