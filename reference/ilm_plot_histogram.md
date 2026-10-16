# Histogram

Histogram

## Usage

``` r
ilm_plot_histogram(
  data,
  x,
  by = NULL,
  breaks = "Sturges",
  ...,
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

- x:

  Name of the numeric column to plot.

- by:

  Optional grouping column, overlaying one histogram per level.

- breaks:

  Passed to
  [`tinyplot::type_histogram()`](https://grantmcdermott.com/tinyplot/man/type_histogram.html).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

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

[`ilm_plot()`](https://huttoncp.github.io/illumex/reference/ilm_plot.md),
which picks a geometry for you.

## Examples

``` r
ilm_plot_histogram(mtcars, "mpg")

ilm_plot_histogram(mtcars, "mpg", by = "cyl")

ilm_plot_histogram(mtcars, "mpg", facet = "am")
```
