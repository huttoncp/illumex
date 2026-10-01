# Density plot

Density plot

## Usage

``` r
ilm_plot_density(data, x, by = NULL, ..., facet = NULL, legend = NULL)
```

## Arguments

- data:

  A data frame.

- x:

  Name of the numeric column to plot.

- by:

  Optional grouping column, overlaying one histogram per level.

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

## Value

`NULL`, invisibly.

## See also

[`ilm_plot_histogram()`](https://huttoncp.github.io/illumex/reference/ilm_plot_histogram.md).

## Examples

``` r
ilm_plot_density(mtcars, "mpg", by = "cyl")
```
