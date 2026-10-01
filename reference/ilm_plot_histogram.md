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
  legend = NULL
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
