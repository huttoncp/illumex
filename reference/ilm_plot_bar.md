# Bar plot

Bar plot

## Usage

``` r
ilm_plot_bar(data, x, by = NULL, ..., facet = NULL, legend = NULL)
```

## Arguments

- data:

  A data frame.

- x:

  Name of the categorical column.

- by:

  Optional grouping column.

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

[`ilm_counts()`](https://huttoncp.github.io/illumex/reference/ilm_counts.md)
for the same information as a table.

## Examples

``` r
ilm_plot_bar(mtcars, "cyl", by = "am")
```
