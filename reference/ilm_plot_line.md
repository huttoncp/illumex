# Line plot

Line plot

## Usage

``` r
ilm_plot_line(
  data,
  y,
  x,
  by = NULL,
  ...,
  pch = NULL,
  facet = NULL,
  legend = NULL
)
```

## Arguments

- data:

  A data frame.

- y, x:

  Column names; `x` is usually a date or a sequence.

- by:

  Optional grouping column.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

- pch:

  Plotting character. Takes a NAME as well as a number:
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

## Value

`NULL`, invisibly.

## See also

`illume::ilm_plot_acf()` for what a line plot of residuals cannot show.

## Examples

``` r
d <- data.frame(t = 1:40, v = cumsum(rnorm(40)))
ilm_plot_line(d, "v", "t")
```
