# Group means or medians with an error bar

A summary per group with a measure of spread around it.

## Usage

``` r
ilm_plot_stat_error(
  data,
  y,
  x,
  by = NULL,
  stat = c("mean", "median"),
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

  Name of the numeric column to summarise.

- x:

  Name of the column defining the groups.

- by:

  Optional secondary grouping column.

- stat:

  `"mean"` with its standard error, or `"median"` with the quartiles.

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

## Details

The two `stat` options show different things and are not
interchangeable. `"mean"` draws the standard error, which is about where
the *mean* is; `"median"` draws the quartiles, which is about where the
*data* are. The first shrinks as the sample grows and the second does
not.

Overlapping error bars are a poor test of a difference – they are
conservative and lossy.
[`ilm_boot_diff()`](https://huttoncp.github.io/illumex/reference/ilm_boot_diff.md)
gives the difference itself with its own interval.

## See also

[`ilm_boot_diff()`](https://huttoncp.github.io/illumex/reference/ilm_boot_diff.md)
for the comparison this plot invites.

## Examples

``` r
ilm_plot_stat_error(mtcars, "mpg", "cyl")
```
