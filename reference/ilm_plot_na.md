# Missing values in one column, across groups

Where
[`ilm_plot_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_na_all.md)
compares columns, this compares groups within one column – which is how
a pattern in *who* is missing shows itself.

## Usage

``` r
ilm_plot_na(
  data,
  x,
  by = NULL,
  stat = c("p_na", "na", "n"),
  ...,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame.

- x:

  Name of the column whose missingness to show.

- by:

  Grouping column(s) to split by, as a character vector. Required:
  without it there is only one bar, which is
  [`ilm_plot_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_na_all.md)'s
  job.

- stat:

  `"p_na"`, `"na"` or `"n"`.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

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

[`ilm_plot_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_na_all.md),
[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md).

## Examples

``` r
ilm_plot_na(airquality, "Ozone", by = "Month")
```
