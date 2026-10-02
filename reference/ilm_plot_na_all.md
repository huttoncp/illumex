# Missing values by column

A bar per column. The first question to ask of an unfamiliar data set,
and the one
[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
then turns into advice.

## Usage

``` r
ilm_plot_na_all(
  data,
  by = NULL,
  cols = NULL,
  stat = c("p_na", "na", "n"),
  ...,
  cols_negate = FALSE
)
```

## Arguments

- data:

  A data frame.

- by:

  Optional grouping column(s), as a character vector – bars are drawn
  per group, which is how you see whether missingness is concentrated
  somewhere.

- cols:

  Columns to plot. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  `by` columns are never among them.

- stat:

  `"p_na"` (proportion missing), `"na"` (count missing) or `"n"` (count
  present).

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`. A `by` argument is never negated.

## Value

`NULL`, invisibly.

## See also

[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
for whether it matters,
[`ilm_profile_na()`](https://huttoncp.github.io/illumex/reference/ilm_profile_na.md)
for which columns go missing together.

## Examples

``` r
ilm_plot_na_all(airquality)

ilm_plot_na_all(airquality, by = "Month")
```
