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

[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
for whether it matters,
[`ilm_profile_na()`](https://huttoncp.github.io/illumex/reference/ilm_profile_na.md)
for which columns go missing together.

## Examples

``` r
ilm_plot_na_all(airquality)

ilm_plot_na_all(airquality, by = "Month")
```
