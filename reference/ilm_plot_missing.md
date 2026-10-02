# Proportion missing, by variable

Proportion missing, by variable

## Usage

``` r
ilm_plot_missing(
  data,
  ...,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame.

- ...:

  Passed to
  [`graphics::barplot()`](https://rdrr.io/r/graphics/barplot.html).

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

Invisibly, the named vector of proportions.

## See also

[`ilm_describe_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_na_all.md)
for the same information as a table.

## Examples

``` r
ilm_plot_missing(ilm_sim())
```
