# Missingness in one variable

Missingness in one variable

## Usage

``` r
ilm_describe_na(
  data,
  y = NULL,
  by = NULL,
  digits = 4,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame, or a vector when `y` is `NULL`.

- y:

  Name of the column.

- by:

  Optional grouping columns.

- digits:

  Rounding for `p_na`.

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

A data frame with `obs`, `n`, `na` and `p_na`.

## See also

[`ilm_describe_na_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_na_all.md),
[`ilm_plot_missing()`](https://huttoncp.github.io/illumex/reference/ilm_plot_missing.md).

## Examples

``` r
ilm_describe_na(ilm_sim(), "lab_value")
#>   obs   n  na p_na
#> 1 900 792 108 0.12
```
