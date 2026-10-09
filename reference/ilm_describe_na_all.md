# Missingness in every variable

Sorted with the most missing first, since those are the variables worth
looking at.

## Usage

``` r
ilm_describe_na_all(
  data,
  by = NULL,
  cols = NULL,
  digits = 4,
  sort = TRUE,
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

  Optional grouping columns.

- cols:

  Columns to describe. A character vector of names, a regular
  expression, a predicate function such as `is.numeric`, or `NULL` for
  all of them – see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  `by` columns are never among them.

- digits:

  Rounding for `p_na`.

- sort:

  Sort by proportion missing, descending.

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`.

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

A data frame with `variable`, `obs`, `n`, `na` and `p_na`.

## Examples

``` r
ilm_describe_na_all(ilm_sim())
#>     variable obs   n  na p_na
#> 1  lab_value 900 792 108 0.12
#> 2     claims 900 900   0 0.00
#> 3     cohort 900 900   0 0.00
#> 4  consented 900 900   0 0.00
#> 5       date 900 900   0 0.00
#> 6   downtime 900 900   0 0.00
#> 7       flag 900 900   0 0.00
#> 8        grp 900 900   0 0.00
#> 9         id 900 900   0 0.00
#> 10    income 900 900   0 0.00
#> 11     score 900 900   0 0.00
#> 12      site 900 900   0 0.00
#> 13    visits 900 900   0 0.00
```
