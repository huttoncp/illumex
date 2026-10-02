# Frequency counts for every column

Values are stacked across columns of different classes, so they are
rendered as character; the counts stay integer.

## Usage

``` r
ilm_counts_all(
  data,
  by = NULL,
  cols = NULL,
  n = "all",
  order = c("d", "a", "i"),
  na.rm = TRUE,
  cols_negate = FALSE
)
```

## Arguments

- data:

  A data frame.

- by:

  Optional grouping columns.

- cols:

  Columns to count. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  `by` columns are never among them.

- n:

  Number of rows to return, or `"all"`.

- order:

  `"d"` by count descending, `"a"` by count ascending, or `"i"` by the
  value itself (factor level order, not label order).

- na.rm:

  Drop missing values before counting.

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`. A `by` argument is never negated.

## Value

A data frame with `variable`, `value` and `n`.

## Examples

``` r
ilm_counts_all(ilm_sim()[, c("grp", "site")], n = 2)
#>   variable value   n
#> 1      grp alpha 404
#> 2      grp  beta 329
#> 3     site North 277
#> 4     site South 265
```
