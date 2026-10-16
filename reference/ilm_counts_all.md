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
