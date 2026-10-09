# Duplicated rows only

A shorthand for `ilm_copies(data, cols, filter = "dupes")`.

## Usage

``` r
ilm_dupes(
  data,
  cols = NULL,
  na_last = TRUE,
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

- cols:

  The columns that define a copy: names, a pattern or a predicate, as in
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md);
  all columns if `NULL`.

- na_last:

  Sort missing values last.

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

A data frame of repeated rows with `n_copies` appended, and the same
message as
[`ilm_copies()`](https://huttoncp.github.io/illumex/reference/ilm_copies.md)
when there are any.

## Examples

``` r
ilm_dupes(data.frame(a = c(1, 1, 2), b = c("x", "x", "y")))
#> 2 of 3 rows share their values with another row; ilm_copies(data, filter = "first") keeps one of each, leaving 2.
#>   a b n_copies
#> 1 1 x        2
#> 2 1 x        2
```
