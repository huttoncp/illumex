# Find copied or duplicated rows

Name the columns that should identify one row, such as an id and a date,
and run it before and after a join: a join that went wrong shows up as
copies. `ilm_copies(data, filter = "first")` keeps one of each.

## Usage

``` r
ilm_copies(
  data,
  cols = NULL,
  filter = c("all", "dupes", "first", "last", "unique"),
  na_last = TRUE,
  sort_by = TRUE,
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

- filter:

  `"all"` appends `copy_number` and `n_copies`; `"dupes"` keeps only
  repeated rows; `"first"`, `"last"` and `"unique"` filter rows.

- na_last:

  Sort missing values last.

- sort_by:

  Sort the result by the key columns.

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

A data frame; the columns depend on `filter`. With `"all"` or `"dupes"`,
a message says how many rows are copies and gives the call that keeps
one of each; [`suppressMessages()`](https://rdrr.io/r/base/message.html)
silences it.

## See also

[`ilm_dupes()`](https://huttoncp.github.io/illumex/reference/ilm_dupes.md)
for the common case.

## Examples

``` r
d <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
ilm_copies(d)
#> 2 of 3 rows share their values with another row; ilm_copies(d, filter = "first") keeps one of each, leaving 2.
#>   a b copy_number n_copies
#> 1 1 x           1        2
#> 2 1 x           2        2
#> 3 2 y           1        1
ilm_copies(d, filter = "unique")
#>   a b
#> 3 2 y
```
