# Find copied or duplicated rows

Name the columns that should identify one row, such as an id and a date,
and run it before and after a join: a join that went wrong shows up as
copies. `ilm_copies(data, filter = "first")` keeps one of each.

## Usage

``` r
ilm_copies(
  data,
  ...,
  filter = c("all", "dupes", "first", "last", "unique"),
  na_last = TRUE,
  sort_by = TRUE
)
```

## Arguments

- data:

  A data frame.

- ...:

  Columns defining a duplicate. All columns if none are given.

- filter:

  `"all"` appends `copy_number` and `n_copies`; `"dupes"` keeps only
  repeated rows; `"first"`, `"last"` and `"unique"` filter rows.

- na_last:

  Sort missing values last.

- sort_by:

  Sort the result by the key columns.

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
#> 1 2 y
```
