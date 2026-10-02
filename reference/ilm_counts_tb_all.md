# Most and least frequent values for every column

Most and least frequent values for every column

## Usage

``` r
ilm_counts_tb_all(
  data,
  cols = NULL,
  n = 10L,
  na.rm = TRUE,
  cols_negate = FALSE
)
```

## Arguments

- data:

  A data frame.

- cols:

  Columns to count. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- n:

  How many values from each end.

- na.rm:

  Drop missing values before counting.

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`. A `by` argument is never negated.

## Value

A data frame with `variable` and the top/bottom columns.

## Examples

``` r
ilm_counts_tb_all(ilm_sim()[, c("grp", "site")], n = 2)
#>   variable top_value top_n bot_value bot_n
#> 1      grp     alpha   404     delta     3
#> 2      grp      beta   329     gamma   164
#> 3     site     North   277    North     46
#> 4     site     South   265              46
```
