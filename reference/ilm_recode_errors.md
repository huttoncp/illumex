# Replace known-bad values with NA or another value

Sentinels such as 999, -1 or `"unknown"` mean missing in one column and
can be a legitimate measurement in another, which is why the replacement
can be restricted to particular rows and columns.

## Usage

``` r
ilm_recode_errors(
  data,
  errors,
  replacement = NA,
  subset = NULL,
  cols = NULL,
  ind = NULL,
  keep_all = TRUE,
  subset_negate = FALSE,
  subset_fixed = FALSE,
  cols_negate = FALSE,
  cols_fixed = FALSE
)
```

## Arguments

- data:

  A vector, data frame or matrix.

- errors:

  Values to recode.

- replacement:

  What to put in their place. `NA` by default.

- subset, subset_negate, subset_fixed:

  The rows whose cells are recoded (data frame or matrix input): a
  logical vector, row positions or named patterns, as in
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  A random sample
  ([`ilm_sample()`](https://huttoncp.github.io/illumex/reference/ilm_sample.md))
  makes no sense here and is refused.

- cols, cols_negate, cols_fixed:

  The columns whose cells are recoded (data frame or matrix input):
  names, positions, a pattern or a predicate, as in
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- ind:

  Restrict the replacement (vector input).

- keep_all:

  `TRUE` (the default) returns every row and column, with only the
  chosen cells recoded; `FALSE` returns just the chosen rows and
  columns, recoded.

## Value

An object of the same shape as `data`, or with `keep_all = FALSE` the
chosen rows and columns of it.

## Examples

``` r
ilm_recode_errors(c(1, 2, 999, -1), errors = c(999, -1))
#> [1]  1  2 NA NA
d <- data.frame(x = c(1, 999, 3), y = c(999, 2, 3), site = c("a", "b", "a"))
ilm_recode_errors(d, errors = 999, cols = "x")
#>    x   y site
#> 1  1 999    a
#> 2 NA   2    b
#> 3  3   3    a
## only at site a, and only there returned
ilm_recode_errors(d, errors = 999, subset = c(site = "^a$"), keep_all = FALSE)
#>   x  y site
#> 1 1 NA    a
#> 3 3  3    a
```
