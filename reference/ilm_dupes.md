# Duplicated rows only

A shorthand for `ilm_copies(data, ..., filter = "dupes")`.

## Usage

``` r
ilm_dupes(data, ..., na_last = TRUE)
```

## Arguments

- data:

  A data frame.

- ...:

  Columns defining a duplicate. All columns if none are given.

- na_last:

  Sort missing values last.

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
