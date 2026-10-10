# Leave columns out

A remedy: returns the data without the columns named. Written into
cleaning scripts by
[`ilm_check_frame()`](https://huttoncp.github.io/illumex/reference/ilm_check_frame.md)
and the other checks, and usable by hand.

## Usage

``` r
ilm_drop_cols(data, cols)
```

## Arguments

- data:

  A data frame.

- cols:

  Names of the columns to leave out.

## Value

The data frame without them.

## Examples

``` r
ilm_drop_cols(data.frame(a = 1:3, b = 4:6), "b")
#>   a
#> 1 1
#> 2 2
#> 3 3
```
