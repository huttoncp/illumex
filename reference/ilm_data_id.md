# A data frame's fingerprint

A short string that identifies a data frame's contents: its size, its
column names and classes, and every value in its place. It changes when
any value changes, when rows or columns are put in another order, and
when two edits would offset each other in a sum; row names do not enter
it.
[`ilm_apply_remedy()`](https://huttoncp.github.io/illumex/reference/ilm_apply_remedy.md)
uses it to refuse a remedy listed for other data, and the cleaning log
records it before and after every remedy.

## Usage

``` r
ilm_data_id(data)
```

## Arguments

- data:

  A data frame.

## Value

A single string, `"<rows>x<columns>:<hash>"`.

## Details

It is computed exactly in base R and is the same on every platform and
in every session. It is not a cryptographic hash.

## Examples

``` r
d <- data.frame(a = 1:3, b = c("x", "y", "z"))
ilm_data_id(d)
#> [1] "3x2:5e4ed90f98"
ilm_data_id(d[3:1, ])
#> [1] "3x2:18b3928d73"
```
