# Check the data frame as a whole

The problems
[`ilm_frame_issues()`](https://huttoncp.github.io/illumex/reference/ilm_frame_issues.md)
finds – constant and identifier-like columns, duplicated and collinear
ones, categorical columns that carry the same grouping or one inside
another, and columns that are an exact combination of others – as a
check, with remedies that can be applied.

## Usage

``` r
ilm_check_frame(data, cor_cut = 0.999, v_cut = 0.95, ...)
```

## Arguments

- data:

  A data frame.

- cor_cut:

  Absolute correlation at or above which a numeric pair is reported as
  collinear.

- v_cut:

  Cramér's V at or above which a pair of categorical columns is reported
  as redundant.

- ...:

  Unused.

## Value

An object of class `"ilm_data_check"`; see
[`ilm_check_data()`](https://huttoncp.github.io/illumex/reference/ilm_check_data.md).

## Details

A constant column, the second of a duplicated or aliased pair, and a
column that is an exact combination of others can be left out
([`ilm_drop_cols()`](https://huttoncp.github.io/illumex/reference/ilm_drop_cols.md),
tier `values`). Which of two collinear or redundant columns to keep, and
whether an identifier stays as a key, are decisions about what the data
mean, so those remedies are made by hand. A nested pair is expected of
grouping factors and is reported as OK.

## Examples

``` r
d <- data.frame(a = rnorm(20), b = rnorm(20), same = 1)
d$total <- d$a + d$b
chk <- ilm_check_frame(d)
chk
#> <ilm_data_check> frame: FAIL
#>   2 findings about the frame as a whole.
#>   FAIL constant  same: zero variance; breaks the model matrix
#>   FAIL rank_deficient  total ~ a + b: total is an exact linear combination of a and b
#> ilm_remedies() lists 2 remedies.
ilm_remedies(chk)
#> 2 remedies
#> 
#> [1] values -- constant (FAIL)   key: drop_cols/same
#>     Leave out same: a column with one value cannot explain anything.
#>     change: ilm_drop_cols(data, "same")
#> 
#> [2] values -- rank_deficient (FAIL)   key: drop_cols/total
#>     Leave out total, an exact combination of a and b.
#>     change: ilm_drop_cols(data, "total")
#> 
#> Apply one with ilm_apply_remedy(data, <this list>, id or key, reason =
#> "..."). Representation reads the same values correctly; values changes
#> values, sets them missing or removes columns; rows drops or excludes rows,
#> so apply one of those only by choice.
```
