# The cleaning log

Every remedy applied to reach these data, in order: its key, the check
and status that called for it, its tier, the remedy and the change made,
the reason given, any secret's environment variable (never its value),
and the data's
[`ilm_data_id()`](https://huttoncp.github.io/illumex/reference/ilm_data_id.md)
before and after.

## Usage

``` r
ilm_cleaning_log(data)
```

## Arguments

- data:

  A data frame that remedies were applied to.

## Value

A data frame, one row per remedy, or `NULL` with a message when there is
no log.

## Details

The log travels with the data as an attribute. Some operations drop
attributes (base subsetting, some joins); the script from
[`ilm_cleaning_script()`](https://huttoncp.github.io/illumex/reference/ilm_cleaning_script.md)
is the durable record, so write it when the cleaning is done.

## Examples

``` r
d <- data.frame(x = rnorm(10), same = 1)
rem <- ilm_remedies(ilm_check_frame(d))
d2 <- ilm_apply_remedy(d, rem, "drop_cols/same", reason = "one value only")
#> ilm_apply_remedy(): ilm_drop_cols(data, "same")
#>   reason given: one value only
#>   constant: FAIL before, not found now
#>   frame check now: OK
ilm_cleaning_log(d2)
#>   step                time            key    check status   tier
#> 1    1 2026-10-10 16:41:00 drop_cols/same constant   FAIL values
#>                                                             remedy
#> 1 Leave out same: a column with one value cannot explain anything.
#>                        change         reason secrets       id_before
#> 1 ilm_drop_cols(data, "same") one value only         10x2:afc434ed4f
#>          id_after
#> 1 10x1:b3f162ec33
```
