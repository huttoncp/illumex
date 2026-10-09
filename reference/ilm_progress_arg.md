# Progress reporting in illumex and illume

Long-running functions take a `progress` argument. It defaults to
[`interactive()`](https://rdrr.io/r/base/interactive.html), so a bar
appears when someone is watching and nothing is written in a script, a
test or a knitted document.

## Details

The bar costs nothing worth measuring: on 2000 bootstrap replicates over
20,000 rows the loop took no longer with a bar than without one.

## Examples

``` r
d <- ilm_sim()
ilm_boot_ci(d, "score", R = 200, progress = FALSE)
#>   stat observed    lower    upper conf   R    ci_type   n
#> 1 mean 49.67677 49.26611 50.06166 0.95 200 percentile 900
```
