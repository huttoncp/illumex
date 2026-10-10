# Write the cleaning as an R script

Turns the cleaning log into a script that replays every remedy, in
order, on the raw data: one plain call per step, with the step's key,
the check that called for it, its tier and the reason given as comments
above it. It does not run the checks again, so it replays the same way
under later versions of illumex; it needs only the remedy functions it
calls.

## Usage

``` r
ilm_cleaning_script(data, file = NULL)
```

## Arguments

- data:

  The cleaned data, carrying its log.

- file:

  A file to write the script to; `NULL` returns it only.

## Value

The script's lines, invisibly when written to a file.

## Details

A secret is never written: the step calls `ilm_secret("VAR")`, and a
comment says to set that environment variable first.

## Examples

``` r
d <- data.frame(x = rnorm(10), same = 1)
rem <- ilm_remedies(ilm_check_frame(d))
d2 <- ilm_apply_remedy(d, rem, "drop_cols/same", reason = "one value only")
#> ilm_apply_remedy(): ilm_drop_cols(data, "same")
#>   reason given: one value only
#>   constant: FAIL before, not found now
#>   frame check now: OK
ilm_cleaning_script(d2)
#> ## Cleaning script, written by illumex 0.0.8.9005 on 2026-10-10.
#> ## It replays 1 remedy, in order, on the data they were made for,
#> ## whose ilm_data_id() is 10x2:44d8314a2b. Read those data into `data` first.
#> library(illumex)
#> if (!identical(ilm_data_id(data), "10x2:44d8314a2b"))
#>   warning("these are not the data the cleaning was made on")
#> 
#> ## [1] drop_cols/same -- constant (FAIL), tier values
#> ## reason: one value only
#> data <- ilm_drop_cols(data, "same")
#> 
```
