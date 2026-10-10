# Check a data frame for problems, all at once

Runs every data check (or those named) and gathers what they found. Each
check says what it saw and how sure it is, and names remedies as code;
list them with
[`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md)
and make one with
[`ilm_apply_remedy()`](https://huttoncp.github.io/illumex/reference/ilm_apply_remedy.md).
Nothing in the data changes until a remedy is applied.

## Usage

``` r
ilm_check_data(data, checks = NULL, ...)
```

## Arguments

- data:

  A data frame.

- checks:

  Names of the checks to run; `NULL` runs them all.

- ...:

  Passed to every check.

## Value

An object of class `"ilm_data_checks"`: `checks`, the list of each
check's result, `summary`, a table of each check's status and headline,
and `target_id`, the data's
[`ilm_data_id()`](https://huttoncp.github.io/illumex/reference/ilm_data_id.md).

## Details

The checks so far:

- `frame`:

  [`ilm_check_frame()`](https://huttoncp.github.io/illumex/reference/ilm_check_frame.md):
  constant, identifier-like, duplicated, collinear, aliased, nested and
  redundant columns, and columns that are an exact combination of
  others.

## See also

[`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md),
[`ilm_apply_remedy()`](https://huttoncp.github.io/illumex/reference/ilm_apply_remedy.md),
[`ilm_cleaning_script()`](https://huttoncp.github.io/illumex/reference/ilm_cleaning_script.md).

## Examples

``` r
d <- data.frame(id = 1:30, x = rnorm(30), same = 1)
d$x2 <- d$x
chk <- ilm_check_data(d)
chk
#> <ilm_data_checks> 1 check
#>   FAIL frame
#>        3 findings about the frame as a whole.
#> ilm_remedies() lists the remedies (3 before duplicates are merged).
ilm_remedies(chk)
#> 3 remedies
#> 
#> [1] values -- id_like (WARN)   key: as_key/id
#>     Keep id to identify or join rows, and leave it out of the predictors of
#>     any model or clustering.
#>     by hand: a decision about what the data mean
#> 
#> [2] values -- constant (FAIL)   key: drop_cols/same
#>     Leave out same: a column with one value cannot explain anything.
#>     change: ilm_drop_cols(data, "same")
#> 
#> [3] values -- duplicate_columns (FAIL)   key: drop_cols/x2
#>     Leave out x2, an exact copy of x.
#>     change: ilm_drop_cols(data, "x2")
#> 
#> Apply one with ilm_apply_remedy(data, <this list>, id or key, reason =
#> "..."). Representation reads the same values correctly; values changes
#> values, sets them missing or removes columns; rows drops or excludes rows,
#> so apply one of those only by choice.
```
