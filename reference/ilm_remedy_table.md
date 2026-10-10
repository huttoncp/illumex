# A table of remedies of one's own

For remedies that come from somewhere other than illumex's checks – a
rule of one's own, another package's diagnostics – so that they are
listed, applied and logged as illumex's own are. A generic: the method
for a data frame is here, and illume has one for fitted models.

## Usage

``` r
ilm_remedy_table(object, ...)

# S3 method for class 'data.frame'
ilm_remedy_table(object, check, status, tier, remedy, change = NULL, key, ...)

# S3 method for class 'ilm_remedies'
c(...)
```

## Arguments

- object:

  The target: the data frame the remedies would change.

- ...:

  For [`c()`](https://rdrr.io/r/base/c.html): remedy tables for the same
  target.

- check:

  Character: the name of the check each remedy answers.

- status:

  Character: what the check found, `"WARN"` or `"FAIL"`.

- tier:

  Character: `"representation"`, `"values"` or `"rows"`, as described in
  [`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md).

- remedy:

  Character: the remedy in words, a sentence a person can act on.

- change:

  A list with one element per remedy: a call that makes it, with the
  data written as `data` (`quote(my_fix(data, "x"))`), or `NULL` for a
  remedy made by hand.

- key:

  Character: each remedy's key, `<kind>/<columns>`; see
  [`ilm_remedy_key()`](https://huttoncp.github.io/illumex/reference/ilm_remedy_key.md).

## Value

A data frame of class `"ilm_remedies"`, as from
[`ilm_remedies()`](https://huttoncp.github.io/illumex/reference/ilm_remedies.md).

## Details

[`c()`](https://rdrr.io/r/base/c.html) combines tables for the same
target into one, numbered afresh: a change several tables name (the same
key) is listed once, with every check that named it, and where two
tables put it in different tiers it takes the more cautious. Tables for
different targets are not combined.

## Examples

``` r
d <- data.frame(id = 1:5, temp = c(20, 21, 70, 22, 68))
rem <- ilm_remedy_table(d, check = "my_range", status = "WARN",
                        tier = "values", key = "set_missing/temp",
                        remedy = "Set temperatures above 50 to missing.",
                        change = list(quote(within(data, temp[temp > 50] <- NA))))
rem
#> 1 remedy
#> 
#> [1] values -- my_range (WARN)   key: set_missing/temp
#>     Set temperatures above 50 to missing.
#>     change: within(data, temp[temp > 50] <- NA)
#> 
#> Apply one with ilm_apply_remedy(data, <this list>, id or key, reason =
#> "..."). Representation reads the same values correctly; values changes
#> values, sets them missing or removes columns; rows drops or excludes rows,
#> so apply one of those only by choice.
ilm_apply_remedy(d, rem, "set_missing/temp", reason = "out of range for the site")
#> ilm_apply_remedy(): within(data, temp[temp > 50] <- NA)
#>   reason given: out of range for the site
#>   my_range: WARN before; run its check again on the result
#>   id temp
#> 1  1   20
#> 2  2   21
#> 3  3   NA
#> 4  4   22
#> 5  5   NA
```
