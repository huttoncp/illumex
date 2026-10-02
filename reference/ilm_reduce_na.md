# Reduce a data frame's missingness pattern to a few dimensions

The missingness counterpart to
[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md).
Builds a present/missing marker for every column, drops any whose
missingness never varies, and reduces the markers – which, being
two-level categorical variables, always takes the MCA route. The
dimensions that come back describe which columns tend to go missing
*together*, which is what separates a block of variables lost to one
skipped section from values that went missing independently.

## Usage

``` r
ilm_reduce_na(
  data,
  cols = NULL,
  ndim = 5,
  cols_negate = FALSE,
  cols_fixed = FALSE,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame.

- cols:

  Columns to use. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).

- ndim:

  Number of dimensions to keep.

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`. A `by` argument is never negated.

- cols_fixed:

  If `TRUE`, a `cols` string read as a pattern is matched literally, as
  a substring (as `grepl(fixed = TRUE)` does); a column name still wins.
  It changes nothing when `cols` is names or a predicate.

- subset:

  Which rows to use, before anything else: a logical vector (one value
  per row; `NA` is left out), row positions, named patterns
  (`c(site = "^north")`), or
  [`ilm_sample()`](https://huttoncp.github.io/illumex/reference/ilm_sample.md).
  See
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  Results keep the data's own row numbers.

- subset_negate:

  If `TRUE`, the rows `subset` would not take: the other rows, or the
  rows not sampled (a holdout). With a logical `subset`, rows where it
  is `NA` stay out either way.

- subset_fixed:

  If `TRUE`, `subset`'s patterns are matched literally, as substrings.
  It changes nothing for a logical, positions or a sample.

## Value

An object of class `"ilm_reduce_na"`, which is also an `"ilm_reduce"` –
see
[`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md)
for the shared structure. In `var_contrib`, `sqload` means how strongly
a column's *missingness* relates to a dimension, not its values.

## See also

[`ilm_profile_na()`](https://huttoncp.github.io/illumex/reference/ilm_profile_na.md)
for the whole pipeline,
[`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
for whether any of it matters to your model.

## Examples

``` r
r <- ilm_reduce_na(airquality)
#> ilm_reduce_na(): dropping column(s) whose missingness never varies (always or never missing): Wind, Temp, Month, Day
r
#> <ilm_reduce_na> method = mca, n = 153, 2 dimension(s) retained
#>   first 2 dimension(s) explain 100.0% of the variance
#> 
#>   strongest variable per dimension (squared loading)
#>     dim 1   Ozone                    0.511
#>     dim 2   Ozone                    0.489
```
