# Remedies for what a check found

Lists the remedies for every finding of a check that is not `"OK"`, each
written out as the change that makes it, so that it can be applied with
[`ilm_apply_remedy()`](https://huttoncp.github.io/illumex/reference/ilm_apply_remedy.md)
rather than retyped. A generic: illumex gives it methods for its data
checks
([`ilm_check_data()`](https://huttoncp.github.io/illumex/reference/ilm_check_data.md)
and the `ilm_check_*()` functions), and illume for fitted models.

## Usage

``` r
ilm_remedies(object, ...)

# S3 method for class 'ilm_data_check'
ilm_remedies(object, ...)

# S3 method for class 'ilm_data_checks'
ilm_remedies(object, ...)

# S3 method for class 'ilm_remedies'
print(x, ...)
```

## Arguments

- object:

  A result whose package gives `ilm_remedies()` a method: an
  [`ilm_check_data()`](https://huttoncp.github.io/illumex/reference/ilm_check_data.md)
  result or one `ilm_check_*()` result.

- ...:

  Arguments for methods.

- x:

  An `"ilm_remedies"` table.

## Value

A data frame of class `"ilm_remedies"`, one row per remedy, with `id`,
`key`, the `check` (or checks) it answers and its `status`, the `tier`,
the `remedy` in words, and the `change` that makes it, as code, or `""`
when it is made by hand. An empty table when every check is OK.

## Details

Every remedy has a `tier`, which says what applying it would change,
from the least to the most consequential. For data:

- `representation`:

  The same values, read correctly: text parsed as numbers, a header set,
  leading zeros restored.

- `values`:

  Values changed or set missing, or columns removed: a unit converted, a
  sentinel set to `NA`, levels merged.

- `rows`:

  Rows dropped or excluded.

Some remedies can only be made by hand – which of two columns to keep is
a question about what they mean – and those have no `change`. A remedy
is a candidate, not a cure: apply it, and read the check again.

Each remedy also has a `key` that names the change (`drop_cols/id`,
`convert/temp:degF>degC`) and stays the same from one version of the
package to the next, so a cleaning script can name the remedy it
applied.

## See also

[`ilm_apply_remedy()`](https://huttoncp.github.io/illumex/reference/ilm_apply_remedy.md)
to make one,
[`ilm_remedy_table()`](https://huttoncp.github.io/illumex/reference/ilm_remedy_table.md)
to build a table of one's own.

## Examples

``` r
d <- data.frame(x = rnorm(20), same = 1, y = rnorm(20))
ilm_remedies(ilm_check_frame(d))
#> 1 remedy
#> 
#> [1] values -- constant (FAIL)   key: drop_cols/same
#>     Leave out same: a column with one value cannot explain anything.
#>     change: ilm_drop_cols(data, "same")
#> 
#> Apply one with ilm_apply_remedy(data, <this list>, id or key, reason =
#> "..."). Representation reads the same values correctly; values changes
#> values, sets them missing or removes columns; rows drops or excludes rows,
#> so apply one of those only by choice.
```
