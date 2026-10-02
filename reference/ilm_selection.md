# How columns can be chosen

Wherever illumex takes a `cols` argument, it accepts any of four things.
All are ordinary values, so any of them can be held in a variable and
passed along – which a bare-name interface cannot.

## Details

- `NULL`, meaning every eligible column.

- A **character vector** of names: `c("age", "income")`. Every name must
  exist, and a typo is an error naming the columns that do exist.

- A **regular expression**, as a single string that is not itself a
  column name: `"^score_"` takes every column whose name starts with
  `score_`. Matching nothing is an error rather than a silent empty
  selection.

- A **predicate function** applied to each column: `is.numeric`, or
  `function(v) is.numeric(v) && !anyNA(v)`. A column on which the
  function fails counts as not matching.

The one ambiguity is a single string, which could be a name or a
pattern. A name wins: `cols = "score"` selects the column called `score`
even if `scores_2024` also exists. Anything that is not a column name is
read as a pattern.

**Leaving columns out.** With `cols_negate = TRUE`, `cols` says which
columns to leave out, and every other eligible column is used:
`cols = c("id", "site")` uses all but `id` and `site`;
`cols = "^score_"` all but the columns whose names start with `score_`;
`cols = is.numeric` all but the numeric columns, which is every column
of another type. The selection is made within the columns the function
can use (a numeric-only function leaves out text columns either way), so
`cols_negate = TRUE` takes exactly the eligible columns that
`cols_negate = FALSE` would not. A column on which a predicate fails
counts as not matching, so `cols_negate = TRUE` selects it.
`cols_negate = TRUE` needs `cols`, and leaving out every eligible column
is an error naming them.

A `by` argument takes column names.
[`ilm_outliers_all()`](https://huttoncp.github.io/illumex/reference/ilm_outliers_all.md)'s
`by` also takes a pattern or a predicate, as `cols` does; elsewhere a
pattern or a function given as `by` is an error. `by` is never negated.

## Examples

``` r
d <- data.frame(id = 1:5, score_a = rnorm(5), score_b = rnorm(5),
                label = letters[1:5])
ilm_outliers_all(d, cols = "^score_")
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
ilm_outliers_all(d, cols = is.numeric)
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
## leaving columns out
ilm_outliers_all(d, cols = "id", cols_negate = TRUE)
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
ilm_outliers_all(d, cols = "^score_", cols_negate = TRUE)
#> [1] row_id     variable   value      score      is_outlier
#> <0 rows> (or 0-length row.names)
ilm_outliers_all(d, cols = function(v) all(v == round(v)), cols_negate = TRUE)
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
```
