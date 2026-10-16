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

**Matching literally.** With `cols_fixed = TRUE`, a `cols` string read
as a pattern is matched as it is written, as a substring:
`cols = "wt.", cols_fixed = TRUE` takes `wt.kg` and `wt.lb` but not
`wt_2`. A column name still wins, and names or a predicate are
unaffected.

**Choosing rows.** A function that takes `subset` uses only the rows it
gives, before `cols`, `by` and everything else –
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
before the call, as an ordinary value. `subset` is one of:

- A **logical vector**, one value per row: `subset = d$age >= 18`. Rows
  where it is `NA` are left out, as in
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

- **Row positions**, positive whole numbers: `subset = 1:100`. A number
  on its own always means a position.

- **Named patterns**, one per column:
  `subset = c(site = "^north", arm = "drug")` keeps the rows where every
  named column matches; a missing value matches nothing.
  `subset_fixed = TRUE` matches them literally.

- A **random sample**,
  [`ilm_sample()`](https://huttoncp.github.io/illumex/reference/ilm_sample.md):
  `subset = ilm_sample(prop = 0.2, seed = 1)`, or whole groups with
  `by`.

- A **model**, for the rows it analysed: a fit from illume's
  `ilm_model()`, whose dropped rows are left out, or an
  `ilm_dag_model()`, whose adjustment sets can drop different rows, for
  the rows every set used. `data` must be the data the model was fitted
  to. A table of characteristics beside the model's estimates describes
  these rows: `ilm_describe_all(d, subset = fit)`.

`subset_negate = TRUE` takes the rows `subset` would not: the other
rows, the rows not sampled (a holdout), or the rows a model dropped –
the check on whether they differ from the rows it analysed. For a
logical `subset`, rows where it is `NA` stay out either way. A subset
that keeps no row is an error saying what it was. Results name rows by
the data's own row numbers, never by their place in the subset, so they
join back onto the data as they are.
[`ilm_subset()`](https://huttoncp.github.io/illumex/reference/ilm_subset.md)
returns the rows and columns themselves.

A result made from a subset, or from a choice of columns, says so above
what it prints – `119 of 600 rows (subset)`,
`297 of 300 rows (analysed by the model)` and
`Columns: 5 of 8 (excluded: id, name, date)` – and keeps what was chosen
in its attribute `"ilm_select"`, with, for a model, copies of what
identifies each fit (formula, rows given, rows dropped and any subset),
to check against it with
[`identical()`](https://rdrr.io/r/base/identical.html). A plot says the
same in a message.

A `by` argument takes column names.
[`ilm_outliers_all()`](https://huttoncp.github.io/illumex/reference/ilm_outliers_all.md)'s
`by` also takes a pattern or a predicate, as `cols` does; elsewhere a
pattern or a function given as `by` is an error. `by` is never negated.

## Examples

``` r
d <- data.frame(id = 1:5, score_a = rnorm(5), score_b = rnorm(5),
                label = letters[1:5])
ilm_outliers_all(d, cols = "^score_")
#> Columns: 2 of 4 (excluded: id, label)
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
ilm_outliers_all(d, cols = is.numeric)
#> Columns: 3 of 4 (excluded: label)
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
## leaving columns out
ilm_outliers_all(d, cols = "id", cols_negate = TRUE)
#> Columns: 2 of 4 (excluded: id, label)
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
ilm_outliers_all(d, cols = "^score_", cols_negate = TRUE)
#> Columns: 1 of 4 (excluded: score_a, score_b, label)
#> [1] row_id     variable   value      score      is_outlier
#> <0 rows> (or 0-length row.names)
ilm_outliers_all(d, cols = function(v) all(v == round(v)), cols_negate = TRUE)
#> Columns: 2 of 4 (excluded: id, label)
#>   row_id variable     value score is_outlier
#> 1      1  score_a -1.706430 3.513       TRUE
#> 2      4  score_a  1.954017 2.572       TRUE
## choosing rows
ilm_outliers_all(d, subset = d$id > 2)
#> 3 of 5 rows (subset)
#> [1] row_id     variable   value      score      is_outlier
#> <0 rows> (or 0-length row.names)
ilm_outliers_all(d, subset = c(label = "^[ab]$"), subset_negate = TRUE)
#> 3 of 5 rows (subset, negated)
#> [1] row_id     variable   value      score      is_outlier
#> <0 rows> (or 0-length row.names)
ilm_outliers_all(d, subset = ilm_sample(3, seed = 1))
#> 3 of 5 rows (subset)
#> [1] row_id     variable   value      score      is_outlier
#> <0 rows> (or 0-length row.names)
```
