# Describe every column of a data frame

Applies
[`ilm_describe()`](https://huttoncp.github.io/illumex/reference/ilm_describe.md)
to each column and returns one table per class, so columns with
different summaries are not forced into a common shape.

## Usage

``` r
ilm_describe_all(
  data,
  by = NULL,
  cols = NULL,
  digits = 3,
  gauss = c("index", "ks_d", "both", "none"),
  probs = c(0, 0.5, 1),
  skew = FALSE,
  kurt = FALSE,
  dispersion = TRUE,
  class = "all",
  rare_n = 5L,
  cap = 0.06,
  min_n = 20L,
  smd = FALSE,
  cols_negate = FALSE,
  cols_fixed = FALSE,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame, or a vector when `y` is `NULL`.

- by:

  Optional character vector of grouping columns.

- cols:

  Columns to describe. A character vector of names, a regular
  expression, a predicate function such as `is.numeric`, or `NULL` for
  all of them – see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  `by` columns are never among them, and the choice is made among the
  columns `class` allows.

- digits:

  Rounding for numeric columns, quantiles included.

- gauss:

  One of `"index"` (the 0-1 agreement index), `"ks_d"` (the raw
  Kolmogorov distance behind it), `"both"`, or `"none"`.

- probs:

  Quantiles to report, as proportions. `c(0, 0.5, 1)` gives `p0`, `p50`
  and `p100`; any vector works and the column names follow it.

- skew, kurt:

  Add skewness and excess kurtosis: type 2, as SAS, SPSS and Excel print
  them (Joanes and Gill 1998), 0 on average for normal data. The same
  kurtosis decides `gauss_note`'s "heavy-tailed" and "light-tailed".
  When `gauss_note` says skewed, the median and quartiles describe the
  variable better than the mean and SD: `probs = c(0.25, 0.5, 0.75)`.

- dispersion:

  Add the variance-to-mean ratio for non-negative integer variables.
  Meaningful only if you intend to model the variable as a count.

- class:

  `"all"`, or one or more of `"numeric"`, `"categorical"`, `"logical"`,
  `"time"`.

- rare_n:

  Levels with fewer observations than this count as rare.

- cap, min_n:

  Control the `gauss` index; see
  [`ilm_gauss_check()`](https://huttoncp.github.io/illumex/reference/ilm_gauss_check.md).

- smd:

  `FALSE` (the default), `TRUE`, or the name of a group. With `by`, adds
  `smd`: each group's standardised difference from a reference group,
  the first group for `TRUE` or the one named. A number's is the
  difference in means over the square root of the average of the two
  groups' variances (Austin 2009); a binary's, the difference in
  proportions over the square root of the average of their p(1 - p); a
  category of more than two levels, Yang and Dalton's (2012)
  multivariate difference, which has no sign. A date is compared as a
  number. Each group's uses its non-missing values. The reference
  group's own row has none.

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

A named list of data frames, one per class present, plus `constant` when
any column has a single value; a single data frame if only one section
results.

## Details

Constant columns are split into their own `constant` section rather than
padding every other table with rows of `NA`: a variable with one value
tells you nothing a statistic can show, and it will break a model
matrix.

## See also

[`ilm_frame_issues()`](https://huttoncp.github.io/illumex/reference/ilm_frame_issues.md)
for problems belonging to the data frame as a whole: duplicated,
aliased, nested and collinear columns.

## Examples

``` r
d <- ilm_sim()
ilm_describe_all(d)
#> $numeric
#>    variable obs   n  na         sum      mean        sd      se      p0
#> 1        id 900 900   0    34200.00    38.000    21.661   0.722    1.00
#> 2     score 900 900   0    44709.09    49.677     5.880   0.196   29.99
#> 3    income 900 900   0 33361609.93 37068.455 22902.677 763.423 6356.18
#> 4    visits 900 900   0     2779.00     3.088     1.830   0.061    0.00
#> 5    claims 900 900   0     3773.00     4.192     4.657   0.155    0.00
#> 6  downtime 900 900   0     1339.00     1.488     2.386   0.080    0.00
#> 7 lab_value 900 792 108     5840.86     7.375     1.153   0.041    4.04
#>         p50      p100 p_zero dispersion gauss
#> 1    38.000     75.00  0.000     12.347 0.438
#> 2    49.835     65.34  0.000         NA 1.000
#> 3 31137.270 182886.22  0.000         NA 0.000
#> 4     3.000     10.00  0.053      1.085 0.000
#> 5     3.000     27.00  0.177      5.172 0.000
#> 6     0.000     11.00  0.640      3.826 0.000
#> 7     7.330     11.66  0.000         NA 1.000
#>                                                                                 gauss_note
#> 1                                                     bounded at zero; light-tailed / flat
#> 2                                                                                         
#> 3                                                               right-skewed; heavy-tailed
#> 4                                           discrete (11 distinct values); bounded at zero
#> 5 18% of values sit exactly at the minimum (0): a floor, see ilm_censor(); bounded at zero
#> 6                                           discrete (12 distinct values); bounded at zero
#> 7                                                                                         
#> 
#> $time
#>   variable obs   n na n_unique      start        end span_days spacing regular
#> 1     date 900 900  0       12 2024-01-01 2024-12-01       335 monthly    TRUE
#>   n_gaps n_dup_times   tz
#> 1      0         888 <NA>
#>                                                       note
#> 1 repeated dates: 12 unique across 900 rows (long format?)
#> 
#> $categorical
#>   variable obs   n na n_empty n_unique ordered p_max n_rare n_unused
#> 1      grp 900 900  0       0        4   FALSE 0.449      1        1
#> 2     site 900 900  0      46        6   FALSE 0.308      0        0
#>   case_variants                      counts_tb
#> 1             0 alpha_404, beta_329, gamma_164
#> 2             1 North_277, South_265, East_217
#>                                               note
#> 1 1 unused levels; 1 rare levels (separation risk)
#> 2 46 empty strings (not NA); 1 case/space variants
#> 
#> $logical
#>    variable obs   n na n_TRUE n_FALSE p_TRUE                            note
#> 1      flag 900 900  0    459     441  0.510                                
#> 2 consented 900 900  0    898       2  0.998 near-constant (separation risk)
#> 
#> $constant
#>   variable       class obs   n na value
#> 1   cohort categorical 900 900  0  2024
#> 
ilm_describe_all(d, class = "numeric", skew = TRUE)
#> ilm_describe_all: 1 constant column(s) set aside (cohort); use class = "all" to see them
#>    variable obs   n  na         sum      mean        sd      se      p0
#> 1        id 900 900   0    34200.00    38.000    21.661   0.722    1.00
#> 2     score 900 900   0    44709.09    49.677     5.880   0.196   29.99
#> 3    income 900 900   0 33361609.93 37068.455 22902.677 763.423 6356.18
#> 4    visits 900 900   0     2779.00     3.088     1.830   0.061    0.00
#> 5    claims 900 900   0     3773.00     4.192     4.657   0.155    0.00
#> 6  downtime 900 900   0     1339.00     1.488     2.386   0.080    0.00
#> 7 lab_value 900 792 108     5840.86     7.375     1.153   0.041    4.04
#>         p50      p100   skew p_zero dispersion gauss
#> 1    38.000     75.00  0.000  0.000     12.347 0.438
#> 2    49.835     65.34 -0.077  0.000         NA 1.000
#> 3 31137.270 182886.22  1.713  0.000         NA 0.000
#> 4     3.000     10.00  0.563  0.053      1.085 0.000
#> 5     3.000     27.00  1.952  0.177      5.172 0.000
#> 6     0.000     11.00  1.558  0.640      3.826 0.000
#> 7     7.330     11.66 -0.038  0.000         NA 1.000
#>                                                                                 gauss_note
#> 1                                                     bounded at zero; light-tailed / flat
#> 2                                                                                         
#> 3                                                               right-skewed; heavy-tailed
#> 4                                           discrete (11 distinct values); bounded at zero
#> 5 18% of values sit exactly at the minimum (0): a floor, see ilm_censor(); bounded at zero
#> 6                                           discrete (12 distinct values); bounded at zero
#> 7                                                                                         
```
