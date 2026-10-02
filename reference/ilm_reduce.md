# Reduce a data frame's variables to a few dimensions

Takes the columns of a data frame and summarises them as a small number
of dimensions. Which method that means is decided by the column types:
PCA when they are all numeric, multiple correspondence analysis when
they are all categorical, and a mixed method when both are present. A
date or date-time column is used as the time elapsed since its earliest
value, which keeps its order, and by the rhythms in it that the other
columns follow; see `time`.

## Usage

``` r
ilm_reduce(
  data,
  cols = NULL,
  ndim = 5,
  method = c("famd", "glrm", "pcamix"),
  time = c("cycles", "elapsed", "drop"),
  ...,
  cols_negate = FALSE
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

- method:

  `"famd"` (the default) for PCA, MCA or FAMD depending on the column
  types, in closed form; `"pcamix"`, its name from when PCAmixdata
  computed it, is still accepted and means the same. `"glrm"` fits a
  generalized low rank model instead, which uses a loss appropriate to
  each column's type rather than squared error on one-hot indicators,
  and reconstructs a category as a category. It costs an iterative fit,
  and on all-numeric data the two are the same model – see
  [`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md)
  for when it is worth that.

- time:

  What to do with date and date-time columns. `"cycles"`, the default,
  uses each as the time since its earliest value – its order and
  spacing, in one column – and adds the time of day, the day of the
  week, the day of the month and the time of year, each as a sine and
  cosine so that the ends of the cycle meet, but only the cycles some
  other column varies with, and only where the data cover two of the
  cycle. A cycle nothing else follows is noise to a clustering: on two
  known clusters, every cycle given unasked took recovery from 0.38 to
  0.10 where the date meant nothing, while the tested ones left it at
  0.36 there and, where a rhythm was real, raised it from 0.36 to
  between 0.58 (month-end) and 0.94 (winter against summer). The test
  looks at no more than 5,000 rows, so its cost is bounded however large
  the data. `"elapsed"` is the time since the earliest value alone – it
  cannot see a rhythm, since a number that only grows puts every Monday
  somewhere new – and skips the test, for very large data or when only
  order matters. `"drop"` leaves dates out. A duration (`difftime`) is
  used as its number of days.

- ...:

  Passed to
  [`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md)
  when `method = "glrm"`.

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`. A `by` argument is never negated.

## Value

An object of class `"ilm_reduce"`: `method` (`"pca"`, `"mca"` or
`"famd"`), `eig` (dimension, eigenvalue, percent of variance and its
cumulative total), `ind_coord` (`row_id` and one column per retained
dimension – this is what
[`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)
takes), `var_contrib` (`variable`, `dim`, `sqload`: how strongly each
original variable relates to each dimension, on a 0 to 1 scale, for
numeric and categorical variables alike), `n`, and `fit`: the
eigenvalues, coordinates and squared loadings as computed, for anyone
who wants to go past this wrapper. Each dimension's sign is fixed by a
rule, so the same data give the same coordinates everywhere: the column
that loads most on a dimension loads positively.

With `method = "glrm"`, `method` is `"glrm"` and `fit` is the
[`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md)
fit. Its factors are not unique – any rotation of one can be undone in
the other – so the coordinates and loadings reported are an orthogonal
rotation of the fitted low-rank product, as principal components are,
which leaves the reconstruction unchanged. `pct_var` and `cum_pct_var`
are then shares of the numeric columns' variance, and `pct_categorical`
and `cum_pct_categorical` each dimension's share of the fitted
categorical signal, which is on the logit scale and so reported apart.

## Details

The mixed method is Chavent et al.'s, which belongs to the same
generalised-PCA family as the FAMD of Pages without being a
reimplementation of it. The two were checked against each other directly
and agree for this purpose, matching on eigenvalues and on individual
coordinates.

## Size

The default method is closed-form: on one core of a 16 GB Windows
machine (`dev/studies/scale_check.R` in the source) it took under a
second at 250,000 rows. `method = "glrm"` fits iteratively and first
chooses its penalty from 18 held-out fits: 30 seconds at 1,000 rows, 5
minutes at 10,000 and over 15 minutes at 50,000. Giving `lambda` (passed
to
[`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md))
saves the search.

## References

Pages, J. (2004). Analyse factorielle de donnees mixtes. Revue de
Statistique Appliquee 52(4), 93-111.

Chavent, M., Kuentz-Simonet, V., Labenne, A. and Saracco, J. (2014).
Multivariate analysis of mixed data: the PCAmixdata R package. arXiv
1411.4911.

## See also

[`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)
to group the rows,
[`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md)
for the whole pipeline,
[`ilm_reduce_na()`](https://huttoncp.github.io/illumex/reference/ilm_reduce_na.md)
for the same thing applied to missingness.

## Examples

``` r
r <- ilm_reduce(mtcars)
r
#> <ilm_reduce> method = pca, n = 32, 5 dimension(s) retained
#>   first 3 dimension(s) explain 89.9% of the variance
#> 
#>   strongest variable per dimension (squared loading)
#>     dim 1   cyl                      0.924
#>     dim 2   qsec                     0.569
#>     dim 3   carb                     0.175
#>     dim 4   drat                     0.197
#>     dim 5   vs                       0.080
head(r$var_contrib[order(-r$var_contrib$sqload), ])
#>   variable dim    sqload
#> 1      cyl   1 0.9239416
#> 2     disp   1 0.8958370
#> 3      mpg   1 0.8685312
#> 4       wt   1 0.7916038
#> 5       hp   1 0.7199031
#> 6       vs   1 0.6208539
```
