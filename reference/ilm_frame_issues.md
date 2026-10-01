# Problems that belong to the data frame as a whole

Constant columns, identifier-like columns, duplicated columns,
near-perfect collinearity, categorical columns that carry the same
grouping, and columns that are an exact combination of others. These
cannot live in a per-variable table because they are properties of the
data frame as a whole, and most of them break a model matrix: a model
given two columns that say the same thing cannot tell their effects
apart.

## Usage

``` r
ilm_frame_issues(data, cor_cut = 0.999, v_cut = 0.95)
```

## Arguments

- data:

  A data frame.

- cor_cut:

  Absolute correlation at or above which a numeric pair is reported as
  collinear.

- v_cut:

  Cramer's V at or above which a pair of categorical columns is reported
  as redundant.

## Value

A data frame with `issue`, `columns`, `detail` and `remedy`, one row per
problem; zero rows when nothing is found.

## Details

A continuous variable is unique per row by construction, so uniqueness
is reported as identifier-like only for discrete-valued columns.

Categorical columns (factors, characters and logicals) are compared in
pairs. Two whose levels match one to one are `aliased_factors`: the same
grouping under different labels. One whose every level falls within a
single level of the other is `nested`, as patients within clinics are;
that is reported only when at least half of the finer column's levels
have two or more rows, since a level seen once sits inside one level of
anything. Otherwise a pair is `redundant_categories` when Cramer's V
reaches `v_cut`: nearly every level of one predicts a level of the
other.

Pairs are not the whole story: a column can be an exact combination of
several others (a total and its parts, a numeric code fixed by a
factor's levels) with no pair looking alike. So the numeric and
categorical columns are also put into one model matrix, with an
intercept and treatment contrasts, on the rows complete across them, and
each column the matrix cannot separate from the rest is reported as
`rank_deficient` with what it is a combination of. So that each problem
is reported once, columns already reported as constant, identifier-like,
duplicated or aliased are left out of that matrix, and a nested pair is
not reported again from it.

## References

Cramer, H. (1946). Mathematical Methods of Statistics. Princeton
University Press.

## Examples

``` r
ilm_frame_issues(ilm_sim())
#>      issue columns                                 detail
#> 1 constant  cohort zero variance; breaks the model matrix
#>                                                           remedy
#> 1 Leave it out: a column with one value cannot explain anything.

d <- data.frame(a = rnorm(50), b = rnorm(50),
                clinic = rep(c("x", "y"), each = 25),
                ward = rep(c("x1", "x2", "y1", "y2"), c(12, 13, 12, 13)))
d$total <- d$a + d$b
ilm_frame_issues(d)
#>            issue        columns
#> 1         nested ward in clinic
#> 2 rank_deficient  total ~ a + b
#>                                                                    detail
#> 1 each of the 4 levels of ward falls within one of the 2 levels of clinic
#> 2                         total is an exact linear combination of a and b
#>                                                                                                                                                                        remedy
#> 1 Expected for grouping factors: as random effects, write (1 | clinic/ward). As fixed effects keep one of them, since ward absorbs every difference between levels of clinic.
#> 2                                                                    Leave out total or one of the columns it is built from; with all of them a model has no unique solution.
```
