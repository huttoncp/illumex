# Scatter plot

Scatter plot

## Usage

``` r
ilm_plot_scatter(
  data,
  y,
  x,
  by = NULL,
  trend = c("none", "lm", "loess", "gam"),
  ...,
  pch = NULL,
  facet = NULL,
  legend = NULL,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame.

- y, x:

  Names of the numeric columns.

- by:

  Optional grouping column.

- trend:

  `"none"`, `"lm"`, `"loess"` or `"gam"`, drawn over the points with its
  band; see Trend bands.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

- pch:

  Plotting character. Takes a NAME as well as a number:
  `"filled circle"` is 16, and every code from 0 to 25 has one. Case,
  spaces, underscores and hyphens are ignored. A single character is
  drawn literally, so `pch = "x"` is still the letter x.

- facet:

  Optional column to draw in panels, one per level, named as a string
  the way `by` is: `facet = "site"`.

- legend:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html),
  in any form it takes. With `by`, the legend is titled with the `by`
  column's name unless you give a title.

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

`NULL`, invisibly.

## Trend bands

Each trend is fitted separately in every group and panel and drawn over
the points on 100 values spanning that group's `x`, with a 95% band for
the fitted line:

- `"lm"`: a straight line; the band is the confidence interval for the
  mean from
  [`stats::predict.lm()`](https://rdrr.io/r/stats/predict.lm.html), with
  Student's t on the residual degrees of freedom (what tinyplot's own
  `"lm"` type draws).

- `"loess"`: [`stats::loess()`](https://rdrr.io/r/stats/loess.html) at
  its defaults (span 0.75, degree 2); the band is the fit plus or minus
  t times loess's standard error, on loess's own degrees of freedom
  (what tinyplot's `"loess"` type draws).

- `"gam"`: `mgcv::gam(y ~ s(x), method = "REML")`, a smooth whose
  wiggliness is chosen from the data; the band is the fit plus or minus
  1.96 of mgcv's standard errors, mgcv's Bayesian credible interval,
  which holds close to 95% coverage averaged across the curve rather
  than at each point (Marra and Wood 2012). Where a group has fewer than
  ten distinct `x` values the smooth's basis is cut to that number; with
  fewer than four no line is drawn. Needs the mgcv package, which comes
  with R.

A trend line is a description of the two columns shown and nothing more
– it holds nothing else fixed, so it is not the effect
`illume::ilm_model()` would estimate.

## References

Marra, G. and Wood, S. N. (2012). Coverage properties of confidence
intervals for generalized additive model components. Scandinavian
Journal of Statistics, 39(1), 53-74.

Wood, S. N. (2017). Generalized Additive Models: An Introduction with R,
2nd edition. Chapman and Hall/CRC.

## See also

[`ilm_plot_var_pairs()`](https://huttoncp.github.io/illumex/reference/ilm_plot_var_pairs.md)
for every pair at once.

## Examples

``` r
ilm_plot_scatter(mtcars, "mpg", "wt", by = "cyl", trend = "lm")

ilm_plot_scatter(mtcars, "mpg", "wt", facet = "cyl")

ilm_plot_scatter(mtcars, "mpg", "hp", trend = "gam")
```
