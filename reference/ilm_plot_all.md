# Small multiples of every variable

Legends are suppressed in this layout: tinyplot reserves legend space by
altering the device layout, which blanks a multi-panel figure. Plot a
single variable with
[`ilm_plot()`](https://huttoncp.github.io/illumex/reference/ilm_plot.md)
when you need one.

## Usage

``` r
ilm_plot_all(
  data,
  by = NULL,
  cols = NULL,
  class = "all",
  max_panels = 12L,
  n_max = 5000L,
  verdict = FALSE,
  ...,
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

- by:

  Optional grouping variable, drawn as colour with a legend.

- cols:

  Columns to plot. A character vector of names, a regular expression, a
  predicate function such as `is.numeric`, or `NULL` for all of them –
  see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  `by` columns are never among them, and the choice is made among the
  columns `class` allows.

- class:

  `"all"`, or one or more of `"numeric"`, `"categorical"`, `"logical"`,
  `"time"`.

- max_panels:

  Stop after this many variables.

- n_max:

  Above this many points a scatter becomes a binned density.

- verdict:

  Annotate the plot with the diagnostic verdict.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

- cols_negate:

  If `TRUE`, `cols` names the columns to leave out, and every other
  eligible column is used; see
  [ilm_selection](https://huttoncp.github.io/illumex/reference/ilm_selection.md).
  It needs `cols`.

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

Invisibly, a list of the per-panel results.

## Examples

``` r
ilm_plot_all(ilm_sim(), class = "numeric", max_panels = 4)
#> ilm_plot_all: showing the first 4 of 7 variables; raise `max_panels` to see more
```
