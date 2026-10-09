# Adaptive plot of one or two variables

Chooses a plot appropriate to the classes of what you give it, annotates
it with the same diagnostic verdict
[`ilm_describe()`](https://huttoncp.github.io/illumex/reference/ilm_describe.md)
reports, and switches to a binned density when there are too many points
to show individually.

## Usage

``` r
ilm_plot(
  data,
  x,
  y = NULL,
  by = NULL,
  geom = "auto",
  colour = NULL,
  color = NULL,
  fill = NULL,
  alpha = NULL,
  size = NULL,
  palette = NULL,
  theme = NULL,
  n_max = 5000L,
  max_levels = 20L,
  verdict = TRUE,
  main = NULL,
  ...,
  pch = NULL,
  facet = NULL,
  subset = NULL,
  subset_negate = FALSE,
  subset_fixed = FALSE
)
```

## Arguments

- data:

  A data frame.

- x:

  Name of the variable on the horizontal axis.

- y:

  Optional second variable; required by some geoms.

- by:

  Optional grouping variable, drawn as colour with a legend.

- geom:

  `"auto"` or one of the geoms in the table below.

- colour, color:

  Colour of lines, points and borders. Synonyms; give one.

- fill:

  Fill colour for bars, boxes, violins and densities.

- alpha:

  Opacity between 0 (transparent) and 1 (opaque).

- size:

  Point size for `"point"`, line or border width elsewhere.

- palette:

  Colours for groups: a vector of colour names, or the name of a palette
  such as `"Dark 2"`.

- theme:

  A tinyplot theme name, such as `"clean"`; the available names are
  listed by
  [`tinyplot::tinytheme_list()`](https://grantmcdermott.com/tinyplot/man/tinytheme_register.html),
  and an unknown one is reported here.

- n_max:

  Above this many points a scatter becomes a binned density.

- max_levels:

  Categorical levels beyond this are pooled into `(other)`.

- verdict:

  Annotate the plot with the diagnostic verdict.

- main:

  Plot title.

- ...:

  Passed to
  [`tinyplot::tinyplot()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).

- pch:

  Plotting character. Takes a name as well as a number:
  `"filled circle"` is 16, and every code from 0 to 25 has one. Case,
  spaces, underscores and hyphens are ignored. A single character is
  drawn literally, so `pch = "x"` is still the letter x.

- facet:

  Optional column to draw in panels, one per level, named as a string
  the way `by` is: `facet = "site"`. Not available for the binned
  density (`"bin2d"`).

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

Invisibly, a list with the geom used, the reason, and the note shown.

## Which arguments each geom needs

`geom = "auto"` picks from the data. When you name a geom instead, some
require both `x` and `y` and some use `x` alone.
[`ilm_geom_spec()`](https://huttoncp.github.io/illumex/reference/ilm_geom_spec.md)
returns the full table, and it is the same table the argument checks
read, so it cannot drift from the behaviour:

|           |                        |                      |                     |
|-----------|------------------------|----------------------|---------------------|
| **geom**  | **x**                  | **y**                | **`size` controls** |
| histogram | numeric                | not used             | border width        |
| density   | numeric                | not used             | line width          |
| bar       | categorical or logical | not used             | border width        |
| box       | categorical or numeric | required             | line width          |
| violin    | categorical or numeric | required             | line width          |
| point     | numeric                | required numeric     | point size          |
| bin2d     | numeric                | required numeric     | not used            |
| line      | time or numeric        | required             | line width          |
| spine     | categorical            | required categorical | border width        |

## See also

[`ilm_geom_spec()`](https://huttoncp.github.io/illumex/reference/ilm_geom_spec.md),
[`ilm_pick_geom()`](https://huttoncp.github.io/illumex/reference/ilm_pick_geom.md),
[`ilm_plot_all()`](https://huttoncp.github.io/illumex/reference/ilm_plot_all.md),
`illume::ilm_plot_model()`.

## Examples

``` r
d <- ilm_sim()
ilm_plot(d, "score")
#> ilm_plot: gauss 1.00

ilm_plot(d, "grp", "score", geom = "violin", fill = "lightblue")
#> ilm_plot: gauss 1.00

ilm_plot(d, "score", "income", by = "grp", alpha = 0.6)
#> ilm_plot: gauss 0.00 — right-skewed; heavy-tailed
```
