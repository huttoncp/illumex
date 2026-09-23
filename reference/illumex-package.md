# illumex: Describe, Clean, Plot and Profile Data Before a Model

The exploratory half of the `illume` workflow, usable on its own: the
routine work before a model, each step one call with a consistent
interface. `illume` attaches this package, so with `illume` loaded
everything here is available as it always was.

## What is in it

- Describing:

  [`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
  summarises every column of a data frame at once, with a gaussian index
  in place of a normality test;
  [`ilm_counts()`](https://huttoncp.github.io/illumex/reference/ilm_counts.md),
  [`ilm_dupes()`](https://huttoncp.github.io/illumex/reference/ilm_dupes.md),
  [`ilm_copies()`](https://huttoncp.github.io/illumex/reference/ilm_copies.md)
  and
  [`ilm_frame_issues()`](https://huttoncp.github.io/illumex/reference/ilm_frame_issues.md)
  find what is wrong with a data frame before any model sees it.

- Cleaning:

  [`ilm_wash_df()`](https://huttoncp.github.io/illumex/reference/ilm_wash_df.md),
  [`ilm_recode_errors()`](https://huttoncp.github.io/illumex/reference/ilm_recode_errors.md),
  [`ilm_translate()`](https://huttoncp.github.io/illumex/reference/ilm_translate.md).

- Uncertainty without a model:

  [`ilm_boot_ci()`](https://huttoncp.github.io/illumex/reference/ilm_boot_ci.md)
  and
  [`ilm_boot_diff()`](https://huttoncp.github.io/illumex/reference/ilm_boot_diff.md).

- Unusual values:

  [`ilm_outliers()`](https://huttoncp.github.io/illumex/reference/ilm_outliers.md)
  for a value extreme in its own column;
  [`ilm_anomaly()`](https://huttoncp.github.io/illumex/reference/ilm_anomaly.md)
  and
  [`ilm_plot_anomaly()`](https://huttoncp.github.io/illumex/reference/ilm_plot_anomaly.md)
  for a row implausible as a combination.

- Structure:

  [`ilm_profile()`](https://huttoncp.github.io/illumex/reference/ilm_profile.md)
  reduces, clusters and describes the clusters in one call, for numeric
  and mixed columns alike;
  [`ilm_reduce()`](https://huttoncp.github.io/illumex/reference/ilm_reduce.md),
  [`ilm_cluster()`](https://huttoncp.github.io/illumex/reference/ilm_cluster.md)
  and
  [`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md)
  are its parts.

- Missing values:

  [`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md),
  [`ilm_describe_na()`](https://huttoncp.github.io/illumex/reference/ilm_describe_na.md)
  and
  [`ilm_plot_missing()`](https://huttoncp.github.io/illumex/reference/ilm_plot_missing.md)
  describe them. Imputing and pooling them is `illume`'s: see
  `illume::ilm_impute()`.

- Plots:

  [`ilm_plot()`](https://huttoncp.github.io/illumex/reference/ilm_plot.md)
  and the `ilm_plot_*()` family, built on `tinyplot` and named for what
  they show.

- Example data:

  [`ilm_sim()`](https://huttoncp.github.io/illumex/reference/ilm_sim.md),
  a grouped data set with a known structure.

## Two spellings

Every exported function whose name starts `ilm_` also answers to `iml_`,
an easy transposition to type:
[`iml_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
is
[`ilm_describe_all()`](https://huttoncp.github.io/illumex/reference/ilm_describe_all.md)
itself, not a wrapper, and opens the same help page.

## Lineage

This is a direct descendant of `elucidate`, by the same author, rebuilt
on `collapse` and `tinyplot`. The two share an interface rather than an
implementation.

## See also

Useful links:

- <https://github.com/huttoncp/illumex>

- <https://huttoncp.github.io/illumex/>

- Report bugs at <https://github.com/huttoncp/illumex/issues>

## Author

**Maintainer**: Craig Hutton <craig.hutton@gmail.com>
