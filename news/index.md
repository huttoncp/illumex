# Changelog

## illumex 0.0.7.9000

- First version: the exploratory half of `illume` 0.0.7.9000, split out
  into a package of its own. Every function keeps its name, its
  arguments and its behaviour, and `illume` attaches `illumex`, so code
  written against `illume` runs unchanged.
- What moved: description and cleaning (`ilm_describe*()`,
  `ilm_counts*()`,
  [`ilm_dupes()`](https://huttoncp.github.io/illumex/reference/ilm_dupes.md),
  [`ilm_copies()`](https://huttoncp.github.io/illumex/reference/ilm_copies.md),
  [`ilm_wash_df()`](https://huttoncp.github.io/illumex/reference/ilm_wash_df.md),
  `ilm_recode_errors*()`,
  [`ilm_translate()`](https://huttoncp.github.io/illumex/reference/ilm_translate.md),
  [`ilm_frame_issues()`](https://huttoncp.github.io/illumex/reference/ilm_frame_issues.md),
  [`ilm_gauss_check()`](https://huttoncp.github.io/illumex/reference/ilm_gauss_check.md));
  bootstrap intervals without a model
  ([`ilm_boot_ci()`](https://huttoncp.github.io/illumex/reference/ilm_boot_ci.md),
  [`ilm_boot_diff()`](https://huttoncp.github.io/illumex/reference/ilm_boot_diff.md));
  outliers and multivariate anomalies; dimension reduction, clustering
  and profiling,
  [`ilm_glrm()`](https://huttoncp.github.io/illumex/reference/ilm_glrm.md)
  included; describing missing values
  ([`ilm_check_missing()`](https://huttoncp.github.io/illumex/reference/ilm_check_missing.md)
  and the `*_na()` functions); the plots of data rather than of a model;
  and the example data,
  [`ilm_sim()`](https://huttoncp.github.io/illumex/reference/ilm_sim.md).
- What stayed in `illume`: everything that fits or reads a model,
  including imputation and pooling (`ilm_impute()`, `ilm_mi_pool()`),
  which need one.
- It needs neither TMB nor RTMB. Its only imports are `collapse`,
  `tinyplot` and base R’s own packages, and `cluster`, `isotree` and
  `PCAmixdata` are used where they are installed.
- illumex has a hex sticker of its own. It is shown in the README and on
  the pkgdown site, and the site’s favicons are made from it.
