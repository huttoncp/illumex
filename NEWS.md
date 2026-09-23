# illumex 0.0.7.9000

* First version: the exploratory half of `illume` 0.0.7.9000, split out into a
  package of its own. Every function keeps its name, its arguments and its
  behaviour, and `illume` attaches `illumex`, so code written against `illume`
  runs unchanged.
* What moved: description and cleaning (`ilm_describe*()`, `ilm_counts*()`,
  `ilm_dupes()`, `ilm_copies()`, `ilm_wash_df()`, `ilm_recode_errors*()`,
  `ilm_translate()`, `ilm_frame_issues()`, `ilm_gauss_check()`); bootstrap
  intervals without a model (`ilm_boot_ci()`, `ilm_boot_diff()`); outliers and
  multivariate anomalies; dimension reduction, clustering and profiling,
  `ilm_glrm()` included; describing missing values (`ilm_check_missing()` and
  the `*_na()` functions); the plots of data rather than of a model; and the
  example data, `ilm_sim()`.
* What stayed in `illume`: everything that fits or reads a model, including
  imputation and pooling (`ilm_impute()`, `ilm_mi_pool()`), which need one.
* It needs neither TMB nor RTMB. Its only imports are `collapse`, `tinyplot`
  and base R's own packages, and `cluster`, `isotree` and `PCAmixdata` are used
  where they are installed.
