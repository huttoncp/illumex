# illumex 0.0.8.9000

* The version moves in step with `illume` 0.0.8.9000, which now requires
  `illumex (>= 0.0.8.9000)`.
* Dates are used in clustering rather than dropped. `ilm_reduce()`,
  `ilm_glrm()` and `ilm_profile()` gain `time`. By default (`"cycles"`) a
  date or date-time column becomes the time since its earliest value -- R's
  own number for it, which keeps order and spacing in one column -- plus the
  time of day, the day of the week, the day of the month and the time of
  year as sine-cosine pairs, so that 23:00 sits next to midnight and December
  next to January; but only the cycles some other column varies with, since a
  cycle nothing else follows is noise the clustering will split on. A cycle
  is let in when that variation stands five standard deviations above what
  shuffled dates give, a bar set from its measured errors: at the usual p <
  0.01 chance let a cycle in for 3 of 160 data sets with no rhythm, and one
  such admission took a clustering from 0.36 to 0.00. `"elapsed"` keeps the
  time line alone and skips the test, for very large data; `"drop"` is the
  old behaviour. A duration is used as its number of days.
* Measured on two known clusters (400 rows, 20 replicates, adjusted Rand
  index): elapsed time costs nothing where the date is irrelevant (0.38 to
  0.36) but cannot see a rhythm; every cycle given unasked took recovery to
  0.10 where the date meant nothing; the tested cycles left it at 0.36 there
  and raised it from 0.36 to 0.94 for winter against summer, 0.93 for night
  against day, 0.79 for weekends and 0.58 for month-ends. Expanding a date
  into year, month, day and weekday numbers instead scored 0.00 to 0.02 in
  all of those: the year and the date count the trend twice and take over the
  reduction, and a numbered month puts December as far from January as it
  can.
* `ilm_profile()` and `ilm_profile_na()` describe each cluster by the original
  variables that set it apart, in their own units, rather than by the
  reduction's dimensions: "employment is 'retired' for 86% of them, against
  21% overall; age is higher: the middle half 64 to 72, against 29 to 54
  overall". The dimension-based sentences named which variables a cluster's
  position was made of without saying which way the cluster lay on them: on a
  sample with three known subgroups, which the clustering recovered almost
  exactly, two of the three got the same sentence word for word. Variables are
  ranked by a v-test against all rows (as FactoMineR's `catdes()`) and named
  when it clears `vtest_threshold` and the difference is big enough to matter,
  0.2 standard deviations or 10 percentage points -- on 1,200 rows a column of
  pure noise cleared 1.96 in two clusters of three. A date is described in
  dates, down to the stretch of a cycle where a cluster stands out ("falls on
  Sat-Sun for 87% of them, against 50% overall"); markers of missingness as
  "income is missing for 92% of them". The variables that set no cluster apart
  are named after the paragraphs, or counted when there are many.
* `ilm_profile()`'s `characterization` is now one row per cluster and variable
  (and aspect of a date); `frequencies` is new (every value of every
  categorical variable, per cluster, with its count, its share and its share
  among all rows), as is `by_cluster` (`ilm_describe_all()` of the variables,
  by cluster). `top_n_vars` now caps the variables named for one cluster, and
  defaults to 4.
* `ilm_var_contrib()` scores a date on the aspects the clustering used, its
  time line or a cycle, and says which in `type` ("date: day of the week").
  Scored on the time line alone, a clustering that split on the day of the
  week called its own defining variable no better than chance.
* `print.ilm_var_contrib()` prints a subset of its columns as the plain table
  it is, rather than stopping on a column that is not there.

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
* illumex has a hex sticker of its own. It is shown in the README and on the
  pkgdown site, and the site's favicons are made from it.
